library(tidyverse)
library(tidygraph)
library(ggraph)
library(magrittr)
library(visNetwork)
library(RColorBrewer)

files <- list.files("data", pattern = ".csv", full.names = T)
data <- files %>% map_df(read_csv)

data %<>% 
  rename(from = Source, to = Target) %>% 
  select(-Type) 


red_tbl <- tidygraph::as_tbl_graph(data, directed = FALSE)
red_tbl

###################

data1 <- data %>% 
  filter(book == 1) %>% 
  dplyr::group_by(from, to) %>% 
  summarise(weight = sum(weight), .groups = "drop") #%>% 
  filter(weight > 20)

nodes <- tidygraph::as_tbl_graph(data1, directed = FALSE) %>% 
  mutate(value = centrality_degree(),
         value_btw = centrality_betweenness(directed = F),
         value_cls = centrality_closeness(normalized = TRUE) ,
         value_eigen = centrality_eigen(directed = F),
         group = as.factor(group_fast_greedy(weight)),
         id = name,
         title = name,
         ) %>% 
  as_tibble() %>% 
  mutate(label = name) %>% 
  select(id, label, title, name, group,
         value, value_btw, value_cls, value_eigen)


cols <- colorRampPalette(rainbow(length(unique(nodes$group))))
colores <- cols(length(nodes$group))

nodes %<>% mutate(color = colores[rank(group)])
edges = data1 %>% mutate(value = weight)
col <- arrange(distinct(select(nodes, group, color)), group)

legendNodesSize <- data.frame(
  label = c(1,round((2:6)/6*max(nodes$value))), 
  shape = "icon",
  icon.code = 'f111', 
  icon.size = (1:6)*15, 
  icon.color = "gray"
)

network <- visNetwork(
    nodes, edges, height = "600px", width = "100%",
    main = "Analytics Methods: Game Of Thrones Network") %>% 
  visPhysics(
    solver = 'forceAtlas2Based', 
    stabilization = T,
    forceAtlas2Based = list(
      gravitationalConstant = -20, 
      centralGravity = 0.01, 
      springLength = 100,
      springConstant = 0.08,
      avoidOverlap = 0
      )) %>% 
  visNodes(
    value = 1, scaling = list(min = 10, max = 50),
    labelHighlightBold = T, borderWidth = 2) %>% 
  visEdges(color = "darkgray") %>% 
  visOptions(highlightNearest = TRUE, selectedBy = "name")

for (i in 1:length(unique(nodes$group))) {
  network <- network %>% 
    visGroups(
      groupname = as.character(i),
      color = col[i,2]$color
    )
}

network %>% 
  visLegend(
    main = 'Community Groups & Centrality Level', 
    addNodes = legendNodesSize,
    useGroups = T, 
    position = "right") %>% 
  addFontAwesome()







