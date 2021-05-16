library(tidyverse)
library(tidygraph)
library(ggraph)
library(magrittr)
library(visNetwork)

files <- list.files("data", pattern = ".csv", full.names = T)
data <- files %>% map_df(read_csv)

data %<>% 
  rename(from = Source, to = Target) %>% 
  select(-Type) 


red_tbl <- tidygraph::as_tbl_graph(data, directed = FALSE)
red_tbl



edges <- red_tbl %>% activate(edges) %>% as_tibble
set.seed(13)
red_vis <- visNetwork(nodes = tibble(id = 1:796, label = 1:796), 
                      edges, width = "100%") %>%
  visPhysics(solver ='forceAtlas2Based', 
             forceAtlas2Based = list(gravitationalConstant = - 50, # negativo!
                                     centralGravity = 0.01, 
                                     springLength = 100,
                                     springConstant = 0.08,
                                     avoidOverlap = 0
             ))
red_vis

###################

data1 <- data %>% filter(book == 1) %>% filter(weight > 10)

nodes <- tidygraph::as_tbl_graph(data1, directed = FALSE) %>% 
  mutate(value = centrality_degree(),
         value_btw = centrality_betweenness(),
         id = name,
         title = name,
         ) %>% 
  as_tibble() %>% 
  rename(label = name) %>% 
  select(id, label, title, value, value_btw)

cols <- colorRampPalette(c("red", "blue"))
colores <- cols(length(nodes$value_btw))

nodes %<>% mutate(color = colores[rank(value_btw)])

edges = data1

visNetwork(nodes, edges, height = "800px", width = "100%",
           main = "Analytics Methods: Game Of Thrones Network") %>% 
  #visInteraction(navigationButtons = TRUE) %>%
  visPhysics(
    solver = 'forceAtlas2Based', 
    stabilization = T,
    forceAtlas2Based = list(
      gravitationalConstant = -20, # negativo!
      centralGravity = 0.01, 
      springLength = 100,
      springConstant = 0.08,
      avoidOverlap = 0
      )
    ) %>% 
  visNodes(value = 1, scaling = list(min = 10, max = 60)) %>% 
  visEdges(color = "darkgray") %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = T)

















