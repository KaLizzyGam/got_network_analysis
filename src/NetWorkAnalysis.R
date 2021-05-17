library(tidyverse)
library(tidygraph)
library(ggraph)
library(magrittr)
library(visNetwork)


files <- list.files("../data", pattern = '.csv', full.names = T)
data_got <- files %>% map_df (read_csv)


data<- data_got %>% 
  rename (from = Source, to = Target) %>% 
  select (-Type) 


# SELECCION DE TEMPORADA

season = 1
data_ss = data %>% filter(book == season) #%>% filter(weight >20)
edges <- data_ss %>% as_tibble()


  # CENTRALIDAD

{  
  nodes_centrality <- tidygraph::as_tbl_graph(data_ss, directed = FALSE) %>% 
    mutate(value = centrality_degree(),
           id = name,
           title = name,
    ) %>% 
    as_tibble() %>% 
    rename(label = name) %>% 
    select(id, label, title, value)


  nw_ss_centrality <- visNetwork(nodes_centrality, edges, height = "600px", width = "100%",
             main = paste("Analytics Methods: Game Of Thrones", season, "Network: Centralidad")) %>% 
    #visInteraction(navigationButtons = TRUE) %>%
    visPhysics(
      solver = 'forceAtlas2Based', 
      stabilization = T,
      forceAtlas2Based = list(
        gravitationalConstant = -20, 
        centralGravity = 0.01, 
        springLength = 100,
        springConstant = 0.08,
        avoidOverlap = 0
      )
    ) %>% 
    visNodes(color = 'lightpink', value = 1, scaling = list(min = 10, max = 60)) %>% 
    visEdges(color = "darkgray") %>% 
    visOptions(highlightNearest = TRUE, nodesIdSelection = T) %>% 
    visLegend(main = 'value')
  
  nw_ss_centrality

  top_ss_centralidad <- nodes_centrality %>%   arrange(desc(value)) %>% head(10)
  top_ss_centralidad
  }

  # BETWEENEESS

{
  nodes_btw <- tidygraph::as_tbl_graph(data_ss, directed = FALSE) %>% 
    mutate(value = centrality_betweenness(),
           id = name,
           title = name,
    ) %>% 
    as_tibble() %>% 
    rename(label = name) %>% 
    select(id, label, title, value)
  
  
  nw_ss_btw <- visNetwork(nodes_btw, edges, height = "600px", width = "100%",
                                 main = paste("Analytics Methods: Game Of Thrones", season, "Network: Betweenness")) %>% 
    #visInteraction(navigationButtons = TRUE) %>%
    visPhysics(
      solver = 'forceAtlas2Based', 
      stabilization = T,
      forceAtlas2Based = list(
        gravitationalConstant = -20, 
        centralGravity = 0.01, 
        springLength = 100,
        springConstant = 0.08,
        avoidOverlap = 0
      )
    ) %>% 
    visNodes(color = 'lightblue', value = 1, scaling = list(min = 10, max = 60)) %>% 
    visEdges(color = "darkgray") %>% 
    visOptions(highlightNearest = TRUE, nodesIdSelection = T) %>% 
    visLegend(main = 'value')
  
  nw_ss_btw
  
  top_ss_btw<- nodes_btw %>%   arrange(desc(value)) %>% head(10)
  top_ss_btw

}

  # CERCANIA

{  
  
  nodes_cls <- tidygraph::as_tbl_graph(data_ss, directed = FALSE) %>% 
    mutate(value = centrality_closeness(normalized = TRUE),
           id = name,
           title = name,
    ) %>% 
    as_tibble() %>% 
    rename(label = name) %>% 
    select(id, label, title, value)
  
  
  nw_ss_cls <- visNetwork(nodes_cls, edges, height = "600px", width = "100%",
                          main = paste("Analytics Methods: Game Of Thrones", season, "Network: Cercania")) %>% 
    #visInteraction(navigationButtons = TRUE) %>%
    visPhysics(
      solver = 'forceAtlas2Based', 
      stabilization = T,
      forceAtlas2Based = list(
        gravitationalConstant = -20, 
        centralGravity = 0.01, 
        springLength = 100,
        springConstant = 0.08,
        avoidOverlap = 0
      )
    ) %>% 
    visNodes(color = 'lightgreen', value = 1, scaling = list(min = 10, max = 60)) %>% 
    visEdges(color = "darkgray") %>% 
    visOptions(highlightNearest = TRUE, nodesIdSelection = T) %>% 
    visLegend(main = 'value')
  
  nw_ss_cls
  
  top_ss_cls<- nodes_cls %>%   arrange(desc(value)) %>% head(10)
  top_ss_cls
  
}  


  # EIGEN  

{  
  
  nodes_eigen <- tidygraph::as_tbl_graph(data_ss, directed = FALSE) %>% 
    mutate(value = centrality_eigen(),
           id = name,
           title = name,
    ) %>% 
    as_tibble() %>% 
    rename(label = name) %>% 
    select(id, label, title, value)
  
  
  nw_ss_eigen <- visNetwork(nodes_eigen, edges, height = "600px", width = "100%",
                          main = paste("Analytics Methods: Game Of Thrones", season, "Network: Eigen")) %>% 
    #visInteraction(navigationButtons = TRUE) %>%
    visPhysics(
      solver = 'forceAtlas2Based', 
      stabilization = T,
      forceAtlas2Based = list(
        gravitationalConstant = -20, 
        centralGravity = 0.01, 
        springLength = 100,
        springConstant = 0.08,
        avoidOverlap = 0
      )
    ) %>% 
    visNodes(color = 'wheat', value = 1, scaling = list(min = 10, max = 60)) %>% 
    visEdges(color = "darkgray") %>% 
    visOptions(highlightNearest = TRUE, nodesIdSelection = T) %>% 
    visLegend(main = 'value')
  
  nw_ss_eigen
  
  top_ss_eigen<- nodes_eigen %>%   arrange(desc(value)) %>% head(10)
  top_ss_eigen
  
}  



# TODAS LAS TEMPORADAS 


#data = data %>% filter(weight >20)
edges <- data %>% as_tibble()


# CENTRALIDAD

{  
  nodes_centrality <- tidygraph::as_tbl_graph(data, directed = FALSE) %>% 
    mutate(value = centrality_degree(),
           id = name,
           title = name,
    ) %>% 
    as_tibble() %>% 
    rename(label = name) %>% 
    select(id, label, title, value)
  
  
  nw_centrality <- visNetwork(nodes_centrality, edges, height = "600px", width = "100%",
                                 main = paste("Analytics Methods: Game Of Thrones", "", "Network: Centralidad")) %>% 
    #visInteraction(navigationButtons = TRUE) %>%
    visPhysics(
      solver = 'forceAtlas2Based', 
      stabilization = T,
      forceAtlas2Based = list(
        gravitationalConstant = -20, 
        centralGravity = 0.01, 
        springLength = 100,
        springConstant = 0.08,
        avoidOverlap = 0
      )
    ) %>% 
    visNodes(color = 'lightpink', value = 1, scaling = list(min = 10, max = 60)) %>% 
    visEdges(color = "darkgray") %>% 
    visOptions(highlightNearest = TRUE, nodesIdSelection = T) %>% 
    visLegend(main = 'value')
  
  nw_centrality
  
  top_centralidad <- nodes_centrality %>%   arrange(desc(value)) %>% head(10)
  top_centralidad
}

# BETWEENEESS

{
  nodes_btw <- tidygraph::as_tbl_graph(data, directed = FALSE) %>% 
    mutate(value = centrality_betweenness(),
           id = name,
           title = name,
    ) %>% 
    as_tibble() %>% 
    rename(label = name) %>% 
    select(id, label, title, value)
  
  
  nw_btw <- visNetwork(nodes_btw, edges, height = "600px", width = "100%",
                          main = paste("Analytics Methods: Game Of Thrones", "", "Network: Betweenness")) %>% 
    #visInteraction(navigationButtons = TRUE) %>%
    visPhysics(
      solver = 'forceAtlas2Based', 
      stabilization = T,
      forceAtlas2Based = list(
        gravitationalConstant = -20, 
        centralGravity = 0.01, 
        springLength = 100,
        springConstant = 0.08,
        avoidOverlap = 0
      )
    ) %>% 
    visNodes(color = 'lightblue', value = 1, scaling = list(min = 10, max = 60)) %>% 
    visEdges(color = "darkgray") %>% 
    visOptions(highlightNearest = TRUE, nodesIdSelection = T) %>% 
    visLegend(main = 'value')
  
  nw_btw
  
  top_btw<- nodes_btw %>%   arrange(desc(value)) %>% head(10)
  top_btw
  
}

# CERCANIA

{  
  
  nodes_cls <- tidygraph::as_tbl_graph(data, directed = FALSE) %>% 
    mutate(value = centrality_closeness(normalized = TRUE),
           id = name,
           title = name,
    ) %>% 
    as_tibble() %>% 
    rename(label = name) %>% 
    select(id, label, title, value)
  
  
  nw_cls <- visNetwork(nodes_cls, edges, height = "600px", width = "100%",
                          main = paste("Analytics Methods: Game Of Thrones", "", "Network: Cercania")) %>% 
    #visInteraction(navigationButtons = TRUE) %>%
    visPhysics(
      solver = 'forceAtlas2Based', 
      stabilization = T,
      forceAtlas2Based = list(
        gravitationalConstant = -20, 
        centralGravity = 0.01, 
        springLength = 100,
        springConstant = 0.08,
        avoidOverlap = 0
      )
    ) %>% 
    visNodes(color = 'lightgreen', value = 1, scaling = list(min = 10, max = 60)) %>% 
    visEdges(color = "darkgray") %>% 
    visOptions(highlightNearest = TRUE, nodesIdSelection = T) %>% 
    visLegend(main = 'value')
  
  nw_cls
  
  top_cls<- nodes_cls %>%   arrange(desc(value)) %>% head(10)
  top_cls
  
}  


# EIGEN  

{  
  
  nodes_eigen <- tidygraph::as_tbl_graph(data, directed = FALSE) %>% 
    mutate(value = centrality_eigen(),
           id = name,
           title = name,
    ) %>% 
    as_tibble() %>% 
    rename(label = name) %>% 
    select(id, label, title, value)
  
  
  nw_eigen <- visNetwork(nodes_eigen, edges, height = "600px", width = "100%",
                            main = paste("Analytics Methods: Game Of Thrones", "", "Network: Eigen")) %>% 
    #visInteraction(navigationButtons = TRUE) %>%
    visPhysics(
      solver = 'forceAtlas2Based', 
      stabilization = T,
      forceAtlas2Based = list(
        gravitationalConstant = -20, 
        centralGravity = 0.01, 
        springLength = 100,
        springConstant = 0.08,
        avoidOverlap = 0
      )
    ) %>% 
    visNodes(color = 'wheat', value = 1, scaling = list(min = 10, max = 60)) %>% 
    visEdges(color = "darkgray") %>% 
    visOptions(highlightNearest = TRUE, nodesIdSelection = T) %>% 
    visLegend(main = 'value')
  
  nw_eigen
  
  top_eigen<- nodes_eigen %>%   arrange(desc(value)) %>% head(10)
  top_ss_eigen
  
}  



# GRAFICA CONJUNTA 



nodes <- tidygraph::as_tbl_graph(data, directed = FALSE) %>% 
  mutate(value = centrality_degree(),
         value_btw = centrality_betweenness(directed = F),
         value_cls = centrality_closeness(normalized = TRUE) ,
         value_eigen = centrality_eigen(directed = F),
         id = name,
         title = name,
  ) %>% 
  as_tibble() %>% 
  rename(label = name) %>% 
  select(id, label, title, value, value_btw, value_cls, value_eigen)


  cols <- colorRampPalette(c("red", "blue"))
  colores <- cols(length(nodes$value_btw))
  nodes %<>% mutate(color = colores[rank(value_btw)])

  nw_centralidad_btw <- visNetwork(nodes, edges, height = "600px", width = "100%",
           main = "Analytics Methods: Game Of Thrones Network \n
           Centralidad y Betweenness") %>% 
  #visInteraction(navigationButtons = TRUE) %>%
  visPhysics(
    solver = 'forceAtlas2Based', 
    stabilization = T,
    forceAtlas2Based = list(
      gravitationalConstant = -20, 
      centralGravity = 0.01, 
      springLength = 100,
      springConstant = 0.08,
      avoidOverlap = 0
    )
  ) %>% 
  visNodes(value = 1, scaling = list(min = 10, max = 60)) %>% 
  visEdges(color = "darkgray") %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = T) %>% 
  visLegend(main = 'value')

  nw_centralidad_btw


  cols <- colorRampPalette(c("pink", "blue"))
  colores <- cols(length(nodes$value_eigen))
  nodes %<>% mutate(color = colores[rank(value_eigen)])
  
  
  nw_centralidad_eigen <- visNetwork(nodes, edges, height = "600px", width = "100%",
             main = "Analytics Methods: Game Of Thrones Network \n
             Centralidad y Eigen") %>% 
    #visInteraction(navigationButtons = TRUE) %>%
    visPhysics(
      solver = 'forceAtlas2Based', 
      stabilization = T,
      forceAtlas2Based = list(
        gravitationalConstant = -20, 
        centralGravity = 0.01, 
        springLength = 100,
        springConstant = 0.08,
        avoidOverlap = 0
      )
    ) %>% 
    visNodes(value = 1, scaling = list(min = 10, max = 60)) %>% 
    visEdges(color = "darkgray") %>% 
    visOptions(highlightNearest = TRUE, nodesIdSelection = T) %>% 
    visLegend(main = 'value')
  

  nw_centralidad_eigen
















