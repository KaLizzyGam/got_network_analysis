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
data_ss = data %>% filter(book == season) %>% filter(weight >20)
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

















