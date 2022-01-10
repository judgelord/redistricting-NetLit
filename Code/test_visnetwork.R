library(tidyverse)

# load data
# NOTE: dag.csv is from from DAG_edgelist, not the full dag on the google sheet
dag <- read.csv(here::here("Data",  "dag.csv")) %>% filter(cite_weight > 0)
node_attributes <- read.csv(here::here("data", "node_attributes.csv"))

## OLD PACKAGE
# devtools::install_github("judgelord/literature")
# library(literature)

# NEW PACKAGE (pre-CRAN version)
#devtools::install_github("ngreifer/netlit")
library(netlit)

# now with node and edge attributes 
lit <- review(dag, 
              edge_attributes = c("edge",
                                  "core",
                                  "mechanism", 
                                  "cites", 
                                  "cites_empirical", 
                                  "cite_weight", 
                                  "cite_weight_empirical"), 
              node_attributes = node_attributes %>% distinct())



# all edges
edges <- lit$edgelist %>% 
  mutate(
    detail = paste(edge, mechanism, cites, sep = "<br>") %>% str_remove_all("NA"),
    type = edge,
    title = paste0("<p>", detail, "</p>"),
    #label = type,
    color = ifelse(str_detect(type, "^increase"), "#81a275", "#617d9f"),
    color = ifelse(str_detect(type, "^decrease"), "#b14552", color),
    value = cite_weight) %>%
  distinct()

core <- edges %>% filter(core, !is.na(cites))

cited <- edges %>% filter(cite_weight>0)



# node attributes
nodes <- lit$nodelist %>% mutate(label = node, 
                                 id = node,
                                 # scale nodes by degree
                                 icon.size = degree + 40,
                                 title = paste0("<p>", type, ": ", label,"</p>") %>% str_remove("NA:"),
                                 # levels in case we want Hierarchical Layout
                                 level = ifelse(type == "goal", 1:2, 3:4),
                                 # FontAwesome.com shapes for fun
                                 shape = "icon",
                                 icon.color = case_when(node %in% c(cited$to, cited$from) ~ "black",
                                                        !node %in% c(cited$to, cited$from) ~ "grey"),
                                 icon.code = case_when(type == "condition" ~ "f205", # chess board
                                                       type == "goal" ~ "f24e", # scale  "f05b", # crosshairs
                                                       type == "policy" ~ "f0e3", # gavel
                                                       type == "value" ~ "f004", # "f4be", # hand with heart
                                                       type == "effect" ~ "f080", # "f681", # data 
                                                       type == "metric" ~ "f1de",# "f548", # ruler 
                                                       TRUE ~ "f0c8"), #square
                                 icon.face =  "'FontAwesome'",
                                 icon.weight = "bold")


# save datasets to call in Shiny
save(nodes, file = here("data", "nodes.RData"))
save(edges, file = here("data", "edges.RData"))

write_csv(nodes, here::here("data", "nodes.csv"))
write_csv(edges, here::here("data", "edges.csv"))

library(visNetwork)
#TODO add static dag in new Rmd file
visNetwork::visNetwork(nodes=nodes, edges=edges, width = "100%") %>% 
  visEdges(width=5, color= edges$color, arrows = "to", arrowStrikethrough = F, smooth = T) %>%
  visNodes(scaling=list(min=40, max=50)) %>%
  visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T)) %>%
  visInteraction(hover=TRUE, zoomView = TRUE) %>%
  #visHierarchicalLayout() %>% 
  visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -50)) %>%
  addFontAwesome(name = "font-awesome-visNetwork") %>%
  visLayout(randomSeed = 12)

