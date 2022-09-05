# This script updates the csvs and Rdata files used in the shiny app and NetLit examples 

library(googlesheets4)
library(googledrive)
library(tidyverse)
library(here)
library(igraph)
library(magrittr)

# refresh data from google sheet if token is present
if(gs4_has_token()){
  
  
  # Redistricing 
  dag <- googledrive::drive_get("redistricting vars") %>%
    gs4_get() %>% 
    read_sheet("DAG_edgelist") 
  1
  1
  
  # Correspondence: https://docs.google.com/spreadsheets/d/1LACTrfsXGX2J7JlFgnF3bniUQpsMtKBVSQSYo0KPmkc/edit?usp=sharing
  
  write_csv(dag, here("Data",  "dag.csv"))
  
  literature <- dag %>% 
    mutate(to = str_to_lower(to),
           from = str_to_lower(from)) %>% 
    as.data.frame()
  
  
  # trim down for package example 
  literature %<>% 
    select(to, edge, from, cites, cites_empirical) %>% 
    #FIXME in sheet 
    mutate(edge = edge %>% str_extract("increase|decrease") %>% replace_na("other") %>% str_to_title())
  
  write_csv(literature, here("Data",  "literature.csv"))
  
  literature %>% 
    save(file =  here::here("Data", "literature.rda") )
  
  literature %>% 
    save(file =  here::here("Data", "literature.rdata") )
  
  node_attributes <-  googledrive::drive_get("redistricting vars") %>%
    gs4_get() %>% 
    read_sheet("DAG_node_attributes") %>% 
    mutate(node = str_to_lower(node)) %>%
    distinct() 

  node_attributes %<>% filter(node %in% c(literature$to, literature$from))  %>% 
    as.data.frame()
  
  node_attributes %>% write_csv(here("data", "node_attributes.csv"))
  
  save(node_attributes, file =  here("data", "node_attributes.rda"))
  
  
  #  CHECK COMPLETENESS OF node_attributes sheet 
  # from  nodes missing node attributes
  literature %>% filter(!from  %in% node_attributes$node, !is.na(from)) %>% select(from)  %>% knitr::kable("markdown")
  
  # to nodes missing node attributes
  literature %>% filter(!to  %in% node_attributes$node, !is.na(to)) %>% select(to)  %>% knitr::kable("markdown")
  
  master_bib <-  googledrive::drive_get("redistricting vars") %>%
    gs4_get() %>% 
    read_sheet("Master bib")  %>% 
    as.data.frame()
  
  save(master_bib, file =  here("data", "master_bibliography.rda"))
  literature_metadata <- master_bib
  save(literature_metadata, file =  here("data", "literature_metadata.rda"))
  
} else { 
  warning("Google sheets is not authorized, run lines above to get auth tokens if you want to update the data.")}

source(here::here("code", "make_vignette_appendix.R"))

