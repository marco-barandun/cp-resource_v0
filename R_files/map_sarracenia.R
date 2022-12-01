library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(scales)
library(tidyverse)

setwd("/Users/marco/GitHub/cp-resource/R_files/")

my_plants <- rbind(read_csv("/Users/marco/2TB_kDrive/Plants/CP_list_seeds.csv") %>% filter(!is.na(county)),
                   read_csv("/Users/marco/2TB_kDrive/Plants/CP_list_plants.csv") %>% filter(!is.na(county))
                   ) %>% distinct(genus, species, subspecies, county, state, country, .keep_all = TRUE) %>% 
  mutate(source = "1_mine")



l2 <- sf::st_read("./l2/l2.shp")

my_plants <- rbind(read_csv("./plant_lists/CP_list_seeds.csv") %>% filter(!is.na(county)),
                   read_csv("./plant_lists/CP_list_plants.csv") %>% filter(!is.na(county))
                   ) %>% distinct(genus, species, subspecies, county, state, country, .keep_all = TRUE) %>% 
  mutate(source = "1_mine")

to_acquire <- read_csv("./plant_lists/to_acquire.csv")

sarracenia <- read_csv("./plant_lists/sarracenia_native.csv") %>%
  mutate(county = str_remove_all(county, pattern = " County"))

sarracenia_df <- my_plants %>%
  bind_rows(., to_acquire) %>%
  bind_rows(., sarracenia) %>%
  arrange(genus, species, subspecies, country, state, county, source) %>%
  select(genus, species, subspecies, country, state, county, source, everything()) %>%
  distinct(genus, species, subspecies, country, state, county, .keep_all = TRUE) %>%
  mutate(scientificName = paste(genus, species))

map_sarracenia <- function(species_list,
                           subspecies = NA,
                           df,
                           l2_global,
                           export = FALSE) {
  
  browser()
  
  for (species in species_list) {
    
    jsCode <- paste0('
 function(el, x, data) {
  var marker = document.getElementsByClassName("leaflet-interactive");
  for(var i=0; i < marker.length; i++){
    (function(){
      var v = data.win_url[i];
      marker[i].addEventListener("click", function() { window.open(v);}, false);
  }()); 
  }
 }
')
    
    
    # Setting the desidered species
    df[df$scientificName == species & df$source == "3_distribution", ] -> df_distribution_sp 
    if(!is.na(subspecies)) {df_distribution_sp <- df_distribution_sp[df_distribution_sp$subspecies == subspecies,] %>% filter(!is.na(genus))}
    df[df$scientificName == species, ] %>% filter(source == "1_mine") -> df_collection_sp
    if(!is.na(subspecies)) {df_collection_sp <- df_collection_sp[df_collection_sp$subspecies == subspecies,]}
    df[df$scientificName == species, ] %>% filter(stringr::str_starts(source, '2_')) -> df_acquire_sp
    if(!is.na(subspecies)) {df_acquire_sp <- df_acquire_sp[df_acquire_sp$subspecies == subspecies,]}
    
    # Filter the global l2 layer to only the polygons of interest
    sp_distribution <-
      l2_global %>% 
      filter(paste(.$COUNTRY, .$NAME_1, .$NAME_2, sep = "_") %in% unique(paste(df_distribution_sp$country, df_distribution_sp$state, gsub("* County", "", df_distribution_sp$county), sep = "_"))) %>%
      mutate(label = paste(.$NAME_2, " County, ", .$NAME_1, sep = ""))
    
    to_acquire <-
      l2_global %>% 
      filter(paste(.$COUNTRY, .$NAME_1, .$NAME_2, sep = "_") %in% unique(paste(df_acquire_sp$country, df_acquire_sp$state, gsub("* County", "", df_acquire_sp$county), sep = "_"))) %>%
      mutate(label = paste(.$NAME_2, " County, ", .$NAME_1, sep = ""))
    
    in_collection <-
      l2_global %>% 
      filter(paste(.$COUNTRY, .$NAME_1, gsub("* County", "", .$NAME_2), sep = "_") %in% paste(df_collection_sp$country, df_collection_sp$state, gsub("* County", "", df_collection_sp$county), sep = "_")) %>%
      mutate(label = paste(.$NAME_2, " County, ", .$NAME_1, sep = "")) %>%
      mutate(win_url = 
               case_when(
                 NAME_1 == 'Florida' & NAME_2 == "Osceola" ~ 'https://google.ch/'))
    
    sp_dist <- sp_distribution %>%
      filter(!paste(.$NAME_1, .$NAME_2, sep = "_") %in% paste(in_collection$NAME_1, in_collection$NAME_2, sep = "_")) %>%
      filter(!paste(.$NAME_1, .$NAME_2, sep = "_") %in% paste(to_acquire$NAME_1, to_acquire$NAME_2, sep = "_"))
    
    labs_sp_distribution <- lapply(seq(nrow(sp_dist)), function(i) {
      paste0(as.data.frame(sp_dist[i, "NAME_2"])$NAME_2,  " ", as.data.frame(sp_dist[i, "TYPE_2"])$TYPE_2, ", ", as.data.frame(sp_dist[i, "NAME_1"])$NAME_1) 
    })
    
    labs_to_acquire <- lapply(seq(nrow(to_acquire)), function(i) {
      paste0( '<p>', as.data.frame(to_acquire[i, "NAME_2"])$NAME_2,  " ", as.data.frame(to_acquire[i, "TYPE_2"])$TYPE_2, ", ", as.data.frame(to_acquire[i, "NAME_1"])$NAME_1, '</p>') 
    })
    
    labs_in_collection <- lapply(seq(nrow(in_collection)), function(i) {
      paste0( '<p>', as.data.frame(in_collection[i, "NAME_2"])$NAME_2,  " ", as.data.frame(in_collection[i, "TYPE_2"])$TYPE_2, ", ", as.data.frame(in_collection[i, "NAME_1"])$NAME_1, '</p>') 
    })
    
    m <- leaflet(sp_dist, width = '100%') %>% 
      addMapPane(name = "polygons", zIndex = 410) %>% 
      addMapPane(name = "maplabels", zIndex = 420) %>% 
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addProviderTiles("CartoDB.PositronOnlyLabels", 
                       options = leafletOptions(pane = "maplabels"),
                       group = "map labels") %>%
      addPolygons(
        data = sp_dist,
        #group = "label1",
        label = ~sp_dist$label,
        fillOpacity = 0.5, 
        color = 'Grey', 
        stroke = TRUE, 
        weight = 1, 
        opacity = .5, 
        highlightOptions = highlightOptions(
          color = "#ff4a4a", 
          weight = 5,
          bringToFront = TRUE
        )
      ) %>%
      addPolygons(
        data = to_acquire,
        #label = lapply(labs_to_acquire, htmltools::HTML),
        fillOpacity = .5, 
        color = 'Red', 
        stroke = TRUE, 
        weight = 1, 
        opacity = .5, 
        highlightOptions = highlightOptions(
          color = "#ff4a4a", 
          weight = 5,
          bringToFront = TRUE
        )
      ) %>%
      addPolygons(
        #label = lapply(labs_in_collection, htmltools::HTML),
        data = in_collection,
        #label = in_collection$label,
        fillOpacity = .5, 
        color = 'Green', 
        stroke = TRUE, 
        weight = 1, 
        opacity = .5, 
        highlightOptions = highlightOptions(
          color = "#ff4a4a", 
          weight = 5,
          bringToFront = TRUE
        )
      ) %>%
      #addTiles() %>%
      #addCircles(data = d, lng = ~as.numeric(longitude), lat = ~as.numeric(latitude), popup = ~info) %>%
      addScaleBar(position = "bottomleft") %>%
      htmlwidgets::onRender(jsCode, data=in_collection)
    
    if (export == TRUE) {saveWidget(m, file=paste("./maps/", gsub(" ", "_", species), ".html", sep = ""))}
    
    
  }
  
  return(m)
  
}

map_sarracenia(species_list = c("Sarracenia_rubra", "Sarracenia rosea"),
               #subspecies = "alabamensis",
               df = sarracenia_df,
               l2_global = l2,
               export = TRUE)
