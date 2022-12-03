library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(scales)
library(tidyverse)

setwd("/Users/marco/GitHub/cp-resource/R_files/")

### Reading in the world shapefile
world_adm <- rgdal::readOGR("/Users/marco/Desktop/sarracenia/ADM_2.shp")
l2 <- sf::st_read("/Users/marco/Desktop/sarracenia/ADM_2.shp")

#------------------------------------------------------------------------------
### Importing and cleaning Sarracenia distriburion data
get_counties <- function(occurrences_df,
                         world_shapefile) {
  
  # Remove points that are located in the ocean
  occs_to_clean <- sp::SpatialPointsDataFrame(coords = occurrences_df %>% select(decimalLongitude, decimalLatitude), 
                                              data = occurrences_df) ##check columns for long/lat

  raster::crs(occs_to_clean) <- raster::crs(world_shapefile)
  ovr <- sp::over(occs_to_clean, world_shapefile) %>%###overlay world and points
    select("COUNTRY", "NAME_1", "NAME_2", "TYPE_2", "ENGTYPE_2")

  cleaned_ds <- cbind(occurrences_df, ovr) %>%
    select(genus, species, subspecies, county, NAME_2, state, NAME_1, country, COUNTRY, everything()) %>%
    mutate(NAME_2 = ifelse(is.na(NAME_2), county, NAME_2)) %>%
    mutate(NAME_1 = ifelse(is.na(NAME_1), state, NAME_1)) %>%
    mutate(COUNTRY = ifelse(is.na(COUNTRY), country, COUNTRY)) %>%
    arrange(genus, species, subspecies, NAME_2, NAME_1, COUNTRY, elevation, elevationAccuracy, .keep_all = TRUE) %>%
    distinct(genus, species, subspecies, NAME_2, NAME_1, COUNTRY, .keep_all = TRUE) %>%
    select(-county, -state, -country) %>%
    rename(county = NAME_2) %>%
    rename(state = NAME_1) %>%
    rename(country = COUNTRY)
  
  return(cleaned_ds)
}

# Importing information about the native Sarracenia counties
native <- read_csv("./plant_lists/sarracenia_native.csv") %>%
  filter(!str_detect(taxon_name, "×")) %>%
  mutate(taxon_name = gsub(" subsp.", "", taxon_name))

# sarracenia is a dataset including the native Sarracenia counties
sarracenia <- read.csv("./plant_lists/sarracenia_withCounties.csv") %>%
  mutate(sciname = verbatimScientificName) %>%
  mutate(verbatimScientificName = as.character(paste(verbatimScientificName))) %>%
  rename(subspecies = infraspecificEpithet) %>%
  merge(., native, by.x = c("sciname", "state"), by.y = c("taxon_name", "area")) %>%
  filter(!is.na(county)) %>%
  select("gbifID", "verbatimScientificName", "county", "state", "country", "elevation", "elevationAccuracy",
         "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "year", "scientificName") %>%
  filter(!verbatimScientificName == "Sarracenia") %>%
  mutate(source = "3_distribution") %>%
  mutate(county = str_remove_all(county, pattern = " County")) %>%
  separate(verbatimScientificName, c("genus", "species", "subspecies")) %>%
  mutate(subspecies = ifelse(scientificName == "Sarracenia rubra Walter", "rubra", subspecies)) %>%
  mutate(subspecies = ifelse(scientificName == "Sarracenia purpurea L.", "purpurea", subspecies)) %>%
  mutate(subspecies = ifelse(scientificName == "Sarracenia alabamensis F.W. & R.B.Case", "alabamensis", subspecies)) %>%
  mutate(subspecies = ifelse(scientificName == "Sarracenia jonesii Wherry", "jonesii", subspecies)) %>%
  mutate(species = ifelse(scientificName == "Sarracenia jonesii Wherry", "rubra", species)) %>%
  mutate(species = ifelse(scientificName == "Sarracenia alabamensis F.W. & R.B.Case", "rubra", species)) %>%
  mutate(species = ifelse(scientificName == "Sarracenia alabamensis subsp. wherryi Case & Case", "rubra", species)) %>%
  get_counties(occurrences_df = .,
               world_shapefile = world_adm)

# Importing the Sarracenia I already have
my_plants <- rbind(read_csv("./plant_lists/CP_list_seeds.csv") %>% filter(!is.na(county)),
                   read_csv("./plant_lists/CP_list_plants.csv") %>% filter(!is.na(county))
                   ) %>% distinct(genus, species, subspecies, county, state, country, .keep_all = TRUE) %>% 
  mutate(source = "1_mine")

# Importing the Sarracenia I know where to get
to_acquire <- read_csv("./plant_lists/to_acquire.csv")

# Merging the datasets
sarracenia_df <- my_plants %>%
  bind_rows(., to_acquire) %>%
  bind_rows(., sarracenia) %>%
  mutate(species = gsub("leucopyhlla", "leucophylla", species)) %>%
  filter(species != "x moorei") %>%
  arrange(genus, species, subspecies, country, state, county, source) %>%
  select(genus, species, subspecies, country, state, county, source, everything()) %>%
  distinct(genus, species, subspecies, country, state, county, .keep_all = TRUE) %>%
  mutate(scientificName = paste(genus, species)) %>%
  filter(!is.na(county))


map_sarracenia <- function(species_list,
                           subspecies = NA,
                           df,
                           l2_global,
                           export = FALSE) {
  
  #browser()
  
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
    
    p <- df %>%
      merge(., l2_global, by.x = c("country", "state", "county"), by.y = c("COUNTRY", "NAME_1", "NAME_2"))
    
    
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
        label = lapply(labs_sp_distribution, htmltools::HTML),
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
        label = lapply(labs_to_acquire, htmltools::HTML),
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
        label = lapply(labs_in_collection, htmltools::HTML),
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
    
    print(paste("Mapped:", species))
    
    if (export == TRUE) {
      if (is.na(subspecies)) {saveWidget(m, file=paste("./maps/", gsub(" ", "_", species), ".html", sep = ""))}
      if (!is.na(subspecies)) {saveWidget(m, file=paste("./maps/", gsub(" ", "_", species), "_", subspecies, ".html", sep = ""))}
      }
  
  }
  
  return(m)
}

map_sarracenia(species_list = "Sarracenia rubra",
               #subspecies = "purpurea",
               df = sarracenia_df,
               l2_global = l2,
               export = TRUE)
