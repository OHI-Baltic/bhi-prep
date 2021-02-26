## Libraries
source(here::here("R", "setup.R"))
library(sf)

## Functions

#' create regions shape objects
#' 
#' @param sp_dir the file location of the shapefiles folders; the directory should contain a folder for each shapefile
#' @param foldernames folders containing respective shapefiles, besides the standard BHI, ICES, HELCOM subbasins, etc
#'
#' @return defines spatial objects: bhi_rgns, helcom_subbasins, nuts2_rgns, ices_rgns, baltic_mpas in the global environment

regions_shape <- function(sp_dir = file.path(dirname(dir_B), "Shapefiles"), foldernames = NULL){
  
  foldernames <- c(
    foldernames,
    # "bhi_shapefile", 
    # "helcom_subbasins_shapefile",
    # "nuts2_shapefile",
    # "ices_rgn_shapefile",
    # "mpas_shapefile", 
    # "BHI_shapefile_25km_buffer",
    #"HELCOM_subbasins_holasbasins",
    "BHI_shapefile_corrected",
    "ICES_areas"
  )
  
  for(i in foldernames){
    
    if(file.exists(file.path(sp_dir, i))){
      
      ## assuming 1 shapefile per folder!!!
      shp_path <- list.files(
        file.path(sp_dir, i), 
        full.names = TRUE, 
        pattern = "[A-Za-z0-9_]+.shp$"
      )
      
      shp <- sf::st_read(
        dsn = file.path(sp_dir, i), 
        layer = str_extract(basename(shp_path), pattern = "[A-Za-z0-9_]+"),
        quiet = TRUE
      )
      
      name_obj <- paste0(str_extract(i, pattern = "[a-zA-Z0-9]+"), "_rgns_shp")
      message("creating object in global environment: ", name_obj, "\n")
      assign(name_obj, shp, envir = .GlobalEnv) # creates object in global environment!
      
    } else {
      warning(paste(i, "not found, check it exists in spatial directory", sp_dir))
    }
  }
}


#' add information about regions to dataset
#'
#' @param dataset the dataset to add spatial information to (i.e bhi, helcom, and ices regions); ideally with latitute/longitude
#' @param helcomID_col string naming the column in the dataset with HELCOM IDs
#' @param country_col string indicating name of column in the dataset containing country names
#' @param latlon_vars a two element character-vector with names of latitude and longitude columns, values of which can be numeric or character
#' @param rgn_shps_loc the location of the shapefiles with information BHI, HELCOM, and ICES regions
#'
#' @return

join_rgns_info <- function(dataset, helcomID_col = "helcom_id", country_col = "country", 
                           latlon_vars = c("^lat", "^lon"), return_spatial = FALSE, 
                           rgn_shps_loc = file.path(dir_B, "Spatial"), buffer_shp = NULL){
  
  
  colnames(dataset) <- stringr::str_to_lower(names(dataset))
  
  lat <- grep(colnames(dataset), pattern = latlon_vars[1], value = TRUE)
  lon <- grep(colnames(dataset), pattern = latlon_vars[2], value = TRUE)
  message(sprintf("using latitude longitude colnames identified: '%s' and '%s' respectively", lat, lon))
  
  ## call regions_shape function also in this script, to define shapefiles needed in global env
  if(!exists("BHI_rgns_shp", .GlobalEnv)|!exists("ICES_rgns_shp", .GlobalEnv)){
    message("loading BHI regions w/ HELCOM ID, and ICES shapefiles to global environment")
    regions_shape(sp_dir = rgn_shps_loc)
  }
  if(is.na(st_crs(ICES_rgns_shp)$epsg) || st_crs(ICES_rgns_shp)$epsg != 4326){
    ICES_rgns_shp <- st_transform(ICES_rgns_shp, 4326)
  }
  if("Bothian Sea" %in% unique(BHI_rgns_shp$Subbasin)){
    BHI_rgns_shp <- BHI_rgns_shp %>% 
      mutate(Subbasin = as.character(Subbasin)) %>% 
      mutate(Subbasin = ifelse(Subbasin == "Bothian Sea", "Bothnian Sea", Subbasin))
  }
  
  ## latitude/longitude approach ----
  ## if there is lat lon information, use sf::st_join with shapefiles to assign regions
  if(length(lat) == 1 & length(lon) == 1 & !any(is.na(latlon_vars))){
    data_sf0 <- st_as_sf(
      dataset, 
      crs = 4326, 
      agr = "constant", 
      coords = c(lon, lat), 
      remove = FALSE
    )
    
    ## points in the BHI regions 10km buffer zone ----
    if(!is.null(buffer_shp)){
      data_buff_sf <- data_sf0 %>% 
        st_join(select(buffer_shp, BHI_ID)) %>% 
        filter(!is.na(BHI_ID)) %>%
        left_join(st_drop_geometry(BHI_rgns_shp), by = "BHI_ID") %>% 
        mutate(ICES_area = NA, Area_km2_ICES = NA, in_25km_buffer = TRUE) %>% 
        rename(Area_km2_BHI = Area_km2)
    }
    
    ## combine data joined with bhi regions and buffer zone if included
    ## also account for a few other stations specifically
    data_rgns_joined <- data_sf0 %>% 
      st_join(rename(BHI_rgns_shp, Area_km2_BHI = Area_km2)) %>% 
      st_join(rename(ICES_rgns_shp, Area_km2_ICES = Area_km2)) %>%
      mutate(in_25km_buffer = ifelse(!is.na(BHI_ID), FALSE, NA))
    
    if(!is.null(buffer_shp)){
      data_rgns_joined <- rbind(data_rgns_joined, data_buff_sf)
      duplicates <- data_rgns_joined %>% 
        dplyr::select(names(data_sf0)) %>% 
        st_drop_geometry() %>% 
        group_by_all() %>% 
        summarize(count = n()) %>% 
        filter(count > 1)
      data_rgns_joined <- data_rgns_joined %>% 
        left_join(duplicates, by = setdiff(names(data_sf0), "geometry")) %>% 
        ## delete only NAs for BHI_ID etc where count is non-NA i.e. where are duplicates,
        ## effectively keeping only NAs where the points were not within BHI assessment area...
        filter(!(is.na(BHI_ID) & !is.na(count))) %>% 
        dplyr::select(-count)
    }
    
    ## check if any points are not matched with a BHI id ----
    test <- data_rgns_joined %>% 
      dplyr::select(
        zn = one_of("station", country_col, helcomID_col), 
        yr = starts_with("year"), 
        BHI_ID
      ) %>% 
      filter(is.na(BHI_ID))
    
    if(nrow(test) != 0){
      test_result <- sprintf(
        "%s of %s data points (%s, years: %s) have not been",
        nrow(test),
        nrow(dataset),
        paste(
          c(head(unique(test$zn1)) %>% as.character(), 
            ifelse(length(unique(test$zn1)) > 6, "...", "")), 
          collapse = ", "
        ),
        paste(
          c(head(unique(test$yr)), 
            ifelse(length(unique(test$yr)) > 6, "...", "")), 
          collapse = ", "
        )
      )
    } else {test_result <- "all data points are"}
    message(paste(test_result, "assigned corresponding BHI IDs"))
    
    
    ## return spatial object sf or just dataframe
    if(!return_spatial){
      data_rgns_joined <- data_rgns_joined %>% 
        st_drop_geometry() %>% 
        dplyr::select(-Area_km2_BHI, -Area_km2_ICES) %>%
        mutate(Subbasin = as.character(Subbasin), HELCOM_ID = as.character(HELCOM_ID))
    }
  
  ## country/subbasin approach ----
  ## if no lat lon, need to use country or subbasin or other information... 
  } else {
    bhi_rgns_df <- BHI_rgns_shp %>% 
      as_tibble() %>% 
      select(rgn_nam, rgn_key, HELCOM_ID, BHI_ID)
    colnames(bhi_rgns_df) <- stringr::str_to_lower(colnames(bhi_rgns_df))
    
    ## join by helcom_id then use combination of helcom_id and country name/key to determine bhi_id
    data_rgns_joined <- dplyr::left_join(
      dataset, bhi_rgns_df,
      by = c(helcomID_col = "helcom_id", country_col = "rgn_nam"))
  }
  
  ## return result
  return(data_rgns_joined)
}
