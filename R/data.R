## Libraries
source(here::here("R", "setup.R"))
library(httr)
library(jsonlite)
library(xml2)
library(XML)
library(lubridate)
library(data.table)
library(doParallel)
library(foreach)
# library(eurostat)
# library(RCurl)
# library(dbplyr)
# library(DBI)
# library(odbc)
# library(config)
# library(RMySQL)
# library(rvest)
# library(splashr)
# library(RSelenium)

## Functions

#' get data from Baltic Nest data portal
#' 
#' @param date_range list or vector with two values given in YYYYMMDD format
#' @param months values 1 through 12 corresponding to months to obtain data for
#' @param param_codes baltic nest code options: TEMP,QTEMP,SALIN,QSALIN,TOTOXY,QTOTOXY,PO4P,QPO4P,TOTP,
#' QTOTP,SIO4,QSIO4,NO3N,QNO3N,NO2N,QNO2N,NO23N,QNO23N,NH4N,QNH4N,TOTN,QTOTN,CHL,QCHL
#' 
#' @return
#' 
get_nest_data <- function(date_range = c(20050101, 20191231), months = 1:12, 
                          param_codes = c("PO4P", "TOTP", "SIO4", "NO3N", "NO2N", "NO23N", "NH4N", "TOTN", "CHL")){
  
  
  ## check dates input
  date_begin <- lubridate::ymd(date_range[1])
  date_end <- lubridate::ymd(date_range[2])
  if(is.na(date_begin)|is.na(date_end)){
    stop("issue with dates, check date_range was given in YYYYMMDD format")
  }
  
  ## construct dates sequence, to download data only associated w selected months ----
  ## note: only works if more than one yr, otherwise will have only one entry in dates_lst
  months <- stringr::str_pad(months, width = 2, side = "left", pad = "0")
  dates_seq <- seq(date_begin, date_end, by = "months") %>% 
    grep(pattern = paste0("-", months, "-", collapse = "|"), value = TRUE)
  
  dates_lst <- list()
  j <- 1
  for(i in 2:length(dates_seq)){
    ## trying to keep only months of interest, not data in between...
    midyrmonthapart <- lubridate::month(dates_seq[i-1])==lubridate::month(dates_seq[i])-1
    dectojan <- lubridate::month(dates_seq[i-1])==12 & lubridate::month(dates_seq[i])==1
    if(!(midyrmonthapart|dectojan)){
      dates_lst <- c(dates_lst, list(dates_seq[c(j, (i-1))]))
      j <- i
    }
  }
  if(length(dates_lst) == 0){
    dates_lst <- list(c(dates_seq[1], dates_seq[length(dates_seq)]))
  }
  
  ## spatial polygons/boxes to batch download ----
  latlon <- tibble::tibble(
    box1 = c(53.6021, 56.0021, 10.0000, 11.8208), 
    box2 = c(53.6021, 56.0021, 11.8208, 14.2208), 
    box3 = c(53.6021, 56.0021, 14.2208, 16.6208), 
    box4 = c(53.6021, 56.0021, 16.6208, 19.0208), 
    box5 = c(53.6021, 56.0021, 19.0208, 21.4208), 
    box6 = c(65.6021, 68.0021, 21.4208, 23.8208), 
    box7 = c(65.6021, 68.0021, 23.8208, 26.2208), 
    box8 = c(56.0021, 58.4021, 10.1008, 12.5008), 
    box9 = c(56.0021, 58.4021, 12.5008, 14.9008), 
    box10 = c(56.0021, 58.4021, 14.9008, 17.3008), 
    box11 = c(56.0021, 58.4021, 17.3008, 19.7008), 
    box12 = c(56.0021, 58.4021, 19.7008, 22.1008), 
    box13 = c(56.0021, 58.4021, 22.1008, 24.5008), 
    box14 = c(58.4021, 60.8021, 16.1208, 18.5208), 
    box15 = c(58.4021, 60.8021, 18.5208, 20.9208), 
    box16 = c(58.4021, 60.8021, 20.9208, 23.3208), 
    box17 = c(58.4021, 60.8021, 23.3208, 25.7208), 
    box18 = c(58.4021, 60.8021, 25.7208, 28.1208), 
    box19 = c(58.4021, 60.8021, 28.1208, 30.5208), 
    box20 = c(60.8021, 63.2021, 16.9708, 19.3708), 
    box21 = c(60.8021, 63.2021, 19.3708, 21.7708), 
    box22 = c(63.2021, 65.6021, 18.4708, 20.8708), 
    box23 = c(63.2021, 65.6021, 20.8708, 23.2708), 
    box24 = c(63.2021, 65.6021, 23.2708, 25.6708)
  )
 
  ## function to query nest.su.se/dataPortal for data ----
  try_query <- function(queryurl){
    
    ## with error handling...
    tab <- try(
      ## https://csgillespie.github.io/efficientR/5-3-importing-data.html#fast-data-reading
      data.table::fread(queryurl) %>% 
        select("ID", "LATITUDE", "LONGITUDE", "OBSDATE", "OBSTIME", "SHIP", "OBSDEP", param_codes) %>% 
        rowwise() %>% 
        ## filter to include only measurements with nutrient info
        mutate(chk = sum(!!!syms(grep("Q[A-Z0-9]+", param_codes, value = TRUE, invert = TRUE)))) %>% 
        filter(chk != 0) %>% 
        ungroup() %>% 
        select(-chk),
      
      silent = TRUE
    )
    return(tab)
    closeAllConnections()
  }
  
  ## loop through spatial grid boxes and dates ----
  cl <- parallel::makeCluster(3)
  doParallel::registerDoParallel(cl)
  result <- foreach::foreach(box = 1:ncol(latlon), .packages= c("stringr", "readr", "dplyr"), .combine = rbind) %dopar% {
    
    ## initialize results dataframe
    resultpar <- data.frame()
    
    for(dat in 1:length(dates_lst)){
      
      ## construct and open urls
      full_url <- sprintf(
        "%s?latBegin=%s&latEnd=%s&lonBegin=%s&lonEnd=%s&dateBegin=%s&dateEnd=%s",
        "http://nest.su.se/dataPortal/getStations",
        latlon[[1, box]], latlon[[2, box]], latlon[[3, box]], latlon[[4, box]], 
        dates_lst[[dat]][1], dates_lst[[dat]][2]
      )
      
      ## GET THE DATA ----
      ## if error in search, split and try again, show warning with query
      tmp <- try_query(full_url)
      
      if("try-error" %in% class(tmp)){
        
        url1 <- str_replace(
          full_url, 
          "latEnd=[0-9]+.[0-9]+", 
          sprintf("latEnd=%s", latlon[[1, box]]+(latlon[[2, box]]-latlon[[1, box]])/2)
        )
        tmp1 <- try_query(url1)
        if("try-error" %in% class(tmp1)){
          warning(sprintf(
            "ERROR in retrieving data with query: ?%s", 
            url1 %>% stringr::str_extract("latBegin.*$")
          ))
        } else {
          
          if(nrow(tmp1) == 0){
            message(sprintf(
              "no rows of data found for: ?%s", 
              url1 %>% stringr::str_extract("latBegin.*$")
            ))
            
          } else {resultpar <- rbind(resultpar, tmp1)}
        }
        
        url2 <- str_replace(
          full_url, 
          "latBegin=[0-9]+.[0-9]+", 
          sprintf("latBegin=%s", latlon[[1, box]]+(latlon[[2, box]]-latlon[[1, box]])/2)
        )
        tmp2 <- try_query(url2)
        if("try-error" %in% class(tmp2)){
          warning(sprintf(
            "ERROR in retrieving data with query: ?%s", 
            url2 %>% stringr::str_extract("latBegin.*$")
          ))
        } else {
          
          if(nrow(tmp2) == 0){
            message(sprintf(
              "no rows of data found for: ?%s", 
              url2 %>% stringr::str_extract("latBegin.*$")
            ))
            
          } else {resultpar <- rbind(resultpar, tmp2)}
        }
        
      } else {
        ## message if no rows data returned for query
        ## otherwise bind results
        if(nrow(tmp) == 0){
          message(sprintf(
            "no rows of data found for: ?%s", 
            full_url %>% stringr::str_extract("latBegin.*$")
          ))
          
        } else {resultpar <- rbind(resultpar, tmp)}
      }
    }
    result <- resultpar
  }
  stopCluster(cl)
  closeAllConnections()
  return(result)
}


#' compare raw data inputs
#' 
#' checks raw data inputs -from same source/table but obtained different years- against each other
#' the aim is to ensure consistency between sequential assessments, where expected
#'
#' @param data1
#' @param data2
#' @param compare
#' @param keys
#' @param keep_cols
#' @param color_var
#'
#' @return

compare_yrs_data <- function(data1, data2, compare, keys, keep_cols = NA, color_var = NA){

  if("string" %in% class(data1)){data1 <- readr::read_csv(data1)}
  if("string" %in% class(data2)){data2 <- readr::read_csv(data2)}
  
  if(is.na(keep_cols)){keep_cols = compare}
  keep_cols <- union(union(keep_cols, compare), keys)
  
  if(any(!c(keep_cols, keys) %in% names(data1))){
    stop("some key variables and/or selected columns not found in dataset 1")
  } else { df1 <- select(data1, keep_cols) }

  if(any(!c(keep_cols, keys) %in% names(data2))){
    stop("some key variables and/or selected columns not found in dataset 2")
  } else { df2 <- select(data2, keep_cols) }
  
  compare_df <- dplyr::full_join(df1, df2, by = keys)
  colnames(compare_df) <- c(paste0(names(df1), "1"), paste0(setdiff(names(df2), keys), "2")) %>% 
    stringr::str_to_lower() %>% 
    stringr::str_replace_all(pattern = str_to_lower(compare), replacement = "VALUE")

  ## other ideas of things to include in a script for comparing data...
  ## group by relevant categorical vars and check summary stats
  ## check for NAs after filtering to shared date range
  
  if(!is.na(color_var)){
    p <- ggplot2::ggplot(compare_df, aes(x = VALUE1, y = VALUE2, color = color_var))
  } else { p <- ggplot2::ggplot(compare_df, aes(x = VALUE1, y = VALUE2))}
  
  plot_obj <- p + geom_abline(slope = 1, intercept = 0, color = "grey") + 
    geom_point(cex = 2, alpha = 0.1) + 
    theme(legend.position = "none")

  return(plot_obj)
}

#' revise csv file from semicolon to comma delimiters
#'
#' @param csv_filepath the filepath to the csv file to edit, including the filename
#' @param remove_na_cols boolean indicating whether to remove all columns with only NA values
#' @param overwrite boolean indicating whether to immediately overwrite the csv file with updated version
#'
#' @return if overwritten, returns head of updated file read with read_csv, else original table with NA cols removed

semicolon_to_comma <- function(csv_filepath, remove_na_cols = TRUE, overwrite = FALSE){
  
  ## read with semicolon delimiter
  file_semicolon <- read_delim(csv_filepath, delim =  ";")
  remove_cols <- c()
  
  chk <- head(file_semicolon, 15) %>% 
    mutate(n_comma = str_count(as.name(names(file_semicolon)), ","))
  comma_delim <- ncol(file_semicolon)==1 & length(unique(chk$n_comma))==1
  
  ## if already comma delimited
  if(comma_delim){
    message(sprintf(
      "it appears %s is already comma-delimited", 
      basename(csv_filepath)
    ))
    file_commas <- read_csv(csv_filepath, col_types = cols())
    chk_cols <- file_commas
    
    ## if semicolon delimited
  } else { chk_cols <- file_semicolon }
  
  if(remove_na_cols){
    for(i in ncol(chk_cols)){
      column <- chk_cols[, i]
      if(nrow(column) == sum(is.na(column))){
        remove_cols <- c(remove_cols, names(column))
      }
    }
    if(length(remove_cols) > 0){
      print(
        sprintf(
          "removing columns with only NAs: %s",
          paste(remove_cols, collapse = ", ")
        )
      )
    }
  }
  file_commas <- chk_cols %>% 
    select(setdiff(names(chk_cols), remove_cols))
  
  if(overwrite & !(comma_delim & !remove_na_cols)){
    write_csv(file_commas, csv_filepath)
    file_commas <- read_csv(csv_filepath, col_types = cols()) %>% head()
  }
  
  return(file_commas)
}