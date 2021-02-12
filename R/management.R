## Libraries
source(file.path(here::here(), "R", "setup.R"))
library(tools)

## Functions

#' provide a link to the data prep github_document in readme
#' this function is specifically for the data prep documents! not for readmes in general folder structure
#'
#' @param prep_folder filepath starting from `prep`, to a specific data prep folder (e.g. "AO/v2019")
#'
#' @return no immediate output; result is the created readme

create_goal_readme <- function(prep_folder){
  
  dir <- file.path(dir_prep, "prep")
  
  ## don't overwrite existing README.md...
  if(!file.exists(file.path(dir, dirname(prep_folder), "README.md"))) {
    
    ## create git urls
    repo <- "bhi-prep"
    w <- stringr::str_split(file.path(dir, prep_folder), repo)[[1]][2]
    rawurl_goal <- sprintf(
      "https://github.com/OHI-Baltic/%s/tree/master/supplement/goal_summaries/%s.Rmd",
      repo, str_extract(w, pattern = "[A-Z]{2,3}")
    )
    rawurl_prep <- sprintf(
      "https://github.com/OHI-Baltic/%s/tree/master%s/%s_prep.md", 
      repo, tools::file_path_sans_ext(w), 
      str_split(str_to_lower(dirname(prep_folder)), "/")[[1]][length(str_split(dirname(prep_folder), "/")[[1]])]
    )
    
    g <- read_csv(file.path(dir_assess, "index", "conf", "goals.csv")) %>% 
      dplyr::filter(goal == str_extract(w, pattern = "[A-Z]{2,3}"))
    g1 <- ifelse(dim(g)[1] == 0, "Pressure or Resilience Dimensions", g$name)
    g2 <- ifelse(dim(g)[1] == 0, "pressure or resilience dimensions", paste("the", g$name, "goal"))
    
    ## rewrite from readme_template.md
    tmp <- readLines(file.path(dir_prep, "supplement", "templates", "readme_template.md")) %>%
      stringr::str_replace("GOAL1", sprintf("%s", g1)) %>%
      stringr::str_replace("GOAL2", sprintf("%s", g2)) %>%
      stringr::str_replace("SUMMARY", sprintf("[Summary of the goal](%s)", rawurl_goal)) %>%
      stringr::str_replace("FULLDATAPREP", sprintf("[Formatted version of the most recent data prep file (with methods and sources)](%s)", rawurl_prep))
    # stringr::str_replace("DATA MANAGEMENT SOP", sprintf("[data management SOP](%s)", rawurl_prep)) %>% 
    if(is.na(str_extract(w, pattern = "[A-Z]{2,3}"))){ tmp <- tmp[-9][-9]}
    writeLines(tmp, file.path(dir_prep, "prep", dirname(prep_folder), "README.md"))
  }
}


#' compile readme information associated with functions defined in a script
#'
#' written to generate readme content for functions in bhi/R scripts, but could be used elsewhere...
#'
#' @param bhiR_dir file path to the directory containing the script of interest
#' @param script_name the name of the script with functions you want readme documentation for
#'
#' @return text for readme content is returned in the console, but output is also configured as a character vector

bhiRfun_readme <- function(bhiR_dir, script_name){

  funs_text <- scan(file = file.path(bhiR_dir, script_name), what = "character", sep = "\n")

  funs_names <- funs_text %>%
    grep(pattern = "^[a-z_]+.*function.*", value = TRUE) %>%
    stringr::str_extract("^\\S+") %>%
    stringr::str_pad(width = str_length(.)+4, side = "both", pad = "*")
  funs_info <- funs_text %>%
    grep(pattern = "^#'\\s", value = TRUE) %>%
    stringr::str_remove_all("#' ")
  sep <- c(0, which(stringr::str_detect(funs_info, pattern = "@return|@return.*\n\n"))) # last roxygen element assumed @return... if have anything else after...

  out <- vector()
  if(length(sep) == length(funs_names)+1 & length(sep) > 1){

    for(i in 1:length(funs_names)){
      funs_doc <- c(funs_names[i], funs_info[(sep[i]+1):(sep[i+1])], "<br/>")
      cat(funs_doc, sep = " <br/> \n")
      out <- c(out, paste0(funs_doc, sep = " <br/>"))
    }
    return(out)

  } else { print("cannot parse... check script for missing roxygen documentation") }
}


#' generate basic readme outline
#'
#' look within a folder and create structure dependent on content and file tree
#' if has subfolders...
#' result not to be the end all be all, just a starting point or rough outline to start from
#' will still have to actually open and manually edit some fields, but readme_content function will help with that
#' describe_objects could be: file, table, folder, function, script
#'
#' could use `sink` function to write readme outline output directly to a specified readme file
#'
#' @param folder_filepath file path to folder where README will be located and which contains objects to document
#' @param type_objects character string with type of thing to document in the readme: folders, functions, files, tables, or scripts
#'
#' @return text for readme outline is printed to the console, and can be copied from there or sunk to a file

readme_outline <- function(folder_filepath, type_objects = "files", delim = ","){

  ## setup and extract file tree
  S <- .Platform$file.sep
  title <- basename(folder_filepath)

  tree <- list.files(folder_filepath, recursive = TRUE, full.names = TRUE) %>%
    gsub(pattern = folder_filepath, replacement = "")
  tree <- tree[tree != paste0(S, "README.md")]

  obj_info <- list()
  subtitles <- NULL
  general <- paste0(c("Created on:",
                      "Last modified on:",
                      "Used by or referenced in functions/scripts: <br/>\n\n"),
                    collapse = " <br/>\n") ## can change and include more/different fields later...

  ## if we want subfolders listed (with summary descriptions for each)
  if(type_objects == "folders"){
    obj_names <- tree[stringr::str_count(tree, pattern = S) >= 2] %>%
      stringr::str_extract(paste0("^", S, "[a-z0-9_]+", S)) %>%
      gsub(pattern = S, replacement = "") %>%
      unique() # non-empty immediately-adjacent subdirectories
    for(n in obj_names){
      obj_info[[n]] <- "add short description here"
    }
  }
  ## if we want all functions within a set of R scripts listed (with summary of what each does)
  if(type_objects == "functions"){
    subtitles <- tree[stringr::str_detect(tree, pattern = "\\.R")] %>%
      gsub(pattern = S, replacement = "")
    general <- vector() # for functions get rid of 'general' fields...
    for(s in subtitles){
      obj_names <- c(scan(file = file.path(folder_filepath, s),
                          what = "character", sep = "\n") %>%
                       grep(pattern = "^[a-z_]+.*function.*", value = TRUE) %>%
                       stringr::str_extract("^\\S+"))
      obj_info[[s]][["obj_names"]] <- list(obj_names)
    }
  }
  ## if we want each file listed (with summary of purpose and description of its contents)
  if(type_objects %in% c("files", "tables", "scripts")){
    tree <- tree[stringr::str_count(tree, pattern = S) < 2] # eliminate files in subdirectories
    obj_names <- basename(tree)[basename(tree) != "README.md"] %>%
      gsub(pattern = S, replacement = "")
    for(n in obj_names){
      obj_info[[n]] <- "add a short description here"
    }

    if(type_objects == "tables"){
      obj_names <- obj_names[grep(".csv$|.shp$", obj_names)]
      obj_info <- list() # ignore bit above about including description in obj_info...

      for(j in obj_names){
        if(stringr::str_detect(j, "\\.csv$")){ # for csv files
          if(delim == ","){
            tmp <- read.csv(file.path(folder_filepath, j), stringsAsFactors = FALSE)
          } else {
            tmp <- readr::read_delim(file.path(folder_filepath, j), delim = delim) %>%
              as.data.frame()
          }
        }
        if(stringr::str_detect(j, "\\.shp$")){ # for shapefiles; requires sf package
          tmp <- sf::st_read(folder_filepath, substr(j, 1, nchar(j)-4))
          st_geometry(tmp) <- NULL # get just table; coerces to dataframe
        }
        nms <- names(tmp) # names of columns in table
        cla <- vector() # classes of each attribute or column
        lvl <- vector() # levels or categories (or a place for descriptions)
        for(k in 1:ncol(tmp)){
          lvls <- ifelse(length(unique(tmp[, k])) < 10 & # these won't work if tmp is a tibble
                                 all(str_length(unique(tmp[, 1])) < 10),
                         paste(unique(tmp[, k]), collapse = ", "), "")
          cla <- c(cla, paste0("* ", nms[k], ": ", class(tmp[, k])))
          lvl <- c(lvl, paste0("* ", nms[k], ": ", paste(lvls, collapse = ", ")))
        }
        obj_info[[j]][["class"]] <- cla
        obj_info[[j]][["levels"]] <- lvl
      }
    }
  }
  ## put everything in order and print out
  t <- paste0("# `", title, "`") # title
  cat(t, "\n\n<br/>\n\n")
  out <- vector()
  for(i in names(obj_info)){
    line <- unlist(obj_info[[i]])
    if(i %in% subtitles){
      subt <- paste0("### ",
                    i %>% stringr::str_pad(width = str_length(.) + 2,
                                            side = "both", pad = "`"))
      line <- line %>% stringr::str_pad(width = str_length(.) + 4, side = "both", pad = "*")
    } else {
      subt <- paste0(i %>% stringr::str_pad(width = str_length(.) + 2,
                                    side = "both", pad = "`") %>%
                       stringr::str_pad(width = str_length(.) + 4,
                                        side = "both", pad = "*"))
    }
    cat(subt, "\n\n")
    cat(general)
    if(type_objects == "tables"){
      cat(paste0(line, collapse = " <br/>\n"),
          ifelse(length(line) == 0, "\n", " <br/>\n\n<br/>\n\n"))
    } else {
      cat(paste0(line, collapse = " <br/>\n\n<br/>\n\n"),
          ifelse(length(line) == 0, "<br/>\n\n", "<br/>\n\n<br/>\n\n"))
    }
    out <- c(out, subt, general, line)
  }
  out <- c(t, out)
  return(out)
}



make_layer_summaries <- function(){
  
  ## layers information
  layers0 <- readr::read_csv(file.path(dir_assess, "index", "layers.csv"))
  lyr_csv <- layers0 %>%
    filter(name != "proxy_layer") %>% 
    select(targets, name, layer, description, units, filename) %>% 
    mutate(description = ifelse(is.na(description), "See goal description above or data prep documents for more information.", description)) %>% 
    mutate(description = str_remove_all(description, pattern = "\\n\\n\\n")) %>% 
    mutate(
      name_abbrev = name %>% 
        str_replace_all(pattern = "\\(", replacement = "openparentheses ") %>% 
        str_replace_all(pattern = "\\.", replacement = " periodpunctuation") %>% 
        str_replace_all(pattern = "to be used", replacement = "tobeused") %>% 
        str_replace_all(pattern = "based on|based upon", replacement = "basedon") %>% 
        str_replace_all(pattern = "of the European Parliament", replacement = "oftheEuropeanParliament")
    ) %>%
    rowwise() %>% 
    mutate(
      name_abbrev = ifelse(
        str_length(name) <= 45,
        name,
        str_split(
          name_abbrev,
          pattern = paste0(
            "\\s", 
            intersect(
              c("oftheEuropeanParliament", "openparentheses", "periodpunctuation", ",", "basedon", "tobeused", "as", "of"),
              str_split(name_abbrev, pattern = " ") %>% unlist())[1], "\\s"
          )
        )[[1]][1]
      )
    ) %>%
    mutate(
      name_abbrev = name_abbrev %>% 
        str_replace_all(pattern = " periodpunctuation", replacement = "\\.") %>% 
        str_replace_all(pattern = " basedon", replacement = " based on")
    )
  
  ## layer summaries templates text
  filetxt <- scan(
    file.path(dir_prep, "supplement", "templates", "layer_summaries_template.md"),
    what = "character",
    sep = "\n",
    blank.lines.skip = FALSE
  )
  
  bhiRmd_txt <- c(
    "### LAYERFULLNAME",
    "",
    "```{r, echo = FALSE, results = \"hide\"}",
    "tmp <- tempfile(fileext = \"Rmd\")",
    "on.exit(unlink(tmp))",
    "download.file(file.path(short_layer_web, \"LAYERNAME.Rmd\"), tmp)", 
    "```",
    "",
    "```{r, child = tmp, echo = FALSE, results = \"asis\"}", 
    "```",
    "",
    "---",
    ""
  )
  
  ## create docs
  lyrs_names <- lyr_csv$layer %>% sort()
  for(lyr in lyrs_names){
    make_file <- file.path(here::here(), "supplement", "layer_summaries", paste(lyr, "Rmd", sep = "."))
    if(file.exists(make_file)){file.remove(make_file)}
    file.create(make_file)
    
    lyr_txt <- filetxt
    
    ## layername
    lyr_txt[grep(x = lyr_txt, pattern = "LAYERNAME")] <- lyr_txt[grep(
      x = lyr_txt, 
      pattern = "LAYERNAME"
    )] %>% str_replace_all(pattern = "LAYERNAME", lyr)
    ## layer filename
    lyr_txt[grep(x = lyr_txt, pattern = "LAYERFILENAME")] <- lyr_txt[grep(
      x = lyr_txt, 
      pattern = "LAYERFILENAME"
    )] %>% str_replace_all(pattern = "LAYERFILENAME", filter(lyr_csv, layer == lyr)$filename)
    ## units
    lyr_txt[grep(x = lyr_txt, pattern = "UNITS")] <- lyr_txt[grep(
      x = lyr_txt, 
      pattern = "UNITS"
    )] %>% str_replace_all(pattern = "UNITS", filter(lyr_csv, layer == lyr)$units)
    ## index dimension
    lyr_txt[grep(x = lyr_txt, pattern = "INDEXDIMENSION")] <- lyr_txt[grep(
      x = lyr_txt, 
      pattern = "INDEXDIMENSION"
    )] %>% str_replace_all(
      pattern = "INDEXDIMENSION", 
      replacement = ifelse(
        "pressures" %in% filter(lyr_csv, layer == lyr)$targets %>% str_split(pattern = " ") %>% unlist(),
        "Pressure",
        ifelse(
          "resilience" %in% filter(lyr_csv, layer == lyr)$targets %>% str_split(pattern = " ") %>% unlist(),
          "Resilience",
          "Status and Trend"
        )
      )
    )
    ## goal targets
    lyr_txt[grep(x = lyr_txt, pattern = "GOALTARGETS")] <- lyr_txt[grep(
      x = lyr_txt, 
      pattern = "GOALTARGETS"
    )] %>% 
      str_replace_all(
        pattern = "GOALTARGETS", 
        replacement = ifelse(
          is.na(filter(lyr_csv, layer == lyr)$targets),
          "This layer is no longer used but has not yet been removed from the BHI",
          ifelse(
            filter(lyr_csv, layer == lyr)$targets %in% c("pressures", "resilience"),
            filter(lyr_csv, layer == lyr)$targets %>% str_to_sentence(),
            filter(lyr_csv, layer == lyr)$targets
          )
        )
      )
    ## layer description
    lyr_txt[grep(x = lyr_txt, pattern = "DESCRIPTION")] <- lyr_txt[grep(
      x = lyr_txt, 
      pattern = "DESCRIPTION"
    )] %>% str_replace_all(pattern = "DESCRIPTION", filter(lyr_csv, layer == lyr)$description)
    ## goal prep link if relevant
    lyr_txt[grep(x = lyr_txt, pattern = "THISGOAL")] <- ifelse(
      !filter(lyr_csv, layer == lyr)$targets %in% 
        c("FP", "FIS", "MAR", "AO", "NP", "CS", "TR", "LE", "LIV", "ECO", "SP", "ICO", "LSP", "CW", "EUT", "TRA", "CON", "BD"),
      "", # could add something more for pressures or resilience docs here with more ifelses
      lyr_txt[grep(x = lyr_txt, pattern = "THISGOAL")] %>% 
        str_replace(
          pattern = "THISGOALPREP",
          replacement = paste(str_to_lower(filter(lyr_csv, layer == lyr)$targets), "prep", sep =  "_")
        ) %>% 
        str_replace_all(
          pattern = "THISGOAL",
          replacement = filter(lyr_csv, layer == lyr)$targets
        )
    )
    
    ## for layers.Rmd doc in the bhi repo...
    bhiRmd_lyrtxt <- bhiRmd_txt
    
    bhiRmd_lyrtxt[grep(x = bhiRmd_lyrtxt, pattern = "LAYERFULLNAME")] <- bhiRmd_lyrtxt[grep(
      x = bhiRmd_lyrtxt, 
      pattern = "LAYERFULLNAME"
    )] %>% str_replace_all(pattern = "LAYERFULLNAME", filter(lyr_csv, layer == lyr)$name_abbrev %>% str_to_title())
    bhiRmd_lyrtxt[grep(x = bhiRmd_lyrtxt, pattern = "LAYERNAME")] <- bhiRmd_lyrtxt[grep(
      x = bhiRmd_lyrtxt, 
      pattern = "LAYERNAME"
    )] %>% str_replace_all(pattern = "LAYERNAME", lyr)
    
    ## write the configured/layer-specific text to the rmd docs
    sink(file = make_file, append = FALSE)
    cat(lyr_txt, sep = "\n")
    closeAllConnections()
  }
}
