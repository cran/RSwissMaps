# FUNCTION TO GENERATE MUNICIPALITY TEMPLATES
#' Generating templates for municipality-level data of Switzerland
#'
#' \code{mun.template} offers the possbility to generate Swiss municipality-level data templates in
#'     the form of data frames. The package comes with data for \code{2016}.  Additional data can be
#'     downloaded with \code{map.load}.  The templates contain the municipality identification numbers
#'     as used by the Swiss Federal Statistical Office and the municipality names.
#'
#' @param year numeric. Available with package: \code{2016}. For additional years, see: \code{map.load}.
#' @param add_geodata_path a character string with the path to the folder where additional geodata is saved.
#' @param endofyear if \code{TRUE}, municipalities as by December 31 of \code{year} are used for the template.
#' @param cantons input either numeric (canton identification numbers) or character (canton abbreviations)
#'      vector to create canton-specific templates.
#' @param districts input either numeric (district identification numbers) or character (district names)
#'      vector to create district-specific templates.
#' @param municipalities input either numeric (municipality identification numbers) or character
#'     (municipality names) vector to create tailor-made templates.
#' @examples
#' # Generating template:
#' mun.template(2016)
#'
#' # Generating template for the municipalities of the canton of Aargau:
#' mun.template(2016, cantons = c("AG", "ZH"))
#' @export
mun.template <- function(year, add_geodata_path = ".", endofyear = FALSE, cantons = NULL, districts = NULL, municipalities = NULL){

  # Checking YEAR and ENDOFYEAR ------------------------
  if(missing(year)) stop("year missing")
  if(!is.numeric(year)) stop("year is not numeric")
  if(length(year) > 1) stop("please choose a year")

  # Loading map data based on YEAR and ENDOFYEAR ------------------------
  if(!is.logical(endofyear)) stop("endofyear: TRUE or FALSE needed")
  if(!is.numeric(year)) stop("year: numeric input needed")
  if(year<2001|year>2017) stop("year: no data available")

  mapCH2017 = mapCH2015 = mapCH2014 = mapCH2013 = mapCH2012 = mapCH2011 = mapCH2010 = mapCH2009 =
    mapCH2008 = mapCH2007 = mapCH2006 = mapCH2005 = mapCH2004 = mapCH2003 = mapCH2002 = mapCH2001 = NULL

  if(year == 2016) dt <- RSwissMaps::mapCH2016
  if(year != 2016){

    files <- list.files(add_geodata_path)

    if(paste0("mapCH", year, ".rda") %in% files){

      if(substr(add_geodata_path, nchar(add_geodata_path), nchar(add_geodata_path)) != "/"){

        load(paste0(add_geodata_path, "/mapCH", year, ".rda"))

      } else {

        load(paste0(add_geodata_path, "mapCH", year, ".rda"))

      }

      if(year == 2017){

        dt <- mapCH2017
        rm(mapCH2017)

      }
      if(year == 2015){

        dt <- mapCH2015
        rm(mapCH2015)

      }
      if(year == 2014){

        dt <- mapCH2014
        rm(mapCH2014)

      }
      if(year == 2013){

        dt <- mapCH2013
        rm(mapCH2013)

      }
      if(year == 2012){

        dt <- mapCH2012
        rm(mapCH2012)

      }
      if(year == 2011){

        dt <- mapCH2011
        rm(mapCH2011)

      }
      if(year == 2010){

        dt <- mapCH2010
        rm(mapCH2010)

      }
      if(year == 2009){

        dt <- mapCH2009
        rm(mapCH2009)

      }
      if(year == 2008){

        dt <- mapCH2008
        rm(mapCH2008)

      }
      if(year == 2007){

        dt <- mapCH2007
        rm(mapCH2007)

      }
      if(year == 2006){

        dt <- mapCH2006
        rm(mapCH2006)

      }
      if(year == 2005){

        dt <- mapCH2005
        rm(mapCH2005)

      }
      if(year == 2004){

        dt <- mapCH2004
        rm(mapCH2004)

      }
      if(year == 2003){

        dt <- mapCH2003
        rm(mapCH2003)

      }
      if(year == 2002){

        dt <- mapCH2002
        rm(mapCH2002)

      }
      if(year == 2001){

        dt <- mapCH2001
        rm(mapCH2001)

      }

      rm(files, add_geodata_path)

    } else {

      stop(paste0("no geodata found for ", year, ": download geodata with map.load(", year, ") or correct 'add_geodata_path'"))

    }
  }

  if(endofyear == TRUE){

    data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==1,]
    data_districts <- dt[dt$year==year&dt$type=="district"&dt$endofyear==1,]
    data_municipalities <- dt[dt$year==year&dt$type=="municipality"&dt$endofyear==1,]

    if(nrow(data_municipalities)==0){

      data_municipalities <- dt[dt$year==year&dt$type=="municipality"&dt$endofyear==0,]
      message(paste0("no data found for endofyear = TRUE; automatically applied endofyear = FALSE; map dates from 01.01.", year))

    }

    if(nrow(data_cantons)==0) data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]
    if(nrow(data_districts)==0) data_districts <- dt[dt$year==year&dt$type=="district"&dt$endofyear==0,]


  } else {

    data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]
    data_districts <- dt[dt$year==year&dt$type=="district"&dt$endofyear==0,]
    data_municipalities <- dt[dt$year==year&dt$type=="municipality"&dt$endofyear==0,]

  }

  # Apply CANTONS on data ------------------------
  dt_c <- NULL

  if(!missing(cantons)){

    cantons_name <- unique(as.character(data_cantons$name))
    cantons_id <- unique(as.numeric(data_cantons$id))

    if(!is.numeric(cantons)){

      # checking non-numeric input
      wrong_cantons <- setdiff(tolower(cantons), tolower(cantons_name))
      if(length(wrong_cantons) > 0) stop(paste0("unknown canton id: ", wrong_cantons, collapse = ", "))

      # building data
      for(i in 1:length(cantons_name)){
        if(length(intersect(tolower(cantons_name[i]), tolower(cantons))) > 0) dt_c <- rbind.data.frame(dt_c, data_municipalities[data_municipalities$can==cantons_id[i],])
      }

    }
    if(is.numeric(cantons)){

      # checking numeric input
      wrong_cantons <- setdiff(cantons, cantons_id)
      if(length(wrong_cantons) > 0) stop(paste0("unknown canton id: ", wrong_cantons, collapse = ", "))

      # building data
      for(i in 1:length(cantons_id)){
        if(length(intersect(cantons_id[i], cantons)) > 0) dt_c <- rbind.data.frame(dt_c, data_municipalities[data_municipalities$can==cantons_id[i],])

      }

    }

    rm(cantons_name, cantons_id, wrong_cantons, data_cantons)

  }

  # Apply DISTRICTS on data ------------------------
  dt_d <- NULL

  if(!missing(districts)){

    districts_name <- unique(as.character(data_districts$name))
    districts_id <- unique(as.numeric(data_districts$id))

    if(!is.numeric(districts)){

      # checking non-numeric input
      wrong_districts <- setdiff(tolower(districts), tolower(districts_name))
      if(length(wrong_districts) > 1) stop(paste0("unknown district: ", wrong_districts, collapse = ", "))

      # building data
      for(i in 1:length(districts_name)){
        if(length(intersect(tolower(districts_name[i]), tolower(districts))) > 0) dt_d <- rbind.data.frame(dt_d, data_municipalities[data_municipalities$dis==districts_id[i],])
      }

    }
    if(is.numeric(districts)){

      # checking numeric input
      wrong_districts <- setdiff(districts, districts_id)
      if(length(wrong_districts) > 1) stop(paste0("unknown district: ", wrong_districts, collapse = ", "))

      # building data
      for(i in 1:length(districts_id)){
        if(length(intersect(districts_id[i], districts)) > 0) dt_d <- rbind.data.frame(dt_d, data_municipalities[data_municipalities$dis==districts_id[i],])

      }

    }

    rm(districts_name, districts_id, wrong_districts, data_districts)

  }
  # Apply MUNICIPALITY on data ------------------------
  dt_m <- NULL

  if(!missing(municipalities)){

    municipalities_name <- unique(data_municipalities$name)
    municipalities_id <- unique(data_municipalities$id)

    if(!is.numeric(municipalities)){

      # checking non-numeric input
      wrong_municipalities <- setdiff(tolower(municipalities), tolower(municipalities_name))
      if(length(wrong_municipalities) == 1) stop(paste0("unknown municipality: ", wrong_municipalities, collapse = ", "))

      # building data
      for(i in 1:length(municipalities_name)){
        if(length(intersect(tolower(municipalities_name[i]), tolower(municipalities))) > 0) dt_m <- rbind.data.frame(dt_m, data_municipalities[data_municipalities$id==municipalities_id[i],])
      }

    }
    if(is.numeric(municipalities)){

      # checking numeric input
      wrong_municipalities <- setdiff(municipalities, municipalities_id)
      if(length(wrong_municipalities) == 1) stop(paste0("unknown municipality: ", wrong_municipalities, collapse = ", "))

      # building data
      for(i in 1:length(municipalities_id)){
        if(length(intersect(municipalities_id[i], municipalities)) > 0) dt_m <- rbind.data.frame(dt_m, data_municipalities[data_municipalities$id==municipalities_id[i],])

      }

    }

    rm(municipalities_name, municipalities_id, wrong_municipalities, data_municipalities)

  }

  # Joining CANTONS, DISTRICTS and MUNICIPALITY ------------------------
  if(!is.null(dt_c) | !is.null(dt_d) | !is.null(dt_m)){

    bfs_nr <- c(dt_c$id, dt_d$id, dt_m$id)
    name <- c(as.character(dt_c$name), as.character(dt_d$name), as.character(dt_m$name))
    values <- NA
    dt <- cbind.data.frame(bfs_nr, name, values)
    dt <- dt[!duplicated(dt$bfs_nr),]

  } else {

    bfs_nr <- data_municipalities$id
    name <- as.character(data_municipalities$name)
    values <- NA
    dt <- cbind.data.frame(bfs_nr, name, values)
    dt <- dt[!duplicated(dt$bfs_nr),]

  }

  dt <- dt[order(dt$bfs_nr),]
  row.names(dt) <- NULL
  rm(dt_c, dt_d, dt_m, bfs_nr, name, values)

  dt
}

# FUNCTION TO GENERATE DISTRICT TEMPLATES
#' Generating templates for district-level data of Switzerland
#'
#' \code{dis.template} offers the possbility to generate Swiss district-level data templates in
#'     the form of data frames. The package comes with data for \code{2016}.  Additional data can be
#'     downloaded with \code{map.load}.  The templates contain the district identification numbers
#'     as used by the Swiss Federal Statistical Office and the district names.
#'
#' @param year numeric. Available with package: \code{2016}. For additional years, see: \code{map.load}.
#' @param add_geodata_path a character string with the path to the folder where additional geodata is saved.
#' @param endofyear if \code{TRUE}, districts as by December 31 of \code{year} are used for the template.
#' @param cantons input either numeric (canton identification numbers) or character (canton abbreviations)
#'      vector to create canton-specific templates.
#' @param districts input either numeric (district identification numbers) or character (district names)
#'      vector to create tailor-made templates.
#' @examples
#' # Generating template:
#' dis.template(2016)
#'
#' # Generating template for the districts of the canton of Aargau:
#' dis.template(2016, cantons = c("AG", "ZH"))
#' @export
dis.template <- function(year, add_geodata_path = ".", endofyear = FALSE, cantons = NULL, districts = NULL){

  # Checking YEAR and ENDOFYEAR ------------------------
  if(missing(year)) stop("year missing")
  if(!is.numeric(year)) stop("year is not numeric")
  if(length(year) > 1) stop("please choose a year")
  if(missing(endofyear)) endofyear <- FALSE
  if(!is.logical(endofyear)) stop("endofyear: TRUE or FALSE needed")

  # Loading map data based on YEAR and ENDOFYEAR ------------------------
  if(!is.logical(endofyear)) stop("endofyear: TRUE or FALSE needed")
  if(!is.numeric(year)) stop("year: numeric input needed")
  if(year<2001|year>2017) stop("year: no data available")

  mapCH2017 = mapCH2015 = mapCH2014 = mapCH2013 = mapCH2012 = mapCH2011 = mapCH2010 = mapCH2009 =
    mapCH2008 = mapCH2007 = mapCH2006 = mapCH2005 = mapCH2004 = mapCH2003 = mapCH2002 = mapCH2001 = NULL

  if(year == 2016) dt <- RSwissMaps::mapCH2016
  if(year != 2016){

    files <- list.files(add_geodata_path)

    if(paste0("mapCH", year, ".rda") %in% files){

      if(substr(add_geodata_path, nchar(add_geodata_path), nchar(add_geodata_path)) != "/"){

        load(paste0(add_geodata_path, "/mapCH", year, ".rda"))

      } else {

        load(paste0(add_geodata_path, "mapCH", year, ".rda"))

      }

      if(year == 2017){

        dt <- mapCH2017
        rm(mapCH2017)

      }
      if(year == 2015){

        dt <- mapCH2015
        rm(mapCH2015)

      }
      if(year == 2014){

        dt <- mapCH2014
        rm(mapCH2014)

      }
      if(year == 2013){

        dt <- mapCH2013
        rm(mapCH2013)

      }
      if(year == 2012){

        dt <- mapCH2012
        rm(mapCH2012)

      }
      if(year == 2011){

        dt <- mapCH2011
        rm(mapCH2011)

      }
      if(year == 2010){

        dt <- mapCH2010
        rm(mapCH2010)

      }
      if(year == 2009){

        dt <- mapCH2009
        rm(mapCH2009)

      }
      if(year == 2008){

        dt <- mapCH2008
        rm(mapCH2008)

      }
      if(year == 2007){

        dt <- mapCH2007
        rm(mapCH2007)

      }
      if(year == 2006){

        dt <- mapCH2006
        rm(mapCH2006)

      }
      if(year == 2005){

        dt <- mapCH2005
        rm(mapCH2005)

      }
      if(year == 2004){

        dt <- mapCH2004
        rm(mapCH2004)

      }
      if(year == 2003){

        dt <- mapCH2003
        rm(mapCH2003)

      }
      if(year == 2002){

        dt <- mapCH2002
        rm(mapCH2002)

      }
      if(year == 2001){

        dt <- mapCH2001
        rm(mapCH2001)

      }

      rm(files, add_geodata_path)

    } else {

      stop(paste0("no geodata found for ", year, ": download geodata with map.load(", year, ") or correct 'add_geodata_path'"))

    }
  }


  if(endofyear == TRUE){

    data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==1,]
    data_districts <- dt[dt$year==year&dt$type=="district"&dt$endofyear==1,]

    if(nrow(data_districts)==0){

      data_districts <- dt[dt$year==year&dt$type=="district"&dt$endofyear==0,]
      message(paste0("no data found for endofyear = TRUE; automatically applied endofyear = FALSE; map dates from 01.01.", year))

    }

    if(nrow(data_cantons)==0) data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]

  } else {

    data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]
    data_districts <- dt[dt$year==year&dt$type=="district"&dt$endofyear==0,]

  }

  # Apply CANTONS on data ------------------------
  dt_c <- NULL

  if(!missing(cantons)){

    cantons_name <- unique(data_cantons$name)
    cantons_id <- unique(data_cantons$id)

    if(!is.numeric(cantons)){

      # checking non-numeric input
      wrong_cantons <- setdiff(tolower(cantons), tolower(cantons_name))
      if(length(wrong_cantons) > 0) stop(paste0("unknown canton id: ", wrong_cantons, collapse = ", "))

      # building data
      for(i in 1:length(cantons_name)){
        if(length(intersect(tolower(cantons_name[i]), tolower(cantons))) > 0) dt_c <- rbind.data.frame(dt_c, data_districts[data_districts$can==cantons_id[i],])
      }

    }
    if(is.numeric(cantons)){

      # checking numeric input
      wrong_cantons <- setdiff(cantons, cantons_id)
      if(length(wrong_cantons) > 0) stop(paste0("unknown canton id: ", wrong_cantons, collapse = ", "))

      # building data
      for(i in 1:length(cantons_id)){
        if(length(intersect(cantons_id[i], cantons)) > 0) dt_c <- rbind.data.frame(dt_c, data_districts[data_districts$can==cantons_id[i],])

      }

    }

    rm(cantons_name, cantons_id, wrong_cantons, data_cantons)

  }

  # Apply DISTRICTS on data ------------------------
  dt_d <- NULL

  if(!missing(districts)){

    districts_name <- unique(data_districts$name)
    districts_id <- unique(data_districts$id)

    if(!is.numeric(districts)){

      # checking non-numeric input
      wrong_districts <- setdiff(tolower(districts), tolower(districts_name))
      if(length(wrong_districts) > 1) stop(paste0("unknown district: ", wrong_districts, collapse = ", "))

      # building data
      for(i in 1:length(districts_name)){
        if(length(intersect(tolower(districts_name[i]), tolower(districts))) > 0) dt_d <- rbind.data.frame(dt_d, data_districts[data_districts$id==districts_id[i],])
      }

    }
    if(is.numeric(districts)){

      # checking numeric input
      wrong_districts <- setdiff(districts, districts_id)
      if(length(wrong_districts) > 1) stop(paste0("unknown district: ", wrong_districts, collapse = ", "))

      # building data
      for(i in 1:length(districts_id)){
        if(length(intersect(districts_id[i], districts)) > 0) dt_d <- rbind.data.frame(dt_d, data_districts[data_districts$id==districts_id[i],])

      }

    }

    rm(districts_name, districts_id, wrong_districts, data_districts)

  }
  # Joining CANTONS, DISTRICTS and MUNICIPALITY ------------------------
  if(!is.null(dt_c) | !is.null(dt_d)){

    bfs_nr <- c(dt_c$id, dt_d$id)
    name <- c(as.character(dt_c$name), as.character(dt_d$name))
    values <- NA
    dt <- cbind.data.frame(bfs_nr, name, values)
    dt <- dt[!duplicated(dt$bfs_nr),]

  } else {

    bfs_nr <- data_districts$id
    name <- as.character(data_districts$name)
    values <- NA
    dt <- cbind.data.frame(bfs_nr, name, values)
    dt <- dt[!duplicated(dt$bfs_nr),]

  }

  dt <- dt[order(dt$bfs_nr),]
  row.names(dt) <- NULL
  rm(dt_c, dt_d, bfs_nr, name, values)

  dt

}

# FUNCTION TO GENERATE CANTON TEMPLATES
#' Generating templates for canton-level data of Switzerland
#'
#' \code{can.template} offers the possbility to generate Swiss canton-level data templates in
#'     the form of data frames. The package comes with data for \code{2016}.  Additional data can be
#'     downloaded with \code{map.load}.  The templates contain the canton identification numbers
#'     as used by the Swiss Federal Statistical Office and the canton abbreviations.
#'
#' @param year numeric. Available with package: \code{2016}. For additional years, see: \code{map.load}.
#' @param add_geodata_path a character string with the path to the folder where additional geodata is saved.
#' @param endofyear if \code{TRUE}, cantons as by December 31 of \code{year} are used for the template.
#' @param cantons input either numeric (canton identification numbers) or character (canton abbreviations)
#'     vector to create tailor-made templates.
#' @examples
#' # Generating template:
#' can.template(2016)
#' @export
can.template <- function(year, add_geodata_path = ".", endofyear = FALSE, cantons = NULL){

  # Checking YEAR ------------------------
  if(missing(year)) stop("year missing")
  if(!is.numeric(year)) stop("year is not numeric")
  if(length(year) > 1) stop("please choose a year")
  if(missing(endofyear)) endofyear <- FALSE
  if(!is.logical(endofyear)) stop("endofyear: TRUE or FALSE needed")

  # Loading map data based on YEAR and ENDOFYEAR ------------------------
  if(!is.logical(endofyear)) stop("endofyear: TRUE or FALSE needed")
  if(!is.numeric(year)) stop("year: numeric input needed")
  if(year<2001|year>2017) stop("year: no data available")

  mapCH2017 = mapCH2015 = mapCH2014 = mapCH2013 = mapCH2012 = mapCH2011 = mapCH2010 = mapCH2009 =
    mapCH2008 = mapCH2007 = mapCH2006 = mapCH2005 = mapCH2004 = mapCH2003 = mapCH2002 = mapCH2001 = NULL

  if(year == 2016) dt <- RSwissMaps::mapCH2016
  if(year != 2016){

    files <- list.files(add_geodata_path)

    if(paste0("mapCH", year, ".rda") %in% files){

      if(substr(add_geodata_path, nchar(add_geodata_path), nchar(add_geodata_path)) != "/"){

        load(paste0(add_geodata_path, "/mapCH", year, ".rda"))

      } else {

        load(paste0(add_geodata_path, "mapCH", year, ".rda"))

      }

      if(year == 2017){

        dt <- mapCH2017
        rm(mapCH2017)

      }
      if(year == 2015){

        dt <- mapCH2015
        rm(mapCH2015)

      }
      if(year == 2014){

        dt <- mapCH2014
        rm(mapCH2014)

      }
      if(year == 2013){

        dt <- mapCH2013
        rm(mapCH2013)

      }
      if(year == 2012){

        dt <- mapCH2012
        rm(mapCH2012)

      }
      if(year == 2011){

        dt <- mapCH2011
        rm(mapCH2011)

      }
      if(year == 2010){

        dt <- mapCH2010
        rm(mapCH2010)

      }
      if(year == 2009){

        dt <- mapCH2009
        rm(mapCH2009)

      }
      if(year == 2008){

        dt <- mapCH2008
        rm(mapCH2008)

      }
      if(year == 2007){

        dt <- mapCH2007
        rm(mapCH2007)

      }
      if(year == 2006){

        dt <- mapCH2006
        rm(mapCH2006)

      }
      if(year == 2005){

        dt <- mapCH2005
        rm(mapCH2005)

      }
      if(year == 2004){

        dt <- mapCH2004
        rm(mapCH2004)

      }
      if(year == 2003){

        dt <- mapCH2003
        rm(mapCH2003)

      }
      if(year == 2002){

        dt <- mapCH2002
        rm(mapCH2002)

      }
      if(year == 2001){

        dt <- mapCH2001
        rm(mapCH2001)

      }

      rm(files, add_geodata_path)

    } else {

      stop(paste0("no geodata found for ", year, ": download geodata with map.load(", year, ") or correct 'add_geodata_path'"))

    }
  }

  if(endofyear == TRUE){

    data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==1,]

    if(nrow(data_cantons)==0){

      data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]
      message(paste0("no data found for endofyear = TRUE; automatically applied endofyear = FALSE; map dates from 01.01.", year))

    }

  } else {

    data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]

  }

  # Apply CANTONS on data ------------------------
  dt_c <- NULL

  if(!missing(cantons)){

    cantons_name <- unique(data_cantons$name)
    cantons_id <- unique(data_cantons$id)

    if(!is.numeric(cantons)){

      # checking non-numeric input
      wrong_cantons <- setdiff(tolower(cantons), tolower(cantons_name))
      if(length(wrong_cantons) > 0) stop(paste0("unknown canton id: ", wrong_cantons, collapse = ", "))

      # building data
      for(i in 1:length(cantons_name)){
        if(length(intersect(tolower(cantons_name[i]), tolower(cantons))) > 0) dt_c <- rbind.data.frame(dt_c, data_cantons[data_cantons$id==cantons_id[i],])
      }

    }
    if(is.numeric(cantons)){

      # checking numeric input
      wrong_cantons <- setdiff(cantons, cantons_id)
      if(length(wrong_cantons) > 0) stop(paste0("unknown canton id: ", wrong_cantons, collapse = ", "))

      # building data
      for(i in 1:length(cantons_id)){
        if(length(intersect(cantons_id[i], cantons)) > 0) dt_c <- rbind.data.frame(dt_c, data_cantons[data_cantons$id==cantons_id[i],])

      }

    }

    rm(cantons_name, cantons_id, wrong_cantons, data_cantons)

  }

  # Joining CANTONS, DISTRICTS and MUNICIPALITY ------------------------
  if(!is.null(dt_c)){

    bfs_nr <- c(dt_c$id)
    name <- c(as.character(dt_c$name))
    values <- NA
    dt <- cbind.data.frame(bfs_nr, name, values)
    dt <- dt[!duplicated(dt$bfs_nr),]

  } else {

    bfs_nr <- data_cantons$id
    name <- as.character(data_cantons$name)
    values <- NA
    dt <- cbind.data.frame(bfs_nr, name, values)
    dt <- dt[!duplicated(dt$bfs_nr),]

  }

  dt <- dt[order(dt$bfs_nr),]
  row.names(dt) <- NULL
  rm(dt_c, bfs_nr, name, values)

  dt

}
