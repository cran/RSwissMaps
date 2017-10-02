# DATA
#' Geodata of Swiss municipalities, districts, cantons and lakes (2016)
#'
#'  A ggplot2-compatible data frame containing geodata of Swiss municipalities, districts,
#'  cantons and lakes for 2016.  The data is made publicly available by the Swiss
#'  Federal Statistical Office.  The administrative divisions are scaled at 1:2,000,000.
#'
#' @format A data frame with 162,148 rows and 10 variables:
#' \describe{
#'   \item{year}{year of geodata}
#'   \item{endofyear}{if \code{0}, the data dates from \code{year}-1-1, else from \code{year}-31-12}
#'   \item{long}{longitude of point}
#'   \item{lat}{latitude of point}
#'   \item{group}{if two adjacent points are in the same group, then they get connected}
#'   \item{type}{type of object point belongs to: municipality, district, canton, country, lake)}
#'   \item{id}{official identification number of object}
#'   \item{name}{name of object}
#'   \item{dis}{district object belongs to}
#'   \item{can}{canton object belongs to}}
#'
#' @name mapCH2016
#' @docType data
#' @author David Zumbach \email{david.zumbach@gfzb.ch}
#' @source Swiss Federal Statistical Office, GEOSTAT (last download 2017-09-07)
#' @references \url{https://www.bfs.admin.ch/bfs/de/home/dienstleistungen/geostat/geodaten-bundesstatistik/administrative-grenzen/generalisierte-gemeindegrenzen.html}
"mapCH2016"

# FUNCTION TO PLOT MUNICIPALITIES
#' Plotting municipality-level maps of Switzerland
#'
#' \code{mun.plot} offers the possbility to visualise Swiss municipality-level data on a map.
#'     It uses the geodata that is made publicly available by the
#'     \href{https://www.bfs.admin.ch/bfs/en/home.html}{Swiss Federal Statistical Office}.  The
#'     package comes with geodata for \code{2016}.  Additional geodata can be downloaded with
#'     \code{map.load}.  The arguments \code{cantons}, \code{districts} and \code{municipalities}
#'     make it easy to plot customised maps. The function \code{\link{mun.template}} provides templates
#'     for data collection.
#'
#' @param bfs_id a numeric vector with municipality identification numbers as used by the Swiss Federal
#'     Statistical Office corresponding to \code{data}. For templates, see \code{\link{mun.template}}.
#' @param data a vector of data at municipality-level.
#' @param year numeric. Indicates what map version to plot. Available with package: \code{2016}. For additional
#'     geodata, see: \code{map.load}.
#' @param add_geodata_path a character string with the path to the folder where additional geodata is saved.
#' @param endofyear if \code{TRUE}, administrative boundaries as by December 31 of \code{year} are plotted.
#' @param cantons input either numeric (canton identification numbers) or character (canton abbreviations)
#'     vector to plot municipalities of selected cantons.
#' @param districts input either numeric (district identification numbers) or character (district names)
#'     vector to plot municipalities of selected districts.
#' @param municipalities input either numeric (municipality identification numbers) or character (municipality
#'     names) vector to plot selected municipalities.
#' @param lakes input either numeric (lake identification numbers) or character (lakes names) vector to plot
#'     selected lakes.  If \code{"none"}, no lakes are plotted. If \code{"all"}, all available lakes are plotted.
#' @param boundaries character vector naming the boundaries to map.\itemize{
#'   \item "m": municipality boundaries (default)
#'   \item "d": district boundaries
#'   \item "c": canton boundaries
#'   \item "n": national boundaries}
#' @param boundaries_size numeric vector that specifies the size of the corresponding element in \code{boundaries}.
#' @param boundaries_color character vector that specifies the color of the corresponding element in \code{boundaries}.
#' @param extrema if \code{local}, data of municipalities that are not selected by \code{cantons}, \code{districts},
#'     or \code{municipalities} is not considered in the color scaling process.
#' @param continuous set to \code{FALSE}, if \code{data} is non-continuous.
#' @param color_continuous character vector with 2 elements to specifiy the plot color if \code{continuous = TRUE}.
#'     \code{color_continuous = c(low, high)} resulting in a two color gradient from color \code{low} to color \code{high}.
#' @param color_discrete character vector with 2 elements to specifiy the plot color if \code{continuous = FALSE}.
#'     \code{color_discrete = c(type, palette)}
#'    resulting in a color scheme from \href{http://colorbrewer2.org}{ColorBrewer} that are particularly well suited
#'    to display discrete values on a map.\itemize{
#'   \item \code{type}: one of seq (sequential), div (diverging) or qual (qualitative)
#'   \item \code{palette}: index into the list of palettes of appropriate \code{type}; character needed!}
#' @param color_na color of missing values.
#' @param title,subtitle,caption,legend_title text for titles or caption below the plot.
#' @param legend_position the position of the legend ("none", "left", "right", "bottom", "top")
#' @param save if \code{TRUE}, the plot will be saved to disk.
#' @param filename file name to create on disk incl. image file format (".jpeg", ".png", ".tiff", etc.).
#' @param dpi plot resolution. Applies only to raster output types.
#' @param width,heigth,units plot size in units ("in", "cm", or "mm"). If not supplied, uses the size of current
#'     graphics device.
#' @examples
#'  # Generating sample data:
#'  dt <- mun.template(2016)
#'  for(i in 1:nrow(dt)){dt$values[i] <- sample(c(300:700), 1)/1000}
#'
#'  # Plotting sample data:
#'  mun.plot(dt$bfs_nr, dt$values, 2016)
#'
#'  # Plotting sample data for the canton of Aargau:
#'  mun.plot(dt$bfs_nr, dt$values, 2016, cantons = c("AG"),
#'  lakes = c("Hallwilersee"))
#' @export
mun.plot <- function(bfs_id, data, year, add_geodata_path = ".", endofyear = FALSE,
                     cantons = NULL, districts = NULL, municipalities = NULL, lakes = "all",
                     boundaries = "m", boundaries_size = 0.2, boundaries_color = "white",
                     extrema = "global", continuous = TRUE,
                     color_continuous = c("#fee5d9", "#a50f15"), color_discrete = c("seq", "1"), color_na = "gray90",
                     title = NULL, subtitle = NULL, caption = NULL, legend_title = NULL, legend_position = "bottom",
                     save = FALSE, filename = "mun_plot.png", dpi = 300, width = NA, heigth = NA, units = "cm") {

  # Checking BFS_ID and data ------------------------
  if(missing(bfs_id)) stop("bfs_id is missing")
  if(missing(data)) stop("data is missing")
  if(length(bfs_id)!=length(data)) stop("inputs differ in length")

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

      data_map <- dt[dt$year==year&dt$type=="municipality"&dt$endofyear==1,]
      data_country <- dt[dt$year==year&dt$type=="country"&dt$endofyear==1,]
      data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==1,]
      data_districts <- dt[dt$year==year&dt$type=="district"&dt$endofyear==1,]
      data_municipalities <- dt[dt$year==year&dt$type=="municipality"&dt$endofyear==1,]
      data_lakes <- dt[dt$year==year&dt$type=="lake"&dt$endofyear==1,]

      if(nrow(data_map)==0){

        data_map <- dt[dt$year==year&dt$type=="municipality"&dt$endofyear==0,]
        message(paste0("no data found for endofyear = TRUE; automatically applied endofyear = FALSE; map dates from 01.01.", year))

      }

      if(nrow(data_country)==0) data_country <- dt[dt$year==year&dt$type=="country"&dt$endofyear==0,]
      if(nrow(data_cantons)==0) data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]
      if(nrow(data_districts)==0) data_districts <- dt[dt$year==year&dt$type=="district"&dt$endofyear==0,]
      if(nrow(data_municipalities)==0) data_municipalities <- dt[dt$year==year&dt$type=="municipality"&dt$endofyear==0,]
      if(nrow(data_lakes)==0) data_lakes <- dt[dt$year==year&dt$type=="lake"&dt$endofyear==0,]


      } else {

        data_map <- dt[dt$year==year&dt$type=="municipality"&dt$endofyear==0,]
        data_country <- dt[dt$year==year&dt$type=="country"&dt$endofyear==0,]
        data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]
        data_districts <- dt[dt$year==year&dt$type=="district"&dt$endofyear==0,]
        data_municipalities <- dt[dt$year==year&dt$type=="municipality"&dt$endofyear==0,]
        data_lakes <- dt[dt$year==year&dt$type=="lake"&dt$endofyear==0,]

      }

  # Merge input data with map data ------------------------
  data_bfs <- cbind.data.frame(bfs_id, data)
  names(data_bfs) <- c("id", "data")
  data_combined <-  dplyr::left_join(data_map, data_bfs, by = c("id" = "id"))

  # Apply CANTONS on pre-loaded map data ------------------------
  data_combined_c <- NULL
  districts_boundary <- NULL

  if(!is.null(cantons)){

    cantons_name <- unique(data_cantons$name)
    cantons_id <- unique(data_cantons$id)
    cantons_boundary <- NULL

    if(!is.numeric(cantons)){

      # checking non-numeric input
      wrong_cantons <- setdiff(tolower(cantons), tolower(cantons_name))
      if(length(wrong_cantons) > 0) stop(paste0("unknown canton: ", wrong_cantons, collapse = ", "))

      # building map data
      for(i in 1:length(cantons_name)){
        if(length(intersect(tolower(cantons_name[i]), tolower(cantons))) > 0) data_combined_c <- rbind.data.frame(data_combined_c, data_combined[data_combined$can==cantons_id[i],])
        if(length(intersect(tolower(cantons_name[i]), tolower(cantons))) > 0) cantons_boundary <- rbind.data.frame(cantons_boundary, data_cantons[data_cantons$id==cantons_id[i],])
        if(length(intersect(tolower(cantons_name[i]), tolower(cantons))) > 0) districts_boundary <- rbind.data.frame(districts_boundary, data_districts[data_districts$can==cantons_id[i],])

      }

    }
    if(is.numeric(cantons)){

      # checking numeric input
      wrong_cantons <- setdiff(cantons, cantons_id)
      if(length(wrong_cantons) > 0) stop(paste0("unknown canton id: ", wrong_cantons, collapse = ", "))

      # building map data
      for(i in 1:length(cantons_id)){
        if(length(intersect(cantons_id[i], cantons)) > 0) data_combined_c <- rbind.data.frame(data_combined_c, data_combined[data_combined$can==cantons_id[i],])
        if(length(intersect(cantons_id[i], cantons)) > 0) cantons_boundary <- rbind.data.frame(cantons_boundary, data_cantons[data_cantons$id==cantons_id[i],])
        if(length(intersect(cantons_id[i], cantons)) > 0) districts_boundary <- rbind.data.frame(districts_boundary, data_districts[data_districts$can==cantons_id[i],])

      }

    }

    rm(cantons_name, cantons_id)

  }
  # Apply DISTRICTS on pre-loaded map data ------------------------
  data_combined_d <- NULL

  if(!is.null(districts)){

    districts_name <- unique(data_districts$name)
    districts_id <- unique(data_districts$id)

    if(!is.numeric(districts)){

      # checking non-numeric input
      wrong_districts <- setdiff(tolower(districts), tolower(districts_name))
      if(length(wrong_districts) > 1) stop(paste0("unknown district: ", wrong_districts, collapse = ", "))

      # building map data
      for(i in 1:length(districts_name)){
        if(length(intersect(tolower(districts_name[i]), tolower(districts))) > 0) data_combined_d <- rbind.data.frame(data_combined_d, data_combined[data_combined$dis==districts_id[i],])
        if(length(intersect(tolower(districts_name[i]), tolower(districts))) > 0) districts_boundary <- rbind.data.frame(districts_boundary, data_districts[data_districts$id==districts_id[i],])
      }

    }
    if(is.numeric(districts)){

      # checking numeric input
      wrong_districts <- setdiff(districts, districts_id)
      if(length(wrong_districts) > 1) stop(paste0("unknown district: ", wrong_districts, collapse = ", "))

      # building map data
      for(i in 1:length(districts_id)){
        if(length(intersect(districts_id[i], districts)) > 0) data_combined_d <- rbind.data.frame(data_combined_d, data_combined[data_combined$dis==districts_id[i],])
        if(length(intersect(districts_id[i], districts)) > 0) districts_boundary <- rbind.data.frame(districts_boundary, data_districts[data_districts$id==districts_id[i],])

      }

    }

    rm(districts_name, districts_id)

  }
  # Apply MUNICIPALITY on pre-loaded map data ------------------------
  data_combined_m <- NULL

  if(!is.null(municipalities)){

    municipalities_name <- unique(data_municipalities$name)
    municipalities_id <- unique(data_municipalities$id)
    municipalities_boundary <- NULL

    if(!is.numeric(municipalities)){

      # checking non-numeric input
      wrong_municipalities <- setdiff(tolower(municipalities), tolower(municipalities_name))
      if(length(wrong_municipalities) == 1) stop(paste0("unknown municipality: ", wrong_municipalities, collapse = ", "))

      # building map data
      for(i in 1:length(municipalities_name)){
        if(length(intersect(tolower(municipalities_name[i]), tolower(municipalities))) > 0) data_combined_m <- rbind.data.frame(data_combined_m, data_combined[data_combined$id==municipalities_id[i],])
        if(length(intersect(tolower(municipalities_name[i]), tolower(municipalities))) > 0) municipalities_boundary <- rbind.data.frame(municipalities_boundary, data_combined[data_combined$id==municipalities_id[i],])
      }

    }
    if(is.numeric(municipalities)){

      # checking numeric input
      wrong_municipalities <- setdiff(municipalities, municipalities_id)
      if(length(wrong_municipalities) == 1) stop(paste0("unknown municipality: ", wrong_municipalities, collapse = ", "))

      # building map data
      for(i in 1:length(municipalities_id)){
        if(length(intersect(municipalities_id[i], municipalities)) > 0) data_combined_m <- rbind.data.frame(data_combined_m, data_combined[data_combined$id==municipalities_id[i],])
        if(length(intersect(municipalities_id[i], municipalities)) > 0) municipalities_boundary <- rbind.data.frame(municipalities_boundary, data_combined[data_combined$id==municipalities_id[i],])

      }

    }

    rm(municipalities_name, municipalities_id)

  }
  # joining CANTONS, DISTRICTS and MUNICIPALITY ------------------------
  if(!is.null(data_combined_c) | !is.null(data_combined_d) | !is.null(data_combined_m)){

    if(continuous == FALSE){

    long <- c(data_combined_c$long, data_combined_d$long, data_combined_m$long)
    lat <- c(data_combined_c$lat, data_combined_d$lat, data_combined_m$lat)
    group <- c(data_combined_c$group, data_combined_d$group, data_combined_m$group)
    data <- c(as.character(data_combined_c$data), as.character(data_combined_d$data), as.character(data_combined_m$data))
    data_combined <- cbind.data.frame(long, lat, group, data)
    data_combined$data <- as.factor(data_combined$data)
    data_combined$id2 <- paste0(data_combined$long, "-", data_combined$lat, "-", data_combined$group)
    data_combined <- data_combined[!duplicated(data_combined$id),]
    data_combined$id2 <- NULL

    } else {

      long <- c(data_combined_c$long, data_combined_d$long, data_combined_m$long)
      lat <- c(data_combined_c$lat, data_combined_d$lat, data_combined_m$lat)
      group <- c(data_combined_c$group, data_combined_d$group, data_combined_m$group)
      data <- c(data_combined_c$data, data_combined_d$data, data_combined_m$data)
      data_combined <- cbind.data.frame(long, lat, group, data)
      data_combined$id2 <- paste0(data_combined$long, "-", data_combined$lat, "-", data_combined$group)
      data_combined <- data_combined[!duplicated(data_combined$id),]
      data_combined$id2 <- NULL

    }
  }

  # Apply LAKES on pre-loaded map data ------------------------
  lakes_l <- tolower(lakes[1])

  if(lakes_l != "none"&lakes_l != "all"){

    lakes_name <- unique(data_lakes$name)
    lakes_id <- unique(data_lakes$id)
    data_lakes_s <- NULL

    if(!is.numeric(lakes)){

      # checking non-numeric input
      wrong_lakes <- setdiff(tolower(lakes), tolower(lakes_name))
      if(length(wrong_lakes) > 0) stop(paste0("unknown lake: ", wrong_lakes, collapse = ", "))

      # building lake map data
      for(i in 1:length(lakes_name)){
        if(length(intersect(tolower(lakes_name[i]), tolower(lakes))) > 0) data_lakes_s <- rbind.data.frame(data_lakes_s, data_lakes[data_lakes$id==lakes_id[i],])
      }

    }
    if(is.numeric(lakes)){

      # checking numeric input
      wrong_lakes <- setdiff(lakes, lakes_id)
      if(length(wrong_lakes) > 0) stop(paste0("unknown lake: ", wrong_lakes, collapse = ", "))

      # building lake map data
      for(i in 1:length(lakes_id)){
        if(length(intersect(lakes_id[i], lakes)) > 0) data_lakes_s <- rbind.data.frame(data_lakes_s, data_lakes[data_lakes$id==lakes_id[i],])
      }

    }

    data_lakes <- data_lakes_s
    rm(data_lakes_s, lakes_name, lakes_id)

  }

  # Apply EXTREMA ------------------------
  if(extrema != "global" & extrema != "local") stop("extrema: unknown input")

  if(extrema == "global" & continuous == TRUE){

    if(!is.null(data_combined_c) | !is.null(data_combined_d) | !is.null(data_combined_m)){

      data_combined <- rbind.data.frame(data_combined, data_combined[nrow(data_combined),])
      data_combined <- rbind.data.frame(data_combined, data_combined[nrow(data_combined),])

      data_combined$data[nrow(data_combined)-1] <- min(data_bfs$data)
      data_combined$long[nrow(data_combined)-1] <- NA
      data_combined$lat[nrow(data_combined)-1] <- NA
      data_combined$group[nrow(data_combined)-1] <- NA
      data_combined$data[nrow(data_combined)] <- max(data_bfs$data)
      data_combined$long[nrow(data_combined)] <- NA
      data_combined$lat[nrow(data_combined)] <- NA
      data_combined$group[nrow(data_combined)] <- NA

    }
  }

  rm(data_combined_c, data_combined_d, data_combined_m, data_bfs)

  # Plot areas ------------------------
  if(!is.logical(continuous)) stop("continuous: TRUE or FALSE needed")

  if(continuous == FALSE){

    p <- ggplot2::ggplot() +

      ggplot2::geom_polygon(data = data_combined, ggplot2::aes(fill = data_combined$data, x = long, y = lat, group = group)) +

      ggplot2::coord_equal() +

      ggplot2::theme_minimal() +

      ggplot2::theme(
        axis.line = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        legend.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        legend.position = legend_position) +

      ggplot2::labs(x = NULL,
           y = NULL,
           title = title,
           subtitle = subtitle,
           caption = caption) +

      ggplot2::scale_fill_brewer(
        type = color_discrete[1],
        palette = as.numeric(color_discrete[2]),
        na.value = color_na,
        name = legend_title,
        guide = ggplot2::guide_legend(
          keyheight = ggplot2::unit(5, units = "mm"),
          title.position = 'top',
          reverse = T
        ))

  } else {

      p <- ggplot2::ggplot() +

        ggplot2::geom_polygon(data = data_combined, ggplot2::aes(fill = data_combined$data, x = long, y = lat, group = group)) +

        ggplot2::coord_equal() +

        ggplot2::theme_minimal() +

        ggplot2::theme(
          axis.line = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          plot.background = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          legend.background = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          legend.position = "bottom"
        ) +

        ggplot2::labs(x = NULL,
             y = NULL,
             title = title,
             subtitle = subtitle,
             caption = caption) +

        ggplot2::scale_fill_gradient(
          low = color_continuous[1],
          high = color_continuous[2],
          na.value = color_na,
          name = legend_title,
          guide = ggplot2::guide_colorbar(
            direction = "horizontal",
            barheight = ggplot2::unit(2, units = "mm"),
            barwidth = ggplot2::unit(50, units = "mm"),
            draw.ulim = FALSE,
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 0.5))

    }
  # Add boundaries to plot ------------------------
  if(length(boundaries)!=length(boundaries_size) | length(boundaries)!=length(boundaries_color)) stop("boundary settings are not correctly specified")

  if(!is.null(boundaries)){
  # boundaries municipalities
  if("m" %in% boundaries == TRUE){
    p <- p + ggplot2::geom_path(data = data_combined, ggplot2::aes(x = long, y = lat, group = group), color = boundaries_color[which(boundaries == "m")], size = boundaries_size[which(boundaries == "m")])
  }

  # boundaries districts
  if("d" %in% boundaries == TRUE){

    if(!missing(districts)){
      p <- p + ggplot2::geom_path(data = districts_boundary, ggplot2::aes(x = long, y = lat, group = group), color = boundaries_color[which(boundaries == "d")], size = boundaries_size[which(boundaries == "d")])
    } else {
      p <- p + ggplot2::geom_path(data = data_districts, ggplot2::aes(x = long, y = lat, group = group), color = boundaries_color[which(boundaries == "d")], size = boundaries_size[which(boundaries == "d")])
    }
  }

  # boundaries cantons
  if("c" %in% boundaries == TRUE){
    if(!missing(cantons)){
      p <- p + ggplot2::geom_path(data = cantons_boundary, ggplot2::aes(x = long, y = lat, group = group), color = boundaries_color[which(boundaries == "c")], size = boundaries_size[which(boundaries == "c")])
    } else {
      p <- p + ggplot2::geom_path(data = data_cantons, ggplot2::aes(x = long, y = lat, group = group), color = boundaries_color[which(boundaries == "c")], size = boundaries_size[which(boundaries == "c")])
    }
  }

  # boundary country
  if("n" %in% boundaries == TRUE){
    p <- p + ggplot2::geom_path(data = data_country, ggplot2::aes(x = long, y = lat, group = group), color = boundaries_color[which(boundaries == "n")], size = boundaries_size[which(boundaries == "n")])
  }
  }

  # Add LAKES to plot ------------------------
  if(lakes[1] != "none"){
    p <- p +

      # Lakes
      ggplot2::geom_polygon(data = data_lakes, ggplot2::aes(x = long, y = lat, group = group), fill = "skyblue") +

      # Lakes (boarders)
      ggplot2::geom_path(data = data_lakes, ggplot2::aes(x = long, y = lat, group = group), color = "white", size = 0.1)

  }

  # Saving plot ------------------------
  if(save == TRUE){
    ggplot2::ggsave(paste0(filename), dpi = dpi, width = width, height = heigth, units = units)
  }

  rm(dt)
  p

}

# FUNCTION TO PLOT DISTRICTS
#' Plotting district-level maps of Switzerland
#'
#' \code{dis.plot} offers the possbility to visualise Swiss district-level data on a map.
#'     It uses the geodata that is made publicly available by the
#'     \href{https://www.bfs.admin.ch/bfs/en/home.html}{Swiss Federal Statistical Office}.  Geodata
#'     is available for the period between \code{2001} to \code{2017}.  The arguments \code{cantons},
#'     and \code{districts} make it easy to plot customised maps.
#'     The function \code{\link{dis.template}} provides templates for data collection.
#'
#' @param bfs_id a numeric vector with district identification numbers as used by the Swiss Federal Statistical
#'     Office corresponding to \code{data}.  For templates, see \code{\link{dis.template}}.
#' @param data a vector of data at district-level.
#' @param year numeric. Indicates what map version to plot. Available with package: \code{2016}. For additional
#'     geodata, see: \code{map.load}.
#' @param add_geodata_path a character string with the path to the folder where additional geodata is saved.
#' @param endofyear if \code{TRUE}, administrative boundaries as by December 31 of \code{year} are plotted.
#'     Available: \code{year > 2010}.
#' @param cantons input either numeric (canton identification numbers) or character (canton abbreviations)
#'     vector to plot district of selected cantons.
#' @param districts input either numeric (district identification numbers) or character (district names) vector
#'     to plot selected districts.
#' @param lakes input either numeric (lake identification numbers) or character (lakes names) vector to plot
#'     selected lakes. If \code{"none"}, no lakes are plotted.  If \code{"all"}, all available lakes are plotted.
#' @param boundaries character vector naming the boundaries to map.\itemize{
#'   \item "d": district boundaries
#'   \item "c": canton boundaries
#'   \item "n": national boundaries}
#' @param boundaries_size numeric vector that specifies the size of the corresponding element in \code{boundaries}.
#' @param boundaries_color character vector that specifies the color of the corresponding element in \code{boundaries}.
#' @param extrema if \code{local}, data of municipalities that are not selected by \code{cantons}
#'     or \code{districts} is not considered in the color scaling process.
#' @param continuous set to \code{FALSE}, if \code{data} is non-continuous.
#' @param color_continuous character vector with 2 elements to specifiy the plot color if \code{continuous = TRUE}.
#'    \code{color_continuous = c(low, high)} resulting in a two color gradient from color \code{low} to color \code{high}.
#' @param color_discrete character vector with 2 elements to specifiy the plot color if \code{continuous = FALSE}.
#'     \code{color_discrete = c(type, palette)} resulting in a color scheme from
#'     \href{http://colorbrewer2.org}{ColorBrewer} that are particularly well suited to display discrete
#'     values on a map.\itemize{
#'    \item \code{type}: one of seq (sequential), div (diverging) or qual (qualitative)
#'    \item \code{palette}: index into the list of palettes of appropriate \code{type}; character needed!}
#' @param color_na color of missing values.
#' @param title,subtitle,caption,legend_title text for titles or caption below the plot.
#' @param legend_position the position of the legend ("none", "left", "right", "bottom", "top")
#' @param save if \code{TRUE}, the plot will be saved to disk.
#' @param filename file name to create on disk incl. image file format (".jpeg", ".png", ".tiff", etc.).
#' @param dpi plot resolution. Applies only to raster output types.
#' @param width,heigth,units plot size in units ("in", "cm", or "mm"). If not supplied, uses the size of current
#'     graphics device.
#' @examples
#'  # Generating sample data:
#'  dt <- dis.template(2016)
#'  for(i in 1:nrow(dt)){dt$values[i] <- sample(c(300:700), 1)/1000}
#'
#'  # Plotting sample data:
#'  dis.plot(dt$bfs_nr, dt$values, 2016)
#'
#'  # Plotting sample data for the canton of Aargau:
#'  dis.plot(dt$bfs_nr, dt$values, 2016, cantons = c("AG"),
#'  lakes = c("Hallwilersee"))
#' @export
dis.plot <- function(bfs_id, data, year, add_geodata_path = ".", endofyear = FALSE,
                     cantons = NULL, districts = NULL, lakes = "all",
                     boundaries = "d", boundaries_size = 0.2, boundaries_color = "white",
                     extrema = "global", continuous = TRUE,
                     color_continuous = c("#fee5d9", "#a50f15"), color_discrete = c("seq", "1"), color_na = "gray90",
                     title = NULL, subtitle = NULL, caption = NULL, legend_title = NULL, legend_position = "bottom",
                     save = FALSE, filename = "dis_plot.png", dpi = 300, width = NA, heigth = NA, units = "cm") {

  # Checking BFS_ID and data ------------------------
  if(missing(bfs_id)) stop("bfs_id is missing")
  if(missing(data)) stop("data is missing")
  if(length(bfs_id)!=length(data)) stop("inputs differ in length")

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

    data_map <- dt[dt$year==year&dt$type=="district"&dt$endofyear==1,]
    data_country <- dt[dt$year==year&dt$type=="country"&dt$endofyear==1,]
    data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==1,]
    data_districts <- dt[dt$year==year&dt$type=="district"&dt$endofyear==1,]
    data_lakes <- dt[dt$year==year&dt$type=="lake"&dt$endofyear==1,]

    if(nrow(data_map)==0){

      data_map <- dt[dt$year==year&dt$type=="district"&dt$endofyear==0,]
      message(paste0("no data found for endofyear = TRUE; automatically applied endofyear = FALSE; map dates from 01.01.", year))

    }

    if(nrow(data_country)==0) data_country <- dt[dt$year==year&dt$type=="country"&dt$endofyear==0,]
    if(nrow(data_cantons)==0) data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]
    if(nrow(data_districts)==0) data_districts <- dt[dt$year==year&dt$type=="district"&dt$endofyear==0,]
    if(nrow(data_lakes)==0) data_lakes <- dt[dt$year==year&dt$type=="lake"&dt$endofyear==0,]


  } else {

    data_map <- dt[dt$year==year&dt$type=="district"&dt$endofyear==0,]
    data_country <- dt[dt$year==year&dt$type=="country"&dt$endofyear==0,]
    data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]
    data_districts <- dt[dt$year==year&dt$type=="district"&dt$endofyear==0,]
    data_lakes <- dt[dt$year==year&dt$type=="lake"&dt$endofyear==0,]

  }

  # Merge input data with map data ------------------------
  data_bfs <- cbind.data.frame(bfs_id, data)
  names(data_bfs) <- c("id", "data")
  data_combined <- dplyr::left_join(data_map, data_bfs, by = c("id" = "id"))

  # Apply CANTONS on pre-loaded map data ------------------------
  data_combined_c <- NULL

  if(!is.null(cantons)){

    cantons_name <- unique(data_cantons$name)
    cantons_id <- unique(data_cantons$id)
    cantons_boundary <- NULL

    if(!is.numeric(cantons)){

      # checking non-numeric input
      wrong_cantons <- setdiff(tolower(cantons), tolower(cantons_name))
      if(length(wrong_cantons) > 0) stop(paste0("unknown canton: ", wrong_cantons, collapse = ", "))

      # building map data
      for(i in 1:length(cantons_name)){
        if(length(intersect(tolower(cantons_name[i]), tolower(cantons))) > 0) data_combined_c <- rbind.data.frame(data_combined_c, data_combined[data_combined$can==cantons_id[i],])
        if(length(intersect(tolower(cantons_name[i]), tolower(cantons))) > 0) cantons_boundary <- rbind.data.frame(cantons_boundary, data_cantons[data_cantons$id==cantons_id[i],])

      }

    }
    if(is.numeric(cantons)){

      # checking numeric input
      wrong_cantons <- setdiff(cantons, cantons_id)
      if(length(wrong_cantons) > 0) stop(paste0("unknown canton id: ", wrong_cantons, collapse = ", "))

      # building map data
      for(i in 1:length(cantons_id)){
        if(length(intersect(cantons_id[i], cantons)) > 0) data_combined_c <- rbind.data.frame(data_combined_c, data_combined[data_combined$can==cantons_id[i],])
        if(length(intersect(cantons_id[i], cantons)) > 0) cantons_boundary <- rbind.data.frame(cantons_boundary, data_cantons[data_cantons$id==cantons_id[i],])

      }

    }

    rm(cantons_name, cantons_id)

  }
  # Apply DISTRICTS on pre-loaded map data ------------------------
  data_combined_d <- NULL

  if(!is.null(districts)){

    districts_name <- unique(data_districts$name)
    districts_id <- unique(data_districts$id)
    districts_boundary <- NULL

    if(!is.numeric(districts)){

      # checking non-numeric input
      wrong_districts <- setdiff(tolower(districts), tolower(districts_name))
      if(length(wrong_districts) > 1) stop(paste0("unknown district: ", wrong_districts, collapse = ", "))

      # building map data
      for(i in 1:length(districts_name)){
        if(length(intersect(tolower(districts_name[i]), tolower(districts))) > 0) data_combined_d <- rbind.data.frame(data_combined_d, data_combined[data_combined$id==districts_id[i],])
        if(length(intersect(tolower(districts_name[i]), tolower(districts))) > 0) districts_boundary <- rbind.data.frame(districts_boundary, data_districts[data_districts$id==districts_id[i],])
      }

    }
    if(is.numeric(districts)){

      # checking numeric input
      wrong_districts <- setdiff(districts, districts_id)
      if(length(wrong_districts) > 1) stop(paste0("unknown district: ", wrong_districts, collapse = ", "))

      # building map data
      for(i in 1:length(districts_id)){
        if(length(intersect(districts_id[i], districts)) > 0) data_combined_d <- rbind.data.frame(data_combined_d, data_combined[data_combined$id==districts_id[i],])
        if(length(intersect(districts_id[i], districts)) > 0) districts_boundary <- rbind.data.frame(districts_boundary, data_districts[data_districts$id==districts_id[i],])

      }

    }

    rm(districts_name, districts_id)

  }
  # joining CANTONS and DISTRICTS ------------------------
  if(!is.null(data_combined_c) | !is.null(data_combined_d)){

    if(continuous == FALSE){

      long <- c(data_combined_c$long, data_combined_d$long)
      lat <- c(data_combined_c$lat, data_combined_d$lat)
      group <- c(data_combined_c$group, data_combined_d$group)
      data <- c(as.character(data_combined_c$data), as.character(data_combined_d$data))
      data_combined <- cbind.data.frame(long, lat, group, data)
      data_combined$data <- as.factor(data_combined$data)
      data_combined$id2 <- paste0(data_combined$long, "-", data_combined$lat, "-", data_combined$group)
      data_combined <- data_combined[!duplicated(data_combined$id),]
      data_combined$id2 <- NULL

    } else {

      long <- c(data_combined_c$long, data_combined_d$long)
      lat <- c(data_combined_c$lat, data_combined_d$lat)
      group <- c(data_combined_c$group, data_combined_d$group)
      data <- c(data_combined_c$data, data_combined_d$data)
      data_combined <- cbind.data.frame(long, lat, group, data)
      data_combined$id2 <- paste0(data_combined$long, "-", data_combined$lat, "-", data_combined$group)
      data_combined <- data_combined[!duplicated(data_combined$id),]
      data_combined$id2 <- NULL

    }
  }

  # Apply LAKES on pre-loaded map data ------------------------
  lakes_l <- tolower(lakes[1])

  if(lakes_l != "none"&lakes_l != "all"){

    lakes_name <- unique(data_lakes$name)
    lakes_id <- unique(data_lakes$id)
    data_lakes_s <- NULL

    if(!is.numeric(lakes)){

      # checking non-numeric input
      wrong_lakes <- setdiff(tolower(lakes), tolower(lakes_name))
      if(length(wrong_lakes) > 0) stop(paste0("unknown lake: ", wrong_lakes, collapse = ", "))

      # building lake map data
      for(i in 1:length(lakes_name)){
        if(length(intersect(tolower(lakes_name[i]), tolower(lakes))) > 0) data_lakes_s <- rbind.data.frame(data_lakes_s, data_lakes[data_lakes$id==lakes_id[i],])
      }

    }
    if(is.numeric(lakes)){

      # checking numeric input
      wrong_lakes <- setdiff(lakes, lakes_id)
      if(length(wrong_lakes) > 0) stop(paste0("unknown lake: ", wrong_lakes, collapse = ", "))

      # building lake map data
      for(i in 1:length(lakes_id)){
        if(length(intersect(lakes_id[i], lakes)) > 0) data_lakes_s <- rbind.data.frame(data_lakes_s, data_lakes[data_lakes$id==lakes_id[i],])
      }

    }

    data_lakes <- data_lakes_s
    rm(data_lakes_s, lakes_name, lakes_id)

  }

  # Apply EXTREMA ------------------------
  if(extrema != "global" & extrema != "local") stop("extrema: unknown input")

  if(extrema == "global" & continuous == TRUE){

    if(!is.null(data_combined_c) | !is.null(data_combined_d)){

      data_combined <- rbind.data.frame(data_combined, data_combined[nrow(data_combined),])
      data_combined <- rbind.data.frame(data_combined, data_combined[nrow(data_combined),])

      data_combined$data[nrow(data_combined)-1] <- min(data_bfs$data)
      data_combined$long[nrow(data_combined)-1] <- NA
      data_combined$lat[nrow(data_combined)-1] <- NA
      data_combined$group[nrow(data_combined)-1] <- NA
      data_combined$data[nrow(data_combined)] <- max(data_bfs$data)
      data_combined$long[nrow(data_combined)] <- NA
      data_combined$lat[nrow(data_combined)] <- NA
      data_combined$group[nrow(data_combined)] <- NA

    }
  }

  rm(data_combined_c, data_combined_d, data_bfs)

  # Plot areas ------------------------
  if(!is.logical(continuous)) stop("continuous: TRUE or FALSE needed")

  if(continuous == FALSE){

    data_combined$data <- as.factor(data_combined$data)

    p <- ggplot2::ggplot() +

      ggplot2::geom_polygon(data = data_combined, ggplot2::aes(fill = data_combined$data, x = long, y = lat, group = group)) +

      ggplot2::coord_equal() +

      ggplot2::theme_minimal() +

      ggplot2::theme(
        axis.line = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        legend.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        legend.position = legend_position) +

      ggplot2::labs(x = NULL,
           y = NULL,
           title = title,
           subtitle = subtitle,
           caption = caption) +

      ggplot2::scale_fill_brewer(
        type = color_discrete[1],
        palette = as.numeric(color_discrete[2]),
        na.value = color_na,
        name = legend_title,
        guide = ggplot2::guide_legend(
          keyheight = ggplot2::unit(5, units = "mm"),
          title.position = 'top',
          reverse = T))

  } else {

    p <- ggplot2::ggplot() +

      ggplot2::geom_polygon(data = data_combined, ggplot2::aes(fill = data_combined$data, x = long, y = lat, group = group)) +

      ggplot2::coord_equal() +

      ggplot2::theme_minimal() +

      ggplot2::theme(
        axis.line = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        legend.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        legend.position = "bottom"
      ) +

      ggplot2::labs(x = NULL,
           y = NULL,
           title = title,
           subtitle = subtitle,
           caption = caption) +

      ggplot2::scale_fill_gradient(
        low = color_continuous[1],
        high = color_continuous[2],
        na.value = color_na,
        name = legend_title,
        guide = ggplot2::guide_colorbar(
          direction = "horizontal",
          barheight = ggplot2::unit(2, units = "mm"),
          barwidth = ggplot2::unit(50, units = "mm"),
          draw.ulim = FALSE,
          title.position = 'top',
          title.hjust = 0.5,
          label.hjust = 0.5))

  }
  # Add boundaries to plot ------------------------
  if(length(boundaries)!=length(boundaries_size) | length(boundaries)!=length(boundaries_color)) stop("boundary settings are not correctly specified")

  if(!is.null(boundaries)){
    # boundaries municipalities
    if("d" %in% boundaries == TRUE){
      p <- p + ggplot2::geom_path(data = data_combined, ggplot2::aes(x = long, y = lat, group = group), color = boundaries_color[which(boundaries == "d")], size = boundaries_size[which(boundaries == "d")])
    }

    # boundaries cantons
    if("c" %in% boundaries == TRUE){
      if(!missing(cantons)){
        p <- p + ggplot2::geom_path(data = cantons_boundary, ggplot2::aes(x = long, y = lat, group = group), color = boundaries_color[which(boundaries == "c")], size = boundaries_size[which(boundaries == "c")])
      } else {
        p <- p + ggplot2::geom_path(data = data_cantons, ggplot2::aes(x = long, y = lat, group = group), color = boundaries_color[which(boundaries == "c")], size = boundaries_size[which(boundaries == "c")])
      }
    }

    # boundary country
    if("n" %in% boundaries == TRUE){
      p <- p + ggplot2::geom_path(data = data_country, ggplot2::aes(x = long, y = lat, group = group), color = boundaries_color[which(boundaries == "n")], size = boundaries_size[which(boundaries == "n")])
    }
  }

  # Add LAKES to plot ------------------------
  if(lakes[1] != "none"){
    p <- p +

      # Lakes
      ggplot2::geom_polygon(data = data_lakes, ggplot2::aes(x = long, y = lat, group = group), fill = "skyblue") +

      # Lakes (boarders)
      ggplot2::geom_path(data = data_lakes, ggplot2::aes(x = long, y = lat, group = group), color = "white", size = 0.1)

  }

  # Saving plot ------------------------
  if(save == TRUE){
    ggplot2::ggsave(paste0(filename), dpi = dpi, width = width, height = heigth, units = units)
  }

  rm(dt)

  p

}

# FUNCTION TO PLOT CANTONS
#' Plotting canton-level maps of Switzerland
#'
#' \code{can.plot} offers the possbility to visualise Swiss canton-level data on a map.
#'     It uses the geodata that is made publicly available by the
#'     \href{https://www.bfs.admin.ch/bfs/en/home.html}{Swiss Federal Statistical Office}.  Geodata
#'     is available for the period between \code{2001} to \code{2017}.  The argument \code{cantons} makes
#'     it easy to plot customised maps. The function \code{\link{can.template}} provides templates
#'     for data collection.
#'
#' @param bfs_id a numeric vector with canton identification numbers as used by the Swiss Federal
#'     Statistical Office corresponding to \code{data}.  For templates, see \code{\link{can.template}}.
#' @param data a vector of data at canton-level.
#' @param year numeric. Indicates what map version to plot. Available with package: \code{2016}. For additional
#'     geodata, see: \code{map.load}.
#' @param add_geodata_path a character string with the path to the folder where additional geodata is saved.
#' @param endofyear if \code{TRUE}, administrative boundaries as by December 31 of \code{year} are
#'     plotted. Available: \code{year > 2010}.
#' @param cantons input either numeric (canton identification numbers) or character (canton abbreviations)
#'     vector to plot district of selected cantons.
#' @param lakes input either numeric (lake identification numbers) or character (lakes names) vector to
#'     plot selected lakes. If \code{"none"}, no lakes are plotted.
#'     If \code{"all"}, all available lakes are plotted.
#' @param boundaries character vector naming the boundaries to map.\itemize{
#'   \item "c": canton boundaries
#'   \item "n": national boundaries}
#' @param boundaries_size numeric vector that specifies the size of the corresponding element in \code{boundaries}.
#' @param boundaries_color character vector that specifies the color of the corresponding element
#'     in \code{boundaries}.
#' @param extrema if \code{local}, data of municipalities that are not selected by \code{cantons} is not
#'     considered in the color scaling process.
#' @param continuous set to \code{FALSE}, if \code{data} is non-continuous.
#' @param color_continuous character vector with 2 elements to specifiy the plot color if \code{continuous =
#'     TRUE}. \code{color_continuous = c(low, high)} resulting in a two color gradient from color
#'     \code{low} to color \code{high}.
#' @param color_discrete character vector with 2 elements to specifiy the plot color if \code{continuous = FALSE}.
#'     \code{color_discrete = c(type, palette)} resulting in a color scheme from
#'     \href{http://colorbrewer2.org}{ColorBrewer} that are particularly well suited to display discrete
#'     values on a map.\itemize{
#'   \item \code{type}: one of seq (sequential), div (diverging) or qual (qualitative)
#'   \item \code{palette}: index into the list of palettes of appropriate \code{type}; character needed!}
#' @param color_na color of missing values.
#' @param title,subtitle,caption,legend_title text for titles or caption below the plot.
#' @param legend_position the position of the legend ("none", "left", "right", "bottom", "top")
#' @param save if \code{TRUE}, the plot will be saved to disk.
#' @param filename file name to create on disk incl. image file format (".jpeg", ".png", ".tiff", etc.).
#' @param dpi plot resolution. Applies only to raster output types.
#' @param width,heigth,units plot size in units ("in", "cm", or "mm"). If not supplied, uses the size of current graphics device.
#' @examples
#'  # Generating sample data:
#'  dt <- can.template(2016)
#'  for(i in 1:nrow(dt)){dt$values[i] <- sample(c(300:700), 1)/1000}
#'
#'  # Plotting sample data:
#'  can.plot(dt$bfs_nr, dt$values, 2016)
#' @export
can.plot <- function(bfs_id, data, year, add_geodata_path = ".", endofyear = FALSE,
                     cantons = NULL, lakes = "all",
                     boundaries = "c", boundaries_size = 0.2, boundaries_color = "white",
                     extrema = "global", continuous = TRUE,
                     color_continuous = c("#fee5d9", "#a50f15"), color_discrete = c("seq", "1"), color_na = "gray90",
                     title = NULL, subtitle = NULL, caption = NULL, legend_title = NULL, legend_position = "bottom",
                     save = FALSE, filename = "can_plot.png", dpi = 300, width = NA, heigth = NA, units = "cm") {

  # Checking BFS_ID and data ------------------------
  if(missing(bfs_id)) stop("bfs_id is missing")
  if(missing(data)) stop("data is missing")
  if(length(bfs_id)!=length(data)) stop("inputs differ in length")
  long = lat = group = NULL

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

    data_map <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==1,]
    data_country <- dt[dt$year==year&dt$type=="country"&dt$endofyear==1,]
    data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==1,]
    data_lakes <- dt[dt$year==year&dt$type=="lake"&dt$endofyear==1,]

    if(nrow(data_map)==0){

      data_map <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]
      message(paste0("no data found for endofyear = TRUE; automatically applied endofyear = FALSE; map dates from 01.01.", year))

    }

    if(nrow(data_country)==0) data_country <- dt[dt$year==year&dt$type=="country"&dt$endofyear==0,]
    if(nrow(data_cantons)==0) data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]
    if(nrow(data_lakes)==0) data_lakes <- dt[dt$year==year&dt$type=="lake"&dt$endofyear==0,]


  } else {

    data_map <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]
    data_country <- dt[dt$year==year&dt$type=="country"&dt$endofyear==0,]
    data_cantons <- dt[dt$year==year&dt$type=="canton"&dt$endofyear==0,]
    data_lakes <- dt[dt$year==year&dt$type=="lake"&dt$endofyear==0,]

  }

  # Merge input data with map data ------------------------
  data_bfs <- cbind.data.frame(bfs_id, data)
  names(data_bfs) <- c("id", "data")
  data_combined <- dplyr::left_join(data_map, data_bfs, by = c("id" = "id"))

  # Apply CANTONS on pre-loaded map data ------------------------
  data_combined_c <- NULL

  if(!is.null(cantons)){

    cantons_name <- unique(data_cantons$name)
    cantons_id <- unique(data_cantons$id)
    cantons_boundary <- NULL

    if(!is.numeric(cantons)){

      # checking non-numeric input
      wrong_cantons <- setdiff(tolower(cantons), tolower(cantons_name))
      if(length(wrong_cantons) > 0) stop(paste0("unknown canton: ", wrong_cantons, collapse = ", "))

      # building map data
      for(i in 1:length(cantons_name)){
        if(length(intersect(tolower(cantons_name[i]), tolower(cantons))) > 0) data_combined_c <- rbind.data.frame(data_combined_c, data_combined[data_combined$id==cantons_id[i],])
        if(length(intersect(tolower(cantons_name[i]), tolower(cantons))) > 0) cantons_boundary <- rbind.data.frame(cantons_boundary, data_cantons[data_cantons$id==cantons_id[i],])

      }

    }
    if(is.numeric(cantons)){

      # checking numeric input
      wrong_cantons <- setdiff(cantons, cantons_id)
      if(length(wrong_cantons) > 0) stop(paste0("unknown canton id: ", wrong_cantons, collapse = ", "))

      # building map data
      for(i in 1:length(cantons_id)){
        if(length(intersect(cantons_id[i], cantons)) > 0) data_combined_c <- rbind.data.frame(data_combined_c, data_combined[data_combined$id==cantons_id[i],])
        if(length(intersect(cantons_id[i], cantons)) > 0) cantons_boundary <- rbind.data.frame(cantons_boundary, data_cantons[data_cantons$id==cantons_id[i],])

      }

    }

    rm(cantons_name, cantons_id)

  }
  # preparing map based on CANTONS ------------------------
  if(!is.null(data_combined_c)){

    data_combined <- data_combined_c

  }

  # Apply LAKES on pre-loaded map data ------------------------
  lakes_l <- tolower(lakes[1])

  if(lakes_l != "none"&lakes_l != "all"){

    lakes_name <- unique(data_lakes$name)
    lakes_id <- unique(data_lakes$id)
    data_lakes_s <- NULL

    if(!is.numeric(lakes)){

      # checking non-numeric input
      wrong_lakes <- setdiff(tolower(lakes), tolower(lakes_name))
      if(length(wrong_lakes) > 0) stop(paste0("unknown lake: ", wrong_lakes, collapse = ", "))

      # building lake map data
      for(i in 1:length(lakes_name)){
        if(length(intersect(tolower(lakes_name[i]), tolower(lakes))) > 0) data_lakes_s <- rbind.data.frame(data_lakes_s, data_lakes[data_lakes$id==lakes_id[i],])
      }

    }
    if(is.numeric(lakes)){

      # checking numeric input
      wrong_lakes <- setdiff(lakes, lakes_id)
      if(length(wrong_lakes) > 0) stop(paste0("unknown lake: ", wrong_lakes, collapse = ", "))

      # building lake map data
      for(i in 1:length(lakes_id)){
        if(length(intersect(lakes_id[i], lakes)) > 0) data_lakes_s <- rbind.data.frame(data_lakes_s, data_lakes[data_lakes$id==lakes_id[i],])
      }

    }

    data_lakes <- data_lakes_s
    rm(data_lakes_s, lakes_name, lakes_id)

  }

  # Apply EXTREMA ------------------------
  if(extrema != "global" & extrema != "local") stop("extrema: unknown input")

  if(extrema == "global" & continuous == TRUE){

    if(!is.null(data_combined_c)){

      data_combined <- rbind.data.frame(data_combined, data_combined[nrow(data_combined),])
      data_combined <- rbind.data.frame(data_combined, data_combined[nrow(data_combined),])

      data_combined$data[nrow(data_combined)-1] <- min(data_bfs$data)
      data_combined$long[nrow(data_combined)-1] <- NA
      data_combined$lat[nrow(data_combined)-1] <- NA
      data_combined$group[nrow(data_combined)-1] <- NA
      data_combined$data[nrow(data_combined)] <- max(data_bfs$data)
      data_combined$long[nrow(data_combined)] <- NA
      data_combined$lat[nrow(data_combined)] <- NA
      data_combined$group[nrow(data_combined)] <- NA

    }
  }

  rm(data_combined_c, data_bfs)

  # Plot areas ------------------------
  if(!is.logical(continuous)) stop("continuous: TRUE or FALSE needed")

  data_combined_1 <- data_combined[data_combined$id < 15 | data_combined$id > 16,]
  data_combined_ai_ar <- data_combined[data_combined$id == 15 | data_combined$id == 16,]

  if(continuous == FALSE){

    p <- ggplot2::ggplot() +

      ggplot2::geom_polygon(data = data_combined_1, ggplot2::aes(fill = data_combined_1$data, x = long, y = lat, group = group)) +

      ggplot2::geom_polygon(data = data_combined_ai_ar, ggplot2::aes(fill = data_combined_ai_ar$data, x = long, y = lat, group = group)) +

      ggplot2::coord_equal() +

      ggplot2::theme_minimal() +

      ggplot2::theme(
        axis.line = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        legend.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        legend.position = legend_position) +

      ggplot2::labs(x = NULL,
           y = NULL,
           title = title,
           subtitle = subtitle,
           caption = caption) +

      ggplot2::scale_fill_brewer(
        type = color_discrete[1],
        palette = as.numeric(color_discrete[2]),
        na.value = color_na,
        name = legend_title,
        guide = ggplot2::guide_legend(
          keyheight = ggplot2::unit(5, units = "mm"),
          title.position = 'top',
          reverse = T))

  } else {

    p <- ggplot2::ggplot() +

      ggplot2::geom_polygon(data = data_combined_1, ggplot2::aes(fill = data_combined_1$data, x = long, y = lat, group = group)) +

      ggplot2::geom_polygon(data = data_combined_ai_ar, ggplot2::aes(fill = data_combined_ai_ar$data, x = long, y = lat, group = group)) +

      ggplot2::coord_equal() +

      ggplot2::theme_minimal() +

      ggplot2::theme(
        axis.line = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        plot.background = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        legend.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        legend.position = "bottom"
      ) +

      ggplot2::labs(x = NULL,
           y = NULL,
           title = title,
           subtitle = subtitle,
           caption = caption) +

      ggplot2::scale_fill_gradient(
        low = color_continuous[1],
        high = color_continuous[2],
        na.value = color_na,
        name = legend_title,
        guide = ggplot2::guide_colorbar(
          direction = "horizontal",
          barheight = ggplot2::unit(2, units = "mm"),
          barwidth = ggplot2::unit(50, units = "mm"),
          draw.ulim = FALSE,
          title.position = 'top',
          title.hjust = 0.5,
          label.hjust = 0.5))

  }

  # Add boundaries to plot ------------------------
  if(length(boundaries)!=length(boundaries_size) | length(boundaries)!=length(boundaries_color)) stop("boundary settings are not correctly specified")

  if(!is.null(boundaries)){
    # boundaries municipalities
    if("d" %in% boundaries == TRUE){
      p <- p + ggplot2::geom_path(data = data_combined, ggplot2::aes(x = long, y = lat, group = group), color = boundaries_color[which(boundaries == "d")], size = boundaries_size[which(boundaries == "d")])
    }

    # boundaries cantons
    if("c" %in% boundaries == TRUE){
      if(!missing(cantons)){
        p <- p + ggplot2::geom_path(data = cantons_boundary, ggplot2::aes(x = long, y = lat, group = group), color = boundaries_color[which(boundaries == "c")], size = boundaries_size[which(boundaries == "c")])
      } else {
        p <- p + ggplot2::geom_path(data = data_cantons, ggplot2::aes(x = long, y = lat, group = group), color = boundaries_color[which(boundaries == "c")], size = boundaries_size[which(boundaries == "c")])
      }
    }

    # boundary country
    if("n" %in% boundaries == TRUE){
      p <- p + ggplot2::geom_path(data = data_country, ggplot2::aes(x = long, y = lat, group = group), color = boundaries_color[which(boundaries == "n")], size = boundaries_size[which(boundaries == "n")])
    }
  }

  # Add LAKES to plot ------------------------
  if(lakes[1] != "none"){
    p <- p +

      # Lakes
      ggplot2::geom_polygon(data = data_lakes, ggplot2::aes(x = long, y = lat, group = group), fill = "skyblue") +

      # Lakes (boarders)
      ggplot2::geom_path(data = data_lakes, ggplot2::aes(x = long, y = lat, group = group), color = "white", size = 0.1)

  }

  # Saving plot ------------------------
  if(save == TRUE){
    ggplot2::ggsave(paste0(filename), dpi = dpi, width = width, height = heigth, units = units)
  }

  rm(dt)
  p

}
