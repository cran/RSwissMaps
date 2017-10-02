# FUNCTION TO DOWNLOAD MAP DATA
#' Download additional geodata of Swiss municipalities, districts, cantons and lakes
#'
#' \code{map.load} offers the possbility to download geodata of Swiss subdivisions and
#'    lakes for the years 2001-2017 from the official
#'    \href{https://github.com/zumbov2/RSwissMaps}{RSwissMaps repository} on GitHub.
#'
#' @param year numeric. Available years: \code{2001-2017}.
#' @param add_geodata_path a character string with the path to the folder where downloaded geodata
#'  is saved.
#' @examples
#' ## Not run:
#' #Download Swiss geodata for 2017
#' map.load(2017)
#' ## End(Not run)
#' @export
map.load <- function(year, add_geodata_path = "."){

  # Checking YEAR ------------------------
  if(missing(year)) stop("year missing")
  if(!is.numeric(year)) stop("year is not numeric")
  if(length(year) > 1) stop("please choose a year")

  # Download file based on YEAR and store based on PATH ------------------------
  if(add_geodata_path == "."){

    downloader::download(paste0("https://github.com/zumbov2/RSwissMaps/raw/master/data/mapCH", year, ".rda"), destfile = paste0("mapCH", year, ".rda"))

}
  if(add_geodata_path != ""){

    if(substring(add_geodata_path, nchar(add_geodata_path), nchar(add_geodata_path)) == "/"){

      downloader::download(paste0("https://github.com/zumbov2/RSwissMaps/raw/master/data/mapCH", year, ".rda"), destfile = paste0(add_geodata_path, "mapCH", year, ".rda"))

    } else {

      downloader::download(paste0("https://github.com/zumbov2/RSwissMaps/raw/master/data/mapCH", year, ".rda"), destfile = paste0(add_geodata_path, "/mapCH", year, ".rda"))

    }

  }
}
