#' List files and folders from NINA's GeoSpatialData directory
#'
#' Lists the contents of a specified folder within `/data/R/GeoSpatialData` on NINA’s network.
#'
#' @param folder Character. Either the full path or a relative subpath from `/data/R/GeoSpatialData`.
#' @param type Character. One of `"both"` (default), `"file"`, or `"folder"` to filter results.
#'
#' @return A character vector of full paths, sorted with folders first.
#' @export
#' 
#' @examples
#' # List all files and folders in the top-level GeoSpatialData directory
#' list_from_rgsd()
#'
#' # List only folders under the Topography category
#' list_from_rgsd("Topography", type = "folder")
#'
#' # List only files in a specific catchment subdirectory
#' list_from_rgsd("Hydrography/Norway_Catchments/Original", type = "file")
#'
#' # Use a full path to a Land Cover folder
#' list_from_rgsd("/data/R/GeoSpatialData/LandCover/Europe/Copernicus_HighResolutionLayers/Original", type = "both")
list_from_rgsd <- function(folder = "/data/R/GeoSpatialData", 
                           type = c("both", "file", "folder")) {
  type <- match.arg(type)
  
  host <- try(system("hostname", intern = TRUE), silent = TRUE)
  if (inherits(host, "try-error") || !grepl("ninrstudio|ningis|lipgis|liprstudio", host)) {
    stop("This function must be run on a NINA server.")
  }
  
  base_path <- "/data/R/GeoSpatialData"
  
  if (!dir.exists(folder)) {
    short_path <- file.path(base_path, folder)
    if (dir.exists(short_path)) {
      folder <- short_path
    } else {
      stop("The folder does not exist or cannot be accessed: ", folder)
    }
  }
  
  folder <- normalizePath(folder, winslash = "/", mustWork = TRUE)
  all_items <- list.files(folder, full.names = TRUE, recursive = FALSE, include.dirs = TRUE)
  info <- file.info(all_items)
  
  if (type == "file") {
    items <- all_items[!info$isdir]
    info <- info[!info$isdir, , drop = FALSE]
  } else if (type == "folder") {
    items <- all_items[info$isdir]
    info <- info[info$isdir, , drop = FALSE]
  } else {
    items <- all_items
    info <- info
  }
  
  df <- data.frame(
    path = items,
    name = basename(items),
    isdir = info$isdir,
    stringsAsFactors = FALSE
  )
  
  df <- df[order(-df$isdir, tolower(df$name)), ]
  return(df$path)
}


#' Import spatial data from NINA's GeoSpatialData directory
#'
#' Loads vector or raster data using appropriate methods from `sf`, `terra`, or `sfarrow`.
#'
#' @param name Character. Relative path from `/data/R/GeoSpatialData`, or full path.
#' @param layer Character. Required if reading from a GPKG or GDB with multiple layers.
#'
#' @return An `sf` object or a `SpatRaster`, depending on input type.
#' @export
#' 
#' @examples
#' # Load a GeoTIFF (raster)
#' elev <- import_from_rgsd("Elevation/Fenoscandia_DEM_10m/Original/dem_10m_fenoscandia.tif")
#'
#' # Load a shapefile (vector)
#' huts <- import_from_rgsd("Buildings/Norway_FKB_Buildings/Processed/N50_TouristCabins/Norway_TouristCabins_ETRS89_UTM_zone_33N.shp")
#'
#' # Load from a full absolute path
#' cover <- import_from_rgsd("/data/R/GeoSpatialData/Topography/Norway_N50/Original/versjon20241231/N50 Kartdata FGDB-format/Basisdata_03_Oslo_25832_N50Kartdata_FGDB.gdb",
#' layer = "N50_Arealdekke_omrade")
#'
#' # NOTE: GeoPackage and GeoParquet files are not yet consistently supported in this directory
import_from_rgsd <- function(name, layer = NULL) {
  host <- try(system("hostname", intern = TRUE), silent = TRUE)
  if (inherits(host, "try-error") || !grepl("ninrstudio|ningis|lipgis|liprstudio", host)) {
    stop("This function must be run on a NINA server (e.g., ninrstudio, ningis, lipgis, or liprstudio).")
  }
  
  base_path <- "/data/R/GeoSpatialData"
  
  if (grepl("^/", name)) {
    full_path <- normalizePath(name, winslash = "/", mustWork = FALSE)
  } else {
    full_path <- normalizePath(file.path(base_path, name), winslash = "/", mustWork = FALSE)
  }
  
  if (!file.exists(full_path) && !dir.exists(full_path)) {
    stop("The specified file or folder does not exist: ", full_path)
  }
  
  ext <- tolower(tools::file_ext(full_path))
  
  if (ext == "gpkg" || (dir.exists(full_path) && grepl("\\.gdb$", name))) {
    if (!requireNamespace("sf", quietly = TRUE)) stop("The 'sf' package is required to read vector data.")
    if (is.null(layer)) stop(
      "You must specify a 'layer' name when importing from a GPKG or GDB file.\n",
      "To see available layers, run:\n  sf::st_layers(\"", full_path, "\")"
    )
    return(sf::st_read(full_path, layer = layer, quiet = TRUE))
    
  } else if (ext %in% c("shp", "geojson")) {
    if (!requireNamespace("sf", quietly = TRUE)) stop("The 'sf' package is required to read vector data.")
    return(sf::st_read(full_path, quiet = TRUE))
    
  } else if (ext == "parquet") {
    if (!requireNamespace("sfarrow", quietly = TRUE)) stop("The 'sfarrow' package is required to read GeoParquet files.")
    return(sfarrow::st_read_parquet(full_path))
    
  } else if (ext %in% c("tif", "tiff", "img", "asc", "vrt", "nc")) {
    if (!requireNamespace("terra", quietly = TRUE)) stop("The 'terra' package is required to read raster data.")
    return(terra::rast(full_path))
    
  } else {
    stop("Unsupported or unknown file type: ", ext)
  }
}
