#' Read spatial or raster data from S3
#'
#' Automatically infers file type from file extension if not provided.
#'
#' @param s3_path The object key (path to the file in the bucket)
#' @param bucket The S3 bucket name
#' @param file_type Optional file extension (e.g. ".parquet", ".gpkg", ".tif"). If NULL (default), inferred from `s3_path`.
#' @param endpoint S3 endpoint (default is NINA's internal S3)
#' @param use_https Use HTTPS (default TRUE)
#' @param virtual Use virtual-hosted-style URLs (default FALSE)
#' @param ... Additional arguments passed to the reader
#'
#' @return A spatial (`sf`) or raster (`SpatRaster`) object
#' @export
read_from_s3 <- function(s3_path,
                         bucket,
                         file_type = NULL,
                         endpoint = "s3-int-1.nina.no",
                         use_https = TRUE,
                         virtual = FALSE,
                         ...) {
  tryCatch({
    # Auto-detect file type if not provided
    if (is.null(file_type)) {
      ext <- tools::file_ext(s3_path)
      if (ext == "") stop("Unable to determine file type from s3_path.")
      file_type <- paste0(".", tolower(ext))  
    }
    
    if (file_type == ".parquet") {
      s3 <- arrow::s3_bucket(
        bucket = bucket,
        endpoint_override = endpoint,
        anonymous = TRUE,
        scheme = ifelse(use_https, "https", "http")
      )
      s3_file <- s3$path(s3_path)
      
      sf_data <- sfarrow::st_read_parquet(s3_file, ...)
      if (!inherits(sf_data, "sf")) stop("The file is not a valid `sf` object.")
      return(sf_data)
      
    } else if (file_type %in% c(".gpkg", ".tif")) {
      tmp <- tempfile(fileext = file_type)
      on.exit(unlink(tmp), add = TRUE)
      
      aws.s3::save_object(object = s3_path, bucket = bucket, file = tmp,
                          base_url = endpoint, use_https = use_https, region = "", virtual = virtual)
      
      if (file_type == ".gpkg") {
        return(sf::st_read(tmp, quiet = TRUE, ...))
      } else if (file_type == ".tif") {
        return(terra::rast(tmp, ...))
      }
      
    } else {
      stop(paste("Unsupported file type:", file_type))
    }
    
  }, error = function(e) {
    stop(paste("Failed to read from S3:", e$message))
  })
}

#' Write spatial data to an S3 bucket
#'
#' @description
#' Upload a spatial dataset (vector or raster) to an S3 bucket using a single-file format.
#'
#' @details
#' Note that shapefiles (`.shp`) and file geodatabases (`.gdb`) are not supported for upload.
#' Shapefiles consist of multiple companion files (e.g., `.shp`, `.shx`, `.dbf`) that cannot be
#' reliably stored or reconstructed as standalone objects in S3. Similarly, `.gdb` is a folder-based 
#' format that cannot be uploaded as a single file.
#'
#' Please use `.gpkg`, `.parquet`, or `.tif` formats for compatibility with S3.
#'
#' @param obj An `sf` or `SpatRaster` object.
#' @param bucket Character. Name of the S3 bucket (e.g., `"geolink-test"`).
#' @param key Character. Path (including filename) within the S3 bucket to write to.
#' @param layer Character. Optional layer name when writing to a GPKG file. Required if using `.gpkg`.
#' @param endpoint Character. The S3-compatible endpoint. Default is NINA’s internal `"s3-int-1.nina.no"`.
#' @param use_https Logical. Whether to use HTTPS (default `TRUE`).
#' @param virtual Logical. Use virtual-hosted-style URLs (default `FALSE`).
#' @param ... Additional arguments passed to `st_write()`, `st_write_parquet()`, or `writeRaster()`.
#'
#' @return Logical. `TRUE` if upload was successful.
#' @export
#'
#' @examples
#' # These examples demonstrate how to write spatial data to S3.
#' # They will not run automatically — copy and paste them into the console
#' # after loading `oppdal` (or the `sf` object of your choice), and ensure
#' # your AWS credentials and endpoint access are configured.
#'
#' \dontrun{
#' # Upload an sf object as a GeoPackage (requires layer name)
#' write_to_s3(oppdal, "geolink-test", "admin/Oppdal_municipality.gpkg", layer = "Oppdal")
#'
#' # Upload as GeoParquet
#' write_to_s3(oppdal, "geolink-test", "admin/Oppdal_municipality.parquet")
#' }
write_to_s3 <- function(obj, bucket, key,
                        layer = NULL,
                        endpoint = "s3-int-1.nina.no",
                        use_https = TRUE,
                        virtual = FALSE, ...) {
  ext <- tolower(tools::file_ext(key))
  tmp <- tempfile(fileext = paste0(".", ext))
  on.exit(unlink(tmp), add = TRUE)
  
  if (inherits(obj, "sf")) {
    if (ext == "gpkg") {
      if (is.null(layer)) stop("You must specify `layer` when writing to a GPKG.")
      sf::st_write(obj, tmp, layer = layer, driver = "GPKG", quiet = TRUE, ...)
    } else if (ext == "parquet") {
      if (!requireNamespace("sfarrow", quietly = TRUE)) stop("Package 'sfarrow' is required for writing parquet.")
      sfarrow::st_write_parquet(obj, tmp, ...)
    } else {
      stop("Unsupported vector file type: ", ext)
    }
  } else if (inherits(obj, "SpatRaster")) {
    if (!requireNamespace("terra", quietly = TRUE)) stop("Package 'terra' is required for writing raster.")
    terra::writeRaster(obj, tmp, ...)
  } else {
    stop("Unsupported object type: must be `sf` or `SpatRaster`.")
  }
  
  if (!requireNamespace("aws.s3", quietly = TRUE)) stop("The 'aws.s3' package is required for S3 upload.")
  success <- aws.s3::put_object(
    file = tmp,
    object = key,
    bucket = bucket,
    base_url = endpoint,
    use_https = use_https,
    region = "",
    virtual = virtual
  )
  
  return(success)
}


#' List files in an S3 bucket (optionally filtered by file type)
#'
#' Lists the contents of an S3 bucket, optionally filtering by path prefix and/or file extension.
#' This function is configured for NINA’s internal S3.
#'
#' @param bucket Character. The name of the S3 bucket (e.g., `"geolink-test"`).
#' @param prefix Character (optional). Filter by key prefix (e.g., `"grunnkart/"` to return only files in that folder).
#' @param file_type Character (optional). File extension filter (e.g., `".parquet"`, `".gpkg"`). Default is `NULL` (no filtering).
#' @param endpoint Character. S3 endpoint URL. Default is `"s3-int-1.nina.no"` (NINA internal).
#' @param use_https Logical. Whether to use HTTPS protocol (default `TRUE`).
#' @param virtual Logical. Use virtual-hosted-style URLs (default `FALSE`).
#' @param full_info Logical. If `TRUE`, returns a `data.frame` with metadata. If `FALSE`, returns a vector of keys only.
#'
#' @return A character vector or a `data.frame`, depending on `full_info`.
#' @export
#'
#' @examples
#' # List all files in the test bucket
#' list_from_s3("geolink-test")
#'
#' # List only .parquet files under the 'admin/' subfolder
#' list_from_s3("geolink-test", prefix = "admin/", file_type = ".parquet")
#'
#' # List all files in 'grunnkart/' but return just the keys (not metadata)
#' list_from_s3("geolink-test", prefix = "grunnkart/", full_info = FALSE)
list_from_s3 <- function(bucket,
                         prefix = NULL,
                         file_type = NULL,
                         endpoint = "s3-int-1.nina.no",
                         use_https = TRUE,
                         virtual = FALSE,
                         full_info = TRUE) {
  tryCatch({
    files <- aws.s3::get_bucket(
      bucket = bucket,
      prefix = prefix,
      base_url = endpoint,
      use_https = use_https,
      region = "",
      virtual = virtual
    )
    
    if (length(files) == 0) {
      message("No files found in bucket ", bucket,
              if (!is.null(prefix)) paste0(" with prefix '", prefix, "'."), call. = FALSE)
      return(if (full_info) data.frame() else character(0))
    }
    
    # Filter by file type (if specified)
    if (!is.null(file_type)) {
      if (!startsWith(file_type, ".")) {
        file_type <- paste0(".", file_type)  # allow "tif" as shorthand for ".tif"
      }
      
      files <- Filter(function(x) {
        grepl(paste0("\\", file_type, "$"), x$Key, ignore.case = TRUE)
      }, files)
    }
    
    if (length(files) == 0) {
      message("No matching files found.")
      return(if (full_info) data.frame() else character(0))
    }
    
    if (full_info) {
      df <- data.frame(
        Key = vapply(files, function(x) x$Key, character(1)),
        Size_MB = round(vapply(files, function(x) x$Size, numeric(1)) / 1024^2, 2),
        LastModified = vapply(files, function(x) as.character(x$LastModified), character(1)),
        stringsAsFactors = FALSE
      )
      return(df)
    } else {
      return(vapply(files, function(x) x$Key, character(1)))
    }
  },
  error = function(e) {
    stop(paste("Failed to list files in S3 bucket:", e$message))
  })
}