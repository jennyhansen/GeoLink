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

#' Write spatial or raster data to S3
#'
#' Supports writing `.parquet` (default for sf), `.gpkg`, and `.tif` (for raster).
#'
#' @param data An `sf` object or `SpatRaster`
#' @param bucket The S3 bucket name
#' @param object_key The object key (e.g. "folder/filename.tif")
#' @param file_type Optional. File extension (".parquet", ".gpkg", ".tif"). If NULL, inferred from object_key.
#' @param endpoint The S3 endpoint (default is NINA's internal S3)
#' @param use_https Use HTTPS (default TRUE)
#' @param virtual Use virtual-hosted-style URLs (default FALSE)
#' @param ... Additional arguments passed to the writing function
#'
#' @return TRUE if upload succeeds
#' @export
write_to_s3 <- function(data,
                        bucket,
                        object_key,
                        file_type = NULL,
                        endpoint = "s3-int-1.nina.no",
                        use_https = TRUE,
                        virtual = FALSE,
                        ...) {
  tryCatch({
    # Auto-detect file type from object_key if not given
    if (is.null(file_type)) {
      ext <- tools::file_ext(object_key)
      if (ext == "") stop("Cannot infer file type from object_key.")
      file_type <- paste0(".", tolower(ext))
    }
    
    # Create temp file
    tmp <- tempfile(fileext = file_type)
    on.exit(unlink(tmp), add = TRUE)
    
    # Dispatch based on file type and object class
    if (file_type == ".parquet") {
      if (!inherits(data, "sf")) stop("Must be `sf` data format to write .parquet")
      sfarrow::st_write_parquet(data, dsn = tmp, ...)
    } else if (file_type == ".gpkg") {
      if (!inherits(data, "sf")) stop("Must be `sf` data format to write .gpkg")
      sf::st_write(data, dsn = tmp, quiet = TRUE, ...)
    } else if (file_type == ".tif") {
      if (!inherits(data, "SpatRaster")) stop("Must be a `SpatRaster` to write .tif")
      terra::writeRaster(data, filename = tmp, overwrite = TRUE, ...)
    } else {
      stop("Unsupported file type. Only .parquet, .gpkg, and .tif are supported.")
    }
    
    # Upload to S3
    result <- aws.s3::put_object(
      file = tmp,
      object = object_key,
      bucket = bucket,
      base_url = endpoint,
      use_https = use_https,
      region = "",
      virtual = virtual,
      multipart = TRUE,
      ...
    )
    
    if (!isTRUE(result)) stop("Upload failed.")
    return(TRUE)
  },
  error = function(e) {
    stop(paste("Failed to write to S3:", e$message))
  })
}

#' List files in an S3 bucket (optionally filtered by file type)
#'
#' @param bucket The S3 bucket name
#' @param prefix Optional prefix to filter files (e.g. "data/")
#' @param file_type Optional file extension to filter by (e.g. ".parquet", ".gpkg", ".tif", ".txt"). Default is NULL (no filtering).
#' @param endpoint The S3 endpoint (default is NINA's internal S3)
#' @param use_https Logical, use HTTPS for the connection (default TRUE)
#' @param virtual Logical, use virtual-hosted-style URLs (default FALSE)
#' @param full_info Logical, return a data.frame with metadata (default TRUE)
#'
#' @return A character vector of file keys (or a data.frame if `full_info = TRUE`)
#' @export
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