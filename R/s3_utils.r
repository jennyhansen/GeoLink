#' Read GeoParquet spatial data from S3
#'
#' @param s3_path The object key (path to the file in the bucket)
#' @param bucket The S3 bucket name
#' @param endpoint The S3 endpoint (default is NINA's internal S3)
#' @param use_https Logical, use HTTPS for the connection (default TRUE)
#' @param virtual Logical, use virtual-hosted-style URLs (default FALSE)
#' @param ... Additional arguments passed to `sfarrow::st_read_parquet()`
#'
#' @return An `sf` object read from S3
#' @export
read_geoparquet_from_s3 <- function(s3_path,
                                    bucket,
                                    endpoint = "s3-int-1.nina.no",
                                    use_https = TRUE,
                                    virtual = FALSE,
                                    ...) {
  stopifnot(requireNamespace("arrow"),
            requireNamespace("sfarrow"),
            requireNamespace("sf"))
  
  tryCatch({
    # Set up S3 file system using arrow
    s3 <- arrow::s3_bucket(
      bucket = bucket,
      endpoint_override = endpoint,
      anonymous = TRUE,
      scheme = ifelse(use_https, "https", "http")
    )
    
    # Construct full path
    s3_file <- s3$path(s3_path)
    
    # Read into sf
    sf_data <- sfarrow::st_read_parquet(s3_file, ...)
    
    if (!inherits(sf_data, "sf")) {
      stop("The file was read but did not return a valid `sf` object.")
    }
    
    return(sf_data)
  },
  error = function(e) {
    stop(paste("Failed to read GeoParquet from S3:", e$message))
  })
}

#' List GeoParquet files in an S3 bucket (with optional prefix)
#'
#' @param bucket The S3 bucket name
#' @param prefix Optional prefix to filter files (e.g. "sam_env/")
#' @param endpoint The S3 endpoint (default is NINA's internal S3)
#' @param use_https Logical, use HTTPS for the connection (default TRUE)
#' @param virtual Logical, use virtual-hosted-style URLs (default FALSE)
#' @param full_info Logical, return a data.frame with file size and date (default TRUE)
#'
#' @return A character vector of file keys (or a data.frame if `full_info = TRUE`)
#' @export
list_s3_geoparquet_files <- function(bucket,
                                     prefix = NULL,
                                     endpoint = "s3-int-1.nina.no",
                                     use_https = TRUE,
                                     virtual = FALSE,
                                     full_info = TRUE) {
  stopifnot(requireNamespace("aws.s3"))
  
  # Attempt listing
  tryCatch({
    files <- aws.s3::get_bucket(
      bucket = bucket,
      prefix = prefix,
      base_url = endpoint,
      use_https = use_https,
      region = "",
      virtual = virtual
    )
    
    # No files?
    if (length(files) == 0) {
      message("No files found in bucket ", bucket,
              if (!is.null(prefix)) paste0(" with prefix '", prefix, "'."), call. = FALSE)
      return(if (full_info) data.frame() else character(0))
    }
    
    # Filter to .parquet files
    parquet_files <- Filter(function(x) grepl("\\.parquet$", x$Key), files)
    
    if (length(parquet_files) == 0) {
      message("📭 No GeoParquet (.parquet) files found.")
      return(if (full_info) data.frame() else character(0))
    }
    
    if (full_info) {
      df <- data.frame(
        Key = vapply(parquet_files, function(x) x$Key, character(1)),
        Size_MB = round(vapply(parquet_files, function(x) x$Size, numeric(1)) / 1024^2, 2),
        LastModified = vapply(parquet_files, function(x) as.character(x$LastModified), character(1)),
        stringsAsFactors = FALSE
      )
      return(df)
    } else {
      return(vapply(parquet_files, function(x) x$Key, character(1)))
    }
  }, error = function(e) {
    stop(paste("Failed to list files in S3 bucket:", e$message))
  })
}
