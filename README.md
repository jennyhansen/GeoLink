
# GeoLink

**GeoLink** is an internal R package for working with spatial data stored in NINA’s physical and cloud architecture. It provides utilities for reading and listing spatial files into your R workflow.

## ✨ Features

- Read geospatial data\* from S3 buckets into R environment
- Write geospatial data\* to S3 buckets from R environment
- List available files in individual buckets 

\* currently .parquet, .gpkg, and .tif file types are available

## 📦 Installation

```r
remotes::install_github("jennyhansen/GeoLink")

