# GeoLink

**GeoLink** is an internal R package for working with spatial data stored in NINA’s physical and cloud architecture. It provides utilities for reading and listing spatial files into your R workflow.

---

## ✨ Features

- Read geospatial data* from **S3 buckets** into your R environment  
- Write geospatial data* to **S3 buckets** from R  
- List available files in individual S3 buckets  
- **NEW**: Read and list spatial data directly from `/data/R/GeoSpatialData` on NINA's R drive (RGSD)  

> *Currently supports `.parquet`, `.gpkg`, `.tif`, `.shp`, and `.gdb` formats

---

## 📥 Installation

You can install GeoLink from GitHub using:

```r
# If remotes isn't installed yet
install.packages("remotes")

# Install GeoLink from your GitHub
remotes::install_github("jennyhansen/GeoLink")
```

---

## 📦 Functions

### Cloud (S3) functions
- `read_from_s3()` – Read spatial data from an S3 bucket  
- `write_to_s3()` – Write spatial data to an S3 bucket  
- `list_s3_files()` – List files available in a given bucket path

### R-drive functions (RGSD)
- `list_from_rgsd()` – List files and folders in `/data/R/GeoSpatialData`  
- `import_from_rgsd()` – Read spatial data from the R drive into R (vector or raster)

---

## 🚀 Example Usage

```r
# List contents of the top-level RGSD directory
list_from_rgsd()

# List only folders within a subdirectory
list_from_rgsd("LandCover", type = "folder")

# List all items in a known raster directory
list_from_rgsd("Elevation/Fenoscandia_DEM_10")

# Import a raster from the R drive
import_from_rgsd("Elevation/Fenoscandia_DEM_10/Original/dem_10m_fenoscandia.tif")

# Import a layer from a GeoPackage
import_from_rgsd("LandCover/vegetation.gpkg", layer = "dominant_types")

# Import a shapefile using a full path
import_from_rgsd("/data/R/GeoSpatialData/Buildings/.../N50_TouristCabins.shp")
```

> Note: These functions require that you're running R on a NINA server (`ninrstudio`, `ningis`, `lipgis`, or `liprstudio`).

---

## 🛠 Requirements

- R ≥ 4.1  
- Required packages:
  - `sf` – for vector data  
  - `terra` – for raster data  
  - `sfarrow` – for GeoParquet  
  - `arrow` – used internally  
  - `aws.s3` – for S3 integration  
  - `remotes` – for installation  

---

## 📫 Contact

For help with GeoLink, contact the package maintainer or submit an issue.


