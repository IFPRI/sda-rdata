#####################################################################################
# Title:   Generate SPAM2003v3r0 Data Packages
# Date:    October 2015
# Project: mapspam.info
# Author:  Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(data.table)
library(raster)
library(curl)
library(foreign)
library(stringr)

setwd("~/Projects/hc-data")
load("./SPAM/tmp/spam.RData")

# Download SPAM2005v3r0 from Dropbox
var <- c("H", "A", "P", "Y", "V")
tech <- c("A", "H", "I", "L", "R", "S")

url <- c(
  "https://www.dropbox.com/s/961dh4e4zq1cscj/spam2005V3r0_global_harv_area.zip?dl=1",
  "https://www.dropbox.com/s/qtgkj8qmx0eqgxy/spam2005V3r0_global_phys_area.zip?dl=1",
  "https://www.dropbox.com/s/vyb6tk4ip2bmabj/spam2005V3r0_global_prod.zip?dl=1",
  "https://www.dropbox.com/s/2mabctoyimx8tl6/spam2005V3r0_global_yield.zip?dl=1",
  "https://www.dropbox.com/s/il6vj493hxhm8cu/spam2005V3r0_global_val_prod.zip?dl=1")

url <- lapply(1:5, function(x) curl_download(url[x],
  paste0("./SPAM/SPAM2005v3r0/in/SPAM2005v3r0_", var[x], ".zip")))
url <- lapply(unlist(url), unzip, exdir="./SPAM/SPAM2005v3r0/in/")

file.remove(unlist(sapply(url, `[`, c(2,4,6,8,10,12))))
url <- unlist(sapply(url, `[`, c(1,3,5,7,9,11)))

# Load global CELL5M grid
grid <- raster("./SPAM/aux/hc_seq5m.asc")
grid.dt <- as.data.frame(grid, xy=T, long=T)
summary(grid.dt$value)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#    0 2333000 4666000 4666000 6998000 9331000

grid.dt <- data.table(grid.dt)
grid.dt[, layer := NULL]
setnames(grid.dt, c("X", "Y", "CELL5M"))

# Load SPAM metadata from HCAPI
crop <- fread("./SPAM/aux/cropList.csv")
crop <- crop[, .SD, .SDcols=2:7]
crop <- crop[SPAM_short!=""]

# Load meta for VoP variables
vi <- fread("./SPAM/aux/vopMeta.csv")


for (i in var[1:4]) for (j in tech) {

  if (i=="H" & j=="A") next()
  if (i=="H" & j=="H") next()

  # Load SPAM from CSV
  f <- paste0("./SPAM/SPAM2005v3r0/in/spam2005V3r0_global_", i, "_T", j, ".csv")
  spam <- fread(f)

  # Construct metadata table
  tmp <- data.table(varCode=names(spam))
  tmp[, format := unlist(sapply(spam, class))]
  tmp[, varCode := str_replace(varCode, paste0("_", tolower(j)), "")]
  setkey(tmp, varCode)
  setkey(crop, SPAM_short)
  tmp$crop <- crop[tmp][, SPAM_long]
  tmp <- tmp[!is.na(crop)]
  tmp[, cat := switch(i, H="Harvested Area", A="Physical Area", P="Production", Y="Yield")]
  tmp[, tech := switch(j, A="all systems", H="rainfed high inputs", I="irrigated",
    L="rainfed low inputs", R="rainfed", S="subsistence")]
  tmp[, unit := switch(i, H="ha", A="ha", P="mt", Y="kg/ha")]
  tmp[, SPAM_short := varCode]
  tmp[, varCode := tolower(paste0(varCode, "_", i, "_", j))]

  # Append vars to main metadata table
  vi <- rbind(vi, tmp, fill=T)

  # Rename data vars
  setnames(spam, paste0(tmp$SPAM_short, "_", tolower(j)), tmp$varCode)

  # Add X,Y coordinates
  setkey(grid.dt, CELL5M)
  setkey(spam, cell5m)
  spam <- grid.dt[spam]
  setcolorder(spam, c(3:9, 1:2, 10:57))

  # Save to rds
  saveRDS(spam, file=str_replace(str_replace(f, ".csv", ".rds"), "/in/", "/rds/"))

  # Convert to multi-band raster
  spam <- spam[grid.dt]
  spam <- SpatialPixelsDataFrame(spam[, .(i.X, i.Y)],
    data.frame(spam[, .SD, .SDcols=tmp$varCode]),
    proj4string=CRS("+init=epsg:4326"))

  # Save raster to disk in NetCDF format
  writeRaster(brick(spam),
    filename=str_replace(str_replace(f, ".csv", ".nc"), "/in/", "/"), format="CDF",
    longname="SPAM 2005 v3r0", varname=paste(i, j, sep="_"), zname="SPAM crop")

}

# Treat VoP tables separately
rm(spam)


# Complete the metadata table `vi`



# Generate auxiliary files
datapackage <- function(var, format=NULL, dir="./") {

  # Auxiliary file names
  d <- paste(dir, c("meta.csv", "README.md", "datapackage.json"), sep="/")

  # Retrieve metadata records
  dt <- vi[var][, list(
    code=varCode,
    label=varLabel,
    description=varDesc,
    type=type,
    unit=unit,
    version=version,
    aggFormula=aggFunR,
    period=ifelse(is.na(yearEnd), year, paste(year, yearEnd, sep=" - ")),
    category=cat1,
    subcategory=cat2,
    item=cat3,
    source=ifelse(is.na(sources), sourceMini, sources),
    contact=owner,
    downloadedOn=format(Sys.Date(), "%m/%d/%Y"),
    citation=citation)]

  # Write to `meta.csv`
  write.csv(dt, d[1], row.names=F, na="")

  # Add Tabular Data Package specifications or stop here
  if( missing(format) ) return(d[1])
  format <- tolower(format)
  if( !format %in% c("csv", "nc", "tif") ) return(d[1])

  # Write custom `README.md` and `README.html`
  r <- readLines(system.file("./www/readme.md", package="hcapi3"))
  r <- gsub("$date$", Sys.Date(), r, fixed=T)
  r <- gsub("$json$", toJSON(list(var=var)), r, fixed=T)
  write(r, d[2])

  # Write `datapackage.json`
  j <- list(
    name="IFPRI-CELL5M-SSA",
    datapackage_version="1.0-beta-2",
    title="CELL5M: A Multidisciplinary Geospatial Database for Africa South of the Sahara",
    description="HarvestChoice CELL5M-SSA is an on-line database of over 750 biophysical and socio-economic indicators for Africa south of the Sahara. All indicators are currently geo-referenced to a spatial resolution of 5-arc-minute (equivalent to around 10 sq. km. at the Equateur).",
    version=as.character(packageVersion("hcapi3")),
    last_updated=Sys.Date(),
    homepage="http://harvestchoice.org/",
    image="http://harvestchoice.github.io/hc-api3/img/hc-logo-white.png",
    sources=list(
      name="IFPRI/HarvestChoice",
      web="http://harvestchoice.org/",
      email="info@harvestchoice.org"),
    keywords=c("climate", "agriculture", "poverty", "market access", "nutrition",
      "Africa South of the Sahara", "climate adaptation", "farming", "soils", "livestock"),
    license="ODC-BY-1.0",
    publishers="International Food Policy Research Institute (IFPRI)",
    resources=list(
      # This assumes that data files have been generated in the same `dir`
      # and that there is only one, not perfect but well...
      name="CELL5M-SSA Data Extracts",
      path=setdiff(list.files(dir, paste0("^.*\\.", format, "$")), "meta.csv"),
      format=switch(format,
        csv="csv",
        nc="netCDF",
        tif="GeoTIFF"),
      mediatype=switch(format,
        csv="text/csv",
        nc="application/x-netcdf",
        rds="application/octet-stream",
        tif="image/tiff"),
      schema=list(fields=dt[, .(
        name=code,
        type=ifelse(type=="class", "string", "numeric"),
        description=paste(label, description, sep=" - "),
        unit=unit)]))
  )

  write(toJSON(j, dataframe="rows", pretty=T, auto_unbox=T), file=d[3])
  return(d)
}


# Generate PNG
spamPNG <- function(x, path) {

}







# Generate data packages
dp <- function(x, path) {


  # TIF

  # CSV

  # netCDF

}





# Make ZIP packages
for (i in cat) {
  vars <- vi[cat2==i, varCode]
  for (f in c("asc", "csv", "dta")) {
    # Generate CSV, STATA, and ESRI ASCII grid with README and TERMS
    tmp <- hcapi(vars, format=f, dir=tempdir())
    path <- paste0("../www/bulk/", gsub(" ", "_", tolower(i), fixed=T), "-",
      format(Sys.Date(), "%y.%m.%d"), ".", f, ".zip")
    zip(path, tmp, flags="-9Xjm", zip="zip")
  }
}




# Save all
rm(tmp, i, j, spam)
save.image("./SPAM/tmp/spam.RData")

