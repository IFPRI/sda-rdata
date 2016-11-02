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
library(tmap)

setwd("~/Projects/hc-data")
load("./SPAM/tmp/spam.RData")

# File preparation
var <- c("H", "A", "P", "Y", "V")
tech <- c("A", "H", "I", "L", "R", "S")

# Download and unzip SPAM2005v3r0 from Dropbox
url <- c(
  "https://www.dropbox.com/s/961dh4e4zq1cscj/spam2005V3r0_global_harv_area.zip?dl=1",
  "https://www.dropbox.com/s/qtgkj8qmx0eqgxy/spam2005V3r0_global_phys_area.zip?dl=1",
  "https://www.dropbox.com/s/vyb6tk4ip2bmabj/spam2005V3r0_global_prod.zip?dl=1",
  "https://www.dropbox.com/s/2mabctoyimx8tl6/spam2005V3r0_global_yield.zip?dl=1",
  "https://www.dropbox.com/s/il6vj493hxhm8cu/spam2005V3r0_global_val_prod.zip?dl=1")

url <- lapply(1:5, function(x) curl_download(url[x],
  paste0("./SPAM/2005v3r0/in/SPAM2005v3r0_", var[x], ".zip")))
url <- lapply(unlist(url), unzip, exdir="./SPAM/2005v3r0/in/")

# Keep only CSV files
file.remove(unlist(sapply(url, `[`, c(2,4,6,8,10,12))))
url <- unlist(sapply(url, `[`, c(1,3,5,7,9,11)))

# Load global CELL5M grid
grid <- raster("./Grids/hc_seq5m.asc")
grid.dt <- as.data.frame(grid, xy=T, long=T)
summary(grid.dt$value)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#    0 2333000 4666000 4666000 6998000 9331000

grid.dt <- data.table(grid.dt)
grid.dt[, layer := NULL]
setnames(grid.dt, c("X", "Y", "CELL5M"))

# Load SPAM metadata
crop <- fread("./SPAM/aux/cropList.csv")
crop <- crop[, .SD, .SDcols=2:7]
crop <- crop[SPAM_short!=""]

# Load meta for VoP variables
vi <- fread("./SPAM/aux/vopMeta.csv")

# Load and combine all CSV files
for (i in var) for (j in tech) {

  # Load SPAM from CSV
  f <- paste0("./SPAM/2005v3r0/in/spam2005V3r0_global_", i, "_T", j, ".csv")
  spam <- fread(f)

  if (i!="V") {
    # We already have metadata for VoP, so skip "V"
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

    # Append new vars to master metadata table
    vi <- rbind(vi, tmp, fill=T)

    # Rename data vars
    if (j=="A") { setnames(spam, tmp$SPAM_short, tmp$varCode)
    } else {
      setnames(spam, paste0(tmp$SPAM_short, "_", tolower(j)), tmp$varCode)
    }
  }

  # Add X,Y coordinates
  setkey(grid.dt, CELL5M)
  setkey(spam, cell5m)
  spam <- grid.dt[spam]
  setcolorder(spam, c(3:9, 1:2, 10:length(spam)))

  # Save to RDS for reuse (each file is ~15Mb)
  saveRDS(spam, file=str_replace(str_replace(f, ".csv", ".rds"), "/in/", "/rds/"))
}

rm(spam)

# In fact we should be able to combine all vars into 1 simple RDA file
r <- list.files("./SPAM/2005v3r0/rds", full.names=T)
r <- lapply(r, readRDS)
cell5m <- lapply(r, `[[`, "CELL5M")
sapply(cell5m, length)
sapply(r, function(x) x[, range(CELL5M)])
spam <- r[[1]][, .SD, .SDcols=c(1:9, 52:57)]
r <- lapply(r, function(x) x <- x[, .SD, .SDcols=-c(names(spam)[-1])])
r <- do.call(cbind, r)

setkey(spam, CELL5M)
setkey(r, CELL5M)
spam <- spam[r]
rm(r, cell5m)

# Verify that we have all the vars we expect
setkey(vi, varCode)
vi <- vi[names(spam)]
setkey(vi, cat, crop, tech)

# Save all vars (500Mb)
var <- names(spam)[(names(spam) %like% "i.CELL5M")]
spam[, (var) := NULL]
spam[, rec_type := NULL]
spam[, tech_type := NULL]
spam[, unit := NULL]
spam[, crea_date := NULL]
spam[, source := NULL]

# Rename GAUL vars for consistency with HCAPI
setnames(spam, c("name_cntr", "name_adm1", "name_adm2"), c("ADM0_NAME", "ADM1_NAME", "ADM2_NAME"))
setnames(spam, "iso3", "ISO3")
setnames(spam, "prod_level", "PROD_LEVEL")

# Save
save(spam, file="./SPAM/2005v3r0/rds/SPAM2005V3r0_global.rda", compress=T)


#####################################################################################
# also complete and save metadata table `vi`
# Best to use the same metadata schema as CELL5M (?) to make APIs compatible
# Export and fix metadata in MSExcel
write.csv(vi, "./SPAM/2005v3r0/rds/vi.csv", na="", row.names=F)
vi <- fread("./SPAM/2005v3r0/rds/vi.csv")

# More manual recodes
vi[cropCode=="", cropCode := NA]
vi[cat2=="robusta coffe", cat2 := "robusta coffee"]
vi[, unique(cat3)]
vi[cat3=="irrigated", cat3 := "irrigated system"]
vi[cat3=="rainfed", cat3 := "rainfed system"]
vi[cat3=="rainfed low inputs", cat3 := "rainfed low-input system"]
vi[cat3=="rainfed high inputs", cat3 := "rainfed high-input system"]
vi[cat3=="subsistence", cat3 := "subsistence system"]
vi[cat3=="irrigated", cat3 := "irrigated system"]

vi[!is.na(cropCode), varTitle := paste0(tools::toTitleCase(paste(cat2, cat1)), " - ", cat3)]
vi[cropCode %in% c("crop", "food", "nfood"), varTitle := paste0(tools::toTitleCase(paste(cat2, cat1)), " - ", cat3)]
vi[, unique(unit)]
vi[unit=="hectare", unit := "ha"]
vi[unit=="International $", unit := "int$"]
vi[unit=="International $ / hectare", unit := "int$/ha"]
vi[!is.na(cropCode), varTitle := paste0(varTitle, " (", unit, ", 2005)")]
vi[!is.na(cropCode), varLabel := tolower(paste0(cat1, " of ", cat2, " under ", cat3))]
vi[, varLabel := gsub("of crops", "of all crops", varLabel, fixed=T)]
vi[, varLabel := gsub("of food", "of food crops", varLabel, fixed=T)]
vi[, varLabel := gsub("of non-food", "of non-food crops", varLabel, fixed=T)]

vi[, `:=`(varDesc=NULL, citation=NULL, sources=NULL, mxdName=NULL, aggFunR=NULL)]
vi[!is.na(cropCode), varDesc := "Spatially disaggregated production statistics of circa 2005 using the Spatial Production Allocation Model (SPAM). Values are for 5 arc-minute grid cells."]
vi[!is.na(cropCode), citation := paste0('You, L., U. Wood-Sichra, S. Fritz, Z. Guo, L. See, and J. Koo. 2016. "', varTitle, '", International Food Policy Research Institute, Washington, DC. Available online at http://mapspam.info/data/.')]
vi[!is.na(cropCode), sources := "You, L., U. Wood-Sichra, S. Fritz, Z. Guo, L. See, and J. Koo. 2016. Spatial Production Allocation Model (SPAM) 2005 V3r0."]
vi[!is.na(cropCode), mxdName := "spamv3r0"]

# Rename GAUL vars for consistency with HCAPI
vi[varCode=="name_cntr", varCode := "ADM0_NAME"]
vi[varCode=="name_adm1", varCode := "ADM1_NAME"]
vi[varCode=="name_adm2", varCode := "ADM2_NAME"]
vi[varCode=="prod_level", varCode := "PROD_LEVEL"]
vi[is.na(varDesc), varDesc := varLabel]

save(vi, file="./SPAM/2005v3r0/rds/vi.rda")
# => this should be enough to run SPAM through HCAPI3 (eventually)

# Define auxiliary variables for CSV and metadata
g <- c("CELL5M", "PROD_LEVEL", "ISO3", "ADM0_NAME", "ADM1_NAME", "ADM2_NAME", "X", "Y")
cite <- 'You, L., U. Wood-Sichra, S. Fritz, Z. Guo, L. See, and J. Koo. 2016. "Spatial Production Allocation Model (SPAM) 2005 v3.0", September 2016. Available from http://mapspam.info/.'


#####################################################################################
# Helper - Generate PNG (no Data Packaging needed)
spamPNG <- function(var, pal="YlOrRd", file=paste0("./", var, ".png"), ...) {

  # Load country boundaries
  if (!"World" %in% ls()) data(World)

  # Convert to raster
  r <- SpatialPixelsDataFrame(spam[, .(X, Y)], data.frame(spam[, .SD, .SDcols=var]),
    proj4string=CRS("+init=epsg:4326"))
  r <- raster(r)

  # Cut to 98 percentile
  q98 <- quantile(r, probs=.98, na.rm=T)
  r[r > q98] <- q98

  # Tmap it
  m <- tm_shape(World) +
    tm_fill("white", border.col="grey70", lwd=.2) +
    tm_grid(n.x=7, n.y=6, projection="longlat", lwd=.1, col="grey70", labels.size=0) +
    tm_shape(r, is.master=T, projection="eck4") +

    # Using `cont` style not always best
    tm_raster(title=vi[var, paste(unit, 2005, sep=", ")],
      n=9, style="cont", palette=pal) +
    tm_shape(World) + tm_borders("grey70", lwd=.2) +

    tm_credits(str_wrap(vi[var, varDesc], 80),
      position=c(.5, 1.04), just=c("center", "top"), size=.55) +
    tm_logo("./SPAM/aux/logo.png", height=2,
      position=c(.5, .5), just=c("center", "center")) +
    tm_credits(str_wrap(cite, 116),
      position=c(.5, .08), just=c("center", "bottom"), size=.45) +

    tm_layout(
      title=vi[var, varTitle],
      title.size=.7,
      title.snap.to.legend=F,
      title.position=c("center", "top"),
      bg.color="#AEDFE5",
      outer.bg.color="white",
      earth.boundary=c(-180, 180, -70, 90),
      earth.boundary.color="white",
      earth.boundary.lwd=.4,
      space.color="white",
      frame=F,
      legend.position=c(.1, .2),
      legend.text.size=.45,
      legend.title.size=.6,
      attr.outside=T,
      #title.color="grey05",
      attr.color="grey20",
      legend.text.color="grey40",
      inner.margins=c(.04, .01, .08, .01),
      asp=6/3.3)

  save_tmap(m, file, width=6, height=4, units="in", ...)
  return(file)
}


#####################################################################################
# Helper - Generate Data Package auxiliary files
spamAux <- function(var, format=c("csv", "tif", "nc"), outdir="./") {

  # Auxiliary file names
  d <- paste0(outdir, c("META.csv", "README.md", "README.html", "datapackage.json"))

  # Retrieve metadata records
  setkey(vi, varCode)
  dt <- vi[c(g, var)]

  # Write to `META.csv`
  write.csv(dt, d[1], row.names=F, na="")

  # Write to `README.md`
  r <- readLines("/home/projects/hc-data/SPAM/aux/README.md")
  r <- gsub("$date$", Sys.Date(), r, fixed=T)
  write(r, d[2])

  # Knit to `README.html`
  rmarkdown::render(d[2])

  # Write to `datapackage.json`
  setkey(dt, varCode)
  dt <- dt[var]
  j <- list(
    name="IFPRI-SPAM-GLOBAL-V3r0",
    datapackage_version="1.0-beta-2",
    title="Spatial Production Allocation Model (SPAM) 2005 Version 3.0",
    description="The Spatial Production Allocation Model is an effective way to map detailed patterns of crop production using much less specific input data.",
    version="V3r0, Sep. 2016",
    last_updated=Sys.Date(),
    homepage="http://mapspam.info/",
    image="http://mapspam.info/wp-content/themes/mapspam/assets/img/images/logo_main_2x.png",
    contributors=list(
      list(name="Liang You", email="l.you@cgiar.org"),
      list(name="Ulrike Wood-Sichra", email="u.wood-sichra@cgiar.org"),
      list(name="Steffen Fritz", email="fritz@iiasa.ac.at"),
      list(name="Zhe Guo", email="z.guo@cgiar.org"),
      list(name="Linda See", email="see@iiasa.ac.at"),
      list(name="Jawoo Koo", email="j.koo@cgiar.org")),
    sources=list(
      name="IFPRI/SPAM",
      web="http://mapspam.info/",
      email="info@mapspam.info"),
    keywords=c("agriculture", "farming", "production", "yield", "productivity", "climate change",
      "maize", "wheat", "cassava", "fruits", "vegetables", "pulses", "cereals", "legumes"),
    license=list(type="ODC-BY-1.0", url="http://opendatacommons.org/licenses/by"),
    publishers="International Food Policy Research Institute (IFPRI)",
    resources=list(
      name=dt[, varCode],
      title=dt[, varTitle],
      description=dt[, varDesc],
      # This assumes that data files have been generated in the same `outdir`
      path=setdiff(list.files(outdir, paste0("^.*\\.", format, "$")),
        c("META.csv", "README.md", "README.html")),
      format=switch(format, csv="CSV", nc="netCDF", tif="GeoTIFF"),
      mediatype=switch(format,
        csv="text/csv",
        nc="application/x-netcdf",
        rds="application/octet-stream",
        tif="image/tiff"))
  )

  write(jsonlite::toJSON(j, dataframe="rows", pretty=T, auto_unbox=T), file=d[4])
  return(d)
}


#####################################################################################
# Helper - Generate complete Data Packages (ZIP)
spamDP <- function(var, format=c("tif", "nc", "csv"), path="./", prefix="spam2005v3r0_") {

  if (format=="csv") {
    # Pull all columns
    r <- spam[, .SD, .SDcols=c(g, var)]
  } else {
    # Convert to raster
    r <- SpatialPixelsDataFrame(spam[, .(X, Y)], data.frame(spam[, .SD, .SDcols=var]),
      proj4string=CRS("+init=epsg:4326"))
    r <- raster(r)
  }

  switch(format,
    # CSV
    csv = write.csv(r, paste0("./tmp/", prefix, var, ".csv")),
    # TIF
    tif = writeRaster(r, paste0("./tmp/", prefix, var, ".tif"), overwrite=T,
      options=c("INTERLEAVE=BAND", "TFW=YES", "ESRI_XML_PAM=YES")),
    # netCDF
    nc = writeRaster(r, paste0("./tmp/", prefix, var, ".nc"), overwrite=T,
      xname="lon", yname="lat",
      varname=vi[var, varCode], varunit=vi[var, unit], longname=vi[var, varTitle])
  )

  # Make ZIP packages
  f <- switch(format,
    csv=paste0(path, prefix, var, ".csv.zip"),
    tif=paste0(path, prefix, var, ".tif.zip"),
    nc=paste0(path, prefix, var, ".nc.zip"))

  spamAux(var, format, "./tmp/")

  # Also add License.pdf
  file.copy("./SPAM/aux/License.pdf", "./tmp/License.pdf")
  zip(f, list.files("./tmp/", full.names=T), flags="-9Xjm", zip="zip")
  return(f)
}


#####################################################################################
# Package SPAM 2005 V3r0
#####################################################################################
load("./SPAM/2005v3r0/rds/SPAM2005V3r0_global.rda")
load("./SPAM/2005v3r0/rds/vi.rda")

# Generate all PNG
for (i in vi[genRaster==T &
    cat3 %in% c("all systems", "rainfed system", "irrigated system"), varCode]) {

  # Choose color palette
  pal <- switch(vi[i, cat3],
    `all systems`="YlOrBr",
    `irrigated system`="GnBu",
    `rainfed system`="YlOrRd",
    `subsistence system`="BuPu")

  # Plot
  spamPNG(i, pal,
    file=paste0("./SPAM/2005v3r0/png/spam2005v3r0_", i, ".png"), dpi=600)

  # Generate all TIF, NC, CSV
  spamDP(i, "csv", "./SPAM/2005v3r0/csv/")
  spamDP(i, "tif", "./SPAM/2005v3r0/tiff/")
  spamDP(i, "nc", "./SPAM/2005v3r0/nc/")
}



# Save all
rm(tmp, i, j, var, spam, World, pal, m, grid.dt)
save.image("./SPAM/tmp/spam.RData")


