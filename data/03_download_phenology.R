# load libraries
library(tidyverse)

# load site locations

sites <- read.table(
  "data/site_meta_data.csv",
  sep = ",",
  header = TRUE)

# change this depending on system settings
python_path = "/usr/bin/python3"
#python_path = "/usr/local/bin/python" # OSX

# clone the gee_subset project
# relies on git being installed
# and will work out of the box for most
# on OSX or Linux.
#
# basic gee_subset requirements apply
# mainly, having a working GEE python API install
path = "src/gee_subset/"

if(!dir.exists(path)){
  system(sprintf("git clone https://github.com/bluegreen-labs/gee_subset.git %s", path))
}

# set product parameters, such as
# product name, band(s) to query, start and end date of the range
# and the lcoation
product = "MODIS/006/MCD12Q2"
band = "MidGreenup_1 MidGreendown_1 MidGreenup_2 MidGreendown_2"

# store output in the R temporary directory
directory = tempdir()

seconds_to_date <- function(x){
  days <- seq.Date(as.Date("1970-01-01"), Sys.Date(), by = "day")
  if(is.na(x)){
    return(NA)
  }else{
    return(days[x])
  }
}

site_data <- apply(sites, 1, function(site){

  # set site location
  location <- paste(
    site['latitude'],
    site['longitude']
  )

  # make the gee_subset.py python call
  # time the duration of the call for reporting
  start_date <- "2001-01-01"
  end_date <- "2019-12-31"

  system(sprintf(
    "%s %s/src/gee_subset/gee_subset.py -p %s -b %s -s %s -e %s -l %s -d %s -sc 30",
    python_path,
    path,
    product,
    band,
    start_date,
    end_date,
    location,
    directory
  ), wait = TRUE)
  end = Sys.time()

  # read in the data stored in the temporary directory
  df <- try(read.table(
    paste0(directory,
           "/site_",
           tail(unlist(strsplit(product, "[/]")), n=1),
           "_",
           gsub(" ","_", band),
           "_gee_subset.csv"
    ),
    sep = ",", header = TRUE, stringsAsFactors = FALSE))

  # add site data
  df$site <- site['site']
  df$veg_type <- site['veg_type']

  # summarize data before returning it
  # otherwise the resulting initial data
  # frame is rather large
  df <- df %>%
    mutate(
      across(starts_with("Mid"), seconds_to_date)
    )

  return(df)
})

# collect all data in one data frame
site_data <- bind_rows(site_data) %>%
  select(
    -id
  )

# save data
saveRDS(site_data, "data/phenology.rds", compress = "xz")
