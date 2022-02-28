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
product = "TRMM/3B42"
band = "precipitation"

# store output in the R temporary directory
directory = tempdir()

# years to cycle over
years <- 2001:2019

site_data <- apply(sites, 1, function(site){

  # set site location
  location <- paste(
    site['latitude'],
    site['longitude']
  )

  # make the gee_subset.py python call
  # time the duration of the call for reporting

  yearly_data <- lapply(years, function(year){

    # set dates
    start_date <- sprintf("%s-01-01", year)
    end_date <- sprintf("%s-12-31", year)

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

    return(df)
  })

  # bind rows (from nested list)
  yearly_data <- bind_rows(yearly_data)

  # add site data
  yearly_data$site <- site['site']
  yearly_data$veg_type <- site['veg_type']

  # summarize data before returning it
  # otherwise the resulting initial data
  # frame is rather large
  yearly_data <- yearly_data %>%
    mutate(
      date = as.Date(date, "%Y-%m-%d %H:%M:%S"),
      precipitation = ifelse(is.na(precipitation), 0, precipitation)
    ) %>%
    group_by(site, date) %>%
    summarize(
      latitude = latitude[1],
      longitude = longitude[1],
      product = product[1],
      veg_type = veg_type[1],
      precipitation = sum(precipitation) * 3
    )

  return(yearly_data)
})

# collect all data in one data frame
site_data <- bind_rows(site_data)

# save data
saveRDS(site_data, "data/TRMM.rds", compress = "xz")
