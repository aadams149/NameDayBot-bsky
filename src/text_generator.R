

library(dplyr)
#library(mapdeck)
#capitals <-
#  mapdeck::capitals %>%
#  mutate(
#    country = case_when(
#      country == "Czech Republic" ~ "Czechia",
#      country == "United States\n        of America" ~ "United States",
#      TRUE ~ country
#    )
#  )
# write.csv(capitals, "capitals.csv")

capitals <-
  read.csv("capitals.csv")
library(countrycode)
library(lutz)

today <-
  Sys.time()# + lubridate::days(1)

day <-
  as.character(lubridate::day(today))

month <-
  as.character(lubridate::month(today))

library(jsonlite)

url <- "https://raw.githubusercontent.com/xnekv03/nameday-api/refs/heads/master/json/namedays.json"
req <- readLines(url, warn = FALSE)

# Match the JSON element within the array for this day and month
regex_pattern <- paste0("\\{[^\\{\\}]*\"day\"\\s*:\\s*", day,
                        ",\\s*\"month\"\\s*:\\s*", month,
                        "[^\\{\\}]*\\}")

# Collapse to single string
json_text <- paste(req, collapse = "")
rm(req)

json_text <- stringr::str_extract(json_text, regex_pattern)

# Remove any trailing comma before a closing curly brace `}`
json_text <- gsub(",\\s*}", "}", json_text)

# Read as JSON
namedays <- fromJSON(json_text)
# Convert to dataframe
namedays <- as.data.frame(namedays)
# Restructure and pivot
namedays <- 
  namedays %>%
  dplyr::select(!c(day, month)) %>%
  tidyr::pivot_longer(colnames(.), names_to = "country_code",
                      values_to = "names") %>%
  dplyr::filter(country_code != "ru") %>%
  dplyr::left_join(
    countrycode::codelist %>%
      dplyr::select("country_code" = cctld, "country" = `country.name.en`,
                    "flag" = `unicode.symbol`) %>%
      dplyr::mutate(country_code = substring(country_code, 2, 3))
  ) %>%
  dplyr::left_join(capitals) %>%
  dplyr::mutate(time_zone =
                  lutz::tz_lookup_coords(lat = lat, lon = lon, method = "fast"))

time_now <-
  namedays %>%
  rowwise() %>%
  do(time_now = lubridate::hour(lubridate::with_tz(
    time = today, tzone = .$time_zone
  ))) %>%
  unlist()

namedays <-
  namedays %>%
  cbind.data.frame(time_now) %>%
  filter(time_now == 9, names != "n/a") %>%
  mutate(name_str = paste0(country," ",flag,": ", names, "\n")) %>%
  arrange(country)

if (nrow(namedays) == 0) {
  errorCondition("No names to publish right now")
} else{
  post_date <-
    format(today + 1, format = "%B %d, %Y")
  for (ii in 1:nrow(namedays)) {
    message <-
      paste0(
        "It's ",
        post_date,
        ", the name day for these names:\n\n",
        paste(paste(namedays$name_str[[ii]], collapse = "\n")),
        "\nHappy name day!"
      )
    
    filename <-
      paste0("posts/", namedays$country[ii], "_post.txt")
    fileConn <-
      file(filename)
    
    writeLines(enc2utf8(message), fileConn)
    
    close(fileConn)
    
  }
}