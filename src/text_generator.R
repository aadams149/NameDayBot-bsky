

library(dplyr)
library(mapdeck)
capitals <-
  mapdeck::capitals %>%
  mutate(
    country = case_when(
      country == "Czech Republic" ~ "Czechia",
      country == "United States\n        of America" ~ "United States",
      TRUE ~ country
    )
  )

library(countrycode)
library(lutz)
library(httr2)

today <-
  Sys.time()

day <-
  as.character(lubridate::day(today))

month <-
  as.character(lubridate::month(today))

url <-
  paste0("https://nameday.abalin.net/api/V2/date?day=",
         day,
         "&month=",
         month)

req <-
  request(url) %>%
  req_headers("Accept" = "application/json") %>%
  req_perform() %>%
  resp_body_json()

req1 <-
  req$data

rm(req)

df <- as.data.frame(matrix(unlist(req1), nrow = length(req1))) %>%
  dplyr::mutate("country_code" = names(req1)) %>%
  dplyr::rename("names" = V1) %>%
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
  df %>%
  rowwise() %>%
  do(time_now = lubridate::hour(lubridate::with_tz(
    time = today, tzone = .$time_zone
  ))) %>%
  unlist()

df <-
  df %>%
  cbind.data.frame(time_now) %>%
  filter(time_now == 0, names != "n/a") %>%
  mutate(name_str = paste0(country," ",flag,": ", names, "\n")) %>%
  arrange(country)

if (nrow(df) == 0) {
  errorCondition("No names to publish right now")
} else{
  post_date <-
    format(today + 1, format = "%B %d, %Y")
  for (ii in 1:nrow(df)) {
    message <-
      paste0(
        "It's ",
        post_date,
        ", the name day for these names:\n\n",
        paste(paste(df$name_str[[ii]], collapse = "\n")),
        "\nHappy name day!"
      )
    
    filename <-
      paste0("posts/", df$country[ii], "_post.txt")
    fileConn <-
      file(filename)
    
    writeLines(enc2utf8(message), fileConn)
    
    close(fileConn)
    
  }
}
