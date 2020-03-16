write.csv(dat, "zoom_v4/data1.csv",row.names = FALSE)

library(jsonlite)
library(tidyverse)

dat <- read_json("https://usafactsstatic.blob.core.windows.net/public/2020/coronavirus-timeline/allData.json",simplifyVector = TRUE)

confirmed <- dat %>%
  transform(confirmed = strsplit(as.character(confirmed), ",")) %>%
  unnest(confirmed) %>% mutate(confirmed = str_remove(confirmed,"\\)"),
                            confirmed = str_remove(confirmed,"c\\("),
                            confirmed = str_remove(confirmed," "),
                            confirmed = as.numeric(confirmed)) %>% 
  select(-deaths,-recovered)

deaths <- dat %>%
  transform(deaths = strsplit(as.character(deaths), ",")) %>%
  unnest(deaths) %>% mutate(deaths = str_remove(deaths,"\\)"),
                            deaths = str_remove(deaths,"c\\("),
                            deaths = str_remove(deaths," "),
                            deaths = as.numeric(deaths)) %>% 
  select(-confirmed,-recovered)

recovered <- dat %>%
  transform(recovered = strsplit(as.character(recovered), ",")) %>%
  unnest(recovered) %>% mutate(recovered = str_remove(recovered,"\\)"),
                            recovered = str_remove(recovered,"c\\("),
                            recovered = str_remove(recovered," "),
                            recovered = as.numeric(recovered)) %>% 
  select(-deaths,-confirmed)


days <- rep(seq(as.Date("2020-01-23"),as.Date("2020-01-22")+length(dat$deaths[[1]]),by="days"),length(dat[[1]]))

confirmed <- confirmed %>% mutate(Date = days) %>% pivot_wider(names_from = Date, values_from = confirmed)
deaths <- deaths %>% mutate(Date = days) %>% pivot_wider(names_from = Date, values_from = deaths)
recovered <- recovered %>% mutate(Date = days) %>% pivot_wider(names_from = Date, values_from = recovered)

write.csv(confirmed, "COVID_data/confirmed.csv",row.names = FALSE)
write.csv(deaths, "COVID_data/confirmed.csv",row.names = FALSE)
write.csv(recovered, "COVID_data/confirmed.csv",row.names = FALSE)