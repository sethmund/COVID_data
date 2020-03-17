library(jsonlite)
library(tidyverse)

dat <- read_json("https://usafactsstatic.blob.core.windows.net/public/2020/coronavirus-timeline/allData.json",simplifyVector = TRUE)

days <- rep(seq(as.Date("2020-01-23"),as.Date("2020-01-22")+length(dat$confirmed[[1]]),by="days"),length(dat[[1]]))

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
                            deaths = as.numeric(deaths),
                            Date = as.Date(days)) %>% 
  select(-confirmed,-recovered)

recovered <- dat %>%
  transform(recovered = strsplit(as.character(recovered), ",")) %>%
  unnest(recovered) %>% mutate(recovered = str_remove(recovered,"\\)"),
                            recovered = str_remove(recovered,"c\\("),
                            recovered = str_remove(recovered," "),
                            recovered = as.numeric(recovered),
                            Date = as.Date(days)) %>% 
  select(-deaths,-confirmed)

#Give it the pivot

confirmed <- confirmed %>% 
  mutate(Date = days) %>% 
  pivot_wider(names_from = Date, 
              values_from = confirmed,
              values_fn = list(confirmed = sum))

deaths <- deaths %>% 
  mutate(Date = days) %>% 
  pivot_wider(names_from = Date, 
              values_from = deaths,
              values_fn = list(deaths = sum))

recovered <- recovered %>%
  mutate(Date = days) %>% 
  pivot_wider(names_from = Date, 
              values_from = recovered,
              values_fn = list(recovered = sum))

#States Summaries

state_sum <- function(state_dat) {

dat1 <- state_dat %>% 
  filter(county == "" | stateAbbr == "DC") %>% 
  select(stateFIPS,countyFIPS,county,stateAbbr)
dat2 <- confirmed %>% 
  filter(county != "")

dat3 <- state_dat %>% 
  filter(!is.na(county)) %>% 
  group_by(stateFIPS) %>% 
  summarise_at(vars(-countyFIPS,-county,-stateAbbr), sum)

state_dat <- dat1 %>% 
  left_join(dat3,by = "stateFIPS") %>% 
  rbind(dat2) %>% 
  select(stateFIPS,countyFIPS,county,stateAbbr,everything()) %>% 
  distinct()

return(state_dat)
}

confirmed <- state_sum(confirmed)
deaths <- state_sum(deaths)
recovered <- state_sum(recovered)

write.csv(confirmed, "confirmed.csv",row.names = FALSE)
write.csv(deaths, "deaths.csv",row.names = FALSE)
write.csv(recovered, "recovered.csv",row.names = FALSE)
