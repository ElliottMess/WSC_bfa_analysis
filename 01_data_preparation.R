library(dplyr)
library(readr)
library(googlesheets4)
library(httr)
library(jsonlite)
library(rhdx)
library(lubridate)
library(googledrive)
library(haven)
library(butteR)

source("function/small_function.r")

gsheets_df <- read_csv("gsheets_ref.csv")

pdi_conasur <- read_sheet("https://docs.google.com/spreadsheets/d/1zwk6ecDJ0t_GhSsn-wEjCKUBZugswQ5rUEeN26UkxGA/edit?usp=sharing", sheet = "clean_data")


population_data <- read_sheet("https://docs.google.com/spreadsheets/d/1-U2UHvlnkHFxaqQOBwtURBNTb4mQOlUjXQlWlYh5Dwc/edit?usp=sharing", "Sheet1", na = c("", "NA"))

pcodes_admin3 <- population_data%>%select(admin3, admin3pcode)%>%distinct()%>%
  mutate(admin3 = str_replace_all(admin3, "é", "e")
  )

pdi_conasur_pcoded <- pdi_conasur%>%
  left_join(pcodes_admin3, by = "admin3")%>%
  select(admin3pcode, total_pdi)

population_data <- population_data%>%
  select(-total_pdi)%>%
  left_join(pdi_conasur_pcoded, by = "admin3pcode")%>%
  rowwise()%>%
  mutate(total = sum(total_local, total_pdi, na.rm = T))

pdi_conasur_pcoded_admin2 <- population_data%>%
  group_by(admin2)%>%
  summarise(total_pdi = sum(total_pdi, na.rm = T),
            total_local = sum(total_local, na.rm = T),
            total = sum(total, na.rm = T))%>%
  mutate(admin2 = tolower(admin2))%>%
  rowwise()%>%
  mutate(
         pop_disp = total_pdi/total
           )

write_sheet(pdi_conasur_pcoded_admin2, "https://docs.google.com/spreadsheets/d/1zwk6ecDJ0t_GhSsn-wEjCKUBZugswQ5rUEeN26UkxGA/edit?usp=sharing",
            sheet = "clean_data_admin2")

bfa_admin_levels <- read_sheets_all(gsheets_df$url[gsheets_df$name == "bfa_admin_levels"], na= c("NA",""))%>%list2env(.GlobalEnv)
WSC_BFA_Pilote_carto_data <- read_sheets_all(gsheets_df$url[gsheets_df$name == "WSC_BFA_Pilote_carto_data"], na= c("NA",""))%>%list2env(.GlobalEnv)
WSC_analysis_plan <- read_sheets_all(gsheets_df$url[gsheets_df$name == "WSC_analysis_plan"], na= c("NA",""))%>%list2env(.GlobalEnv)
WASH_insecurity_scoring <- read_sheets_all(gsheets_df$url[gsheets_df$name == "WASH_insecurity_scoring"], na= c("NA",""))%>%list2env(.GlobalEnv)


# data_sources <- bfa_analysis_plan%>%
#   select(data_source_name, data_source_url, sheet)%>%
#   distinct()%>%
#   filter(!is.na(.))%>%
#   mutate(data_source_name = case_when(duplicated(data_source_name) ~ paste(data_source_name, sheet, sep = "_"),
#                                       TRUE ~ data_source_name))
# data_sources_withURL <- data_sources%>%filter(!is.na(data_source_url))
# data_sources_df <- purrr::map2(data_sources_withURL$data_source_url, as.character(data_sources_withURL$sheet), read_sheet, na = c("NA", ""))
# names(data_sources_df) <- as.character(data_sources$data_source_name)
# list2env(data_sources_df, .GlobalEnv)
# 
# lapply(names(data_sources_df), function(x){write_csv(data_sources_df[[x]], file= paste0("data/", x, ".csv"))})

all_data_sources <- read_all_csvs_in_folder("data")
names(all_data_sources) <- str_replace_all(names(all_data_sources), ".csv","")
all_data_sources%>%list2env(.GlobalEnv)


msna_2020 <- msna_2020%>%
  ungroup()%>%
  mutate(
    across(c(starts_with("retour_condition."),"pdi_present_combien", "nombre_soins_difficile"), as.numeric),
    latrine_hygienique = case_when(
      infra_sanitaire %in% c("lat_publiq", "lat_prive", "lat_privep", "toilette") ~ paste("lat", lat_hygiene, sep = "_"),
      infra_sanitaire %in% c("dal_zonep", "dal_precis", "dal_eau", "dal_zonep_am") ~ "dal",
      TRUE ~ NA_character_
    ),
    latrine_shared = case_when(
      infra_sanitaire  %in% c("lat_prive", "toilette") & is.na(type_latrine) ~ "ind",
      type_latrine == "lat_ind" ~ "ind",
      TRUE ~ type_latrine
    ),
    open_defecation = case_when(
      infra_sanitaire %in% c("dal_zonep", "dal_precis", "dal_eau", "dal_zonep_am") ~ 1L,
      infra_sanitaire %in% c("lat_publiq", "lat_prive", "lat_privep", "toilette") ~ 0L,
      TRUE ~ NA_integer_
      ),
    diarrhea_rate_hh_2w = case_when(
      `maladie_moins_5ans.diarrhee` > 0 ~ 1L,
      `maladie_moins_5ans.diarrhee` == 0 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

msna_2020$admin0 <- "BFA"

write_csv(msna_2020, "data/msna_2020.csv")

climats_bfa <- read_csv("data/bfa_zone_climatique.csv")%>%
  mutate(admin2 = case_when(
    is.na(ADM2_REF) ~ tolower(ADM2_FR),
    !is.na(ADM2_REF) ~ tolower(ADM2_REF),
    TRUE ~ NA_character_
  ))%>%
  select(admin2, type_clima)%>%
  filter(!is.na(type_clima))%>%
  group_by(admin2)%>%
  pivot_wider(names_from = type_clima, values_from= type_clima)

write_csv(climats_bfa,"data/climats_bfa.csv")


wpdx_df <- httr::GET("https://data.waterpointdata.org/resource/amwk-dedf.json?country_name=Burkina%20Faso")%>%
  content("text")%>%
  fromJSON()

# write_csv(wpdx_df, "outputs/wpdx_bfa.csv")

ch_phasing <- read_csv("data/bfa_ch_phasing.csv")%>%
  rename(admin2pcodes = "Code adm2",
         score_1 = "Phase 1",
         score_2 = "Phase 2",
         score_3 = "Phase 3",
         score_4 = "Phase 4",
         score_5 = "Phase 5")%>%
  mutate(
    score_1 = as.numeric(score_1)/as.numeric(Population),
    score_2 = as.numeric(score_2)/as.numeric(Population),
    score_3 = as.numeric(score_3)/as.numeric(Population),
    score_4 = as.numeric(score_4)/as.numeric(Population),
    score_5 = as.numeric(score_5)/as.numeric(Population),
    indicator = "score_final"
  )%>%
  left_join(admin2, by = c("admin2pcodes" ="admin2_pcodes"))%>%
  select(admin1, admin2,indicator, starts_with("score"))%>%
  group_by(admin1, admin2, indicator)%>%
  pivot_longer(starts_with("score"),names_to = "choice", values_to = "value")%>%
  mutate(choice = str_remove(choice,"score_"))

# hno_wash <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/186eEWojvXIO3vfPbTLFlKEyTVgXSYPBA4Bq8GWmQloc/edit?usp=sharing", sheet = "clean_data")%>%
#   rename(admin2 = Province,
#          admin2pcode = Admin2Pcod)%>%
#   mutate(admin2 = tolower(admin2))%>%
#   select(admin2, Severite_Assainissement, Severite_Eau, Severite_WASH)

admin3_withPop <- admin3%>%
  left_join(population_data, by = c("admin3_pcodes" = "admin3pcode"))%>%
  mutate(weights_adm3 = total/sum(total))

pop_data_admin2 <- population_data%>%
  group_by(admin2, admin2pcode)%>%
  summarise(across(where(is.numeric),sum))%>%
  mutate(admin2 = tolower(admin2),
         admin2 = case_when(
           admin2 == "koulpelgo" ~ "koulpelogo",
           admin2 == "komandjori" ~ "komonjdjari",
           TRUE ~ admin2
         ))

admin2_withPop <- admin2%>%
  left_join(pop_data_admin2, by = c("admin2" = "admin2"))%>%
  mutate(weights_adm2 = total/sum(total, na.rm = T))

pop_data_admin1 <- population_data%>%
  group_by(admin1, admin1pcode)%>%
  summarise(across(where(is.numeric),sum))%>%
  mutate(admin1 = tolower(admin1),
         admin1pcode = str_replace(admin1pcode, "BF0", "BF"))

admin1_withPop <- admin1%>%
  mutate(admin1_pcodes = str_replace(admin1_pcodes, "BF0", "BF"))%>%
  left_join(pop_data_admin1, by = c("admin1_pcodes" = "admin1pcode"))%>%
  mutate(weights_adm1 = total/sum(total))%>%
  rename(admin1 = admin1.x)

acled_data<- read_csv("data/acled_Burkina_Faso.csv")%>%
  mutate(admin2 = tolower(admin2), date = dmy(event_date), month = month(date))%>%
  filter(year >= 2015)%>%
  group_by(admin2, year, month)%>%
  summarise(nb_events = n())

acf_sahel_monitoring <- lapply(list.files("data/Pastoral_monitoring_acf"), function(x)readxl::read_excel(paste0("data/Pastoral_monitoring_acf/",x)))%>%
  bind_rows()%>%
  mutate(month = case_when(
    month == "june_july" ~ ymd(paste0(year, "-07-01")),
    month == "april_may" ~ ymd(paste0(year, "-04-01")),
    month == "dec_jan" ~ ymd(paste0(year-1, "-12-01")),
    month == "feb_mar" ~ ymd(paste0(year, "-02-01")),
    month == "oct_nov" ~ ymd(paste0(year, "-10-01")),
    month == "aug_sep" ~ ymd(paste0(year, "-09-01"))
  ))%>%
  filter(admin0Pcod == "BF")

disp_eau_acf_monitoring <- acf_sahel_monitoring%>%
  mutate(Eau_dispo = as.character(case_when(
      Eau_dispo == "Tres insuffisante" ~ 1L,
      Eau_dispo == "Insuffisante" ~ 2L,
      Eau_dispo == "Moyenne" ~ 3L,
      Eau_dispo == "Suffisante" ~ 4L,
      Eau_dispo == "Tres suffisante" ~ 5L,
      TRUE ~ NA_integer_
    )))%>%
  group_by(admin2Name, month)%>%
  summarise(mode_eau = aok_mode(Eau_dispo))%>%
  mutate(mode_eau = case_when(
    mode_eau != "NC" ~ as.integer(mode_eau),
    TRUE ~ NA_integer_
  ))
  
ggplot(disp_eau_acf_monitoring, aes(x = month, y = mode_eau, color = admin2Name))+
  geom_line(size = 2, position = position_dodge2(width=20), alpha = 0.6)+
  scale_y_continuous(
    limits =  c(0,5),
    labels = c("", "Très insuffisante", "Insuffisante", "Moyenne",
                              "Suffisante", "Très suffisante"))+
  scale_color_manual(values=c('#F89D5E','#9D9FA2'))+
  labs(x = "Date",
       y = "Disponibilité de l'eau aux points d'eau pastoraux",
       title = "Disponibilité de l'eau aux points d'eau pastoraux",
       caption = "Source: ACF - geosahel.info  (2020)",
       color = "Province")+
  ggsave("outputs/acf_pt_eau_pastoraux.png")

bfa_mics_2006_hh <- read_sav("data/bfa_mics_2006_hh.sav")

read_all_dta <- function (input_dta_folder){
  filenames_long <- list.files(input_dta_folder, full.names = TRUE, 
                               pattern = "*.dta")
  filenames_short <- list.files(input_dta_folder, full.names = FALSE, 
                                pattern = "*.dta")
  all_dtas <- list()
  for (i in 1:length(filenames_long)) {
    file_of_interest <- filenames_long[i]
    file_of_interest_short_name <- filenames_short[i]
    data <- read_dta(file_of_interest)
    all_dtas[[file_of_interest_short_name]] <- data
  }
  return(all_dtas)
}

format_dta <- function(df){
  df%>%
    mutate(across(where(is.labelled), as_factor))
}

pma_longi_dfs <- read_all_dta("data/PMA/")%>%
  lapply(format_dta)%>%
  bind_rows()
  

names(pma_longi_dfs) <- lapply(c("2015", "2016","2017","2018"), function(x){paste0("pma_", x)})

list2env(pma_longi_dfs, .GlobalEnv)

