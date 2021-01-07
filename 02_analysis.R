library(tidyverse)
library(readxl)
library(magrittr)
library(srvyr)
library(butteR)
library(lubridate)
library(hypegrammaR)
library(googledrive)
library(googlesheets4)


source("function/small_function.r")
source("function/scoring_WIS.R")
source("function/aggregations.R")
source("function/drive_manipulation.R")

source("01_data_preparation.R")

WSI_score <- scoring_WIS(
  context = "bfa_2020", context_AP = bfa_analysis_plan, WSC_AP = analysis_plan,
  WIS_water = WIS_water, WIS_sanitation = WIS_sanitation, WIS_final = WIS_final, data = msna_2020
)

WSI_score_admin2 <- admin2_agg_score(context = "bfa_2020", context_AP = bfa_analysis_plan, WSC_AP = analysis_plan, data = msna_2020, agg_level = "admin2")
WSI_score_overall <- WSI_score%>%
  group_by(admin2)%>%
  summarise(across(where(is.numeric)))

write_csv(WSI_score_admin2, "outputs/WSI_score_admin2.csv")

final_scores_adm2 <- twenty_rule(WSI_score_admin2,
  col_score = "indicator", col_label = "choice", name_final_score = "score_final", col_agg = "admin2",
  col_value = "value"
)

climats_bfa <- read_csv("data/climats_bfa.csv")

final_ch_adm2 <- twenty_rule(ch_phasing,
  col_score = "indicator", col_label = "choice", name_final_score = "score_final", col_agg = "admin2",
  col_value = "value"
) %>%
  rename(score_final_ch = score_final) %>%
  select(admin2, score_final_ch)

msna_interviews_count <- count(msna_2020, status, admin2) %>% pivot_wider(everything(), names_from = status, values_from = n)

score_climats <- final_scores_adm2 %>%
  left_join(climats_bfa) %>%
  select(-indicator, -context) %>%
  left_join(final_ch_adm2, by = "admin2") %>%
  left_join(msna_interviews_count, by = "admin2")

write_csv(score_climats, "outputs/score_climats.csv")

msna_2020_admin2_agg <- admin2_agg("msna_2020", data = msna_2020, context = "bfa_2020", context_AP = bfa_analysis_plan, WSC_AP = analysis_plan)
write_csv(msna_2020_admin2_agg, "outputs/msna_2020_admin2_agg.csv")

msna_2020_admin0_agg_diarh <- msna_2020 %>%
  select(diarrhea_rate_hh_2w, weights_sampling) %>%
  summarise(mean_diarr_rate = weighted.mean(diarrhea_rate_hh_2w, weights_sampling, na.rm = T))

msna_2020_admin0_agg_income <- msna_2020 %>%
  select(lcsi, weights_sampling) %>%
  count(lcsi, wt = weights_sampling) %>%
  filter(lcsi != "<NA>") %>%
  mutate(perc = n / sum(n))


msna_2020_admin0_agg_lcsi <- msna_2020 %>%
  select(revenu_mensuel, weights_sampling) %>%
  count(revenu_mensuel, wt = weights_sampling) %>%
  filter(revenu_mensuel != "<NA>") %>%
  mutate(perc = n / sum(n))

msna_2020_admin0_agg_src_rev <- msna_2020 %>%
  select(source_revenu1, weights_sampling) %>%
  count(source_revenu1, wt = weights_sampling) %>%
  filter(source_revenu1 != "<NA>") %>%
  mutate(perc = n / sum(n))


smart_rapid_idp_2020_with_weights <- smart_rapid_idp_2020 %>%
  left_join(admin2_withPop, by = "admin2")



smart_idp_2020_admin2_agg <- admin2_agg("smart_rapid_idp_2020",
  data = smart_rapid_idp_2020_with_weights, context = "bfa_2020",
  context_AP = bfa_analysis_plan, WSC_AP = analysis_plan,
  weights = "weights_adm2"
)

smart_idp_2020_admin0_agg <- smart_rapid_idp_2020_with_weights %>%
  summarise(across(where(is.numeric), weighted.mean, w = weights_adm2, na.rm = T))

smart_2019_admin2_with_weights <- smart_2019_admin2 %>%
  left_join(admin2_withPop, by = "admin2")

smart_2019_admin2_admin2_agg <- admin2_agg("smart_2019_admin2",
  data = smart_2019_admin2_with_weights, context = "bfa_2020",
  context_AP = bfa_analysis_plan, WSC_AP = analysis_plan,
  weights = "weights_adm2"
)


smart_2019_admin1_admin2_agg <- admin1_to_admin2_agg("smart_2019_admin1",
  context = "bfa_2020", context_AP = bfa_analysis_plan, WSC_AP = analysis_plan,
  data = smart_2019_admin1, admin2_df = admin2
)

smart_2019_admin2_admin0_agg <- smart_2019_admin2_with_weights %>%
  summarise(across(where(is.numeric), weighted.mean, w = weights_adm2, na.rm = T))

smart_2019_admin1_with_weights <- smart_2019_admin1 %>%
  left_join(admin1_withPop, by = "admin1")

smart_2019_admin1_admin0_agg <- smart_2019_admin1_with_weights %>%
  summarise(across(where(is.numeric), weighted.mean, w = weights_adm1, na.rm = T))


bdd_min_sante_df <- bdd_min_sante%>%
  # left_join(ds, by = c("ds_pcodes" = "ds_pcode")) %>%
  # left_join(admin2_withPop, by = c("admin2.y" = "admin2_pcodes")) %>%
  select(-ends_with(".y"), -ends_with(".x")) %>%
  group_by(admin2, semaine) %>%
  summarise(across(where(is.numeric), sum)) %>%
  select(-annee, -debut) %>%
  ungroup() %>%
  arrange(admin2, semaine) %>%
  group_by(admin2) %>%
  mutate(
    semaine = as.factor(semaine),
    across(starts_with("nb_"), .fns = list(twoWeeks_mean = function(col) {
      (lag(col, order_by = admin2) + col) / 2
    })),
    across(starts_with("nb_"), .fns = list(per100k = function(col) {
      (col / total) * 100000
    }))
  ) %>%
  ungroup()

write_csv(bdd_min_sante_df, "data/bdd_min_sante_agg.csv")

bdd_min_sante_admin2_agg <- bdd_min_sante_df %>%
  group_by(admin2) %>%
  summarise(
    nb_cases_cholera = sum(nb_cases_cholera, na.rm = T),
    nb_cases_awd = sum(nb_cases_awd, na.rm = T),
    nb_cases_meningitis = sum(nb_cases_meningitis, na.rm = T),
    nb_cases_malaria = sum(nb_cases_malaria, na.rm = T),
    nb_cases_dengues = sum(nb_cases_dengues, na.rm = T)
  ) %>%
  left_join(admin2_withPop, by = c("admin2" = "admin2")) %>%
  mutate(across(starts_with("nb_"), .fns = list(per100k = function(col) {
    (col / total) * 100000
  }))) %>%
  select(!matches("total|local|weights|admin1|admin2_fr|admin2_pcodes|admin2pcode")) %>%
  pivot_longer(-admin2, names_to = "indicator", values_to = "value") %>%
  mutate(choice = NA, context = "bfa_2020")


jmp_data <- read_csv("data/JMP_2019_BFA_Burkina_Faso.csv")%>%
  select(-Country, -population, -improved_water)%>%
  filter(Setting == "National")%>%
  group_by(Year, Setting)%>%
  pivot_longer(cols = c(-Year, -Setting), values_to = "value", names_to = "indicator")

msna_JMP_data <- WSI_score%>%
  mutate(jmp_water = case_when(
    water_source_dist == "improved_less_30" ~ "basic_water",
    water_source_dist == "improved_more_30" ~ "limited_water",
    water_source_dist == "unimproved" ~ "unimproved_water",
    water_source_dist == "improved_premises" ~ "basic_water",
    water_source_dist == "surface_water" ~ "surface_water",
    TRUE ~ NA_character_
  ),
  weights = as.numeric(weights),
  )%>%
  filter(!is.na(jmp_water))%>%
  srvyr::as_survey_design(weights = weights, ids = "cluster_id")%>%
  group_by(jmp_water)%>%
  summarise(jmp_water_mean = survey_mean())%>%
  pivot_longer(cols = c(jmp_water_mean), names_to = "indicator", values_to = "value")%>%
  mutate(
    Year = 2020,
    Setting = "National",
    value = round(value*100,1)
  )%>%
  select(Year, Setting, indicator, value)

msna_JMP_data_water <- WSI_score%>%
  mutate(jmp_water = case_when(
    water_source_dist == "improved_less_30" ~ "basic_water",
    water_source_dist == "improved_more_30" ~ "limited_water",
    water_source_dist == "unimproved" ~ "unimproved_water",
    water_source_dist == "improved_premises" ~ "basic_water",
    water_source_dist == "surface_water" ~ "surface_water",
    TRUE ~ NA_character_
  ),
  weights = as.numeric(weights)
  )%>%
  filter(!is.na(jmp_water))%>%
  srvyr::as_survey_design(weights = weights, ids = "cluster_id")%>%
  group_by(jmp_water)%>%
  summarise(jmp_water_mean = survey_mean())%>%
  pivot_longer(cols = c(jmp_water_mean), names_to = "indicator", values_to = "value")%>%
  mutate(
    Year = 2020,
    Setting = "National",
    value = round(value*100,1),
    indicator = jmp_water,
  )%>%
  select(Year, Setting, indicator, value)

msna_JMP_data_san <- WSI_score%>%
  mutate(jmp_san = case_when(
    type_of_sanitation_facility == "open_defec" ~ 1L,
    !is.na(type_of_sanitation_facility) ~ 0L,
    TRUE ~ NA_integer_
  ),
  weights = as.numeric(weights)
  )%>%
  filter(!is.na(jmp_san))%>%
  srvyr::as_survey_design(weights = weights, ids = "cluster_id")%>%
  summarise(jmp_san_mean = survey_mean(jmp_san))%>%
  pivot_longer(cols = c(jmp_san_mean), names_to = "indicator", values_to = "value")%>%
  mutate(
    Year = 2020,
    Setting = "National",
    value = round(value*100,1),
    indicator = "open_defecation",
  )%>%
  select(Year, Setting, indicator, value)


jmp_data_updated <- bind_rows(jmp_data, msna_JMP_data_water, msna_JMP_data_san)

jmp_data_updated_od <- filter(jmp_data_updated, grepl("open", indicator))%>%
  mutate(latrine_value = 100-value, latrine = "latrine")%>%
  group_by(Year, Setting)%>%
  pivot_longer(c(value, latrine_value))%>%
  mutate(
    indicator = factor(case_when(
      name == "value" ~ "Défécation\nà l'air libre",
      name == "latrine_value" ~ "Latrine",
      TRUE ~ NA_character_
  ), levels = c("Latrine","Défécation\nà l'air libre")))

ggplot(jmp_data_updated_od, aes(x = Year, y = value, fill = indicator))+
  geom_area()+
  scale_fill_manual(values = c("#c9d9d9","#77113a"))+
  labs(x = NULL, y = "Couverture (%)", fill = NULL, title = "Tendance en défécation à l'air libre (2000-2020)", caption = "Source: JMP (2000-2017); MSNA (2020)")+
  theme_classic()+
  ggsave("outputs/jmp_san_updated.png", width = 6)


jmp_data_updated_water <- filter(jmp_data_updated, grepl("_water|premise", indicator))%>%
  mutate(indicator = factor(case_when(indicator == "surface_water" ~ "Eau de surface",
                                      indicator == "unimproved_water" ~ "Non-amélioré",
                                      indicator == "limited_water" ~ "Limité",
                                      indicator == "basic_water" ~ "Basique")
                            , levels = c("Eau de surface", "Non-amélioré", "Limité", "Basique")))

ggplot(jmp_data_updated_water, aes(x = Year, y = value, fill = indicator))+
  geom_area()+
  scale_fill_manual(values = c("#77113a", "#884269", "#a46f92", "#c9d9d9"))+
  labs(x = NULL, y = "Couverture (%)", fill = NULL, title = "Tendance en services d'eau (2000-2020)", caption = "Source: JMP (2000-2017); MSNA (2020)")+
  theme_classic()+
  ggsave("outputs/jmp_water_updated.png")



# summarise(across(!matches("twoWeeks_mean$") & where(is.numeric), sum, na.rm = T),
#           across(matches("twoWeeks_mean$|nb_cases_meningitis_per100k$"), mean, na.rm = T)
#           )%>%
# pivot_longer(-admin2, names_to = "indicator", values_to = "value" )%>%
# mutate(choice = NA, context = "bfa_2020")

bdd_ino_2019_df <- bdd_ino_2019 %>%
  left_join(admin3_withPop, by = c("admin3" = "admin3.x")) %>%
  mutate(admin2 = tolower(admin2.y))


bdd_ino_2019_agg_admin2 <- admin2_agg(
  df_name = "bdd_ino_2019", context = "bfa_2020", context_AP = bfa_analysis_plan, WSC_AP = analysis_plan,
  data = bdd_ino_2019_df, weights = "weights_adm3"
)


bdd_crbf_153_centresSante_agg_admin2 <- admin2_agg(
  df_name = "bdd_crbf_153_centresSante", context = "bfa_2020", context_AP = bfa_analysis_plan,
  WSC_AP = analysis_plan, data = bdd_crbf_153_centresSante
)


extrait_annuaire_primaire <- read_sheet("https://docs.google.com/spreadsheets/d/1E5MtqFVOEHaeAhiS5eX-_Ed3W1XoIx2bn7OPg89vdoY/edit?usp=sharing",
  sheet = "clean_data"
)

bdd_edu_2020_agg_admin2 <- admin2_agg(
  df_name = "extrait_annuaire_primaire", context = "bfa_2020", context_AP = bfa_analysis_plan,
  WSC_AP = analysis_plan, data = extrait_annuaire_primaire
)

pdi_conasur_agg_admin2 <- admin2_agg(
  df_name = "situation_pdi_par_commune_CONASUR_08sep2020", context = "bfa_2020",
  context_AP = bfa_analysis_plan,
  WSC_AP = analysis_plan, data = pdi_conasur_pcoded_admin2
)

acled_data <- read_csv("data/1997-01-01-2020-12-10-Burkina_Faso.csv") %>%
  mutate(
    event_date = dmy(event_date),
    month = month(event_date)
  ) %>%
  mutate(admin2 = tolower(admin2))

acled_admin2_months <- data.frame(
  year = rep(min(acled_data$year):max(acled_data$year), each = 12),
  month = rep(1:12, length(min(acled_data$year):max(acled_data$year)))
)

acled_data_month <- acled_data %>%
  group_by(admin2, year, month) %>%
  summarise(nb_events = n()) %>%
  ungroup() %>%
  complete(admin2, nesting(year, month), fill = list(nb_events = 0))

acled_admin2_months_analysed_plot <- acled_data%>%
  filter(admin2 %in% c("houet", "gourma", "seno"), year >=2010, !event_type %in% c("Protests", "Riots"))%>%
  mutate(admin2 = case_when(
    admin2 == "houet" ~ "Houet",
    admin2 == "gourma" ~ "Gourma",
    admin2 == "seno" ~ "Séno",
    TRUE ~ admin2
  ))%>%
  group_by(admin2, year, month)%>%
  summarise(n = n())%>%
  mutate(date = dmy(paste("01",month, year, sep = "-")))%>%
  ggplot(aes(x = date, color = admin2))+
    geom_density(size =1)+
    labs(x ="Temps", y = "Nombre d'événements violents", caption = "Source: ACLED (2020)",
       color = "Province", title = "Evolution de l'activité conflictuelle dans les provinces analysées")+
    theme_classic()+
    scale_color_manual(values=c("#884269", "#4e4f52", "#9d9fa2"))+
    ggsave("outputs/conflict_data.png", width = 8)
  

write_csv(acled_data_month, "data/acled_burkina_faso_byMonth.csv")


acled_30days <- acled_data_month %>%
  group_by(admin2, year, month) %>%
  filter(year == 2020 & month == 11) %>%
  mutate(indicator = "nb_sec_incidents_30d", choice = NA, value = nb_events, context = "bfa_2020") %>%
  ungroup() %>%
  select(admin2, indicator, choice, value, context)

write_csv(acled_30days, "data/acled_burkina_faso_30d.csv")

idmc_disasters <- read_sheet("https://docs.google.com/spreadsheets/d/138HGrWTL4VB1Dtw8hyAo711oFz-49rfLaLi0OUMBFys/edit?usp=sharing", sheet = "Sheet1")

idmc_disasters_df <- idmc_disasters %>%
  filter(ISO3 == "BFA") %>%
  rename(start_date = `Start Date`, new_displacement = `New Displacements`) %>%
  mutate(start_date = ymd(start_date))

# plot_idmc_disasters <- ggplot(idmc_disasters_df, aes(x = start_date, y = new_displacement)) +
#   geom_point(aes(size = new_displacement)) +
#   labs(
#     x = "Date",
#     y = "Nouveaux déplacements",
#     title = "Déplacements dus aux innondations",
#     caption = "Source: IDMC (2019)",
#     size = "Nouveaux déplacés"
#   ) +
#   theme_classic()+
#   ggsave("outputs/idmc_catastrophes naturelles.png", width = 7)


idmc_displacement <- read_sheet("https://docs.google.com/spreadsheets/d/1IT6g3j17WVKUyT1WqKO-xIzwmJy__HspiJVwXU6005o/edit#gid=1498341122", sheet = "Sheet1")
idmc_displacement_df <- idmc_displacement %>%
  filter(ISO3 == "BFA") %>%
  select(Year, "Conflict Stock Displacement", "Disaster New Displacements") %>%
  rename(conflict = "Conflict Stock Displacement", nat_disaster = "Disaster New Displacements") %>%
  pivot_longer(c(conflict, nat_disaster), values_to = "displaced", names_to = "category") %>%
  mutate(
    displaced = case_when(
      is.na(displaced) ~ 0,
      TRUE ~ displaced
    ),
    category = case_when(
      category == "conflict" ~ "Conflit armé",
      category == "nat_disaster" ~ "Catastrophe naturelle"
    ),
    Year = ymd(paste0(Year, "-01-01"))
  )


plot_idmc_displacement <- ggplot(idmc_displacement_df, aes(x = Year, y = displaced, fill = category)) +
  geom_area() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(
    x = "Date",
    y = "Nombres de personnes déplacées",
    title = "Déplacements au Burkina Faso",
    caption = "Source: IDMC (2019)",
    fill = "Type d'événement"
  ) +
  theme_classic()+
  scale_fill_manual(values=c("#884269", "#4e4f52"))+
  ggsave("outputs/idmc_deplacements.png", width = 7)


# mutate(start_date = ymd(start_date))%>%
# separate(admin1, into = c("admin1_1", "admin1_2", "admin1_3", "admin1_4", "admin1_5", "admin1_6"), sep = ",")%>%
# pivot_longer(cols = starts_with("admin1_"), names_to = "ref", values_to = "admin1")%>%
# separate(admin2, into = c("admin2_1", "admin2_2", "admin2_3"), sep = ",")%>%
# pivot_longer(cols = starts_with("admin2_"), names_to = "ref2", values_to = "admin2")%>%
# select(-ref, -ref2)%>%
# distinct()%>%
# filter(is.na())



admin2_agg_bfa <- rbind(
  msna_2020_admin2_agg, smart_idp_2020_admin2_agg, smart_2019_admin2_admin2_agg, smart_2019_admin1_admin2_agg,
  WSI_score_admin2, bdd_min_sante_admin2_agg, bdd_ino_2019_agg_admin2, bdd_edu_2020_agg_admin2, bdd_crbf_153_centresSante_agg_admin2,
  acled_30days, pdi_conasur_agg_admin2
)

scoring_df_fromAP <- scoring_AP(admin2_agg_bfa,
  context_AP = bfa_analysis_plan,
  WSC_AP = analysis_plan, hh_data = msna_2020, context = "bfa_2020"
) %>%
  mutate(phase = value) %>%
  select(-choice, -value)

admin2_agg_bfa <- admin2_agg_bfa %>%
  left_join(scoring_df_fromAP, by = c("admin2", "indicator", "context"))

write_csv(admin2_agg_bfa, "outputs/admin2_agg_bfa.csv")

admin2_data_indiv_files <- function(prv_name, context, context_AP, WSC_AP) {
  analyse_wrkshp_drive_path <- "https://drive.google.com/drive/folders/16ytPLyS7D47bHHRdrIR7rOZj769j20cE?usp=sharing"

  worksheet_template_fr_path <- "https://docs.google.com/spreadsheets/d/1fTZJnLH2eom9ajlplivClytvF6DWoM8jka9x1pEQo3U/edit?usp=sharing"

  copy_wrksheet_prvFolder(analyse_wrkshp_drive_path, prv_name, worksheet_template_fr_path)


  all_provinces_folder <- prv_fldr_ls(analyse_wrkshp_drive_path)
  id_province_folder <- as_id(all_provinces_folder$id[all_provinces_folder$name == prv_name])


  full_AP <- context_AP %>%
    filter(context_AP$context == context) %>%
    left_join(WSC_AP, by = "indicator_code")

  right_order <- as.character(full_AP$indicator_code)


  all_disags_msna <- read_csv("outputs/msna_2020_admin2_agg.csv") %>%
    filter(admin2 == !!prv_name) %>%
    as.data.frame() %>%
    pivot_wider(names_from = context) %>%
    # left_join(full_AP, by = c("indicator" = "indicator_code"))%>%
    mutate(choice = if_else(choice == "filtered_values", "NAs", choice)) %>%
    arrange(indicator) %>%
    mutate(choice_key = paste(indicator, choice, sep = ""))

  if (!dir.exists("outputs/province_data/")) {
    dir.create("outputs/province_data")
  }
  if (!dir.exists(paste0("outputs/province_data/", prv_name))) {
    dir.create(paste0("outputs/province_data/", prv_name))
  }

  acled_data_month <- read_csv("data/acled_burkina_faso_byMonth.csv") %>%
    filter(admin2 == !!prv_name) %>%
    mutate(date = ymd(paste(year, month, "1", sep = "-")))

  plot_incidents_secu <- ggplot(acled_data_month, aes(x = date, y = nb_events)) +
    geom_line() +
    geom_smooth(method = "lm", se = FALSE) +
    xlab("Temps") +
    ylab("Nombre d'événements") +
    ggsave(paste0("outputs/province_data/", prv_name, "/nb_incidents_secu", prv_name, ".png"))

  plot_incidents_secu_upload <- drive_upload(
    paste0("outputs/province_data/", prv_name, "/nb_incidents_secu", prv_name, ".png"),
    id_province_folder
  )


  bdd_maladies <- read_csv("data/bdd_min_sante.csv") %>%
    filter(admin2 == !!prv_name) %>%
    select(!matches("total|local|weights")) %>%
    group_by(admin2) %>%
    pivot_longer(starts_with("nb_"), names_to = "indicator", values_to = "value") %>%
    separate(indicator, sep = "_", into = c("x", "y", "z", "a", "b", "c")) %>%
    rename("type" = y, "maladie" = z) %>%
    mutate(
      unit = case_when(
        !is.na(c) ~ paste0(a, b, c),
        !is.na(b) ~ paste0(a, b),
        !is.na(a) ~ paste0(a),
        TRUE ~ "number"
      ),
      maladie = case_when(
        maladie == "malaria" ~ "Malaria",
        maladie == "awd" ~ "Diarrhée aqueuse aiguë",
        maladie == "cholera" ~ "Choléra",
        maladie == "dengues" ~ "Dengues",
        maladie == "meningitis" ~ "Méningite",
        TRUE ~ NA_character_
      )
    ) %>%
    select(-x, -a, -b, -c)

  plot_cases_notMalaria <- bdd_maladies %>%
    filter(unit == "number" & maladie != "Malaria") %>%
    mutate(semaine = lubridate::ymd("2020-01-01") + lubridate::weeks(semaine - 1))

  plot_cases_malaria <- bdd_maladies %>%
    filter(unit == "number" & maladie == "Malaria") %>%
    mutate(semaine = lubridate::ymd("2020-01-01") + lubridate::weeks(semaine - 1))

  ggplot(plot_cases_malaria, aes(x = semaine, y = value, color = maladie)) +
    geom_line(se = FALSE) +
    xlab("Semaine") +
    ylab("Nombre de cas") +
    ggsave(paste0("outputs/province_data/", prv_name, "/malaria_", prv_name, ".png"))

  plot_malaria_upload <- drive_upload(
    paste0("outputs/province_data/", prv_name, "/malaria_", prv_name, ".png"),
    id_province_folder
  )

  ymax <- max(plot_cases_notMalaria$value)
  ggplot(plot_cases_notMalaria, aes(x = semaine, y = value, color = maladie)) +
    geom_smooth(se = FALSE) +
    coord_cartesian(ylim = c(0.0, ymax)) +
    xlab("Semaine") +
    ylab("Nombre de cas") +
    ggsave(paste0("outputs/province_data/", prv_name, "/maladies_", prv_name, ".png"))

  plot_diseases_upload <- drive_upload(
    paste0("outputs/province_data/", prv_name, "/maladies_", prv_name, ".png"),
    id_province_folder
  )

  write_csv(all_disags_msna, paste0("outputs/province_data/", prv_name, "/MSNA_", prv_name, "_all_msna.csv"))

  indicator_labels_FR <- full_AP %>%
    select(indicator_code, `Indicateur FR`) %>%
    rename(Indicateur = `Indicateur FR`)

  data_sources <- full_AP %>%
    select(indicator_code, data_source_name, data_source_url) %>%
    distinct()

  choices_labels_FR <- full_AP %>%
    select(indicator_code, score, ch_label, ch_name)

  all_data <- read_csv("outputs/admin2_agg_bfa.csv", col_types = cols(phase = col_character())) %>%
    filter(admin2 == !!prv_name) %>%
    left_join(indicator_labels_FR, by = c("indicator" = "indicator_code")) %>%
    distinct() %>%
    mutate(label_choice = case_when(
      is.na(Indicateur) ~ paste0(indicator, "_", choice),
      TRUE ~ paste0(Indicateur, "_", choice)
    )) %>%
    left_join(data_sources, by = c("indicator" = "indicator_code")) %>%
    left_join(choices_labels_FR, by = c("indicator" = "indicator_code", "choice" = "ch_name")) %>%
    mutate(label_choiceLabel = case_when(
      is.na(Indicateur) & is.na(ch_label) ~ paste0(indicator, "_", choice),
      TRUE ~ paste0(Indicateur, ch_label)
    )) %>%
    distinct()

  write_csv(all_data, paste0("outputs/province_data/", prv_name, "/all_data_", prv_name, ".csv"))


  all_disags_score <- read_csv("outputs/WSI_score_admin2.csv") %>%
    filter(admin2 == !!prv_name) %>%
    as.data.frame() %>%
    pivot_wider(names_from = context)

  write.csv(all_disags_score, paste0("outputs/province_data/", prv_name, "/MSNA_", prv_name, "_all_scores.csv"))

  write_sheet(all_data, ss = copied_worksheet_path(analyse_wrkshp_drive_path, prv_name), sheet = "data")

  evidence_repo_share <- googledrive::drive_share_anyone(id_province_folder)
  evidence_repo_link <- googledrive::drive_link(id_province_folder)
  evidence_repo_link_df <- tibble(x = gs4_formula(paste0('=HYPERLINK("', evidence_repo_link, '", "Répertoire des preuves")')))

  range_write(
    ss = copied_worksheet_path(analyse_wrkshp_drive_path, prv_name), sheet = "Feuille d'analyse", range = "H1",
    data = evidence_repo_link_df, reformat = FALSE, col_names = FALSE
  )

  title <- tibble(x = as.character(paste0("Classification de la sévérité WASH - Burkina Faso - ", simpleCap(prv_name))))

  range_write(
    ss = copied_worksheet_path(analyse_wrkshp_drive_path, prv_name), sheet = "Feuille d'analyse", range = "B1",
    data = title, reformat = FALSE, col_names = FALSE
  )

  sec_outcomes <- tibble(x = c(
    gs4_formula(paste0('=HYPERLINK("', plot_incidents_secu_upload$drive_resource[[1]]$webContentLink, '", "Nombre incidents sécuritaires")')),
    gs4_formula(paste0('=HYPERLINK("', plot_diseases_upload$drive_resource[[1]]$webContentLink, '", "Fardeau des maladies (hors malaria)")')),
    gs4_formula(paste0('=HYPERLINK("', plot_malaria_upload$drive_resource[[1]]$webContentLink, '", "Fardeau des maladies (malaria)")'))
  ))

  range_write(
    ss = copied_worksheet_path(analyse_wrkshp_drive_path, prv_name), sheet = "Feuille d'analyse", range = "K38",
    data = sec_outcomes, reformat = FALSE, col_names = FALSE
  )
}

# lapply(unique(admin2_withPop$admin2), function(x){admin2_data_indiv_files(x,context = "bfa_2020",context_AP = bfa_analysis_plan, WSC_AP = analysis_plan)})

province_pilot <- c("seno", "gourma", "houet", "bam", "comoe", "sanmatenga")
province_analysed <- c("seno", "gourma", "houet")
lapply(province_pilot, function(x) {
  admin2_data_indiv_files(x, context = "bfa_2020", context_AP = bfa_analysis_plan, WSC_AP = analysis_plan)
})


water_point_bf019 <- read_csv("data/bfa_waterpoints/bf019.csv") %>%
  mutate(
    date = ymd(date),
    year = factor(year(date)),
    date_no_year = update(date, year = 1),
    date_no_day = update(date, day = 1),
    date_no_day_no_year = update(date, day = 1, year = 1)
  ) %>%
  group_by(date_no_day, date_no_day_no_year) %>%
  summarise(median_depth = median(depth))

ggplot(water_point_bf019, aes(x = date_no_day_no_year, y = median_depth, color = year)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

admin3_admin2 <- admin3_withPop%>%
  select(admin2.x, admin3.x)%>%
  rename(admin3 = admin3.x, admin2 = admin2.x)

bdd_ino_2019_agg_admin3 <- admin_agg(  df_name = "bdd_ino_2019", context = "bfa_2020", context_AP = bfa_analysis_plan, WSC_AP = analysis_plan,
                                       data = bdd_ino_2019_df, weights = "weights_adm3",
                                       target_admlvl = "admin3"
)%>%
  left_join(admin3_admin2, by = c("admin3" = "admin3"))%>%
  mutate(source = "Base de donnée INO, 2020")

bdd_crbf_153_centresSante_agg_admin3 <- admin_agg(
  df_name = "bdd_crbf_153_centresSante", context = "bfa_2020", context_AP = bfa_analysis_plan,
  WSC_AP = analysis_plan, data = bdd_crbf_153_centresSante, target_admlvl = "admin3"
)%>%
  left_join(admin3_admin2, by = c("admin3" = "admin3"))%>%
  mutate(source = "Croix-Rouge Burkinabé, Besoin WASH dans 153 Centres de santé, 2020")

smart_idp_2020_admin3_agg <- admin_agg("smart_rapid_idp_2020",
                                        data = smart_rapid_idp_2020_with_weights, context = "bfa_2020",
                                        context_AP = bfa_analysis_plan, WSC_AP = analysis_plan,
                                        weights = "weights_adm2", target_admlvl = "admin3"
)%>%
  left_join(admin3_admin2, by = c("admin3" = "admin3"))%>%
  mutate(source = "SMART Rapid 2020 dans 11 communes affectées par la crise, 2020")

acled_data_month_admin3 <- acled_data %>%
  group_by(admin3, year, month) %>%
  summarise(nb_events = n()) %>%
  ungroup() %>%
  complete(admin3, nesting(year, month), fill = list(nb_events = 0))%>%
  mutate(admin3 = tolower(admin3))%>%
  left_join(admin3_admin2, by = c("admin3" = "admin3"))

label_indicators <- WSC_AP%>%
  select(indicator_code, `Indicateur FR`)

all_admin3 <- bind_rows(bdd_ino_2019_agg_admin3, bdd_crbf_153_centresSante_agg_admin3, smart_idp_2020_admin3_agg)%>%
  filter(admin2 %in% c("seno", "houet", "gourma"))%>%
  distinct()%>%
  left_join(label_indicators, by = c("indicator" = "indicator_code"))

seno_admin3 <- all_admin3%>%
  filter(admin2 == "seno")%>%
  pivot_wider(names_from = "admin3", values_from = "value")

gourma_admin3 <- all_admin3%>%
  filter(admin2 == "gourma")%>%
  pivot_wider(names_from = "admin3", values_from = "value")

houet_admin3 <- all_admin3%>%
  filter(admin2 == "houet")%>%
  pivot_wider(names_from = "admin3", values_from = "value")


msna_2020_admin0_agg <- admin0_agg_prvAnalysed(df_name = "msna_2020", data = msna_2020, context = "bfa_2020", context_AP = bfa_analysis_plan,
                                               weights = "weights_sampling", WSC_AP = analysis_plan, target_admlvl = "admin0")%>%
  mutate(source = "MSNA, 2020")

bdd_ino_2019_agg_admin0 <- admin0_agg_prvAnalysed(df_name = "bdd_ino_2019", context = "bfa_2020", context_AP = bfa_analysis_plan, WSC_AP = analysis_plan,
                                       data = bdd_ino_2019_df, weights = "weights_adm3",
                                       target_admlvl = "admin0")%>%
  mutate(source = "Base de donnée INO, 2020")

bdd_crbf_153_centresSante_agg_admin0 <- admin0_agg_prvAnalysed(df_name = "bdd_crbf_153_centresSante", context = "bfa_2020", context_AP = bfa_analysis_plan,
  WSC_AP = analysis_plan, data = bdd_crbf_153_centresSante, target_admlvl = "admin0")%>%
  mutate(source = "Croix-Rouge Burkinabé, Besoin WASH dans 153 Centres de santé, 2020")

smart_idp_2020_admin0_agg <- admin0_agg_prvAnalysed("smart_rapid_idp_2020",
                                       data = smart_rapid_idp_2020_with_weights, context = "bfa_2020",
                                       context_AP = bfa_analysis_plan, WSC_AP = analysis_plan,
                                       weights = "weights_adm2", target_admlvl = "admin0")%>%
  mutate(source = "SMART Rapid 2020 dans 11 communes affectées par la crise, 2020")

smart_2019_admin0_agg <- admin0_agg_prvAnalysed("smart_2019_admin2",
  data = smart_2019_admin2_with_weights, context = "bfa_2020",
  context_AP = bfa_analysis_plan, WSC_AP = analysis_plan,
  weights = "weights_adm2", target_admlvl = "admin0")%>%
  mutate(source = "SMART 2019 nationale, 2019")

WSI_score_admin0 <- admin2_agg_score(context = "bfa_2020", context_AP = bfa_analysis_plan, WSC_AP = analysis_plan, data = msna_2020, agg_level = "admin0")%>%
  mutate(source = "MSNA, 2020")

bdd_min_sante_admin0_agg <- bdd_min_sante%>%
  filter(admin2 %in% province_analysed)%>%
  summarise(
    nb_cases_cholera = sum(nb_cases_cholera, na.rm = T),
    nb_cases_awd = sum(nb_cases_awd, na.rm = T),
    nb_cases_meningitis = sum(nb_cases_meningitis, na.rm = T),
    nb_cases_malaria = sum(nb_cases_malaria, na.rm = T),
    nb_cases_dengues = sum(nb_cases_dengues, na.rm = T),
    total = total,
    weights_adm2 = weights_adm2
  )%>%
  mutate(across(starts_with("nb_"), .fns = list(per100k = function(col) {
    (col / total) * 100000
  }))) %>%
  summarise(across(where(is.numeric), weighted.mean, w = weights_adm2, na.rm = T))%>%
  select(!matches("total|local|weights"))%>%
  pivot_longer(everything(), names_to = "indicator", values_to = "value")%>%
  mutate(choice = NA, context = "bfa_2020")%>%
  mutate(source = "Base de données des maladies, 2020",
         admin0 = "admin0")%>%
  select("admin0","indicator", "choice","value","context","source")


bdd_edu_2020_agg_admin0 <- admin0_agg_prvAnalysed(
  df_name = "extrait_annuaire_primaire", context = "bfa_2020", context_AP = bfa_analysis_plan,
  WSC_AP = analysis_plan, data = extrait_annuaire_primaire, target_admlvl = "admin0"
)%>%
  mutate(source = "Annuaire statistique éducation, 2018")


label_indicators <- WSC_AP%>%
  select(indicator_code, `Indicateur FR`)

admin0_agg_bfa <- rbind(
  msna_2020_admin0_agg, smart_idp_2020_admin0_agg, smart_2019_admin0_agg,
  WSI_score_admin0, bdd_min_sante_admin0_agg, bdd_ino_2019_agg_admin0, bdd_edu_2020_agg_admin0, bdd_crbf_153_centresSante_agg_admin0
)


all_admin0 <- admin0_agg_bfa%>%
  distinct()%>%
  left_join(label_indicators, by = c("indicator" = "indicator_code"))

url_overall_sheet <- "https://docs.google.com/spreadsheets/d/1f9_Lu98cP5ioLy3SP5DEfXQp17ezp_r0IW3rWy620fY/edit?usp=sharing"

indicators_label_FR <- bfa_analysis_plan%>%
  mutate(`Niveau du cadre analytique` = fct_inorder(`Niveau du cadre analytique`),
         indicator_code = fct_inorder(indicator_code))%>%
  select(`Niveau du cadre analytique`, indicator_code, Indicateur, data_source_name)

order_indic <- levels(indicators_label_FR$indicator_code)

choices_label_FR <- bfa_analysis_plan%>%
  mutate(q_choice = fct_inorder(paste(indicator_code,ch_name, sep = "_")))%>%
  select(q_choice, ch_label)

data_source_label <- bfa_analysis_plan%>%
  mutate(data_source_label = case_when(
    data_source_name == "smart_rapid_idp_2020" ~ "SMART rapid 2020",
    data_source_name == "smart_2019_admin2" ~ "SMART nationale 2019",
    data_source_name == "smart_2019_admin1" ~ "SMART nationale 2019",
    data_source_name == "msna_2020" ~ "MSNA 2020",
    data_source_name == "bdd_min_sante" ~ "Base de donnée des maladies du Ministère de la Santé",
    data_source_name == "ACLED" ~ "ACLED",
    data_source_name == "situation_pdi_par_commune_CONASUR_08sep2020" ~ "Situation des PDI par commune, CONASUR, septembre 2020",
    data_source_name == "bdd_ino_2019" ~ "Inventaire national des ouvrages (INO)",
    data_source_name == "bdd_crbf_153_centresSante" ~ "Enquête Croix-Rouge Burkinabé dans les centres de santé",
    data_source_name == "extrait_annuaire_primaire" ~ "Extrait de l'annuaire statistique sur l'éducation primaire",
    TRUE ~ NA_character_
  ))%>%
  select(data_source_name, data_source_label)%>%
  distinct()

admin2_label_FR <- admin2_withPop%>%
  mutate(admin1_fr = factor(admin1_fr))%>%
  select(admin2, admin2_fr)

admin2_agg_bfa_clean <- admin2_agg_bfa%>%
  left_join(indicators_label_FR, by = c("indicator" = "indicator_code"))%>%
  distinct()%>%
  mutate(q_choice = paste(indicator, choice, sep = "_"),
         indicator = factor(indicator, ordered = T, level = order_indic))%>%
  left_join(choices_label_FR, by = "q_choice")%>%
  left_join(admin2_label_FR, by = "admin2")%>%
  left_join(data_source_label, by = "data_source_name")%>%
  arrange(`Niveau du cadre analytique`, indicator,admin2_fr)%>%
  select(`Niveau du cadre analytique`, Indicateur, ch_label, value, admin2_fr)%>%
  distinct()%>%
  filter(!is.na(Indicateur), !Indicateur %in% c("% de ménages n'ayant pu pratiquer l'agriculture de manière optimale par rapport aux années précédentes, par type de problème rencontré",
                                                "% des structures de santé ayant accès à des services EHA appropriés"))%>%
  group_by(admin2_fr)%>%
  pivot_wider(names_from = "admin2_fr", values_from = "value", values_fn = first)%>%
  rename(Choix = ch_label)

write_sheet(admin2_agg_bfa_clean, url_overall_sheet, sheet = "data")
