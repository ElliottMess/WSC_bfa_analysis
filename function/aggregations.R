admin2_agg_score <- function(context, context_AP, WSC_AP, data = NULL, agg_level = "admin2"){
  
  full_AP <- context_AP%>%
    filter(context_AP$context == context)%>%
    left_join(WSC_AP, by = "indicator_code")
  
  data_scoring <- read_csv(sprintf("outputs/scoring_final_%s.csv",context))%>%
    mutate(water_score = factor(water_score),
           sanit_score = factor(sanit_score),
           score_final = factor(score_final),
           key_score = if_else(str_detect(key_score, "NA"), NA_character_, key_score),
           key_water = if_else(str_detect(key_water, "NA"), NA_character_, key_water),
           key_sanit = if_else(str_detect(key_sanit, "^NA"), NA_character_, key_sanit)
    )
  
  cluster_id <- full_AP$indicator_code[full_AP$indicator_code=="cluster_id"]
  weights <- full_AP$indicator_code[full_AP$indicator_code=="weights"]
  
  ### Formating data_scoring
  design_data_scoring <- as_survey_design(data_scoring,ids=cluster_id, weights = weights)%>%
    mutate(water_score = as.factor(water_score),
           sanit_score = as.factor(sanit_score))
  
  var_to_analyse <- c("water_score", "key_water",
                      "sanit_score", "key_sanit",
                      "score_final", "key_score")
  
  score_agg_table <- data.frame(indicator = NA, choice = NA, value = NA)
  
  for(i in 1:length(var_to_analyse)){
    
    print(var_to_analyse[i])
    
    if(class(design_data_scoring$variables[[var_to_analyse[i]]]) %in% c("factor", "character")){
      design_data_scoring$variables[[var_to_analyse[i]]] <- factor(design_data_scoring$variables[[var_to_analyse[i]]])
      score_agg_table <- design_data_scoring%>%
        filter(!is.na(!!sym(var_to_analyse[i])))%>%
        group_by(!!sym(agg_level),!!sym(var_to_analyse[i]))%>%
        summarise(value= survey_mean(na.rm = TRUE))%>%
        mutate(indicator = var_to_analyse[i])%>%
        rename(choice = as.character(var_to_analyse[i]))%>%
        select(indicator, choice, value)%>%
        bind_rows(score_agg_table)
    }else{
      score_agg_table <- design_data_scoring%>%
        filter(!is.na(!!sym(var_to_analyse[i])))%>%
        group_by(!!sym(agg_level))%>%
        summarise(!!sym(var_to_analyse[i]):= survey_mean(!!sym(var_to_analyse[i]), na.rm = TRUE))%>%
        mutate(indicator = var_to_analyse[i],
               choice = NA, value = !!sym(var_to_analyse[i]))%>%
        select(indicator, choice, value)%>%
        bind_rows(score_agg_table)
      
    }
  }
  
  score_agg_table$context <- context
  
  
  # score_agg_table <- mean_prop_working(design = design_data_scoring,
  #                               list_of_variables = c("water_score", "key_water",
  #                                                     "sanit_score", "key_sanit",
  #                                                     "score_final", "key_score"),
  #                               aggregation_level = "admin2")%>%
  #                    pivot_longer(-admin2)%>%
  #   separate(name, into = c("indicator", "choice"), sep = "\\.")%>%
  #   mutate(context = dryrunname)
  return(score_agg_table)
}


admin2_agg <- function(df_name, context,context_AP, WSC_AP, data = NULL, weights = NULL ){
  
  full_AP <- context_AP%>%
    filter(context_AP$context == context)%>%
    left_join(WSC_AP, by = "indicator_code")%>%
    filter(data_source_name == df_name)
  
  
  names(data)<-car::recode(names(data),"c('x_uuid','X_uuid','_uuid')='uuid'")
  
  cluster_id <- full_AP$indicator[full_AP$indicator_code=="cluster_id"]
  if(length(cluster_id) == 0){
    data$cluster_id <- data$admin2
    
  }
  weights_ap <- full_AP$indicator[full_AP$indicator_code=="weights"]
  
    if(is.null(weights) & length(weights_ap) == 0){
      data$weights <- 1
      weights <- "weights"
    }else if(length(weights_ap) != 0){
      weights <- weights_ap
    }
  
  
  from <- full_AP$indicator
  to <- full_AP$indicator_code
  
  var_to_analyse <- unique(full_AP$indicator_code)[!is.na(unique(full_AP$indicator_code))]
  var_to_analyse <- var_to_analyse[!var_to_analyse %in% c("admin1", "admin2", "admin3","weights", "cluster_id")]  
  
  names(data)<-r3c(names(data),from,to)
  
  reduced_data <- data%>%
    select(starts_with(all_of(var_to_analyse)), weights, cluster_id, admin2)
  
  design_data <- as_survey(reduced_data ,ids= cluster_id, weights = weights)
  
  addVars_agg_table <- data.frame(admin2=NA, indicator = NA, choice = NA, value = NA)
  
  select_multiple_in_data<-auto_detect_select_multiple(design_data$variables)
  select_multiples_in_var_to_analyse<-var_to_analyse[which(var_to_analyse%in%select_multiple_in_data)]
  
  if(length(select_multiples_in_var_to_analyse)>0){
    select_multiples_in_data_with_dot<-paste0(select_multiple_in_data,".")
    select_multiples_in_given_list_with_dot<-paste0(select_multiples_in_var_to_analyse, ".")
    vars_selection_helper <- paste0("^(", paste(select_multiples_in_given_list_with_dot, collapse="|"), ")")
    # vars_selection_helper <- paste0("^(", paste(select_multiples_in_data_with_dot, collapse="|"), ")")
    select_multiple_logical_names<-select(design_data$variables, matches(vars_selection_helper)) %>%
      select(-ends_with("_other")) %>% colnames()
    var_to_analyse_no_concatenated_select_multiple<-var_to_analyse [which(var_to_analyse%in%select_multiple_in_data==FALSE)]
    var_to_analyse<-c(var_to_analyse_no_concatenated_select_multiple,select_multiple_logical_names)
  }
  if(length(select_multiples_in_var_to_analyse)==0){
    var_to_analyse<-var_to_analyse
  }
  
  
  for(i in 1:length(var_to_analyse)){
    
    print(var_to_analyse[i])
    
    if(class(design_data$variables[[var_to_analyse[i]]]) %in% c("factor", "character")){
      design_data$variables[[var_to_analyse[i]]] <- factor(design_data$variables[[var_to_analyse[i]]])
      addVars_agg_table <- design_data%>%
        filter(!is.na(!!sym(var_to_analyse[i])))%>%
        group_by(admin2,!!sym(var_to_analyse[i]))%>%
        summarise(value= survey_mean(na.rm = TRUE))%>%
        mutate(admin2 = admin2, indicator = var_to_analyse[i])%>%
        rename(choice = as.character(var_to_analyse[i]))%>%
        select(admin2, indicator, choice, value)%>%
        bind_rows(addVars_agg_table)
      
    }else{
      addVars_agg_table <- design_data%>%
        filter(!is.na(!!sym(var_to_analyse[i])))%>%
        group_by(admin2)%>%
        summarise(!!sym(var_to_analyse[i]):= survey_mean(!!sym(var_to_analyse[i]), na.rm = TRUE))%>%
        mutate(admin2 = admin2, indicator = var_to_analyse[i],
               choice = NA, value = !!sym(var_to_analyse[i]))%>%
        select(admin2, indicator, choice, value)%>%
        bind_rows(addVars_agg_table)
      
    }
    
  }
  
  addVars_agg_table <- addVars_agg_table%>%
    separate(indicator, into = c("indicator", "choice2"), sep = "\\.")%>%
    mutate(context = context,
           choice = case_when(!is.na(choice2)~ as.character(choice2), 
                              TRUE ~ as.character(choice))
    )%>%
    select(admin2, indicator, choice, value)%>%
    mutate(context = context)%>%
    filter(!is.na(indicator))
  
  return(addVars_agg_table)
  
}

admin1_to_admin2_agg <- function(df_name, context,context_AP, WSC_AP, data = NULL, admin2_df = admin2){
  
  full_AP <- context_AP%>%
    filter(context_AP$context == context)%>%
    left_join(WSC_AP, by = "indicator_code")%>%
    filter(data_source_name == df_name)
  
  cluster_id <- "admin1"
  data$cluster_id <- data$admin1
  
  data$weights <- 1
  
  names(data)<-car::recode(names(data),"c('x_uuid','X_uuid','_uuid')='uuid'")
  
  from <- full_AP$indicator
  to <- full_AP$indicator_code
  
  var_to_analyse <- unique(full_AP$indicator_code)[!is.na(unique(full_AP$indicator_code))]
  var_to_analyse <- var_to_analyse[!var_to_analyse %in% c("admin1", "admin2", "admin3","weights", "cluster_id")]  
  
  names(data)<-r3c(names(data),from,to)
  
  
  addVars_agg_table <- data%>%
    select(any_of(var_to_analyse), admin1)
  
  addVars_agg_table_adm2 <- admin2_df%>%
    left_join(addVars_agg_table, by = "admin1")%>%
    pivot_longer(any_of(var_to_analyse), names_to = "indicator", values_to = "value")%>%
    mutate(
      context = context,
           choice = NA
    )%>%
    select(admin2, indicator, choice, value)%>%
    mutate(context = context)%>%
    filter(!is.na(indicator))
  

  return(addVars_agg_table_adm2)
  
}

scoring_AP <- function(data, context_AP,
                       WSC_AP = analysis_plan, hh_data, context){

  full_AP <- context_AP%>%
    filter(context_AP$context == context)%>%
    left_join(WSC_AP, by = "indicator_code")%>%
    rename(minimal = "None/ minimal", stress = "Stressed", crisis = "Crisis", critical = "Critical", catastrophic = "Catastrophic")%>%
    mutate(across(c(minimal,stress, crisis, critical, catastrophic), function(col){ str_replace_all(as.character(col), '\\"', "'")}))
  
  ap_scaled <- full_AP%>%
   filter(globally_scaled == TRUE & wash_scoring == FALSE)
  
  
  area_indic <- ap_scaled$indicator_code[ap_scaled$level == "area"]
  hh_indic <- unique(ap_scaled$indicator_code[ap_scaled$level == "hh"])
  
  
  scores_AP <- ap_scaled%>%
    select(indicator_code, minimal, stress, crisis, critical, catastrophic)%>%
    rowwise()%>%
    mutate(across(c(minimal,stress, crisis, critical, catastrophic), function(col){ str_replace_all(col, as.character(indicator_code), "value")}))%>%
    mutate(across(c(minimal,stress, crisis, critical, catastrophic), function(col){ case_when(
      col == "NULL" ~ NA_character_,
      col == "FALSE" ~ NA_character_,
      TRUE ~ col
    )}))
  
  
  area_data <- data%>%
    left_join(scores_AP, by = c("indicator" = "indicator_code"))%>%
    filter(indicator %in% !!area_indic)%>%
    rowwise()%>%
    mutate(
      score_final = case_when(
        eval(parse(text = critical)) & eval(parse(text = catastrophic)) ~ "4-5",
        eval(parse(text = critical)) & eval(parse(text = crisis)) ~ "3-4",
        eval(parse(text = stress)) & eval(parse(text = crisis)) ~ "2-3",
        eval(parse(text = stress)) & eval(parse(text = minimal)) ~ "1-2",
        eval(parse(text = catastrophic)) ~ "5",
        eval(parse(text = critical)) ~ "4",
        eval(parse(text = crisis)) ~ "3",
        eval(parse(text = stress)) ~ "2",
        eval(parse(text = minimal)) ~ "1",
        TRUE ~ NA_character_
      )
    )%>%
    mutate(choice = "Phase", value = score_final, context = "bfa_2020")%>%
    select(admin2, indicator, choice, value, context)
  
  from <- full_AP$indicator
  to <- full_AP$indicator_code
  names(hh_data)<-r3c(names(hh_data),from,to)
  
  
  hh_data_df <- hh_data%>%
    select(admin2, !!hh_indic, weights, cluster_id)%>%
    group_by(admin2)%>%
    mutate(
      fcs_score = case_when(
        fcs_score == "acceptable" ~ "1-2",
        fcs_score == "limite" ~ "3",
        fcs_score == "pauvre" ~ "4",
        TRUE ~ NA_character_
      ),
      lcsi_score = case_when(
        lcsi_score == "none" ~ "1",
        lcsi_score == "stress" ~ "2",
        lcsi_score == "crisis" ~ "3",
        lcsi_score == "emergency" ~ "4",
        TRUE ~ NA_character_
      ),
      rcsi_score = case_when(
        rcsi_score >= 0 & rcsi_score <4 ~ "1",
        rcsi_score >= 4 & rcsi_score <19 ~ "2",
        rcsi_score >= 19 ~ "3",
        TRUE ~ NA_character_
      ),
      hhs_score = case_when(
        hhs_score == 0 ~ "1",
        hhs_score == 1 ~ "2",
        hhs_score > 1 & hhs_score < 4 ~ "3",
        hhs_score == 4 ~ "4",
        hhs_score > 4 ~ "5"
      )
    )%>%
    as_survey(ids = cluster_id, weights = weights)
  
  addVars_agg_table <- data.frame(admin2 = NA, indicator = NA, choice = NA, value = NA)
  
  for(i in 1:length(hh_indic)){
    
    print(hh_indic[i])
    
    if(class(hh_data_df$variables[[hh_indic[i]]]) %in% c("factor", "character")){
      hh_data_df$variables[[hh_indic[i]]] <- factor(hh_data_df$variables[[hh_indic[i]]])
      addVars_agg_table <- hh_data_df%>%
        filter(!is.na(!!sym(hh_indic[i])))%>%
        group_by(admin2,!!sym(hh_indic[i]))%>%
        summarise(value= survey_mean(na.rm = TRUE))%>%
        mutate(admin2 = admin2, indicator = hh_indic[i])%>%
        rename(choice = as.character(hh_indic[i]))%>%
        select(admin2, indicator, choice, value)%>%
        bind_rows(addVars_agg_table)
    }else{
      addVars_agg_table <- hh_data_df%>%
        filter(!is.na(!!sym(hh_indic[i])))%>%
        group_by(admin2)%>%
        summarise(!!sym(hh_indic[i]):= survey_mean(!!sym(hh_indic[i]), na.rm = TRUE))%>%
        mutate(admin2 = admin2, indicator = hh_indic[i],
               choice = NA, value = !!sym(hh_indic[i]))%>%
        select(admin2, indicator, choice, value)%>%
        bind_rows(addVars_agg_table)
      
    }
    
  }
  
  addVars_agg_table <- addVars_agg_table%>%
    separate(indicator, into = c("indicator", "choice2"), sep = "\\.")%>%
    mutate(context = context,
           choice = case_when(!is.na(choice2)~ as.character(choice2), 
                              TRUE ~ as.character(choice))
    )%>%
    select(admin2, indicator, choice, value)%>%
    mutate(context = context)%>%
    filter(!is.na(indicator))
  
  addVars_agg_table_phases <- lapply(unique(addVars_agg_table$indicator), function(x){
    twenty_rule(data = addVars_agg_table, col_score = "indicator", col_label = "choice",
                                         name_final_score = x, col_agg = "admin2", col_value = "value")
  })%>%do.call(rbind,.)%>%
    mutate(choice= "Phase", value = score_final)%>%
    select(admin2, indicator, choice, value, context)
    
  
  final_data <- rbind(addVars_agg_table_phases, area_data)
  
  return(final_data)
}


admin_agg <- function(df_name, context,context_AP, WSC_AP, data = NULL, weights = NULL, target_admlvl){
  
  data$admin0 <- context
  
  full_AP <- context_AP%>%
    filter(context_AP$context == context)%>%
    left_join(WSC_AP, by = "indicator_code")%>%
    filter(data_source_name == df_name)
  
  
  names(data)<-car::recode(names(data),"c('x_uuid','X_uuid','_uuid')='uuid'")
  
  cluster_id <- full_AP$indicator[full_AP$indicator_code=="cluster_id"]
  if(length(cluster_id) == 0){
    data$cluster_id <- data[,target_admlvl]
    cluster_id <- NULL
  }
  weights_ap <- full_AP$indicator[full_AP$indicator_code=="weights"]
  
  if(is.null(weights) & length(weights_ap) == 0){
    data$weights <- 1
    weights <- "weights"
  }else if(length(weights_ap) != 0){
    weights <- weights_ap
  }

  from <- full_AP$indicator
  to <- full_AP$indicator_code
  
  var_to_analyse <- unique(full_AP$indicator_code)[!is.na(unique(full_AP$indicator_code))]
  var_to_analyse <- var_to_analyse[!var_to_analyse %in% c("admin1", "admin2", "admin3","weights", "cluster_id")]  
  
  names(data)<-r3c(names(data),from,to)
  
  
  reduced_data <- data%>%
    select(starts_with(all_of(var_to_analyse)), weights, cluster_id, !!target_admlvl)
  if(length(cluster_id) == 0){
    design_data <- as_survey(reduced_data ,ids= !!cluster_id, weights = weights)
  }else{
    design_data <- as_survey(reduced_data ,ids= cluster_id, weights = weights)
  }
  
  addVars_agg_table <- tibble(!!target_admlvl :=NA, indicator = NA, choice = NA, value = NA)
  
  select_multiple_in_data<-auto_detect_select_multiple(design_data$variables)
  select_multiples_in_var_to_analyse<-var_to_analyse[which(var_to_analyse%in%select_multiple_in_data)]
  
  if(length(select_multiples_in_var_to_analyse)>0){
    select_multiples_in_data_with_dot<-paste0(select_multiple_in_data,".")
    select_multiples_in_given_list_with_dot<-paste0(select_multiples_in_var_to_analyse, ".")
    vars_selection_helper <- paste0("^(", paste(select_multiples_in_given_list_with_dot, collapse="|"), ")")
    # vars_selection_helper <- paste0("^(", paste(select_multiples_in_data_with_dot, collapse="|"), ")")
    select_multiple_logical_names<-select(design_data$variables, matches(vars_selection_helper)) %>%
      select(-ends_with("_other")) %>% colnames()
    var_to_analyse_no_concatenated_select_multiple<-var_to_analyse [which(var_to_analyse%in%select_multiple_in_data==FALSE)]
    var_to_analyse<-c(var_to_analyse_no_concatenated_select_multiple,select_multiple_logical_names)
  }
  if(length(select_multiples_in_var_to_analyse)==0){
    var_to_analyse<-var_to_analyse
  }
  
  
  for(i in 1:length(var_to_analyse)){
    
    print(var_to_analyse[i])
    
    if(class(design_data$variables[[var_to_analyse[i]]]) %in% c("factor", "character")){
      design_data$variables[[var_to_analyse[i]]] <- factor(design_data$variables[[var_to_analyse[i]]])
      addVars_agg_table <- design_data%>%
        filter(!is.na(!!sym(var_to_analyse[i])))%>%
        group_by(!!sym(target_admlvl),!!sym(var_to_analyse[i]))%>%
        summarise(value= survey_mean(na.rm = TRUE))%>%
        mutate(!!target_admlvl := !!sym(target_admlvl), indicator = var_to_analyse[i])%>%
        rename(choice = as.character(var_to_analyse[i]))%>%
        select(!!target_admlvl, indicator, choice, value)%>%
        bind_rows(addVars_agg_table)
      
    }else{
      addVars_agg_table <- design_data%>%
        filter(!is.na(!!sym(var_to_analyse[i])))%>%
        group_by(!!sym(target_admlvl))%>%
        summarise(!!sym(var_to_analyse[i]):= survey_mean(!!sym(var_to_analyse[i]), na.rm = TRUE))%>%
        mutate(!!target_admlvl := !!sym(target_admlvl), indicator = var_to_analyse[i],
               choice = NA, value = !!sym(var_to_analyse[i]))%>%
        select(!!target_admlvl, indicator, choice, value)%>%
        bind_rows(addVars_agg_table)
      
    }
    
  }
  
  addVars_agg_table <- addVars_agg_table%>%
    separate(indicator, into = c("indicator", "choice2"), sep = "\\.")%>%
    mutate(context = context,
           choice = case_when(!is.na(choice2)~ as.character(choice2), 
                              TRUE ~ as.character(choice))
    )%>%
    select(!!target_admlvl, indicator, choice, value)%>%
    mutate(context = context)%>%
    filter(!is.na(indicator))
  
  return(addVars_agg_table)
  
}

admin0_agg_prvAnalysed <- function(df_name , context , context_AP,weights = NULL,
                                   WSC_AP, data, target_admlvl = "admin0", admin2_analysed = c("houet", "gourma", "seno")){
  data_admin0 <- data%>%
    filter(admin2 %in% !!admin2_analysed)
  data_admin0 <- admin_agg(df_name, context, context_AP,
                           WSC_AP, data, target_admlvl = "admin0", weights)
  
  return(data_admin0)
}


