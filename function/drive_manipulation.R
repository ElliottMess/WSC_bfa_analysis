sub_folder_ls <- function(aw_path, folder_name){
  folder_content <- drive_ls(aw_path)
  sub_folder_id <- as_id(folder_content$id[folder_content$name == folder_name])
  sub_folder_content <- drive_ls(sub_folder_id)
  return(sub_folder_content)
}

prv_fldr_ls <- function(aw_path){
  repo_content <- sub_folder_ls(as_id(aw_path),"Répertoire de preuves" )
  province_folder_id <- as_id(repo_content$id[repo_content$name == "Données provinces"])
  province_folder_content <- drive_ls(province_folder_id)
  return(province_folder_content)
}

prv_fldr_dir_cntnt <- function(dir_cntnt_df, prv_name){
  prv_folder_id <- as_id(dir_cntnt_df$id[dir_cntnt_df$name == prv_name])
  drive_ls(prv_folder_id)
}

copy_wrksheet_prvFolder <- function(aw_path, prv_name, worksheet_template_fr_path){
  aw_path <- as_id(aw_path)
  all_provinces_folder <- prv_fldr_ls(aw_path)
  repo_content <- sub_folder_ls(aw_path,"Répertoire de preuves" )
  id_donnees_provinces <- as_id(repo_content$id[repo_content$name == "Données provinces"])
  
  if(!prv_name %in% all_provinces_folder$name){
    drive_mkdir(prv_name, as_id(id_donnees_provinces))
  }
  new_all_provinces_folder <- prv_fldr_ls(aw_path)
  
  id_province_folder <- as_id(new_all_provinces_folder$id[new_all_provinces_folder$name == prv_name])
  province_folder_content <- drive_ls(id_province_folder)
  
  file_name <- paste0("CSW_FeuilleAnalyse_",prv_name)
  
  if(!file_name %in% province_folder_content$name){
    drive_cp(worksheet_template_fr_path, path = id_province_folder, name = file_name, overwrite = TRUE)
    
  }
}

copied_worksheet_path <- function(aw_path, prv_name){
  all_provinces_folder <- prv_fldr_ls(aw_path)
  repo_content <- sub_folder_ls(aw_path,"Répertoire de preuves" )
  id_donnees_provinces <- as_id(repo_content$id[repo_content$name == "Données provinces"])
  prov_folder_ls <- sub_folder_ls(id_donnees_provinces,prv_name )
  file_name <- paste0("CSW_FeuilleAnalyse_",prv_name)
  sheet_id <- as_id(prov_folder_ls$id[prov_folder_ls$name == file_name])
  return(sheet_id)
}

