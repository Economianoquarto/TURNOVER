get_and_save <- function(api, filename, dir = my_dir) {
  
  df <- sidrar::get_sidra(api = api)
  
  writexl::write_xlsx(df, paste0(dir, "/", filename, ".xlsx"))
}