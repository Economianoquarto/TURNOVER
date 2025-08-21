# A function that creates directories if they do not already exist and cleans up the files if so
# Author: Gabriel Gaze Gonçalves Fontenele Gomes
# Last script update: 2024-05-30

create_and_clean_dir <- function(dir_path){
  # creates directories if they do not already exist and cleans up the files if so
  # ARGS:
  #   dir_path - the new directory path
  #   
  # Returns: 
  #   a new clean directory in the entered directory path
  
  if (base::class(dir_path) != 'character'){
    stop('Error: input dir_path is not a character')
  }
  
  if(!base::dir.exists(dir_path)){ 
    base::dir.create(dir_path) 
  }
  base::file.remove(base::list.files(dir_path, 
                                     full.names = TRUE)) 
}
