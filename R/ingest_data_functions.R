

load_file <- function(file_name, res_path){
  message("loading ", file_name)
  res_df <- readr::read_delim(file.path(res_path,file_name), delim="\t")
  point_no <- stringi::stri_extract(file_name, regex="(?<=_)[0-9]+(?=_)")
  rep_no <- stringi::stri_extract(file_name, regex="(?<=_)[0-9]+(?=\\.)")
  res_df %<>% dplyr::mutate(Point=point_no, 
                     Repetition=rep_no)
  return(res_df)
}


load_params <- function(file_name, res_path){
  message("loading ", file_name)
  par_df <- readr::read_delim(file.path(res_path, file_name), 
                              delim="\t",  
                              col_names=c("Param", "Value"),
                              col_types="cc")
  point_no <- stringi::stri_extract(file_name, regex="(?<=_)[0-9]+(?=_)")
  par_df %<>% tidyr::spread(Param,Value) %>% mutate(Point=point_no)
  return(par_df)
}
