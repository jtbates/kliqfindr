#' Get path to doskliq executable
#'
#' @return A string representing the path to the doskliq executable
doskliq_exe_path <- function(){
  exe_name <- switch(Sys.info()[['sysname']], Windows='doskliq.exe', 'doskliq')
  file.path(system.file(package='kliqfindr'), 'bin', exe_name)
}

#' Get path to doskliq shell script
#'
#' @return A string representing the path to the doskliq shell script
doskliq_sh_path <- function(){
  sh_name <- 'doskliq.sh'
  file.path(system.file(package='kliqfindr'), 'src', 'doskliq', sh_name)
}

#' Run doskliq executable
#'
#' @param input_path The path to read the input files from
#' @param output_path The path to write the output files to
doskliq_run <- function(input_path, output_path){
  input_path <- normalizePath(input_path)
  output_path <- normalizePath(output_path)
  cmd_path <- file.path(input_path, 'test.cmd')
  list_path <- file.path(input_path, 'test.list')

  wd <- getwd()
  setwd(output_path)
  bash_cmd <- paste0('{ echo "\\"',
                     cmd_path,
                     '\\""; echo "\\"',
                     list_path,
                     '\\""; } | ',
                     doskliq_exe_path())

  res <- system(bash_cmd)

  setwd(wd)
  return(res)
}
