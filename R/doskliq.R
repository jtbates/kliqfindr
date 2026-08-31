#' Get path to doskliq executable
#'
#' @return A string representing the path to the doskliq executable
doskliq_exe_path <- function(){
  exe_name <- switch(Sys.info()[['sysname']], Windows='doskliq.exe', 'doskliq')
  return(system.file('bin', exe_name, package='kliqfindr'))
}

#' Run doskliq executable
#'
#' @param input_path The path to read the input files from
#' @param output_path The path to write the output files to
doskliq_run <- function(input_path, output_path){
  cmd_path <- file.path(input_path, 'test.cmd')
  list_path <- file.path(input_path, 'test.list')
  file.copy(c(cmd_path, list_path), output_path)

  # execute command in the output directory
  wd <- getwd()
  setwd(output_path)
  res <- system2(doskliq_exe_path(),
                 input=c('test.cmd', 'test.list'))

  setwd(wd)
  return(res)
}
