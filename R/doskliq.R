#' Get path to doskliq executable
#'
#' @return A string representing the path to the doskliq executable
doskliq_exe_path <- function(){
  exe_name <- switch(Sys.info()[['sysname']], Windows='doskliq.exe', 'doskliq')
  file.path(system.file(package="kliqfindr"), "bin", exe_name)
}
