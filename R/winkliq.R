#' Get path to winkliq executable
#'
#' @return A string representing the path to the winkliq executable
winkliq_exe_path <- function(){
  exe_name <- switch(Sys.info()[['sysname']], Windows='kliqmain.exe', 'kliqmain')
  return(system.file('bin', exe_name, package='kliqfindr'))
}

#' Get path to smacof1b executable
#'
#' @return A string representing the path to the smacof1b executable
smacof1b_exe_path <- function(){
  exe_name <- switch(Sys.info()[['sysname']], Windows='smacof1b.exe', 'smacof1b')
  return(system.file('bin', exe_name, package='kliqfindr'))
}

#' Run winkliq executable
#'
#' @param input_file The path to the input datafile
#' @param printo_file The path to the printo config file
#' @param param_file The path to the kliqfind.par config file
#' @param working_dir The path to the working directory
#' @param get_place Whether to read the place file
#' @param overwrite Whether to overwrite input files in working directory
#' @return An error code from system2 (0 for success)
#' @examples
#' winkliq_run('stanne.list', 'basic.printo', 'basic.param', get_place=FALSE)
winkliq_run <- function(input_file,
                        printo_file='basic.printo', param_file='basic.param',
                        working_dir=NA, get_place=TRUE, overwrite=TRUE){
  # locate paths
  input_file <- kf_locate_input(input_file)
  printo_file <- kf_locate_input(printo_file)
  param_file <- kf_locate_input(param_file)
  if (is.na(working_dir)) {
    working_dir <- tempfile('kliqfindr_')
    dir.create(working_dir)
  }
  if (!dir.exists(working_dir)) error('no directory found')

  # copy files
  res_copy0 <- file.copy(input_file, working_dir, overwrite=overwrite)
  res_copy1 <- file.copy(printo_file, file.path(working_dir, 'printo'),
                         overwrite=overwrite)
  res_copy2 <- file.copy(param_file, file.path(working_dir, 'kliqfind.par'),
                         overwrite=overwrite)
  res_symlink <- file.symlink(smacof1b_exe_path(), working_dir)

  # execute commands in the working directory
  wd <- getwd()
  setwd(working_dir)
  res_kf <- system2(winkliq_exe_path(), args=basename(input_file))
  setwd(wd)

  res <- list(error_code=res_kf, output_dir=working_dir)
  if (get_place) {
    place_file <- file.path(working_dir,
                            paste0(substr(basename(input_file),1,6), '.place'))
    place <- read_place(place_file)
    res$place <- place
  }
  return(res)
}

#' Get the path to a kliqfindr input file
#'
#' @param input_file The path to an input file or name of file in extdata
#' @return The path to the input file
kf_locate_input <- function(input_file) {
  if (file.exists(input_file)) { input_file }
  else {
    system.file('extdata', input_file,
                package = 'kliqfindr', mustWork = TRUE)
  }
}

#' Read a place file
#'
#' @param place_file The path to the place file
#' @return A data frame with each actor's subgroup assignment
read_place <- function(place_file) {
  # TODO complete column names? V1 is internal actor ID, actor is user specified ID. V4 and V5?
  read.table(place_file,
             col.names=c('V1', 'actor', 'subgroup', 'V4', 'V5'))
}
