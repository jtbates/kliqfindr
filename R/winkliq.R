#' Get path to winkliq installation
#'
#' @return A string representing the path to the winkliq installation directory
winkliq_install_path <- function(){
  # TODO error checking: OS is Windows and folder is found
  return('C:/KLIQFIND')
}

#' Get path to winkliq executable
#'
#' @return A string representing the path to the winkliq executable
winkliq_exe_path <- function(){
  install_path <- winkliq_install_path()
  exe_name <- 'kliqfind.exe'
  # TODO error checking: OS is Windows and file is found
  return(file.path(install_path, exe_name))
}

#' Run winkliq executable
#'
#' @param input_file The path to the input datafile
#' @param working_dir The path to the working directory
#' @param setup The setup configuration
#' @return An error code from system2 (0 for success)
winkliq_run <- function(input_file, working_dir, setup='basic'){
  # TODO error checking:
  # * working_dir exists and is directory
  # * setup batch file is found and executes successfully
  # * input file is found and copy is successful
  input_file <- tools::file_path_as_absolute(input_file)
  setup_bat <- file.path(winkliq_install_path(), 'setups', paste0(setup, '.bat'))

  # execute commands in the working directory
  wd <- getwd()
  setwd(working_dir)
  res_setup <- system2(setup_bat)
  res_copy <- file.copy(input_file, working_dir)
  res_kf <- system2(winkliq_exe_path(), args=basename(input_file))
  setwd(wd)
  return(res_kf)
}


#' Read a place file
#'
#' @param place_file The path to the place file
#' @return A data frame with each actor's subgroup assignment
read_plc <- function (place_file) {
  # TODO complete column names? V1 is internal actor ID, actor is user specified ID. V4 and V5?
  read.table(place_file,
             col.names=c('V1', 'actor', 'subgroup', 'V4', 'V5'))
}
