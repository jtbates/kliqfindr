
dir_create_if_not_exists <- function(path, recursive=TRUE) {
  if (!dir.exists(path)) return(dir.create(path, recursive=recursive))
  else return(TRUE)
}

