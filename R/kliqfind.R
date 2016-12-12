#' Run the KliqueFinder algorithm
#' @param datamat The matrix of contact frequency
#' @param numact The number of actors
#' @param maxiter The maximum number of iterations to go through before
#'                stopping on account of non-convergence
#' @param attachi Flag if set to 1 isolates are to be added even if it results
#'                in a decrease in theta1
#' @param output_path The path to write the output files to
#'
#' @return The result list from the KliqueFinder Fortran code
klique_find <- function(datamat, numact, maxiter=999, attachi=1, output_path='.') {
  wd <- getwd()
  setwd(output_path)

  # input values
  maxwt <- max(datamat)

  # return values
  group <- integer(100)
  onbad <- integer(100)
  pair <- integer(100)
  groupid <- integer(100)

  res <- .Fortran('kliqfindr', maxiter=maxiter, attachi=attachi,
                  datamat=datamat, numact=numact, maxwt=maxwt,
                  group=group, onbad=onbad, pair=pair, groupid=groupid)

  setwd(wd)
  return(res)
}


#' Calculate the subgroup memberships
#' @param res A result list from the KliqueFinder Fortran code
#'
#' @return A data.frame with actor IDs, subgroup membership, and paired actor
get_plc <- function(res) {
  N = res$numact
  actor <- 1:N
  subgroup <- res$groupid[res$group[actor]]
  taggedto <- rep(NA, N)
  taggedto[(res$onbad == 1)[1:N]] <- -1
  taggedto[(res$onbad == 2)[1:N]] <- res$pair[actor[(res$onbad == 2)[1:N]]]

  plc <- data.frame(actor=actor, subgroup=subgroup, taggedto=taggedto)
  return(plc)
}

