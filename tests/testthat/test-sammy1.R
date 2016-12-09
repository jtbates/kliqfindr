test_cmd <- read.table("../sammy1/test.cmd")
maxiter <- as.integer(test_cmd[1,1])
attachi <- as.integer(test_cmd[1,2])

test_list <- read.table("../sammy1/test.list", colClasses=rep("integer",3))
datamat <- matrix(as.integer(0), nrow=100, ncol=100)
datamat[as.matrix(test_list[,c(1,2)])] <- test_list[,3]
numact <- max(test_list[,c(1,2)])
maxwt <- max(datamat)


wd <- getwd()
setwd("../sammy1/output_kliqfindr")
res <- .Fortran("kliqfindr", maxiter=maxiter, attachi=attachi, datamat=datamat,
                numact=numact, maxwt=maxwt)

setwd(wd)
system(paste("bash doskliq.sh", doskliq_exe_path(),
             "../sammy1", "../sammy1/output_doskliq"))

cmd <- paste("diff", "../sammy1/output_doskliq/kliqfind.plc",
             "../sammy1/output_kliqfindr/kliqfind.plc")
test_that("kliqfindr and doskliq output same for sammy1 data",
  expect_equal(length(suppressWarnings(system(cmd, intern=TRUE))), 0))

