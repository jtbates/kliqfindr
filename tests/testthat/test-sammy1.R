
# input values
input_path <- file.path('input', 'sammy1')
test_cmd <- read.table(file.path(input_path, 'test.cmd'))
maxiter <- as.integer(test_cmd[1,1])
attachi <- as.integer(test_cmd[1,2])

test_list <- read.table(file.path(input_path, 'test.list'),
                        colClasses=rep('integer',3))
datamat <- matrix(as.integer(0), nrow=100, ncol=100)
datamat[as.matrix(test_list[,c(1,2)])] <- test_list[,3]
numact <- max(test_list[,c(1,2)])

output_path_r <- file.path('output', 'sammy1', 'kliqfindr')
res <- klique_find(datamat, numact, maxiter=maxiter, attachi=attachi,
                   output_path=output_path_r)
kliqfindr.plc <- get_plc(res)

output_path_dos <- file.path('output', 'sammy1', 'doskliq')
doskliq_run(input_path, output_path_dos)

doskliq.plc <- read.fwf(file.path(output_path_dos, 'kliqfind.plc'), c(5,5,5))

test_that("kliqfindr and doskliq output same for sammy1 data",
  expect_true(all.equal(kliqfindr.plc, doskliq.plc, check.names=FALSE)))

