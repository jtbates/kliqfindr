input_name <- 'n16790'

# input values
input_path <- file.path('input', input_name)
test_cmd <- read.table(file.path(input_path, 'test.cmd'))
maxiter <- as.integer(test_cmd[1,1])
attachi <- as.integer(test_cmd[1,2])

test_list <- read.table(file.path(input_path, 'n16790.list'),
                        colClasses=rep('integer',3))
datamat <- matrix(as.integer(0), nrow=100, ncol=100)
datamat[as.matrix(test_list[,c(1,2)])] <- test_list[,3]
numact <- max(test_list[,c(1,2)])

output_path_r <- file.path('output', input_name, 'kliqfindr')
res <- klique_find(datamat, numact, maxiter=maxiter, attachi=attachi,
                   output_path=output_path_r)
kliqfindr.plc <- get_plc(res)

# write input as fixed width file
fwf_input_path <- file.path('input', input_name, 'test.list')
writeLines(sprintf("%10i%10i%10i", test_list[,1], test_list[,2], test_list[,3]),
           fwf_input_path)

output_path_dos <- file.path('output', input_name, 'doskliq')
doskliq_run(input_path, output_path_dos)

doskliq.plc <- read.fwf(file.path(output_path_dos, 'kliqfind.plc'), c(5,5,5))

test_that(paste("kliqfindr and doskliq output same", input_name, "data"),
          expect_true(all.equal(kliqfindr.plc, doskliq.plc, check.names=FALSE)))
