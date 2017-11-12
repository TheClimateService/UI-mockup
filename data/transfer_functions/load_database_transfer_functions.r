
tf = read.table("./data/transfer_functions/Prototype-Data-Model-2_transferfunctions.csv", sep=";", header=TRUE)
database_tf = as.tibble(tf)
test_select_tf = database_tf %>% filter(Transfer_Function_Name=="CPL-to-Fees")

