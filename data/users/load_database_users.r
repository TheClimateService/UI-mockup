
dbname = "./data/users/users.csv"
users = read.table(dbname, sep=";", header=TRUE)
database_users = as.tibble(users)
test_select_user = database_users %>% filter(Name=="Joe Brown")

