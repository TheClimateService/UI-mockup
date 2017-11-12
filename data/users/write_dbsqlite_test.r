
con = dbConnect(drv=RSQLite::SQLite(), dbname="/Users/terry/working_mac/os/apps/django/tcs/user_interface/db.sqlite3")
alltables = dbListTables(con)
dbtitle="django/tcs/user_interface/db.sqlite3"

newtable=tibble(
  id = 1:5, 
  x = 1:5, 
  y = 1, 
  z = x^3 + y
)

# The following reads the contents of an existing table and puts it into a df.
from = dbReadTable(con, "UI_newtable")

# The following appends to a table already defined in the UI via django.
# It sets the id to the next higher value based on the current length of that table.
table=tibble(id=length(from)+1, x="XX", y="YY", z="ZZ")
dbWriteTable(con, "UI_newtable", table, append=TRUE)

# The following updates a specified row in the UI table.
dbExecute(con, "UPDATE UI_newtable SET x='XXX' WHERE id='4'")

#dbWriteTable(con, "UI_newtable", newtable)
alltables = dbListTables(con)

#dbRemoveTable(con, "newtable")

