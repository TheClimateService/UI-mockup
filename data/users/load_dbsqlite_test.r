
con = dbConnect(drv=RSQLite::SQLite(), dbname="/Users/terry/working_mac/os/apps/django/tcs/user_interface/db.sqlite3")
alltables = dbListTables(con)
from1 = dbReadTable(con, "UI_user")
from2 = dbReadTable(con, "UI_newtable")
dbtitle="django/tcs/user_interface/db.sqlite3"


table12=alltables[12]
p1 = dbSendQuery( con,'select * from UI_user' )
p2 = dbFetch(p1)
items = p2[,2]
qlist=""
for(i in 1:length(items)) qlist = paste(qlist,items[i])

#p2 = dbGetQuery( con,'select * from UI_newtable' )
#choices = p1[,2]



