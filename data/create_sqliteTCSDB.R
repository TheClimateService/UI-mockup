db = dbConnect(drv=RSQLite::SQLite(), dbname="data/db.sqliteTCSDB")
dbRemoveTable(ValueAtRisk)
dbSendQuery(conn = db, "CREATE TABLE ValueAtRisk
              (LocationID INTEGER,
              TCFDCategoryID TEXT,
              TCFDSubCatID TEXT,
              ScenarioID INTEGER,
              ModeledYear INTEGER,
              ValueAtRisk DECIMAL
              )")