library(RSQLite)

con = dbConnect(drv = SQLite(), dbname = "BAZ.db")

transac <- dbGetQuery(con,
    "SELECT 
    	NPAIS, 
    	NCANAL, 
    	NSUCURSAL, 
    	NFOLIO, 
    	HASH_MAP_CU, 
    	HASH_MAP_CTA,
    	T071_NUM_OPERATION, 
    	T071_CODE, 
    	T071_AMOUNT, 
    	T071_DAT_OPERATION, 
    	T071_TIM_OPERATION
  	FROM transacciones_2014_2015;  
  ")

saveRDS(transac, "./transacciones_2014_2015.RDS")

