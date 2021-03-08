

# To download NDVI layers

#install.packages("sen2r")
library(sen2r)

# per entrar al "scihub" cal posar uid i pwd concrets:
write_scihub_login("servusbitte", "bitteservus")
# des del pc del despatx m'ha demanat aquestes llibreries també:
#install.packages(c("leaflet", "leafpm", "mapedit", "shinyFiles", "shinydashboard", "shinyjs",
                   #"shinyWidgets", "xml2","geojsonlint"))

# per a obrir la eina com a finestra o també web
sen2r()
?write_scihub_login


# Semana 1-7 Febrero 2019
safe_is_online("C:/Users/ANA~1.SAN/DOCUME~1/SEN2R~1/lta_orders/lta_20201020_125047.json")
sen2r("C:/Users/ANA~1.SAN/DOCUME~1/SEN2R~1/proc_par/s2proc_20201020_125033.json")

s2_order("C:/Users/ANA~1.SAN/DOCUME~1/SEN2R~1/lta_orders/lta_20201020_125047.json", service = "dhus")
s2_download("C:/Users/ANA~1.SAN/DOCUME~1/SEN2R~1/lta_orders/lta_20201020_125047.json")

# Semana 8-15 Febrero 2019
safe_is_online("C:/Users/ANA~1.SAN/DOCUME~1/SEN2R~1/lta_orders/lta_20201020_174534.json")
sen2r("C:/Users/ANA~1.SAN/DOCUME~1/SEN2R~1/proc_par/s2proc_20201020_174512.json")

# Semana 16-22 Febrero 2019
safe_is_online("C:/Users/ANA~1.SAN/DOCUME~1/SEN2R~1/lta_orders/lta_20201020_174938.json")
sen2r("C:/Users/ANA~1.SAN/DOCUME~1/SEN2R~1/proc_par/s2proc_20201020_174926.json")

# Semana 23-28 Febrero 2019
safe_is_online("C:/Users/ANA~1.SAN/DOCUME~1/SEN2R~1/lta_orders/lta_20201020_175142.json")
sen2r("C:/Users/ANA~1.SAN/DOCUME~1/SEN2R~1/proc_par/s2proc_20201020_175131.json")