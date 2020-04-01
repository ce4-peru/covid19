require(XML)

setwd("C:/Users/Micaela/Documents/covid19PE-LIEZ")

data <- xmlParse("data/abasto/001-PER-INEI-CENAMA-2016.xml")

xml_data <- xmlToList(data)

#https://stackoverflow.com/questions/17198658/how-to-parse-xml-to-r-data-frame
#https://subscription.packtpub.com/book/big_data_and_business_intelligence/9781783989065/1/ch01lvl1sec11/reading-xml-data
