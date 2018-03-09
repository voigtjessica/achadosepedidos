
#divisão da planilha em 5

library(dplyr)
library(data.table)

setwd("C:\\Users\\jvoig\\Documents\\Achados_e_pedidos\\planilha_cgu")
cgu_upload <- fread("cgu_upload.csv", encoding = "UTF-8")

cgu_upload[] <- lapply(cgu_upload, gsub, pattern=';', replacement='/')

19074/5

cgu_upload1 <-cgu_upload %>%
  slice(1:3814)

cgu_upload2 <-cgu_upload %>%
  slice(3815:7628)

cgu_upload3 <-cgu_upload %>%
  slice(7629:11442)

cgu_upload4 <-cgu_upload %>%
  slice(11443:15256)

cgu_upload5 <-cgu_upload %>%
  slice(15257:n())

setwd("C:\\Users\\jvoig\\Documents\\Achados_e_pedidos\\planilha_cgu\\fragmentada")
write.table(cgu_upload1 , file="cgu_upload1.csv", 
            sep=";", row.names=F, na="", quote = T, fileEncoding = "UTF-8")
write.table(cgu_upload2 , file="cgu_upload2.csv", 
            sep=";", row.names=F, na="", quote = T, fileEncoding = "UTF-8")
write.table(cgu_upload3 , file="cgu_upload3.csv", 
            sep=";", row.names=F, na="", quote = T, fileEncoding = "UTF-8")
write.table(cgu_upload4 , file="cgu_upload4.csv", 
            sep=";", row.names=F, na="", quote = T, fileEncoding = "UTF-8")
write.table(cgu_upload5 , file="cgu_upload5.csv", 
            sep=";", row.names=F, na="", quote = T, fileEncoding = "UTF-8")
