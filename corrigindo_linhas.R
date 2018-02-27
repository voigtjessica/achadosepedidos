#Corrigindo linhas

library(data.table)
library(dplyr)

setwd("C:\\Users\\jvoig\\Documents\\Achados_e_pedidos\\import_erro")

pendencias <- fread("Pendencias_CGU_importador.csv", encoding="UTF-8")


# Tutorial com gsub e loops : http://www.endmemo.com/program/R/gsub.php

# Substituindo ; e " 

y <- pendencias %>%
  mutate(protocolo = as.character(protocolo),
         data = as.character(data))

y$Erro <- gsub("[[:punct:]]", "", y$Erro) 
y$interac <- gsub("[[:punct:]]", "", y$interac) 
y$conteudo <- gsub(";", ",", y$conteudo)


write.table(y, file="pendencias_resolv.csv", 
            sep=";", row.names=F, na="", quote = F)
