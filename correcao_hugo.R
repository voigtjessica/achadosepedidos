library(data.table)
library(dplyr)
library(stringi)
library(magrittr)
library(RMySQL)
library(tidyr)

setwd("C:\\Users\\jvoig\\Documents\\Achados_e_pedidos\\20180129_Arquivos_csv_2017")

# carrega cabeçalho do banco
load("head_pedidos.RData")

## abre tbl no bloco de notas e salva como utf-8

cgu1 <- fread("20180129_Pedidos_csv_2017_utf8.txt", encoding = "UTF-8")

cgu1 <- cgu1 %>%
  set_colnames(as.character(nome$nome))

load("agentes.RData")
head(agentes)

# ClassificacaoTipoResposta # se em branco, em Tramitação

cgu1 <- cgu1 %>%
  mutate(CodigoTipoPedidoSituacao = ifelse(ClassificacaoTipoResposta == '', 1, 2)) %>%
  rename(CodigoAgente = Codigo)


## importando recursos

recursos <- fread("20180129_Recursos_csv_2017_utf8.txt", encoding = "UTF-8")

# var <- read.table("clipboard", header=T)
# save(var, file="head_recursos.RData")
load("head_recursos.RData")

nome_var <- as.character(var$var)
nome_var <- c(nome_var[1:3], nome_var[16], nome_var[c(4:15,17)]) # ordem das colunas errada

recursos <- recursos %>%
  set_colnames(nome_var) %>%
  mutate(ProtocoloPedido = as.character(ProtocoloPedido))

## pedidos sem recursos
cgu_sem_recursos <-  cgu1 %>%
  mutate(ProtocoloPedido = as.character(ProtocoloPedido)) %>%
  left_join(select(recursos, "ProtocoloPedido", "Instancia"), by="ProtocoloPedido") %>%
  filter(is.na(Instancia))

#85470 pedidos sem recursos

## Preparando para template

tmpl_cgusem <- cgu_sem_recursos %>%
  gather(interac, conteudo, DetalhamentoSolicitacao, Resposta) %>%
  mutate(id = "",
         protocolo = ProtocoloPedido,
         titulo = ResumoSolicitacao,
         conteudo = conteudo,
         interac = interac,
         data = ifelse(interac == "DetalhamentoSolicitacao", DataRegistro, DataResposta),
         prorrog = FoiProrrogado,
         orgao = OrgaoDestinatario,
         status = TipoResposta,
         sit = Situacao,
         uf = "",
         munic = "",
         esf = "Executivo",
         niv = "Federal",
         pastanx = "",
         nomeanex = "") %>%
  select(id,protocolo,titulo,conteudo,interac,data,prorrog,orgao,status,sit,uf,munic,esf,niv,pastanx,
         nomeanex) %>%
  mutate(interac = ifelse(interac == "DetalhamentoSolicitacao", "Pedido", "Resposta do Pedido"),
         status = gsub("Acesso Concedido", "Atendido", status),
         status = gsub("Acesso Negado", "Não Atendido", status),
         status = gsub("Acesso Parcialmente Concedido", "Parcialmente Atendido", status),
         status = gsub("Encaminhado para o e-Ouv", "Não classificado", status),
         status = gsub("Informação Inexistente", "Não atendido", status),
         status = gsub("Não se trata de solicitação de informação", "Não classificado", status),
         status = gsub("Órgão não tem competência para responder sobre o assunto", "Não classificado", status),
         status = gsub("Pergunta Duplicada/Repetida", "Não classificado", status),
         status = gsub("Não atendido", "Não Atendido", status),
         sit = gsub("Respondido", "Finalizado", sit),
         prorrog = gsub("NÃO", "Não", prorrog),
         prorrog = gsub("SIM", "Sim", prorrog))

tmpl_cgusem %>%
  filter(interac == "Pedido") %>%
  group_by(status) %>%
  summarise(contagem = n()) %>%
  mutate(total = sum(contagem),
         perc = round(contagem/total, 2))

#tirando os ; da planilha

tmpl_cgusem[] <- lapply(tmpl_cgusem, gsub, pattern=';', replacement='/')

write.table(tmpl_cgusem , file="pedidos_cgu_2017_sem_recurso.csv", 
            sep=";", row.names=F, na="", quote = T, fileEncoding = "UTF-8")


## pedidos com recursos
cgu_com_recursos <-  cgu1 %>%
  mutate(ProtocoloPedido = as.character(ProtocoloPedido)) %>%
  inner_join(select(recursos, "ProtocoloPedido", "Instancia"), by="ProtocoloPedido")

#9537 pedidos com recursos

## Preparando para template

cgu_com_recurso_templ <- cgu_com_recursos %>%
  gather(interac, conteudo, DetalhamentoSolicitacao, Resposta) %>%
  mutate(id = "",
         protocolo = ProtocoloPedido,
         titulo = ResumoSolicitacao,
         conteudo = conteudo,
         interac = interac,
         data = ifelse(interac == "DetalhamentoSolicitacao", DataRegistro, DataResposta),
         prorrog = FoiProrrogado,
         orgao = OrgaoDestinatario,
         status = TipoResposta,
         sit = Situacao,
         uf = "",
         munic = "",
         esf = "Executivo",
         niv = "Federal",
         pastanx = "",
         nomeanex = "") %>%
  select(id,protocolo,titulo,conteudo,interac,data,prorrog,orgao,status,sit,uf,munic,esf,niv,pastanx,
         nomeanex) %>%
  mutate(interac = ifelse(interac == "DetalhamentoSolicitacao", "Pedido", "Resposta do Pedido"),
         status = gsub("Acesso Concedido", "Atendido", status),
         status = gsub("Acesso Negado", "Não Atendido", status),
         status = gsub("Acesso Parcialmente Concedido", "Parcialmente Atendido", status),
         status = gsub("Encaminhado para o e-Ouv", "Não classificado", status),
         status = gsub("Informação Inexistente", "Não atendido", status),
         status = gsub("Não se trata de solicitação de informação", "Não classificado", status),
         status = gsub("Órgão não tem competência para responder sobre o assunto", "Não classificado", status),
         status = gsub("Pergunta Duplicada/Repetida", "Não classificado", status),
         status = gsub("Não atendido", "Não Atendido", status),
         sit = gsub("Respondido", "Finalizado", sit),
         prorrog = gsub("NÃO", "Não", prorrog),
         prorrog = gsub("SIM", "Sim", prorrog))


cgu_com_recurso_templ %>%
  filter(interac == "Pedido") %>%
  group_by(status) %>%
  summarise(contagem = n()) %>%
  mutate(total = sum(contagem),
         perc = round(contagem/total, 2))

cgu_com_recurso_templ[] <- lapply(cgu_com_recurso_templ, gsub, pattern=';', replacement='/')

write.table(cgu_com_recurso_templ, file="pedidos_cgu_2017_com_recurso.csv", 
            sep=";", row.names=F, na="", quote = T, fileEncoding = "UTF-8")

##Recursos no template
recursos <- recursos %>%
  mutate(DataRegistro= as.Date(DataRegistro, "%d/%m/%Y"))


recursos_templ <- recursos %>%
  gather(interac, conteudo, DescRecurso, RespostaRecurso) %>%
  mutate(id = "",
         protocolo = ProtocoloPedido,
         titulo = "",
         conteudo = conteudo,
         interac = interac,
         data = ifelse(interac == "DetalhamentoSolicitacao", DataRegistro, DataResposta),
         prorrog = "",
         orgao = OrgaoDestinatario,
         status = TipoResposta,
         sit = Situacao,
         uf = "",
         munic = "",
         esf = "Executivo",
         niv = "Federal",
         pastanx = "",
         nomeanex = "") %>%
  select(id,protocolo,titulo,conteudo,interac,data,prorrog,orgao,status,sit,uf,munic,esf,niv,pastanx,
         nomeanex, Instancia) %>%
  mutate(data= as.Date(data, "%d/%m/%Y"))

recursos_templ <- recursos_templ %>%
  arrange(protocolo) %>%
  group_by(protocolo, interac) %>%
  arrange(data) %>%
  mutate(contagem_instancia = 1:n(),
         instancia_final = case_when(contagem_instancia == 1 & interac == "DescRecurso" ~ "Recurso - 1ª Instância",
                                     contagem_instancia == 1 & interac == "RespostaRecurso" ~ "Resposta do Recurso - 1ª Instância",
                                     contagem_instancia == 2 & interac == "DescRecurso" ~ "Recurso - 2ª Instância",
                                     contagem_instancia == 2 & interac == "RespostaRecurso" ~ "Resposta do Recurso - 2ª Instância",
                                     contagem_instancia == 3 & interac == "DescRecurso" ~ "Recurso - 3ª Instância",
                                     contagem_instancia == 3 & interac == "RespostaRecurso" ~ "Resposta do Recurso - 3ª Instância",
                                     contagem_instancia == 4 & interac == "DescRecurso" ~ "Recurso - 4ª Instância",
                                     contagem_instancia == 4 & interac == "RespostaRecurso" ~ "Resposta do Recurso - 4ª Instância")) %>%
  ungroup() %>%
  mutate(interac = instancia_final) %>%
  select(-Instancia, -instancia_final, -contagem_instancia)

recursos_templ <- recursos_templ %>%
  mutate(status = gsub("Parcialmente deferido", "Parcialmente Atendido", status),
         status = gsub("Deferido", "Atendido", status),
         status = gsub("Indeferido", "Não Atendido", status),
         status = gsub("Perda de objeto", "Não Atendido", status),
         status = gsub("Acolhimento", "Atendido", status),
         status = gsub("Perda de objeto parcial", "Parcialmente Atendido", status),
         status = gsub("Não conhecimento", "Não Atendido", status),
         status = gsub("Não Atendido parcial", "Parcialmente Atendido", status),
         sit = gsub("Respondido", "Finalizado", sit))

recursos_templ[] <- lapply(recursos_templ, gsub, pattern=';', replacement='/')

write.table(recursos_templ, file="recursos_cgu.csv", 
            sep=";", row.names=F, na="", quote = T, fileEncoding = "UTF-8")
