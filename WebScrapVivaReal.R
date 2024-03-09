# References:
# http://notesofdabbler.github.io/201408_hotelReview/scrapeTripAdvisor.html
# https://github.com/hadley/rvest/blob/master/demo/tripadvisor.R
# http://notesofdabbler.github.io/201408_hotelReview/scrapeTripAdvisor.html

library(rvest)
#library(xlsx)
#library(tm)
#library(lsa)
library(dplyr)
library(data.table)
library(readxl)

#######Todos links
#Pega o link apos aplicar o filtro:
#urllinkmain <- "https://www.vivareal.com.br/aluguel/sp/sao-paulo/zona-sul/vila-nova-conceicao/apartamento_residencial/?__vt=plp:b#onde=BR-Sao_Paulo-NULL-Sao_Paulo-Zona_Sul-Vila_Nova_Conceicao&tipos=apartamento_residencial"
#urllinkmain <- "https://www.vivareal.com.br/venda/minas-gerais/uberlandia/bairros/santa-monica/#onde=,Minas%20Gerais,Uberl%C3%A2ndia,Bairros,Santa%20M%C3%B4nica,,,neighborhood,BR%3EMinas%20Gerais%3ENULL%3EUberlandia%3EBarrios%3ESanta%20Monica,-18.918538,-48.248448,&itl_id=1000183&itl_name=vivareal_-_botao-cta_buscar_to_vivareal_resultado-pesquisa"
#urllinkmain <- "https://www.vivareal.com.br/venda/minas-gerais/uberlandia/bairros/santa-monica/#onde=Brasil,Minas%20Gerais,Uberl%C3%A2ndia,Bairros,Santa%20M%C3%B4nica,,,,BR%3EMinas%20Gerais%3ENULL%3EUberlandia%3EBarrios%3ESanta%20Monica,,,"
urllinkmain <- "https://www.vivareal.com.br/aluguel/minas-gerais/uberlandia/bairros/santa-monica/#onde=Brasil,Minas%20Gerais,Uberl%C3%A2ndia,Bairros,Santa%20M%C3%B4nica,,,,BR%3EMinas%20Gerais%3ENULL%3EUberlandia%3EBarrios%3ESanta%20Monica,,,"

#Verifica o total de resultados da busca:
openCon <- urllinkmain %>% read_html

total_busca <- openCon %>%
  html_node("[class='results-summary__count js-total-records']") %>%
  html_text()

#Transforma em numeric (\\. para escapar so do "." que significa "all characters")
total_busca <- as.numeric(gsub("\\.", "", total_busca))

#Calcula ate que pagina vamos (truncado)
pag <- trunc(total_busca / 36)

DFLink <- vector()

for (i in 2:pag){
     
  #Fase 1: insere no main link a pagina
  #urllinkpre=paste(strsplit(urllinkmain,"plp:b")[[1]][1],"plp:&",sep="")
  #urllinkpost=strsplit(urllinkmain,"plp:b")[[1]][2]
  #pagina = paste("pagina=",i)
  #pagina = gsub("\\s","",pagina)
  #novolink = paste(urllinkpre,pagina,urllinkpost, sep="")
  
  urllinkpre=paste(strsplit(urllinkmain,"#onde")[[1]][1],"?",sep="")
  urllinkpost=strsplit(urllinkmain,"#onde")[[1]][2]
  pagina = paste("pagina=",i,"#onde")
  pagina = gsub("\\s","",pagina)
  novolink = paste(urllinkpre,pagina,urllinkpost, sep="")
  
  #Fase 2: tira um pedaço do link que desaparece ao clicar em "proxima pagina"
  #urllinkpre=paste(strsplit(novolink,"/apartamento_residencial")[[1]][1],"",sep="")
  #urllinkpost=strsplit(novolink,"/apartamento_residencial")[[1]][2]
  #urlopen <- paste (urllinkpre,urllinkpost,sep="")
  
  #DFLink[i] <- urlopen
  DFLink[i] <- novolink

}

#Insere na primeira posicao do vetor o link "main", o do resultado da busca
DFLink[1] <- urllinkmain
#Cria o DF que insere todos os titulos completos
DFtextos <- as.data.frame(matrix(ncol = 1,nrow=1))
colnames(DFtextos) <- c("name.f")
DFtextos$name.f <- as.character(DFtextos$name.f)
f <- 1
#######Captura de infos dos imoveis
i = 1
for (i in 1:length(DFLink)){
  
  url <- DFLink[i]
  
  openCon <- url %>% read_html 
  
  #Aqui captura TUDO em um grande caracter
  name <- openCon %>%
    html_nodes("[class='property-card__content']") %>%
    html_text()
  
  #titulo_anunc <- openCon %>%
    #html_nodes("[class='property-card__title js-cardLink js-card-title']") %>%
    #html_nodes("[class='property-card__content']") %>%
    #html_text()  
  
  #Nao funciona pois tem alguns espacos entre o nome da classe, parecem uns enters
  #enderecos <- openCon %>%
    #html_nodes("[class='property-card__address js-property-card-address']") %>%
    #html_text()  
  
  #ammenities <- openCon %>%
  #  html_nodes("[class='property-card__amenities']")
  #  html_text()
  
  #Guarda os 36 textoes de cada imovel no DF
  for (f in 1:length(name)){
  
  newRow <- data.frame(name[f])
  names(newRow) <- names(DFtextos)
  DFtextos <- rbind(DFtextos,newRow)
  }
}

#####Teste para tratamento dos super textos
#Usei para descobrir que o texto comeca no 6 caracter/espaco
strsplit(DFtextos[2,], " ")
#Remove todos espacos "sobrando"
gsub("\\s+", " ", DFtextos[2,])

i = 1
metros2 <- vector()
quartos <- vector()
aluguel <- vector()
for (i in 2:nrow(DFtextos)){
  anunc_terms <- strsplit(DFtextos[i,], " ")
  #Pega dados
  #m2 <- anunc_terms[[1]][11]
  #metros2[i] <- m2
  metros2[i] <- anunc_terms[[1]][11]
  qt <- anunc_terms[[1]][7]
  quartos[i] <- ifelse(is.na(qt)|qt=="",1,qt)
  #for (j in 1:lengths(anunc_terms)){
    indice_mes <- which(anunc_terms[[1]] %in% "/Mês")
    aluguel[i] <- anunc_terms[[1]][indice_mes - 1]
  #}
  
}

#Cria DF unico para exportar
DFunico <- data.frame(metros2,quartos,aluguel)

####################
write.csv(DFunico, "DFunico.csv")
#######