library(rvest)

url <- "https://repositorio.ufu.br/handle/123456789/5471?offset=0"

urls <- vector() #Vetor que armazena todas as urls de cada página
offset <- seq(from=0,to=240,by=20)

j=1 #Pega a ulr inicial e faz um vetor com a url de todas as páginas
for(i in offset){
  urlpagpre = paste(strsplit(url,"offset=")[[1]][1],"offset=",sep="")
  urlpag <- paste(urlpagpre,i,sep="")
  urls[j] <- urlpag
  j=j+1
}

total_Urls <- length(urls)
links <- vector() # Vetor que armazena a referência de cada dissertação

#Pega o link de referência de todas as dissertações
for(i in 1:total_Urls){
pagina <- read_html(urls[i])

#Pega o link de cada dissertação na página
plinks <- pagina %>%
  html_node("[class='table']") %>%
  html_nodes("[headers='t2']") %>%
  html_node("a") %>%
  html_attr("href")

links <- c(links,plinks)
}

total_Links <- length(links)

dis_Links <- vector() #Vetor que armazena o link de todas as dissertações

#Junta a referência de cada redação com a url e armazena o link no vetor
for(i in 1:total_Links){
  urlpre = paste(strsplit(url,"/handle/123456789/")[[1]][1])
  hrefdis = paste(links[i])
  urllinkdis <- paste(urlpre,hrefdis,sep="")
  
  dis_Links[i] <- urllinkdis
}

DFtextos <- as.data.frame(matrix(ncol = 9,nrow = 1))
colnames(DFtextos) <- c("data","titulo","titulo_alt","resumo","abstract","autor","orientador","prim_mem_banca","seg_mem_banca")

f=1
i=1
for(i in 1:total_Links){
  urllink <- dis_Links[i]
  dissertacao <- read_html(urllink)
  
  disData <- dissertacao %>%
    html_node("[class='metadataFieldValue dc_date_issued']") %>%
    html_text()
    
  disTitulo <- dissertacao %>%
    html_node("[class='metadataFieldValue dc_title']") %>%
    html_text()
    
  disTitulo_alt <- dissertacao %>%
    html_node("[class='metadataFieldValue dc_title_alternative']") %>%
    html_text()
  
  disResumo <- dissertacao %>%
    html_node("[class='metadataFieldValue dc_description_resumo']") %>%
    html_text()
  
  disAbstract <- dissertacao %>%
    html_node("[class='metadataFieldValue dc_description_abstract']") %>%
    html_text()
  
  disAutor <- dissertacao %>%
    html_node("[class='metadataFieldValue dc_creator']") %>%
    html_text()
  
  disOrientador <- dissertacao %>%
    html_node("[class='metadataFieldValue dc_contributor_advisor1']") %>%
    html_text()
  
  dis1MemBanca <- dissertacao %>%
    html_node("[class='metadataFieldValue dc_contributor_referee1']") %>%
    html_text()
  
  dis2MemBanca <- dissertacao %>%
    html_node("[class='metadataFieldValue dc_contributor_referee2']") %>%
    html_text()
  
  for(f in 1:length(disData)){
    newRow <- data.frame(disData[f],disTitulo[f],disTitulo_alt[f],disResumo[f],disAbstract[f],disAutor[f],disOrientador[f],dis1MemBanca[f],dis2MemBanca[f])
    names(newRow) <- names(DFtextos)
    DFtextos <- rbind(DFtextos,newRow)
  }
}

write.csv(DFtextos,"Dissertações.csv")