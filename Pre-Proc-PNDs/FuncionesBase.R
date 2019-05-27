#rm(list=ls())

### Generación de una lista de stopwords desde varias fuentes

ListaStopwords <- function(useTM=TRUE, # Usar lista stopwords paquete tm
                           useranks.nl=TRUE, # Usar lista ranks.nl
                           useGH=TRUE, # Usar lista github
                           useNombres=TRUE, # Incluir nombres propios como sws
                           # Stopwords adicionales; ejm: aaa
                           vectorAddi=c(), # Vector terminos añadir
                           listAddi=NA, # Path lista terminos añadir
                           # No son stopwords; ejm: empleo
                           vectorExclus=c(), # Vector terminos excluir
                           listExclus=NA, # Path lista terminos excluir
                           saveFile=FALSE # Guardar lista en archivo de texto
){
  hayVA <- !is.na(listAddi) | !(length(vectorAddi)==0)
  if(!hayVA & !useTM & !useranks.nl & !useGH & !useNombres) return(c())
  
  stpwrds <- c()
  
  # Lista TM
  if(useTM) stpwrds <- union(stpwrds, stopwords::stopwords("spanish"))
  
  # Lista ranks.nl
  if(useranks.nl){
    swdsnl <- readLines("swdsranknl.txt", encoding ="UTF-8")
    stpwrds <- union(stpwrds, swdsnl[-1])
  }
  
  # Lista GH
  if(useGH){
    swdsgh <- readLines("swdsgh.txt", encoding ="UTF-8")
    stpwrds <- union(stpwrds, swdsgh[-1])
  }
  
  # Nombres Propios 
  if(useNombres){
    swdsnms <- readLines("swdsnm.txt", encoding="UTF-8")
    stpwrds <- union(stpwrds, swdsnms[-1])
  }
  
  # Vector términos a añadir
  if(length(vectorAddi)>0){
    stpwrds <- union(stpwrds, as.character(vectorAddi))
  }
  
  # Archivo términos a añadir
  if(!is.na(listAddi)){
    swdsaddi <- readLines(listAddi, encoding="UTF-8")
    stpwrds <- union(stpwrds, swdsaddi[-1])
  }
  
  # Vector términos a excluir
  if(length(vectorExclus)>0){
    iExcl <- which(stpwrds %in% vectorExclus)
    stpwrds <- stpwrds[-iExcl]
  }
  
  # Archivo términos a excluir
  if(!is.na(listExclus)){
    swdsexclu <- readLines(listExclus, encoding="UTF-8")
    iExcl <- which(stpwrds %in% swdsexclu)
    stpwrds <- stpwrds[-iExcl]
  }
  
  if(saveFile){
    conn <- file("listaStopwords.txt",encoding = "UTF-8")
    on.exit(close(conn))
    writeLines(sort(stpwrds), con = conn)
    print("Lista de palabras guardada en ../listaStopwords.txt")
  }
  
  # return
  return(sort(stpwrds))
}

#### Ejemplo uso
# milista <- ListaStopwords(useNombres=TRUE, listAddi = "testlistaddi.txt",
#                          listExclus = "testlistaexcl.txt", saveFile = TRUE)


### Generación lista de nombres geográficos colombianos.
ListaLocs <- function(rawList = "geocolombia.txt", # Lista base estándar
                      saveFile = FALSE # Guardar lista en archivo de texto
){
  require(magrittr)
  geo <- readLines(rawList,encoding="UTF-8")[-1]
  geo <- sort(unique(geo))
  geo <- geo  %>% 
    gsub("[^[:print:]]", " ", .) %>%
    tolower %>% 
    gsub("[^[:lower:]^[:space:]]", " ", .) %>%
    gsub("[[:space:]]{1,}", " ", .) %>%
    trimws
  
  #geo2 <- gsub(" ", "_", geo)
  
  ord <- order(lengths(strsplit(geo, " ")), decreasing = TRUE)
  geo <- geo[ord]
  
  if(saveFile){
    conn <- file("listaGeo.txt", encoding = "UTF-8")
    on.exit(close(conn))
    writeLines(geo, con = conn)
    print("Lista de palabras guardada en ../listaGeo.txt")
  }
  
  return(geo)
}

#### Ejemplo uso
# geo <- ListaLocs()

BaseRemoval <- function(word, text){
  wordA <- paste("\\<", word, "\\>", sep="")
  text <- gsub(wordA, "", text)
  text <- trimws(text)
  text <- gsub("[[:space:]]{2,}", " ", text)
  return(text)
}

RemoveStopwordsFromText <- function(texto, # texto
                                    swords # términos _o frases_ a remover
){
  for(i in 1:length(swords)){
    if((i-1)%%100==0) print(c(i-1, length(swords)))
    texto <- BaseRemoval(swords[i], texto)
  }
  
  return(texto)
}

# Ejemplo uso
# load("61.RData")
# textoLimpio1 <- RemoveStopwordsFromText(text.conp, geo)
#textoLimpio2 <- RemoveStopwordsFromText(textoLimpio1, milista)