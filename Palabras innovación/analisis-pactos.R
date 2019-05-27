
trace(utils:::unpackPkgZip, edit = T)
# Buscar lista de palabras y n-gramas dentro de cada uno de los pactos del PND

# Archivos con texto limpio de los pactos
pactos <- list.files(path = 'C:/Users/ScmayorquinS/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/PND-Innovation/Palabras innovación/docs/PlainTxts/2018/',pattern = ".*.txt",full.names=T)

# Lista de caracteres con cada pacto y la introducción
lista.pactos <- lapply(pactos, function(x) paste(readLines(x),collapse = " "))


# Lista de palabras y n-gramas a buscar dentro de los pactos
objetivo <- readLines("C:/Users/ScmayorquinS/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/PND-Innovation/Palabras innovación/palabras_clave_inn.txt",warn = F)

preproctext <- function(x){
  require(magrittr)
  require(stringi)
  x[which(is.na(x))] <- ""
  y <- x %>% 
    #iconv(.,from="utf-8",to="ASCII//TRANSLIT") %>%
    gsub("[^[:print:]]", " ", .) %>%
    tolower %>% 
    gsub("[^[:lower:]^[:space:]]", " ", .) %>%
    gsub("[[:space:]]{1,}", " ", .) %>%
    trimws
  return(y)
}

# Funciones veloces de preprocesamiento
source("C:/Users/ScmayorquinS/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/PND-Innovation/Pre-Proc-PNDs/FuncionesBase.R")

# Preprocesar palabras y todos los pactos
objetivo.clean <- lapply(objetivo, function(x) preproctext(x))
lista.pactos.clean <- lapply(lista.pactos, function(x) preproctext(x))

# Stop words para este problema
sw <- readLines(paste0("C:/Users/ScmayorquinS/OneDrive - Departamento Nacional de Planeacion/DIDE/2019/Data Science Projects/PND-Innovation/Pre-Proc-PNDs/stop_words_spanish.txt"),warn = F)

# Remover stop words para palabras y para todos los pactos
objetivo.clean <- lapply(objetivo.clean, function(x) RemoveStopwordsFromText(texto = x,swords = sw)) 
lista.pactos.clean <- lapply(lista.pactos.clean, function(x) RemoveStopwordsFromText(texto = x,swords = sw))

library(stringi)
# Remover tildes
objetivo.clean <- stri_trans_general(str = objetivo.clean,id = "Latin-ASCII")
lista.pactos.clean <- stri_trans_general(str = lista.pactos.clean,id = "Latin-ASCII")


#-------------------------------------------------


library(tm)
library(tidytext)
library(openxlsx) # No necesita Java

# Volatile Corpus
my.corpus = VCorpus(VectorSource(lista.pactos.clean))


# This tokenizer is built on NLP and creates bigrams. 
# If you want multi-grams specify 1:2 for uni- and bi-gram, 
# 2:3 for bi- and trigram, 1:3 for uni-, bi- and tri-grams.
# etc. etc. ...(ngrams(words(x), 1:3)...

bigram_tokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 1:3), paste, collapse = " "), use.names = FALSE)
}


dtm <- DocumentTermMatrix(my.corpus, control=list(tokenizer = bigram_tokenizer, dictionary = objetivo.clean))
inspect(dtm)

DF <- tidy(dtm)

# Restarle uno a toda la columna para cuadrar con número del pacto
DF$document <- as.numeric(DF$document)
DF$document <- DF$document - 1

# Reemplazar 0 por introducción
DF$document <- gsub(pattern = "\\b0\\b",replacement = 'introducción',x = DF$document)

# Exportar Excel
#openxlsx::write.xlsx(DF, "C:\\Users\\cmayorquin\\Desktop\\PND\\PNDTT\\Palabras innovación\\conteo-palabras-pacto.xlsx")

# Palabras únicas
prueba <- unlist(strsplit(lista.pactos.clean[6],' '))
prueba <- unlist(strsplit(as.character(lista.pactos[6]),split = ' '))

# sumar palabras de DF
para.nube <- DF[,2:3]

para.nube <- aggregate(count ~ term, data=para.nube, sum)
openxlsx::write.xlsx(para.nube, "C:\\Users\\cmayorquin\\Desktop\\PND\\PNDTT\\Palabras innovación\\para.nube.xlsx")

# Nube de palabras
library(wordcloud2)
library(wordcloud)
install.packages(c("SnowballC", "wordcloud", "RCurl", "XML"))

wordcloud(words = para.nube$term,freq = para.nube$count)

pal2 <- brewer.pal(8,"Dark2")
wordcloud(para.nube$term,para.nube$count, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
#------------------------------------------------------------------------------------------------------------
# Buscar frases con los n gramas
#------------------------------------------------------------------------------------------------------------

# Archivos con texto limpio de los pactos
pactos.puntuacion <- list.files(path = 'C:/Users/cmayorquin/Desktop/PND/PNDTT/Palabras innovación/docs/PlainTxts/2018/',pattern = ".*.txt",full.names=T)

# Lista de caracteres con cada pacto y la introducción
lista.pactos.puntuacion <- lapply(pactos.puntuacion, function(x) paste(readLines(x),collapse = " "))

# Quitar toda puntuación excepto period, question mark, y exclamation mark
pre.frases <- gsub("[^[:alnum:][:space:].]", "", lista.pactos.puntuacion)

preproctext <- function(x){
  require(magrittr)
  require(stringi)
  x[which(is.na(x))] <- ""
  y <- x %>% 
    #iconv(.,from="utf-8",to="ASCII//TRANSLIT") %>%
    gsub("[^[:print:]]", " ", .) %>%
    tolower %>% 
    #gsub("[^[:lower:]^[:space:]]", " ", .) %>%
    gsub("[^[:alnum:]^[:space:](.)]", " ", .) %>%
    gsub("[[:space:]]{1,}", " ", .) %>%
    trimws
  return(y)
}

pre.frases <- preproctext(pre.frases)
# Quitar números
pre.frases.clean <- removeNumbers(x = pre.frases)
# Trwim
pre.frases.clean <- trimws(x = pre.frases.clean)


# Objetivo 2
objetivo.clean2 <- tolower(objetivo)

# Dividir frases en cada punto
lst <- unlist(strsplit(pre.frases.clean[1], "(?<=[a-z]\\.\\s)", perl=TRUE))
# Lista de frases por cada pacto del PND
frases <- lapply(pre.frases.clean, function(x) unlist(strsplit(x, "(?<=[a-z]\\.\\s)",perl = T)) )

# Quitar frase fastidiosa
frases <-  lapply(frases, function(x) gsub(pattern = "bases del plan nacional de desarrollo   pacto por colombia pacto por la equidad",replacement = "",x))
frases <-  lapply(frases, function(x) gsub(pattern = " o y introducción ",replacement = "",x))


library(tidyverse)
library(rlist)
# ejemplo
lenguaje <- lapply(frases, function(x) grep("lenguaje claro", x, value = T))
names(lenguaje) <- seq_along(lenguaje)

#lenguaje <- list.clean(lenguaje,function(x) length(x) == 0L, TRUE)
lenguaje <- compact(keep(lenguaje,negate(is_empty)))

# La mejor hasta ahora
lenguaje <- do.call("rbind", lapply(lenguaje, as.data.frame))
colnames(lenguaje) <- 'lenguaje claro'
lenguaje <- cbind(rownames(lenguaje), lenguaje)
colnames(lenguaje) <- c('pacto','lenguaje claro')
rownames(lenguaje) <- NULL


# Lista de 24 palabras, cada una con una lista de los 29 pactos donde aparecen estas palabras
contenedor <- list()
for(i in 1:length(objetivo.clean2)) {
  contenedor[[i]] <- lapply(frases, function(x) grep(objetivo.clean2[i], x, value = T))
  #contenedor <- lapply(contenedor, function(i) compact(keep(i,negate(is_empty))))  
}

dd <- data.frame(t(sapply(contenedor,c)))

dd <- as.data.frame(t(as.matrix(data.frame(t(sapply(contenedor,c))))))
filas_n <- seq(1:28)
rownames(dd) <- filas_n
library(data.table)
dd <-  setDT(dd, keep.rownames = TRUE)[]
dd$rn <- as.numeric(dd$rn)
rownames(dd) <- NULL
dd$rn <-  dd$rn - 1

dd$rn[which(dd$rn==0)] = "introducción"

colnames(dd) <-  c('capítulo',objetivo.clean2)

# Reemplazar número de los capítulo por su respectivo nombre

nombre_capitulos <- c("Introducción","I. Pacto por la legalidad","II. Pacto por el emprendimiento, la formalización y la productividad",
                      "III. Pacto por la equidad","IV. Pacto por la sostenibilidad",
                      "V. Pacto por la Ciencia, la Tecnología y la innovación","VI. Pacto por el transporte y la logística para la competitividad y la integración regional",
                      "VII. Pacto por la transformación digital de Colombia","VII. Pacto por la calidad y eficiencia de servicios públicos",
                      "IX. Pacto por los recursos minero-energéticos para el crecimiento sostenible y la expansión de oportunidades",
                      "X. Pacto por la protección y promoción de nuestra cultura y desarrollo de la economía naranja",
                      "XI. Pacto por la Construcción de Paz","XII. Pacto por la equidad de oportunidades para grupos étnicos",
                      "XIII. Pacto por la inclusión de todas las personas con discapacidad","XIV. Pacto de equidad para las mujeres",
                      "XV. Pacto por una gestión pública efectiva","XVI. Pacto por la descentralización",
                      "XVII. Pacto Región Pacífico","XVIII. Pacto Región Caribe","XIX. Pacto Seaflower Region",
                      "XX. Pacto Región Central","XXI. Pacto región Santanderes","XXII. Pacto Región Amazonia","XXIII. Pacto Eje Cafetero y Antoquia","XXIV. Pacto Región Llanos-Orinoquia",
                      "XXV. Pacto Región Océanos","XXVI. Consistencia macroeconómica, fiscal y de resultados económicos y sociales","XXVII. Metas del  PND  y cumpllimiento de los ODS")

dd$capítulo <- nombre_capitulos



# Exportar Excel de frases
openxlsx::write.xlsx(dd, "C:\\Users\\cmayorquin\\Desktop\\PND\\PNDTT\\Palabras innovación\\frases-contexto.xlsx")
