# Rutina de lectura Planes Nacionales de Desarrollo
# Archivos de entrada en formato .pdf
# Árbol de archivos en "../PDFs"
# Legibles, importados con pdftools
# No legibles, procesados (OCR) con tesseract
# Salida de esta rutina:
# Texto plano (no mayúsculas, puntuación, números, secuencias de escape)
# Arreglos de caractéres, un elemento por página
# Un arreglo por capítulo
# Conteos de palabras

# Directorio de trabajo
dir <- "C:/Users/cmayorquin/Desktop/PND/PNDTT/Pre-Proc-PNDs"
setwd("C:/Users/cmayorquin/Desktop/PND/PNDTT/Pre-Proc-PNDs")
remove(list=ls())
require(pdftools)
# La magia
require(tesseract)
#tesseract_download(lang = 'spa')
esp.engine <- tesseract("spa")
require(magrittr)

# Planes Nacionales de Desarrollo a analizar
inicios <- c(2018)
# Por tomo (FALSE  si el documento está dividido por capítulos Cap)
porTomo <- c(TRUE)
# Leer con Tesseract (FALSE si el documento puede copiarse y pegarse)
aOcriar <- c(TRUE)
# Número de archivos (Relacionado directamente con el hecho de venir por tomos o capítulos)
numFils <- c(1)
# Número de años
numAns <- length(inicios)

caps.per.file <- list("1970"=c(2, 14, 3),
                      "1974"=c(1, 3, 4, 5),
                      "1978"=c(1, 3, 2, 7, 2, 4),
                      "1982"=c(2, 1, 1, 1),
                      "1986"=c(2, 1, 1, 1, 1, 1, 1),
                      "1990"=c(2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                      "1994"=c(3, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                      "1998"=c(2, 1, 1, 1, 1, 1, 1),
                      "2002"=c(6),
                      "2006"=c(6, 1),
                      "2010"=c(5, 3),
                      "2014"=c(8, 4))
# Capítulos por archivo, en este caso es un archivo con 29 capítulos
caps.per.file <- list("2018"=c(28))

# 29 archivos fantasma pdf
filers <- vector("list", numAns)
#
names(filers) <- as.character(inicios)

# Para cada i-año, dentro del número de años totales, lo siguiente:

for(ian in 1:numAns){
  # Convertir a caracter los años
  an <- as.character(inicios[ian])
  # Si el año no corresponde a un PND por tomos, sino por capítulos, entonces:
  if(!porTomo[ian]){
    # Formato de C para los capítulos, luego pegar .pdf, y hacerlo caps.per.file veces para cada año
    filers[[an]] <- rep(paste("Cap",sprintf("%02d",1:numFils[ian]),".pdf",sep=""),
                        caps.per.file[[an]])
    # Si en lugar de capítulos el año corresponde a tomos, se sigue el mismo procedimiento cambiando
    # el formateo de C y cambiando 'Cap' por 'Tomo' 
  } else {
    filers[[an]] <- rep(paste("Tomo",sprintf("%01d",1:numFils[ian]),".pdf",sep=""),
                        caps.per.file[[an]])
  }
}

# Índices de las páginas efectivas de texto para cada capítulo en cada PND
# En este caso el PND 2018 está compuesto por 28 pactos más un capítulo de introducción
ind.pages <- list(
"2018"=list("1"=matrix(c( 35,  40), 1, 2, byrow=TRUE),
            "2"=matrix(c( 45,  145), 1, 2, byrow=TRUE),
            "3"=matrix(c( 153, 234), 1, 2, byrow=TRUE),
            "4"=matrix(c( 231, 450), 1, 2, byrow=TRUE),
            "5"=matrix(c(461, 516), 1, 2, byrow=TRUE),
            "6"=matrix(c(523, 569), 1, 2, byrow=TRUE),
            "7"=matrix(c(575, 621), 1, 2, byrow=TRUE),
            "8"=matrix(c(627, 658), 1, 2, byrow=TRUE),
            "9"=matrix(c(663, 691), 1, 2, byrow=TRUE),
            "10"=matrix(c(697, 722), 1, 2, byrow=TRUE),
            "11"=matrix(c(727, 759), 1, 2, byrow=TRUE),
            "12"=matrix(c(765, 818), 1, 2, byrow=TRUE),
            "13"=matrix(c(823, 853,
                          855, 986), 2, 2, byrow=TRUE),
            "14"=matrix(c(991, 1006), 1, 2, byrow=TRUE),
            "15"=matrix(c(1011, 1060), 1, 2, byrow=TRUE),
            "16"=matrix(c(1067, 1085), 1, 2, byrow=TRUE),
            "17"=matrix(c(1089, 1090,
                          1092, 1151), 2, 2, byrow=TRUE),
            "18"=matrix(c(1173, 1183), 1, 2, byrow=TRUE),
            "19"=matrix(c(1187, 1194), 1, 2, byrow=TRUE),
            "20"=matrix(c(1199, 1204), 1, 2, byrow=TRUE),
            "21"=matrix(c(1207, 1212), 1, 2, byrow=TRUE),
            "22"=matrix(c(1217, 1223), 1, 2, byrow=TRUE),
            "23"=matrix(c(1227, 1233), 1, 2, byrow=TRUE),
            "24"=matrix(c(1237, 1243), 1, 2, byrow=TRUE),
            "25"=matrix(c(1247, 1253), 1, 2, byrow=TRUE),
            "26"=matrix(c(1257, 1262), 1, 2, byrow=TRUE),
            "27"=matrix(c(1271, 1285,
                          1287, 1313), 2, 2, byrow=TRUE),
            "28"=matrix(c(1317, 1404,
                          1406, 1419), 2, 2, byrow=TRUE)

))


PreProcDir <- "../../PreProcTexto/" 
source(paste(PreProcDir, "FuncionesBase.R", sep=""))
liSTopwords <- ListaStopwords(listAddi = "listaddi.txt",
                              listExclus = "listaexcl.txt",
                              saveFile = FALSE)
# Lista de stopwords que sí es
liSTopwords <- readLines('C:/Users/cmayorquin/Desktop/PND/PNDTT/Pre-Proc-PNDs/stop_words_spanish.txt')

oldw <- getOption("warn")
options(warn = -1)

for(ian in 1:numAns){#:numAns
  an <- as.character(inicios[ian])
  print(an)
  cont.caps.an <- length(ind.pages[[an]])
  for(icap in 1:cont.caps.an ){#:cont.caps.an
    cap <- as.character(icap)
    print(cap)
    pagsCap <- c()
    nblkscap <- nrow(ind.pages[[an]][[cap]])
    for(iblk in 1:nblkscap){
      pagsCap <- c(pagsCap,
                   ind.pages[[an]][[cap]][iblk,1]:ind.pages[[an]][[cap]][iblk,2])
    }
    #print(pagsCap)
    
    path.pdf <- paste("../PDFs/",an,"/",filers[[an]][icap],sep="")
    #print(path.pdf)
    
    if(aOcriar[ian]){
      img.filer <- pdf_convert(path.pdf,format="tiff",pages=pagsCap,dpi=300)
      text.capt <- tesseract::ocr(img.filer,engine=esp.engine)
      file.remove(img.filer)
    } else {
      text.capt <- pdf_text(path.pdf)
      text.capt <- text.capt[pagsCap]
    }
    text.capt <- text.capt %>% 
      tolower %>%
      #Puntuación, eestá comentada porque se necesitan sacar las frases.
      #gsub("[^[:lower:]^[:space:]]", " ", .) %>%
      gsub("[[:space:]]{1,}", " ", .) %>%
      trimws
    
    words.capt <- length(unlist(strsplit(text.capt, " ")))
    print(words.capt)
    
    save(text.capt, file = paste("../PlainPages/",an,"/Cap",
                                 sprintf("%02d",icap),".RData",sep=""))
    
    cat(text.capt,
        file = paste("../PlainTxts/",an,"/Cap",
                     sprintf("%02d",icap),".txt",sep=""),
        sep = "\n")
    
    text.capt <- RemoveStopwordsFromText(text.capt,liSTopwords)
    words.capt <- length(unlist(strsplit(text.capt, " ")))
    print(words.capt)
    
    save(text.capt, file = paste("../CleanPages/",an,"/Cap",
                                 sprintf("%02d",icap),".RData",sep=""))
    
    cat(text.capt,
        file = paste("../CleanTxts/",an,"/Cap",
                     sprintf("%02d",icap),".txt",sep=""),
        sep = "\n")
    
  }
}

options(warn = oldw)

