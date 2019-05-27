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

#### 
remove(list=ls())
require(pdftools)
require(tesseract)
esp.engine <- tesseract("spa")
require(magrittr)


inicios <- c( 1970, 1974, 1978, 1982, 1986, 1990,  1994,  1998,  2002,  2006,  2010,  2014)
porTomo <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE,  TRUE,  TRUE,  TRUE)
aOcriar <- c( TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE, FALSE,  TRUE, FALSE, FALSE, FALSE)
numFils <- c( 3, 4, 6, 4, 7, 12, 10, 7, 1, 2, 2, 2)
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

filers <- vector("list", numAns)
names(filers) <- as.character(inicios)
for(ian in 1:numAns){
  an <- as.character(inicios[ian])
  if(!porTomo[ian]){
    filers[[an]] <- rep(paste("Cap",sprintf("%02d",1:numFils[ian]),".pdf",sep=""),
                        caps.per.file[[an]])
  } else {
    filers[[an]] <- rep(paste("Tomo",sprintf("%01d",1:numFils[ian]),".pdf",sep=""),
                        caps.per.file[[an]])
  }
}

#Índices de las páginas efectivas de texto para cada capítulo en cada PND
ind.pages <- list("1970"=list("1"=matrix(c(  4, 12), 1, 2, byrow=TRUE),
                              "2"=matrix(c(  14, 15,
                                             17, 23), 2, 2, byrow=TRUE),
                              "3"=matrix(c(  3, 9), 1, 2, byrow=TRUE),
                              "4"=matrix(c(  13, 24), 1, 2, byrow=TRUE),
                              "5"=matrix(c(  26, 31), 1, 2, byrow=TRUE),
                              "6"=matrix(c(  34, 43), 1, 2, byrow=TRUE),
                              "7"=matrix(c(  45, 61), 1, 2, byrow=TRUE),
                              "8"=matrix(c(  63, 70), 1, 2, byrow=TRUE),
                              "9"=matrix(c(  74, 84), 1, 2, byrow=TRUE),
                              "10"=matrix(c( 86, 92), 1, 2, byrow=TRUE),
                              "11"=matrix(c( 94, 99), 1, 2, byrow=TRUE),
                              "12"=matrix(c( 101, 112), 1, 2, byrow=TRUE),
                              "13"=matrix(c( 114, 121), 1, 2, byrow=TRUE),
                              "14"=matrix(c( 124, 133), 1, 2, byrow=TRUE),
                              "15"=matrix(c( 135, 142), 1, 2, byrow=TRUE),
                              "16"=matrix(c( 144, 151), 1, 2, byrow=TRUE),
                              "17"=matrix(c( 2, 11,
                                             13, 35), 2, 2, byrow=TRUE),
                              "18"=matrix(c( 37, 54), 1, 2, byrow=TRUE),
                              "19"=matrix(c( 64, 69,
                                             71, 75), 1, 2, byrow=TRUE)
                              ),
                  "1974"=list("1"=matrix(c(  3, 4), 1, 2, byrow=TRUE),
                              "2"=matrix(c(  2, 5), 1, 2, byrow=TRUE),
                              "3"=matrix(c(  6, 9), 1, 2, byrow=TRUE),
                              "4"=matrix(c(  10, 14), 1, 2, byrow=TRUE),
                              "5"=matrix(c(  2, 18), 1, 2, byrow=TRUE),
                              "6"=matrix(c(  19, 32), 1, 2, byrow=TRUE),
                              "7"=matrix(c(  33, 43), 1, 2, byrow=TRUE),
                              "8"=matrix(c(  44, 55), 1, 2, byrow=TRUE),
                              "9"=matrix(c(  2, 15), 1, 2, byrow=TRUE),
                              "10"=matrix(c(  16, 21), 1, 2, byrow=TRUE),
                              "11"=matrix(c(  22, 27), 1, 2, byrow=TRUE),
                              "12"=matrix(c(  28, 36), 1, 2, byrow=TRUE),
                              "13"=matrix(c(  37, 43), 1, 2, byrow=TRUE)
                              ),
                  "1978"=list("1"=matrix(c(  8, 10), 1, 2, byrow=TRUE),
                              "2"=matrix(c( 3, 8), 1, 2, byrow=TRUE),
                              "3"=matrix(c( 10, 14), 1, 2, byrow=TRUE),                              
                              "4"=matrix(c( 16, 30), 1, 2, byrow=TRUE),
                              "5"=matrix(c( 3, 8), 1, 2, byrow=TRUE),
                              "6"=matrix(c( 10, 19), 1, 2, byrow=TRUE),
                              "7"=matrix(c( 3, 28), 1, 2, byrow=TRUE),
                              "8"=matrix(c( 30, 43), 1, 2, byrow=TRUE),
                              "9"=matrix(c( 45, 57,
                                            59, 83), 2, 2, byrow=TRUE),
                              "10"=matrix(c( 85, 105,
                                             107, 118,
                                             120, 123), 3, 2, byrow=TRUE),
                              "11"=matrix(c( 126, 132), 1, 2, byrow=TRUE),
                              "12"=matrix(c( 134, 141), 1, 2, byrow=TRUE),
                              "13"=matrix(c( 143, 150), 1, 2, byrow=TRUE),
                              "14"=matrix(c( 14, 17,
                                             19, 28,
                                             30, 37,
                                             39, 39), 4, 2, byrow=TRUE),
                              "15"=matrix(c( 41, 41,
                                             43, 43,
                                             45, 46,
                                             48, 48,
                                             50, 52,
                                             54, 60,
                                             62, 67,
                                             70, 81,
                                             83, 96,
                                             98, 103), 10, 2, byrow=TRUE),
                              "16"=matrix(c( 2, 3,
                                             6, 6,
                                             8, 9,
                                             13, 13,
                                             15, 15,
                                             17, 19,
                                             21, 22,
                                             24, 32,
                                             35, 40,
                                             44, 49,
                                             51, 61,
                                             64, 73), 12, 2, byrow=TRUE),
                              "17"=matrix(c( 75, 82,
                                             84, 89,
                                             92, 128,
                                             130, 148), 4, 2, byrow=TRUE),
                              "18"=matrix(c( 150, 167,
                                             169, 179,
                                             182, 185,
                                             187, 192,
                                             195, 226), 5, 2, byrow=TRUE),
                              "19"=matrix(c( 228, 231,
                                             233, 241), 2, 2, byrow=TRUE)
                              ),
                  "1982"=list("1"=matrix(c(  3, 9), 1, 2, byrow=TRUE),
                              "2"=matrix(c( 16, 16,
                                            19, 36), 2, 2, byrow=TRUE),
                              "3"=matrix(c(  2, 5,
                                             7, 97), 2, 2, byrow=TRUE),                              
                              "4"=matrix(c(  2, 2,
                                             4, 21), 2, 2, byrow=TRUE),
                              "5"=matrix(c(  2, 3,
                                             6, 43), 2, 2, byrow=TRUE)
                              ),
                  "1986"=list("1"=matrix(c(  7,   14), 1, 2, byrow=TRUE),
                              "2"=matrix(c( 20,  35), 1, 2, byrow=TRUE),
                              "3"=matrix(c(  2,   4,
                                             7,   12,
                                             16,  28,
                                             30,  31,
                                             33,  36,
                                             38, 48,
                                             50, 58,
                                             60, 67,
                                             69, 75,
                                             79, 86), 10, 2, byrow=TRUE),                              
                              "4"=matrix(c(  2, 6,
                                             8, 31,
                                             34, 36,
                                             38, 51,
                                             53, 60,
                                             63, 75), 6, 2, byrow=TRUE),
                              "5"=matrix(c(  2, 4,
                                             6, 16,
                                             22, 44,
                                             46, 60), 4, 2, byrow=TRUE),
                              "6"=matrix(c(  2, 11,
                                             13, 30,
                                             32, 41,
                                             44, 66), 4, 2, byrow=TRUE),
                              "7"=matrix(c(  2, 17), 1, 2, byrow=TRUE),
                              "8"=matrix(c(  2, 10), 1, 2, byrow=TRUE)
                              ),
                  "1990"=list("1"=matrix(c(  5,   9), 1, 2, byrow=TRUE),
                              "2"=matrix(c( 10,  16), 1, 2, byrow=TRUE),
                              "3"=matrix(c(  2,  19), 1, 2, byrow=TRUE),
                              "4"=matrix(c(  2,  11), 1, 2, byrow=TRUE),
                              "5"=matrix(c(  2,   4,
                                             6,   6,
                                             8,  10,
                                            12,  21,
                                            23,  30), 5, 2, byrow=TRUE),
                              "6"=matrix(c(  1,   5,
                                            10,  10,
                                            12,  14,
                                            18,  42), 4, 2, byrow=TRUE),
                              "7"=matrix(c(  1,  18,
                                            20,  25,
                                            28,  46,
                                            49,  80,
                                            82,  83,
                                            85,  94), 6, 2, byrow=TRUE),
                              "8"=matrix(c(  1,   6,
                                             8,  23,
                                            25,  25,
                                            28,  29,
                                            33,  49), 5, 2, byrow=TRUE),
                              "9"=matrix(c(  1,  52), 1, 2, byrow=TRUE),
                             "10"=matrix(c(  2,  30), 1, 2, byrow=TRUE),
                             "11"=matrix(c(  2,   6,
                                             8,  15,
                                            17,  22,
                                            24,  31), 4, 2, byrow=TRUE),
                             "12"=matrix(c(  2,  14,
                                            16,  21), 2, 2, byrow=TRUE),
                             "13"=matrix(c(  2,  33), 1, 2, byrow=TRUE)
                              ),
                  "1994"=list("1"=matrix(c(  8,  14), 1, 2, byrow=TRUE),
                              "2"=matrix(c( 16,  27), 1, 2, byrow=TRUE),
                              "3"=matrix(c( 29,  34), 1, 2, byrow=TRUE),
                              "4"=matrix(c(  2,  25), 1, 2, byrow=TRUE),
                              "5"=matrix(c(  2,  10), 1, 2, byrow=TRUE),
                              "6"=matrix(c(  2,  14), 1, 2, byrow=TRUE),
                              "7"=matrix(c(  2,   6,
                                             8,  18,
                                            20,  61), 3, 2, byrow=TRUE),
                              "8"=matrix(c(  2,  28,
                                            30,  30,
                                            33,  45), 3, 2, byrow=TRUE),
                              "9"=matrix(c(  2,  12), 1, 2, byrow=TRUE),
                             "10"=matrix(c(  2,   8), 1, 2, byrow=TRUE),
                             "11"=matrix(c(  2,  16), 1, 2, byrow=TRUE),
                             "12"=matrix(c(  2,   9), 1, 2, byrow=TRUE)
                              ),
                  "1998"=list("1"=matrix(c( 13,  15), 1, 2, byrow=TRUE),
                              "2"=matrix(c( 17,  78,
                                           101, 146), 2, 2, byrow=TRUE),
                              "3"=matrix(c(  2,  41), 1, 2, byrow=TRUE),
                              "4"=matrix(c(  2,  28,
                                            30, 111), 2, 2, byrow=TRUE),
                              "5"=matrix(c(  2,  27,
                                            29,  83,
                                            85, 116,
                                           118, 124), 4, 2, byrow=TRUE),
                              "6"=matrix(c(  2,  89), 1, 2, byrow=TRUE),
                              "7"=matrix(c(  2,   4,
                                             5,  44,
                                            46,  48), 3, 2, byrow=TRUE),
                              "8"=matrix(c(  2,  16), 1, 2, byrow=TRUE)
                              ),
                  "2002"=list("1"=matrix(c( 15,  23), 1, 2, byrow=TRUE),
                              "2"=matrix(c( 25,  31,
                                            33,  93), 2, 2, byrow=TRUE),
                              "3"=matrix(c( 95, 154), 1, 2, byrow=TRUE),
                              "4"=matrix(c(156, 179,
                                           181, 249), 2, 2, byrow=TRUE),
                              "5"=matrix(c(251, 269), 1, 2, byrow=TRUE),
                              "6"=matrix(c(271, 271,
                                           273, 273,
                                           275, 275,
                                           277, 281), 4, 2, byrow=TRUE)
                              ),
                  "2006"=list("1"=matrix(c( 17,  35), 1, 2, byrow=TRUE),
                              "2"=matrix(c( 39, 103), 1, 2, byrow=TRUE),
                              "3"=matrix(c(107, 123,
                                           125, 179,
                                           181, 225), 3, 2, byrow=TRUE),
                              "4"=matrix(c(229, 232,
                                           234, 338), 2, 2, byrow=TRUE),
                              "5"=matrix(c(343, 374), 1, 2, byrow=TRUE),
                              "6"=matrix(c(379, 437), 1, 2, byrow=TRUE),
                              "7"=matrix(c( 17,  27,
                                            29, 125), 2, 2, byrow=TRUE)
                              ),
                  "2010"=list("1"=matrix(c( 21,  24), 1, 2, byrow=TRUE),
                              "2"=matrix(c( 27,  30,
                                            37,  59), 2, 2, byrow=TRUE),
                              "3"=matrix(c( 63, 315), 1, 2, byrow=TRUE),
                              "4"=matrix(c(321, 368,
                                           370, 427,
                                           431, 431,
                                           438, 444,
                                           446, 454,
                                           463, 496), 6, 2, byrow=TRUE),
                              "5"=matrix(c(499, 539), 1, 2, byrow=TRUE),
                              "6"=matrix(c( 21,  78), 1, 2, byrow=TRUE),
                              "7"=matrix(c( 81,  83,
                                            85, 174,
                                           176, 207), 3, 2, byrow=TRUE),
                              "8"=matrix(c(211, 226,
                                           228, 247), 2, 2, byrow=TRUE)
                              ),
                  "2014"=list("1"=matrix(c( 25,  33), 1, 2, byrow=TRUE),
                              "2"=matrix(c( 37,  39,
                                            42,  53), 2, 2, byrow=TRUE),
                              "3"=matrix(c( 59,  74), 1, 2, byrow=TRUE),
                              "4"=matrix(c( 79, 104), 1, 2, byrow=TRUE),
                              "5"=matrix(c(111, 257), 1, 2, byrow=TRUE),
                              "6"=matrix(c(269, 277,
                                           279, 411), 2, 2, byrow=TRUE),
                              "7"=matrix(c(419, 447), 1, 2, byrow=TRUE),
                              "8"=matrix(c(451, 545), 1, 2, byrow=TRUE),
                              "9"=matrix(c( 23, 100), 1, 2, byrow=TRUE),
                             "10"=matrix(c(107, 157), 1, 2, byrow=TRUE),
                             "11"=matrix(c(165, 218,
                                           225, 246,
                                           249, 285,
                                           288, 314,
                                           317, 343,
                                           347, 371), 6, 2, byrow=TRUE),
                             "12"=matrix(c(377, 380,
                                           382, 385,
                                           387, 393,
                                           395, 395,
                                           397, 403,
                                           405, 432), 6, 2, byrow=TRUE)
                              ))


PreProcDir <- "../../PreProcTexto/" 
source(paste(PreProcDir, "FuncionesBase.R", sep=""))
liSTopwords <- ListaStopwords(listAddi = "listaddi.txt",
                              listExclus = "listaexcl.txt",
                              saveFile = FALSE)

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
      gsub("[^[:lower:]^[:space:]]", " ", .) %>%
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

