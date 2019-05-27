# PND-Innovation
Pdf reading, OCR and text mining of all PND documents through time for innovation terms.

## Objective
The main objective is to capture the number of times a certain list of words belonging to Innovation context appear in all chapters of the PND 2018-2022. Additional to this, a word cloud was generated and a table where the context of the words in the chapter can be found.

List of objective words:

![image] (https://github.com/FoxHound112263/PND-Innovation/blob/master/Palabras%20innovaci%C3%B3n/img%20for%20repo/objective.png)

## Methodology
* Read the PDFs files with '''pdftools''' and '''tesseract'''.
* Text extracting and cleaning (remove stopwords, numbers, special characters, etc).
* Count words in each chapter.
* Generate list of word frequencies for external word cloud service.
* Create algorithm to find the phrases in which the objective words are present. This gives a sense of context, as the innovation words might not always be used with the desired intentions of this analysis.

## Results

Preview of objective words per chapter:

![image](https://github.com/FoxHound112263/PND-Innovation/blob/master/Palabras%20innovaci%C3%B3n/img%20for%20repo/count.png)
As expected, chapter 5 (Innovation) contains a good chunk of the words. Almost all of the chapters contain at leats one innovation word.

Wordcloud:
![image](https://github.com/FoxHound112263/PND-Innovation/blob/master/Palabras%20innovaci%C3%B3n/nube.png)
