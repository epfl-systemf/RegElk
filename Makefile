.PHONY: all viewpdf pdf clean

TARGET       = main
SOURCE_FILES = $(TARGET).tex $(wildcard */*.tex) 
BIB_FILES    = $(wildcard *.bib)
FIGURES      = $(wildcard */figures/*)

# Set the pdf reader according to the operating system
OS = $(shell uname)
ifeq ($(OS), Darwin)
	PDF_READER = open
endif
ifeq ($(OS), Linux)
	PDF_READER = evince
endif

all: pdf

viewpdf: pdf
	$(PDF_READER) $(TARGET).pdf &

pdf: $(TARGET).pdf

$(TARGET).pdf: $(SOURCE_FILES) $(BIB_FILES) $(FIGURES)
	pdflatex -jobname=$(TARGET) $(SOURCE_FILES)
	bibtex $(TARGET)
	pdflatex -jobname=$(TARGET) $(SOURCE_FILES) # For biber
	pdflatex -jobname=$(TARGET) $(SOURCE_FILES) # For biber

clean:
	rm -f $(TARGET).{ps,pdf,bcf,run.xml,los,lof}
	for suffix in dvi aux bbl blg toc ind out brf ilg idx synctex.gz log bcf run.xml; do \
		find . -type d -name ".git" -prune -o -type f -name "*.$${suffix}" -print -exec rm {} \;  ; \
	done
