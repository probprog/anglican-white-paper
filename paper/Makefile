all: paper.pdf paper.aux

PDFLATEX=pdflatex -shell-escape

%.aux: %.tex
	$(PDFLATEX) ${basename $<}

%.bbl: %.aux
	bibtex ${basename $<}

%.pdf: %.bbl
	$(PDFLATEX) ${basename $<}
	$(PDFLATEX) ${basename $<}

paper.pdf: paper.tex \
	refs.bib


clean:
	rm -f *.aux *.log *.bbl *.blg 

distclean: clean
	rm -f paper.pdf
