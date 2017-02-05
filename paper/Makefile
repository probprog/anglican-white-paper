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
	toggle.tex \
	anglican-color.tex \
	anglican.tex \
	refs.bib

full: 
	cp toggle-full.tex toggle.tex

ifl:
	cp toggle-ifl.tex toggle.tex

toggle.tex: toggle-ifl.tex
	cp $< $@

clean:
	rm -f *.aux *.log *.bbl *.blg 

distclean: clean
	rm -f paper.pdf
