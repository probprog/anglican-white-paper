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

acmzip: acm.zip 
	
acm.zip: ifl paper.pdf paper.bbl paper.aux
	zip $@ acmart.cls anglican-color.tex different-customers-times.pdf paper.tex refs.bib same-customer-time.pdf toggle.tex paper.bbl paper.aux paper.out paper.pdf


clean:
	rm -f *.aux *.log *.bbl *.blg 

distclean: clean
	rm -f paper.pdf
