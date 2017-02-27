all: paper.pdf paper.aux

PDFLATEX=pdflatex -shell-escape

%.aux: %.tex
	$(PDFLATEX) ${basename $<}

%.bbl: %.aux
	bibtex ${basename $<}

%.pdf: %.bbl
	$(PDFLATEX) ${basename $<}
	$(PDFLATEX) ${basename $<}

arxiv: arxiv.zip

arxiv.zip: full paper.bbl
	zip $@ anglican-color.tex case-study-bbvb.tex code-explosion.tex different-customers-times.pdf expression-kinds.tex number-of-customers.pdf paper.tex same-customer-time.pdf toggle.tex sigplanconf.cls paper.bbl

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
