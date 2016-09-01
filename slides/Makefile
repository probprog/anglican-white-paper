all: slides.pdf

PDFLATEX=pdflatex -file-line-error -shell-escape

slides.pdf: slides.tex
	$(PDFLATEX) $<

clean:
	rm -f *.snm *.out *.log *.aux *.nav *.toc
