BUILDDIR  = _build
RSCRIPT := cd $(BUILDDIR) && Rscript -e
TEX2PDF  := cd $(BUILDDIR) && pdflatex -interaction=batchmode

all: clean python-and-r

clean:
	rm -rf $(BUILDDIR)/* *.pdf *.png

$(BUILDDIR):
	mkdir -p $@

python-and-r: python-and-r.tex $(BUILDDIR)
	$(RSCRIPT) "library(knitr); knit('../$(<F)', '$(@).tex')"
	($(TEX2PDF) $(@))
	($(TEX2PDF) $(@))
	cp $(BUILDDIR)/$(@).pdf $(@).pdf 
