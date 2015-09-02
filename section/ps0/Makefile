BUILDDIR  = _build
#BINDIR    = ../../bin
#LYX2PDF  := cd $(BUILDDIR) && lyx --export pdf2
TEX2PDF  := cd $(BUILDDIR) && TEXINPUTS="..:" pdflatex -interaction=batchmode
#KNIT     := cd $(BUILDDIR) && $(BINDIR)/knitr

all: clean ps1

clean:
	rm -rf $(BUILDDIR)/* *.pdf

$(BUILDDIR):
	mkdir -p $@

#$(BUILDDIR)/%.pdf: $(BUILDDIR)/%.tex
#	($(TEX2PDF) $(<F) 1>/dev/null)


%: %.tex $(BUILDDIR)
#	($(LYX2PDF) ../$(<F))
	($(TEX2PDF) ../$(<F))
	($(TEX2PDF) ../$(<F))
	cp $(BUILDDIR)/$(@).pdf $(@).pdf 
