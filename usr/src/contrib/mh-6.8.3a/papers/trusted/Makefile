#######################################################################
# Instructions to Make, for generation of nice-looking papers using TeX
#######################################################################

.SUFFIXES:	.pic .web .tex .dvi .imp

.pic.tex:;	tpic $<

.web.tex:;	sh -c 'if weave $<; then exit 0; else rm $@; exit 1; fi'

.tex.dvi:;	sh -c 'if TEXINPUTS=../doclib:.:${TEXINPUTS} tex \
			\\nonstopmode\\input $<\\bye; \
			  then exit 0; else rm $@; exit 1; fi'

.dvi.imp:;	dviimp $(DFLAGS) -i $@ $<


#############################################################
# Here it is...
#############################################################

all:		i-all
print:		i-print


#############################################################
# trusted
#############################################################

i-all:		trusted.dvi

i-print:	trusted.dvi true
		dvisp $(DFLAGS) -J Trusted trusted.dvi


# customization files, et. al., not included below
trusted.dvi:	version.tex

version.tex:	trusted.tex \
		text.tex \
			figure1.tex figure2.tex figure3.tex table4.tex \
		appendixA.tex \
			diagramA1-1.tex diagramA1-2.tex diagramA1-3.tex \
			diagramA1-4.tex \
		appendixB.tex \
			diagramB1-1.tex diagramB1-2.tex \
		appendixC.tex refs.tex \
		version.sh
		@: version.sh

true:;


##############################################################################
#	Miscellaneous tasks
##############################################################################

distribution:   clean
		rm -f trusted.dvi trusted.imp

clean:;		rm -f trusted.lof trusted.log trusted.lot trusted.toc _*
