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
# tutorial
#############################################################

i-all:		tutorial.dvi

i-print:	tutorial.dvi true
		dvisp -J Tutorial tutorial.dvi


# customization files, et. al., not included below
tutorial.dvi:	version.tex

version.tex:	tutorial.tex text.tex refs.tex version.sh
		@: version.sh

true:;




##############################################################################
#	Miscellaneous tasks
##############################################################################

distribution:   clean
		rm -f tutorial.dvi tutorial.imp

clean:;		rm -f tutorial.lof tutorial.log tutorial.lot tutorial.toc _*
