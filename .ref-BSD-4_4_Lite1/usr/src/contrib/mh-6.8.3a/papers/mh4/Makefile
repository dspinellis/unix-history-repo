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
# mh4
#############################################################

i-all:		mh4.dvi

i-print:	mh4.dvi true
		dvisp -V -J MH4 $(DFLAGS) mh4.dvi

i-count:	true
		detex text.tex | wc -cw


# customization files, et. al., not included below
mh4.dvi:	version.tex

version.tex:	mh4.tex text.tex version.sh
		@: version.sh

true:;


##############################################################################
#	Miscellaneous tasks
##############################################################################

distribution:   clean
		rm -f mh4.dvi mh4.imp

clean:;		rm -f CONTENTS.tex FIGURES.tex \
			mh4.lof mh4.log mh4.lot mh4.toc _*
