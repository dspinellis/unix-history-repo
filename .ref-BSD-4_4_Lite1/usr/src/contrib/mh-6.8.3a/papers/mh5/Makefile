#######################################################################
# Instructions to Make, for generation of nice-looking papers using TeX
#######################################################################

.SUFFIXES:	.pic .web .tex .dvi

.pic.tex:;	tpic $<

.web.tex:;	sh -c 'if weave $<; then exit 0; else rm $@; exit 1; fi'

.tex.dvi:;	sh -c 'if TEXINPUTS=../doclib:.:${TEXINPUTS} tex \
			\\nonstopmode\\input $<\\bye; \
			  then exit 0; else rm $@; exit 1; fi'


#############################################################
# Here it is...
#############################################################

all:		i-all
print:		i-print


#############################################################
# mh5
#############################################################

i-all:		mh5.dvi

i-print:	mh5.dvi true
		dvisp -J mh5 mh5.dvi

i-count:	true
		detex text.tex | wc -cw


# customization files, et. al., not included below
mh5.dvi:	version.tex

version.tex:	mh5.tex text.tex diagram1.tex diagram2.tex refs.tex version.sh
		@: version.sh

true:;




##############################################################################
#	Miscellaneous tasks
##############################################################################

distribution:   clean
		rm -f mh5.dvi mh5.imp

clean:;		rm -f CONTENTS.tex FIGURES.tex \
			mh5.lof mh5.log mh5.lot mh5.toc _*
