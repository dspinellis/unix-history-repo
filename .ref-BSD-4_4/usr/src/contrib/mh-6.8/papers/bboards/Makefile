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
# bboards
#############################################################

i-all:		bboards.dvi

i-print:	bboards.dvi true
		dvisp -J BBoards bboards.dvi

i-count:	true
		detex text.tex | wc -cw


# customization files, et. al., not included below
bboards.dvi:	version.tex

version.tex:	bboards.tex text.tex refs.tex version.sh
		@: version.sh

true:;


##############################################################################
#	Miscellaneous tasks
##############################################################################

distribution:   clean
		rm -f bboards.dvi bboards.imp

clean:;		rm -f bboards.lof bboards.log bboards.lot bboards.toc _*
