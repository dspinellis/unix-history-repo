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
# realwork
#############################################################

i-all:		realwork.dvi

i-print:	realwork.dvi true
		dvisp $(DFLAGS) -J RealWork realwork.dvi


# customization files, et. al., not included below
realwork.dvi:	version.tex

version.tex:	realwork.tex text.tex appendixA.tex appendixB.tex refs.tex \
		diagram1.tex  diagram2.tex  diagram3.tex  diagram4.tex \
		diagram5.tex  diagram6.tex  diagram7.tex  diagram8.tex \
		diagram10.tex \
		version.sh
		@: version.sh

true:;


##############################################################################
#	Miscellaneous tasks
##############################################################################

distribution:   clean
		rm -f realwork.dvi realwork.imp

clean:;		rm -f realwork.lof realwork.log realwork.lot realwork.toc _*
