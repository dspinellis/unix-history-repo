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
# multifarious
#############################################################

i-all:		multifarious.dvi

i-print:	multifarious.dvi true
		dvisp $(DFLAGS) -J Multifarious multifarious


# customization files, et. al., not included below
multifarious.dvi:	version.tex

version.tex:	multifarious.tex \
		text.tex \
			figure0.tex figure1.tex diagram2.tex diagram3.tex \
		refs.tex appendixA.tex \
		version.sh
		@: version.sh

true:;


##############################################################################
#	Miscellaneous tasks
##############################################################################

distribution:   clean
		rm -f multifarious.dvi multifarious.imp

clean:;		rm -f multifarious.lof multifarious.log multifarious.lot \
			multifarious.toc _*
