#######################################################################
# Instructions to Make, for generation of nice-looking papers using TeX
#######################################################################

.SUFFIXES:	.pic .web .tex .dvi .imp

.pic.tex:;	tpic $<

.web.tex:;	sh -c 'if weave $<; then exit 0; else rm $@; exit 1; fi'

.tex.dvi:;	sh -c 'if latex \\nonstopmode\\input $<; \
			  then exit 0; else rm $@; exit 1; fi'

.dvi.imp:;	dviimp $(DFLAGS) -i $@ $<


#############################################################
# Here it is...
#############################################################

all:		i-all
print:		i-print


#############################################################
# mh6
#############################################################

i-all:		mh6.dvi

i-print:	mh6.dvi true
		dvisp -J mh6 mh6.dvi


# customization files, et. al., not included below
mh6.dvi:	version.tex

version.tex:	mh6.tex version.sh
		@: version.sh

true:;




##############################################################################
#	Miscellaneous tasks
##############################################################################

distribution:   clean
		rm -f mh6.dvi mh6.imp

clean:;		rm -f mh6.log _*
