#########################################################################
# Instructions to Make, for generation of nice-looking papers using LaTeX
#########################################################################

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
# mznet
#############################################################

i-all:		mznet.dvi

i-print:	mznet.dvi true
		dvisp $(DFLAGS) -J MZnet mznet


# customization files, et. al., not included below
mznet.dvi:	mznet.tex figure1.tex figure2.tex


##############################################################################
#	Miscellaneous tasks
##############################################################################

distribution:   clean
		rm -f mznet.dvi mznet.imp

clean:;		rm -f mznet.log _*

true:;
