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
# myths
#############################################################

i-all:		myths.dvi

i-print:	myths.dvi true
		dvisp $(DFLAGS) -J Myths myths


# customization files, et. al., not included below
myths.dvi:	version.tex

version.tex:	myths.tex version.sh
		@: version.sh


##############################################################################
#	Miscellaneous tasks
##############################################################################

distribution:   clean
		rm -f myths.dvi myths.imp

clean:;		rm -f myths.log _*

true:;
