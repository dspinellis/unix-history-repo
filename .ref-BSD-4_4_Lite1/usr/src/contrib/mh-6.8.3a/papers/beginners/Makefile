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
# beginners
#############################################################

i-all:		beginners.dvi

i-print:	beginners.dvi true
		dvisp $(DFLAGS) -J Beginners beginners

true:;


##############################################################################
#	Miscellaneous tasks
##############################################################################

distribution:   clean
		rm -f beginners.dvi beginners.imp

clean:;		rm -f beginners.log beginners.blg beginners.tmp _*
