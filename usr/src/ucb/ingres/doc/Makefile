NROFF=	nroff
TROFF=	troff
NMACS=	../naa
TMACS=	../taa

.SILENT:

ref:
	$(NROFF) naa 0aa.nr 1aa.nr toc.nr
	chdir quel; $(NROFF) $(NMACS) *.nr
	chdir unix; $(NROFF) $(NMACS) *.nr
	chdir files; $(NROFF) $(NMACS) *.nr
	chdir error; $(NROFF) $(NMACS) *.nr

tref:
	$(TROFF) taa 0aa.nr 1aa.nr toc.nr
	chdir quel; $(TROFF) $(TMACS) *.nr
	chdir unix; $(TROFF) $(TMACS) *.nr
	chdir files; $(TROFF) $(TMACS) *.nr
	chdir error; $(TROFF) $(TMACS) *.nr
