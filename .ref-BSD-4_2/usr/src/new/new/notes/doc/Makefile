#	Makefile for the notesfile reference manual
#	March 29, 1982
#	Ray Essick, University of Illinois
#	uiucdcs!essick     uiucdcs!notes
#
#       Modified by Rick Spickelmier, UCB
#       Modified by Lou Salkind, NYU

TROFF = vtroff
FILES = 0 1 2 3 4 5 a.checklist b.future c.update vtoc

doc:	$(FILES) sum.tbl.out
	$(TROFF) -ms $(FILES)

sum.tbl.out: summary
	tbl summary > sum.tbl.out
