#	@(#)Makefile	6.1 (Berkeley) 5/26/86
#
# make the various documents

FMT=ditroff
PRINTER=-Pdp
FMTOPTS=
LPR=

manner: manner.mn tmac.n
	$(FMT) $(PRINTER) $(FMTOPTS) tmac.n manner.mn
manner.aps: manner.mn tmac.n
	$(FMT) $(PRINTER) $(FMTOPTS) tmac.n manner.mn | $(LPR)
