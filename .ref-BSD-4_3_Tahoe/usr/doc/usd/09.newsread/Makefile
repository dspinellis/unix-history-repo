#	@(#)Makefile	6.1 (Berkeley) 5/26/86
#
# make the various documents

FMT=ditroff
PRINTER=-Pdp
FMTOPTS=
LPR=

howto: howto.mn tmac.n
	$(FMT) $(PRINTER) $(FMTOPTS) tmac.n howto.mn
howto.aps: howto.mn tmac.n
	$(FMT) $(PRINTER) $(FMTOPTS) tmac.n howto.mn | $(LPR)

copyright: copyright.mn tmac.n
	$(FMT) $(PRINTER) $(FMTOPTS) tmac.n copyright.mn
