troff=itroff

what:
		@echo "what do you want to make?"

Cover:
		$(troff) cover
Distpack:	distpack
		$(troff) distpack
Icon-pi:	icon-pi.1
		$(troff) -man icon-pi.1

Icont:		icont.1
		$(troff) -man icont.1

Porting:	porting
		$(troff) porting

Reportform:	reportform
		$(troff) reportform

Tr83-10:	tr83-10
		$(troff) tr83-10

Tr83-3:		tr83-3
		$(troff) tr83-3

Tr84-10:	tr84-10
		$(troff) tr84-10

Tr84-11:	tr84-11
		tbl tr84-11b.roff >tr84-11b.tbl
		$(troff) tr84-11

Tr84-12:	tr84-12
		$(troff) tr84-12

Tr84-13:	tr84-13
		$(troff) tr84-13

Tr84-14:	tr84-14
		$(troff) tr84-14

Version5.9:	version5.9
		$(troff) version5.9

Dist-clean:
		rm -f .BK* .CK* .emacs*
		rm -rf `gcomp Makefile tr* porting version5.9 distpack \
		   reportform *.1 tmac.tr cover READ.ME`
