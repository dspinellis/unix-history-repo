#	From: @(#)Makefile.inc	5.1 (Berkeley) 2/18/91
#	$Id: Makefile.inc,v 1.3 1994/04/06 16:48:32 rich Exp $

# locale sources
.PATH: ${.CURDIR}/${MACHINE}/locale ${.CURDIR}/locale

SRCS+=	ansi.c frune.c mbrune.c lconv.c localeconv.c rune.c setlocale.c \
	table.c isctype.c \
	euc.c none.c utf2.c
MAN3+=	locale/mbrune.3 locale/multibyte.3 locale/rune.3 locale/setlocale.3
MAN4+=	locale/euc.4 locale/utf2.4

MLINKS+=	utf2.4 utf8.4
