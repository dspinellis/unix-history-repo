#	From: @(#)Makefile.inc	5.1 (Berkeley) 2/18/91
#	$Id$

# locale sources
.PATH: ${.CURDIR}/${MACHINE}/locale ${.CURDIR}/locale

SRCS+=	ansi.c frune.c mbrune.c lconv.c localeconv.c rune.c setlocale.c \
	table.c isctype.c \
	euc.c none.c utf2.c
MAN3+=	mbrune.3 multibyte.3 rune.3 setlocale.3
MAN4+=	euc.4 utf2.4

