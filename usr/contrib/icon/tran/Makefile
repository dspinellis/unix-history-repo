CFLAGS = -O
LDFLAGS=

itran:		itran.o parse.o lex.o code.o sym.o mem.o \
		err.o char.o keyword.o optab.o toktab.o lnklist.o
		cc $(LDFLAGS) -o itran *.o

itran.o:	itran.h token.h tree.h sym.h ../h/config.h
parse.o:	itran.h tree.h sym.h ../h/config.h
lex.o:		itran.h token.h lex.h char.h tree.h ../h/config.h
code.o:		itran.h token.h tree.h code.h sym.h ../h/config.h
sym.o:		itran.h token.h sym.h char.h ../h/config.h lfile.h
mem.o:		itran.h sym.h ../h/config.h
err.o:		itran.h token.h tree.h lex.h synerr.h ../h/config.h
char.o:		char.h
keyword.o:	../h/keyword.h sym.h
optab.o:	lex.h
toktab.o:	itran.h lex.h token.h ../h/config.h
lnklist.o:	itran.h lfile.h ../h/config.h

#
#  The following sections are commented out because they do not need
#  to be performed unless changes are made to icon.g, keywords, tokens,
#  or optab.  Such changes involve modifications to the syntax of Icon
#  and are not part of the installation process.  However, if the
#  distribution files are unloaded in a fashion such that their dates
#  are not set properly, the following sections would be attempted.
#
#  Note that if any changes are made to the four files mentioned above,
#  the following comments should be removed.
#
#parse.c	token.h: icon.g
#		yacc -v -d icon.g	# expect 169 shift/reduce conflicts
#		mv y.tab.c parse.c
#		ed parse.c <pscript
#		mv y.tab.h token.h
#
#keyword.h	keyword.c: keywords
#		mkkeytab
#
#toktab.c	optab.c: tokens optab
#		mktoktab
#
#mkkeytab:	mkkeytab.icn
#		icont -o mkkeytab mkkeytab.icn
#
#mktoktab:	mktoktab.icn
#		icont -o mktoktab mktoktab.icn

Listall:
		@pr *.h *.c icon.g
		@date >List

List:		char.h code.h synerr.h lex.h sym.h \
		token.h tree.h itran.h lfile.h \
		char.c code.c err.c keyword.c lex.c lnklist.c mem.c \
		optab.c parse.c sym.c toktab.c itran.c icon.g
		@pr $?
		@date >List
Clean:
		rm -f *.o itran

Dist-clean:
		rm -f .BK* .CK* .emacs*
		rm -f `gcomp Makefile *.c *.h mkkeytab.icn mktoktab.icn \
		tokens optab keywords icon.g pscript *.gen Files`
