#	@(#)Makefile	6.4 (Berkeley) 5/6/91

PROG=		cc1plus
BINDIR=		/usr/libexec
SRCS=		$(CINSN) caller-save.c case.c combine.c cplus-class.c \
		cplus-cvt.c cplus-decl.c cplus-decl2.c cplus-except.c \
		cplus-expr.c cplus-field.c cplus-init.c cplus-lex.c \
		cplus-method.c cplus-parse.y cplus-ptree.c \
		cplus-search.c cplus-tree.c cplus-type2.c cplus-typeck.c \
		cse.c dbxout.c emit-rtl.c explow.c expmed.c expr.c \
		final.c flow.c fold-const.c global-alloc.c integrate.c \
		jump.c local-alloc.c loop.c obstack.c optabs.c \
		print-tree.c recog.c regclass.c reload.c reload1.c \
		rtl.c rtlanal.c sdbout.c stmt.c stor-layout.c \
		stupid.c toplev.c tree.c varasm.c version.c
CFLAGS+=	-I- -I. -I$(.CURDIR) -I$(.CURDIR)/../../gcc/cc1 \
		-I$(.CURDIR)/../../gcc/cc1/config
NOMAN=		noman
CLEANFILES+=	$(HINSN) $(CINSN) cplus-tab.h
.PATH:		$(.CURDIR)/../../gcc/cc1

#
# Some machine-dependent source files are generated from
# the machine description file.
#
HINSN=	insn-codes.h insn-config.h insn-flags.h
CINSN=	insn-emit.c insn-extract.c insn-peep.c insn-recog.c insn-output.c
GEN=	gencodes.c genconfig.c genflags.c genemit.c genextract.c \
	genpeep.c genrecog.c genoutput.c

$(CINSN):	md rtl.o obstack.o $(GEN)
	$(CC) $(CFLAGS) $(.CURDIR)/../../gcc/cc1/gencodes.c \
			rtl.o obstack.o -o gen; ./gen md > insn-codes.h
	$(CC) $(CFLAGS) $(.CURDIR)/../../gcc/cc1/genconfig.c \
			rtl.o obstack.o -o gen; ./gen md > insn-config.h
	$(CC) $(CFLAGS) $(.CURDIR)/../../gcc/cc1/genflags.c \
			rtl.o obstack.o -o gen; ./gen md > insn-flags.h
	$(CC) $(CFLAGS) $(.CURDIR)/../../gcc/cc1/genemit.c \
			rtl.o obstack.o -o gen; ./gen md > insn-emit.c
	$(CC) $(CFLAGS) $(.CURDIR)/../../gcc/cc1/genextract.c \
			rtl.o obstack.o -o gen; ./gen md > insn-extract.c
	$(CC) $(CFLAGS) $(.CURDIR)/../../gcc/cc1/genpeep.c \
			rtl.o obstack.o -o gen; ./gen md > insn-peep.c
	$(CC) $(CFLAGS) $(.CURDIR)/../../gcc/cc1/genrecog.c \
			rtl.o obstack.o -o gen; ./gen md > insn-recog.c
	$(CC) $(CFLAGS) $(.CURDIR)/../../gcc/cc1/genoutput.c \
			rtl.o obstack.o -o gen; ./gen md > insn-output.c
	rm -f gen

cplus-tab.h:	cplus-parse.o
	mv y.tab.h cplus-tab.h
	touch cplus-tab.h

cplus-lex.o:	cplus-tab.h

depend:

.include <bsd.prog.mk>
.include "../Makefile.symlinks"
