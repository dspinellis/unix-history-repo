#	@(#)Makefile	6.2 (Berkeley) 2/2/91

PROG=		cc1
BINDIR=		/usr/libexec
SRCS=		$(CINSN) c-convert.c c-decl.c c-parse.y c-typeck.c \
		caller-save.c combine.c cse.c dbxout.c emit-rtl.c \
		explow.c expmed.c expr.c final.c flow.c fold-const.c \
		global-alloc.c integrate.c jump.c local-alloc.c \
		loop.c obstack.c optabs.c print-tree.c recog.c \
		regclass.c reload.c reload1.c rtl.c rtlanal.c \
		sdbout.c stmt.c stor-layout.c stupid.c symout.c \
		toplev.c tree.c varasm.c version.c
CFLAGS+=	-I. -I$(.CURDIR) -I$(.CURDIR)/config
YFLAGS=
NOMAN=		noman
CLEANFILES+=	$(HINSN) $(CINSN)

#
# Some machine-dependent source files are generated from
# the machine description file.
#
HINSN=	insn-codes.h insn-config.h insn-flags.h
CINSN=	insn-emit.c insn-extract.c insn-peep.c insn-recog.c insn-output.c
GEN=	gencodes.c genconfig.c genflags.c genemit.c genextract.c \
	genpeep.c genrecog.c genoutput.c

$(CINSN):	md rtl.o obstack.o $(GEN)
	$(CC) $(CFLAGS) $(.CURDIR)/../../gcc1/cc1/gencodes.c \
			rtl.o obstack.o -o gen; ./gen md > insn-codes.h
	$(CC) $(CFLAGS) $(.CURDIR)/../../gcc1/cc1/genconfig.c \
			rtl.o obstack.o -o gen; ./gen md > insn-config.h
	$(CC) $(CFLAGS) $(.CURDIR)/../../gcc1/cc1/genflags.c \
			rtl.o obstack.o -o gen; ./gen md > insn-flags.h
	$(CC) $(CFLAGS) $(.CURDIR)/../../gcc1/cc1/genemit.c \
			rtl.o obstack.o -o gen; ./gen md > insn-emit.c
	$(CC) $(CFLAGS) $(.CURDIR)/../../gcc1/cc1/genextract.c \
			rtl.o obstack.o -o gen; ./gen md > insn-extract.c
	$(CC) $(CFLAGS) $(.CURDIR)/../../gcc1/cc1/genpeep.c \
			rtl.o obstack.o -o gen; ./gen md > insn-peep.c
	$(CC) $(CFLAGS) $(.CURDIR)/../../gcc1/cc1/genrecog.c \
			rtl.o obstack.o -o gen; ./gen md > insn-recog.c
	$(CC) $(CFLAGS) $(.CURDIR)/../../gcc1/cc1/genoutput.c \
			rtl.o obstack.o -o gen; ./gen md > insn-output.c
	rm -f gen

.include <bsd.prog.mk>
.include "../Makefile.symlinks"
