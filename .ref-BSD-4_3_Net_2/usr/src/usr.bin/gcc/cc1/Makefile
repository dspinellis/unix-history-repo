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
	for f in $(HINSN) $(CINSN); do \
		p=`expr $$f : "insn-\(.*\)\.[ch]"`; \
		$(CC) $(CFLAGS) $(.CURDIR)/gen$$p.c rtl.o obstack.o -o gen; \
		./gen md > $$f; \
	done
	rm -f gen

.include <bsd.prog.mk>
.include "../Makefile.symlinks"
