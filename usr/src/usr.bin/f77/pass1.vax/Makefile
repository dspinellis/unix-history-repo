#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
CFLAGS	      = -O -DHERE=VAX -DTARGET=VAX -DFAMILY=PCC -DUCBVAXASM -DUCBPASS2 \

		-I../../include

DEST	      = $(DESTDIR)/usr/lib

EXTHDRS	      = ../../include/defines.h \
		../../include/machdefs.h \
		/usr/include/a.out.h \
		/usr/include/ctype.h \
		/usr/include/signal.h \
		/usr/include/stab.h \
		/usr/include/stdio.h \
		/usr/include/sys/file.h \
		/usr/include/sys/types.h

HDRS	      = conv.h \
		data.h \
		defs.h \
		fio.h \
		ftypes.h \
		io.h \
		optim.h \
		tokdefs.h

LDFLAGS	      =

LIBS	      =

LINKER	      = cc

MAKEFILE      = Makefile

OBJS	      = bb.o \
		conv.o \
		data.o \
		equiv.o \
		error.o \
		exec.o \
		expr.o \
		fmt.o \
		gram.o \
		init.o \
		intr.o \
		io.o \
		lex.o \
		main.o \
		misc.o \
		optcse.o \
		optim.o \
		optloop.o \
		paramset.o \
		proc.o \
		put.o \
		putpcc.o \
		regalloc.o \
		setfmt.o \
		stab.o \
		vax.o \
		vaxx.o

PRINT	      = pr

PROGRAM	      = f77pass1

SRCS	      = bb.c \
		conv.c \
		data.c \
		equiv.c \
		error.c \
		exec.c \
		expr.c \
		fmt.c \
		gram.c \
		init.c \
		intr.c \
		io.c \
		lex.c \
		main.c \
		misc.c \
		optcse.c \
		optim.c \
		optloop.c \
		paramset.c \
		proc.c \
		put.c \
		putpcc.c \
		regalloc.c \
		setfmt.c \
		stab.c \
		vax.c \
		vaxx.c

all:		$(PROGRAM)

$(PROGRAM):     $(OBJS) $(LIBS)
		@echo -n "Loading $(PROGRAM) ... "
		@$(LINKER) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
		@echo "done"

clean:;		@rm -f $(OBJS) gram.c gram.in

depend:;	@mkmf -f $(MAKEFILE) PROGRAM=$(PROGRAM) DEST=$(DEST)

index:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(PROGRAM)
		@echo Installing $(PROGRAM) in $(DEST)
		@install -s $(PROGRAM) $(DEST)

lint:;		lint -h $(CFLAGS) $(SRCS)

print:;		@$(PRINT) $(HDRS) $(SRCS)

program:        $(PROGRAM)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:		$(DEST)/$(PROGRAM)

$(DEST)/$(PROGRAM): $(SRCS) $(LIBS) $(HDRS) $(EXTHDRS)
		@make -f $(MAKEFILE) DEST=$(DEST) install

gram.c:	gram.head gram.dcl gram.expr gram.exec gram.io tokdefs.h
	( sed <tokdefs.h "s/#define/%token/" ;\
		cat gram.head gram.dcl gram.expr gram.exec gram.io ) >gram.in
	$(YACC) $(YFLAGS) gram.in
	@echo "(expect 4 shift/reduce)"
	mv y.tab.c gram.c

gram.head: defs.h data.h

tokdefs.h: tokens
	grep -n . <tokens | sed "s/\([^:]*\):\(.*\)/#define \2 \1/" >tokdefs.h
###
bb.o: /usr/include/stdio.h /usr/include/ctype.h ftypes.h \
	../../include/defines.h ../../include/machdefs.h defs.h optim.h
conv.o: defs.h ../../include/machdefs.h conv.h
data.o: defs.h ../../include/machdefs.h data.h
equiv.o: defs.h ../../include/machdefs.h /usr/include/a.out.h \
	/usr/include/stab.h
error.o: defs.h ../../include/machdefs.h
exec.o: defs.h ../../include/machdefs.h optim.h
expr.o: defs.h ../../include/machdefs.h
fmt.o: defs.h format.h ../../include/machdefs.h
gram.o: defs.h ../../include/machdefs.h data.h /usr/include/a.out.h \
	/usr/include/stab.h
init.o: defs.h ../../include/machdefs.h io.h /usr/include/sys/file.h
intr.o: defs.h ../../include/machdefs.h
io.o: defs.h ../../include/machdefs.h io.h
lex.o: defs.h ../../include/machdefs.h tokdefs.h
main.o: defs.h ../../include/machdefs.h /usr/include/signal.h \
	/usr/include/a.out.h /usr/include/stab.h
misc.o: defs.h ../../include/machdefs.h
optcse.o: defs.h ../../include/machdefs.h optim.h
optim.o: defs.h ../../include/machdefs.h optim.h
optloop.o: defs.h ../../include/machdefs.h optim.h
paramset.o: defs.h ../../include/machdefs.h data.h
proc.o: defs.h ../../include/machdefs.h /usr/include/a.out.h \
	/usr/include/stab.h
put.o: defs.h ../../include/machdefs.h /usr/include/pcc.h
putpcc.o: defs.h ../../include/machdefs.h /usr/include/pcc.h
regalloc.o: defs.h ../../include/machdefs.h optim.h
setfmt.o: defs.h format.h ../../include/machdefs.h
stab.o: defs.h ../../include/machdefs.h /usr/include/sys/types.h \
	/usr/include/a.out.h /usr/include/stab.h
vax.o: defs.h ../../include/machdefs.h /usr/include/a.out.h \
	/usr/include/stab.h /usr/include/pcc.h
vaxx.o: /usr/include/stdio.h ../../include/defines.h \
	../../include/machdefs.h
