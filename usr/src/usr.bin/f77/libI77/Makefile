#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#

# Makefile for f77 I/O lib, libI77.a

CFLAGS	      = -O

DEST	      = $(DESTDIR)/usr/lib

DESTDIR	      =

EXTHDRS	      =

FFLAGS	      = -O

HDRS	      = f_errno.h \
		fio.h \
		fiodefs.h \
		format.h \
		lio.h

LIBRARY	      = libI77.a

LIBRARY_P     = libI77_p.a

MAKEFILE      = Makefile

OBJS	      = backspace.o \
		c_dfe.o \
		c_iio.o \
		c_sfe.o \
		close.o \
		dofio.o \
		dolio.o \
		douio.o \
		due.o \
		endfile.o \
		err.o \
		f_errlist.o \
		fmt.o \
		fmtlib.o \
		inquire.o \
		lread.o \
		lwrite.o \
		open.o \
		rdfe.o \
		rdfmt.o \
		rewind.o \
		rfi.o \
		rsfe.o \
		rsli.o \
		sue.o \
		util.o \
		wdfe.o \
		wfi.o \
		wrtfmt.o \
		wsfe.o \
		wsli.o

OLDDIR	      = $(DESTDIR)/usr/old/lib

PRINT	      = pr

SRCS	      = backspace.c \
		c_dfe.c \
		c_iio.c \
		c_sfe.c \
		close.c \
		dofio.c \
		dolio.c \
		douio.c \
		due.c \
		endfile.c \
		err.c \
		f_errlist.c \
		fmt.c \
		fmtlib.c \
		inquire.c \
		lread.c \
		lwrite.c \
		open.c \
		rdfe.c \
		rdfmt.c \
		rewind.c \
		rfi.c \
		rsfe.c \
		rsli.c \
		sue.c \
		util.c \
		wdfe.c \
		wfi.c \
		wrtfmt.c \
		wsfe.c \
		wsli.c

all:		$(LIBRARY) $(LIBRARY_P) libI66.o

$(LIBRARY):	$(OBJS) Version
		@echo -n "Loading $(LIBRARY) ... "
		@ar cru $(LIBRARY) $(OBJS) Version
		@ranlib $(LIBRARY)
		@echo "done"

$(LIBRARY_P):	$(OBJS) Version
		@echo -n "Loading $(LIBRARY_P) ... "
		@cd profiled; ar cru ../$(LIBRARY_P) $(OBJS) Version
		@ranlib $(LIBRARY_P)
		@echo "done"

Version:	$(SRCS) mkvers
		@rm -f Version.c
		./mkvers $(LIBRARY) $(SRCS) > Version.c
		$(CC) -c Version.c
		mv Version.o Version
		@rm -f profiled/Version
		ln Version profiled/Version

mkvers:		mkvers.c
		@$(CC) mkvers.c -o mkvers

clean:;		@rm -f $(OBJS) profiled/*.o $(LIBRARY) $(LIBRARY_P)

depend:;	@echo Construct dependencies manually

extract:;	@ar xo $(DEST)/$(LIBRARY); rm -f __.SYMDEF
		@cd profiled; -ar xo $(DEST)/$(LIBRARY_P); rm -f __.SYMDEF

index:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(LIBRARY) $(LIBRARY_P) libI66.o
		install -m 644 $(LIBRARY) $(DEST)/$(LIBRARY)
		ranlib $(DEST)/$(LIBRARY)
		install -m 644 $(LIBRARY_P) $(DEST)/$(LIBRARY_P)
		ranlib $(DEST)/$(LIBRARY_P)
		install -m 644 -c libI66.o $(DEST)/libI66.a

library:        $(LIBRARY) $(LIBRARY_P)

print:;		@$(PRINT) $(HDRS) $(SRCS)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)

update:         $(DEST)/$(LIBRARY)

$(DEST)/$(LIBRARY): $(SRCS) $(HDRS) $(EXTHDRS)
		@-ar xo $(DEST)/$(LIBRARY)
		@cd profiled; -ar xo $(DEST)/$(LIBRARY_P)
		@make -f $(MAKEFILE) DEST=$(DEST) install clean

f_errlist.o:	f_errlist.c
		$(CC) $(CFLAGS) -c f_errlist.c
		@rm -f profiled/f_errlist.o
		ln f_errlist.o profiled/f_errlist.o
.c.o:
		$(CC) -p $(CFLAGS) -c $*.c
		-ld -x -r $*.o
		mv a.out profiled/$*.o
		$(CC) $(CFLAGS) -c $*.c
		-ld -x -r $*.o
		mv a.out $*.o

backspace.o:	fio.h f_errno.h fiodefs.h backspace.c
c_dfe.o:	fio.h f_errno.h fiodefs.h c_dfe.c
rdfe.o:		fio.h f_errno.h fiodefs.h rdfe.c
wdfe.o:		fio.h f_errno.h fiodefs.h wdfe.c
due.o:		fio.h f_errno.h fiodefs.h due.c
rfi.o:		fio.h f_errno.h fiodefs.h rfi.c
wfi.o:		fio.h f_errno.h fiodefs.h wfi.c
rsli.o:		fio.h f_errno.h fiodefs.h lio.h rsli.c
wsli.o:		fio.h f_errno.h fiodefs.h lio.h wsli.c
c_iio.o:	fio.h f_errno.h fiodefs.h lio.h c_iio.c
inquire.o:	fio.h f_errno.h fiodefs.h inquire.c
rewind.o:	fio.h f_errno.h fiodefs.h rewind.c
rdfmt.o:	fio.h f_errno.h fiodefs.h format.h rdfmt.c
sue.o:		fio.h f_errno.h fiodefs.h sue.c
douio.o:	fio.h f_errno.h fiodefs.h douio.c
rsfe.o:		fio.h f_errno.h fiodefs.h rsfe.c
wsfe.o:		fio.h f_errno.h fiodefs.h wsfe.c
c_sfe.o:	fio.h f_errno.h fiodefs.h c_sfe.c
fmt.o:		fio.h f_errno.h fiodefs.h format.h fmt.c
dofio.o:	fio.h f_errno.h fiodefs.h format.h dofio.c
lwrite.o:	fio.h f_errno.h fiodefs.h lio.h lwrite.c
lread.o:	fio.h f_errno.h fiodefs.h lio.h lread.c
dolio.o:	fio.h f_errno.h fiodefs.h lio.h dolio.c
open.o:		fio.h f_errno.h fiodefs.h open.c
close.o:	fio.h f_errno.h fiodefs.h close.c
util.o:		fio.h f_errno.h fiodefs.h util.c
endfile.o:	fio.h f_errno.h fiodefs.h endfile.c
wrtfmt.o:	fio.h f_errno.h fiodefs.h format.h wrtfmt.c
err.o:		fio.h f_errno.h fiodefs.h err.c
fmtlib.o:	fio.h f_errno.h fiodefs.h fmtlib.c
