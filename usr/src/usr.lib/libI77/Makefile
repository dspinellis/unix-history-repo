# Makefile for f77 I/O lib, libI77.a
#
# @(#)Makefile	1.1 (Berkeley) 5/27/83

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
		close.o \
		dfe.o \
		dofio.o \
		dolio.o \
		douio.o \
		due.o \
		endfile.o \
		err.o \
		f_errlist.o \
		fmt.o \
		fmtlib.o \
		iio.o \
		inquire.o \
		ioinit.o \
		lread.o \
		lwrite.o \
		open.o \
		rdfmt.o \
		rewind.o \
		sfe.o \
		sue.o \
		util.o \
		wrtfmt.o

OLDDIR	      = $(DESTDIR)/usr/old/lib

PRINT	      = pr

SRCS	      = backspace.c \
		close.c \
		dfe.c \
		dofio.c \
		dolio.c \
		douio.c \
		due.c \
		endfile.c \
		err.c \
		f_errlist.c \
		fmt.c \
		fmtlib.c \
		iio.c \
		inquire.c \
		ioinit.f \
		lread.c \
		lwrite.c \
		open.c \
		rdfmt.c \
		rewind.c \
		sfe.c \
		sue.c \
		util.c \
		wrtfmt.c

all:		$(LIBRARY) $(LIBRARY_P)

$(LIBRARY):	$(OBJS) Version
		@echo -n "Loading $(LIBRARY) ... "
		@ar cru $(LIBRARY) $(OBJS) Version ioinit.f
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
		@-mv $(DEST)/$(LIBRARY) $(OLDDIR)/$(LIBRARY)
		install -m 644 $(LIBRARY) $(DEST)/$(LIBRARY)
		ranlib $(DEST)/$(LIBRARY)
		@-mv $(DEST)/$(LIBRARY_P) $(OLDDIR)/$(LIBRARY_P)
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

.f.o:
		$(FC) -p $(FFLAGS) -c $*.f
		-ld -x -r $*.o
		mv a.out profiled/$*.o
		$(FC) $(FFLAGS) -c $*.f
		-ld -x -r $*.o
		mv a.out $*.o

backspace.o:	fio.h backspace.c
dfe.o:		fio.h dfe.c
due.o:		fio.h due.c
iio.o:		fio.h lio.h iio.c
inquire.o:	fio.h inquire.c
rewind.o:	fio.h rewind.c
rdfmt.o:	fio.h format.h rdfmt.c
sue.o:		fio.h sue.c
douio.o:	fio.h douio.c
sfe.o:		fio.h sfe.c
fmt.o:		fio.h format.h fmt.c
dofio.o:	fio.h format.h dofio.c
lwrite.o:	fio.h lio.h lwrite.c
lread.o:	fio.h lio.h lread.c
dolio.o:	fio.h lio.h dolio.c
open.o:		fio.h open.c
close.o:	fio.h close.c
util.o:		fio.h util.c
endfile.o:	fio.h endfile.c
wrtfmt.o:	fio.h format.h wrtfmt.c
err.o:		fiodefs.h err.c
fmtlib.o:	fio.h fmtlib.c
