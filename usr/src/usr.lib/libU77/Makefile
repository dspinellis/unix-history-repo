# Makefile for f77 system lib, libU77.a
#	@(#)makefile	1.7.1	2/10/83

LIB = libU77.a
LPR = lpr

# for 11's
#ENV = ucb
#LIBDIR = /usr/$(ENV)/lib
#TAPE = /dev/nrmt1

# for Vax
LIBDIR = /usr/lib
TAPE = /dev/rnmt0.16

CFLAGS = -O

SOURCES = \
	access_.c \
	alarm_.c \
	chdir_.c \
	ctime_.c \
	dtime_.c \
	etime_.c \
	fdate_.c \
	fgetc_.c \
	fork_.c \
	fputc_.c \
	fseek_.c \
	fstat_.c \
	ftell_.c \
	gerror_.c \
	getarg_.c \
	getc_.c \
	getcwd_.c \
	getenv_.c \
	getgid_.c \
	getlog_.c \
	getpid_.c \
	getuid_.c \
	gmtime_.c \
	iargc_.c \
	idate_.c \
	ierrno_.c \
	isatty_.c \
	itime_.c \
	kill_.c \
	loc_.c \
	link_.c \
	ltime_.c \
	perror_.c \
	putc_.c \
	qsort_.c \
	signal_.c \
	sleep_.c \
	stat_.c \
	system_.c \
	time_.c \
	ttynam_.c \
	unlink_.c \
	wait_.c

OBJECTS = \
	access_.o \
	alarm_.o \
	chdir_.o \
	ctime_.o \
	dtime_.o \
	etime_.o \
	fdate_.o \
	fgetc_.o \
	fork_.o \
	fputc_.o \
	fseek_.o \
	fstat_.o \
	ftell_.o \
	gerror_.o \
	getarg_.o \
	getc_.o \
	getcwd_.o \
	getenv_.o \
	getgid_.o \
	getlog_.o \
	getpid_.o \
	getuid_.o \
	gmtime_.o \
	iargc_.o \
	idate_.o \
	ierrno_.o \
	isatty_.o \
	itime_.o \
	kill_.o \
	loc_.o \
	ltime_.o \
	link_.o \
	perror_.o \
	putc_.o \
	qsort_.o \
	signal_.o \
	sleep_.o \
	stat_.o \
	system_.o \
	time_.o \
	ttynam_.o \
	unlink_.o \
	wait_.o

$(LIB):	$(OBJECTS) Version
	@echo Loading...
	@rm -f $(LIB)
	@ar rc $(LIB) $(OBJECTS) Version
	@chmod 664 $(LIB)
	@echo "Done..."

install:	$(LIB)
	install -m 644 $(LIB) $(LIBDIR)/$(LIB)
	ranlib $(LIBDIR)/$(LIB)


#compile and strip unnecessary symbols
.c.o:
	cc $(CFLAGS) -c $*.c
	-ld -r -x $*.o
	mv a.out $*.o

Version:	$(SOURCES) mkvers
	@rm -f Version.c
	mkvers $(LIB) $(SOURCES) > Version.c
	cc -c Version.c
	mv Version.o Version

mkvers:	mkvers.c
	@cc mkvers.c -o mkvers

print:	index
	@pr index Makefile `ls $(SOURCES)` | $(LPR)

index:	$(SOURCES) mkindx
	@mkindx "f77 $(LIB) Source Listing " Makefile $(SOURCES)

mkindx:	mkindx.c
	@cc mkindx.c -o mkindx

clean:
	rm -f $(OBJECTS) $(LIB)

# End of Makefile
