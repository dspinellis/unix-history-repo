# Makefile for f77 system lib, libU77.a
#	@(#)makefile	1.17	%G%

DESTDIR =
LIB = libU77.a
LIB_P = libU77_p.a
LPR = lpr

# for 11's
#ENV = ucb
#LIBDIR = ${DESTDIR}/usr/$(ENV)/lib
#OLDDIR = ${DESTDIR}/usr/old/lib
#TAPE = /dev/nrmt1

# for Vax
LIBDIR = ${DESTDIR}/usr/lib
OLDDIR = ${DESTDIR}/usr/old/lib
TAPE = /dev/rnmt0

CFLAGS = -O

SOURCES = \
	access_.c \
	alarm_.c \
	chdir_.c \
	chmod_.c \
	ctime_.c \
	dtime_.c \
	etime_.c \
	f77lid_.c \
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
	hostnm_.c \
	iargc_.c \
	idate_.c \
	ierrno_.c \
	isatty_.c \
	itime_.c \
	kill_.c \
	link_.c \
	loc_.c \
	lstat_.c \
	ltime_.c \
	perror_.c \
	putc_.c \
	qsort_.c \
	rename_.c \
	signal_.c \
	sleep_.c \
	stat_.c \
	symlnk_.c \
	system_.c \
	tapeio.c \
	time_.c \
	ttynam_.c \
	unlink_.c \
	wait_.c

OBJECTS = \
	access_.o \
	alarm_.o \
	chdir_.o \
	chmod_.o \
	ctime_.o \
	dtime_.o \
	etime_.o \
	f77lid_.o \
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
	hostnm_.o \
	iargc_.o \
	idate_.o \
	ierrno_.o \
	isatty_.o \
	itime_.o \
	kill_.o \
	link_.o \
	loc_.o \
	lstat_.o \
	ltime_.o \
	perror_.o \
	putc_.o \
	qsort_.o \
	rename_.o \
	signal_.o \
	sleep_.o \
	stat_.o \
	symlnk_.o \
	system_.o \
	tapeio.o \
	time_.o \
	ttynam_.o \
	unlink_.o \
	wait_.o

all:	$(LIB) $(LIB_P)

$(LIB):	$(OBJECTS) Version
	ar ru $(LIB) $?
	ranlib $(LIB)
	@chmod 644 $(LIB)

$(LIB_P):	$(OBJECTS) Version
	cd profiled; ar ru ../$(LIB_P) $?
	ranlib $(LIB_P)
	@chmod 644 $(LIB_P)

install:	$(LIBDIR)/$(LIB) $(LIBDIR)/$(LIB_P)

$(LIBDIR)/$(LIB):	$(LIB)
	@-mv $(LIBDIR)/$(LIB) $(OLDDIR)/$(LIB)
	tar cf - $(LIB) | (cd $(LIBDIR); tar xvf - )

$(LIBDIR)/$(LIB_P):	$(LIB_P)
	@-mv $(LIBDIR)/$(LIB_P) $(OLDDIR)/$(LIB_P)
	tar cf - $(LIB_P) | (cd $(LIBDIR); tar xvf - )


#compile and strip unnecessary symbols
.c.o:
	-cd profiled; cc $(CFLAGS) -p -c ../$*.c; ld -r -x $*.o; mv a.out $*.o
	cc $(CFLAGS) -c $*.c
	-ld -r -x $*.o
	mv a.out $*.o

Version:	$(SOURCES) mkvers
	@rm -f Version.c
	./mkvers $(LIB) $(SOURCES) > Version.c
	cc -c Version.c
	mv Version.o Version
	@rm -f profiled/Version
	ln Version profiled/Version

mkvers:	mkvers.c
	@cc mkvers.c -o mkvers

print:	index
	@pr index Makefile `ls $(SOURCES)` | $(LPR)

index:	$(SOURCES) mkindx
	@./mkindx "f77 $(LIB) Source Listing " Makefile $(SOURCES)

mkindx:	mkindx.c
	@cc mkindx.c -o mkindx

clean:
	rm -f $(OBJECTS) Version $(LIB) $(LIB_P) profiled/*

# End of Makefile
# Below this is not normally distributed ...

Makefile:	makefile
	@sed -n -e "1,/End of Makefile/p" makefile > Makefile

tape:	Makefile
	-tar cbf 20 $(TAPE) READ_ME Makefile $(SOURCES) mkindx.c mkvers.c


# below here is SCCS stuff ---

access_.c:	SCCS/s.access_.c
	rm -f access_.c
	get SCCS/s.access_.c

alarm_.c:	SCCS/s.alarm_.c
	rm -f alarm_.c
	get SCCS/s.alarm_.c

chdir_.c:	SCCS/s.chdir_.c
	rm -f chdir_.c
	get SCCS/s.chdir_.c

chmod_.c:	SCCS/s.chmod_.c
	rm -f chmod_.c
	get SCCS/s.chmod_.c

ctime_.c:	SCCS/s.ctime_.c
	rm -f ctime_.c
	get SCCS/s.ctime_.c

dtime_.c:	SCCS/s.dtime_.c
	rm -f dtime_.c
	get SCCS/s.dtime_.c

etime_.c:	SCCS/s.etime_.c
	rm -f etime_.c
	get SCCS/s.etime_.c

f77lid_.c:	SCCS/s.f77lid_.c
	rm -f f77lid_.c
	get SCCS/s.f77lid_.c

fdate_.c:	SCCS/s.fdate_.c
	rm -f fdate_.c
	get SCCS/s.fdate_.c

fgetc_.c:	SCCS/s.fgetc_.c
	rm -f fgetc_.c
	get SCCS/s.fgetc_.c

fork_.c:	SCCS/s.fork_.c
	rm -f fork_.c
	get SCCS/s.fork_.c

fputc_.c:	SCCS/s.fputc_.c
	rm -f fputc_.c
	get SCCS/s.fputc_.c

fseek_.c:	SCCS/s.fseek_.c
	rm -f fseek_.c
	get SCCS/s.fseek_.c

fstat_.c:	SCCS/s.fstat_.c
	rm -f fstat_.c
	get SCCS/s.fstat_.c

ftell_.c:	SCCS/s.ftell_.c
	rm -f ftell_.c
	get SCCS/s.ftell_.c

gerror_.c:	SCCS/s.gerror_.c
	rm -f gerror_.c
	get SCCS/s.gerror_.c

getarg_.c:	SCCS/s.getarg_.c
	rm -f getarg_.c
	get SCCS/s.getarg_.c

getc_.c:	SCCS/s.getc_.c
	rm -f getc_.c
	get SCCS/s.getc_.c

getcwd_.c:	SCCS/s.getcwd_.c
	rm -f getcwd_.c
	get SCCS/s.getcwd_.c

getenv_.c:	SCCS/s.getenv_.c
	rm -f getenv_.c
	get SCCS/s.getenv_.c

getgid_.c:	SCCS/s.getgid_.c
	rm -f getgid_.c
	get SCCS/s.getgid_.c

getlog_.c:	SCCS/s.getlog_.c
	rm -f getlog_.c
	get SCCS/s.getlog_.c

getpid_.c:	SCCS/s.getpid_.c
	rm -f getpid_.c
	get SCCS/s.getpid_.c

getuid_.c:	SCCS/s.getuid_.c
	rm -f getuid_.c
	get SCCS/s.getuid_.c

gmtime_.c:	SCCS/s.gmtime_.c
	rm -f gmtime_.c
	get SCCS/s.gmtime_.c

hostnm_.c:	SCCS/s.hostnm_.c
	rm -f hostnm_.c
	get SCCS/s.hostnm_.c

iargc_.c:	SCCS/s.iargc_.c
	rm -f iargc_.c
	get SCCS/s.iargc_.c

idate_.c:	SCCS/s.idate_.c
	rm -f idate_.c
	get SCCS/s.idate_.c

ierrno_.c:	SCCS/s.ierrno_.c
	rm -f ierrno_.c
	get SCCS/s.ierrno_.c

isatty_.c:	SCCS/s.isatty_.c
	rm -f isatty_.c
	get SCCS/s.isatty_.c

itime_.c:	SCCS/s.itime_.c
	rm -f itime_.c
	get SCCS/s.itime_.c

kill_.c:	SCCS/s.kill_.c
	rm -f kill_.c
	get SCCS/s.kill_.c

link_.c:	SCCS/s.link_.c
	rm -f link_.c
	get SCCS/s.link_.c

loc_.c:	SCCS/s.loc_.c
	rm -f loc_.c
	get SCCS/s.loc_.c

lstat_.c:	SCCS/s.lstat_.c
	rm -f lstat_.c
	get SCCS/s.lstat_.c

ltime_.c:	SCCS/s.ltime_.c
	rm -f ltime_.c
	get SCCS/s.ltime_.c

perror_.c:	SCCS/s.perror_.c
	rm -f perror_.c
	get SCCS/s.perror_.c

putc_.c:	SCCS/s.putc_.c
	rm -f putc_.c
	get SCCS/s.putc_.c

qsort_.c:	SCCS/s.qsort_.c
	rm -f qsort_.c
	get SCCS/s.qsort_.c

rename_.c:	SCCS/s.rename_.c
	rm -f rename_.c
	get SCCS/s.rename_.c

signal_.c:	SCCS/s.signal_.c
	rm -f signal_.c
	get SCCS/s.signal_.c

sleep_.c:	SCCS/s.sleep_.c
	rm -f sleep_.c
	get SCCS/s.sleep_.c

stat_.c:	SCCS/s.stat_.c
	rm -f stat_.c
	get SCCS/s.stat_.c

symlnk_.c:	SCCS/s.symlnk_.c
	rm -f symlnk_.c
	get SCCS/s.symlnk_.c

system_.c:	SCCS/s.system_.c
	rm -f system_.c
	get SCCS/s.system_.c

time_.c:	SCCS/s.time_.c
	rm -f time_.c
	get SCCS/s.time_.c

tapeio.c:	SCCS/s.tapeio.c
	@rm -f tapeio.c
	get SCCS/s.tapeio.c

ttynam_.c:	SCCS/s.ttynam_.c
	rm -f ttynam_.c
	get SCCS/s.ttynam_.c

unlink_.c:	SCCS/s.unlink_.c
	rm -f unlink_.c
	get SCCS/s.unlink_.c

wait_.c:	SCCS/s.wait_.c
	rm -f wait_.c
	get SCCS/s.wait_.c

