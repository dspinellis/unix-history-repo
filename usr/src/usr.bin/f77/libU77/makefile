# Makefile for f77 system lib, libU77.a
#	@(#)makefile	1.8	%G%

LIB = libU77.a
LPR = lpr

# for 11's
#ENV = ucb
#LIBDIR = /usr/$(ENV)/lib
#TAPE = /dev/nrmt1

# for Vax
LIBDIR = /usr/lib
TAPE = /dev/rnmt0.1600

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

all:	$(LIB)

$(LIB):	$(OBJECTS) Version
	@echo Loading...
	@rm -f $(LIB)
	@ar rc $(LIB) $(OBJECTS) Version
	@chmod 664 $(LIB)
	@echo "Done..."

install:	$(LIB)
	@-mv $(LIBDIR)/$(LIB) $(LIBDIR)/$(LIB).old
	cp $(LIB) $(LIBDIR)/$(LIB)
	ranlib $(LIBDIR)/$(LIB)
	@chmod 644 $(LIBDIR)/$(LIB)


#compile and strip unnecessary symbols
.c.o:
	cc $(CFLAGS) -c $*.c
	-ld -r -x $*.o
	mv a.out $*.o

Version:	$(SOURCES) mkvers
	@rm -f Version.c
	./mkvers $(LIB) $(SOURCES) > Version.c
	cc -c Version.c
	mv Version.o Version

mkvers:	mkvers.c
	@cc mkvers.c -o mkvers

print:	index
	@pr index Makefile `ls $(SOURCES)` | $(LPR)

index:	$(SOURCES) mkindx
	@./mkindx "f77 $(LIB) Source Listing " Makefile $(SOURCES)

mkindx:	mkindx.c
	@cc mkindx.c -o mkindx

clean:
	rm -f $(OBJECTS) $(LIB)

# End of Makefile
# Below this is not normally distributed ...

Makefile:	makefile
	@sed -n -e "1,/End of Makefile/p" makefile > Makefile

tape:	Makefile
	@-ln man/s.makefile man.s.makefile
	@ls man.s.makefile > /dev/null
	@rm -f man/s.makefile
	@-tar cbf 20 $(TAPE) READ_ME Makefile man $(SOURCES) mkindx.c mkvers.c
	@ln man.s.makefile man/s.makefile
	@rm -f man.s.makefile


# below here is SCCS stuff ---

access_.c:	s.access_.c
	rm -f access_.c
	get s.access_.c
alarm_.c:	s.alarm_.c
	rm -f alarm_.c
	get s.alarm_.c
chdir_.c:	s.chdir_.c
	rm -f chdir_.c
	get s.chdir_.c
ctime_.c:	s.ctime_.c
	rm -f ctime_.c
	get s.ctime_.c
dtime_.c:	s.dtime_.c
	rm -f dtime_.c
	get s.dtime_.c
etime_.c:	s.etime_.c
	rm -f etime_.c
	get s.etime_.c
fdate_.c:	s.fdate_.c
	rm -f fdate_.c
	get s.fdate_.c
fgetc_.c:	s.fgetc_.c
	rm -f fgetc_.c
	get s.fgetc_.c
fork_.c:	s.fork_.c
	rm -f fork_.c
	get s.fork_.c
fputc_.c:	s.fputc_.c
	rm -f fputc_.c
	get s.fputc_.c
fseek_.c:	s.fseek_.c
	rm -f fseek_.c
	get s.fseek_.c
fstat_.c:	s.fstat_.c
	rm -f fstat_.c
	get s.fstat_.c
ftell_.c:	s.ftell_.c
	rm -f ftell_.c
	get s.ftell_.c
gerror_.c:	s.gerror_.c
	rm -f gerror_.c
	get s.gerror_.c
getarg_.c:	s.getarg_.c
	rm -f getarg_.c
	get s.getarg_.c
getc_.c:	s.getc_.c
	rm -f getc_.c
	get s.getc_.c
getcwd_.c:	s.getcwd_.c
	rm -f getcwd_.c
	get s.getcwd_.c
getenv_.c:	s.getenv_.c
	rm -f getenv_.c
	get s.getenv_.c
getgid_.c:	s.getgid_.c
	rm -f getgid_.c
	get s.getgid_.c
getlog_.c:	s.getlog_.c
	rm -f getlog_.c
	get s.getlog_.c
getpid_.c:	s.getpid_.c
	rm -f getpid_.c
	get s.getpid_.c
getuid_.c:	s.getuid_.c
	rm -f getuid_.c
	get s.getuid_.c
gmtime_.c:	s.gmtime_.c
	rm -f gmtime_.c
	get s.gmtime_.c
iargc_.c:	s.iargc_.c
	rm -f iargc_.c
	get s.iargc_.c
idate_.c:	s.idate_.c
	rm -f idate_.c
	get s.idate_.c
ierrno_.c:	s.ierrno_.c
	rm -f ierrno_.c
	get s.ierrno_.c
isatty_.c:	s.isatty_.c
	rm -f isatty_.c
	get s.isatty_.c
itime_.c:	s.itime_.c
	rm -f itime_.c
	get s.itime_.c
kill_.c:	s.kill_.c
	rm -f kill_.c
	get s.kill_.c
loc_.c:	s.loc_.c
	rm -f loc_.c
	get s.loc_.c
link_.c:	s.link_.c
	rm -f link_.c
	get s.link_.c
ltime_.c:	s.ltime_.c
	rm -f ltime_.c
	get s.ltime_.c
perror_.c:	s.perror_.c
	rm -f perror_.c
	get s.perror_.c
putc_.c:	s.putc_.c
	rm -f putc_.c
	get s.putc_.c
qsort_.c:	s.qsort_.c
	rm -f qsort_.c
	get s.qsort_.c
signal_.c:	s.signal_.c
	rm -f signal_.c
	get s.signal_.c
sleep_.c:	s.sleep_.c
	rm -f sleep_.c
	get s.sleep_.c
stat_.c:	s.stat_.c
	rm -f stat_.c
	get s.stat_.c
system_.c:	s.system_.c
	rm -f system_.c
	get s.system_.c
time_.c:	s.time_.c
	rm -f time_.c
	get s.time_.c
ttynam_.c:	s.ttynam_.c
	rm -f ttynam_.c
	get s.ttynam_.c
unlink_.c:	s.unlink_.c
	rm -f unlink_.c
	get s.unlink_.c
wait_.c:	s.wait_.c
	rm -f wait_.c
	get s.wait_.c
