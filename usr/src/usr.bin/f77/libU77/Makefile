#	@(#)Makefile	5.7 (Berkeley) %G%

LIB=	U77
SRCS=	access_.c alarm_.c chdir_.c chmod_.c ctime_.c dtime_.c etime_.c \
	f77lid_.c fdate_.c fgetc_.c fork_.c fputc_.c fseek_.c fstat_.c \
	ftell_.c gerror_.c getarg_.c getc_.c getcwd_.c getenv_.c getgid_.c \
	getlog_.c getpid_.c getuid_.c gmtime_.c hostnm_.c iargc_.c idate_.c \
	ierrno_.c ioinit.f isatty_.c itime_.c kill_.c link_.c loc_.c \
	lstat_.c ltime_.c malloc_.c perror_.c putc_.c qsort_.c rename_.c \
	sleep_.c stat_.c symlnk_.c system_.c tapeio.c time_.c ttynam_.c \
	unlink_.c wait_.c

LDADD=		Version.o
CLEANFILES+=	Version.o Version.c mkvers

all: Version.o

Version.o: Version.c
	${CC} -R -c Version.c

Version.c: mkvers ${SRCS}
	mkvers libU77.a ${.ALLSRC:M*.c} > ${.TARGET}

mkvers: mkvers.c
	${CC} ${CFLAGS} ${.CURDIR}/mkvers.c -o ${.TARGET}

.include <bsd.lib.mk>

# Fortran compiler doesn't let you rename .o's, so we'll have to cheat.
.f.o:
	${FC} -p ${FFLAGS} -c ${.IMPSRC}
	@${LD} -x -r ${.TARGET}
	@mv a.out ${.TARGET}

.f.po:
	ln -s ${.IMPSRC} xxx.f
	${FC} -p ${FFLAGS} -c xxx.f
	@${LD} -X -r xxx.o
	@mv a.out ${.TARGET}
	@rm -f xxx.f xxx.o
