#	@(#)Makefile.inc	5.2 (Berkeley) %G%

# stdlib sources
.PATH: ${.CURDIR}/${MACHINE}/stdlib ${.CURDIR}/stdlib

SRCS+=	abort.c atexit.c atoi.c atol.c bsearch.c calloc.c div.c exit.c \
	getenv.c labs.c ldiv.c malloc.c putenv.c qsort.c radixsort.c \
	rand.c setenv.c strtol.c strtoul.c system.c

.if   (${MACHINE} == "hp300")
SRCS+=	abs.s atof.c
.elif (${MACHINE} == "i386")
SRCS+=	abs.s atof.c
.elif (${MACHINE} == "tahoe")
SRCS+=	abs.s atof.s
.elif (${MACHINE} == "vax")
SRCS+=	abs.s atof.s
.endif

MAN3+=	abort.0 abs.0 atexit.0 atof.0 atoi.0 atol.0 bsearch.0 div.0 exit.0 \
	getenv.0 labs.0 ldiv.0 malloc.0 qsort.0 radixsort.0 rand.0 strtod.0 \
	strtol.0 strtoul.0 system.0

MLINKS+=malloc.3 alloca.3 malloc.3 calloc.3 malloc.3 free.3 malloc.3 realloc.3
MLINKS+=getenv.3 setenv.3 getenv.3 unsetenv.3 getenv.3 putenv.3
MLINKS+=rand.3 srand.3
