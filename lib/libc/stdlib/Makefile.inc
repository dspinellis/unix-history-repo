#	@(#)Makefile.inc	5.6 (Berkeley) 6/4/91

# stdlib sources
.PATH: ${.CURDIR}/${MACHINE}/stdlib ${.CURDIR}/stdlib

SRCS+=	abort.c atexit.c atoi.c atol.c bsearch.c calloc.c div.c exit.c \
	getenv.c getopt.c heapsort.c labs.c ldiv.c malloc.c multibyte.c \
	putenv.c qsort.c radixsort.c rand.c random.c setenv.c strtod.c \
	strtol.c strtoul.c system.c

.if   (${MACHINE} == "hp300")
SRCS+=	abs.s atof.c
.elif (${MACHINE} == "i386")
SRCS+=	abs.s atof.c
.elif (${MACHINE} == "tahoe")
SRCS+=	abs.s atof.c
.elif (${MACHINE} == "vax")
SRCS+=	abs.s atof.c
.endif

MAN3+=	stdlib/abort.3 stdlib/abs.3 stdlib/alloca.3 stdlib/atexit.3 \
	stdlib/atof.3 stdlib/atoi.3 stdlib/atol.3 stdlib/bsearch.3 \
	stdlib/calloc.3 stdlib/div.3 stdlib/exit.3 stdlib/free.3 \
	stdlib/getenv.3 stdlib/getopt.3 stdlib/labs.3 stdlib/ldiv.3 \
	stdlib/malloc.3 stdlib/memory.3 stdlib/qsort.3 stdlib/radixsort.3 \
	stdlib/rand.3 stdlib/random.3 stdlib/realloc.3 \
	stdlib/strtod.3 stdlib/strtol.3 stdlib/strtoul.3 stdlib/system.3

MLINKS+=getenv.3 setenv.3 getenv.3 unsetenv.3 getenv.3 putenv.3
MLINKS+=qsort.3 heapsort.3
MLINKS+=rand.3 srand.3
MLINKS+=random.3 initstate.3 random.3 setstate.3 random.3 srandom.3
