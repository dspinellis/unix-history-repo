#	@(#)Makefile.inc	5.4 (Berkeley) 6/25/90

# string sources
.PATH: ${.CURDIR}/${MACHINE}/string ${.CURDIR}/string

SRCS+=	memccpy.c strcasecmp.c strcoll.c strdup.c strerror.c \
	strftime.c strmode.c strtok.c strxfrm.c swab.c

.if   (${MACHINE} == "hp300")
SRCS+=	bcmp.s bcopy.s bzero.s ffs.s index.s memchr.c memcmp.c memset.c \
	rindex.s strcat.c strcmp.s strcpy.s strcspn.c strlen.s \
	strncat.c strncmp.s strncpy.s strpbrk.c strsep.c \
	strstr.c strstr.c
.elif (${MACHINE} == "i386")
SRCS+=	bcmp.c bcopy.c bzero.s ffs.c index.c memchr.c memcmp.c memset.c \
	rindex.c strcat.c strcmp.c strcpy.c strcspn.c strlen.c \
	strncat.c strncmp.c strncpy.c strpbrk.c strsep.c \
	strspn.c strstr.c
.elif (${MACHINE} == "tahoe")
SRCS+=	bcmp.s bcopy.s bzero.s ffs.s index.c memchr.c memcmp.s memset.c \
	rindex.c strcat.s strcmp.s strcpy.s strcspn.c strlen.s \
	strncat.s strncmp.s strncpy.s strpbrk.c strsep.c \
	strspn.c strstr.c
SRCS+=	memmove.s
.elif (${MACHINE} == "vax")
SRCS+=	bcmp.s bcopy.s bzero.s ffs.s index.c memchr.s memcmp.s memset.s \
	rindex.s strcat.s strcmp.s strcpy.s strcspn.s strlen.s \
	strncat.s strncmp.s strncpy.s strpbrk.s strsep.s \
	strspn.s strstr.s
SRCS+=	memmove.s strchr.s strrchr.s
.endif

# if no machine specific memmove(3), build one out of bcopy(3).
.if empty(SRCS:Mmemmove.s)
OBJS+=	memmove.o
memmove.o: bcopy.c
	${CC} -DMEMMOVE ${CFLAGS} -c ${.ALLSRC} -o ${.TARGET}
	@${LD} -x -r ${.TARGET}
	@mv a.out ${.TARGET}

memmove.po: bcopy.c
	${CC} -DMEMMOVE ${CFLAGS} -c -p ${.ALLSRC} -o ${.TARGET}
	@${LD} -X -r ${.TARGET}
	@mv a.out ${.TARGET}
.endif

# if no machine specific memcpy(3), build one out of bcopy(3).
.if empty(SRCS:Mmemcpy.s)
OBJS+=	memcpy.o
memcpy.o: bcopy.c
	${CC} -DMEMCOPY ${CFLAGS} -c ${.ALLSRC} -o ${.TARGET}
	@${LD} -x -r ${.TARGET}
	@mv a.out ${.TARGET}

memcpy.po: bcopy.c
	${CC} -DMEMCOPY ${CFLAGS} -c -p ${.ALLSRC} -o ${.TARGET}
	@${LD} -X -r ${.TARGET}
	@mv a.out ${.TARGET}
.endif

# if no machine specific strchr(3), build one out of index(3).
.if empty(SRCS:Mstrchr.s)
OBJS+=	strchr.o
strchr.o: index.c
	${CC} -DSTRCHR ${CFLAGS} -c ${.ALLSRC} -o ${.TARGET}
	@${LD} -x -r ${.TARGET}
	@mv a.out ${.TARGET}

strchr.po: index.c
	${CC} -DSTRCHR ${CFLAGS} -c -p ${.ALLSRC} -o ${.TARGET}
	@${LD} -X -r ${.TARGET}
	@mv a.out ${.TARGET}
.endif

# if no machine specific strrchr(3), build one out of rindex(3).
.if empty(SRCS:Mstrrchr.s)
OBJS+=	strrchr.o
strrchr.o: rindex.c
	${CC} -DSTRRCHR ${CFLAGS} -c ${.ALLSRC} -o ${.TARGET}
	@${LD} -x -r ${.TARGET}
	@mv a.out ${.TARGET}

strrchr.po: rindex.c
	${CC} -DSTRRCHR ${CFLAGS} -c -p ${.ALLSRC} -o ${.TARGET}
	@${LD} -X -r ${.TARGET}
	@mv a.out ${.TARGET}
.endif

MAN3+=	bcmp.0 bcopy.0 bstring.0 bzero.0 ffs.0 index.0 memccpy.0 memchr.0 \
	memcmp.0 memcpy.0 memmove.0 memset.0 rindex.0 strcasecmp.0 strcat.0 \
	strchr.0 strcmp.0 strcoll.0 strcpy.0 strcspn.0 strftime.0 string.0 \
	strlen.0 strmode.0 strdup.0 strpbrk.0 strrchr.0 strsep.0 strspn.0 \
	strstr.0 strtok.0 strxfrm.0 swab.0

MLINKS+=strcasecmp.3 strncasecmp.3
MLINKS+=strcat.3 strncat.3
MLINKS+=strcmp.3 strncmp.3
MLINKS+=strcpy.3 strncpy.3
