#	@(#)Makefile.inc	5.1 (Berkeley) %G%

# stdio sources
.PATH: ${.CURDIR}/${MACHINE}/stdio ${.CURDIR}/stdio

SRCS+=	clrerr.c doprnt.c doscan.c fdopen.c fgetc.c filbuf.c findiop.c \
	flsbuf.c fopen.c fprintf.c fputc.c fread.c freopen.c fseek.c \
	ftell.c fwrite.c getchar.c gets.c getw.c printf.c putchar.c \
	putw.c rew.c scanf.c setbuf.c setbuffer.c sprintf.c ungetc.c \
	vfprintf.c vprintf.c vsprintf.c

.if   (${MACHINE} == "hp300")
SRCS+=	fgets.c fputs.c puts.c
.elif (${MACHINE} == "i386")
SRCS+=	fgets.c fputs.c puts.c
.elif (${MACHINE} == "tahoe")
SRCS+=	fgets.c fputs.c puts.c
.elif (${MACHINE} == "vax")
SRCS+=	fgets.s fputs.s puts.s
.endif

MAN3+=	fclose.0 ferror.0 fgets.0 fopen.0 fread.0 fseek.0 getc.0 \
	gets.0 printf.0 putc.0 puts.0 scanf.0 setbuf.0 stdio.0 ungetc.0 \

MLINKS+=fclose.3 fflush.3
MLINKS+=ferror.3 clearerr.3 ferror.3 feof.3 ferror.3 fileno.3
MLINKS+=fopen.3 fdopen.3 fopen.3 freopen.3
MLINKS+=fread.3 fwrite.3
MLINKS+=fseek.3 ftell.3 fseek.3 rewind.3
MLINKS+=getc.3 fgetc.3 getc.3 getchar.3 getc.3 getw.3
MLINKS+=printf.3 fprintf.3 printf.3 sprintf.3 printf.3 vfprintf.3 \
	printf.3 vprintf.3 printf.3 vsprintf.3
MLINKS+=putc.3 fputc.3 putc.3 putchar.3 putc.3 putw.3
MLINKS+=puts.3 fputs.3
MLINKS+=scanf.3 fscanf.3 scanf.3 sscanf.3
MLINKS+=setbuf.3 setbuffer.3 setbuf.3 setlinebuf.3
