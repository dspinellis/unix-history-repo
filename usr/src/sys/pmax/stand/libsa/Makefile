#	@(#)Makefile	7.1 (Berkeley) %G%

LIB=	sa
SRCS=	alloc.c bcopy.c bzero.c close.c dev.c disklabel.c getfile.c \
	ioctl.c lseek.c open.c read.c ufs.c write.c \
	callvec.c devopen.c getenv.c gets.c strcat.c strcmp.c strcpy.c

.PATH:	../../../stand
NOPROFILE=noprofile

.include <bsd.lib.mk>

CFLAGS+=-I../../..
