#	@(#)Makefile.inc	5.1 (Berkeley) %G%

# gmon sources
.PATH: ${.CURDIR}/gmon

SRCS+=	gmon.c mcount.c

# mcount cannot be compiled with profiling
mcount.po: mcount.o
	cp mcount.o mcount.po
