CFLAGS = -O

ilink:		builtin.o lcode.o glob.o llex.o lmem.o opcode.o lsym.o ilink.o
		cc -n -o ilink *.o

builtin.o:	ilink.h ../h/config.h ../h/pdef.h
lcode.o:	ilink.h opcode.h datatype.h ../h/config.h
glob.o:		ilink.h opcode.h ../h/config.h
llex.o:		ilink.h opcode.h ../h/config.h
lmem.o:		ilink.h ../h/config.h
opcode.o:	ilink.h opcode.h ../h/config.h
lsym.o:		ilink.h ../h/config.h
ilink.o:	ilink.h ../h/config.h

Listall:
		@pr *.h *.c
		@date >List

List:		datatype.h opcode.h ilink.h \
		builtin.c lcode.c glob.c llex.c lmem.c opcode.c lsym.c ilink.c
		@pr $?
		@date >List
Clean:
		rm -f *.o ilink

Dist-clean:
		rm -f .BK* .CK* .emacs*
		rm -f `gcomp Makefile *.h *.c *.gen Files`
