all:		icont iconx.hdr
	
icont.o:	../h/config.h
icont:		icont.o
		cc -O -o icont icont.o
	
ixhdr.o:	../h/config.h
iconx.hdr:	ixhdr.o
		cc -O -N ixhdr.o -o iconx.hdr
		strip iconx.hdr


Listall:
		@pr *.c
		@date >List

List:		icont.c ixhdr.c
		@pr $?
		@date >List
Clean:
		rm -f icont.o iconx.hdr ixhdr ixhdr.o

Dist-clean:
		rm -f .BK* .CK* .emacs*
		rm -f `gcomp Makefile icont.c ixhdr.c *.gen`
