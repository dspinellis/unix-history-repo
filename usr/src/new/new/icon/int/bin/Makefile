all:	icont iconx.hdr
	cd ../tran;       make ${MFLAGS}
	cd ../link;       make ${MFLAGS}
	cd ../operators;  make ${MFLAGS}
	cd ../functions;  make ${MFLAGS}
	cd ../lib;        make ${MFLAGS}
	cd ../rt;         make ${MFLAGS}
	cd ../iconx;      make ${MFLAGS}
	cp ../tran/utran ../link/ulink ../iconx/iconx .
	
icont: icont.o
	cc -O -o icont icont.o
	
iconx.hdr:  ixhdr.o
	cc -O -N ixhdr.o -o iconx.hdr
	strip iconx.hdr

.s.o:
	cat ../h/defs.s $< >t.c
	cc -E t.c | grep -v "^#" >t.s
	as -o $@ t.s
	rm -f t.c t.s

clean:
	rm -f utran ulink iconx icont icont.o iconx.hdr ixhdr ixhdr.o

clean-all:
	rm -f *.o
	cd ../tran;       make ${MFLAGS} clean
	cd ../link;       make ${MFLAGS} clean
	cd ../operators;  make ${MFLAGS} clean
	cd ../functions;  make ${MFLAGS} clean
	cd ../lib;        make ${MFLAGS} clean
	cd ../rt;         make ${MFLAGS} clean
	cd ../iconx;      make ${MFLAGS} clean
