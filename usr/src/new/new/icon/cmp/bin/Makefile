all: 	iconc
	cd ../tran;       make ${MFLAGS} 
	cd ../link;       make ${MFLAGS}
	cd ../operators;  make ${MFLAGS}
	cd ../functions;  make ${MFLAGS}
	cd ../lib;        make ${MFLAGS}
	cd ../rt;         make ${MFLAGS}
	cp Lib libi.a
	ranlib libi.a
	cp ../tran/utran ../link/ulink ../rt/start.o \
		../rt/pstart.o ../rt/pstop.o .

iconc: iconc.o
	cc -o iconc iconc.o

tran:
	cp ../tran/utran utran

link:
	cp ../link/ulink ulink

start:
	cp ../rt/start.o start.o

lib:
	cp Lib libi.a
	ranlib libi.a
clean:
	rm -f iconc utran ulink Lib libi.a *.o

clean-all: 
	rm -f iconc.o Lib
	cd ../tran;       make ${MFLAGS} clean
	cd ../link;       make ${MFLAGS} clean
	cd ../operators;  make ${MFLAGS} clean
	cd ../functions;  make ${MFLAGS} clean
	cd ../lib;        make ${MFLAGS} clean
	cd ../rt;         make ${MFLAGS} clean
