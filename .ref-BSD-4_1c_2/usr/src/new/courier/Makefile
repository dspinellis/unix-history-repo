# Makefile for UNIX Courier

install:	compiler library daemon examples

compiler:	/tmp
	cd compiler; make install

header:	/tmp
	cd lib; install -c -m 644 courier.h /usr/include

library:	/tmp header
	cd lib; make install

daemon:	/tmp header library
	-mkdir /usr/lib/courier
	cd daemon; make install
	echo "Please change /etc/rc to run \"/etc/courierd &\""

examples: /tmp compiler header library
	for d in examples/*; do (cd $$d; make install); done

clean:
	cd compiler; make clean
	cd lib; make clean
	-cd daemon; rm -f courierd.o
	for d in examples/*; do (cd $$d; make clean); done
