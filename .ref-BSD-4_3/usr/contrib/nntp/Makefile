#
# Makefile for NNTP intstallation
#
all:
	cd server; make ${MFLAGS} all
	cd support; make ${MFLAGS} all
	cd rrn; make depend; make ${MFLAGS} all
	cd inews; make ${MFLAGS} all

install: install_client

clean:
	cd server; make ${MFLAGS} clean
	cd support; make ${MFLAGS} clean
	cd rrn; make ${MFLAGS} clean
	cd inews; make ${MFLAGS} clean

install_server:
	cd server; make ${MFLAGS} install
	cd support; make ${MFLAGS} install

install_client:
	cd rrn; make ${MFLAGS} install
	cd inews; make ${MFLAGS} install
