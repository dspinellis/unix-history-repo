#
# Copyright 1985, Massachusetts Institute of Technology.
#
#
# 
CONFDIR= /usr/doc
# TROFF determines what form of troff
#
# Remember to run off the index to the Xlib manual after makeing sure the 
# page number is correct in the indexmacros file...  Someday we we
# automate this procedure....
#
SUBDIR= Xlib installation Usenix

TROFF= psroff
# set for our printer
PRINTER= lpr -n -Ppsc

all:

paper:
	for i in ${SUBDIR}; do \
	    (cd $$i; echo "troffing $$i";\
	     make ${MFLAGS} TROFF="${TROFF}" PRINTER="${PRINTER}" DESTDIR=${DESTDIR} CONFDIR=${CONFDIR} paper;\
	     cd ..);\
	done

install:
	rm -rf ${DESTDIR}${CONFDIR}/X.old
#	-mv -f ${DESTDIR}${CONFDIR}/X ${DESTDIR}${CONFDIR}/X.old
	-mkdir ${DESTDIR}${CONFDIR}/X > /dev/null 2>&1
	cp -r ${SUBDIR} ${DESTDIR}${CONFDIR}/X

clean:
	for i in ${SUBDIR}; do \
	    (cd $$i; echo "cleaning $$i";\
	     make ${MFLAGS} clean;\
	     cd ..);\
	done
	rm -f *~ \#* *.bak *.BAK
