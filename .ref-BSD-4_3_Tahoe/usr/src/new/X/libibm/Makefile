CONFDIR= /usr/new
XDIR= /lib/X

SUBDIR= bitblt libaed libapa16 libapa8 libapa16 libpqd
all:
	for i in ${SUBDIR}; do \
	    (cd $$i; echo "compiling $$i";\
	     make ${MFLAGS} DESTDIR=${DESTDIR} CONFDIR=${CONFDIR} all;\
	     cd ..);\
	done

install:

inc:

clean:
	rm -f a.out *~ core *.o errs ERRS \#*
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean; cd ..); done

