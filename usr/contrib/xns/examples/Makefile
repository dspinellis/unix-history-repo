# $Header$
#
# $Log$
#
SUBDIRS=print gap ch filing
BROKEN=authchtest retrieve auth 
CRLIB=${DESTDIR}/usr/new/lib/xnscourier
BINDIR=/usr/new

all:
	-for Dir in ${SUBDIRS}; do \
		(cd $$Dir; make ${MFLAGS} CRLIB=${CRLIB} BINDIR=${BINDIR}); done

install:
	-for Dir in ${SUBDIRS}; do \
		(cd $$Dir; \
		make ${MFLAGS} DESTDIR=${DESTDIR} CRLIB=${CRLIB} \
			BINDIR=${BINDIR} install); \
		done

rcs:
	-for Dir in ${SUBDIRS}; do (cd $$Dir; make ${MFLAGS} rcs); done

clean:
	-rm -f *.BAK *.CKP
	-for Dir in ${SUBDIRS}; do (cd $$Dir; make ${MFLAGS} clean); done

