#	%M%	%I%	%E%
#
DESTDIR=

all:	
	cd cmds; make ${MFLAGS}

install: all
	cd cmds; make DESTDIR=${DESTDIR} install
	cd man; make DESTDIR=${DESTDIR} install

clean:
	cd cmds; make ${MFLAGS} clean
