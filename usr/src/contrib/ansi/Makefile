#	@(#)Makefile	5.1	(Berkeley)	85/08/13
#
DESTDIR=
CFLAGS=	-O
INCPATH=
LFLAGS=	
LIBS=

all:	ansitape vmsprep

ansitape: ansitape.c
	cc ${CFLAGS} -o ansitape ansitape.c

vmsprep: vmsprep.c
	cc ${CFLAGS} -o vmsprep vmsprep.c

install: vmsprep ansitape
	install -s vmsprep ${DESTDIR}/usr/new/vmsprep
	install -s ansitape ${DESTDIR}/usr/new/ansitape
	install -c vmsprep.n ${DESTDIR}/usr/man/mann/vmsprep.n
	install -c ansitape.n ${DESTDIR}/usr/man/mann/ansitape.n

clean:
	rm -f a.out core errs makefile.bak Makefile.bak ansitape vmsprep

depend:
	cc -M ${INCPATH} vmsprep.c ansitape.c | \
	awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$2 } } \
	      END { print rec } ' > makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it


vmsprep.o: vmsprep.c /usr/include/stdio.h /usr/include/ctype.h
vmsprep.o: /usr/include/strings.h
ansitape.o: ansitape.c /usr/include/sys/types.h /usr/include/sys/time.h
ansitape.o: /usr/include/time.h /usr/include/sys/mtio.h
ansitape.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
ansitape.o: /usr/include/sys/ttydev.h /usr/include/sys/file.h
ansitape.o: /usr/include/sys/stat.h /usr/include/a.out.h
ansitape.o: /usr/include/sys/exec.h /usr/include/stdio.h /usr/include/ctype.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
