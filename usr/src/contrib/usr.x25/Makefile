all:	lib/x25lib.a x25config nimd/nimd x29d/x29d

install:	all
	install -s -m 755 x25config /etc/x25config
	(cd nimd; make install)
	(cd x29d; make install)
	@echo don't forget to edit /etc/rc.local

lib/x25lib.a:
	(cd lib; make)

x25config:	x25config.c lib/x25lib.a
	cc -O x25config.c lib/x25lib.a -o x25config

nimd/nimd:
	(cd nimd; make)

x29d/x29d:
	(cd x29d; make)

clean:
	(cd lib; make clean)
	(cd nimd; make clean)
	(cd x29d; make clean)
	rm -f core *.o *.CKP .emacs*

cleanall:	clean
	rm -f x25config lib/x25lib.a nimd/nimd x29d/x29d
