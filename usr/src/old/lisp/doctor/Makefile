#	@(#)Makefile	4.1	(Berkeley)	82/11/22
#
DESTDIR=

doctor: /usr/ucb/lisp /usr/ucb/liszt
	liszt -mrq doctor.l
	mv doctor.o doctor
	chmod 755 doctor

install:
	install doctor $(DESTDIR)/usr/games

clean:
	rm -f doctor
