#	@(#)Makefile	4.2	(Berkeley)	83/07/26
#
DESTDIR=

doctor: /usr/ucb/lisp /usr/ucb/liszt
	liszt -mrq -o doctor doctor.l

install:
	install doctor $(DESTDIR)/usr/games

clean:
	rm -f doctor
