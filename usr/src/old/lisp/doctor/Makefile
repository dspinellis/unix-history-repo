#	@(#)Makefile	4.4	(Berkeley)	85/09/07
#
DESTDIR=

doctor: /usr/ucb/lisp /usr/ucb/liszt
	liszt -mrq -o doctor doctor.l

install: doctor
	install -s doctor $(DESTDIR)/usr/games

clean:
	rm -f doctor
