#	@(#)Makefile	4.3	(Berkeley)	85/06/25
#
DESTDIR=

doctor: /usr/ucb/lisp /usr/ucb/liszt
	liszt -mrq -o doctor doctor.l

install: doctor
	install doctor $(DESTDIR)/usr/games

clean:
	rm -f doctor
