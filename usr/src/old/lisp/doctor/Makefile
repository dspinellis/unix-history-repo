#	@(#)Makefile	4.5	(Berkeley)	85/10/25
#
DESTDIR=

doctor: /usr/ucb/lisp /usr/ucb/liszt
	liszt -mrq -o doctor doctor.l

install: doctor
	install doctor $(DESTDIR)/usr/games

clean:
	rm -f doctor
