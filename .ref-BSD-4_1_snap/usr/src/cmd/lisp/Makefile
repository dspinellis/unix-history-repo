all: llib franz/mylisp liszt/nliszt tryhelpman

franz/mylisp:
	cd franz; make DESTDIR=$(DESTDIR) mylisp

liszt/nliszt:
	cd liszt; make DESTDIR=$(DESTDIR) LISP=../franz/mylisp nliszt

llib:
	cd lib; make DESTDIR=$(DESTDIR) all install

tryhelpman:
	-make helpman;
	date > helpman

helpman:
	cd /usr/doc/lisp;
	rm -fr $(DESTDIR)/usr/lib/lisp/manual
	mkdir $(DESTDIR)/usr/lib/lisp/manual
	make DESTDIR=$(DESTDIR) all

install:
	cd franz; make DESTDIR=$(DESTDIR) install
	cd liszt; make DESTDIR=$(DESTDIR) install
	-(cd /usr/doc/lisp; make DESTDIR=$(DESTDIR) install)
clean:
	cd lib; make clean
	cd franz; make clean
	cd liszt; make clean
	-(cd /; cd /usr/doc/lisp; make clean)
	rm -f helpman
