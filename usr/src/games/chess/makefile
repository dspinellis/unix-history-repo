a.out:	agen.o\
	att.o\
	bgen.o\
	bheur.o\
	bmove.o\
	book.o\
	bplay.o\
	ctrl.o\
	data.o\
	init.o\
	io.o\
	mater.o\
	play.o\
	qsort.o\
	savres.o\
	setup.o\
	stat.o\
	stdin.o\
	wgen.o\
	wheur.o\
	wmove.o\
	wplay.o
	cc -n -s agen.o\
	att.o\
	bgen.o\
	bheur.o\
	bmove.o\
	book.o\
	bplay.o\
	ctrl.o\
	data.o\
	init.o\
	io.o\
	mater.o\
	play.o\
	qsort.o\
	savres.o\
	setup.o\
	stat.o\
	stdin.o\
	wgen.o\
	wheur.o\
	wmove.o\
	wplay.o

agen.i:	agen.c old.h
	cc -S -O agen.c
	mv agen.s  agen.i

agen.o:	agen.i
	as - -o agen.o agen.i

bheur.i:	bheur.c old.h
	cc -S -O bheur.c
	mv bheur.s  bheur.i

bheur.o:	bheur.i
	as - -o bheur.o bheur.i

book.i:	book.c old.h
	cc -S -O book.c
	mv book.s  book.i

book.o:	book.i
	as - -o book.o book.i

bplay.i:	bplay.c old.h
	cc -S -O bplay.c
	mv bplay.s  bplay.i

bplay.o:	bplay.i
	as - -o bplay.o bplay.i

data.i:	data.c old.h
	cc -S -O data.c
	mv data.s  data.i

data.o:	data.i
	as - -o data.o data.i

init.i:	init.c old.h
	cc -S -O init.c
	mv init.s  init.i

init.o:	init.i
	as - -o init.o init.i

io.i:	io.c old.h
	cc -S -O io.c
	mv io.s  io.i

io.o:	io.i
	as - -o io.o io.i

mater.i:	mater.c old.h
	cc -S -O mater.c
	mv mater.s  mater.i

mater.o:	mater.i
	as - -o mater.o mater.i

play.i:	play.c old.h
	cc -S -O play.c
	mv play.s  play.i

play.o:	play.i
	as - -o play.o play.i

savres.i:	savres.c old.h
	cc -S -O savres.c
	mv savres.s  savres.i

savres.o:	savres.i
	as - -o savres.o savres.i

setup.i:	setup.c old.h
	cc -S -O setup.c
	mv setup.s  setup.i

setup.o:	setup.i
	as - -o setup.o setup.i

stat.i:	stat.c old.h
	cc -S -O stat.c
	mv stat.s  stat.i

stat.o:	stat.i
	as - -o stat.o stat.i

stdin.i:	stdin.c old.h
	cc -S -O stdin.c
	mv stdin.s  stdin.i

stdin.o:	stdin.i
	as - -o stdin.o stdin.i

wheur.i:	wheur.c old.h
	cc -S -O wheur.c
	mv wheur.s  wheur.i

wheur.o:	wheur.i
	as - -o wheur.o wheur.i

wplay.i:	wplay.c old.h
	cc -S -O wplay.c
	mv wplay.s  wplay.i

wplay.o:	wplay.i
	as - -o wplay.o wplay.i

att.o:	att.s
	as - -o att.o att.s

bgen.o:	bgen.s
	as - -o bgen.o bgen.s

bmove.o:	bmove.s
	as - -o bmove.o bmove.s

ctrl.o:	ctrl.s
	as - -o ctrl.o ctrl.s

qsort.o:	qsort.s
	as - -o qsort.o qsort.s

wgen.o:	wgen.s
	as - -o wgen.o wgen.s

wmove.o:	wmove.s
	as - -o wmove.o wmove.s

