all: assubs.d fpdata.d fpnofetchi.d fpterp.d opcodes.d pix px_header

assubs.d:
	chdir assubs; make
fpdata.d:
	chdir fpdata; make
fpnofetchi.d:
	chdir fpnofetchi; make
fpterp.d:
	chdir fpterp; make
opcodes.d:
	chdir opcodes; make

pix:
	${CC} -O -n pix.c -o pix -s

px_header: px_header.c
	${CC} -O px_header.c -s
	-dd if=a.out of=px_header bs=1b conv=sync
	-rm -f a.out

install:
	cp pix /usr/ucb/pix
	cp px_header /usr/lib/px_header
	rm -f /usr/lib/npx_header
	ln /usr/lib/px_header /usr/lib/npx_header
	cp how_pi /usr/lib/how_pi
	cp how_pix /usr/lib/how_pix
	cp how_pxp /usr/lib/how_pxp

clean:
	echo "*** Clean this directory AFTER making pi*, px*, pxp ***"

cleanup:
	chdir opcodes; make clean
	chdir fpdata; make clean
	chdir fpnofetchi; make clean
	chdir fpterp; make clean
	chdir opcodes; make clean
	rm -f pix px_header a.out
