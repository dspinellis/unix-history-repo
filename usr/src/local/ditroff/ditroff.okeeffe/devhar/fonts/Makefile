#
#	Makefile	(CWI)	1.3	87/03/31
#
DESTDIR=
LIBDIR= /usr/local/lib/ditroff/font/devhar
STANDARDFONTS = R I B BI Vl vl V v S Sm
EXTRAFONTS = Bs G3 RS Rs SO Vm bI bS bb bi br bs cI cb ci cr fs lI lb li lp lr\
	vm sR TR
FONTFILES = B.out BI.out Bs.out DESC.out G3.out I.out R.out RS.out Rs.out\
	S.out SO.out Sm.out V.out Vl.out Vm.out bI.out bS.out bb.out bi.out\
	br.out bs.out cI.out cb.out ci.out cr.out fs.out lI.out lb.out li.out\
	lp.out lr.out v.out vl.out vm.out sR.out TR.out

all:	../../utilities/makedev binaryfonts 

binaryfonts:	DESC ${STANDARDFONTS} ${EXTRAFONTS}
	../../utilities/makedev DESC ${EXTRAFONTS}
	touch binaryfonts
	-if test -f MadeMakedev; then rm MadeMakedev ../../utilities/makedev; fi

install:	all
	for i in ${FONTFILES}; do\
	install -c -m 644 $$i ${DESTDIR}${LIBDIR}/$$i; done

../../utilities/makedev:
	cd ../../utilities; make makedev
	touch MadeMakedev

cmp:	all
	@echo Comparing ...
	@-for i in ${FONTFILES}; do\
	cmp $$i ${DESTDIR}${LIBDIR}/$$i; done

print:
	print DESC ${STANDARDFONTS} ${EXTRAFONTS}

clean:
	rm -f *.out binaryfonts
