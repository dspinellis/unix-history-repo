#	Makefile	5.2	86/07/02
#
DESTDIR=
SRCS=	addd.s addf.s cmpd.s cmpf.s cvtdl.s cvtfl.s cvtld.s cvtlf.s cvtud.s \
	cvtuf.s divd.s divf.s fnorm.s fp_exp.s modf.s muld.s mulf.s negd.s \
	negf.s sfnorm.s sfp_exp.s subd.s subf.s
OBJS=	addd.o addf.o cmpd.o cmpf.o cvtdl.o cvtfl.o cvtld.o cvtlf.o cvtud.o \
	cvtuf.o divd.o divf.o fnorm.o fp_exp.o modf.o muld.o mulf.o negd.o \
	negf.o sfnorm.o sfp_exp.o subd.o subf.o
TAGSFILE=tags

.s.o:
	/lib/cpp -E ${DEFS} -DPROF $*.s | ${AS} -o $*.o
	ld -X -r $*.o
	mv a.out profiled/$*.o
	/lib/cpp -E ${DEFS} $*.s | ${AS} -o $*.o
	ld -x -r $*.o
	mv a.out $*.o

fpelib fpelib_p: ${OBJS}
	@echo "building profiled fpelib"
	@cd profiled; ar cru ../fpelib_p ${OBJS}
	@echo "building normal fpelib"
	@ar cru fpelib ${OBJS}

tags:
	echo "no tags here"

clean:
	rm -f ${OBJS} profiled/*.o errs a.out core fpelib fpelib_p Makefile.bak

depend:
	for i in ${SRCS}; do \
	    cc -M ${CFLAGS} $$i | awk ' { if ($$1 != prev) \
		{ if (rec != "") print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
		END { print rec } ' >> makedep; done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

addd.o: addd.s /usr/include/tahoemath/fp.h ./DEFS.h
addf.o: addf.s /usr/include/tahoemath//fp.h ./DEFS.h
cmpd.o: cmpd.s ./DEFS.h
cmpf.o: cmpf.s ./DEFS.h
cvtdl.o: cvtdl.s /usr/include/tahoemath/fp.h ./DEFS.h
cvtfl.o: cvtfl.s /usr/include/tahoemath/fp.h ./DEFS.h
cvtld.o: cvtld.s /usr/include/tahoemath/fp.h ./DEFS.h
cvtlf.o: cvtlf.s /usr/include/tahoemath/fp.h ./DEFS.h
cvtud.o: cvtud.s /usr/include/tahoemath/fp.h ./DEFS.h
cvtuf.o: cvtuf.s /usr/include/tahoemath/fp.h ./DEFS.h
divd.o: divd.s /usr/include/tahoemath/fp.h ./DEFS.h
divf.o: divf.s /usr/include/tahoemath/fp.h ./DEFS.h
fnorm.o: fnorm.s /usr/include/tahoemath/fp.h ./DEFS.h
fp_exp.o: fp_exp.s /usr/include/tahoemath/fp.h ./DEFS.h
modf.o: modf.s /usr/include/tahoemath/fp.h ./DEFS.h
muld.o: muld.s /usr/include/tahoemath/fp.h ./DEFS.h
mulf.o: mulf.s /usr/include/tahoemath/fp.h ./DEFS.h
negd.o: negd.s /usr/include/tahoemath/fp.h ./DEFS.h
negf.o: negf.s /usr/include/tahoemath/fp.h ./DEFS.h
sfnorm.o: sfnorm.s /usr/include/tahoemath/fp.h ./DEFS.h
sfp_exp.o: sfp_exp.s /usr/include/tahoemath/fp.h ./DEFS.h
subd.o: subd.s /usr/include/tahoemath/fp.h ./DEFS.h
subf.o: subf.s /usr/include/tahoemath/fp.h ./DEFS.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
