#	%W% (Berkeley) %G%
#
# Cloned from /usr/src/include/Makefile
# Doing a make install builds /usr/include/g++
#

all clean cleandir depend lint tags:

FILES=		ACG.h AllocRing.h Binomial.h BitSet.h BitString.h \
		Complex.h CursesW.h DiscUnif.h Erlang.h File.h \
		Filebuf.h Fix.h Fix16.h Fix24.h Fmodes.h Geom.h \
		GetOpt.h HypGeom.h Incremental.h Integer.h LogNorm.h \
		MLCG.h NegExp.h Normal.h Obstack.h Pix.h PlotFile.h \
		Poisson.h RNG.h Random.h Rational.h Regex.h RndInt.h \
		SFile.h SmplHist.h SmplStat.h String.h Uniform.h \
		Weibull.h abs.h assert.h bool.h builtin.h compare.h \
		complex.h curses.h file.h filebuf.h generic.h \
		istream.h malloc.h max.h min.h minmax.h new.h open.h \
		osfcn.h ostream.h regex.h std.h strclass.h stream.h \
		streambuf.h swap.h values.h

DIRS=		gen

NOOBJ=		noobj

INCDIR=		/usr/include/g++

install:
	@echo installing ${FILES}
	@-for i in ${FILES}; do \
		cmp -s $$i ${DESTDIR}${INCDIR}/$$i || \
		    install -c -o ${BINOWN} -g ${BINGRP} -m 444 $$i \
		    ${DESTDIR}${INCDIR}/$$i; \
	done
	@echo installing ${DIRS}
	@-for i in ${DIRS}; do \
		if [ ! -d ${DESTDIR}${INCDIR}/$$i ]; \
		then \
			mkdir ${DESTDIR}${INCDIR}/$$i; \
		fi; \
		chown ${BINOWN}.${BINGRP} ${DESTDIR}${INCDIR}/$$i; \
		chmod 755 ${DESTDIR}${INCDIR}/$$i; \
		(cd $$i; for j in *.*P; do \
			cmp -s $$j ${DESTDIR}${INCDIR}/$$i/$$j || \
			install -c -o ${BINOWN} -g ${BINGRP} -m 444 $$j \
			    ${DESTDIR}${INCDIR}/$$i/$$j; \
		done); \
	done

.include <bsd.prog.mk>
