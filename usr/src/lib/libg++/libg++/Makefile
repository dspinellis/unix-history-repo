#	@(#)Makefile	6.2 (Berkeley) 4/24/91
#
# PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
# --------------------         -----   ----------------------
# CURRENT PATCH LEVEL:         1       00118
# --------------------         -----   ----------------------
#
# 30 Mar 93	Rodney W. Grimes	Add CFLAGS so that make depend works
#

#
# Define g++ compilation rules.
#

.SUFFIXES:
.SUFFIXES: .out .o .po .s .c .f .y .cc .l .8 .7 .6 .5 .4 .3 .2 .1 .0

CFLAGS+=-I$(.CURDIR)/../g++-include -+
GXX=		g++
GXXFLAGS=	-O -felide-constructors
LDXX=		/usr/libexec/ld++

.cc.o:
	$(GXX) $(GXXFLAGS) -c $(.IMPSRC)
	@$(LDXX) -x -r $(.TARGET)
	@mv a.out $(.TARGET)

.cc.po:
	$(GXX) -p $(GXXFLAGS) -c $(.IMPSRC) -o $(.TARGET)
	@$(LDXX) -X -r $(.TARGET)
	@mv a.out $(.TARGET)

#
# Build the library.
#

LIB=		g++
GXXFLAGS+=	-I$(.CURDIR)/../g++-include -DHAVE_VPRINTF -DHAVE_VSCANF \
		-DHAVE_SETVBUF -DHAVE_SETLINEBUF -DHAVE_GETPAGESIZE
SRCS=		AllocRing.cc Obstack.cc File.cc ostream.cc istream.cc \
		streambuf.cc filebuf.cc Filebuf.cc open.o PlotFile.cc \
		SFile.cc builtin.cc regex.cc Regex.cc String.cc \
		Integer.cc Rational.cc Complex.cc Random.cc BitSet.cc \
		BitString.cc LogNorm.cc SmplHist.cc SmplStat.cc \
		Normal.cc NegExp.cc Weibull.cc Erlang.cc DiscUnif.cc \
		Uniform.cc Poisson.cc HypGeom.cc Geom.cc Binomial.cc \
		max.cc min.cc RNG.cc ACG.cc MLCG.cc RndInt.cc \
		Fix.cc Fix16.cc Fix24.cc CursesW.cc GetOpt.cc EH.cc \
		EH2.c xyzzy.cc gnulib3.c new.cc delete.cc  \
		chr.cc dtoa.cc error.cc form.cc gcd.cc hash.cc \
		itoa.cc lg.cc fmtq.cc ioob.cc pow.cc sqrt.cc str.cc \
		timer.cc compare.cc ctype.cc curses.cc math.cc std.cc
NOMAN=		noman

.include <bsd.lib.mk>

#
# For some reason, pmake won't cut it without this redundant declaration.
#
.SUFFIXES:
.SUFFIXES: .out .o .po .s .c .f .y .cc .l .8 .7 .6 .5 .4 .3 .2 .1 .0
