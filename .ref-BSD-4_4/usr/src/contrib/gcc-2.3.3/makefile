#Build for 4.4BSD

all:

# Currently support 'hp300', 'luna68k', 'sparc', 'mips-dec' architectures.

	rm -f tm.h
.if ${MACHINE} == "hp300"
	ln -s config/hp3bsd.h tm.h
.elif ${MACHINE} == "luna68k"
	ln -s config/luna68k.h tm.h
.elif ${MACHINE} == "mips"
	ln -s config/mips-dec-bsd.h tm.h
.elif ${MACHINE} == "sparc"
	ln -s config/sparcbsd.h tm.h
.endif

	make -f Makefile LANGUAGES="c"
	make -f Makefile stage1
	make -f Makefile "CC=stage1/xgcc -Bstage1/" LANGUAGES="c"
	make -f Makefile stage2
	make -f Makefile "CC=stage2/xgcc -Bstage2/" "CFLAGS+=-O2" \
	    LANGUAGES="c c++"

clean:
	make -f Makefile clean
	rm -rf stage1 stage2
	rm -f cccp.0 g++.0 gcc.0

install:
	make -f Makefile install
