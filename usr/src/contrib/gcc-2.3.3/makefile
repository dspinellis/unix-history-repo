#Build for 4.4BSD

# Currently support 'hp300', 'luna68k', 'sparc', 'mips-dec' architectures.
all:
.if ${MACHINE} == "hp300"
	rm -f aux-output.c; ln -s config/m68k.c aux-output.c
	rm -f md; ln -s config/m68k.md md
	rm -f tconfig.h; ln -s config/xm-m68k.h tconfig.h
	rm -f tm.h; ln -s config/hp3bsd.h tm.h
.elif ${MACHINE} == "luna68k"
	rm -f aux-output.c; ln -s config/m68k.c aux-output.c
	rm -f md; ln -s config/m68k.md md
	rm -f tconfig.h; ln -s config/xm-m68k.h tconfig.h
	rm -f tm.h; ln -s config/luna68k.h tm.h
.elif ${MACHINE} == "mips"
	rm -f aux-output.c; ln -s config/mips.c aux-output.c
	rm -f md; ln -s config/mips.md md
	rm -f tconfig.h; ln -s config/xm-mips.h tconfig.h
	rm -f tm.h; ln -s config/mips-dec-bsd.h tm.h
.elif ${MACHINE} == "sparc"
	rm -f aux-output.c; ln -s config/sparc.c aux-output.c
	rm -f md; ln -s config/sparc.md md
	rm -f tconfig.h; ln -s config/xm-sparc.h tconfig.h
	rm -f tm.h; ln -s config/sparcbsd.h tm.h
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
