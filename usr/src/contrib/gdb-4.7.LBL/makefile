#Build for 4.4BSD

# Currently support 'hp300', 'luna68k', 'sparc', 'mips-dec' architectures.
all:
.if ${MACHINE} == "hp300"
	./configure hp300bsd -norecursion
	cd bfd && .././configure hp300bsd -target=hp300bsd -norecursion
	cd gdb && .././configure hp300bsd -target=hp300bsd -norecursion
	cd gdb && rm -f kgdb_proto.h && \
	    ln -s /sys/hp300/hp300/kgdb_proto.h kgdb_proto.h
	cd gdb && rm -f remote-sl.h && \
	    ln -s /sys/hp300/include/remote-sl.h remote-sl.h
.elif ${MACHINE} == "luna68k"
	./configure luna68k -norecursion
	cd bfd && \
	    .././configure m68-omron-bsd -target=m68-omron-bsd -norecursion
	cd gdb && \
	    .././configure m68-omron-bsd -target=m68-omron-bsd -norecursion
	cd gdb && rm -f kgdb_proto.h && \
	    ln -s /sys/luna68k/luna68k/kgdb_proto.h kgdb_proto.h
	cd gdb && rm -f remote-sl.h && \
	    ln -s /sys/luna68k/include/remote-sl.h remote-sl.h
.elif ${MACHINE} == "mips"
	./configure mips-dec -norecursion
	cd bfd && .././configure mips-dec -target=mips-dec -norecursion
	cd gdb && .././configure mips-dec -target=mips-dec -norecursion
	cd gdb && rm -f kgdb_proto.h && \
	    ln -s /sys/mips/mips/kgdb_proto.h kgdb_proto.h
	cd gdb && rm -f remote-sl.h && \
	    ln -s /sys/mips/include/remote-sl.h remote-sl.h
.elif ${MACHINE} == "sparc"
	./configure sparc-bsd -norecursion
	cd bfd && .././configure sparc-bsd -target=sparc-bsd -norecursion
	cd gdb && .././configure sparc-bsd -target=sparc-bsd -norecursion
	cd gdb && rm -f kgdb_proto.h && \
	    ln -s /sys/sparc/sparc/kgdb_proto.h kgdb_proto.h
	cd gdb && rm -f remote-sl.h && \
	    ln -s /sys/sparc/include/remote-sl.h remote-sl.h
.endif
	make -f Makefile

clean:
	make -f Makefile clean

install:
	make -f Makefile install
