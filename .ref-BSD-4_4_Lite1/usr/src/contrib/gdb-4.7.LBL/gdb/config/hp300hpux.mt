# Target: Hewlett-Packard 9000 series 300, running HPUX

#msg Note that GDB can only read symbols from programs that were
#msg compiled with GCC
#msg

# The headers in the directory hp-include override system headers
# and tell GDB to use BSD executable file format (hence -Ihp-include)
MT_CFLAGS=-Ihp-include
TDEPFILES= exec.o m68k-pinsn.o
TM_FILE= tm-hp300hpux.h
