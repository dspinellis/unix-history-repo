# Target: Hewlett-Packard 9000 series 300, running BSD
TDEPFILES= exec.o m68k-pinsn.o m68k-tdep.o m68kbsd-tdep.o
TM_FILE= tm-hp300bsd.h
TM_CLIBS= -lkvm
MT_CFLAGS = -DKERNELDEBUG
