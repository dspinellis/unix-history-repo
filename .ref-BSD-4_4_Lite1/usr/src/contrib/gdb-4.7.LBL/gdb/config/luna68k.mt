# Target: OMRON LUNA-I/II, running BSD
TDEPFILES= exec.o m68k-pinsn.o m68k-tdep.o m68kbsd-tdep.o
TM_FILE= tm-luna68k.h
TM_CLIBS= -lkvm
MT_CFLAGS = -DKERNELDEBUG
