# Target: Intel 386 running BSD
TDEPFILES= i386-tdep.o i386bsd-tdep.o i386-pinsn.o
MT_CFLAGS= -DKERNELDEBUG
TM_FILE= tm-i386bsd.h
TM_CLIBS= -lkvm
