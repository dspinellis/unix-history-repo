# Target: Sun 4 or Sparcstation, running BSD Reno
TDEPFILES= exec.o sparcbsd-tdep.o sparc-tcmn.o sparc-pinsn.o
TM_FILE= tm-sun4bsd.h
TM_CLIBS= -lkvm
MT_CFLAGS= -DKERNELDEBUG
