# Target: Sun 4 or Sparcstation, running SunOS 4
TDEPFILES= exec.o sparc-tdep.o sparc-tcmn.o sparc-pinsn.o solib.o
TM_FILE= tm-sun4os4.h
TM_CLIBS= -lkvm		# XXX this is a native thing
MT_CFLAGS= -DKERNELDEBUG
