# Target: AMD 29000.
# These defines should give you a gdb running on ? (sun3os4 if you like)
# that will be able to communicate over a serial line with either an 
# EB board (remote-eb.c),
# Adapt (remote-adapt.c),
# or a MiniMon debugger (remote-mm.c).
# Or run native on an Ultracomputer.
TDEPFILES= exec.o am29k-pinsn.o am29k-tdep.o remote-eb.o remote-mm.o remote-adapt.o 
TM_FILE= tm-29k.h
MT_CFLAGS = -DNO_HIF_SUPPORT
