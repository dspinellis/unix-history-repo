# Target: VxWorks running on an Intel 960
TDEPFILES= exec.o i960-pinsn.o i960-tdep.o remote-vx.o xdr_ld.o xdr_ptrace.o xdr_rdb.o xdr_regs.o
TM_FILE= tm-vx960.h
# Define this for the vx-share routines, which don't see param.h.
MT_CFLAGS= -DI80960
