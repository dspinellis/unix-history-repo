ntest.$(OBJ): ntest.c

nalloc.$(OBJ): nalloc.c std.h nstd.h nalloc.h nmman.h

nsave.$(OBJ): nsave.c ghost.h nstd.h npacked.h nalloc.h nsave.h nmman.h

ngc.$(OBJ): ngc.c ghost.h nstd.h dict.h estack.h ostack.h npacked.h store.h nalloc.h nsave.h nmman.h

NALL=ntest.$(OBJ) nalloc.$(OBJ)       # nsave.$(OBJ) ngc.$(OBJ)
ntest.exe: $(NALL)
	tlink /m /l c:\bc\lib\c0l $(NALL),ntest,ntest,@libcl.tr
