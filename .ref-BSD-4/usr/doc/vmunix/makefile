TROFF=	csh /usr/ucb/vtroff
TBL= /usr/bin/tbl
VGRIND=	csh /usr/ucb/vgrind

dstruct:
	@${TROFF} -ms dstruct.t
		proc.h pte.h text.h user.h vm.h

setup:
	@${TBL} setup.t | ${TROFF} -ms

design:
	@${TBL} design.t | ${TROFF} -ms

h:
	@cd /usr/include/sys ; ${VGRIND} cmap.h dmap.h inode.h param.h pcb.h \

