TROFF=	csh /usr/ucb/vtroff
TBL= csh /usr/bin/tbl
VGRIND=	csh /usr/ucb/vgrind

dstruct:
	@${TROFF} -ms dstruct.t
		proc.h pte.h text.h user.h vm.h

setup:
	@${TROFF} -ms setup.t

design:
	/usr/bin/tbl design.t | /usr/ucb/vtroff -ms

h:
	@cd /usr/include/sys ; ${VGRIND} cmap.h dmap.h inode.h param.h pcb.h \
