#	@(#)Makefile	5.6 (Berkeley) 5/20/91

SUBDIR=	bad144 cat compress cp disklabel elvis fsck \
	ls mkdir mknod mount mv newfs \
	rm rsh stty sync umount

.if make(clean) || make(cleandir)
SUBDIR+=bin
.endif

install:
	for i in ${SUBDIR}; do \
		mv $$i/obj/$$i bin/$$i; \
	done

.include <bsd.subdir.mk>

