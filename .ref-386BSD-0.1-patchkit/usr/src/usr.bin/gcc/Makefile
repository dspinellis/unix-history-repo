#	@(#)Makefile	6.1 (Berkeley) 1/29/91
#
# PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
# --------------------         -----   ----------------------
# CURRENT PATCH LEVEL:         1       00119
# --------------------         -----   ----------------------
#
# 30 Mar 93	Rodney W. Grimes	Added special rule for make depend
#

SUBDIR=	cc cpp cc1 gnulib

# special top level rule due to symbolic links needed for this to work
#
depend:
	cd $(.CURDIR)/cc; make config.h; make depend
	cd $(.CURDIR)/cc1; make config.h; make depend
	cd $(.CURDIR)/cpp; make config.h; make depend
	cd $(.CURDIR)/gnulib; make config.h; make depend

.include <bsd.subdir.mk>
