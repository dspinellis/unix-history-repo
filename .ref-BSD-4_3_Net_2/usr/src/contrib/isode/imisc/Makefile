###############################################################################
#   Instructions to Make, for compilation of ROS-based miscellaneous services
###############################################################################

###############################################################################
#
# $Header: /f/osi/imisc/RCS/Makefile,v 7.5 91/02/22 09:26:12 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.5  91/02/22  09:26:12  mrose
# Interim 6.8
# 
# Revision 7.4  90/12/23  18:41:53  mrose
# update
# 
# Revision 7.3  90/07/09  14:38:43  mrose
# sync
# 
# Revision 7.2  90/07/01  21:03:59  mrose
# pepsy
# 
# Revision 7.0  89/11/23  21:57:34  mrose
# Release 6.0
# 
###############################################################################

###############################################################################
#
#				 NOTICE
#
#    Acquisition, use, and distribution of this module and related
#    materials are subject to the restrictions of a license agreement.
#    Consult the Preface in the User's Manual for the full terms of
#    this agreement.
#
###############################################################################


PEPYPATH=	-DPEPYPATH

.SUFFIXES:	.ry .py .c .o

.c.o:;		$(CC) $(CFLAGS) -c $*.c


LIBES   =	$(TOPDIR)libisode$(LPP).a
LLIBS   =	$(TOPDIR)llib-lisode$(LPP)
CFILES	=	imiscd.c imisc.c IMISC-ops.c ryinitiator.c ryresponder.c
RYFILES	=	imisc.ry
HFILES	=	$(HDIR)rosy.h $(HDIR)rosap.h $(HDIR)acsap.h $(HDIR)psap2.h \
		$(HDIR)psap.h $(HDIR)ssap.h $(HDIR)isoaddrs.h \
		$(HDIR)manifest.h $(HDIR)general.h $(HDIR)config.h

ROS	=	ros.


##################################################################
# Here it is...
##################################################################

all:		imiscd imisc
inst-all:	inst-imiscd inst-imisc manuals
install:	inst-all clean
lint:		l-imiscd l-imisc

all-lpp:;	$(MAKE) LPP=-lpp all
inst-lpp:;	$(MAKE) LPP=-lpp ROS=lpp. inst-all
install-lpp:;	$(MAKE) LPP=-lpp ROS=lpp. install
lint-lpp:;	$(MAKE) LPP=-lpp lint


##################################################################
# imiscd
##################################################################

inst-imiscd:	$(SBINDIR)$(ROS)imisc

$(SBINDIR)$(ROS)imisc:	ximiscd$(LPP)
		-cp $@ z$(ROS)imisc
		-rm -f $@
		cp ximiscd$(LPP) $@
		-@ls -gls $@
		-@echo ""

imiscd:		ximiscd$(LPP)

ximiscd$(LPP):	imiscd.o IMISC-Rops.o ryresponder.o IMISC_tables.o $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ imiscd.o IMISC-Rops.o ryresponder.o \
			IMISC_tables.o $(LIBES) $(LSOCKET)

l-imiscd:	IMISC-ops.c IMISC_tables.c true
		$(LINT) $(LFLAGS) -DPERFORMER imiscd.c IMISC-ops.c \
			ryresponder.c IMISC_tables.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

imiscd.o:	ryresponder.h IMISC-ops.h IMISC-types.h $(HFILES) \
			$(HDIR)logger.h

IMISC-Rops.o:	IMISC-ops.c IMISC-ops.h $(HFILES)
		$(CC) $(CFLAGS) -DPERFORMER -c IMISC-ops.c
		mv IMISC-ops.o $@

ryresponder.o:	ryresponder.h $(HFILES) $(HDIR)tailor.h


##################################################################
# imisc
##################################################################

inst-imisc:	$(BINDIR)imisc$(LPP)

$(BINDIR)imisc$(LPP):	ximisc$(LPP)
		-cp $@ zximisc$(LPP)
		-rm -f $@
		cp ximisc$(LPP) $@
		-@ls -gls $@
		-@echo ""

imisc:		ximisc$(LPP)

ximisc$(LPP):	imisc.o IMISC-Iops.o ryinitiator.o IMISC_tables.o $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ imisc.o IMISC-Iops.o ryinitiator.o \
			IMISC_tables.o $(LIBES) $(LSOCKET)

l-imisc:	IMISC-ops.c IMISC_tables.c true
		$(LINT) $(LFLAGS) -DINVOKER imisc.c IMISC-ops.c \
			ryinitiator.c IMISC_tables.c IMISC-stubs.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

imisc.o:	ryinitiator.h IMISC-ops.h IMISC-types.h $(HFILES)

IMISC-Iops.o:	IMISC-ops.c IMISC-ops.h $(HFILES)
		$(CC) $(CFLAGS) -DINVOKER -c IMISC-ops.c
		mv IMISC-ops.o $@

ryinitiator.o:	ryinitiator.h $(HFILES)


################################################################
# libimisc
################################################################

IMISC_tables.o:	IMISC_tables.c IMISC-types.h

IMISC_tables.c IMISC-types.h: IMISC-asn.py $(TOPDIR)pepsy/xpepsy
		$(TOPDIR)pepsy/xpepsy -A -f -h -m IMISC-asn.py

IMISC-asn.py:	imisc.ry $(TOPDIR)rosy/xrosy
		$(TOPDIR)rosy/xrosy -m -pepsy -o $@ imisc.ry
IMISC-ops.c:	imisc.ry
IMISC-ops.h:	imisc.ry
IMISC-stubs.c:	imisc.ry


################################################################
# manual pages
################################################################

MANUALS	=	imiscd.8c imisc.1c

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# clean
################################################################

clean:;		rm -f *.ph *.o *.a IMISC* x* z* _* core

grind:;		iprint Makefile
		tgrind -lc ryinitiator.h ryresponder.h $(CFILES)
		tgrind -lpepy -d $(TOPDIR)pepy/grindefs $(RYFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;
