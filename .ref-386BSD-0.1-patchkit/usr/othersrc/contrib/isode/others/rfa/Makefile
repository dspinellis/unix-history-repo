###############################################################################
# 
#  RFA - Remote File Access
# 
#  Access and Management for a partial file system tree that exists
#  at two sites either as master files or slave files
# 
#  Makefile
# 
#  Contributed by Oliver Wenzel, GMD Berlin, 1990
# 
#
###############################################################################

###############################################################################
#
# $Header: /f/osi/others/rfa/RCS/Makefile,v 7.4 91/02/22 09:27:45 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.4  91/02/22  09:27:45  mrose
# Interim 6.8
# 
# Revision 7.3  91/01/14  13:54:20  mrose
# update
# 
# Revision 1.1  91/01/04  16:11:05  ow
# Initial revision
#
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


###############################################################################
# Generation Rules for program modules
###############################################################################

PEPYPATH=	-DPEPYPATH

.SUFFIXES:	.ry .py .c .o

.c.o:;		$(CC) $(CFLAGS) -c $*.c


LIBES   =	librfa.a $(LIBDIR)libisode$(LPP).a
LLIBS   =	$(ISODE)llib-lisode$(LPP)
HFILES	=	$(HDIR)rosy.h $(HDIR)rosap.h $(HDIR)acsap.h $(HDIR)psap2.h \
		$(HDIR)psap.h $(HDIR)ssap.h $(HDIR)isoaddrs.h \
		$(HDIR)manifest.h $(HDIR)general.h $(HDIR)config.h
RFAOBJS	=	ls.o dirname.o rfainfo.o sync.o rfa2fi.o getfile.o printerr.o\
		error.o tailor.o advise.o filemode.o
RFADOBJS=	reqmaster.o filedata.o fileinfo.o dirname.o synctime.o \
		rfa2fi.o rfainfo.o error.o ls.o advise.o tailor.o filemode.o
LLOCKOBJS =	rfainfo.o dirname.o tailor.o ls.o advise.o $(LIBES)

ROS	=	ros.

ROSY	=	$(BINDIR)/rosy
POSY	=	$(BINDIR)/posy
PEPY	=	$(BINDIR)/pepy


##################################################################
# Here it is...
##################################################################

all:		rfad rfa rfatime llock
inst-all:	inst-rfad inst-rfa inst-rfatime inst-llock manuals inst-tailor
install:	inst-all clean
lint:		l-rfad l-rfa l-rfatime l-llock

all-lpp:;	$(MAKE) LPP=-lpp all
inst-lpp:;	$(MAKE) LPP=-lpp ROS=lpp. inst-all
install-lpp:;	$(MAKE) LPP=-lpp ROS=lpp. install
lint-lpp:;	$(MAKE) LPP=-lpp lint


##################################################################
# llock
##################################################################

inst-llock:	$(BINDIR)llock

$(BINDIR)llock:	xllock
		-cp $@ zllock
		-rm -f $@
		-rm -f $(BINDIR)lunlock
		cp xllock $@
		ln $@ $(BINDIR)lunlock 
		-@ls -gls $@
		-@echo ""

llock:		xllock

xllock:		llock.o $(LLOCKOBJS)
		$(LDCC) $(LDFLAGS) -o $@ llock.o $(LLOCKOBJS) 

l-llock:;	$(LINT) $(LFLAGS) llock.c \
			| grep -v "warning: possible pointer alignment problem"


##################################################################
# rfatime
##################################################################

inst-rfatime:	$(SBINDIR)rfatime

$(SBINDIR)rfatime:	xrfatime
		-cp $@ zrfatime
		-rm -f $@
		cp xrfatime $@
		chown root $@
		chmod u+s $@
		-@ls -gls $@
		-@echo ""

rfatime:	xrfatime

xrfatime:	rfatime.o 
		$(LDCC) $(LDFLAGS) -o $@ rfatime.o $(LIBES) 

l-rfatime:;	$(LINT) $(LFLAGS) rfatime.c \
			| grep -v "warning: possible pointer alignment problem"


##################################################################
# rfad
##################################################################

inst-rfad:	$(SBINDIR)$(ROS)rfa

$(SBINDIR)$(ROS)rfa:	xrfad$(LPP)
		-cp $@ z$(ROS)rfa
		-rm -f $@
		cp xrfad$(LPP) $@
		chown root $@
		chmod u+s $@
		-@ls -gls $@
		-@echo ""

rfad:		xrfad$(LPP)

xrfad$(LPP):	rfad.o RFA-Rops.o ryresponder.o $(RFADOBJS) $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ rfad.o $(RFADOBJS) RFA-Rops.o \
			ryresponder.o $(LIBES) $(LSOCKET)

l-rfad:		RFA-ops.c RFA-C true
		$(LINT) $(LFLAGS) -DPERFORMER rfad.c RFA-ops.c \
			ryresponder.c $(RFA-C) $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

rfad.o:		ryresponder.h RFA-ops.h RFA-types.h $(HFILES) $(HDIR)logger.h

RFA-Rops.o:	RFA-ops.c RFA-ops.h $(HFILES)
		$(CC) $(CFLAGS) -DPERFORMER -c RFA-ops.c
		mv RFA-ops.o $@

ryresponder.o:	ryresponder.h $(HFILES) $(HDIR)tailor.h


##################################################################
# rfa
##################################################################

inst-rfa:	$(BINDIR)rfa$(LPP)

$(BINDIR)rfa$(LPP):	xrfa$(LPP)
		-cp $@ zxrfa$(LPP)
		-rm -f $@
		cp xrfa$(LPP) $@
		chown root $@
		chmod u+s $@
		-@ls -gls $@
		-@echo ""

rfa:		xrfa$(LPP)

xrfa$(LPP):	rfa.o RFA-Iops.o ryinitiator.o $(RFAOBJS) $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ rfa.o $(RFAOBJS) RFA-Iops.o \
			ryinitiator.o $(LIBES) $(LSOCKET)

l-rfa:		RFA-ops.c RFA-C true
		$(LINT) $(LFLAGS) -DINVOKER rfa.c RFA-ops.c \
			ryinitiator.c $(RFA-C) RFA-stubs.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

rfa.o:		RFA-ops.h RFA-types.h $(HFILES)

RFA-Iops.o:	RFA-ops.c RFA-ops.h $(HFILES)
		$(CC) $(CFLAGS) -DINVOKER -c RFA-ops.c
		mv RFA-ops.o $@

ryinitiator.o:	$(HFILES)


################################################################
# librfa
################################################################

librfa.a:	RFA-O
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(RFA-O) 
		-@ls -l $@
		-@echo "RFA library built normally"

RFA-O	=	RFA-[0-9]*.o
RFA-C	=	RFA-[0-9]*.c

RFA-O:		RFA-C
		@$(MAKE) `/bin/ls $(RFA-C) | sed 's/\.c$$/.o/'`
		-@touch $@

RFA-C:		RFA-types.py $(PEPY)
		-@rm -f $(RFA-C) $(RFA-O)
		$(PEPY) -a PY_advise -m -A -b RFA $(PYFLAGS) RFA-types.py
		-@touch $@

RFA-types.py:	RFA-asn.py $(POSY)
		$(POSY) -f -h -m -o $@ $(POFLAGS) RFA-asn.py

RFA-types.h:	RFA-types.py

RFA-asn.py:	rfa.ry $(ROSY)
		$(ROSY) -m $(RYFLAGS) -o $@ rfa.ry

RFA-ops.c:	rfa.ry
RFA-ops.h:	rfa.ry
RFA-stubs.c:	rfa.ry


################################################################
# manual pages
################################################################

MANUALS	=	rfad.8c rfa.1c

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# tailoring file
################################################################

inst-tailor:	$(ETCDIR)rfatailor

$(ETCDIR)rfatailor:	rfatailor
		-mv $@ $(ETCDIR)rfatailor.old
		cp rfatailor $@

################################################################
# clean
################################################################

clean:;		rm -f *.o *.a RFA* x* z* _* core

true:;
