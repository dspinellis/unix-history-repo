#
#  Makefile for INGRES EQUEL runtime library
#
#	@(#)Makefile	7.1	2/5/81
#

INGRES=	../..
H=	../h
LIB=	$(INGRES)/lib

OBJS=	IIw_left.o IIw_right.o IIcvar.o IIgettup.o IIn_get.o IIn_ret.o \
	IIingres.o IIretrieve.o IIsetup.o IIflushtup.o IIsync.o \
	IIwrite.o IIexit.o IIsyserr.o IIconvert.o IIerror.o IIp_err.o\
	IIgetc.o IIresync.o IIreadinp.o IIpb_get.o IIpb_put.o IIpb_read.o \
	IIpb_rphys.o IIpb_flush.o IIpb_write.o IIpb_wphys.o IIpb_prime.o \
	IIitos.o  IIconcatv.o IIbmove.o IIlength.o IIatoi.o IIsequal.o
SRCS=	IIw_left.c IIw_right.c IIcvar.c IIgettup.c IIn_get.c IIn_ret.c \
	IIingres.c IIretrieve.c IIsetup.c IIflushtup.c IIsync.c \
	IIwrite.c IIexit.c IIsyserr.c IIconvert.c IIerror.c IIp_err.c\
	IIgetc.c IIresync.c IIreadinp.c IIpb_get.c IIpb_put.c IIpb_read.c \
	IIpb_rphys.c IIpb_flush.c IIpb_write.c IIpb_wphys.c IIpb_prime.c \
	IIitos.c  IIconcatv.c IIbmove.c IIlength.c IIatoi.c IIsequal.c \
	IIglobals.h

SYSLIB=	/usr/local/lib

CFLAGS=	-I$H -I../ctlmod -O
AR=	-ar
ARFLAGS=rvu
RANLIB=	ranlib
CP=	cp
CHMOD=	chmod
CHOWN=	-chown
LIBMODE=664
GET=	sccs get
REL=
PR=	print
VGRIND=	csh /usr/ucb/vgrind
HEADER=	'Equel Runtime Library'

libq.a: $(OBJS)
	$(AR) $(ARFLAGS) $@ $?
	$(CHMOD) $(LIBMODE) libq.a
	$(RANLIB) libq.a

install: $(LIB)/libq.a

$(LIB)/libq.a: libq.a
	$(CP) libq.a $(LIB)/libq.a
	$(RANLIB) $(LIB)/libq.a

sysinstall: $(SYSLIB)/libq.a

$(SYSLIB)/libq.a: libq.a
	$(CP) libq.a $(SYSLIB)/libq.a
	$(CHOWN) ingres $(SYSLIB)/libq.a
	$(RANLIB) $(SYSLIB)/libq.a

sources: $(SRCS)

$(SRCS):
	$(GET) $(REL) SCCS/s.$@

new:
	rm -f libq.a

clean:
	rm -f *.o
	rm -f a.out core libq.a

print: sources
	$(PR) Makefile TraceFlags *.[hm] *.[csy]

vgrind: sources
	cp /dev/null index
	$(VGRIND) -h "$(HEADER) (Release 7)" -n Makefile TraceFlags
	$(VGRIND) -h "$(HEADER) (Release 7)" *.[hm] *.[csy]
	$(VGRIND) -h "$(HEADER) (Release 7)" -x index

depend:
	grep '^#[ 	]*include' $(SRCS) | sed -n -f ../h/depend.sed > makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

IIw_left.o:$H/ingres.h
IIw_left.o:../equel/constants.h
IIw_left.o:IIglobals.h
IIw_right.o:$H/ingres.h
IIw_right.o:../equel/constants.h
IIw_right.o:IIglobals.h
IIcvar.o:$H/ingres.h
IIcvar.o:$H/symbol.h
IIcvar.o:IIglobals.h
IIgettup.o:$H/ingres.h
IIgettup.o:$H/symbol.h
IIgettup.o:IIglobals.h
IIn_get.o:$H/ingres.h
IIn_get.o:$H/symbol.h
IIn_get.o:IIglobals.h
IIn_ret.o:$H/ingres.h
IIn_ret.o:$H/symbol.h
IIn_ret.o:IIglobals.h
IIingres.o:$H/ingres.h
IIingres.o:$H/symbol.h
IIingres.o:$H/opsys.h
IIingres.o:$H/aux.h
IIingres.o:IIglobals.h
IIretrieve.o:$H/ingres.h
IIretrieve.o:$H/symbol.h
IIretrieve.o:IIglobals.h
IIsetup.o:$H/ingres.h
IIsetup.o:$H/symbol.h
IIsetup.o:IIglobals.h
IIflushtup.o:$H/ingres.h
IIflushtup.o:$H/aux.h
IIflushtup.o:$H/symbol.h
IIflushtup.o:IIglobals.h
IIsync.o:$H/ingres.h
IIsync.o:$H/symbol.h
IIsync.o:IIglobals.h
IIwrite.o:$H/ingres.h
IIwrite.o:$H/symbol.h
IIwrite.o:IIglobals.h
IIexit.o:$H/ingres.h
IIexit.o:$H/symbol.h
IIexit.o:IIglobals.h
IIconvert.o:$H/ingres.h
IIconvert.o:$H/symbol.h
IIerror.o:$H/ingres.h
IIerror.o:$H/symbol.h
IIerror.o:IIglobals.h
IIp_err.o:$H/ingres.h
IIp_err.o:$H/aux.h
IIp_err.o:$H/version.h
IIp_err.o:IIglobals.h
IIgetc.o:$H/ingres.h
IIgetc.o:IIglobals.h
IIresync.o:$H/ingres.h
IIresync.o:IIglobals.h
IIreadinp.o:$H/useful.h
IIreadinp.o:$H/ingres.h
IIreadinp.o:$H/aux.h
IIreadinp.o:IIglobals.h
IIpb_get.o:$H/useful.h
IIpb_put.o:$H/useful.h
IIpb_read.o:$H/ingres.h
IIpb_read.o:IIglobals.h
IIpb_write.o:$H/ingres.h
IIpb_write.o:IIglobals.h
IIpb_prime.o:$H/symbol.h
IIglobals.h:$H/resp.h
IIglobals.h:$H/pv.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
