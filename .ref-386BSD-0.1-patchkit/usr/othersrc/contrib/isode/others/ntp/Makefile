###############################################################################
#   Instructions to Make, for compilation of ISODE NTP over ROS
###############################################################################

###############################################################################
#
# $Header: /f/osi/others/ntp/RCS/Makefile,v 7.2 91/02/22 09:33:33 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.2  91/02/22  09:33:33  mrose
# Interim 6.8
# 
# Revision 7.1  90/12/23  18:44:45  mrose
# update
# 
# Revision 7.0  90/12/10  17:21:14  mrose
# *** empty log message ***
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

.c.o:;		$(CC) $(CFLAGS) -c $*.c


###############################################################################
# Programs and Libraries
###############################################################################

LIBES	=	$(TOPDIR)libisode.a
LLIBS   =	$(TOPDIR)llib-lisode


###############################################################################
# Files
###############################################################################

HFILES	=	ntp-config.h ntp.h patchlevel.h
CFILES	=	ntp.c ntp.c ntp_adjust.c ntp_osi.c ntp_proto.c ntp_sock.c \
		ntpd.c ntpdc.c ntpsubs.c ntq.c read_local.c read_psti.c test.c
RYFILES	=	ntp.ry


##################################################################
# Here it is...
##################################################################

all:		ntpd ntp ntpdc ntq ntest
inst-all:	inst-ntpd inst-ntp inst-ntpdc inst-ntq manuals
install:	inst-all clean
lint: 		l-ntpd l-ntp l-ntpdc l-ntq


#################################################################
# ntpd
#################################################################

NTPDOBJ= ntpd.o ntpsubs.o ntp_proto.o ntp_sock.o ntp_adjust.o read_local.o \
	read_psti.o ntp_osi.o NTP-ops.o NTP-stubs.o NTP-types.o NTP-print.o
NTPDSRC= ntpd.o ntpsubs.o ntp_proto.o ntp_sock.o ntp_adjust.o read_local.o \
	read_psti.o ntp_osi.o NTP-ops.o NTP-stubs.o NTP-types.o NTP-print.o


inst-ntpd:	$(SBINDIR)ntpd

$(SBINDIR)ntpd:	xntpd
		-cp $@ zntpd
		-rm -f $@
		cp xntpd $@
		-@ls -gls $@
		-@echo ""

ntpd:	xntpd

xntpd:	${NTPDOBJ}
	$(CC) $(LDFLAGS) -o $@ ${NTPDOBJ} $(LIBES)

l-ntpd: 	$(NTPDSRC)
		$(LINT) $(LINTFLAGS) ${DEFINES} ${FEATURES} ${INCPATH} \
		${NTPDSRC} ${LLIBS} \
			| grep -v "warning: possible pointer alignment problem"


#################################################################
# ntp
#################################################################

NTPOBJ	= ntp.o ntpsubs.o
NTPSRC	= ntp.c ntpsubs.c


inst-ntp:	$(BINDIR)ntp

$(BINDIR)ntp:	xntp
		-cp $@ zntp
		-rm -f $@
		cp xntp $@
		-@ls -gls $@
		-@echo ""

ntp:		xntp

xntp:		$(NTPOBJ)
		$(LDCC) $(LDFLAGS) -o $@ $(NTPOBJ) $(LIBES) $(LSOCKET)

l-ntp: 		$(NTPSRC)
		$(LINT) $(LFLAGS) $(NTPSRC) $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


#################################################################
# ntpdc
#################################################################

NTPDCOBJ	= ntpdc.o
NTPDCSRC	= ntpdc.c


inst-ntpdc:	$(SBINDIR)ntpdc

$(SBINDIR)ntpdc:	xntpdc
		-cp $@ zntpdc
		-rm -f $@
		cp xntpdc $@
		-@ls -gls $@
		-@echo ""

ntpdc:		xntpdc

xntpdc: 	$(NTPDCOBJ)
		$(LDCC) $(LDFLAGS) -o $@ $(NTPDCOBJ) $(LIBES) $(LSOCKET)

l-ntpdc: 	$(NTPDCSRC)
		$(LINT) $(LFLAGS) $(NTPDCSRC) $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


#################################################################
# ntq
#################################################################

NTQOBJ	= NTP-types.o NTP-stubs.o NTP-ops.o ntq.o
NTQSRC	= NTP-types.c NTP-stubs.c NTP-ops.c ntq.c


inst-ntq:	$(BINDIR)ntq

$(BINDIR)ntq:	xntq
		-cp $@ zntq
		-rm -f $@
		cp xntq $@
		-@ls -gls $@
		-@echo ""

ntq:	xntq

xntq: 		$(NTQOBJ)
		$(LDCC) $(LDFLAGS) -o $@ $(NTQOBJ) $(LIBES) $(LSOCKET)

l-ntq: 		$(NTPQSRC)
		$(LINT) $(LFLAGS) $(NTPQSRC) $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


#################################################################
# misc
#################################################################

ntest:		test.o ntpsubs.o
		$(LDCC) $(LDFLAGS) -o ntest test.o ntpsubs.o $(LIBES)

sock_test:	ntp_sock.c
		$(LDCC) $(LDFLAGS) -DTEST -o sock_test ntp_sock.c $(LIBES)


################################################################
# manual pages
################################################################

MANUALS	=	ntp.8 ntpd.8 ntpdc.8

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# clean
################################################################

clean:;		rm -f *.o *.a NTP* x* ntest sock_test z* _* core

grind:;		iprint READ-ME Makefile
		tgrind -lc $(HFILES) $(CFILES)
		tgrind -lpepy -d $(TOPDIR)pepy/grindefs $(RYFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;


#################################################################
# dependencies
#################################################################

ntp.o:		ntp.h ntp-config.h ntp.c
ntp_adjust.o:	ntp.h ntp-config.h ntp_adjust.c
ntp_osi.o:	ntp.h ntp-config.h NTP-ops.h NTP-types.h ntp_osi.c
ntp_proto.o:	ntp.h ntp-config.h ntp_proto.c
ntp_sock.o:	ntp.h ntp-config.h ntp_sock.c
ntpd.o:		ntp.h ntp-config.h patchlevel.h ntpd.c
ntpdc.o:	ntp.h ntp-config.h ntpdc.c
ntpsubs.o:	ntp.h ntp-config.h ntpsubs.c
read_local.o:	ntp-config.h read_local.c
read_psti.o:	ntp-config.h read_psti.c
test.o:		ntp.h ntp-config.h test.c

NTP-ops.o:	NTP-ops.c NTP-ops.h NTP-types.h
	$(CC) $(CFLAGS) -c -DPERFORMER -DINVOKER NTP-ops.c
NTP-print.c: NTP-asn.py
	$(TOPDIR)pepy/xpepy -S PRINT -o $@ -a PY_advise NTP-asn.py
NTP-types.py: NTP-asn.py
	$(TOPDIR)pepy/xposy -f -h -o $@ NTP-asn.py

NTP-types.h: NTP-types.py
NTP-asn.py: ntp.ry
	$(TOPDIR)rosy/xrosy -o $@ ntp.ry
NTP-ops.c: ntp.ry
NTP-ops.h: ntp.ry
NTP-stubs.c: ntp.ry
NTP-types.o: NTP-types.c NTP-types.h
NTP-types.c: NTP-types.py
	$(TOPDIR)pepy/xpepy -a PY_advise -A NTP-types.py
