###############################################################################
#   Instructions to Make, for compilation of 4BSD/ISODE SNMP software
###############################################################################

###############################################################################
#
# $Header: /f/osi/snmp/RCS/Makefile,v 7.37 91/03/09 11:57:21 mrose Exp $
#
# Contributed by NYSERNet Inc.  This work was partially supported by the
# U.S. Defense Advanced Research Projects Agency and the Rome Air Development
# Center of the U.S. Air Force Systems Command under contract number
# F30602-88-C-0016.
#
#
# $Log:	Makefile,v $
# Revision 7.37  91/03/09  11:57:21  mrose
# update
# 
# Revision 7.36  91/02/22  09:42:47  mrose
# Interim 6.8
# 
# Revision 7.35  91/01/13  11:05:37  mrose
# update
# 
# Revision 7.34  91/01/11  15:34:16  mrose
# sets
# 
# Revision 7.33  90/12/23  18:42:57  mrose
# update
# 
# Revision 7.32  90/12/18  10:22:13  mrose
# one-more-time
# 
# Revision 7.31  90/12/18  10:13:04  mrose
# update
# 
# Revision 7.30  90/12/17  22:09:18  mrose
# split-up
# 
# Revision 7.29  90/11/20  15:31:55  mrose
# update
# 
# Revision 7.28  90/09/28  09:53:15  mrose
# tokenring
# 
# Revision 7.27  90/09/27  15:48:30  mrose
# ethernet
# 
# Revision 7.26  90/09/27  10:48:28  mrose
# t1,tokenbus
# 
# Revision 7.25  90/09/26  21:16:39  mrose
# appletalk
# 
# Revision 7.24  90/09/26  19:23:00  mrose
# new-mibs
# 
# Revision 7.23  90/08/30  15:11:06  mrose
# ho-hum
# 
# Revision 7.22  90/08/30  01:32:16  mrose
# fddi
# 
# Revision 7.21  90/08/29  19:28:04  mrose
# touch-up
# 
# Revision 7.20  90/08/08  14:00:55  mrose
# stuff
# 
# Revision 7.19  90/07/09  14:48:31  mrose
# sync
# 
# Revision 7.18  90/07/01  21:07:24  mrose
# pepsy
# 
# Revision 7.16  90/06/21  21:25:59  mrose
# snmpt
# 
# Revision 7.15  90/03/24  10:53:58  mrose
# update
# 
# Revision 7.14  90/03/08  08:05:37  mrose
# isnmp
# 
# Revision 7.13  90/03/06  13:56:47  mrose
# touch-up
# 
# Revision 7.12  90/02/27  18:49:25  mrose
# unix stuff
# 
# Revision 7.11  90/02/19  20:45:03  mrose
# touch-up
# 
# Revision 7.10  90/02/19  16:43:56  mrose
# again
# 
# Revision 7.9  90/02/19  16:25:51  mrose
# typo
# 
# Revision 7.8  90/02/19  15:38:40  mrose
# one more time
# 
# Revision 7.7  90/02/17  17:18:46  mrose
# touch-up
# 
# Revision 7.6  90/02/17  10:37:33  mrose
# smux
# 
# Revision 7.5  89/12/20  16:02:43  mrose
# clns
# 
# Revision 7.4  89/12/19  16:18:20  mrose
# dgram
# 
# Revision 7.3  89/12/17  18:30:09  mrose
# foo
# 
# Revision 7.2  89/12/08  21:46:43  mrose
# ouch-up
# 
# Revision 7.1  89/12/01  10:42:12  mrose
# clts
# 
# Revision 7.0  89/11/23  22:22:54  mrose
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

.SUFFIXES:	.py .c .o

.c.o:;		$(CC) $(CFLAGS) -c $*.c


LIBES   =	libisnmp.a $(TOPDIR)libisode.a
LLIBS   =	$(TOPDIR)llib-lisode
MIB-C	=	$(AE-C) smux-g.c \
			system.c interfaces.c routes.c ip.c icmp.c tcp.c \
			udp.c clns.c 
AE-C	=	mib.c snmp-g.c view-g.c eval.c
MIB-O	=	$(AE-O) smux-g.o \
			system.o interfaces.o routes.o ip.o icmp.o tcp.o \
			udp.o clns.o
AE-O	=	mib.o snmp-g.o view-g.o eval.o
CFILES	=	snmpd.c unixd.c snmpi.c objects.c syntax.c values.c \
			smux.c smuxentry.c $(MIB-C)
PYFILES	=	snmp.py
HFILES	=	$(HDIR)psap.h $(HDIR)manifest.h $(HDIR)general.h \
		$(HDIR)config.h


##################################################################
# Here it is...
##################################################################

all:		snmpd snmpc snmpt unixd snmpi runcom peers objects
inst-all:	inst-snmpd inst-snmpt inst-unixd inst-snmpi \
			inst-runcom inst-peers inst-objects inst-headers \
			inst-libisnmp manuals # inst-snmpc
install:	inst-all clean
lint:		l-snmpd l-snmpc l-snmpt l-unixd l-snmpi


##################################################################
# snmpd
##################################################################

inst-snmpd:	$(SBINDIR)snmpd

$(SBINDIR)snmpd: xsnmpd
		-cp $@ zsnmpd
		-rm -f $@
		cp xsnmpd $@
		-@ls -gls $@
		-@echo ""

snmpd:		xsnmpd

xsnmpd:		snmpd.o $(MIB-O) $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ snmpd.o $(MIB-O) $(LIBES) \
			$(LNLIST) $(LSOCKET)

l-snmpd:	SNMP_tables.c true
		$(LINT) $(LFLAGS) snmpd.c $(MIB-C) \
			SNMP_tables.c objects.c syntax.c values.c \
			smux.c smuxentry.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

snmpd.o:	SNMP-types.h mib.h objects.h smux.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h \
			$(HDIR)tsap.h $(HDIR)isoaddrs.h \
			$(HDIR)dgram.h $(HDIR)internet.h $(HDIR)tp4.h \
			snmp-g.h view-g.h smux-g.h
$(MIB-O):	SNMP-types.h mib.h objects.h $(HFILES) $(HDIR)logger.h
system.o:	$(HDIR)tailor.h $(HDIR)logger.h
interfaces.o:	interfaces.h $(HDIR)internet.h clns.h
addrtrans.o:	interfaces.h $(HDIR)internet.h clns.h
ip.o:		interfaces.h $(HDIR)internet.h clns.h routes.h
icmp.o:		$(HDIR)internet.h
tcp.o:		$(HDIR)internet.h
udp.o:		$(HDIR)internet.h
clns.o:		interfaces.h $(HDIR)internet.h clns.h routes.h
snmp-g.o:	snmp-g.h
view-g.o:	view-g.h $(HDIR)isoaddrs.h $(HDIR)internet.h
smux-g.o:	smux-g.h $(HDIR)internet.h


##################################################################
# snmpc
##################################################################

inst-snmpc:	$(SBINDIR)snmpc

$(SBINDIR)snmpc: xsnmpc
		-cp $@ zsnmpc
		-rm -f $@
		cp xsnmpc $@
		-@ls -gls $@
		-@echo ""

snmpc:		xsnmpc

xsnmpc:		snmpc.o $(AE-O) $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ snmpc.o $(AE-O) $(LIBES) \
			$(LNLIST) $(LSOCKET)

l-snmpc:	SNMP_tables.c true
		$(LINT) $(LFLAGS) -DSNMPC snmpd.c $(AE-C) \
			SNMP_tables.c objects.c syntax.c values.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

snmpc.o:	SNMP-types.h mib.h objects.h smux.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h \
			$(HDIR)tsap.h $(HDIR)isoaddrs.h \
			$(HDIR)dgram.h $(HDIR)internet.h $(HDIR)tp4.h \
			snmpd.c
		$(CC) $(CFLAGS) -DSNMPC -o snmpc.o -c snmpd.c


##################################################################
# snmpt
##################################################################

inst-snmpt:	$(SBINDIR)snmpt

$(SBINDIR)snmpt: xsnmpt
		-cp $@ zsnmpt
		-rm -f $@
		cp xsnmpt $@
		-@ls -gls $@
		-@echo ""

snmpt:		xsnmpt

xsnmpt:		snmpt.o $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ snmpt.o $(LIBES) $(LSOCKET)

l-snmpt:	SNMP_tables.c true
		$(LINT) $(LFLAGS) -DSNMPT snmpd.c SNMP_tables.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

snmpt.o:	SNMP-types.h mib.h objects.h smux.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h \
			$(HDIR)tsap.h $(HDIR)isoaddrs.h \
			$(HDIR)dgram.h $(HDIR)internet.h $(HDIR)tp4.h \
			snmpd.c
		$(CC) $(CFLAGS) -DSNMPT -o snmpt.o -c snmpd.c


##################################################################
# unixd
##################################################################

inst-unixd:	$(SBINDIR)smux.unixd

$(SBINDIR)smux.unixd: xunixd
		-cp $@ zunixd
		-rm -f $@
		cp xunixd $@
		-@ls -gls $@
		-@echo ""

unixd:		xunixd

xunixd:		unixd.o $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ unixd.o $(LIBES) $(LSOCKET)

l-unixd:	SNMP_tables.c true
		$(LINT) $(LFLAGS) unixd.c SNMP_tables.c \
			objects.c syntax.c values.c \
			smux.c smuxentry.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

unixd.o:	SNMP-types.h objects.h smux.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h


##################################################################
# snmpi
##################################################################

inst-snmpi:	$(BINDIR)snmpi

$(BINDIR)snmpi:	xsnmpi
		-cp $@ zxsnmpi
		-rm -f $@
		cp xsnmpi $@
		-@ls -gls $@
		-@echo ""

snmpi:		xsnmpi

xsnmpi:		snmpi.o snmpb.o $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ snmpi.o snmpb.o $(LIBES) $(LSOCKET)

l-snmpi:	SNMP_tables.c true
		$(LINT) $(LFLAGS) snmpi.c snmpb.c SNMP_tables.c \
			objects.c syntax.c values.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

snmpi.o:	SNMP-types.h objects.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h \
			$(HDIR)tsap.h $(HDIR)isoaddrs.h \
			$(HDIR)dgram.h $(HDIR)internet.h $(HDIR)tp4.h
snmpb.o:	SNMP-types.h objects.h $(HFILES) $(HDIR)tailor.h \
			$(HDIR)logger.h


################################################################
# headers
################################################################

inst-headers:	$(INCDIR)snmp/objects.h $(INCDIR)snmp/smux.h \
			$(PEPSYDIR)SNMP-types.h $(PEPSYDIR)SNMP_defs.h \
			$(PEPSYDIR)SNMP_pre_defs.h $(PEPSYDIR)SNMP.ph

$(INCDIR)snmp/objects.h: objects.h
		-cp $@ zobjects.h
		cp objects.h $@
		-@ls -gls $@
		-@echo ""

$(INCDIR)snmp/smux.h: smux.h
		-cp $@ zsmux.h
		cp smux.h $@
		-@ls -gls $@
		-@echo ""

$(PEPSYDIR)SNMP-types.h: SNMP-types.h
		-cp $@ zSNMP-types.h
		cp SNMP-types.h $@
		-@ls -gls $@
		-@echo ""

$(PEPSYDIR)SNMP_defs.h: SNMP_defs.h
		-cp $@ zSNMP_defs.h
		cp SNMP_defs.h $@
		-@ls -gls $@
		-@echo ""

$(PEPSYDIR)SNMP_pre_defs.h: SNMP_pre_defs.h
		-cp $@ zSNMP_pre_defs.h
		cp SNMP_pre_defs.h $@
		-@ls -gls $@
		-@echo ""

$(PEPSYDIR)SNMP.ph: SNMP.ph
		-cp $@ zSNMP.ph
		cp SNMP.ph $@
		-@ls -gls $@
		-@echo ""


################################################################
# libisnmp
################################################################

inst-libisnmp:	$(LIBDIR)libisnmp.a $(LINTDIR)llib-lisnmp

$(LIBDIR)libisnmp.a:	libisnmp.a
		-rm -f $@
		cp libisnmp.a $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib
		-@ls -gls $@
		-@echo ""

$(LINTDIR)llib-lisnmp:	llib-lisnmp
		-cp $@ zllib-lisnmp
		-rm -f $@
		sed -e 's%#include "\(.*\)"%#include "$(INCDIR)snmp/\1"%' \
			< llib-lisnmp | \
			sed -e 's%#include "/usr/include/\(.*\)"%#include <\1>%' > $@
		@$(UTILDIR)inst-lint.sh $(SYSTEM) $(OPTIONS) $@
		-@ls -gls $@ $@.ln
		-@echo ""

libisnmp.a:	SNMP_tables.o objects.o syntax.o values.o smux.o smuxentry.o
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(SNMP-O) \
			SNMP_tables.o objects.o syntax.o values.o smux.o \
			smuxentry.o
		-@ls -l $@
		-@echo "SNMP library built normally"

SNMP_tables.o:	SNMP_tables.c SNMP-types.h

SNMP_tables.c SNMP-types.h: snmp.py $(TOPDIR)pepsy/xpepsy
		$(TOPDIR)pepsy/xpepsy -A -f -h -m snmp.py

objects.o:	objects.h $(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
syntax.o:	objects.h $(HFILES) $(HDIR)tailor.h $(HDIR)logger.h \
		$(HDIR)internet.h clns.h
values.o:	objects.h SNMP-types.h $(HDIR)logger.h
smux.o:		smux.h SNMP-types.h $(HDIR)tailor.h $(HDIR)logger.h \
		$(HDIR)internet.h
smuxentry.o:	smux.h SNMP-types.h $(HDIR)tailor.h


##################################################################
# files
##################################################################

inst-runcom:	$(ETCDIR)snmpd.rc

$(ETCDIR)snmpd.rc:	snmpd.rc
		if ln $(ETCDIR)snmpd.rc $(ETCDIR)snmpd.rc.old; \
		    then -rm -f $@; cp snmpd.rc $@; ls -gls $@; \
		    else exit 0; \
		fi

runcom:;


inst-peers:	$(ETCDIR)snmpd.peers

$(ETCDIR)snmpd.peers:	snmpd.peers
		-cp $@ zsnmpd.peers
		-rm -f $@
		cp snmpd.peers $@
		-chmod 600 $@
		-@ls -gls $@
		-@echo ""

peers:;


DEFS	=	objects.defs snmpd.defs unixd.defs # snmpc.defs
DEFS2	=	smi.defs mib.defs \
		    clns.defs ds1.defs ethernet.defs tokenring.defs ifx.defs \
			tokenbus.defs fddi.defs lanmgr.defs view.defs \
			bgp.defs ds3.defs appletalk.defs ppp.defs \
		    unix.defs smux.defs \
		    eval.defs

inst-objects:;	-mkdir $(INCDIR)snmp
		@for d in $(DEFS); do $(MAKE) TARGET=$$d inst-target; done
		@for d in $(DEFS2); do $(MAKE) TARGET2=$$d inst-target2; done

inst-target:	$(ETCDIR)$(TARGET)

$(ETCDIR)$(TARGET):	$(TARGET)
		-cp $@ z$(TARGET)
		-rm -f $@
		cp $(TARGET) $@
		-@if [ "$(TARGET)" = "objects.defs" ]; then \
		    xsnmpi -f $@ compile $@; fi
		-@ls -gls $@
		-@echo ""

inst-target2:	$(INCDIR)snmp/$(TARGET2)

$(INCDIR)snmp/$(TARGET2):	$(TARGET2)
		-cp $@ z$(TARGET2)
		-rm -f $@
		cp $(TARGET2) $@
		-@ls -gls $@
		-@echo ""

objects:	$(DEFS)

objects.defs:	$(DEFS2)
		cat $(DEFS2) > $@

snmpd.defs:	smi.defs mib.defs clns.defs view.defs smux.defs eval.defs
		cat smi.defs mib.defs clns.defs view.defs smux.defs eval.defs \
			> $@

snmpc.defs:	smi.defs view.defs smux.defs eval.defs
		cat smi.defs view.defs smux.defs eval.defs > $@

unixd.defs:	smi.defs unix.defs
		cat smi.defs unix.defs > $@


################################################################
# manual pages
################################################################

MANUALS	=	snmpd.8c snmpc.8c snmpt.8c unixd.8c snmpi.1c

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# clean
################################################################

clean:;		rm -f *.o *.a SNMP* x* z* _* core $(DEFS)

grind:;		iprint Makefile
		tgrind -lc $(CFILES)
		tgrind -lpepy -d $(TOPDIR)pepy/grindefs $(PYFILES) $(DEFS2)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;
