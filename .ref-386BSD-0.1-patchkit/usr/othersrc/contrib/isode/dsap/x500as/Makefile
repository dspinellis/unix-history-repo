###############################################################################
#   Instructions to Make, for compilation of QUIPU protocol stuff
###############################################################################

###############################################################################
#
# $Header: /f/osi/dsap/x500as/RCS/Makefile,v 7.2 91/02/22 09:21:40 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.2  91/02/22  09:21:40  mrose
# Interim 6.8
# 
# Revision 7.1  90/12/23  18:46:00  mrose
# update
# 
# Revision 7.0  90/12/06  07:34:34  mrose
# *** empty log message ***
# 
# Revision 1.1  90/12/06  07:33:44  mrose
# 7.0
# 
# Revision 7.2  90/10/17  11:43:50  mrose
# sync
# 
# Revision 7.1  90/07/09  14:35:43  mrose
# sync
# 
# Revision 7.0  89/11/23  21:49:59  mrose
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


PEPYPATH= 	-DPEPYPATH

UPDIR   =       ../

LLIBS	=	$(TOPDIR)llib-lisode


#############################################################
# Here it is...
#############################################################

all:            libx500as 
inst-all:	libx500as
install:        inst-all clean
lint:           l-libx500as


#############################################################
# libx500as
#############################################################

PESPY-C	=	IF_tables.c Thorn_tables.c AF_tables.c \
		DO_tables.c DAS_tables.c \
		Quipu_tables.c SA_tables.c NRS_tables.c

PESPY-O	=	IF_tables.o Thorn_tables.o AF_tables.o \
		DO_tables.o DAS_tables.o \
		Quipu_tables.o SA_tables.o NRS_tables.o

HAND-C	=	asn_ext.c
HAND-O  =	asn_ext.o

CFILES	=	$(HAND-C) $(PESPY-C)
OFILES	=	$(HAND-O) $(PESPY-O)

PEPSY   =	$(TOPDIR)pepsy/xpepsy

libx500as:	libx500as.a

libx500as.a:	x500asvrsn.o
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(OFILES) \
			x500asvrsn.o
		-@rm -f $(UPDIR)libx500as.a $(UPDIR)llib-lx500as
		-@ln libx500as.a $(UPDIR)libx500as.a
		-@$(LN) llib-lx500as $(UPDIR)llib-lx500as
		-@ls -l $@
		-@echo "QUIPU X500 Abstract Syntax library built normally"

x500asvrsn.c:	PEPSY.O $(HAND-O)
		@$(UTILDIR)version.sh x500as > $@

l-libx500as:;	$(LINT) $(LFLAGS) $(CFILES) $(PEPY-C) $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


#######################################################
# saber
#######################################################

saber_src:;	#load $(OPTIONS) $(CFILES) 

saber_obj:;	#load libx500as.a

#######################################################
# asn
#######################################################

PEPSY.O:	PEPSY.C $(PESPY-O)
		touch PEPSY.O

PEPSY.C:	DSE.ph $(PESPY-C)
		touch PEPSY.C

# Eek do.py needs DAS.ph, but das.py needs DO.ph
# Need to resolve this somehow 

IF-types.h IF_tables.c: if.py if-cdefs.h $(PEPSY)
		$(PEPSY) -i "if-cdefs.h" -A -m $(PYFLAGS) if.py
		-@rm -f $(TOPDIR)h/quipu/IF*.h IF.ph
		-@$(LN) IF-types.h $(TOPDIR)h/quipu/IF-types.h
		-@$(LN) IF_defs.h $(TOPDIR)h/quipu/IF_defs.h
		-@$(LN) IF_pre_defs.h $(TOPDIR)h/quipu/IF_pre_defs.h
		-@$(LN) 2.5.1.1.ph IF.ph

AF-types.h AF_tables.c: af.py af-cdefs.h $(PEPSY)
		$(PEPSY) -i "af-cdefs.h" -A -m $(PYFLAGS) af.py
		-@rm -f $(TOPDIR)h/quipu/AF*.h AF.ph
		-@$(LN) AF-types.h $(TOPDIR)h/quipu/AF-types.h
		-@$(LN) AF_defs.h $(TOPDIR)h/quipu/AF_defs.h
		-@$(LN) AF_pre_defs.h $(TOPDIR)h/quipu/AF_pre_defs.h
		-@$(LN) 2.5.1.7.ph AF.ph

DO-types.h DO_tables.c: do.py do-cdefs.h $(PEPSY)
	   	$(PEPSY) -i "do-cdefs.h" -A -m $(PYFLAGS) do.py
		-@rm -f $(TOPDIR)h/quipu/DO*.h DO.ph
		-@$(LN) DO-types.h $(TOPDIR)h/quipu/DO-types.h
		-@$(LN) DO_defs.h $(TOPDIR)h/quipu/DO_defs.h
		-@$(LN) DO_pre_defs.h $(TOPDIR)h/quipu/DO_pre_defs.h
		-@$(LN) 2.5.1.3.ph DO.ph
	   	@echo "*** You can ignore WARNINGs about DAS.ph/2.5.1.2.ph ***"

DAS-types.h DAS_tables.c: das.py das-cdefs.h $(PEPSY)
	   	$(PEPSY) -i "das-cdefs.h" -A -m $(PYFLAGS) das.py
		-@rm -f $(TOPDIR)h/quipu/DAS*.h DAS.ph
		-@$(LN) DAS-types.h $(TOPDIR)h/quipu/DAS-types.h
		-@$(LN) DAS_defs.h $(TOPDIR)h/quipu/DAS_defs.h
		-@$(LN) DAS_pre_defs.h $(TOPDIR)h/quipu/DAS_pre_defs.h
		-@$(LN) 2.5.1.2.ph DAS.ph

Quipu-types.h Quipu_tables.c: qu.py qu-cdefs.h $(PEPSY)
	   	$(PEPSY) -i "qu-cdefs.h" -A -m $(PYFLAGS) qu.py
		-@rm -f $(TOPDIR)h/quipu/Quipu*.h Quipu.ph
		-@$(LN) Quipu-types.h $(TOPDIR)h/quipu/Quipu-types.h
		-@$(LN) Quipu_defs.h $(TOPDIR)h/quipu/Quipu_defs.h
		-@$(LN) Quipu_pre_defs.h $(TOPDIR)h/quipu/Quipu_pre_defs.h
		-@$(LN) 0.9.2342.19200300.99.1.ph Quipu.ph

SA-types.h SA_tables.c: sa.py if-cdefs.h $(PEPSY)
	   	$(PEPSY) -i "if-cdefs.h" -A -m $(PYFLAGS) sa.py
		-@rm -f $(TOPDIR)h/quipu/SA*.h SA.ph
		-@$(LN) SA-types.h $(TOPDIR)h/quipu/SA-types.h
		-@$(LN) SA_defs.h $(TOPDIR)h/quipu/SA_defs.h
		-@$(LN) SA_pre_defs.h $(TOPDIR)h/quipu/SA_pre_defs.h
		-@$(LN) 2.5.1.5.ph SA.ph

Thorn-types.h Thorn_tables.c: th.py if-cdefs.h $(PEPSY)
	   	$(PEPSY) -i "if-cdefs.h" -A -m $(PYFLAGS) th.py
		-@rm -f $(TOPDIR)h/quipu/Thorn*.h Thorn.ph
		-@$(LN) Thorn-types.h $(TOPDIR)h/quipu/Thorn-types.h
		-@$(LN) Thorn_defs.h $(TOPDIR)h/quipu/Thorn_defs.h
		-@$(LN) Thorn_pre_defs.h $(TOPDIR)h/quipu/Thorn_pre_defs.h
		-@$(LN) 0.9.2342.19200300.100.1.ph Thorn.ph

NRS-types.h NRS_tables.c: nrs.py nrs-cdefs.h $(PEPSY)
	   	$(PEPSY) -i "nrs-cdefs.h" -A -m $(PYFLAGS) nrs.py
		-@rm -f $(TOPDIR)h/quipu/NRS*.h NRS.ph
		-@$(LN) NRS-types.h $(TOPDIR)h/quipu/NRS-types.h
		-@$(LN) NRS_defs.h $(TOPDIR)h/quipu/NRS_defs.h
		-@$(LN) NRS_pre_defs.h $(TOPDIR)h/quipu/NRS_pre_defs.h
		-@$(LN) 0.9.2342.19200300.99.2.ph NRS.ph

DSE.ph:		$(TOPDIR)acsap/DSE.ph
		-@rm -f DSE.ph
		-$(LN) $(TOPDIR)acsap/DSE.ph DSE.ph
		-@rm -f DSE*.h
		-$(LN) $(TOPDIR)acsap/DSE-types.h DSE-types.h
		-$(LN) $(TOPDIR)acsap/DSE_defs.h DSE_defs.h
		-$(LN) $(TOPDIR)acsap/DSE_pre_defs.h DSE_pre_defs.h

#######################################################
# install headers
#######################################################

HEADERS = IF.ph IF-types.h IF_defs.h IF_pre_defs.h \
	  AF.ph AF-types.h AF_defs.h AF_pre_defs.h \
	  DAS.ph DAS-types.h DAS_defs.h DAS_pre_defs.h \
	  DO.ph DO-types.h DO_defs.h DO_pre_defs.h \
	  SA.ph SA-types.h SA_defs.h SA_pre_defs.h \
	  NRS.ph NRS-types.h NRS_defs.h NRS_pre_defs.h \
	  Quipu.ph Quipu-types.h Quipu_defs.h Quipu_pre_defs.h \
	  Thorn.ph Thorn-types.h Thorn_defs.h Thorn_pre_defs.h \
	  af-cdefs.h do-cdefs.h if-cdefs.h qu-cdefs.h \
	  das-cdefs.h nrs-cdefs.h

inst-headers:;	-mkdir $(PEPSYDIRM)
		@for h in $(HEADERS) ; \
		     do $(MAKE) TARGET=$$h inst-target; done

inst-target:	$(PEPSYDIR)$(TARGET)

$(PEPSYDIR)$(TARGET):	$(TARGET)
		-cp $@ z$(TARGET)
		cp $(TARGET) $@
		-@ls -gls $@
		-@echo ""

#######################################################
# clean
#######################################################

clean:;         rm -f *.ph *.o *.a a.out _* x* z* *.orig core x500asvrsn.c
		rm -f AF* IF* DAS* DO* Quipu* Thorn* SA* NRS* DSE* PEPSY*
		rm -f $(TOPDIR)h/quipu/IF*.h
		rm -f $(TOPDIR)h/quipu/AF*.h
		rm -f $(TOPDIR)h/quipu/DO*.h
		rm -f $(TOPDIR)h/quipu/DAS*.h
		rm -f $(TOPDIR)h/quipu/SA*.h
		rm -f $(TOPDIR)h/quipu/Quipu*.h
		rm -f $(TOPDIR)h/quipu/Thorn*.h
		rm -f $(TOPDIR)h/quipu/NRS*.h

grind:;         iprint Makefile
		tgrind -lpepy -d $(TOPDIR)pepy/grindefs 
		tgrind -lc $(CFILES) llib-lx500as

true:;


# DO NOT DELETE THIS LINE
# Dependencies follow
AF_tables.o: ../../h/UNIV-types.h
AF_tables.o: ../../h/UNIV_defs.h
AF_tables.o: ../../h/UNIV_pre_defs.h
AF_tables.o: ../../h/config.h
AF_tables.o: ../../h/general.h
AF_tables.o: ../../h/logger.h
AF_tables.o: ../../h/manifest.h
AF_tables.o: ../../h/pepsy.h
AF_tables.o: ../../h/psap.h
AF_tables.o: ../../h/quipu/attr.h
AF_tables.o: ../../h/quipu/attrvalue.h
AF_tables.o: ../../h/quipu/authen.h
AF_tables.o: ../../h/quipu/bind.h
AF_tables.o: ../../h/quipu/config.h
AF_tables.o: ../../h/quipu/name.h
AF_tables.o: ../../h/quipu/oid.h
AF_tables.o: ../../h/quipu/util.h
AF_tables.o: ./AF-types.h
AF_tables.o: ./AF_defs.h
AF_tables.o: ./AF_pre_defs.h
AF_tables.o: ./IF-types.h
AF_tables.o: ./IF_defs.h
AF_tables.o: ./IF_pre_defs.h
AF_tables.o: ./af-cdefs.h
AF_tables.o: ./if-cdefs.h
DAS_tables.o: ../../h/UNIV-types.h
DAS_tables.o: ../../h/UNIV_defs.h
DAS_tables.o: ../../h/UNIV_pre_defs.h
DAS_tables.o: ../../h/config.h
DAS_tables.o: ../../h/general.h
DAS_tables.o: ../../h/isoaddrs.h
DAS_tables.o: ../../h/logger.h
DAS_tables.o: ../../h/manifest.h
DAS_tables.o: ../../h/pepsy.h
DAS_tables.o: ../../h/psap.h
DAS_tables.o: ../../h/quipu/abandon.h
DAS_tables.o: ../../h/quipu/add.h
DAS_tables.o: ../../h/quipu/attr.h
DAS_tables.o: ../../h/quipu/attrvalue.h
DAS_tables.o: ../../h/quipu/authen.h
DAS_tables.o: ../../h/quipu/bind.h
DAS_tables.o: ../../h/quipu/common.h
DAS_tables.o: ../../h/quipu/commonarg.h
DAS_tables.o: ../../h/quipu/compare.h
DAS_tables.o: ../../h/quipu/config.h
DAS_tables.o: ../../h/quipu/dap.h
DAS_tables.o: ../../h/quipu/ds_error.h
DAS_tables.o: ../../h/quipu/ds_search.h
DAS_tables.o: ../../h/quipu/dsargument.h
DAS_tables.o: ../../h/quipu/dsp.h
DAS_tables.o: ../../h/quipu/entry.h
DAS_tables.o: ../../h/quipu/list.h
DAS_tables.o: ../../h/quipu/modify.h
DAS_tables.o: ../../h/quipu/modifyrdn.h
DAS_tables.o: ../../h/quipu/name.h
DAS_tables.o: ../../h/quipu/oid.h
DAS_tables.o: ../../h/quipu/read.h
DAS_tables.o: ../../h/quipu/remove.h
DAS_tables.o: ../../h/quipu/util.h
DAS_tables.o: ./AF-types.h
DAS_tables.o: ./AF_defs.h
DAS_tables.o: ./AF_pre_defs.h
DAS_tables.o: ./DAS-types.h
DAS_tables.o: ./DAS_defs.h
DAS_tables.o: ./DAS_pre_defs.h
DAS_tables.o: ./DO-types.h
DAS_tables.o: ./DO_defs.h
DAS_tables.o: ./DO_pre_defs.h
DAS_tables.o: ./DSE-types.h
DAS_tables.o: ./DSE_defs.h
DAS_tables.o: ./DSE_pre_defs.h
DAS_tables.o: ./IF-types.h
DAS_tables.o: ./IF_defs.h
DAS_tables.o: ./IF_pre_defs.h
DAS_tables.o: ./af-cdefs.h
DAS_tables.o: ./das-cdefs.h
DAS_tables.o: ./do-cdefs.h
DAS_tables.o: ./if-cdefs.h
DO_tables.o: ../../h/UNIV-types.h
DO_tables.o: ../../h/UNIV_defs.h
DO_tables.o: ../../h/UNIV_pre_defs.h
DO_tables.o: ../../h/config.h
DO_tables.o: ../../h/general.h
DO_tables.o: ../../h/isoaddrs.h
DO_tables.o: ../../h/logger.h
DO_tables.o: ../../h/manifest.h
DO_tables.o: ../../h/pepsy.h
DO_tables.o: ../../h/psap.h
DO_tables.o: ../../h/quipu/abandon.h
DO_tables.o: ../../h/quipu/add.h
DO_tables.o: ../../h/quipu/attr.h
DO_tables.o: ../../h/quipu/attrvalue.h
DO_tables.o: ../../h/quipu/authen.h
DO_tables.o: ../../h/quipu/bind.h
DO_tables.o: ../../h/quipu/common.h
DO_tables.o: ../../h/quipu/commonarg.h
DO_tables.o: ../../h/quipu/compare.h
DO_tables.o: ../../h/quipu/config.h
DO_tables.o: ../../h/quipu/dap.h
DO_tables.o: ../../h/quipu/ds_error.h
DO_tables.o: ../../h/quipu/ds_search.h
DO_tables.o: ../../h/quipu/dsargument.h
DO_tables.o: ../../h/quipu/dsp.h
DO_tables.o: ../../h/quipu/entry.h
DO_tables.o: ../../h/quipu/list.h
DO_tables.o: ../../h/quipu/modify.h
DO_tables.o: ../../h/quipu/modifyrdn.h
DO_tables.o: ../../h/quipu/name.h
DO_tables.o: ../../h/quipu/oid.h
DO_tables.o: ../../h/quipu/read.h
DO_tables.o: ../../h/quipu/remove.h
DO_tables.o: ../../h/quipu/util.h
DO_tables.o: ./AF-types.h
DO_tables.o: ./AF_defs.h
DO_tables.o: ./AF_pre_defs.h
DO_tables.o: ./DAS-types.h
DO_tables.o: ./DAS_defs.h
DO_tables.o: ./DAS_pre_defs.h
DO_tables.o: ./DO-types.h
DO_tables.o: ./DO_defs.h
DO_tables.o: ./DO_pre_defs.h
DO_tables.o: ./DSE-types.h
DO_tables.o: ./DSE_defs.h
DO_tables.o: ./DSE_pre_defs.h
DO_tables.o: ./IF-types.h
DO_tables.o: ./IF_defs.h
DO_tables.o: ./IF_pre_defs.h
DO_tables.o: ./af-cdefs.h
DO_tables.o: ./das-cdefs.h
DO_tables.o: ./do-cdefs.h
DO_tables.o: ./if-cdefs.h
IF_tables.o: ../../h/UNIV-types.h
IF_tables.o: ../../h/UNIV_defs.h
IF_tables.o: ../../h/UNIV_pre_defs.h
IF_tables.o: ../../h/config.h
IF_tables.o: ../../h/general.h
IF_tables.o: ../../h/logger.h
IF_tables.o: ../../h/manifest.h
IF_tables.o: ../../h/pepsy.h
IF_tables.o: ../../h/psap.h
IF_tables.o: ../../h/quipu/attr.h
IF_tables.o: ../../h/quipu/attrvalue.h
IF_tables.o: ../../h/quipu/config.h
IF_tables.o: ../../h/quipu/name.h
IF_tables.o: ../../h/quipu/oid.h
IF_tables.o: ../../h/quipu/util.h
IF_tables.o: ./IF-types.h
IF_tables.o: ./IF_defs.h
IF_tables.o: ./IF_pre_defs.h
IF_tables.o: ./if-cdefs.h
NRS_tables.o: ../../h/UNIV-types.h
NRS_tables.o: ../../h/UNIV_defs.h
NRS_tables.o: ../../h/UNIV_pre_defs.h
NRS_tables.o: ../../h/config.h
NRS_tables.o: ../../h/general.h
NRS_tables.o: ../../h/logger.h
NRS_tables.o: ../../h/manifest.h
NRS_tables.o: ../../h/pepsy.h
NRS_tables.o: ../../h/psap.h
NRS_tables.o: ../../h/quipu/config.h
NRS_tables.o: ../../h/quipu/nrs_info.h
NRS_tables.o: ../../h/quipu/util.h
NRS_tables.o: ./NRS-types.h
NRS_tables.o: ./NRS_defs.h
NRS_tables.o: ./NRS_pre_defs.h
NRS_tables.o: ./nrs-cdefs.h
Quipu_tables.o: ../../h/UNIV-types.h
Quipu_tables.o: ../../h/UNIV_defs.h
Quipu_tables.o: ../../h/UNIV_pre_defs.h
Quipu_tables.o: ../../h/config.h
Quipu_tables.o: ../../h/general.h
Quipu_tables.o: ../../h/isoaddrs.h
Quipu_tables.o: ../../h/logger.h
Quipu_tables.o: ../../h/manifest.h
Quipu_tables.o: ../../h/pepsy.h
Quipu_tables.o: ../../h/psap.h
Quipu_tables.o: ../../h/quipu/attr.h
Quipu_tables.o: ../../h/quipu/attrvalue.h
Quipu_tables.o: ../../h/quipu/authen.h
Quipu_tables.o: ../../h/quipu/bind.h
Quipu_tables.o: ../../h/quipu/config.h
Quipu_tables.o: ../../h/quipu/dsp.h
Quipu_tables.o: ../../h/quipu/entry.h
Quipu_tables.o: ../../h/quipu/name.h
Quipu_tables.o: ../../h/quipu/oid.h
Quipu_tables.o: ../../h/quipu/util.h
Quipu_tables.o: ./AF-types.h
Quipu_tables.o: ./AF_defs.h
Quipu_tables.o: ./AF_pre_defs.h
Quipu_tables.o: ./IF-types.h
Quipu_tables.o: ./IF_defs.h
Quipu_tables.o: ./IF_pre_defs.h
Quipu_tables.o: ./Quipu-types.h
Quipu_tables.o: ./Quipu_defs.h
Quipu_tables.o: ./Quipu_pre_defs.h
Quipu_tables.o: ./af-cdefs.h
Quipu_tables.o: ./if-cdefs.h
Quipu_tables.o: ./qu-cdefs.h
SA_tables.o: ../../h/UNIV-types.h
SA_tables.o: ../../h/UNIV_defs.h
SA_tables.o: ../../h/UNIV_pre_defs.h
SA_tables.o: ../../h/config.h
SA_tables.o: ../../h/general.h
SA_tables.o: ../../h/logger.h
SA_tables.o: ../../h/manifest.h
SA_tables.o: ../../h/pepsy.h
SA_tables.o: ../../h/psap.h
SA_tables.o: ../../h/quipu/attr.h
SA_tables.o: ../../h/quipu/attrvalue.h
SA_tables.o: ../../h/quipu/config.h
SA_tables.o: ../../h/quipu/name.h
SA_tables.o: ../../h/quipu/oid.h
SA_tables.o: ../../h/quipu/util.h
SA_tables.o: ./IF-types.h
SA_tables.o: ./IF_defs.h
SA_tables.o: ./IF_pre_defs.h
SA_tables.o: ./SA-types.h
SA_tables.o: ./SA_defs.h
SA_tables.o: ./SA_pre_defs.h
SA_tables.o: ./if-cdefs.h
Thorn_tables.o: ../../h/UNIV-types.h
Thorn_tables.o: ../../h/UNIV_defs.h
Thorn_tables.o: ../../h/UNIV_pre_defs.h
Thorn_tables.o: ../../h/config.h
Thorn_tables.o: ../../h/general.h
Thorn_tables.o: ../../h/logger.h
Thorn_tables.o: ../../h/manifest.h
Thorn_tables.o: ../../h/pepsy.h
Thorn_tables.o: ../../h/psap.h
Thorn_tables.o: ../../h/quipu/attr.h
Thorn_tables.o: ../../h/quipu/attrvalue.h
Thorn_tables.o: ../../h/quipu/config.h
Thorn_tables.o: ../../h/quipu/name.h
Thorn_tables.o: ../../h/quipu/oid.h
Thorn_tables.o: ../../h/quipu/util.h
Thorn_tables.o: ./Thorn-types.h
Thorn_tables.o: ./Thorn_defs.h
Thorn_tables.o: ./Thorn_pre_defs.h
Thorn_tables.o: ./if-cdefs.h
asn_ext.o: ../../h/UNIV-types.h
asn_ext.o: ../../h/UNIV_defs.h
asn_ext.o: ../../h/UNIV_pre_defs.h
asn_ext.o: ../../h/config.h
asn_ext.o: ../../h/general.h
asn_ext.o: ../../h/isoaddrs.h
asn_ext.o: ../../h/logger.h
asn_ext.o: ../../h/manifest.h
asn_ext.o: ../../h/pepsy.h
asn_ext.o: ../../h/psap.h
asn_ext.o: ../../h/quipu/abandon.h
asn_ext.o: ../../h/quipu/add.h
asn_ext.o: ../../h/quipu/attr.h
asn_ext.o: ../../h/quipu/attrvalue.h
asn_ext.o: ../../h/quipu/authen.h
asn_ext.o: ../../h/quipu/bind.h
asn_ext.o: ../../h/quipu/common.h
asn_ext.o: ../../h/quipu/commonarg.h
asn_ext.o: ../../h/quipu/compare.h
asn_ext.o: ../../h/quipu/config.h
asn_ext.o: ../../h/quipu/dap.h
asn_ext.o: ../../h/quipu/ds_error.h
asn_ext.o: ../../h/quipu/ds_search.h
asn_ext.o: ../../h/quipu/dsargument.h
asn_ext.o: ../../h/quipu/dsp.h
asn_ext.o: ../../h/quipu/entry.h
asn_ext.o: ../../h/quipu/list.h
asn_ext.o: ../../h/quipu/modify.h
asn_ext.o: ../../h/quipu/modifyrdn.h
asn_ext.o: ../../h/quipu/name.h
asn_ext.o: ../../h/quipu/oid.h
asn_ext.o: ../../h/quipu/read.h
asn_ext.o: ../../h/quipu/remove.h
asn_ext.o: ../../h/quipu/turbo.h
asn_ext.o: ../../h/quipu/util.h
asn_ext.o: ./AF-types.h
asn_ext.o: ./AF_defs.h
asn_ext.o: ./AF_pre_defs.h
asn_ext.o: ./IF-types.h
asn_ext.o: ./IF_defs.h
asn_ext.o: ./IF_pre_defs.h
asn_ext.o: ./Quipu-types.h
asn_ext.o: ./Quipu_defs.h
asn_ext.o: ./Quipu_pre_defs.h
asn_ext.o: ./af-cdefs.h
asn_ext.o: ./if-cdefs.h
asn_ext.o: ./qu-cdefs.h

# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
