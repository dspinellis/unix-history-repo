###############################################################################
#   Instructions to Make, for compilation of ISODE DSAP processes
###############################################################################

###############################################################################
#
# $Header: /f/osi/dsap/RCS/Makefile,v 7.4 91/03/09 11:53:19 mrose Exp $
#
#
# $Log:	Makefile,v $
# Revision 7.4  91/03/09  11:53:19  mrose
# update
# 
# Revision 7.3  91/02/22  09:17:46  mrose
# Interim 6.8
# 
# Revision 7.2  90/12/23  18:39:47  mrose
# update
# 
# Revision 7.1  90/11/20  15:29:06  mrose
# cjr
# 
# Revision 7.0  89/11/23  21:31:23  mrose
# Release 6.0
# 
###############################################################################

###############################################################################
#
#                                NOTICE
#
#    Acquisition, use, and distribution of this module and related
#    materials are subject to the restrictions of a license agreement.
#    Consult the Preface in the User's Manual for the full terms of
#    this agreement.
#
###############################################################################


#PEPYPATH=	-DPEPYPATH

DIRS	=	x500as common net
LIBES   =       libcommon.a libnet.a libx500as.a
LLIBS   =       llib-lcommon llib-lnet llib-lx500as

##################################################################
# Here it is...
##################################################################

all:		sub-all libdsap
inst-all:       inst-libdsap inst-dsaptailor inst-oidtable

parallel:	sub-para libdsap

sub-para:;	@for i in $(DIRS); do (cd $$i; $(MAKE) PARALLEL=par- all) & done; wait

install:        inst-all clean

clean:		sub-clean
	        rm -f lib*.a llib-l* _* *.tmp *.orig *.o z* dsapvrsn.c

sub-all:
		@for i in $(DIRS); \
		    do (echo "cd $$i; $(MAKE) all"; \
			      cd $$i; $(MAKE) all); \
		    done

sub-inst-all:
		@for i in $(DIRS); \
		    do (echo "cd $$i; $(MAKE) inst-all"; \
			      cd $$i; $(MAKE) inst-all); \
		    done

sub-clean:
		@for i in $(DIRS); \
		    do (echo "cd $$i; $(MAKE) clean"; \
			      cd $$i; $(MAKE) clean); \
		    done

lint grind:
		@for i in $(DIRS); \
		    do (echo "cd $$i; $(MAKE) $@"; \
			      cd $$i; $(MAKE) $@); \
		    done

###################################################################
# libdsap
###################################################################

inst-libdsap:   inst-headers $(LIBDIR)libdsap.a $(LINTDIR)llib-ldsap

inst-headers:;	cd x500as; $(MAKE) inst-headers

$(LIBDIR)libdsap.a:     libdsap.a
		@for i in libdsap.* ;\
		do \
			rm -f $(LIBDIR)$$i; \
			echo cp $$i $(LIBDIR)$$i; \
			cp $$i $(LIBDIR)$$i; \
			case "$$i" in *.a) \
			$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib ;;\
			esac; \
			ls -gls $(LIBDIR)$$i ; \
		done
		-@echo ""

$(LINTDIR)llib-ldsap:   llib-ldsap
		-cp $@ zllib-ldsap
		-rm -f $@
		sed -e 's%#include "\(.*\)"%#include "$(INCDIR)\1"%' \
			< llib-ldsap | \
			sed -e 's%#include "$(INCDIR)quipu/\([A-Z].*\)"%#include "$(INCDIR)pepsy/\1"%' | \
			sed -e 's%#include "/usr/include/\(.*\)"%#include <\1>%' > $@
		@$(UTILDIR)inst-lint.sh $(SYSTEM) -I$(INCDIRM) $(OPTIONS) $@
		-@ls -gls $@ $@.ln
		-@echo ""

libdsap:	libdsap.a libdsap-$(SHAREDLIB) llib-ldsap

libdsap.a:      dsapvrsn.o
		-rm -f $@ $(TOPDIR)libdsap.a
		-rm -rf tmp
		-mkdir tmp
		$(LN) dsapvrsn.o tmp
		for i in $(LIBES); do (cd tmp; ar x ../$$i; \
		    ../$(UTILDIR)make-lib.sh -quick $(SYSTEM) $(ARFLAGS) ../$@ *.o; \
		    rm -f *); done
		$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib
		-rm -rf tmp
		-@rm -f $(TOPDIR)libdsap.a
		-@$(LN) $@ $(TOPDIR)libdsap.a
		-@ls -l $@
		-@echo "DSAP library built normally"

libdsap-:;

libdsap-shared: dsapvrsn.o
		@rm -f libdsap.so.* $(TOPDIR)libdsap.so.*
		@$(UTILDIR)make-lib.sh $(SYSTEM) -shared \
			-major `cat version.major``cat version.minor` \
			-minor `cat version.local` \
			libdsap.a 
		@for i in libdsap.s[ao].* ;\
		do \
			rm -f $(TOPDIR)$$i; \
			$(LN) $$i $(TOPDIR)$$i; \
			ls -l $$i; \
		done
		@echo "shared DSAP library built normally"
		@touch $@
		

llib-ldsap:     $(LLIBS)
		-@echo '/* llib-ldsap - lint library for -ldsap */' > $@
		-@echo '' >> $@
		cat $(LLIBS) >> $@
		-@rm -f $(TOPDIR)llib-ldsap
		-@$(LN) llib-ldsap $(TOPDIR)llib-ldsap

dsapvrsn.c:     $(LIBES)
		@$(UTILDIR)version.sh dsap > $@


###################################################################
# dsaptailor/oidtable
###################################################################

inst-dsaptailor:    $(ETCDIR)dsaptailor

$(ETCDIR)dsaptailor:    dsaptailor
		if ln $(ETCDIR)dsaptailor $(ETCDIR)dsaptailor.old; \
		    then rm -f $@; cat dsaptailor dsaptailor.root-dsas > $@; ls -gls $@; \
		    else exit 0; \
		fi

inst-oidtable:  $(ETCDIR)oidtable.at $(ETCDIR)oidtable.gen $(ETCDIR)oidtable.oc

$(ETCDIR)oidtable.at: oidtable.at oidtable.at.local 
		-cp $@ zoidtable.at
		cat oidtable.at oidtable.at.local > $@
		-@ls -gls $@
		-@echo ""

$(ETCDIR)oidtable.gen: oidtable.gen oidtable.gen.local
		-cp $@ zoidtable.gen
		cat oidtable.gen oidtable.gen.local > $@
		-@ls -gls $@
		-@echo ""

$(ETCDIR)oidtable.oc: oidtable.oc oidtable.oc.local
		-cp $@ zoidtable.oc
		cat oidtable.oc oidtable.oc.local > $@
		-@ls -gls $@
		-@echo ""


