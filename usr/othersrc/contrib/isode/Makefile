##########################################################################
#   Instructions to Make, for compilation of ISODE processes
##########################################################################

###############################################################################
#
# $Header: /f/osi/RCS/Makefile,v 7.9 91/02/22 09:13:48 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.9  91/02/22  09:13:48  mrose
# Interim 6.8
# 
# Revision 7.8  91/01/15  09:30:05  mrose
# update
# 
# Revision 7.7  90/10/15  18:18:32  mrose
# iaed
# 
# Revision 7.6  90/07/29  16:47:38  mrose
# test
# 
# Revision 7.5  90/07/27  08:49:43  mrose
# update
# 
# Revision 7.4  90/07/09  14:30:11  mrose
# sync
# 
# Revision 7.3  90/07/01  21:07:55  mrose
# pepsy
# 
# Revision 7.2  90/03/27  07:53:29  mrose
# partial
# 
# Revision 7.1  90/03/05  23:04:11  mrose
# touch-up
# 
# Revision 7.0  89/11/23  21:21:28  mrose
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


##################################################################
# Here it is...
##################################################################

DIRS	=	h compat dirent tsap ssap psap pepsy pepy psap2 \
		acsap rosap rtsap ronot rosy support imisc
OTHERS	=	dsap ftam ftam2 ftam-ftp ftp-ftam others psap2-lpp quipu \
		snmp vt


all:;		@for i in $(DIRS); \
		    do (echo "cd $$i; $(MAKE) all"; \
			      cd $$i; $(MAKE) all); \
		    done

inst-all:;	@for i in $(BINDIR) $(SBINDIR) $(ETCDIR) $(LOGDIR) \
			$(LIBDIR) $(LINTDIR) $(MANDIR); \
		    do (echo $$i X | \
			sed -e 's%\(.*\)/ X%if test ! -d \1; then mkdir \1; chmod 555 \1; fi%' | \
			sh -ve); \
		    done
		@echo $(LOGDIR) X | \
			sed -e 's%\(.*\)/ X%chmod 777 \1%' | \
			sh -ve
		@for i in $(DIRS); \
		    do (echo "cd $$i; $(MAKE) inst-all"; \
			      cd $$i; $(MAKE) inst-all); \
		    done

install:	inst-all clean

lint:;		@for i in $(DIRS); \
		    do (echo "cd $$i; $(MAKE) lint"; \
			      cd $$i; $(MAKE) lint); \
		    done

clean:;		rm -f *.a *.so.* llib-l* _* *.tmp config/_* util/inst-man.sh
		@for i in $(DIRS) $(OTHERS); \
		    do (echo "cd $$i; $(MAKE) clean"; \
			      cd $$i; $(MAKE) clean); \
		    done

test:;		PATH=$$PATH:${BINDIR} sh isode-test

grind:;		@for i in $(DIRS) $(OTHERS); \
		    do (echo "cd $$i; $(MAKE) grind"; \
			      cd $$i; $(MAKE) grind); \
		    done

once-only:;	@touch h/*.h h/quipu/*.h
		@touch dsap/dsaptailor dsap/oidtable.*.local quipu/quiputailor

distribution:	clean once-only READ-ME QUIPU-READ-ME
		@cd doc; make clean

READ-ME:	isode-gen.8
		nroff -man isode-gen.8 > isode-gen.tmp
		mv isode-gen.tmp $@

QUIPU-READ-ME:	quipu-new.8
		nroff -ms quipu-new.8 > quipu-new.tmp
		mv quipu-new.tmp $@

inst-partial:;	cd h; $(MAKE) inst-all
		cd pepsy; $(MAKE) inst-all
		cd pepy; $(MAKE) inst-all
		cd support; $(MAKE) macros inst-libisode


##################################################################
# everything
##################################################################

everything:	all true
		./make all-ftam all-ftam-ftp all-vt all-quipu all-snmp all-lpp
		@echo "a great album by mtr's favorite rock band"

inst-everything: inst-all true
		./make inst-ftam inst-ftam-ftp inst-vt inst-quipu inst-snmp \
			inst-lpp

true:;


##################################################################
# ftam
##################################################################

FTAM	=	ftam ftam2


all-ftam:;	@for i in $(FTAM); \
		    do (echo "cd $$i; $(MAKE) all"; \
			      cd $$i; $(MAKE) all); \
		    done

inst-ftam:;	@for i in $(FTAM); \
		    do (echo "cd $$i; $(MAKE) inst-all"; \
			      cd $$i; $(MAKE) inst-all); \
		    done

install-ftam:;	@for i in $(FTAM); \
		    do (echo "cd $$i; $(MAKE) install"; \
			      cd $$i; $(MAKE) install); \
		    done

lint-ftam:;	@for i in $(FTAM); \
		    do (echo "cd $$i; $(MAKE) lint"; \
			      cd $$i; $(MAKE) lint); \
		    done


##################################################################
# ftam-ftp
##################################################################

FTAM-FTP=	ftam-ftp ftp-ftam


all-ftam-ftp:;	@for i in $(FTAM-FTP); \
		    do (echo "cd $$i; $(MAKE) all"; \
			      cd $$i; $(MAKE) all); \
		    done

inst-ftam-ftp:;	@for i in $(FTAM-FTP); \
		    do (echo "cd $$i; $(MAKE) inst-all"; \
			      cd $$i; $(MAKE) inst-all); \
		    done

install-ftam-ftp:;	@for i in $(FTAM-FTP); \
		    do (echo "cd $$i; $(MAKE) install"; \
			      cd $$i; $(MAKE) install); \
		    done

lint-ftam-ftp:;	@for i in $(FTAM-FTP); \
		    do (echo "cd $$i; $(MAKE) lint"; \
			      cd $$i; $(MAKE) lint); \
		    done


##################################################################
# vt
##################################################################

VT	=	vt


all-vt:;	@for i in $(VT); \
		    do (echo "cd $$i; $(MAKE) all"; \
			      cd $$i; $(MAKE) all); \
		    done

inst-vt:;	@for i in $(VT); \
		    do (echo "cd $$i; $(MAKE) inst-all"; \
			      cd $$i; $(MAKE) inst-all); \
		    done

install-vt:;	@for i in $(VT); \
		    do (echo "cd $$i; $(MAKE) install"; \
			      cd $$i; $(MAKE) install); \
		    done

lint-vt:;	@for i in $(VT); \
		    do (echo "cd $$i; $(MAKE) lint"; \
			      cd $$i; $(MAKE) lint); \
		    done


##################################################################
# quipu
##################################################################

QUIPU	=	dsap quipu


all-quipu:;	cd h/quipu; $(MAKE) all
		@for i in $(QUIPU); \
		    do (echo "cd $$i; $(MAKE) all"; \
			      cd $$i; $(MAKE) all); \
		    done
		cd acsap; $(MAKE) dased
		cd support; $(MAKE) iaed

inst-quipu:;	cd h/quipu; $(MAKE) inst-all
		@for i in $(QUIPU); \
		    do (echo "cd $$i; $(MAKE) inst-all"; \
			      cd $$i; $(MAKE) inst-all); \
		    done
		cd acsap; $(MAKE) inst-dased man-dased
		cd support; $(MAKE) inst-iaed man-iaed

install-quipu:;	cd h/quipu; $(MAKE) install
		@for i in $(QUIPU); \
		    do (echo "cd $$i; $(MAKE) install"; \
			      cd $$i; $(MAKE) install); \
		    done
		cd acsap; $(MAKE) inst-dased man-dased
		cd support; $(MAKE) inst-iaed man-iaed

lint-quipu:;	cd h/quipu; $(MAKE) lint
		@for i in $(QUIPU); \
		    do (echo "cd $$i; $(MAKE) lint"; \
			      cd $$i; $(MAKE) lint); \
		    done		
		cd acsap; $(MAKE) l-dased
		cd support; $(MAKE) l-iaed


##################################################################
# snmp
##################################################################

SNMP	=	snmp


all-snmp:;	@for i in $(SNMP); \
		    do (echo "cd $$i; $(MAKE) all"; \
			      cd $$i; $(MAKE) all); \
		    done

inst-snmp:;	@for i in $(SNMP); \
		    do (echo "cd $$i; $(MAKE) inst-all"; \
			      cd $$i; $(MAKE) inst-all); \
		    done

install-snmp:;	@for i in $(SNMP); \
		    do (echo "cd $$i; $(MAKE) install"; \
			      cd $$i; $(MAKE) install); \
		    done

lint-snmp:;	@for i in $(SNMP); \
		    do (echo "cd $$i; $(MAKE) lint"; \
			      cd $$i; $(MAKE) lint); \
		    done


##################################################################
# lpp
##################################################################

LPP	=	h compat psap pepsy pepy acsap rosap ronot rosy psap2-lpp
LPP-AUX	=	support imisc


all-lpp:;	@for i in $(LPP); \
		    do (echo "cd $$i; $(MAKE) all"; \
			      cd $$i; $(MAKE) all); \
		    done
		@for i in $(LPP-AUX); \
		    do (echo "cd $$i; $(MAKE) all-lpp"; \
			      cd $$i; $(MAKE) all-lpp); \
		    done

inst-lpp:;	@for i in $(LPP); \
		    do (echo "cd $$i; $(MAKE) inst-all"; \
			      cd $$i; $(MAKE) inst-all); \
		    done
		@for i in $(LPP-AUX); \
		    do (echo "cd $$i; $(MAKE) inst-lpp"; \
			      cd $$i; $(MAKE) inst-lpp); \
		    done

install-lpp:;	@for i in $(LPP); \
		    do (echo "cd $$i; $(MAKE) install"; \
			      cd $$i; $(MAKE) install); \
		    done
		@for i in $(LPP-AUX); \
		    do (echo "cd $$i; $(MAKE) install-lpp"; \
			      cd $$i; $(MAKE) install-lpp); \
		    done

lint-lpp:;	@for i in $(LPP); \
		    do (echo "cd $$i; $(MAKE) lint"; \
			      cd $$i; $(MAKE) lint); \
		    done
		@for i in $(LPP-AUX); \
		    do (echo "cd $$i; $(MAKE) lint-lpp"; \
			      cd $$i; $(MAKE) lint-lpp); \
		    done


##################################################################
# image
##################################################################

image:;		find . -name RCS -a -exec rm -rf {} \; -a -prune
		rm -f CHECK-OUT config/CONFIG.make h/config.h \
			support/aliases.local support/entities.local \
			support/macros.local support/objects.local \
			support/services.local
		cp dsap/common/protected.c /tmp/protected.c
		-unifdef -UHAVE_PROTECTED < /tmp/protected.c \
			> dsap/common/protected.c
		rm -f /tmp/protected.c


##################################################################
# zap
##################################################################

zap:;	rm -rf $(INCDIRM)
	rm -f $(BINDIR)pepy  \
	    $(BINDIR)posy    \
	    $(BINDIR)rosy
	rm -f $(LIBDIR)libicompat.a \
	    $(LIBDIR)libdirent.a   \
	    $(LIBDIR)libtsap.a     \
	    $(LIBDIR)libssap.a     \
	    $(LIBDIR)libpsap.a     \
	    $(LIBDIR)libpepy.a     \
	    $(LIBDIR)libpsap2.a    \
	    $(LIBDIR)libacsap.a    \
	    $(LIBDIR)librtsap.a    \
	    $(LIBDIR)libronot.a    \
	    $(LIBDIR)librosap.a    \
	    $(LIBDIR)librosy.a     \
	    $(LIBDIR)libisode.a    \
	    $(LIBDIR)libdsap.a     \
	    $(LIBDIR)libquipu.a
	rm -f $(LINTDIR)llib-licompat* \
	    $(LINTDIR)llib-ldirent*   \
	    $(LINTDIR)llib-ltsap*     \
	    $(LINTDIR)llib-lssap*     \
	    $(LINTDIR)llib-lpsap*     \
	    $(LINTDIR)llib-lpsap2*    \
	    $(LINTDIR)llib-lacsap*    \
	    $(LINTDIR)llib-lrtsap*    \
	    $(LINTDIR)llib-lronot*    \
	    $(LINTDIR)llib-lrosap*    \
	    $(LINTDIR)llib-lrosy*     \
	    $(LINTDIR)llib-lisode*    \
	    $(LINTDIR)llib-ldsap*     \
	    $(LINTDIR)llib-lquipu*
