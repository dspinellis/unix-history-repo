###############################################################################
#   Instructions to Make, for compilation of other QUIPU programs
###############################################################################

###############################################################################
#
# $Header: /f/osi/others/quipu/RCS/Makefile,v 7.4 91/03/09 11:54:24 mrose Exp $
#
#
# $Log:	Makefile,v $
# Revision 7.4  91/03/09  11:54:24  mrose
# update
# 
# Revision 7.3  91/02/22  09:28:54  mrose
# Interim 6.8
# 
# Revision 7.2  91/02/12  18:32:55  mrose
# upate
# 
# Revision 7.1  90/12/23  18:46:36  mrose
# update
# 
# Revision 7.0  89/11/23  22:01:22  mrose
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

ALLDIR  =	photo image
EVERYDIR=	uips 
EXTRA	=       tools/dsastats
OTHERS	=	tools

DIRS	=	$(ALLDIR) $(EVERYDIR)

##################################################################
# Here it is...
##################################################################

all:;		@for i in $(DIRS); \
		    do (echo "cd $$i; $(MAKE) all"; \
			      cd $$i; $(MAKE) all); \
		    done

inst-all:;	@for i in $(DIRS); \
		    do (echo "cd $$i; $(MAKE) inst-all"; \
			      cd $$i; $(MAKE) inst-all); \
		    done

install:;	@for i in $(DIRS); \
		    do (echo "cd $$i; $(MAKE) install"; \
			      cd $$i; $(MAKE) install); \
		    done

everything:;	@for i in $(ALLDIR) $(EXTRA); \
		    do (echo "cd $$i; $(MAKE) all"; \
			      cd $$i; $(MAKE) all); \
		    done
		@for i in $(EVERYDIR); \
		    do (echo "cd $$i; $(MAKE) everything"; \
			      cd $$i; $(MAKE) everything); \
		    done

inst-everything:;  @for i in $(ALLDIR) $(EXTRA); \
		    do (echo "cd $$i; $(MAKE) inst-all"; \
			      cd $$i; $(MAKE) inst-all); \
		    done
		@for i in $(EVERYDIR); \
		    do (echo "cd $$i; $(MAKE) inst-everything"; \
			      cd $$i; $(MAKE) inst-everything); \
		    done

lint:;		@for i in $(DIRS); \
		    do (echo "cd $$i; $(MAKE) lint"; \
			      cd $$i; $(MAKE) lint); \
		    done

clean:;		rm -f _*
		@for i in $(DIRS) $(OTHERS); \
		    do (echo "cd $$i; $(MAKE) clean"; \
			      cd $$i; $(MAKE) clean); \
		    done

grind:;		@for i in $(DIRS); \
		    do (echo "cd $$i; $(MAKE) grind"; \
			      cd $$i; $(MAKE) grind); \
		    done


##################################################################
# pilot
##################################################################

pilot:;		@for i in uips/dish uips/fred tools/dsaconfig; \
		    do (echo "cd $$i; $(MAKE) all"; \
			      cd $$i; $(MAKE) all); \
		    done


inst-pilot:;	@for i in uips/dish uips/fred tools/dsaconfig; \
		    do (echo "cd $$i; $(MAKE) inst-all"; \
			      cd $$i; $(MAKE) inst-all); \
		    done
