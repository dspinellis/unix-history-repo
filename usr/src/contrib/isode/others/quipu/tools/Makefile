###############################################################################
#   Instructions to Make, for compilation of QUIPU tools
###############################################################################

###############################################################################
#
# $Header: /f/osi/others/quipu/tools/RCS/Makefile,v 7.4 91/02/22 09:29:32 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.4  91/02/22  09:29:32  mrose
# Interim 6.8
# 
# Revision 7.3  91/01/16  10:22:33  mrose
# dsastats
# 
# Revision 7.2  90/07/09  14:40:35  mrose
# sync
# 
# Revision 7.1  90/02/19  13:10:54  mrose
# update
# 
# Revision 7.0  89/11/23  22:02:28  mrose
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


DIRS	=	dsaconfig dsastats scripts


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
