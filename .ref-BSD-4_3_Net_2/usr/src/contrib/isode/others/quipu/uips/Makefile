###############################################################################
#   Instructions to Make, for compilation of QUIPU user interface programs
###############################################################################

###############################################################################
#
# $Header: /f/osi/others/quipu/uips/RCS/Makefile,v 7.4 91/03/09 11:54:43 mrose Exp $
#
#
# $Log:	Makefile,v $
# Revision 7.4  91/03/09  11:54:43  mrose
# update
# 
# Revision 7.3  91/02/22  09:30:07  mrose
# Interim 6.8
# 
# Revision 7.2  91/01/16  10:22:04  mrose
# dsc
# 
# Revision 7.1  90/07/09  14:40:51  mrose
# sync
# 
# Revision 7.0  89/11/23  22:08:21  mrose
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


DIRS	=	dish
OTHERS  =	dsc fred pod sd ufn xd manage


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

everything:;	@for i in $(DIRS) $(OTHERS); \
		    do (echo "cd $$i; $(MAKE) all"; \
			      cd $$i; $(MAKE) all); \
		    done

inst-everything:;	@for i in $(DIRS) $(OTHERS); \
		    do (echo "cd $$i; $(MAKE) inst-all"; \
			      cd $$i; $(MAKE) inst-all); \
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
