###############################################################################
#   Instructions to Make, for compilation of other ISODE programs
###############################################################################

###############################################################################
#
# $Header: /f/osi/others/RCS/Makefile,v 7.5 91/02/22 09:26:31 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.5  91/02/22  09:26:31  mrose
# Interim 6.8
# 
# Revision 7.4  90/12/23  18:41:58  mrose
# update
# 
# Revision 7.3  90/12/11  10:55:18  mrose
# lock-and-load
# 
# Revision 7.2  90/07/09  14:38:58  mrose
# sync
# 
# Revision 7.1  90/01/16  20:43:52  mrose
# last check-out
# 
# Revision 7.0  89/11/23  21:57:58  mrose
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


DIRS	=	quipu/photo quipu/image mosy tsbridge

OTHERS	=	callback idist listen lookup ntp osilookup pingpong quipu rfa \
			rtf tp0bridge 


##################################################################
# Here it is...
##################################################################

all:;		@for i in $(DIRS); \
		    do (echo "cd $$i; $(MAKE) all"; \
			      cd $$i; $(MAKE) all); \
		    done
		cd quipu/photo; $(MAKE) Xphoto xfax
		cd quipu; $(MAKE) pilot

inst-all:;	@for i in $(DIRS); \
		    do (echo "cd $$i; $(MAKE) inst-all"; \
			      cd $$i; $(MAKE) inst-all); \
		    done
		cd quipu; $(MAKE) inst-pilot

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
