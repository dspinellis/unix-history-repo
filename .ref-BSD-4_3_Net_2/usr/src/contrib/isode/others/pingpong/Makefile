###############################################################################
#   Instructions to Make, for compilation of ISODE xSAP processes
###############################################################################

###############################################################################
#
# $Header: /f/osi/others/pingpong/RCS/Makefile,v 7.2 91/02/22 09:28:49 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.2  91/02/22  09:28:49  mrose
# Interim 6.8
# 
# Revision 7.1  90/12/23  18:44:35  mrose
# update
# 
# Revision 7.0  89/11/23  22:01:08  mrose
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

.c.o:;	$(CC) $(CFLAGS) -c $*.c


LIBES	=	$(TOPDIR)libisode.a
LLIBS   =	$(TOPDIR)llib-lisode

all:		pingpong
inst-all:	
install:	
lint:		l-pingpong

pingpong:	xpingpong

xpingpong:	pingpong.o
		$(LDCC) -o $@ pingpong.o $(LIBES) $(LSOCKET)

l-pingpong:;	$(LINT) $(LFLAGS) pingpong.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

clean:;		rm -f x* z* _* *.o core
