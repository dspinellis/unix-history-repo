###############################################################################
#   Instructions to Make, for compilation of ISODE QUIPU White Pages interface
###############################################################################

###############################################################################
#
# $Header: /f/osi/others/quipu/uips/fred/RCS/Makefile,v 7.9 91/02/22 09:30:32 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.9  91/02/22  09:30:32  mrose
# Interim 6.8
# 
# Revision 7.8  90/12/23  18:47:05  mrose
# update
# 
# Revision 7.7  90/12/17  22:14:35  mrose
# dad.8c
# 
# Revision 7.6  90/10/28  22:40:56  mrose
# update
# 
# Revision 7.5  90/07/27  08:45:21  mrose
# update
# 
# Revision 7.4  90/07/09  14:41:09  mrose
# sync
# 
# Revision 7.3  90/03/22  08:36:27  mrose
# touch-up
# 
# Revision 7.2  90/03/15  11:20:43  mrose
# quipu-sync
# 
# Revision 7.1  90/01/11  18:36:18  mrose
# real-sync
# 
# Revision 7.0  89/11/23  22:08:51  mrose
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


###############################################################################
# Generation Rules for program modules
###############################################################################

.c.o:;		$(CC) $(CFLAGS) -c $*.c


###############################################################################
# Programs and Libraries
###############################################################################

LIBES	=	$(LIBISODE)
LLIBS   =	$(TOPDIR)llib-lisode
#LIBES	=	$(TOPDIR)libdirent.a  $(TOPDIR)libcompat.a
#LLIBS	=	$(TOPDIR)llib-ldirent $(TOPDIR)llib-lcompat


###############################################################################
# FILES
###############################################################################

HFILES  =       fred.h
CFILES  =       fred.c dispatch.c miscellany.c pipe.c whois.c


##############################################################
# Here it is...
##############################################################

all:            fred whitepages fredrc ufnrc fredsh dad
inst-all:       inst-fred inst-fredrc inst-ufnrc inst-fredsh inst-dad \
		inst-fredman manuals 
# inst-whitepages
install:        inst-all clean
lint:           l-fred l-dad


###################################################################
# fred
###################################################################

inst-fred:	$(BINDIR)fred

$(BINDIR)fred:	xfred
		-cp $@ zxfred
		-rm -f $@
		cp xfred $@
		-rm -f $(SBINDIR)in.whitepages
		if ln $@ $(SBINDIR)in.whitepages; \
		    then exit 0; \
		    else cp xfred $(SBINDIR)in.whitepages; \
		fi
		-@ls -gls $@ $(SBINDIR)in.whitepages
		-@echo ""

fred:		xfred

xfred:		fred.o dispatch.o miscellany.o pipe.o socket.o whois.o
		$(LDCC) $(LDFLAGS) -o $@ fred.o dispatch.o miscellany.o \
			pipe.o socket.o whois.o \
			$(LIBES) $(LSOCKET)

l-fred:;	$(LINT) $(LFLAGS) fred.c dispatch.c miscellany.c pipe.c \
			../dish/socket.c whois.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

fred.o:		fred.h
dispatch.o:	fred.h
miscellany.o:	fred.h
pipe.o:		fred.h

socket.o:	../dish/socket.c
		$(CC) $(CFLAGS) -c ../dish/socket.c

whois.o:	fred.h


###################################################################
# fredrc
###################################################################

inst-fredrc:	$(ETCDIR)fredrc

$(ETCDIR)fredrc: true
		if [ -s $@ ]; \
		    then exit 0; \
		    else cp fredrc $@; ls -gls $@; \
		fi
		if ln $(ETCDIR)fredrc $(ETCDIR)fredrc.old; \
		    then rm -f $@ ; cp fredrc $@; ls -gls $@; \
		    else exit 0; \
		fi

fredrc:


###################################################################
# ufnrc
###################################################################

inst-ufnrc:	$(ETCDIR)ufnrc

$(ETCDIR)ufnrc: true
		if [ -s $@ ]; \
		    then exit 0; \
		    else cp ufnrc $@; ls -gls $@; \
		fi
		if ln $(ETCDIR)ufnrc $(ETCDIR)ufnrc.old; \
		    then rm -f $@ ; cp ufnrc $@; ls -gls $@; \
		    else exit 0; \
		fi

ufnrc:


###################################################################
# fredsh
###################################################################

inst-fredsh:	$(SBINDIR)fredsh

$(SBINDIR)fredsh:	xfredsh Makefile
		-cp $@ zxfredsh
		-rm -f $@
		sed -e 's%@(BINDIR)%$(BINDIR)%' < fredsh.sh > $@
		chmod a+rx $@
		-@ls -gls $@
		-@echo ""

fredsh:		xfredsh

xfredsh:	fredsh.sh
		cp fredsh.sh $@
		chmod a+rx $@


###################################################################
# fredman
###################################################################

inst-fredman:	$(ETCDIR)fred.0

$(ETCDIR)fred.0:	fred.1c Makefile
		-cp $@ zfred.0
		-rm -f $@
		(echo '.ds ED $(ETCDIR)' ; cat fred.1c) | \
			nroff -man -Tlpr | \
			tr "\010" "_" | \
			sed -e "/^/s/__//g" > $@
		-@ls -gls $@
		-@echo ""


###################################################################
# dad
###################################################################

inst-dad:	$(SBINDIR)dad

$(SBINDIR)dad:	xdad
		-cp $@ zxdad
		-rm -f $@
		cp xdad $@
		-@ls -gls $@
		-@echo ""

dad:		xdad

xdad:		dad.o socket.o
		$(LDCC) $(LDFLAGS) -o $@ dad.o socket.o $(LIBES) $(LSOCKET)

l-dad:;		$(LINT) $(LFLAGS) dad.c ../dish/socket.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


###################################################################
# whitepages
###################################################################

inst-whitepages:	$(BINDIR)whitepages

$(BINDIR)whitepages:	xwhitepages
		-cp $@ zxwhitepages
		-rm -f $@
		cp xwhitepages $@
		-@ls -gls $@
		-@echo ""

whitepages:	xwhitepages

xwhitepages:	whitepages.sh
		cp whitepages.sh $@
		chmod a+rx $@


################################################################
# manual pages
################################################################

MANUALS	=	fred.1c dad.8c

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


##############################################################
# clean
##############################################################

clean:;         rm -f *.ph *.o *.a a.out _* x* z* *.orig core

grind:;         iprint Makefile whitepages.sh fredsh
		tgrind -lc $(HFILES) $(CFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;
