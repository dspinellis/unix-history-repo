###############################################################################
#   Instructions to Make, for compilation of UFN interface
###############################################################################

###############################################################################
#
# $Header: /f/osi/others/quipu/uips/ufn/RCS/Makefile,v 7.6 91/02/22 09:33:11 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.6  91/02/22  09:33:11  mrose
# Interim 6.8
# 
# Revision 7.5  90/12/23  18:46:45  mrose
# update
# 
# Revision 7.4  90/11/20  15:33:58  mrose
# update
# 
# Revision 7.3  90/10/17  11:50:39  mrose
# sync
# 
# Revision 7.2  90/07/09  14:42:31  mrose
# sync
# 
# Revision 7.1  90/06/13  18:55:44  mrose
# update
# 
# Revision 7.0  90/06/13  18:52:39  mrose
# *** empty log message ***
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

LIBES	=	$(TOPDIR)libdsap.a $(TOPDIR)libisode.a
LLIBS   =	$(TOPDIR)llib-ldsap $(TOPDIR)llib-lisode


###############################################################################
# FILES
###############################################################################

CFILES  =       ufn_main.c


##############################################################
# Here it is...
##############################################################

all:            ufn
inst-all:       inst-ufn inst-ufnrc manuals
install:        inst-all clean
lint:           l-ufn


###################################################################
# ufn
###################################################################

inst-ufn:	$(BINDIR)ufn

$(BINDIR)ufn:	xufn
		-cp $@ zxufn
		-rm -f $@
		cp xufn $@
		-@ls -gls $@
		-@echo ""

ufn:		xufn

xufn:		ufn_main.o pipe.o socket.o $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ ufn_main.o pipe.o socket.o \
			$(LIBDSAP) $(LIBISODE) $(LSOCKET)

l-ufn:;		$(LINT) $(LFLAGS) ufn_main.c pipe.c ../dish/socket.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

socket.o:	../dish/socket.c
		$(CC) $(CFLAGS) -c ../dish/socket.c


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


################################################################
# manual pages
################################################################

MANUALS	=	ufn.1c

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


##############################################################
# clean
##############################################################

clean:;         rm -f *.ph *.o *.a a.out _* x* z* *.orig core

grind:;         iprint Makefile ufnrc
		tgrind -lc $(CFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;


#depend
pipe.o: ../../../../h/config.h
pipe.o: ../../../../h/dgram.h
pipe.o: ../../../../h/internet.h
pipe.o: ../../../../h/manifest.h
ufn_main.o: ../../../../h/config.h
ufn_main.o: ../../../../h/general.h
ufn_main.o: ../../../../h/isoaddrs.h
ufn_main.o: ../../../../h/logger.h
ufn_main.o: ../../../../h/manifest.h
ufn_main.o: ../../../../h/psap.h
ufn_main.o: ../../../../h/quipu/attr.h
ufn_main.o: ../../../../h/quipu/attrvalue.h
ufn_main.o: ../../../../h/quipu/authen.h
ufn_main.o: ../../../../h/quipu/bind.h
ufn_main.o: ../../../../h/quipu/config.h
ufn_main.o: ../../../../h/quipu/ds_error.h
ufn_main.o: ../../../../h/quipu/dsp.h
ufn_main.o: ../../../../h/quipu/entry.h
ufn_main.o: ../../../../h/quipu/name.h
ufn_main.o: ../../../../h/quipu/oid.h
ufn_main.o: ../../../../h/quipu/ufn.h
ufn_main.o: ../../../../h/quipu/util.h
