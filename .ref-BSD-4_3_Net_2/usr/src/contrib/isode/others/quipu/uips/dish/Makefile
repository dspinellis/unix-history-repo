###############################################################################
#   Instructions to Make, for compilation of ISODE QUIPU Directory Server
###############################################################################

###############################################################################
#
# $Header: /f/osi/others/quipu/uips/dish/RCS/Makefile,v 7.5 91/02/22 09:30:14 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.5  91/02/22  09:30:14  mrose
# Interim 6.8
# 
# Revision 7.4  90/12/23  18:47:08  mrose
# update
# 
# Revision 7.3  90/11/20  15:34:12  mrose
# update
# 
# Revision 7.2  90/10/17  11:48:16  mrose
# sync
# 
# Revision 7.1  90/07/09  14:40:55  mrose
# sync
# 
# Revision 7.0  89/11/23  22:08:28  mrose
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

LIBES	=	$(TOPDIR)libdsap.a $(TOPDIR)libisode.a
LLIBS   =	$(TOPDIR)llib-ldsap $(TOPDIR)llib-lisode


###############################################################################
# FILES
###############################################################################

HFILES	=	
CFILES  =       pipe.c unbind.c socket.c quipurc.c


##############################################################
# Here it is...
##############################################################

all:            xpipe xunbind xquipurc
inst-all:       inst-pipe inst-unbind inst-dishinit inst-quipurc
install:        inst-all clean
lint:           l-pipe l-unbind l-quipurc


###################################################################
# pipe, et. al.
###################################################################

inst-pipe:	$(BINDIR)list

$(BINDIR)list:	xpipe
		-cp $@ zxpipe
		-rm -f $@
		cp xpipe $@
		-rm -f $(BINDIR)compare
		ln $@ $(BINDIR)compare
		-rm -f $(BINDIR)search
		ln $@ $(BINDIR)search
		-rm -f $(BINDIR)add
		ln $@ $(BINDIR)add
		-rm -f $(BINDIR)delete
		ln $@ $(BINDIR)delete
		-rm -f $(BINDIR)modify
		ln $@ $(BINDIR)modify
		-rm -f $(BINDIR)modifyrdn
		ln $@ $(BINDIR)modifyrdn
		-rm -f $(BINDIR)showentry
		ln $@ $(BINDIR)showentry
		-rm -f $(BINDIR)showname
		ln $@ $(BINDIR)showname
		-rm -f $(BINDIR)bind
		ln $@ $(BINDIR)bind
		-rm -f $(BINDIR)moveto
		ln $@ $(BINDIR)moveto
		-rm -f $(BINDIR)dsacontrol
		ln $@ $(BINDIR)dsacontrol
		-@ls -gls $@
		-@echo ""

xpipe:		pipe.o socket.o $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ pipe.o socket.o \
			$(LIBDSAP) $(LIBISODE) $(LSOCKET)

l-pipe:;	$(LINT) $(LFLAGS) pipe.c socket.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


###################################################################
# unbind
###################################################################

inst-unbind:	$(BINDIR)unbind

$(BINDIR)unbind: xunbind
		-cp $@ zxunbind
		-rm -f $@
		cp xunbind $@
		-rm -f $(BINDIR)squid
		ln $@ $(BINDIR)squid
		-@ls -gls $@
		-@echo ""

xunbind:	unbind.o socket.o $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@  unbind.o socket.o \
			$(LIBDSAP) $(LIBISODE) $(LSOCKET)

l-unbind:;	$(LINT) $(LFLAGS) unbind.c socket.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


###################################################################
# dishinit
###################################################################

inst-dishinit:	$(SBINDIR)dishinit

$(SBINDIR)dishinit: dishinit
		-cp $@ zdishinit
		-rm -f $@
		cp dishinit $@
		chmod 600 $@
		-@ls -gls $@
		-@echo ""


###################################################################
# quipurc
###################################################################

inst-quipurc:	 $(SBINDIR)new_quipurc

$(SBINDIR)new_quipurc: xquipurc
		-cp $@ zxquipurc
		-rm -f $@
		cp xquipurc $@
		chmod ugo+s $@
		-@ls -gls $@
		-@echo ""

xquipurc:	quipurc.o $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ quipurc.o \
			$(LIBDSAP) $(LIBISODE) $(LSOCKET) $(LIBGDBM)

l-quipurc:;	$(LINT) $(LFLAGS) quipurc.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


###################################################################
# SID
###################################################################

inst-sid:	$(BINDIR)clist $(BINDIR)dlist $(BINDIR)osearch \
		$(BINDIR)ousearch $(BINDIR)psearch

$(BINDIR)clist:  clist
		 -cp $@ zclist
		 -rm -f $@
		 cp clist $@
		 chmod a+x $@
		 -@ls -gls $@
		 -@echo ""

$(BINDIR)dlist:  dlist
		 -cp $@ zdlist
		 -rm -f $@
		 cp dlist $@
		 chmod a+x $@
		 -@ls -gls $@
		 -@echo ""

$(BINDIR)osearch:  osearch
		 -cp $@ zosearch
		 -rm -f $@
		 cp osearch $@
		 chmod a+x $@
		 -@ls -gls $@
		 -@echo ""

$(BINDIR)ousearch:  ousearch
		 -cp $@ zousearch
		 -rm -f $@
		 cp ousearch $@
		 chmod a+x $@
		 -@ls -gls $@
		 -@echo ""

$(BINDIR)psearch:  psearch
		 -cp $@ zpsearch
		 -rm -f $@
		 cp psearch $@
		 chmod a+x $@
		 -@ls -gls $@
		 -@echo ""

##############################################################
# clean
##############################################################

clean:;         rm -f *.ph *.o *.a a.out _* x* z* *.orig core

grind:;         iprint Makefile
		tgrind -lc $(CFILES)
#		@echo $(MANUALS) | \
#			tr " " "\012" | \
#			sed -e "s%.*%itroff -man &%" | \
#			sh -ve

true:;

# DO NOT DELETE THIS LINE
# Dependencies follow
pipe.o: ../../../../h/config.h
pipe.o: ../../../../h/dgram.h
pipe.o: ../../../../h/general.h
pipe.o: ../../../../h/internet.h
pipe.o: ../../../../h/logger.h
pipe.o: ../../../../h/manifest.h
pipe.o: ../../../../h/quipu/config.h
pipe.o: ../../../../h/quipu/util.h
pipe.o: ../../../../h/tailor.h
pipe.o: ../../../../h/usr.dirent.h
quipurc.o: ../../../../h/config.h
quipurc.o: ../../../../h/general.h
quipurc.o: ../../../../h/isoaddrs.h
quipurc.o: ../../../../h/logger.h
quipurc.o: ../../../../h/manifest.h
quipurc.o: ../../../../h/psap.h
quipurc.o: ../../../../h/quipu/attr.h
quipurc.o: ../../../../h/quipu/attrvalue.h
quipurc.o: ../../../../h/quipu/authen.h
quipurc.o: ../../../../h/quipu/bind.h
quipurc.o: ../../../../h/quipu/commonarg.h
quipurc.o: ../../../../h/quipu/config.h
quipurc.o: ../../../../h/quipu/dap.h
quipurc.o: ../../../../h/quipu/ds_error.h
quipurc.o: ../../../../h/quipu/ds_search.h
quipurc.o: ../../../../h/quipu/dsp.h
quipurc.o: ../../../../h/quipu/dua.h
quipurc.o: ../../../../h/quipu/entry.h
quipurc.o: ../../../../h/quipu/list.h
quipurc.o: ../../../../h/quipu/modify.h
quipurc.o: ../../../../h/quipu/name.h
quipurc.o: ../../../../h/quipu/oid.h
quipurc.o: ../../../../h/quipu/read.h
quipurc.o: ../../../../h/quipu/sequence.h
quipurc.o: ../../../../h/quipu/util.h
quipurc.o: ../../../../h/tailor.h
socket.o: ../../../../h/config.h
socket.o: ../../../../h/dgram.h
socket.o: ../../../../h/general.h
socket.o: ../../../../h/internet.h
socket.o: ../../../../h/logger.h
socket.o: ../../../../h/manifest.h
socket.o: ../../../../h/quipu/config.h
socket.o: ../../../../h/quipu/util.h
socket.o: ../../../../h/tailor.h
unbind.o: ../../../../h/config.h
unbind.o: ../../../../h/dgram.h
unbind.o: ../../../../h/general.h
unbind.o: ../../../../h/internet.h
unbind.o: ../../../../h/logger.h
unbind.o: ../../../../h/manifest.h
unbind.o: ../../../../h/quipu/config.h
unbind.o: ../../../../h/quipu/util.h
unbind.o: ../../../../h/tailor.h
