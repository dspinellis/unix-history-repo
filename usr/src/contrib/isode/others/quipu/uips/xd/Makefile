# $Header: /f/osi/others/quipu/uips/xd/RCS/Makefile,v 7.7 91/02/22 09:32:34 mrose Interim $
#
# $Log:	Makefile,v $
# Revision 7.7  91/02/22  09:32:34  mrose
# Interim 6.8
# 
# Revision 7.6  91/01/24  14:51:57  mrose
# update
# 
# Revision 7.5  90/12/23  18:46:49  mrose
# update
# 
# Revision 7.4  90/11/20  15:33:55  mrose
# update
# 
# Revision 7.3  90/10/17  11:50:11  mrose
# sync
# 
# Revision 7.2  90/07/27  08:45:51  mrose
# update
# 
# Revision 7.1  90/07/09  14:42:07  mrose
# sync
# 
# Revision 7.0  90/06/12  13:10:31  mrose
# *** empty log message ***
# 
# Revision 1.6  90/04/26  10:22:28  emsrssn
# Installation fixed
# 
# 
# Revision 1.5  90/04/26  10:21:41  emsrssn
# *** empty log message ***
# 
# Revision 1.4  90/04/25  17:27:56  emsrssn
# Lint tidy up
# 
# 
# Revision 1.3  90/04/19  13:47:38  emsrssn
# keyboard accelerator now activates button highlight.
# 
# search types available is dependent on current position
# to prevent unreasonable searches.
# 
# the help popup changes automatically depending on the 
# position of the cursor
# 
# buttons remain a fixed size when the application is
# resized
# 
# command line options are now handled properly
# 
# logging added
# 
# "reads" are now sorted to show mail address at top etc.
# 
# 
# Revision 1.2  90/03/09  15:57:21  emsrssn
# First public distribution
# 
# 
# Revision 1.1  90/03/08  13:18:50  emsrssn
# Initial revision
# 
# 
#
##############################################################
#
# Xd Makefile -    Xd uses the Athena widget set 
#
#                  Stefan Nahajski, Brunel University 28/2/90
##############################################################

XLIB = -lX11
XTLIB = -lXt

# athena widget library compiled with debugging option
#XAWLIB = /usr/local/lib/libXaw.a
XAWLIB = -lXaw

XMULIB = -lXmu
XEXTLIB = -lXext

XLIBDIR = /usr/lib/X11/

INCLUDE = -I$(TOPDIR)h

LIBES   = $(TOPDIR)libdsap.a $(TOPDIR)libisode.a
LIBS = $(XAWLIB) $(XMULIB) $(XTLIB) $(XLIB) $(XEXTLIB) 

LLIBES = $(TOPDIR)llib-ldsap $(TOPDIR)llib-lisode

##############################################################

CFILES = filt.c Xd.c sequence.c main.c calls.c symtab.c y.tab.c
OFILES = filt.o Xd.o sequence.o main.o calls.o symtab.o y.tab.o
YFILES = conf_read.y

##############################################################
 
all: 		xxd
#inst-all:	inst-xd config helpfiles app-defaults manuals
inst-all:	inst-xd confandhelp app-defaults manuals
install:	inst-all clean
lint:		l-xd

##############################################################

inst-xd:    $(BINDIR)xd
		-mkdir $(ETCDIR)xd

$(BINDIR)xd: xxd
		-cp $@ zxxd
		-rm -f $@
		cp xxd $@
		-@ls -gls $@
		-@echo ""

################################################################
# app-defaults files for X
################################################################

app-defaults:;	-cp Xd.ad $(XLIBDIR)app-defaults/Xd

################################################################
# config and help  files
################################################################

confandhelp:;	-mkdir $(ETCDIR)xd/
		cp -R Xd/* $(ETCDIR)xd/.
		-@echo ""

################################################################
# manual pages
################################################################

MANUALS =       xd.1c

# Just in case someone does 'rm x*'....
xd.1c:	Xd.1c
	cp Xd.1c xd.1c
	chmod 444 xd.1c

manuals:	$(MANUALS)
		@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""

################################################################
# lint
################################################################

l-xd:;		$(LINT) $(LFLAGS) $(CFILES) $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


#############################################################
# clean
##############################################################

clean:;		rm -f y.tab.* *.o a.out _* xx* z* core

grind:;         iprint Makefile
		tgrind -lc $(CFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve


##############################################################
# xd
##############################################################

xd:         xxd

xxd:	$(OFILES) $(LIBES)
	$(CC) $(INCLUDE) -o $@ $(CFLAGS) $(OFILES) $(LIBS) \
		$(LIBDSAP) $(LIBISODE) $(LSOCKET)


y.tab.c: conf_read.y
	yacc -d $(YFILES)

##############################################################
# RCS
##############################################################

ci:	FRC
	vi RCS_message_text
	ci -l -f RCS/* <RCS_message_text
#	cd Xd/duaconfig/filterTypes; ci -l -f RCS/* <../../../RCS_message_text
#	cd Xd/duaconfig; ci -l -f RCS/* <../../RCS_message_text
	cd Xd/helpdir; ci -l -f RCS/* <../../RCS_message_text
	cp /dev/null RCS_message_text

co:	FRC
	co -l RCS/*
	cd Xd/duaconfig/filterTypes; co -l RCS/*
	cd Xd/duaconfig; co -l RCS/*
	cd Xd/helpdir; co -l RCS/*

dist:	FRC
	cd ..; gnutar -c -f xd.tar.Z -o -X xd/ignore-these -z xd

FRC:

# DO NOT DELETE THIS LINE
# Dependencies follow
Xd.o: ./dirtitle.h
Xd.o: ./sequence.h
calls.o: ../../../../h/config.h
calls.o: ../../../../h/general.h
calls.o: ../../../../h/isoaddrs.h
calls.o: ../../../../h/logger.h
calls.o: ../../../../h/manifest.h
calls.o: ../../../../h/psap.h
calls.o: ../../../../h/quipu/abandon.h
calls.o: ../../../../h/quipu/add.h
calls.o: ../../../../h/quipu/attr.h
calls.o: ../../../../h/quipu/attrvalue.h
calls.o: ../../../../h/quipu/authen.h
calls.o: ../../../../h/quipu/bind.h
calls.o: ../../../../h/quipu/common.h
calls.o: ../../../../h/quipu/commonarg.h
calls.o: ../../../../h/quipu/compare.h
calls.o: ../../../../h/quipu/config.h
calls.o: ../../../../h/quipu/dap.h
calls.o: ../../../../h/quipu/ds_error.h
calls.o: ../../../../h/quipu/ds_search.h
calls.o: ../../../../h/quipu/dsp.h
calls.o: ../../../../h/quipu/entry.h
calls.o: ../../../../h/quipu/list.h
calls.o: ../../../../h/quipu/modify.h
calls.o: ../../../../h/quipu/modifyrdn.h
calls.o: ../../../../h/quipu/name.h
calls.o: ../../../../h/quipu/oid.h
calls.o: ../../../../h/quipu/read.h
calls.o: ../../../../h/quipu/remove.h
calls.o: ../../../../h/quipu/util.h
calls.o: ../../../../h/tailor.h
calls.o: ../../../../h/usr.dirent.h
calls.o: ./filt.h
calls.o: ./sequence.h
calls.o: ./symtab.h
calls.o: ./y.tab.o
filt.o: ../../../../h/config.h
filt.o: ../../../../h/general.h
filt.o: ../../../../h/isoaddrs.h
filt.o: ../../../../h/logger.h
filt.o: ../../../../h/manifest.h
filt.o: ../../../../h/psap.h
filt.o: ../../../../h/quipu/abandon.h
filt.o: ../../../../h/quipu/add.h
filt.o: ../../../../h/quipu/attr.h
filt.o: ../../../../h/quipu/attrvalue.h
filt.o: ../../../../h/quipu/authen.h
filt.o: ../../../../h/quipu/bind.h
filt.o: ../../../../h/quipu/common.h
filt.o: ../../../../h/quipu/commonarg.h
filt.o: ../../../../h/quipu/compare.h
filt.o: ../../../../h/quipu/config.h
filt.o: ../../../../h/quipu/dap.h
filt.o: ../../../../h/quipu/ds_error.h
filt.o: ../../../../h/quipu/ds_search.h
filt.o: ../../../../h/quipu/dsp.h
filt.o: ../../../../h/quipu/entry.h
filt.o: ../../../../h/quipu/list.h
filt.o: ../../../../h/quipu/modify.h
filt.o: ../../../../h/quipu/modifyrdn.h
filt.o: ../../../../h/quipu/name.h
filt.o: ../../../../h/quipu/oid.h
filt.o: ../../../../h/quipu/read.h
filt.o: ../../../../h/quipu/remove.h
filt.o: ../../../../h/quipu/util.h
filt.o: ./filt.h
filt.o: ./symtab.h
filt.o: ./y.tab.o
main.o: ../../../../h/config.h
main.o: ../../../../h/general.h
main.o: ../../../../h/logger.h
main.o: ../../../../h/manifest.h
main.o: ../../../../h/quipu/config.h
main.o: ../../../../h/quipu/util.h
sequence.o: ./sequence.h
symtab.o: ./symtab.h
y.tab.o: ./filt.h
y.tab.o: ./symtab.h

