##############################################################################
#   Instructions to Make, for compilation of ISODE QUIPU Directory Server
###############################################################################

###############################################################################
#
# $Header: /f/osi/others/quipu/uips/sd/RCS/Makefile,v 7.10 91/03/09 11:55:02 mrose Exp $
#
#
# $Log:	Makefile,v $
# Revision 7.10  91/03/09  11:55:02  mrose
# update
# 
# Revision 7.9  91/02/22  09:32:06  mrose
# Interim 6.8
# 
# Revision 7.8  90/12/23  18:46:54  mrose
# update
# 
# Revision 7.7  90/11/20  15:33:52  mrose
# update
# 
# Revision 7.6  90/10/17  12:32:20  mrose
# sync
# 
# Revision 7.5  90/10/17  12:31:55  mrose
# sync
# 
# Revision 7.4  90/10/17  12:31:32  mrose
# sync
# 
# Revision 7.3  90/10/17  11:49:35  mrose
# sync
# 
# Revision 7.2  90/07/27  08:45:39  mrose
# update
# 
# Revision 7.1  90/07/09  14:41:45  mrose
# sync
# 
# Revision 7.0  90/06/12  13:14:00  mrose
# *** empty log message ***
# 
# Revision 1.4  90/04/20  17:58:24  emsrdsm
# i) no more freeing
# 
# Revision 1.3  90/04/18  18:28:51  emsrdsm
# fixed i) does not move to leaves
#       ii) added default mechanism described using "typeDefaults" file.
# 	 iii) added 'sorting' to attribute display
# 
# Revision 1.2  90/03/15  16:32:18  emsrdsm
# fixes i) Prints messages correctly on exit.
#      ii) Added rfc822 to greybook mailbox conversion
#     iii) Removed bug that caused crash if 'local_dit' undefined
# 
# Revision 1.1  90/03/09  18:13:38  emsrdsm
# Initial revision
# 
# Revision 1.1  90/03/09  17:41:27  emsrdsm
# Initial revision
# 
# Revision 1.1  90/03/09  13:40:09  emsrdsm
# Initial revision
# 
#
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

LIBES   =       $(TOPDIR)libdsap.a $(TOPDIR)libisode.a
LLIBS   =       $(TOPDIR)llib-ldsap $(TOPDIR)llib-lisode
CFILES  =	y.tab.c calls.c main.c widget.c help.c sequence.c filt.c
OFILES  =	y.tab.o calls.o main.o widget.o help.o sequence.o filt.o
YFILES  =       conf_read.y

CURSES  = 	-lcurses -ltermcap 
YFLAGS  =	-d

##############################################################
# Here it is...
##############################################################

all:            sd
inst-all:       inst-sd manuals help config
install:        inst-all clean
lint:           l-sd


###################################################################
# sd
###################################################################


inst-sd:    $(BINDIR)sd 
		-mkdir $(ETCDIR)sd

$(BINDIR)sd: xsd
	        -cp $@ zxsd
	        -rm -f $@
	        cp xsd $@
	        -@ls -gls $@
	        -@echo ""

sd:         xsd

xsd:	$(OFILES) $(LIBES)
	$(LDCC) $(LDFLAGS) -o $@ $(OFILES) $(LIBDSAP) $(LIBISODE) $(CURSES) \
		$(LSOCKET)

l-sd:   $(CFILES)  true
		$(LINT) $(LFLAGS) $(LLIBS) -lcurses $(CFILES) \
			| grep -v "warning: possible pointer alignment problem"

help.o:	help.c
		$(CC) $(CFLAGS) -c -DETCDIR=\"$(ETCDIR)\" help.c

y.tab.c y.tab.h: conf_read.y
		yacc $(YFLAGS) conf_read.y

################################################################
# config files
################################################################

config:;	-mkdir $(ETCDIR)sd/duaconfig
		cp -r sd/duaconfig/* $(ETCDIR)sd/duaconfig
		-@echo ""

################################################################
# manual pages
################################################################

MANUALS =       sd.1c sd.5

manuals:;       @$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""

##############################################################
# help
##############################################################

help:
		-mkdir $(ETCDIR)sd/helpdir
		cp sd/helpdir/* $(ETCDIR)sd/helpdir
		-@echo ""


##############################################################
# clean
##############################################################

clean:;         rm -f *.ph *.o *.a a.out _* x* z* *.orig y.tab.* \
			core $(CLEANSTRINGS)

grind:;         iprint Makefile
		tgrind -lc $(CFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;

# DO NOT DELETE THIS LINE
# Dependencies follow
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
calls.o: ./calls.h
calls.o: ./filt.h
calls.o: ./sequence.h
calls.o: ./wdgtdefs.h
calls.o: ./widget.h
calls.o: y.tab.h
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
filt.o: y.tab.h
help.o: ../../../../h/config.h
help.o: ../../../../h/general.h
help.o: ../../../../h/logger.h
help.o: ../../../../h/manifest.h
help.o: ../../../../h/quipu/config.h
help.o: ../../../../h/quipu/util.h
help.o: ../../../../h/tailor.h
help.o: ./sequence.h
main.o: ../../../../h/config.h
main.o: ../../../../h/general.h
main.o: ../../../../h/logger.h
main.o: ../../../../h/manifest.h
main.o: ../../../../h/quipu/config.h
main.o: ../../../../h/quipu/util.h
main.o: ./widget.h
sequence.o: ./sequence.h
symtab.o: ./symtab.h
widget.o: ./widget.h
y.tab.o: ../../../../h/config.h
y.tab.o: ../../../../h/general.h
y.tab.o: ../../../../h/isoaddrs.h
y.tab.o: ../../../../h/manifest.h
y.tab.o: ../../../../h/psap.h
y.tab.o: ../../../../h/quipu/attr.h
y.tab.o: ../../../../h/quipu/attrvalue.h
y.tab.o: ../../../../h/quipu/authen.h
y.tab.o: ../../../../h/quipu/commonarg.h
y.tab.o: ../../../../h/quipu/dap.h
y.tab.o: ../../../../h/quipu/ds_error.h
y.tab.o: ../../../../h/quipu/ds_search.h
y.tab.o: ../../../../h/quipu/dsp.h
y.tab.o: ../../../../h/quipu/name.h
y.tab.o: ../../../../h/quipu/oid.h
y.tab.o: ./filt.h
