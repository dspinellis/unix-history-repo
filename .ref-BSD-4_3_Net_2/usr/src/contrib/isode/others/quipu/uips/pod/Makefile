###############################################################################
#
# $Header: /f/osi/others/quipu/uips/pod/RCS/Makefile,v 7.4 91/02/22 09:31:18 mrose Interim $
#
###############################################################################

#X app-defaults directory.
APPDEFDIR =	/usr/lib/X11/app-defaults

#yacc flags
YFLAGS  =	-d


###############################################################################
#
# Files and libraries.
#
###############################################################################

ISOLIBS =	$(TOPDIR)libdsap.a $(TOPDIR)libisode.a \
		$(TOPDIR)others/quipu/photo/libphoto.a

XLIBS   =	-lXaw -lXmu -lXt -lX11 -lXext

LIBES   =       $(ISOLIBS) $(XLIBS)
LLIBS   =       $(TOPDIR)llib-ldsap $(TOPDIR)llib-lisode

CFILES  =	y.tab.c calls.c main.c pod.c filt.c sequence.c \
		photo.c list.c read.c search.c modify.c dir_entry.c util.c
OFILES  =	y.tab.o calls.o main.o pod.o filt.o sequence.o \
		photo.o list.o read.o search.o modify.o dir_entry.o util.o
TESTOFILES =	y.tab.o testcalls.o main.o pod.o filt.o sequence.o \
		photo.o list.o read.o search.o modify.o dir_entry.o util.o

YFILES  =       conf_read.y

###############################################################################
# Here it is...
###############################################################################

all:            xpod
inst-all:       inst-pod manuals help appdefs config
install:        inst-all clean
lint:           l-pod


##############################################################################
# pod
##############################################################################


inst-pod:	$(BINDIR)pod 
		-mkdir $(ETCDIR)xd

$(BINDIR)pod:	xpod
	        -cp $@ zxpod
	        -rm -f $@
	        cp xpod $@
	        -@ls -gls $@
	        -@echo ""

xpod:  $(OFILES) $(ISOLIBS)
	$(LDCC) $(LDFLAGS) -o $@ $(OFILES) \
		$(LIBDSAP) $(TOPDIR)others/quipu/photo/libphoto.a $(LIBISODE) \
		$(XLIBS) $(LSOCKET)

l-pod: $(CFILES) 
	$(LINT) $(LFLAGS) $(LLIBS) $(CFILES) \
		| grep -v "warning: possible pointer alignment problem"

#testpod: $(TESTOFILES)
#	$(LDCC) $(LDFLAGS) -o $@ $(TESTOFILES) $(LIBES)	

#testcalls.o:
#	$(CC) $(CFLAGS) -DTESTPOD -c calls.c -o testcalls.o

y.tab.c: conf_read.y
	yacc $(YFLAGS) conf_read.y

###############################################################################
# config files
###############################################################################

config:; 	-mkdir $(ETCDIR)xd/duaconfig
		-cp -r Xd/duaconfig/* $(ETCDIR)xd/duaconfig
		-@echo ""

###############################################################################
# app-defaults
###############################################################################

appdefs:;	-cp Pod.ad $(APPDEFDIR)/Pod

###############################################################################
# manual pages
###############################################################################

MANUALS =       pod.1c

manuals:;
		@$(UTILDIR)inst-man.sh $(MANOPTS) pod.1c
		@$(UTILDIR)inst-man.sh $(MANOPTS) pod.5
		-@echo ""

###############################################################################
# help
###############################################################################

help:;		-mkdir $(ETCDIR)xd/podHelpdir
		cp Xd/podHelpdir/* $(ETCDIR)xd/podHelpdir
		-@ls -gls $(ETCDIR)xd/podHelpdir
		-@echo ""


###############################################################################
# clean
###############################################################################

clean:;         rm -f *.ph *.o *.a a.out _* x* z* *.orig \
			core $(CLEANSTRINGS) y.tab.c y.tab.h

grind:;         iprint Makefile
		tgrind -lc $(CFILES)
		@echo $(MANUALS) | \	
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;

# DO NOT DELETE BELOW THIS LINE
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
calls.o: ./defs.h
calls.o: ./filt.h
calls.o: ./sequence.h
calls.o: ./util.h
dir_entry.o: ./defs.h
dir_entry.o: ./dir_entry.h
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
filt.o: ./defs.h
filt.o: ./filt.h
filt.o: ./y.tab.h
list.o: ../../../../h/config.h
list.o: ../../../../h/general.h
list.o: ../../../../h/isoaddrs.h
list.o: ../../../../h/logger.h
list.o: ../../../../h/manifest.h
list.o: ../../../../h/psap.h
list.o: ../../../../h/quipu/abandon.h
list.o: ../../../../h/quipu/add.h
list.o: ../../../../h/quipu/attr.h
list.o: ../../../../h/quipu/attrvalue.h
list.o: ../../../../h/quipu/authen.h
list.o: ../../../../h/quipu/bind.h
list.o: ../../../../h/quipu/common.h
list.o: ../../../../h/quipu/commonarg.h
list.o: ../../../../h/quipu/compare.h
list.o: ../../../../h/quipu/config.h
list.o: ../../../../h/quipu/dap.h
list.o: ../../../../h/quipu/ds_error.h
list.o: ../../../../h/quipu/ds_search.h
list.o: ../../../../h/quipu/dsp.h
list.o: ../../../../h/quipu/entry.h
list.o: ../../../../h/quipu/list.h
list.o: ../../../../h/quipu/modify.h
list.o: ../../../../h/quipu/modifyrdn.h
list.o: ../../../../h/quipu/name.h
list.o: ../../../../h/quipu/oid.h
list.o: ../../../../h/quipu/read.h
list.o: ../../../../h/quipu/remove.h
list.o: ../../../../h/quipu/util.h
list.o: ./defs.h
list.o: ./filt.h
list.o: ./sequence.h
main.o: ../../../../h/config.h
main.o: ../../../../h/general.h
main.o: ../../../../h/isoaddrs.h
main.o: ../../../../h/logger.h
main.o: ../../../../h/manifest.h
main.o: ../../../../h/psap.h
main.o: ../../../../h/quipu/abandon.h
main.o: ../../../../h/quipu/add.h
main.o: ../../../../h/quipu/attr.h
main.o: ../../../../h/quipu/attrvalue.h
main.o: ../../../../h/quipu/authen.h
main.o: ../../../../h/quipu/bind.h
main.o: ../../../../h/quipu/common.h
main.o: ../../../../h/quipu/commonarg.h
main.o: ../../../../h/quipu/compare.h
main.o: ../../../../h/quipu/config.h
main.o: ../../../../h/quipu/dap.h
main.o: ../../../../h/quipu/ds_error.h
main.o: ../../../../h/quipu/ds_search.h
main.o: ../../../../h/quipu/dsp.h
main.o: ../../../../h/quipu/dua.h
main.o: ../../../../h/quipu/entry.h
main.o: ../../../../h/quipu/list.h
main.o: ../../../../h/quipu/modify.h
main.o: ../../../../h/quipu/modifyrdn.h
main.o: ../../../../h/quipu/name.h
main.o: ../../../../h/quipu/oid.h
main.o: ../../../../h/quipu/photo.h
main.o: ../../../../h/quipu/read.h
main.o: ../../../../h/quipu/remove.h
main.o: ../../../../h/quipu/util.h
main.o: ./defs.h
main.o: ./pod.h
main.o: ./sequence.h
modify.o: ../../../../h/config.h
modify.o: ../../../../h/general.h
modify.o: ../../../../h/isoaddrs.h
modify.o: ../../../../h/logger.h
modify.o: ../../../../h/manifest.h
modify.o: ../../../../h/psap.h
modify.o: ../../../../h/quipu/abandon.h
modify.o: ../../../../h/quipu/add.h
modify.o: ../../../../h/quipu/attr.h
modify.o: ../../../../h/quipu/attrvalue.h
modify.o: ../../../../h/quipu/authen.h
modify.o: ../../../../h/quipu/bind.h
modify.o: ../../../../h/quipu/common.h
modify.o: ../../../../h/quipu/commonarg.h
modify.o: ../../../../h/quipu/compare.h
modify.o: ../../../../h/quipu/config.h
modify.o: ../../../../h/quipu/dap.h
modify.o: ../../../../h/quipu/ds_error.h
modify.o: ../../../../h/quipu/ds_search.h
modify.o: ../../../../h/quipu/dsp.h
modify.o: ../../../../h/quipu/entry.h
modify.o: ../../../../h/quipu/list.h
modify.o: ../../../../h/quipu/modify.h
modify.o: ../../../../h/quipu/modifyrdn.h
modify.o: ../../../../h/quipu/name.h
modify.o: ../../../../h/quipu/oid.h
modify.o: ../../../../h/quipu/read.h
modify.o: ../../../../h/quipu/remove.h
modify.o: ../../../../h/quipu/util.h
modify.o: ./defs.h
modify.o: ./dir_entry.h
modify.o: ./sequence.h
modify.o: ./util.h
photo.o: ../../../../h/quipu/photo.h
photo.o: ./defs.h
photo.o: ./pod.h
photo.o: ./sequence.h
pod.o: ../../../../h/config.h
pod.o: ../../../../h/general.h
pod.o: ../../../../h/manifest.h
pod.o: ./bitmap
pod.o: ./bitmaps/About
pod.o: ./bitmaps/AboutPress
pod.o: ./bitmaps/Close
pod.o: ./bitmaps/ClosePress
pod.o: ./bitmaps/Help
pod.o: ./bitmaps/HelpPress
pod.o: ./bitmaps/History
pod.o: ./bitmaps/HistoryPress
pod.o: ./bitmaps/Keep
pod.o: ./bitmaps/KeepPress
pod.o: ./bitmaps/Kept
pod.o: ./bitmaps/List
pod.o: ./bitmaps/ListPress
pod.o: ./bitmaps/Modify
pod.o: ./bitmaps/ModifyPress
pod.o: ./bitmaps/Okay
pod.o: ./bitmaps/OkayPress
pod.o: ./bitmaps/Quit
pod.o: ./bitmaps/QuitPress
pod.o: ./bitmaps/Search
pod.o: ./bitmaps/SearchPress
pod.o: ./bitmaps/ShowAll
pod.o: ./bitmaps/ShowAllPress
pod.o: ./bitmaps/icon
pod.o: ./bitmaps/menuicon
pod.o: ./defs.h
pod.o: ./dir_entry.h
pod.o: ./pod.h
pod.o: ./sequence.h
read.o: ../../../../h/config.h
read.o: ../../../../h/general.h
read.o: ../../../../h/isoaddrs.h
read.o: ../../../../h/logger.h
read.o: ../../../../h/manifest.h
read.o: ../../../../h/psap.h
read.o: ../../../../h/quipu/abandon.h
read.o: ../../../../h/quipu/add.h
read.o: ../../../../h/quipu/attr.h
read.o: ../../../../h/quipu/attrvalue.h
read.o: ../../../../h/quipu/authen.h
read.o: ../../../../h/quipu/bind.h
read.o: ../../../../h/quipu/common.h
read.o: ../../../../h/quipu/commonarg.h
read.o: ../../../../h/quipu/compare.h
read.o: ../../../../h/quipu/config.h
read.o: ../../../../h/quipu/dap.h
read.o: ../../../../h/quipu/ds_error.h
read.o: ../../../../h/quipu/ds_search.h
read.o: ../../../../h/quipu/dsp.h
read.o: ../../../../h/quipu/entry.h
read.o: ../../../../h/quipu/list.h
read.o: ../../../../h/quipu/modify.h
read.o: ../../../../h/quipu/modifyrdn.h
read.o: ../../../../h/quipu/name.h
read.o: ../../../../h/quipu/oid.h
read.o: ../../../../h/quipu/read.h
read.o: ../../../../h/quipu/remove.h
read.o: ../../../../h/quipu/util.h
read.o: ./defs.h
read.o: ./sequence.h
read.o: ./util.h
search.o: ../../../../h/config.h
search.o: ../../../../h/general.h
search.o: ../../../../h/isoaddrs.h
search.o: ../../../../h/logger.h
search.o: ../../../../h/manifest.h
search.o: ../../../../h/psap.h
search.o: ../../../../h/quipu/abandon.h
search.o: ../../../../h/quipu/add.h
search.o: ../../../../h/quipu/attr.h
search.o: ../../../../h/quipu/attrvalue.h
search.o: ../../../../h/quipu/authen.h
search.o: ../../../../h/quipu/bind.h
search.o: ../../../../h/quipu/common.h
search.o: ../../../../h/quipu/commonarg.h
search.o: ../../../../h/quipu/compare.h
search.o: ../../../../h/quipu/config.h
search.o: ../../../../h/quipu/dap.h
search.o: ../../../../h/quipu/ds_error.h
search.o: ../../../../h/quipu/ds_search.h
search.o: ../../../../h/quipu/dsp.h
search.o: ../../../../h/quipu/entry.h
search.o: ../../../../h/quipu/list.h
search.o: ../../../../h/quipu/modify.h
search.o: ../../../../h/quipu/modifyrdn.h
search.o: ../../../../h/quipu/name.h
search.o: ../../../../h/quipu/oid.h
search.o: ../../../../h/quipu/read.h
search.o: ../../../../h/quipu/remove.h
search.o: ../../../../h/quipu/util.h
search.o: ./defs.h
search.o: ./filt.h
search.o: ./sequence.h
sequence.o: ./defs.h
sequence.o: ./sequence.h
util.o: ./util.h
y.tab.o: ./filt.h
