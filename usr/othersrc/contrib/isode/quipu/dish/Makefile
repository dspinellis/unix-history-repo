###############################################################################
#   Instructions to Make, for compilation of ISODE QUIPU Directory Server
###############################################################################

###############################################################################
#
# $Header: /f/osi/quipu/dish/RCS/Makefile,v 7.6 91/02/22 09:40:16 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.6  91/02/22  09:40:16  mrose
# Interim 6.8
# 
# Revision 7.5  90/12/23  18:42:39  mrose
# update
# 
# Revision 7.3  90/10/17  11:55:06  mrose
# sync
# 
# Revision 7.2  90/07/09  14:46:53  mrose
# sync
# 
# Revision 7.1  90/03/15  11:18:14  mrose
# quipu-sync
# 
# Revision 7.0  89/11/23  22:19:53  mrose
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


PEPYPATH=	-DPEPYPATH
LIBES   =       $(TOPDIR)libdsap.a $(TOPDIR)libisode.a
LLIBS   =       $(TOPDIR)llib-ldsap $(TOPDIR)llib-lisode

CFILES  =       add.c bind.c compare.c delete.c dishlib.c dishhelp.c \
		edit.c filteritem.c fred.c get_ava.c get_filter.c list.c \
		modify.c modifyrdn.c move.c read.c search.c showattr.c \
		showentry.c showname.c user.c pipe.c
OFILES  =       add.o bind.o compare.o delete.o dishlib.o dishhelp.o \
		edit.o filteritem.o fred.o get_ava.o get_filter.o list.o \
		modify.o modifyrdn.o move.o read.o search.o showattr.o \
		showentry.o showname.o user.o pipe.o

LGNUREADLINE=

# if using GNU Readline uncomment the following two macros
# If using shared libs you'll need to add a -PIC to LIBCFLAGS as well

#LGNUREADLINE=  -lreadline -ltermcap
#LIBCFLAGS=	$(CFLAGS) -DGNUREADLINE

# Static libdish
LIBDISH= libdish.a
# Shared libdish
#LIBDISH= -L. -ldish

##############################################################
# Here it is...
##############################################################

all:            libdish dish 
inst-all:       inst-dish inst-editentry inst-libdish manuals
install:        inst-all clean
lint:           l-dish


###################################################################
# dish
###################################################################

inst-dish:      $(BINDIR)dish

$(BINDIR)dish:	xdish
		-cp $@ zxdish
		-rm -f $@
		cp xdish $@
		-@ls -gls $@
		-@echo ""

dish:		xdish

xdish:		libdish.a dish.o $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ dish.o $(LIBDISH) \
			$(LIBDSAP) $(LIBISODE) $(LSOCKET) $(LGNUREADLINE) \
			$(LIBGDBM)

l-dish:         $(CFILES) dish.c true
		$(LINT) $(LFLAGS) $(CFILES) dish.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


###################################################################
# saber
###################################################################

saber_src:;	#load $(OPTIONS) $(CFILES) dish.c dishvrsn.c

saber_obj:;	#load libdish.a dish.o

saber_libs:;	#load $(LIBES)

###################################################################
# libdish
###################################################################

inst-libdish:	$(LIBDIR)libdish.a

$(LIBDIR)libdish.a:     libdish.a
		@for i in libdish.* ;\
		do \
			rm -f $(LIBDIR)$$i; \
			echo cp $$i $(LIBDIR)$$i; \
			cp $$i $(LIBDIR)$$i; \
			case "$$i" in *.a) \
			$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib ;;\
			esac; \
			ls -gls $(LIBDIR)$$i ; \
		done
		-@echo ""

libdish:       libdish.a libdish-$(SHAREDLIB)

libdish.a:     dishvrsn.o
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(OFILES) \
			dishvrsn.o
		-@echo "QUIPU-DISH library built normally"

libdish-:;

libdish-shared: dishvrsn.o
		@rm -f libdish.so.* $(TOPDIR)libdish.so.*
		@$(UTILDIR)make-lib.sh $(SYSTEM) -shared \
			-major `cat version.major``cat version.minor` \
			-minor `cat version.local` \
			libdish.a 
		@for i in libdish.s[ao].* ;\
		do \
			rm -f $(TOPDIR)$$i; \
			$(LN) $$i $(TOPDIR)$$i; \
			ls -l $$i; \
		done
		@echo "shared DISH library built normally"
		@touch $@
	

dishvrsn.c:    $(OFILES)
		@$(UTILDIR)version.sh dish > $@

l-libdish:     $(CFILES) true
		$(LINT) $(LFLAGS) $(CFILES) dishvrsn.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


################################################################
# edit entry
################################################################

inst-editentry:	$(BINDIR)editentry

$(BINDIR)editentry: editentry
		-cp $@ zeditentry
		-rm -f $@
		cp editentry $@
		chmod a+rx $@
		-@ls -gls $@
		-@echo ""


################################################################
# manual pages
################################################################

MANUALS =       dish.1c

manuals:;       @$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


##############################################################
# clean
##############################################################

clean:;         rm -f *.ph *.o *.a a.out _* x* z* *.orig core disvrsn.c

grind:;         iprint Makefile
		tgrind -lc $(CFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;

# DO NOT DELETE THIS LINE
# Dependencies follow
add.o: ../../h/config.h
add.o: ../../h/general.h
add.o: ../../h/isoaddrs.h
add.o: ../../h/logger.h
add.o: ../../h/manifest.h
add.o: ../../h/pepsy.h
add.o: ../../h/psap.h
add.o: ../../h/quipu/DAS_pre_defs.h
add.o: ../../h/quipu/add.h
add.o: ../../h/quipu/attr.h
add.o: ../../h/quipu/attrvalue.h
add.o: ../../h/quipu/authen.h
add.o: ../../h/quipu/commonarg.h
add.o: ../../h/quipu/config.h
add.o: ../../h/quipu/dap.h
add.o: ../../h/quipu/ds_error.h
add.o: ../../h/quipu/dsp.h
add.o: ../../h/quipu/dua.h
add.o: ../../h/quipu/entry.h
add.o: ../../h/quipu/name.h
add.o: ../../h/quipu/oid.h
add.o: ../../h/quipu/turbo.h
add.o: ../../h/quipu/util.h
bind.o: ../../h/config.h
bind.o: ../../h/general.h
bind.o: ../../h/isoaddrs.h
bind.o: ../../h/logger.h
bind.o: ../../h/manifest.h
bind.o: ../../h/pepsy.h
bind.o: ../../h/psap.h
bind.o: ../../h/quipu/DAS_pre_defs.h
bind.o: ../../h/quipu/attr.h
bind.o: ../../h/quipu/authen.h
bind.o: ../../h/quipu/bind.h
bind.o: ../../h/quipu/config.h
bind.o: ../../h/quipu/ds_error.h
bind.o: ../../h/quipu/dsp.h
bind.o: ../../h/quipu/dua.h
bind.o: ../../h/quipu/name.h
bind.o: ../../h/quipu/oid.h
bind.o: ../../h/quipu/util.h
bind.o: ../../h/tailor.h
compare.o: ../../h/config.h
compare.o: ../../h/general.h
compare.o: ../../h/isoaddrs.h
compare.o: ../../h/logger.h
compare.o: ../../h/manifest.h
compare.o: ../../h/pepsy.h
compare.o: ../../h/psap.h
compare.o: ../../h/quipu/DAS_pre_defs.h
compare.o: ../../h/quipu/attr.h
compare.o: ../../h/quipu/attrvalue.h
compare.o: ../../h/quipu/authen.h
compare.o: ../../h/quipu/commonarg.h
compare.o: ../../h/quipu/compare.h
compare.o: ../../h/quipu/config.h
compare.o: ../../h/quipu/dap.h
compare.o: ../../h/quipu/ds_error.h
compare.o: ../../h/quipu/dsp.h
compare.o: ../../h/quipu/name.h
compare.o: ../../h/quipu/oid.h
compare.o: ../../h/quipu/util.h
delete.o: ../../h/config.h
delete.o: ../../h/general.h
delete.o: ../../h/isoaddrs.h
delete.o: ../../h/logger.h
delete.o: ../../h/manifest.h
delete.o: ../../h/pepsy.h
delete.o: ../../h/psap.h
delete.o: ../../h/quipu/DAS_pre_defs.h
delete.o: ../../h/quipu/attr.h
delete.o: ../../h/quipu/attrvalue.h
delete.o: ../../h/quipu/authen.h
delete.o: ../../h/quipu/commonarg.h
delete.o: ../../h/quipu/config.h
delete.o: ../../h/quipu/dap.h
delete.o: ../../h/quipu/ds_error.h
delete.o: ../../h/quipu/dsp.h
delete.o: ../../h/quipu/name.h
delete.o: ../../h/quipu/oid.h
delete.o: ../../h/quipu/remove.h
delete.o: ../../h/quipu/util.h
dish.o: ../../h/config.h
dish.o: ../../h/general.h
dish.o: ../../h/logger.h
dish.o: ../../h/manifest.h
dish.o: ../../h/quipu/config.h
dish.o: ../../h/quipu/util.h
dishhelp.o: ../../h/config.h
dishhelp.o: ../../h/general.h
dishhelp.o: ../../h/logger.h
dishhelp.o: ../../h/manifest.h
dishhelp.o: ../../h/psap.h
dishhelp.o: ../../h/quipu/attr.h
dishhelp.o: ../../h/quipu/config.h
dishhelp.o: ../../h/quipu/name.h
dishhelp.o: ../../h/quipu/oid.h
dishhelp.o: ../../h/quipu/util.h
dishlib.o: ../../h/config.h
dishlib.o: ../../h/dgram.h
dishlib.o: ../../h/general.h
dishlib.o: ../../h/internet.h
dishlib.o: ../../h/logger.h
dishlib.o: ../../h/manifest.h
dishlib.o: ../../h/psap.h
dishlib.o: ../../h/quipu/attr.h
dishlib.o: ../../h/quipu/config.h
dishlib.o: ../../h/quipu/name.h
dishlib.o: ../../h/quipu/oid.h
dishlib.o: ../../h/quipu/util.h
edit.o: ../../h/config.h
edit.o: ../../h/general.h
edit.o: ../../h/logger.h
edit.o: ../../h/manifest.h
edit.o: ../../h/psap.h
edit.o: ../../h/quipu/config.h
edit.o: ../../h/quipu/util.h
edit.o: ../../h/tailor.h
filteritem.o: ../../h/config.h
filteritem.o: ../../h/general.h
filteritem.o: ../../h/isoaddrs.h
filteritem.o: ../../h/logger.h
filteritem.o: ../../h/manifest.h
filteritem.o: ../../h/psap.h
filteritem.o: ../../h/quipu/attr.h
filteritem.o: ../../h/quipu/attrvalue.h
filteritem.o: ../../h/quipu/authen.h
filteritem.o: ../../h/quipu/commonarg.h
filteritem.o: ../../h/quipu/config.h
filteritem.o: ../../h/quipu/dap.h
filteritem.o: ../../h/quipu/ds_error.h
filteritem.o: ../../h/quipu/ds_search.h
filteritem.o: ../../h/quipu/dsp.h
filteritem.o: ../../h/quipu/name.h
filteritem.o: ../../h/quipu/oid.h
filteritem.o: ../../h/quipu/util.h
fred.o: ../../h/config.h
fred.o: ../../h/general.h
fred.o: ../../h/isoaddrs.h
fred.o: ../../h/logger.h
fred.o: ../../h/manifest.h
fred.o: ../../h/psap.h
fred.o: ../../h/quipu/attr.h
fred.o: ../../h/quipu/attrvalue.h
fred.o: ../../h/quipu/authen.h
fred.o: ../../h/quipu/commonarg.h
fred.o: ../../h/quipu/config.h
fred.o: ../../h/quipu/dap.h
fred.o: ../../h/quipu/ds_error.h
fred.o: ../../h/quipu/ds_search.h
fred.o: ../../h/quipu/dsp.h
fred.o: ../../h/quipu/entry.h
fred.o: ../../h/quipu/list.h
fred.o: ../../h/quipu/name.h
fred.o: ../../h/quipu/oid.h
fred.o: ../../h/quipu/read.h
fred.o: ../../h/quipu/ufn.h
fred.o: ../../h/quipu/util.h
fred.o: ../../h/tailor.h
get_ava.o: ../../h/config.h
get_ava.o: ../../h/general.h
get_ava.o: ../../h/isoaddrs.h
get_ava.o: ../../h/logger.h
get_ava.o: ../../h/manifest.h
get_ava.o: ../../h/psap.h
get_ava.o: ../../h/quipu/attr.h
get_ava.o: ../../h/quipu/attrvalue.h
get_ava.o: ../../h/quipu/authen.h
get_ava.o: ../../h/quipu/commonarg.h
get_ava.o: ../../h/quipu/config.h
get_ava.o: ../../h/quipu/dsp.h
get_ava.o: ../../h/quipu/name.h
get_ava.o: ../../h/quipu/oid.h
get_ava.o: ../../h/quipu/util.h
get_filter.o: ../../h/config.h
get_filter.o: ../../h/general.h
get_filter.o: ../../h/isoaddrs.h
get_filter.o: ../../h/logger.h
get_filter.o: ../../h/manifest.h
get_filter.o: ../../h/psap.h
get_filter.o: ../../h/quipu/attr.h
get_filter.o: ../../h/quipu/attrvalue.h
get_filter.o: ../../h/quipu/authen.h
get_filter.o: ../../h/quipu/commonarg.h
get_filter.o: ../../h/quipu/config.h
get_filter.o: ../../h/quipu/dap.h
get_filter.o: ../../h/quipu/ds_error.h
get_filter.o: ../../h/quipu/ds_search.h
get_filter.o: ../../h/quipu/dsp.h
get_filter.o: ../../h/quipu/name.h
get_filter.o: ../../h/quipu/oid.h
get_filter.o: ../../h/quipu/util.h
list.o: ../../h/config.h
list.o: ../../h/general.h
list.o: ../../h/isoaddrs.h
list.o: ../../h/logger.h
list.o: ../../h/manifest.h
list.o: ../../h/pepsy.h
list.o: ../../h/psap.h
list.o: ../../h/quipu/DAS_pre_defs.h
list.o: ../../h/quipu/attr.h
list.o: ../../h/quipu/attrvalue.h
list.o: ../../h/quipu/authen.h
list.o: ../../h/quipu/commonarg.h
list.o: ../../h/quipu/config.h
list.o: ../../h/quipu/dap.h
list.o: ../../h/quipu/ds_error.h
list.o: ../../h/quipu/dsp.h
list.o: ../../h/quipu/list.h
list.o: ../../h/quipu/name.h
list.o: ../../h/quipu/oid.h
list.o: ../../h/quipu/sequence.h
list.o: ../../h/quipu/util.h
modify.o: ../../h/config.h
modify.o: ../../h/general.h
modify.o: ../../h/isoaddrs.h
modify.o: ../../h/logger.h
modify.o: ../../h/manifest.h
modify.o: ../../h/pepsy.h
modify.o: ../../h/psap.h
modify.o: ../../h/quipu/DAS_pre_defs.h
modify.o: ../../h/quipu/attr.h
modify.o: ../../h/quipu/attrvalue.h
modify.o: ../../h/quipu/authen.h
modify.o: ../../h/quipu/commonarg.h
modify.o: ../../h/quipu/config.h
modify.o: ../../h/quipu/dap.h
modify.o: ../../h/quipu/ds_error.h
modify.o: ../../h/quipu/dsp.h
modify.o: ../../h/quipu/dua.h
modify.o: ../../h/quipu/entry.h
modify.o: ../../h/quipu/modify.h
modify.o: ../../h/quipu/name.h
modify.o: ../../h/quipu/oid.h
modify.o: ../../h/quipu/read.h
modify.o: ../../h/quipu/turbo.h
modify.o: ../../h/quipu/util.h
modifyrdn.o: ../../h/config.h
modifyrdn.o: ../../h/general.h
modifyrdn.o: ../../h/isoaddrs.h
modifyrdn.o: ../../h/logger.h
modifyrdn.o: ../../h/manifest.h
modifyrdn.o: ../../h/pepsy.h
modifyrdn.o: ../../h/psap.h
modifyrdn.o: ../../h/quipu/DAS_pre_defs.h
modifyrdn.o: ../../h/quipu/attr.h
modifyrdn.o: ../../h/quipu/attrvalue.h
modifyrdn.o: ../../h/quipu/authen.h
modifyrdn.o: ../../h/quipu/commonarg.h
modifyrdn.o: ../../h/quipu/config.h
modifyrdn.o: ../../h/quipu/dap.h
modifyrdn.o: ../../h/quipu/ds_error.h
modifyrdn.o: ../../h/quipu/dsp.h
modifyrdn.o: ../../h/quipu/modifyrdn.h
modifyrdn.o: ../../h/quipu/name.h
modifyrdn.o: ../../h/quipu/oid.h
modifyrdn.o: ../../h/quipu/util.h
move.o: ../../h/config.h
move.o: ../../h/general.h
move.o: ../../h/logger.h
move.o: ../../h/manifest.h
move.o: ../../h/psap.h
move.o: ../../h/quipu/attr.h
move.o: ../../h/quipu/config.h
move.o: ../../h/quipu/name.h
move.o: ../../h/quipu/oid.h
move.o: ../../h/quipu/util.h
pipe.o: ../../h/config.h
pipe.o: ../../h/dgram.h
pipe.o: ../../h/general.h
pipe.o: ../../h/internet.h
pipe.o: ../../h/logger.h
pipe.o: ../../h/manifest.h
pipe.o: ../../h/tailor.h
read.o: ../../h/config.h
read.o: ../../h/general.h
read.o: ../../h/isoaddrs.h
read.o: ../../h/logger.h
read.o: ../../h/manifest.h
read.o: ../../h/pepsy.h
read.o: ../../h/psap.h
read.o: ../../h/quipu/DAS_pre_defs.h
read.o: ../../h/quipu/attr.h
read.o: ../../h/quipu/attrvalue.h
read.o: ../../h/quipu/authen.h
read.o: ../../h/quipu/commonarg.h
read.o: ../../h/quipu/config.h
read.o: ../../h/quipu/dap.h
read.o: ../../h/quipu/ds_error.h
read.o: ../../h/quipu/dsp.h
read.o: ../../h/quipu/entry.h
read.o: ../../h/quipu/name.h
read.o: ../../h/quipu/oid.h
read.o: ../../h/quipu/read.h
read.o: ../../h/quipu/turbo.h
read.o: ../../h/quipu/util.h
search.o: ../../h/config.h
search.o: ../../h/general.h
search.o: ../../h/isoaddrs.h
search.o: ../../h/logger.h
search.o: ../../h/manifest.h
search.o: ../../h/pepsy.h
search.o: ../../h/psap.h
search.o: ../../h/quipu/DAS_pre_defs.h
search.o: ../../h/quipu/attr.h
search.o: ../../h/quipu/attrvalue.h
search.o: ../../h/quipu/authen.h
search.o: ../../h/quipu/commonarg.h
search.o: ../../h/quipu/config.h
search.o: ../../h/quipu/dap.h
search.o: ../../h/quipu/ds_error.h
search.o: ../../h/quipu/ds_search.h
search.o: ../../h/quipu/dsp.h
search.o: ../../h/quipu/entry.h
search.o: ../../h/quipu/list.h
search.o: ../../h/quipu/name.h
search.o: ../../h/quipu/oid.h
search.o: ../../h/quipu/sequence.h
search.o: ../../h/quipu/turbo.h
search.o: ../../h/quipu/util.h
showattr.o: ../../h/config.h
showattr.o: ../../h/general.h
showattr.o: ../../h/isoaddrs.h
showattr.o: ../../h/logger.h
showattr.o: ../../h/manifest.h
showattr.o: ../../h/psap.h
showattr.o: ../../h/quipu/attr.h
showattr.o: ../../h/quipu/attrvalue.h
showattr.o: ../../h/quipu/config.h
showattr.o: ../../h/quipu/dsp.h
showattr.o: ../../h/quipu/entry.h
showattr.o: ../../h/quipu/name.h
showattr.o: ../../h/quipu/oid.h
showattr.o: ../../h/quipu/turbo.h
showattr.o: ../../h/quipu/util.h
showentry.o: ../../h/config.h
showentry.o: ../../h/general.h
showentry.o: ../../h/isoaddrs.h
showentry.o: ../../h/logger.h
showentry.o: ../../h/manifest.h
showentry.o: ../../h/psap.h
showentry.o: ../../h/quipu/attr.h
showentry.o: ../../h/quipu/attrvalue.h
showentry.o: ../../h/quipu/authen.h
showentry.o: ../../h/quipu/commonarg.h
showentry.o: ../../h/quipu/config.h
showentry.o: ../../h/quipu/dap.h
showentry.o: ../../h/quipu/ds_error.h
showentry.o: ../../h/quipu/dsp.h
showentry.o: ../../h/quipu/dua.h
showentry.o: ../../h/quipu/entry.h
showentry.o: ../../h/quipu/name.h
showentry.o: ../../h/quipu/oid.h
showentry.o: ../../h/quipu/read.h
showentry.o: ../../h/quipu/turbo.h
showentry.o: ../../h/quipu/util.h
showname.o: ../../h/config.h
showname.o: ../../h/general.h
showname.o: ../../h/logger.h
showname.o: ../../h/manifest.h
showname.o: ../../h/psap.h
showname.o: ../../h/quipu/attr.h
showname.o: ../../h/quipu/config.h
showname.o: ../../h/quipu/name.h
showname.o: ../../h/quipu/oid.h
showname.o: ../../h/quipu/util.h
user.o: ../../h/config.h
user.o: ../../h/general.h
user.o: ../../h/isoaddrs.h
user.o: ../../h/logger.h
user.o: ../../h/manifest.h
user.o: ../../h/psap.h
user.o: ../../h/quipu/attr.h
user.o: ../../h/quipu/attrvalue.h
user.o: ../../h/quipu/authen.h
user.o: ../../h/quipu/commonarg.h
user.o: ../../h/quipu/config.h
user.o: ../../h/quipu/dap.h
user.o: ../../h/quipu/ds_error.h
user.o: ../../h/quipu/dsp.h
user.o: ../../h/quipu/name.h
user.o: ../../h/quipu/oid.h
user.o: ../../h/quipu/read.h
user.o: ../../h/quipu/sequence.h
user.o: ../../h/quipu/util.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
