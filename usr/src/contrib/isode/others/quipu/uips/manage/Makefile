###############################################################################
#   Instructions to Make, for compilation of Management dish
###############################################################################

###############################################################################
#
# $Header: /f/osi/others/quipu/uips/manage/RCS/Makefile,v 7.6 91/02/22 09:31:58 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.6  91/02/22  09:31:58  mrose
# Interim 6.8
# 
# Revision 7.5  91/01/24  14:43:09  mrose
# update
# 
# Revision 7.1  90/07/09  14:42:37  mrose
# sync
# 
# Revision 7.0  90/06/26  14:52:30  mrose
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

LIBDISH =       $(TOPDIR)quipu/dish/libdish.a
# sharded option...
# LIBDISH = 	-L $(TOPDIR)quipu/dish -ldish

LIBES   =       libmanage.a $(LIBDISH) $(LIBDSAP) $(LIBISODE) $(LSOCKET)
LLIBS   =       $(TOPDIR)llib-ldsap $(TOPDIR)llib-lisode

CFILES  =       add_alias.c del_alias.c alias_chk.c
OFILES  =       add_alias.o del_alias.o alias_chk.o \
		schema.o 

##############################################################
# Here it is...
##############################################################

all:            dish 
inst-all:       inst-dish manuals # inst-shelldish
install:        inst-all clean
lint:           l-dish


###################################################################
# dish
###################################################################

inst-dish:      $(BINDIR)dish_manage

$(BINDIR)dish_manage:	xdish
		-cp $@ zxdish_manage
		-rm -f $@
		cp xdish $@
		-@ls -gls $@
		-@echo ""

dish:		xdish

xdish:		dish.o libmanage.a
		$(LDCC) $(LDFLAGS) -o $@ dish.o $(LIBES) $(LSOCKET) 

l-dish:         $(CFILES) true
		$(LINT) $(LFLAGS) $(LLIBS) $(CFILES)

dish.o:		$(TOPDIR)quipu/dish/dish.c
		$(CC) $(CFLAGS) -DMANAGE -c $(TOPDIR)quipu/dish/dish.c

schema.o:	$(TOPDIR)/quipu/schema.c
		$(CC) $(CFLAGS) -c $(TOPDIR)quipu/schema.c

shadow.o:	$(TOPDIR)/quipu/shadow.c
		$(CC) $(CFLAGS) -c $(TOPDIR)quipu/shadow.c

##############################################################
# saber
##############################################################

saber_src:;     #load $(OPTIONS) $(CFILES)
		#load $(CFLAGS) -DMANAGE -c $(TOPDIR)quipu/dish/dish.c
		#load libmanage.a ../../../../quipu/dish/libdish.a ../../../../libdsap.a ../../../../libisode.a

saber_obj:;     #load $(OFILES)

###################################################################
# libmanage
###################################################################

inst-libmanage:	$(LIBDIR)libmanage.a

$(LIBDIR)libmanage.a:     libmanage.a
		-rm -f $@
		cp libmanage.a $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib
		-@ls -gls $@
		-@echo ""

libmanage:       libmanage.a

libmanage.a:     managevrsn.o
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(OFILES) \
			managevrsn.o
		-@echo "QUIPU-MANAGE library built normally"

managevrsn.c:    $(OFILES)
		@$(UTILDIR)version.sh manage > $@

l-libmanage:     $(CFILES) true
		$(LINT) $(LFLAGS) $(CFILES) managevrsn.c $(LLIBS)

###################################################################
# shell version of dish...
###################################################################

inst-shelldish:	$(BINDIR)add_alias $(BINDIR)del_alias $(BINDIR)dish

$(BINDIR)add_alias:
		-rm -f $@
		ln $(BINDIR)list $@

$(BINDIR)del_alias:
		-rm -f $@
		ln $(BINDIR)list $@
		
$(BINDIR)dish:	$(BINDIR)dish_manage
		-cp $@ zxdish
		-rm -f $@
		cp xdish $@
		-@ls -gls $@
		-@echo ""

##############################################################
# manuals
##############################################################

manuals:;       echo
		@$(UTILDIR)inst-man.sh $(MANOPTS) add_alias.1c

##############################################################
# clean
##############################################################

clean:;         rm -f *.ph *.o *.a a.out _* x* z* *.orig \
			core managevrsn.c

grind:;         iprint Makefile
		tgrind -lc $(CFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;


# depend
add_alias.o: ../../../../h/config.h
add_alias.o: ../../../../h/general.h
add_alias.o: ../../../../h/isoaddrs.h
add_alias.o: ../../../../h/logger.h
add_alias.o: ../../../../h/manifest.h
add_alias.o: ../../../../h/psap.h
add_alias.o: ../../../../h/quipu/add.h
add_alias.o: ../../../../h/quipu/attr.h
add_alias.o: ../../../../h/quipu/attrvalue.h
add_alias.o: ../../../../h/quipu/authen.h
add_alias.o: ../../../../h/quipu/commonarg.h
add_alias.o: ../../../../h/quipu/compare.h
add_alias.o: ../../../../h/quipu/config.h
add_alias.o: ../../../../h/quipu/dap.h
add_alias.o: ../../../../h/quipu/ds_error.h
add_alias.o: ../../../../h/quipu/dsp.h
add_alias.o: ../../../../h/quipu/dua.h
add_alias.o: ../../../../h/quipu/entry.h
add_alias.o: ../../../../h/quipu/modify.h
add_alias.o: ../../../../h/quipu/name.h
add_alias.o: ../../../../h/quipu/oid.h
add_alias.o: ../../../../h/quipu/turbo.h
add_alias.o: ../../../../h/quipu/util.h
alias_chk.o: ../../../../h/config.h
alias_chk.o: ../../../../h/general.h
alias_chk.o: ../../../../h/isoaddrs.h
alias_chk.o: ../../../../h/logger.h
alias_chk.o: ../../../../h/manifest.h
alias_chk.o: ../../../../h/psap.h
alias_chk.o: ../../../../h/quipu/attr.h
alias_chk.o: ../../../../h/quipu/attrvalue.h
alias_chk.o: ../../../../h/quipu/authen.h
alias_chk.o: ../../../../h/quipu/commonarg.h
alias_chk.o: ../../../../h/quipu/compare.h
alias_chk.o: ../../../../h/quipu/config.h
alias_chk.o: ../../../../h/quipu/dap.h
alias_chk.o: ../../../../h/quipu/ds_error.h
alias_chk.o: ../../../../h/quipu/ds_search.h
alias_chk.o: ../../../../h/quipu/dsp.h
alias_chk.o: ../../../../h/quipu/entry.h
alias_chk.o: ../../../../h/quipu/name.h
alias_chk.o: ../../../../h/quipu/oid.h
alias_chk.o: ../../../../h/quipu/read.h
alias_chk.o: ../../../../h/quipu/util.h
del_alias.o: ../../../../h/config.h
del_alias.o: ../../../../h/general.h
del_alias.o: ../../../../h/isoaddrs.h
del_alias.o: ../../../../h/logger.h
del_alias.o: ../../../../h/manifest.h
del_alias.o: ../../../../h/psap.h
del_alias.o: ../../../../h/quipu/attr.h
del_alias.o: ../../../../h/quipu/attrvalue.h
del_alias.o: ../../../../h/quipu/authen.h
del_alias.o: ../../../../h/quipu/commonarg.h
del_alias.o: ../../../../h/quipu/compare.h
del_alias.o: ../../../../h/quipu/config.h
del_alias.o: ../../../../h/quipu/dap.h
del_alias.o: ../../../../h/quipu/ds_error.h
del_alias.o: ../../../../h/quipu/dsp.h
del_alias.o: ../../../../h/quipu/entry.h
del_alias.o: ../../../../h/quipu/modify.h
del_alias.o: ../../../../h/quipu/name.h
del_alias.o: ../../../../h/quipu/oid.h
del_alias.o: ../../../../h/quipu/remove.h
del_alias.o: ../../../../h/quipu/turbo.h
del_alias.o: ../../../../h/quipu/util.h
