###############################################################################
#   Instructions to Make, for compilation of ISODE FTAM2 processes
###############################################################################

###############################################################################
#
# $Header: /f/osi/ftam2/RCS/Makefile,v 7.7 91/02/22 09:23:35 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.7  91/02/22  09:23:35  mrose
# Interim 6.8
# 
# Revision 7.6  90/12/23  18:39:55  mrose
# update
# 
# Revision 7.5  90/11/21  11:30:21  mrose
# sun
# 
# Revision 7.4  90/10/17  14:39:13  mrose
# update
# 
# Revision 7.3  90/07/27  08:44:39  mrose
# update
# 
# Revision 7.2  90/07/01  21:03:05  mrose
# pepsy
# 
# Revision 7.1  90/03/06  13:56:34  mrose
# touch-up
# 
# Revision 7.0  89/11/23  21:54:13  mrose
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


PEPYPATH=	-DPEPYPATH

.SUFFIXES:	.py .c .o

.c.o:;		$(CC) $(CFLAGS) -c $*.c


LIBD	=	$(TOPDIR)libftam.a $(TOPDIR)libisode.a 
LIBES	=	$(TOPDIR)libftam.a $(LIBISODE)
LLIBS	=	$(TOPDIR)llib-lftam $(TOPDIR)llib-lisode
CFILES	=	ftamd.c ftamsystem.c ftamd-manage.c ftamd-select.c \
			ftamd-trans.c \
		ftam.c ftamuser.c ftam-assoc.c ftam-dir.c ftam-get.c \
			ftam-glob.c ftam-ls.c ftam-mgmt.c ftam-put.c \
		ftamsbr.c
PYFILES	=	docs.py
HFILES	=	$(HDIR)ftam.h \
		$(HDIR)acsap.h $(HDIR)psap2.h $(HDIR)psap.h $(HDIR)ssap.h \
		$(HDIR)isoaddrs.h \
		$(HDIR)manifest.h $(HDIR)general.h $(HDIR)config.h


##################################################################
# Here it is...
##################################################################

all:		ftamd ftam
inst-all:	inst-ftamd inst-ftam manuals
install:	inst-all clean
lint:		l-ftamd l-ftam


##################################################################
# ftamd
##################################################################

inst-ftamd:	$(SBINDIR)iso.ftam

$(SBINDIR)iso.ftam:	xftamd
		-cp $@ ziso.ftam
		-rm -f $@
		cp xftamd $@
		-@ls -gls $@
		-@echo ""

ftamd:		xftamd

xftamd:		ftamd.o ftamsystem.o ftamd-manage.o ftamd-select.o \
			ftamd-trans.o ftamsbr.o DOCS_tables.o $(LIBD)
		$(LDCC) $(LDFLAGS) -o $@ ftamd.o ftamsystem.o ftamd-manage.o \
			ftamd-select.o ftamd-trans.o ftamsbr.o DOCS_tables.o \
			$(LIBES) $(LSOCKET)

l-ftamd:	DOCS_tables.c true
		$(LINT) $(LFLAGS) ftamd.c ftamsystem.c ftamd-manage.c \
			ftamd-select.c ftamd-trans.c ftamsbr.c DOCS_tables.c \
			$(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

ftamd.o:	ftamsystem.h ftamsbr.h DOCS-types.h $(HFILES)
ftamsystem.o:	ftamsystem.h ftamsbr.h DOCS-types.h $(HFILES) $(HDIR)logger.h
ftamd-manage.o:	ftamsystem.h ftamsbr.h DOCS-types.h $(HFILES)
ftamd-select.o:	ftamsystem.h ftamsbr.h DOCS-types.h $(HFILES)
ftamd-trans.o:	ftamsystem.h ftamsbr.h DOCS-types.h $(HFILES) \
			$(HDIR)usr.dirent.h $(HDIR)sys.dirent.h


##################################################################
# ftam
##################################################################

inst-ftam:	$(BINDIR)ftam

$(BINDIR)ftam:	xftam
		-cp $@ zxftam
		-rm -f $@
		cp xftam $@
		-@ls -gls $@
		-@echo ""

ftam:		xftam

xftam:		ftam.o ftamuser.o ftam-assoc.o ftam-dir.o ftam-get.o \
			ftam-glob.o ftam-ls.o ftam-mgmt.o ftam-put.o \
			ftamsbr.o DOCS_tables.o $(LIBD)
		$(LDCC) $(LDFLAGS) -o $@ ftam.o ftamuser.o ftam-assoc.o \
			ftam-dir.o ftam-get.o ftam-glob.o ftam-ls.o \
			ftam-mgmt.o ftam-put.o ftamsbr.o DOCS_tables.o \
			$(LIBES) $(LSOCKET)

l-ftam:		DOCS_tables.c true
		$(LINT) $(LFLAGS) ftam.c ftamuser.c ftam-assoc.c ftam-dir.c \
			ftam-get.c ftam-glob.c ftam-ls.c ftam-mgmt.c \
			ftam-put.c ftamsbr.c DOCS_tables.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

ftam.o:		ftamuser.h ftamsbr.h DOCS-types.h $(HFILES) $(HDIR)tailor.h \
		$(HDIR)logger.h
ftamuser.o:	ftamuser.h ftamsbr.h DOCS-types.h $(HFILES) $(HDIR)tailor.h \
		$(HDIR)logger.h
ftam-assoc.o:	ftamuser.h ftamsbr.h DOCS-types.h $(HFILES)
ftam-dir.o:	ftamuser.h ftamsbr.h DOCS-types.h $(HFILES)
ftam-get.o:	ftamuser.h ftamsbr.h DOCS-types.h $(HFILES)
ftam-glob.o:	ftamuser.h ftamsbr.h DOCS-types.h $(HFILES) \
			$(HDIR)usr.dirent.h $(HDIR)sys.dirent.h
ftam-ls.o:	ftamuser.h ftamsbr.h DOCS-types.h $(HFILES)
ftam-mgmt.o:	ftamuser.h ftamsbr.h DOCS-types.h $(HFILES)
ftam-put.o:	ftamuser.h ftamsbr.h DOCS-types.h $(HFILES)


##################################################################
# ftamsbr/DOCS
##################################################################

ftamsbr.o:	ftamsbr.h DOCS-types.h $(HFILES)

DOCS_tables.o:	DOCS_tables.c DOCS-types.h 

DOCS_tables.c DOCS-types.h: docs.py $(TOPDIR)pepsy/xpepsy
		$(TOPDIR)pepsy/xpepsy -A -f -h -m docs.py


##################################################################
# manual pages
##################################################################

MANUALS	=	ftamd.8c ftam.1c ftamusers.5

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


##################################################################
# clean
##################################################################

clean:;		rm -f *.ph *.o DOCS* x* z* _* core

grind:;		iprint Makefile
		tgrind -lc ftamsystem.h ftamuser.h ftamsbr.h $(CFILES)
		tgrind -lpepy -d $(TOPDIR)pepy/grindefs ($PYFILES)
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve

true:;
