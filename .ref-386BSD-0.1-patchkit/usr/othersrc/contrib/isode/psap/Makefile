###############################################################################
#   Instructions to Make, for compilation of ISODE PSAP processes
###############################################################################

###############################################################################
#
# $Header: /f/osi/psap/RCS/Makefile,v 7.5 91/03/09 11:55:24 mrose Exp $
#
#
# $Log:	Makefile,v $
# Revision 7.5  91/03/09  11:55:24  mrose
# update
# 
# Revision 7.4  91/02/22  09:35:26  mrose
# Interim 6.8
# 
# Revision 7.3  91/01/19  13:50:58  mrose
# update
# 
# Revision 7.2  90/12/23  18:43:59  mrose
# update
# 
# Revision 7.1  90/07/09  14:43:29  mrose
# sync
# 
# Revision 7.0  89/11/23  22:12:29  mrose
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


LIBES	=	libpsap.a $(TOPDIR)libcompat.a
LLIBS   =	$(TOPDIR)llib-lcompat
HFILES	=	$(HDIR)psap.h \
		$(HDIR)manifest.h $(HDIR)general.h $(HDIR)config.h


##################################################################
# Here it is...
##################################################################

all:		libpsap psaptest hex2pe dec2pe
inst-all:	# inst-libpsap manuals
install:	inst-all clean
lint:		l-libpsap l-psaptest


################################################################
# libpsap
################################################################

CFILES	=	addr2ref.c bit2prim.c bitstr2strb.c bit_ops.c dg2ps.c \
		fdx2ps.c flag2prim.c gtime.c int2strb.c isobject.c num2prim.c \
		obj2prim.c objectbyname.c objectbyoid.c ode2oid.c oid_cmp.c \
		oid_cpy.c oid_free.c oid2ode.c pe2pl.c pe2ps.c pe2qb_f.c \
		pe2ssdu.c pe2text.c pe2uvec.c pe_alloc.c pe_cmp.c pe_cpy.c \
		pe_error.c pe_expunge.c pe_extract.c pe_free.c pe_pullup.c \
		pl2pe.c pl_tables.c prim2bit.c prim2flag.c prim2num.c \
		prim2oid.c prim2qb.c prim2real.c prim2set.c prim2str.c \
		prim2time.c ps2pe.c ps_alloc.c ps_error.c ps_flush.c \
		ps_free.c ps_get_abs.c ps_io.c ps_prime.c qb2pe.c qb2prim.c \
		qb2str.c qb_free.c qb_pullup.c qbuf2pe.c qbuf2pe_f.c \
		qbuf2ps.c real2prim.c seq_add.c seq_addon.c seq_del.c \
		seq_find.c set_add.c set_addon.c set_del.c set_find.c \
		sprintoid.c sprintref.c ssdu2pe.c std2ps.c str2oid.c \
		str2pe.c str2prim.c str2ps.c str2qb.c strb2bitstr.c \
		strb2int.c time2prim.c time2str.c tm2ut.c ts2ps.c ut2tm.c \
		uvec2ps.c

OFILES	=	addr2ref.o bit2prim.o bitstr2strb.o bit_ops.o dg2ps.o \
		fdx2ps.o flag2prim.o gtime.o int2strb.o isobject.o num2prim.o \
		obj2prim.o objectbyname.o objectbyoid.o ode2oid.o oid_cmp.o \
		oid_cpy.o oid_free.o oid2ode.o pe2pl.o pe2ps.o pe2qb_f.o \
		pe2ssdu.o pe2text.o pe2uvec.o pe_alloc.o pe_cmp.o pe_cpy.o \
		pe_error.o pe_expunge.o pe_extract.o pe_free.o pe_pullup.o \
		pl2pe.o pl_tables.o prim2bit.o prim2flag.o prim2num.o \
		prim2oid.o prim2qb.o prim2real.o prim2set.o prim2str.o \
		prim2time.o ps2pe.o ps_alloc.o ps_error.o ps_flush.o \
		ps_free.o ps_get_abs.o ps_io.o ps_prime.o qb2pe.o qb2prim.o \
		qb2str.o qb_free.o qb_pullup.o qbuf2pe.o qbuf2pe_f.o \
		qbuf2ps.o real2prim.o seq_add.o seq_addon.o seq_del.o \
		seq_find.o set_add.o set_addon.o set_del.o set_find.o \
		sprintoid.o sprintref.o ssdu2pe.o std2ps.o str2oid.o \
		str2pe.o str2prim.o str2ps.o str2qb.o strb2bitstr.o \
		strb2int.o time2prim.o time2str.o tm2ut.o ts2ps.o ut2tm.o \
		uvec2ps.o \
		$(OSTRINGS)

inst-libpsap:	$(LIBDIR)libpsap.a $(LINTDIR)llib-lpsap

$(LIBDIR)libpsap.a:	libpsap.a
		-rm -f $@
		cp libpsap.a $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $@ -ranlib
		-@ls -gls $@
		-@echo ""

$(LINTDIR)llib-lpsap:	llib-lpsap
		-cp $@ zllib-lpsap
		-rm -f $@
		sed -e 's%#include "\(.*\)"%#include "$(INCDIR)\1"%' \
			< llib-lpsap | \
			sed -e 's%#include "/usr/include/\(.*\)"%#include <\1>%' > $@
		@$(UTILDIR)inst-lint.sh $(SYSTEM) $(OPTIONS) $@
		-@ls -gls $@ $@.ln
		-@echo ""

libpsap:	libpsap.a

libpsap.a:	psapvrsn.o
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(OFILES) \
			psapvrsn.o
		-@rm -f $(TOPDIR)libpsap.a $(TOPDIR)llib-lpsap
		-@$(LN) libpsap.a $(TOPDIR)libpsap.a
		-@$(LN) llib-lpsap $(TOPDIR)llib-lpsap
		-@ls -l $@
		-@echo "PSAP library built normally"

psapvrsn.c:	$(OFILES)
		@$(UTILDIR)version.sh psap > $@

l-libpsap:;	$(LINT) $(LFLAGS) $(CFILES) psapvrsn.c $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

addr2ref.o:	$(HFILES) $(HDIR)ssap.h
bit2prim.o:	$(HFILES)
bitstr2strb.o:	$(HFILES)
bit_ops.o:	$(HFILES)
dg2ps.o:	$(HFILES)
fdx2ps.o:	$(HFILES)
flag2prim.o:	$(HFILES)
gtime.o:	$(HFILES)
int2strb.o:	$(HFILES)
isobject.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
num2prim.o:	$(HFILES)
obj2prim.o:	$(HFILES)
objectbyname.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
objectbyoid.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
ode2oid.o:	$(HFILES)
oid_cmp.o:	$(HFILES)
oid_cpy.o:	$(HFILES)
oid_free.o:	$(HFILES)
oid2ode.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
pe2pl.o:	$(HFILES)
pe2ps.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
pe2qb_f.o:	$(HFILES)
pe2ssdu.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
pe2text.o:	$(HFILES) $(HDIR)logger.h
pe2uvec.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
pe_alloc.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
pe_cmp.o:	$(HFILES)
pe_cpy.o:	$(HFILES)
pe_error.o:	$(HFILES)
pe_expunge.o:	$(HFILES)
pe_extract.o:	$(HFILES)
pe_free.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
pe_pullup.o:	$(HFILES)
pl2pe.o:	$(HFILES)
pl_tables.o:	$(HFILES)
prim2bit.o:	$(HFILES)
prim2flag.o:	$(HFILES)
prim2num.o:	$(HFILES)
prim2oid.o:	$(HFILES)
prim2qb.o:	$(HFILES)
prim2set.o:	$(HFILES)
prim2str.o:	$(HFILES)
prim2time.o:	$(HFILES)
ps2pe.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
ps_alloc.o:	$(HFILES)
ps_error.o:	$(HFILES)
ps_flush.o:	$(HFILES)
ps_free.o:	$(HFILES)
ps_get_abs.o:	$(HFILES)
ps_io.o:	$(HFILES)
ps_prime.o:	$(HFILES)
qb2pe.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
qb2prim.o:	$(HFILES)
qb2str.o:	$(HFILES)
qb_free.o:	$(HFILES)
qb_pullup.o:	$(HFILES)
qbuf2pe.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
qbuf2pe_f.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
qbuf2ps.o:	$(HFILES)
seq_add.o:	$(HFILES)
seq_addon.o:	$(HFILES)
seq_del.o:	$(HFILES)
seq_find.o:	$(HFILES)
set_add.o:	$(HFILES)
set_addon.o:	$(HFILES)
set_del.o:	$(HFILES)
set_find.o:	$(HFILES)
sprintoid.o:	$(HFILES)
sprintref.o:	$(HFILES) $(HDIR)ssap.h
ssdu2pe.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
std2ps.o:	$(HFILES)
str2oid.o:	$(HFILES)
str2pe.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h
str2prim.o:	$(HFILES)
str2ps.o:	$(HFILES)
str2qb.o:	$(HFILES)
strb2bitstr.o:	$(HFILES)
strb2int.o:	$(HFILES)
time2prim.o:	$(HFILES)
time2str.o:	$(HFILES)
tm2ut.o:	$(HFILES)
ts2ps.o:	$(HFILES) $(HDIR)tsap.h $(HDIR)tailor.h $(HDIR)logger.h
ut2tm.o:	$(HFILES)
uvec2ps.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h


################################################################
# psaptest
################################################################

psaptest:	psaptest.o $(LIBES)
		$(LDCC) $(LDFLAGS) -o $@ psaptest.o $(LIBES) $(LSOCKET)

psaptest.o:	$(HFILES) $(HDIR)tailor.h $(HDIR)logger.h psaptest.c
		$(CC) $(CFLAGS) -c $*.c

l-psaptest:;	$(LINT) $(LFLAGS) psaptest.c llib-lpsap $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"


################################################################
# hex2pe/dec2pe
################################################################

hex2pe:		hex2pe.o
		$(LDCC) $(LDFLAGS) -o $@ hex2pe.o

hex2pe.o:	$(HFILES) hex2pe.c
		$(CC) $(CFLAGS) -c $*.c


dec2pe:		dec2pe.o
		$(LDCC) $(LDFLAGS) -o $@ dec2pe.o

dec2pe.o:	$(HFILES) dec2pe.c
		$(CC) $(CFLAGS) -c $*.c


################################################################
# manual pages
################################################################

MANUALS	=	libpsap.3

manuals:;	@$(UTILDIR)inst-man.sh $(MANOPTS) $(MANUALS)
		-@echo ""


################################################################
# clean
################################################################

clean:;		rm -f *.o *.a z* _* core psaptest dec2pe hex2pe psapvrsn.c

grind:;		iprint Makefile
		tgrind -lc $(CFILES) psapvrsn.c psaptest.c dec2pe.c hex2pe.c \
			llib-lpsap
		@echo $(MANUALS) | \
			tr " " "\012" | \
			sed -e "s%.*%itroff -man &%" | \
			sh -ve
