#
#  Makefile for INGRES decomposition module
#
#	@(#)Makefile	7.1	2/5/81
#

INGRES=	../..
H=	../h
LIB=	$(INGRES)/lib

CTLMOD=	$(LIB)/ctlmod
OVQP=	$(LIB)/ovqp
DBULIB=	$(LIB)/dbulib
IUTIL=	$(LIB)/iutil
GUTIL=	$(LIB)/gutil

OBJS= 	qryproc.o de_init.o ret_unique.o \
	aggregate.o ageval.o byeval.o makenode.o mapvar.o \
	decomp.o pull_sq.o exec_sq.o tempvar.o reformat.o decision.o \
	reduction.o openrs.o error.o prlinks.o rangetable.o \
	rnum.o reinit.o selectv.o setvar.o usubr.o \
	lockit.o pull_const.o \
	call_ovqp.o mklist.o call_dbu.o

LIBS=	$(CTLMOD) $(OVQP) $(DBULIB) $(IUTIL) $(GUTIL) -lm
NLIBS=	$(CTLMOD) $(OVQP) ../nacc/nlib $(DBULIB) ../access/naccess ../nacc/nlib $(IUTIL) $(GUTIL)

SRCS= 	de_init.c qryproc.c ret_unique.c \
	aggregate.c ageval.c byeval.c makenode.c mapvar.c \
	decomp.c pull_sq.c exec_sq.c tempvar.c reformat.c decision.c \
	reduction.c openrs.c error.c prlinks.c rangetable.c \
	rnum.c reinit.c selectv.c setvar.c usubr.c mklist.c \
	lockit.c pull_const.c \
	call_ovqp.c call_dbu.c decomp.h globs.h

CFLAGS=	-I$H -O
LDFLAGS=-z
AR=	-ar
ARFLAGS=rvu
RANLIB=	ranlib
CP=	cp
CHMOD=	chmod
LIBMODE=664
BINMODE=4740
ROMODE=	444
GET=	sccs get
REL=
PR=	print
VGRIND=	csh /usr/ucb/vgrind
HEADER=	Decomposition

decomplib: $(OBJS)
	$(AR) $(ARFLAGS) decomplib $?
	$(CHMOD) $(LIBMODE) decomplib
	$(RANLIB) decomplib

install: $(LIB)/decomp

$(LIB)/decomp: decomplib
	$(CP) decomplib $(LIB)/decomp
	$(RANLIB) $(LIB)/decomp

new:
	rm -f decomplib

clean:
	rm -f *.o
	rm -f decomplib decomp a.out core

decomp: $(OBJS) config.o
	$(CC) $(LDFLAGS) config.o $(OBJS) $(LIBS) -o decomp
	$(CHMOD) $(BINMODE) decomp

ndecomp: $(OBJS) config.o
	$(CC) config.o $(OBJS) $(LDFLAGS) $(NLIBS) -o ndecomp
	$(CHMOD) $(BINMODE) ndecomp

nnew:
	rm -f ndecomp

sources: $(SRCS)

$(SRCS):
	$(GET) $(REL) SCCS/s.$@

print: sources
	$(PR) Makefile TraceFlags *.[hm] *.[csy]

vgrind: sources
	cp /dev/null index
	$(VGRIND) -h "$(HEADER) (Release 7)" -n Makefile TraceFlags
	$(VGRIND) -h "$(HEADER) (Release 7)" *.[hm] *.[csy]
	$(VGRIND) -h "$(HEADER) (Release 7)" -x index

config.c: ../conf/SCCS/s.decomp.c
	rm -f config.c
	$(GET) $(REL) -p ../conf/SCCS/s.decomp.c > config.c
	$(CHMOD) $(ROMODE) config.c

depend:
	grep '^#[ 	]*include' $(SRCS) | sed -n -f ../h/depend.sed > makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

de_init.o:$H/ingres.h
de_init.o:$H/symbol.h
de_init.o:$H/range.h
de_init.o:globs.h
qryproc.o:$H/ingres.h
qryproc.o:$H/aux.h
qryproc.o:$H/tree.h
qryproc.o:$H/symbol.h
qryproc.o:globs.h
qryproc.o:$H/pv.h
qryproc.o:$H/lock.h
qryproc.o:$H/resp.h
ret_unique.o:$H/ingres.h
ret_unique.o:$H/symbol.h
ret_unique.o:$H/aux.h
ret_unique.o:$H/tree.h
ret_unique.o:globs.h
aggregate.o:$H/ingres.h
aggregate.o:$H/tree.h
aggregate.o:$H/symbol.h
aggregate.o:globs.h
ageval.o:$H/ingres.h
ageval.o:$H/tree.h
ageval.o:$H/symbol.h
ageval.o:$H/pv.h
ageval.o:globs.h
byeval.o:$H/ingres.h
byeval.o:$H/tree.h
byeval.o:$H/symbol.h
byeval.o:$H/pv.h
byeval.o:globs.h
makenode.o:$H/ingres.h
makenode.o:$H/aux.h
makenode.o:$H/tree.h
makenode.o:$H/symbol.h
makenode.o:globs.h
mapvar.o:$H/ingres.h
mapvar.o:$H/aux.h
mapvar.o:$H/tree.h
mapvar.o:$H/symbol.h
decomp.o:$H/ingres.h
decomp.o:$H/aux.h
decomp.o:$H/tree.h
decomp.o:$H/symbol.h
decomp.o:globs.h
pull_sq.o:$H/ingres.h
pull_sq.o:$H/aux.h
pull_sq.o:$H/tree.h
pull_sq.o:$H/symbol.h
pull_sq.o:globs.h
exec_sq.o:$H/ingres.h
exec_sq.o:$H/aux.h
exec_sq.o:$H/tree.h
exec_sq.o:$H/symbol.h
exec_sq.o:$H/pv.h
exec_sq.o:globs.h
tempvar.o:$H/ingres.h
tempvar.o:$H/tree.h
tempvar.o:$H/symbol.h
reformat.o:$H/ingres.h
reformat.o:$H/catalog.h
reformat.o:$H/aux.h
reformat.o:$H/tree.h
reformat.o:$H/symbol.h
reformat.o:$H/pv.h
reformat.o:globs.h
reformat.o:$H/access.h
decision.o:$H/ingres.h
decision.o:$H/symbol.h
decision.o:$H/aux.h
decision.o:$H/tree.h
decision.o:globs.h
reduction.o:$H/ingres.h
reduction.o:$H/symbol.h
reduction.o:$H/aux.h
reduction.o:$H/tree.h
reduction.o:globs.h
openrs.o:$H/ingres.h
openrs.o:$H/aux.h
openrs.o:$H/tree.h
openrs.o:$H/symbol.h
openrs.o:globs.h
error.o:$H/ingres.h
error.o:$H/aux.h
error.o:$H/tree.h
error.o:$H/symbol.h
error.o:globs.h
prlinks.o:$H/ingres.h
prlinks.o:$H/aux.h
prlinks.o:$H/tree.h
prlinks.o:$H/symbol.h
prlinks.o:globs.h
rangetable.o:$H/ingres.h
rangetable.o:$H/symbol.h
rangetable.o:globs.h
rnum.o:$H/ingres.h
rnum.o:globs.h
reinit.o:$H/ingres.h
reinit.o:$H/aux.h
reinit.o:$H/tree.h
reinit.o:$H/symbol.h
reinit.o:$H/lock.h
selectv.o:$H/ingres.h
selectv.o:$H/symbol.h
selectv.o:$H/tree.h
selectv.o:globs.h
setvar.o:$H/ingres.h
setvar.o:$H/aux.h
setvar.o:$H/tree.h
setvar.o:$H/symbol.h
setvar.o:globs.h
usubr.o:$H/ingres.h
usubr.o:$H/aux.h
usubr.o:$H/tree.h
usubr.o:$H/symbol.h
usubr.o:$H/access.h
usubr.o:$H/pv.h
usubr.o:globs.h
mklist.o:$H/ingres.h
mklist.o:$H/aux.h
mklist.o:$H/tree.h
mklist.o:$H/symbol.h
mklist.o:globs.h
lockit.o:$H/ingres.h
lockit.o:$H/aux.h
lockit.o:$H/tree.h
lockit.o:globs.h
lockit.o:$H/lock.h
pull_const.o:$H/ingres.h
pull_const.o:$H/symbol.h
pull_const.o:$H/aux.h
pull_const.o:$H/tree.h
pull_const.o:globs.h
call_ovqp.o:$H/ingres.h
call_ovqp.o:$H/aux.h
call_ovqp.o:$H/opsys.h
call_ovqp.o:$H/access.h
call_ovqp.o:$H/tree.h
call_ovqp.o:$H/symbol.h
call_ovqp.o:globs.h
call_dbu.o:$H/ingres.h
call_dbu.o:$H/aux.h
call_dbu.o:$H/tree.h
call_dbu.o:$H/symbol.h
call_dbu.o:$H/pv.h
call_dbu.o:globs.h
decomp.h:$H/pv.h
globs.h:$H/func.h
globs.h:../decomp/decomp.h
globs.h:../ovqp/ovqp.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
