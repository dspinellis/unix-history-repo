#
#  Makefile for INGRES query modification module
#
#	@(#)Makefile	7.1	2/5/81
#

INGRES=	../..
H=	../h
LIB=	$(INGRES)/lib

CTLMOD=	$(LIB)/ctlmod
IUTIL=	$(LIB)/iutil
GUTIL=	$(LIB)/gutil
LIBS=	$(CTLMOD) $(IUTIL) $(GUTIL)
# PLIB=	../parser/norml.o

CFLAGS=	-O -I$H
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
HEADER=	Query Modification
CTAGS=	/bin/csh /usr/ucb/ctags

OBJS=	d_integ.o d_prot.o d_view.o puttree.o qrymod.o \
	integrity.o protect.o view.o trscan.o \
	issue.o norml.o tree.o util.o
SRCS=	integrity.c issue.c norml.c protect.c \
	qrymod.c tree.c trscan.c \
	util.c view.c puttree.c \
	d_view.c d_prot.c d_integ.c qrymod.h

qrymodlib: $(OBJS)
	$(AR) $(ARFLAGS) qrymodlib $?
	$(CHMOD) $(LIBMODE) qrymodlib
	$(RANLIB) qrymodlib

install: $(LIB)/qrymod

$(LIB)/qrymod: qrymodlib
	$(CP) qrymodlib $(LIB)/qrymod
	$(RANLIB) $(LIB)/qrymod

.PRECIOUS: qrymodlib

qrymod: $(OBJS) config.o
	$(CC) $(LDFLAGS) config.o $(OBJS) $(LIBS) -o qrymod
	$(CHMOD) $(BINMODE) qrymod
	ls -l qrymod; size qrymod

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

ctags:
	$(CTAGS) *.c ../parser/norml.c ../iutil/trbuild.c

new:
	rm -f qrymodlib

clean:
	rm -f *.o
	rm -f core a.out qrymod qrymodlib config.c

$(OBJS): qrymod.h

config.c: ../conf/SCCS/s.qrymod.c
	$(GET) $(REL) -p ../conf/SCCS/s.qrymod.c > config.c
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

integrity.o:$H/ingres.h
integrity.o:$H/aux.h
integrity.o:$H/catalog.h
integrity.o:$H/access.h
integrity.o:$H/tree.h
integrity.o:$H/symbol.h
integrity.o:qrymod.h
issue.o:$H/ingres.h
issue.o:$H/aux.h
issue.o:$H/symbol.h
issue.o:$H/tree.h
issue.o:$H/pv.h
issue.o:qrymod.h
norml.o:$H/ingres.h
norml.o:$H/aux.h
norml.o:$H/tree.h
norml.o:$H/symbol.h
protect.o:$H/ingres.h
protect.o:$H/aux.h
protect.o:$H/catalog.h
protect.o:$H/symbol.h
protect.o:$H/tree.h
protect.o:qrymod.h
qrymod.o:$H/ingres.h
qrymod.o:$H/aux.h
qrymod.o:$H/pv.h
qrymod.o:$H/opsys.h
qrymod.o:$H/func.h
qrymod.o:$H/tree.h
qrymod.o:qrymod.h
tree.o:$H/ingres.h
tree.o:$H/tree.h
tree.o:$H/symbol.h
tree.o:qrymod.h
trscan.o:$H/ingres.h
trscan.o:$H/symbol.h
trscan.o:$H/tree.h
trscan.o:qrymod.h
util.o:$H/ingres.h
util.o:$H/aux.h
util.o:$H/symbol.h
util.o:$H/tree.h
util.o:qrymod.h
view.o:$H/ingres.h
view.o:$H/aux.h
view.o:$H/symbol.h
view.o:$H/tree.h
view.o:qrymod.h
puttree.o:$H/ingres.h
puttree.o:$H/catalog.h
puttree.o:$H/symbol.h
puttree.o:$H/lock.h
puttree.o:$H/tree.h
puttree.o:qrymod.h
d_view.o:$H/ingres.h
d_view.o:$H/aux.h
d_view.o:$H/tree.h
d_view.o:$H/symbol.h
d_view.o:$H/catalog.h
d_view.o:$H/pv.h
d_view.o:$H/func.h
d_view.o:qrymod.h
d_prot.o:$H/ingres.h
d_prot.o:$H/aux.h
d_prot.o:$H/catalog.h
d_prot.o:$H/access.h
d_prot.o:$H/tree.h
d_prot.o:$H/symbol.h
d_prot.o:$H/lock.h
d_prot.o:$H/pv.h
d_prot.o:$H/func.h
d_prot.o:qrymod.h
d_integ.o:$H/ingres.h
d_integ.o:$H/aux.h
d_integ.o:$H/catalog.h
d_integ.o:$H/tree.h
d_integ.o:$H/symbol.h
d_integ.o:$H/pv.h
d_integ.o:$H/resp.h
d_integ.o:$H/func.h
d_integ.o:qrymod.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
