#
#  Makefile for INGRES database utilities
#
#	@(#)Makefile	7.1	2/5/81
#

INGRES=	../..
H=	../h
LIB=	$(INGRES)/lib
BIN=	$(INGRES)/bin

CTLMOD=	$(LIB)/ctlmod
IUTIL=	$(LIB)/iutil
GUTIL=	$(LIB)/gutil
DBULIB=	$(LIB)/dbulib

LIBS=	$(CTLMOD) $(IUTIL) $(GUTIL)
NLIBS=	$(CTLMOD) $(LIB)/naccess $(IUTIL) $(GUTIL)
LIBS1=	$(CTLMOD) $(DBULIB) $(IUTIL) $(GUTIL)
NOBJS=	nmodify.o
OBJS=	index.o create.o modify.o modupdate.o destroy.o \
	udestroy.o copy.o purgetup.o print.o rupdate.o \
	help.o save.o update.o convert.o secupdate.o \
	rdwrbatch.o resetrel.o seq_atts.o \
	get_p_tid.o display.o pr_prot.o pr_tree.o rmqm.o
OOBJS=	index.o create.o modupdate.o destroy.o udestroy.o \
	purgetup.o print.o rupdate.o help.o save.o update.o convert.o \
	secupdate.o rdwrbatch.o resetrel.o seq_atts.o \
	get_p_tid.o copy.o
SRCS=	index.c create.c modupdate.c destroy.c udestroy.c \
	purgetup.c print.c rupdate.c help.c save.c update.c convert.c \
	secupdate.c rdwrbatch.c resetrel.c seq_atts.c \
	get_p_tid.c copy.c modify.c ksort.c \
	display.c rmqm.c pr_prot.c pr_tree.c

AR=	-ar
ARFLAGS=rvu
RANLIB=	ranlib
CFLAGS=	-I$H -O
LDFLAGS=-z
CP=	cp
CHMOD=	chmod
LIBMODE=664
BINMODE=4740
ROMODE=	444
GET=	sccs get
REL=
PR=	print
VGRIND=	csh /usr/ucb/vgrind
HEADER=	Database Utilities

dbulib: $(OBJS)
	$(AR) $(ARFLAGS) dbulib $?
	$(CHMOD) $(LIBMODE) dbulib
	$(RANLIB) dbulib

install: $(LIB)/dbulib $(BIN)/ksort7

$(LIB)/dbulib: dbulib
	$(CP) dbulib $(LIB)/dbulib
	$(RANLIB) $(LIB)/dbulib

$(BIN)/ksort7: ksort
	$(CP) ksort $(BIN)/ksort7; $(CHMOD) $(BINMODE) $(BIN)/ksort7

alldbu: $(OBJS) config.o
	$(CC) config.o $(OBJS) $(LDFLAGS) $(LIBS) -o alldbu
	$(CHMOD) $(BINMODE) alldbu

nalldbu:$(OOBJS) $(NOBJS) config.o
	$(CC) config.o $(NOBJS) $(OOBJS) $(LDFLAGS) $(NLIBS) -o nalldbu
	chmod 4700 nalldbu

ksort:	ksort.o $(LIBS)
	$(CC) $@.o $(LDFLAGS) $(LIBS) -o $@
	$(CHMOD) $(BINMODE) ksort

new:
	rm -f dbulib alldbu

clean:
	rm -f *.o overlay?
	rm -f dbulib ksort alldbu a.out core

sources: $(SRCS)

$(SRCS):
	$(GET) $(REL) SCCS/s.$@

print: sources
	$(PR) Makefile TraceFlags *.[hm] *.[csy]

vgrind: sources
	cp /dev/null index
	$(VGRIND) -h "$(HEADER) (Release 7)" -n Makefile TraceFlags
	$(VGRIND) -h "$(HEADER) (Release 7)" *.[csy]
	$(VGRIND) -h "$(HEADER) (Release 7)" -x index

config.c: ../conf/SCCS/s.alldbu.c
	rm -f config.c
	$(GET) $(REL) -p ../conf/SCCS/s.alldbu.c > config.c
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

index.o:$H/pv.h
index.o:$H/func.h
index.o:$H/symbol.h
index.o:$H/ingres.h
index.o:$H/aux.h
index.o:$H/catalog.h
index.o:$H/access.h
index.o:$H/lock.h
create.o:$H/pv.h
create.o:$H/ingres.h
create.o:$H/access.h
create.o:$H/aux.h
create.o:$H/catalog.h
create.o:$H/symbol.h
create.o:$H/lock.h
create.o:$H/func.h
modupdate.o:$H/pv.h
modupdate.o:$H/ingres.h
modupdate.o:$H/aux.h
modupdate.o:$H/catalog.h
modupdate.o:$H/access.h
modupdate.o:$H/opsys.h
destroy.o:$H/pv.h
destroy.o:$H/ingres.h
destroy.o:$H/aux.h
destroy.o:$H/catalog.h
destroy.o:$H/func.h
udestroy.o:$H/ingres.h
udestroy.o:$H/aux.h
udestroy.o:$H/catalog.h
purgetup.o:$H/ingres.h
print.o:$H/pv.h
print.o:$H/ingres.h
print.o:$H/access.h
print.o:$H/aux.h
print.o:$H/lock.h
print.o:$H/func.h
rupdate.o:$H/pv.h
rupdate.o:$H/ingres.h
rupdate.o:$H/func.h
help.o:$H/pv.h
help.o:$H/ingres.h
help.o:$H/aux.h
help.o:$H/catalog.h
help.o:$H/access.h
help.o:$H/func.h
save.o:$H/pv.h
save.o:$H/ingres.h
save.o:$H/aux.h
save.o:$H/func.h
save.o:$H/opsys.h
update.o:$H/ingres.h
update.o:$H/resp.h
update.o:$H/aux.h
update.o:$H/symbol.h
update.o:$H/access.h
convert.o:$H/ingres.h
convert.o:$H/symbol.h
secupdate.o:$H/ingres.h
secupdate.o:$H/aux.h
secupdate.o:$H/catalog.h
secupdate.o:$H/symbol.h
secupdate.o:$H/access.h
rdwrbatch.o:$H/ingres.h
rdwrbatch.o:$H/aux.h
rdwrbatch.o:$H/symbol.h
rdwrbatch.o:$H/access.h
resetrel.o:$H/pv.h
resetrel.o:$H/ingres.h
resetrel.o:$H/aux.h
resetrel.o:$H/access.h
resetrel.o:$H/func.h
seq_atts.o:$H/ingres.h
seq_atts.o:$H/access.h
get_p_tid.o:$H/ingres.h
get_p_tid.o:$H/catalog.h
copy.o:$H/func.h
copy.o:$H/pv.h
copy.o:$H/ingres.h
copy.o:$H/aux.h
copy.o:$H/access.h
copy.o:$H/symbol.h
copy.o:$H/lock.h
modify.o:$H/ingres.h
modify.o:$H/pv.h
modify.o:$H/aux.h
modify.o:$H/access.h
modify.o:$H/lock.h
modify.o:$H/opsys.h
modify.o:$H/func.h
ksort.o:$H/ingres.h
ksort.o:$H/aux.h
ksort.o:$H/symbol.h
ksort.o:$H/access.h
ksort.o:$H/func.h
display.o:$H/ingres.h
display.o:$H/aux.h
display.o:$H/catalog.h
display.o:$H/tree.h
display.o:$H/symbol.h
display.o:$H/access.h
display.o:$H/func.h
display.o:$H/pv.h
rmqm.o:$H/pv.h
rmqm.o:$H/ingres.h
rmqm.o:$H/access.h
rmqm.o:$H/aux.h
rmqm.o:$H/catalog.h
rmqm.o:$H/symbol.h
rmqm.o:$H/func.h
pr_prot.o:$H/ingres.h
pr_prot.o:$H/tree.h
pr_prot.o:$H/aux.h
pr_prot.o:$H/catalog.h
pr_prot.o:$H/symbol.h
pr_tree.o:$H/symbol.h
pr_tree.o:$H/ingres.h
pr_tree.o:$H/tree.h
pr_tree.o:$H/aux.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
