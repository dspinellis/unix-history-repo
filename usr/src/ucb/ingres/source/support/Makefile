#
#  Makefile for INGRES support routines
#
#	%W%	%G%
#

INGRES=	../..
H=	../h
LIB=	$(INGRES)/lib
BIN=	$(INGRES)/bin
SYSBIN=	/usr/local

ALL=	creatdb destroydb helpr printr purge sysmod \
	restore usersetup ingres printadmin demodb.sh
SRCS=	creatdb.c destroydb.c error.c helpr.c ingres.y \
	printr.c purge.c subs.c sysmod.c usersetup.c \
	indconv.q restore.c copydb.q prvect.c printadmin.c \
	demodb.sh

CTLMOD=	$(LIB)/ctlmod
IUTIL=	$(LIB)/iutil
GUTIL=	$(LIB)/gutil
DBULIB=	$(LIB)/dbulib
LIBS1=	$(GUTIL)
LIBS2=	$(IUTIL) $(GUTIL)
LIBS3=	$(DBULIB) $(IUTIL) $(GUTIL)

CFLAGS=	-O -I$H
EQUEL=	equel
LDFLAGS=-z
CP=	cp
CHMOD=	chmod
BINMODE=751
SUMODE=	4751
ROMODE=	444
SHMODE=	755
CHOWN=	-chown
GET=	sccs get
REL=
PR=	print
VGRIND=	csh /usr/ucb/vgrind
HEADER=	Support Programs

.q.c:
	$(EQUEL) $<

all: $(ALL)

copydb:	copydb.o $(LIBS1)
	$(CC) $(LDFLAGS) copydb.o $(LIBS1) -lq -o copydb
	$(CHMOD) $(BINMODE) copydb
	ls -l copydb; size copydb

copydb5.c: copydb5.q
	equel5 copydb5.q

copydb5: copydb5.o $(LIBS2)
	$(CC) $(LDFLAGS) copydb5.o $(LIBS2) -lq5 -o copydb5
	$(CHMOD) $(BINMODE) copydb5

creatdb: creatdb.o error.o prvect.o $(LIBS3)
	$(CC) $(LDFLAGS) creatdb.o error.o prvect.o $(LIBS3) -o creatdb
	$(CHMOD) $(SUMODE) creatdb
	ls -l creatdb; size creatdb

destroydb: destroydb.o $(LIBS3)
	$(CC) $(LDFLAGS) destroydb.o $(LIBS3) -o destroydb
	$(CHMOD) $(SUMODE) destroydb
	ls -l destroydb; size destroydb

helpr:	helpr.o error.o prvect.o $(LIBS3)
	$(CC) $(LDFLAGS) helpr.o error.o prvect.o $(LIBS3) -o helpr
	$(CHMOD) $(SUMODE) helpr
	ls -l helpr; size helpr

indconv: indconv.o $(LIBS2)
	$(CC) indconv.o $(LIB)/equel $(LIBS2) -o indconv
	$(CHMOD) $(BINMODE) indconv
	ls -l indconv; size indconv

ingres: ingres.o $(LIBS2)
	$(CC) ingres.o $(LDFLAGS) $(LIBS2) -o ingres
	$(CHMOD) $(SUMODE) ingres
	ls -l ingres; size ingres
ingres.o: ingres.c
	$(CC) $(CFLAGS) -I../ctlmod -c ingres.c

printadmin: printadmin.o $(LIBS2)
	$(CC) $(LDFLAGS) printadmin.o $(LIBS2) -o printadmin
	$(CHMOD) $(SUMODE) printadmin
	ls -l printadmin; size printadmin

printr: printr.o error.o prvect.o $(LIBS3)
	$(CC) $(LDFLAGS) printr.o error.o prvect.o $(LIBS3) -o printr
	$(CHMOD) $(SUMODE) printr
	ls -l printr; size printr

purge: purge.o subs.o error.o $(LIBS3)
	$(CC) $(LDFLAGS) purge.o subs.o error.o $(LIBS3) -o purge
	$(CHMOD) $(SUMODE) purge
	ls -l purge; size purge

restore: restore.o subs.o error.o $(LIBS3)
	$(CC) $(LDFLAGS) restore.o subs.o error.o $(LIBS3) -o restore
	$(CHMOD) $(SUMODE) restore
	ls -l restore; size restore

sysmod:	sysmod.o error.o prvect.o $(LIBS3) 
	$(CC) $(LDFLAGS) sysmod.o error.o prvect.o $(LIBS3) -o sysmod
	$(CHMOD) $(SUMODE) sysmod
	ls -l sysmod; size sysmod

usersetup: usersetup.o $(LIBS1)
	$(CC) $(LDFLAGS) usersetup.o $(LIBS1) -o usersetup
	$(CHMOD) 700 usersetup

new:
	rm -f $(ALL)

clean:
	rm -f *.o
	rm -f $(ALL) copydb.c copydb5.c indconv.c ingres.c a.out core y.tab.c

install: $(BIN)/creatdb $(BIN)/destroydb $(BIN)/helpr
install: $(BIN)/printr $(BIN)/purge $(BIN)/restore $(BIN)/sysmod
install: $(BIN)/usersetup $(BIN)/ingres $(BIN)/printadmin
install: $(BIN)/demodb

$(BIN)/creatdb: creatdb
	$(CP) creatdb $(BIN)/creatdb; $(CHMOD) $(SUMODE) $(BIN)/creatdb
$(BIN)/destroydb: destroydb
	$(CP) destroydb $(BIN)/destroydb; $(CHMOD) $(SUMODE) $(BIN)/destroydb
$(BIN)/helpr: helpr
	$(CP) helpr $(BIN)/helpr; $(CHMOD) $(SUMODE) $(BIN)/helpr
$(BIN)/indconv: indconv
	$(CP) indconv $(BIN)/indconv; $(CHMOD) $(BINMODE) $(BIN)/indconv
$(BIN)/printr: printr
	$(CP) printr $(BIN)/printr; $(CHMOD) $(SUMODE) $(BIN)/printr
$(BIN)/purge: purge
	$(CP) purge $(BIN)/purge; $(CHMOD) $(SUMODE) $(BIN)/purge
$(BIN)/restore: restore
	$(CP) restore $(BIN)/restore; $(CHMOD) $(SUMODE) $(BIN)/restore
$(BIN)/sysmod: sysmod
	$(CP) sysmod $(BIN)/sysmod; $(CHMOD) $(SUMODE) $(BIN)/sysmod
$(BIN)/usersetup: usersetup
	$(CP) usersetup $(BIN)/usersetup; $(CHMOD) 700 $(BIN)/usersetup
$(BIN)/ingres: ingres
	$(CP) ingres $(BIN)/ingres; $(CHMOD) $(SUMODE) $(BIN)/ingres
$(BIN)/printadmin: printadmin
	$(CP) printadmin $(BIN)/printadmin; $(CHMOD) $(SUMODE) $(BIN)/printadmin
$(BIN)/demodb: demodb.sh
	$(CP) demodb.sh $(BIN)/demodb; $(CHMOD) $(SHMODE) $(BIN)/demodb

sysinstall:
sysinstall: $(SYSBIN)/ingres $(SYSBIN)/creatdb $(SYSBIN)/destroydb $(SYSBIN)/demodb
sysinstall: $(SYSBIN)/sysmod $(SYSBIN)/purge $(SYSBIN)/restore
sysinstall: $(SYSBIN)/printr $(SYSBIN)/helpr

$(SYSBIN)/ingres: ingres
	$(CP) ingres $(SYSBIN)/ingres
	$(CHOWN) ingres $(SYSBIN)/ingres; $(CHMOD) $(SUMODE) $(SYSBIN)/ingres
$(SYSBIN)/creatdb: creatdb
	$(CP) creatdb $(SYSBIN)/creatdb
	$(CHOWN) ingres $(SYSBIN)/creatdb; $(CHMOD) $(SUMODE) $(SYSBIN)/creatdb
$(SYSBIN)/destroydb: destroydb
	$(CP) destroydb $(SYSBIN)/destroydb
	$(CHOWN) ingres $(SYSBIN)/destroydb; $(CHMOD) $(SUMODE) $(SYSBIN)/destroydb
$(SYSBIN)/demodb: demodb.sh
	$(CP) demodb.sh $(SYSBIN)/demodb
	$(CHOWN) ingres $(SYSBIN)/demodb; $(CHMOD) $(SHMODE) $(SYSBIN)/demodb
$(SYSBIN)/sysmod: sysmod
	$(CP) sysmod $(SYSBIN)/sysmod
	$(CHOWN) ingres $(SYSBIN)/sysmod; $(CHMOD) $(SUMODE) $(SYSBIN)/sysmod
$(SYSBIN)/purge: purge
	$(CP) purge $(SYSBIN)/purge
	$(CHOWN) ingres $(SYSBIN)/purge; $(CHMOD) $(SUMODE) $(SYSBIN)/purge
$(SYSBIN)/restore: restore
	$(CP) restore $(SYSBIN)/restore
	$(CHOWN) ingres $(SYSBIN)/restore; $(CHMOD) $(SUMODE) $(SYSBIN)/restore
$(SYSBIN)/printr: printr
	$(CP) printr $(SYSBIN)/printr
	$(CHOWN) ingres $(SYSBIN)/printr; $(CHMOD) $(SUMODE) $(SYSBIN)/printr
$(SYSBIN)/helpr: helpr
	$(CP) helpr $(SYSBIN)/helpr
	$(CHOWN) ingres $(SYSBIN)/helpr; $(CHMOD) $(SUMODE) $(SYSBIN)/helpr

sources: $(SRCS)

$(SRCS):
	$(GET) $(REL) SCCS/s.$@

print: sources
	$(PR) Makefile TraceFlags *.[hm] *.[csy]

vgrind: sources
	cp /dev/null index
	$(VGRIND) -h "$(HEADER) (Release %R%)" -n Makefile TraceFlags
	$(VGRIND) -h "$(HEADER) (Release %R%)" *.[hm] *.[csy]
	$(VGRIND) -h "$(HEADER) (Release %R%)" -x index

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

creatdb.o:$H/ingres.h
creatdb.o:$H/aux.h
creatdb.o:$H/version.h
creatdb.o:$H/access.h
creatdb.o:$H/symbol.h
creatdb.o:$H/pv.h
destroydb.o:$H/ingres.h
destroydb.o:$H/aux.h
destroydb.o:$H/access.h
error.o:$H/ingres.h
error.o:$H/aux.h
helpr.o:$H/ingres.h
helpr.o:$H/aux.h
helpr.o:$H/access.h
helpr.o:$H/lock.h
helpr.o:$H/pv.h
ingres.y:$H/ingres.h
ingres.y:$H/aux.h
ingres.y:$H/version.h
ingres.y:$H/access.h
ingres.y:$H/lock.h
ingres.y:$H/opsys.h
printr.o:$H/ingres.h
printr.o:$H/aux.h
printr.o:$H/access.h
printr.o:$H/lock.h
printr.o:$H/pv.h
purge.o:$H/ingres.h
purge.o:$H/aux.h
purge.o:$H/lock.h
subs.o:$H/ingres.h
subs.o:$H/aux.h
subs.o:$H/access.h
subs.o:$H/lock.h
sysmod.o:$H/ingres.h
sysmod.o:$H/aux.h
sysmod.o:$H/lock.h
sysmod.o:$H/access.h
sysmod.o:$H/pv.h
usersetup.o:$H/ingres.h
usersetup.o:$H/aux.h
usersetup.o:$H/opsys.h
restore.o:$H/ingres.h
restore.o:$H/aux.h
restore.o:$H/catalog.h
restore.o:$H/access.h
restore.o:$H/opsys.h
restore.o:$H/lock.h
restore.o:$H/symbol.h
restore.o:$H/resp.h
prvect.o:$H/tree.h
prvect.o:$H/pv.h
printadmin.o:$H/ingres.h
printadmin.o:$H/access.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
