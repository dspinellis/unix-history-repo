# Makefile for creating dungeon
# Edit BIN DDIR and FFLAGS suitable for your system
.c.o:
	cc -c $(CFLAGS) $*.c
.F.o:
	f77 -c $(FFLAGS) $*.F

CFLAGS = -O # -i
#
# f77 compiler flags for Vax (and other large address machines)
#
FFLAGS = -O -q # -Ddebug
# f77 compiler flags for pdp (64K split I/D)
#FFLAGS = -O -q -I2 -L1 -i -DPDP # -Ddebug

# Directory path for main program - default /usr/games
BIN = /usr/games
# Directory path for data files - default is /usr/games/lib/dunlib
DDIR = \'/usr/games/lib/dunlib
DCDIR =\"/usr/games/lib/dunlib
# Directory path for man page - default /usr/manl/man6
MDIR = /usr/manl/man6
# userid for wizard - allows invoking game debugging tool - default 0
WIZDEF = -DWIZARDID=20

OBJS =  actors.o ballop.o clockr.o demons.o\
	dgame.o dinit.o dmain.o dso1.o dso2.o\
	dso3.o dso4.o dso5.o dso6.o dso7.o\
	dsub.o dverb1.o dverb2.o gdt.o lightp.o\
	nobjs.o np.o np1.o np2.o np3.o nrooms.o objcts.o\
	rooms.o rtim.o sobjs.o sverbs.o verbs.o villns.o

PDPOBJS = cinit.o cio.o lex.o
LISTEN = listen.o
SPEAK = cspeak.o speak.o

vax:	dungeon dtext.dat
pdp:	dungpdp speak listen dtext.dat

dungeon: $(OBJS)
	f77 -s -o dungeon $(OBJS)

dungpdp: $(OBJS) $(PDPOBJS)
	f77 -i -s -o dungpdp $(OBJS) $(PDPOBJS)

speak:	$(SPEAK)
	f77 -i -s -o speak $(SPEAK)
	
speak.o: speak.F
	f77 $(FFLAGS) -DDDIR=$(DDIR) -c speak.F

listen:	$(LISTEN)
	cc $(CFLAGS) -o listen $(LISTEN)

listen.o: listen.c
	cc $(CFLAGS) -DCINDEXFILE=$(DCDIR)/dindx.dat\" -c listen.c

dinit.o: dinit.F
	f77 $(FFLAGS) -DDDIR=$(DDIR) $(WIZDEF) -c dinit.F

install-vax: vax
	cp dungeon $(BIN)
	-mkdir $(DDIR)
	cp dindx.dat dtext.dat $(DDIR)
	cp dungeon.6 $(MDIR)/dungeon.6

install-pdp: pdp
	cp dungeon.sh $(BIN)
	-mkdir $(DDIR)
	cp dindx.dat dtext.dat rtext.dat dungpdp $(DDIR)
	cp dungeon.6 $(MDIR)/dungeon.6

dtext.dat: 
	./maketxt.sh

