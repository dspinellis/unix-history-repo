CC = cgcc
SRC = ..
GLOBINCS = 
LOCINCS = 
LIBS = -lcurses  -lgdbm -lpml -lgnu

cperl.ttp: $(SRC)/uperl.a usersub.o curses.o
	$(CC) $(SRC)/uperl.a usersub.o curses.o $(LIBS) -o cperl.ttp

usersub.o: usersub.c
	$(CC) -c -I$(SRC) $(GLOBINCS) -O usersub.c

curses.o: curses.c
	$(CC) -c -I$(SRC) $(GLOBINCS) -O curses.c

curses.c: acurses.mus
	perl mus acurses.mus >curses.c
