OBJS=		endgame.o extern.o getguess.o getword.o main.o playgame.o \
		prdata.o prman.o prword.o setup.o
CFILES=		endgame.c extern.c getguess.c getword.c main.c playgame.c \
		prdata.c prman.c prword.c setup.c
HDRS=		hangman.h
CFLAGS=		-O
LDFLAGS=	-g

all: hangman

tags: $(HDRS) $(CFILES)
	ctags $(HDRS) $(CFILES)

install: hangman
	install -s hangman $(DESTDIR)/usr/games/hangman
	
hangman: $(OBJS)
	$(CC) $(LDFLAGS) -o hangman $(OBJS) -lcurses -ltermlib

clean:
	rm -f $(OBJS) hangman ? core

depend:
