DESTDIR=
CFLAGS=	-O
LFLAGS=	-lm -ltermcap -s
BIN=	$(DESTDIR)/usr/games
LIB=	$(DESTDIR)/usr/games/lib
USER=	daemon
UTILS=	snscore
OBJS=	snake.o move.o
snake:	$(OBJS)
	cc $(OBJS) -o snake $(LFLAGS)
snake.o move.o:snake.h
snscore:	snscore.c
	cc $(CFLAGS) snscore.c -o snscore
busy:	busy.c
	cc $(CFLAGS) busy.c -o busy

install: snake $(UTILS)
	rm -f $(BIN)/snake $(BIN)/snscore $(BIN)/busy
	cp snake $(BIN)/snake
	cp snscore $(BIN)/snscore
#	cp busy $(BIN)/busy
	chown $(USER) $(BIN)/snake $(BIN)/snscore
#	chown $(BIN)/busy
	chmod 4755 $(BIN)/snake
	chmod 755 $(BIN)/snscore
#	chmod 755 $(BIN)/busy
	cat /dev/null >> $(LIB)/snakerawscores
	chmod 644 $(LIB)/snakerawscores
	chown $(USER) $(LIB)/snakerawscores
clean:
	rm -f snake snscore busy
	rm -f *.o
