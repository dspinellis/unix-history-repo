CFLAGS = -O
OBJS = y.tab.o main.o sem.o code1.o code2.o
SRCS = y.tab.c main.c sem.c code1.c code2.c
DESTDIR = /usr/local

courier:	$(OBJS)
		$(CC) $(CFLAGS) -o courier $(OBJS) -ll

y.tab.o:	lex.yy.c

lex.yy.c:	scanner.l
		lex scanner.l

y.tab.c:	courier.y
		yacc courier.y

$(OBJS):	Courier.h

lint:		y.tab.c
		lint -hnux $(SRCS)

install:	courier
		install -s courier $(DESTDIR)

clean:
		rm -f *.o y.tab.[ch] lex.yy.c
