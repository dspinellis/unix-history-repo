OBJS = main.o y.tab.o lex.yy.o mkioconf.o mkmakefile.o mkubglue.o mkheaders.o
CFLAGS=-w
LDFLAGS=-ll

config: $(OBJS)
	$(CC) $(OBJS) -o config $(LDFLAGS)

y.tab.o: y.tab.c

y.tab.c: config.y
	yacc -d config.y

lex.yy.c: config.l
	lex config.l

clean:
	rm -f $(OBJS) lex.yy.c y.tab.c y.tab.h config
