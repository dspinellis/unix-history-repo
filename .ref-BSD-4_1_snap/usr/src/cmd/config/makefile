OBJS = y.tab.o main.o lex.yy.o mkioconf.o mkmakefile.o mkubglue.o mkheaders.o
CFLAGS=-w -O
LDFLAGS=-ll
DESTDIR=/

config: $(OBJS)
	$(CC) $(OBJS) -o config $(LDFLAGS)

install: config
	install config $(DESTDIR)/etc

y.tab.o: y.tab.c

y.tab.c: config.y
	yacc -d config.y

lex.yy.c: config.l
	lex config.l

clean:
	rm -f $(OBJS) lex.yy.c y.tab.c

print:
	pr -l57 main.c config.y config.l mk*.c | netlpr -c vpr
