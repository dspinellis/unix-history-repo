RANLIB=ranlib
AR=ar
CFLAGS=-g
OBJECTS=Dvi.o draw.o font.o lex.o page.o parse.o XFontName.o DviChar.o

.c.o:
	$(CC) -c $(CFLAGS) -o $@ $<

all: libXdvi.a

libXdvi.a: $(OBJECTS)
	$(AR) r $@ $?
	if test "$(RANLIB)" ; then $(RANLIB) $@ ; fi

clean:
	-rm -f *.o core libXdvi.a

Dvi.o: DviP.h Dvi.h DviChar.h
draw.o: DviP.h Dvi.h DviChar.h
font.o: DviP.h Dvi.h DviChar.h XFontName.h
lex.o: DviP.h Dvi.h DviChar.h
page.o: DviP.h Dvi.h DviChar.h
parse.o: DviP.h Dvi.h DviChar.h
XFontName.o: XFontName.h
DviChar.o: DviChar.h
