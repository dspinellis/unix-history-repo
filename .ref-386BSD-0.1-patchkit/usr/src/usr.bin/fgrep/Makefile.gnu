#DEFS=-DX_strerror
CFLAGS= -O $(DEFS)
LIBS=
FOBJ=fgrep.o kwset.o obstack.o std.o

fgrep: $(FOBJ)
	$(CC) $(CFLAGS) -o fgrep $(FOBJ) $(LIBS)

clean: FRC
	rm -f fgrep core $(FOBJ)

FRC:

fgrep.o: unix.h
kwset.o fgrep.o: kwset.h obstack.h std.h
obstack.o: obstack.h
std.o: std.h unix.h
