#
# Copyright (c) 1984, 1985, 1986 Xerox Corp.
#
# HISTORY
#  Dec  1 1985 Lee Moore   Xerox Webster Research Center
#	added to toolkit


DESTBIN=../../../bin

makextdev: makextdev.o
	$(CC) $(CFLAGS) -o makextdev makextdev.o

makextdev.o: deviceinfo.h

lint:
	lint makextdev.c -lc

install: $(DESTBIN)/makextdev

$(DESTBIN)/makextdev: makextdev
	install -c -s makextdev $(DESTBIN)

clean:
	rm -f makextdev *.o
