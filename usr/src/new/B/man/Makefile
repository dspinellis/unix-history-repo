# Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984.

DEST1 = /usr/man/man1
DEST5 = /usr/man/man5

PRINT = vtroff -man

all:

install:
	cp man1/b.1 $(DEST1)/b.1
	cp man5/bterminal.5 $(DEST5)/bterminal.5

clean:

update:

print:
	cd man1; $(PRINT) b.1
	cd man5; $(PRINT) bterminal.5
