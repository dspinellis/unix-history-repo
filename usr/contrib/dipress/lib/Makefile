#
# Copyright (c) 1984, 1985 Xerox Corp.
#
CFLAGS=	-O
DESTLIB=/usr/new/lib

all:

install: $(DESTLIB)
	cp -r fonts $(DESTLIB)
	install -c libip.a ${DESTLIB}
	install -c llib-lip.ln ${DESTLIB}

$(DESTLIB):
	mkdir $(DESTLIB)

clean:
	rm -f libip.a llib-lip.ln
	rm -rf fonts/*
