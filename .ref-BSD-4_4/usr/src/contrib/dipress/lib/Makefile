#
# Copyright (c) 1984, 1985 Xerox Corp.
#
CFLAGS=	-O
DESTLIB=/usr/new/lib

all:

install: $(DESTLIB)
	cp -r fonts $(DESTLIB)
	install -c libip.a ${DESTLIB}
	install -c libip_p.a ${DESTLIB}
	install -c llib-lip.ln ${DESTLIB}

# Do a "make install" before running this command.
# Calling rdist so many times in not very efficient
rdist:
	for j in ${DESTMACHINES}; do \
		(rdist -wR -c ${DESTLIB}/fonts \
			${DESTLIB}/libip.a \
			${DESTLIB}/libip_p.a \
			${DESTLIB}/llib-lip.ln $$j); done;

$(DESTLIB):
	mkdir $(DESTLIB)

clean:
	rm -f libip.a libip_p.a llib-lip.ln libgetopt.a llib-lgetopt.ln
	rm -rf fonts/*
