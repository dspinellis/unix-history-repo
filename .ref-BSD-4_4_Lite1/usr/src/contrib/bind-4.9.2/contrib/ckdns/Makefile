BIN=/usr/local/etc
MAN=/usr/local/man/man8
MANEXT=8
LIB=/usr/local/lib/doc

all:

install:
	install -d $(BIN)
	install doc $(BIN)
	install -d $(MAN)
	install -m 444 doc.1 $(MAN)/doc.$(MANEXT)
	install -d $(LIB)
	for f in *.awk; do \
		install $$f $(LIB); \
	done

