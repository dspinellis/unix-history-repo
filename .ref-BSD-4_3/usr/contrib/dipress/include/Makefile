#
# Copyright (c) 1984, 1985 Xerox Corp.
#
#
#	copy include files to destination

DESTINCLUDE=

INCLUDES= ipnames.h iptokens.h literal.h operator.h texttoken.h

install: $(DESTINCLUDE)
	for i in ${INCLUDES}; do \
		(install -c -m 644 $$i ${DESTINCLUDE}); done

$(DESTINCLUDE):
	mkdir $(DESTINCLUDE)

distribution:

