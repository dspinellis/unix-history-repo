#
# Copyright (c) 1984, 1985 Xerox Corp.
#
#
#	copy include files to destination

DESTINCLUDE=

INCLUDES= ipnames.h iptokens.h literal.h operator.h

install: $(DESTINCLUDE)
	for i in ${INCLUDES}; do \
		(install -c -m 644 $$i ${DESTINCLUDE}); done

# Do a "make install" before running this command.
# Calling rdist so many times in not very efficient
rdist:
	for i in ${INCLUDES}; do \
		for j in ${DESTMACHINES}; do \
			(rdist -c ${DESTINCLUDE}/$$i $$j); done; done

$(DESTINCLUDE):
	mkdir $(DESTINCLUDE)

distribution:

