# Copyright (c) 1992 by Texas Internet Consulting
# This code may be freely copied and used so long as this
# copyright notice is attached.  This code may not be sold
# without the express written permission of Texas Internet Consulting.
# Texas Internet Consulting makes no warranty as to the correctness
# nor the applicability of this code for any purpose.
PROGS= poke_ns
SCRIPTS= gendns genstatic make_rev readinfo updatehosts
TARGET=/usr/local/etc

poke_ns: poke_ns.c
	${CC} -O  poke_ns.c -o poke_ns

install: ${SCRIPTS} poke_ns
	for f in ${SCRIPTS}; do \
	install -m 775 $$f ${TARGET}/`basename $$f .sh`; done
	for f in ${PROGS}; do \
	install -s -m 775 $$f ${TARGET}; done
	chown root ${TARGET}/poke_ns
	chmod 4710 ${TARGET}/poke_ns
clean:
	rm -f ${PROGS} *.o
