#	@(#)Makefile.install	5.7 (Berkeley) 5/12/90
#
# Makefile for /usr/include, used to convert system include subdirectories
# between symbolic links and copies of kernel headers.  May also be used to
# update copies from kernel header files.
#
# The ``rm -rf''s used below are safe because rm doesn't follow symbolic
# links.
#

SYS=/sys
DIRS=	sys net netimp netinet netns ${MACHINE}
.if (${MACHINE} == "tahoe")
DIRS+=	tahoeif tahoevba
.elif (${MACHINE} == "vax")
DIRS+=	vaxif vaxmba vaxuba
.endif

all:
	@echo "\"make symlinks\", \"make copies\", or \"make update\" only"
	@false

copies:
	rm -rf machine
	ln -s ${MACHINE} machine
	-for i in ${DIRS}; do \
		rm -rf $$i; \
		(cd ${SYS}; tar cf - $$i/*.h) | tar xpfB -; \
	done

symlinks:
	rm -rf machine
	ln -s ${MACHINE} machine
	for i in ${DIRS}; do \
		rm -rf $$i; \
		ln -s ${SYS}/$$i $$i; \
	done

update:
	rm -rf machine
	ln -s ${MACHINE} machine
	-for i in ${DIRS}; do \
		if [ ! -d $$i ]; \
		then \
			mkdir $$i; \
		fi; \
		for j in `cd ${SYS}/$$i; echo *.[ih]`; do \
			cmp -s ${SYS}/$$i/$$j $$i/$$j || \
			install -c -o ${BINOWN} -g ${BINGRP} -m 444 \
			    ${SYS}/$$i/$$j $$i/$$j; \
		done; \
	done

clean cleandir depend lint tags:
