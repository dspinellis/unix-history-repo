#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	7.1 (Berkeley) %G%
#
# Put the ../h stuff near the end so that subroutine definitions win when
# there is a struct tag with the same name (eg., vmmeter).  The real
# solution would probably be for ctags to generate "struct vmmeter" tags.
CTAGS=	ctags
SRCS =	../net/*.[ch] ../netimp/*.[ch] ../netinet/*.[ch] \
	../netns/*.[ch] ../sys/*.[ch] ../vaxif/*.[ch]  ../vaxmba/*.[ch] \
	../vaxuba/*.[ch] \
	../vax/autoconf.c ../vax/clock.c ../vax/clock.h ../vax/conf.c \
	../vax/cons.c ../vax/cons.h ../vax/cpu.h ../vax/cpudata.c \
	../vax/dkbad.c ../vax/dkio.h ../vax/flp.c ../vax/flp.h \
	../vax/frame.h \
	../vax/genassym.c ../vax/in_cksum.c ../vax/machdep.c \
	../vax/machparam.h ../vax/mem.c \
	../vax/mem.h ../vax/mscp.h ../vax/mtpr.h ../vax/nexus.h \
	../vax/pcb.h ../vax/psl.h ../vax/pte.h ../vax/pup_cksum.c \
	../vax/reg.h \
	../vax/rpb.h ../vax/rpb.s ../vax/rsp.h ../vax/scb.h \
	../vax/scb.s \
	../vax/swapgeneric.c ../vax/sys_machdep.c ../vax/trap.c \
	../vax/trap.h \
	../vax/tu.c ../vax/ufs_machdep.c ../vax/vm_machdep.c \
	../vax/vmparam.h \
	../h/*.h 

DIRS =	h net netimp netinet netns vax vaxif vaxmba vaxuba

all:
	@echo "make tags or links only"

tags: FRC
	mv tags tags.old
	-cp ../machine/astags tags
	${CTAGS} -a ${SRCS}
	sort -o tags.new tags
	mv tags.new tags
	rm -f tags.old

links:
	-for i in ${DIRS}; do rm -f ../$$i/tags; \
	ln -s ../sys/tags ../$$i/tags; done

FRC:
