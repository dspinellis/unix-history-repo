#
#	sdb	Makefile	4.4	%G%
#
# Conditional compilation flags:
#	SDB	this is sdb
#	STD - Distribution systems UNIX/32V, VM/UNIX 2.n
#	VMUNIX - virtual memory system - keep namelist in VM.
#	VFORK - Use vfork call of VM/UNIX Berkeley version
#	FLEXNAMES - New a.out format with string table (VMUNIX Version 4)
#
# The files old.c and old.h contain routines and definitions peculiar
# to old versions of the system.
#
CFLAGS=	-R -DSDB -DVMUNIX -DVFORK -DFLEXNAMES

ADB = /usr/src/bin/adb
AS = /usr/src/bin/as
SHARE = $(ADB)

SRCS=	access.c decode.c display.c docomm.c fio.c main.c message.c \
	old.c $(SHARE)/opset.c $(SHARE)/optab.c pcs.c prvar.c re.c runpcs.c \
	setup.c sub.c symt.c udef.c version.c xeq.c reginit.c
OBJS=	access.o decode.o display.o docomm.o fio.o main.o message.o \
	old.o opset.o optab.o pcs.o prvar.o re.o runpcs.o \
	setup.o sub.o symt.o udef.o version.o xeq.o reginit.o
HDRS=	cdefs.h defs.h head.h mac.h machine.h mode.h old.h

sdb: sources ${OBJS}
	cc -o sdb *.o

#
#	An apparent bug in /lib/cpp prevents us from compiling
#	directly from $(SHARE)/{opset,optab}.c; the wrong
#	version of head.h is included (cpp gets $(SHARE)/head.h;
#	we want head.h).  Consequently, the copy
#
opset.c: $(SHARE)/opset.c
	cp $(SHARE)/opset.c opset.c
optab.c: $(SHARE)/optab.c
	cp $(SHARE)/optab.c optab.c
instrs.adb: $(SHARE)/instrs.adb
	cp $(SHARE)/instrs.adb instrs.adb
#
#	Now, compile them
#
opset.o: opset.c
	$(CC) -I. -I$(AS) $(CFLAGS) -c opset.c
optab.o: optab.c instrs.adb
	$(CC) -I. -I$(AS) $(CFLAGS) -c optab.c
$(SHARE)/instrs.adb: $(AS)/instrs
	cd $(SHARE); make instrs.adb
install:
	cp sdb sdb.temp
	install -s sdb $(DESTDIR)/usr/bin
	mv sdb.temp sdb
clean:
	rm -f *.o sdb

print:
	ls -l Makefile | pr
	pr -f *.h *.c

sources: ${SRCS} ${HDRS}
${SRCS} ${HDRS}:
	sccs get $@

depend:
	/bin/grep '^#[ 	]*include' ${SRCS} \
		| sed '/<.*>/d' \
		| sed 's/:[^"]*"\([^"]*\)".*/: \1/' \
		| sed 's/\.c/.o/' >>makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it
