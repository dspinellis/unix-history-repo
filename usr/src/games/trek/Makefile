#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
DESTDIR=
CFLAGS=	-O
LFLAGS=	
SRCS=	abandon.c attack.c autover.c capture.c check_out.c checkcond.c \
	compkl.c computer.c damage.c damaged.c dcrept.c destruct.c \
	dock.c dumpgame.c dumpme.c dumpssradio.c events.c externs.c \
	getcodi.c getpar.c help.c impulse.c initquad.c kill.c klmove.c \
	lose.c lrscan.c main.c move.c nova.c out.c phaser.c play.c ram.c \
	ranf.c rest.c schedule.c score.c setup.c setwarp.c shell.c \
	shield.c snova.c srscan.c systemname.c torped.c utility.c \
	visual.c warp.c win.c cgetc.c
OBJS=	abandon.o attack.o autover.o capture.o check_out.o checkcond.o \
	compkl.o computer.o damage.o damaged.o dcrept.o destruct.o \
	dock.o dumpgame.o dumpme.o dumpssradio.o events.o externs.o \
	getcodi.o getpar.o help.o impulse.o initquad.o kill.o klmove.o \
	lose.o lrscan.o main.o move.o nova.o out.o phaser.o play.o ram.o \
	ranf.o rest.o schedule.o score.o setup.o setwarp.o shell.o \
	shield.o snova.o srscan.o systemname.o torped.o utility.o \
	visual.o warp.o win.o cgetc.o
HDRS=	getpar.h trek.h

trek: 	${OBJS} ${HDRS}
	cc ${LFLAGS} -o trek ${OBJS} -lm

${OBJS}:
	${CC} ${CFLAGS} -c $*.c

tags:	tags $(SRCS)
	ctags $(SRCS)

install: trek
	install -s trek ${DESTDIR}/usr/games/trek

clean:
	rm -f a.out core ${OBJS} trek

depend:
	cat </dev/null >x.c
	for i in ${SRCS}; do \
		(echo `basename $$i .c`.o: $$i >>makedep; \
		/bin/grep '^#[ 	]*include' x.c $$i | sed \
			-e 's,<\(.*\)>,"/usr/include/\1",' \
			-e 's/:[^"]*"\([^"]*\)".*/: \1/' \
			-e 's/\.c/.o/' >>makedep); done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep x.c
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

abandon.o: abandon.c
abandon.o: trek.h
attack.o: attack.c
attack.o: trek.h
autover.o: autover.c
autover.o: trek.h
capture.o: capture.c
capture.o: trek.h
check_out.o: check_out.c
check_out.o: trek.h
checkcond.o: checkcond.c
checkcond.o: trek.h
compkl.o: compkl.c
compkl.o: trek.h
computer.o: computer.c
computer.o: trek.h
computer.o: getpar.h
computer.o: /usr/include/stdio.h
damage.o: damage.c
damage.o: trek.h
damaged.o: damaged.c
damaged.o: trek.h
dcrept.o: dcrept.c
dcrept.o: trek.h
destruct.o: destruct.c
destruct.o: trek.h
dock.o: dock.c
dock.o: trek.h
dumpgame.o: dumpgame.c
dumpgame.o: trek.h
dumpme.o: dumpme.c
dumpme.o: trek.h
dumpssradio.o: dumpssradio.c
dumpssradio.o: trek.h
events.o: events.c
events.o: trek.h
externs.o: externs.c
externs.o: trek.h
getcodi.o: getcodi.c
getcodi.o: getpar.h
getpar.o: getpar.c
getpar.o: /usr/include/stdio.h
getpar.o: getpar.h
help.o: help.c
help.o: trek.h
impulse.o: impulse.c
impulse.o: trek.h
initquad.o: initquad.c
initquad.o: trek.h
kill.o: kill.c
kill.o: trek.h
klmove.o: klmove.c
klmove.o: trek.h
lose.o: lose.c
lose.o: trek.h
lrscan.o: lrscan.c
lrscan.o: trek.h
main.o: main.c
main.o: trek.h
main.o: /usr/include/stdio.h
main.o: /usr/include/sgtty.h
move.o: move.c
move.o: trek.h
nova.o: nova.c
nova.o: trek.h
out.o: out.c
out.o: trek.h
phaser.o: phaser.c
phaser.o: trek.h
phaser.o: getpar.h
play.o: play.c
play.o: trek.h
play.o: getpar.h
ram.o: ram.c
ram.o: trek.h
ranf.o: ranf.c
ranf.o: /usr/include/stdio.h
rest.o: rest.c
rest.o: trek.h
rest.o: getpar.h
schedule.o: schedule.c
schedule.o: trek.h
score.o: score.c
score.o: trek.h
score.o: getpar.h
setup.o: setup.c
setup.o: trek.h
setup.o: getpar.h
setwarp.o: setwarp.c
setwarp.o: trek.h
setwarp.o: getpar.h
shell.o: shell.c
shield.o: shield.c
shield.o: trek.h
shield.o: getpar.h
snova.o: snova.c
snova.o: trek.h
srscan.o: srscan.c
srscan.o: trek.h
srscan.o: getpar.h
systemname.o: systemname.c
systemname.o: trek.h
torped.o: torped.c
torped.o: /usr/include/stdio.h
torped.o: trek.h
utility.o: utility.c
visual.o: visual.c
visual.o: trek.h
warp.o: warp.c
warp.o: trek.h
win.o: win.c
win.o: trek.h
win.o: getpar.h
cgetc.o: cgetc.c
cgetc.o: /usr/include/stdio.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
