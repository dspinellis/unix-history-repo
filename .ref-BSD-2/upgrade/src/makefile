CFLAGS=-I/usr/include/retrofit -O

all: ucb from last ls mkstr msgs printenv reset setenv strings tset whereis
ucb:
	mkdir /usr/ucb

from: from.c
	${CC} ${CFLAGS} from.c -lretro -ltermlib -lS -s
	cp a.out /usr/ucb/from
last: last.c
	${CC} ${CFLAGS} last.c -lretro -ltermlib -lS -s
	cp a.out /usr/ucb/last
ls: ls.c
	${CC} -O ls.c -s
	cp a.out /usr/ucb/ls
mkstr: mkstr.c
	${CC} ${CFLAGS} mkstr.c -lretro -ltermlib -lS -s
	cp a.out /usr/ucb/mkstr
msgs: msgs.c
	${CC} ${CFLAGS} msgs.c -lretro -ltermlib -lS -s
	cp a.out /usr/ucb/msgs
printenv: printenv.c
	${CC} ${CFLAGS} printenv.c -lretro -ltermlib -lS -s
	cp a.out /usr/ucb/printenv
reset: reset.c
	${CC} -O reset.c -s
	cp a.out /usr/ucb/reset
setenv: setenv.c
	${CC} ${CFLAGS} setenv.c -lretro -ltermlib -lS -s
	cp a.out /usr/ucb/setenv
strings: strings.c
	${CC} ${CFLAGS} strings.c -lretro -ltermlib -lS -s
	cp a.out /usr/ucb/strings
tset: tset.c
	${CC} ${CFLAGS} tset.c -lretro -ltermlib -lS -s
	cp a.out /usr/ucb/tset
whereis: whereis.c
	${CC} ${CFLAGS} whereis.c -lretro -ltermlib -lS -s
	cp a.out /usr/ucb/whereis
xstr: xstr.c
	${CC} ${CFLAGS} xstr.c -lretro -ltermlib -lS -s
	cp a.out /usr/ucb/xstr
