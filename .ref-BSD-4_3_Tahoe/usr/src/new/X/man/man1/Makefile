.SUFFIXES: .1 .0

.1.0:
	/usr/man/manroff $*.1 > $*.0

OBJ1=	X.0 biff.0 bitmap.0 keycomp.0 pikapix.0 resize.0 uwm.0 xclock.0 \
	xcolors.0 xdemo.0 xdpr.0 xdvi.0 xfax.0 xfd.0 xhost.0 ximpv.0 \
	xinit.0 xload.0 xnwm.0 xperfmon.0 xpr.0 xrefresh.0 xset.0 xshell.0 \
	xterm.0 xtrek.0 xwd.0 xwininfo.0 xwm.0 xwud.0

all:;

install: ${OBJ1}
	install -c -o bin -g bin -m 444 ${OBJ1} ${DESTDIR}/usr/new/man/cat1

clean:
	rm -f *~ \#*

paper:
	for i in *.? *.[0-9]?; do \
		echo -n "Troffing ";\
		echo -n $$i;\
		echo '....';\
		${TROFF} -man $$i;\
	done
