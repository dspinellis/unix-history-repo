# Hack or Quest Makefile.

# on some systems the termcap library is in -ltermcap
TERMLIB = -ltermlib


# make hack
GAME = hack
GAMEDIR = /usr/games/lib/hackdir
CFLAGS = -O
HACKCSRC = hack.Decl.c\
	hack.apply.c hack.bones.c hack.c hack.cmd.c hack.do.c\
	hack.do_name.c hack.do_wear.c hack.dog.c hack.eat.c hack.end.c\
	hack.engrave.c hack.fight.c hack.invent.c hack.ioctl.c\
	hack.lev.c hack.main.c hack.makemon.c hack.mhitu.c\
	hack.mklev.c hack.mkmaze.c hack.mkobj.c hack.mkshop.c\
	hack.mon.c hack.monst.c hack.o_init.c hack.objnam.c\
	hack.options.c hack.pager.c hack.potion.c hack.pri.c\
	hack.read.c hack.rip.c hack.rumors.c hack.save.c\
	hack.search.c hack.shk.c hack.shknam.c hack.steal.c\
	hack.termcap.c hack.timeout.c hack.topl.c\
	hack.track.c hack.trap.c hack.tty.c hack.unix.c\
	hack.u_init.c hack.vault.c\
	hack.wield.c hack.wizard.c hack.worm.c hack.worn.c hack.zap.c\
	hack.version.c rnd.c alloc.c

CSOURCES = $(HACKCSRC) makedefs.c

HSOURCES = hack.h hack.mfndpos.h config.h\
	def.edog.h def.eshk.h def.flag.h def.func_tab.h def.gold.h\
	def.mkroom.h\
	def.monst.h def.obj.h def.objclass.h def.objects.h\
	def.permonst.h def.rm.h def.trap.h def.wseg.h

SOURCES = $(CSOURCES) $(HSOURCES)

AUX = data help hh rumors hack.6 hack.sh

DISTR = $(SOURCES) $(AUX) READ_ME Makefile date.h hack.onames.h

HOBJ = hack.Decl.o hack.apply.o hack.bones.o hack.o hack.cmd.o hack.do.o\
	hack.do_name.o hack.do_wear.o hack.dog.o hack.eat.o hack.end.o\
	hack.engrave.o hack.fight.o hack.invent.o hack.ioctl.o\
	hack.lev.o hack.main.o hack.makemon.o hack.mhitu.o hack.mklev.o\
	hack.mkmaze.o hack.mkobj.o hack.mkshop.o hack.mon.o\
	hack.monst.o hack.o_init.o hack.objnam.o hack.options.o\
	hack.pager.o hack.potion.o hack.pri.o\
	hack.read.o hack.rip.o hack.rumors.o hack.save.o\
	hack.search.o hack.shk.o hack.shknam.o hack.steal.o\
	hack.termcap.o hack.timeout.o hack.topl.o\
	hack.track.o hack.trap.o\
	hack.tty.o hack.unix.o hack.u_init.o hack.vault.o hack.wield.o\
	hack.wizard.o hack.worm.o hack.worn.o hack.zap.o\
	hack.version.o rnd.o alloc.o

all: hack
hack: ${HOBJ} ${LIBC}
	@echo "Loading ..."
	@ld -X -o $@ /lib/crt0.o $(HOBJ) $(TERMLIB) -lc

makedefs:	makedefs.c
	${CC} ${CFLAGS} -o $@ makedefs.c

hack.onames.h:	makedefs def.objects.h
	makedefs > hack.onames.h

lint:
# lint cannot have -p here because (i) capitals are meaningful:
# [Ww]izard, (ii) identifiers may coincide in the first six places:
# doweararm() versus dowearring().
# _flsbuf comes from <stdio.h>, a bug in the system libraries.
	@echo lint -axbh -DLINT ...
	@lint -axbh -DLINT $(HACKCSRC) | sed '/_flsbuf/d'


diff:
	@- for i in $(SOURCES) $(AUX) ; do \
		cmp -s $$i $D/$$i || \
		( echo diff $D/$$i $$i ; diff $D/$$i $$i ; echo ) ; done

distribution: Makefile
	@- for i in READ_ME $(SOURCES) $(AUX) Makefile date.h hack.onames.h\
		; do \
		cmp -s $$i $D/$$i || \
		( echo cp $$i $D ; cp $$i $D ) ; done
# the distribution directory also contains the empty files perm and record.

install:
	install -s -o games -g bin -m 4700 hack ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f hack; ln -s dm hack; chown games.bin hack)
	-[ -d ${DESTDIR}/usr/games/lib/hackdir ] || mkdir ${DESTDIR}/usr/games/lib/hackdir
	chmod 700 ${DESTDIR}/usr/games/lib/hackdir
	chown games.bin ${DESTDIR}/usr/games/lib/hackdir
	install -c -o games -g bin -m 666 /dev/null ${DESTDIR}/usr/games/lib/hackdir/perm
	install -c -o games -g bin -m 666 /dev/null ${DESTDIR}/usr/games/lib/hackdir/record
	install -c -o games -g bin -m 666 hh ${DESTDIR}/usr/games/lib/hackdir/hh
	rm -f ${DESTDIR}/usr/games/lib/hackdir/bones*

clean:
	rm -f *.o hack makedefs

depend:
	mkdep ${CFLAGS} ${CSOURCES}

tags:
	ctags ${CSOURCES}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

hack.Decl.o: hack.Decl.c hack.h config.h /usr/include/strings.h def.objclass.h
hack.Decl.o: def.monst.h def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.Decl.o: def.permonst.h hack.onames.h def.mkroom.h
hack.apply.o: hack.apply.c hack.h config.h /usr/include/strings.h
hack.apply.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.apply.o: def.flag.h def.rm.h def.permonst.h hack.onames.h def.edog.h
hack.apply.o: def.mkroom.h
hack.bones.o: hack.bones.c hack.h config.h /usr/include/strings.h
hack.bones.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.bones.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.o: hack.c hack.h config.h /usr/include/strings.h def.objclass.h
hack.o: def.monst.h def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.o: def.permonst.h hack.onames.h /usr/include/stdio.h
hack.cmd.o: hack.cmd.c hack.h config.h /usr/include/strings.h def.objclass.h
hack.cmd.o: def.monst.h def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.cmd.o: def.permonst.h hack.onames.h def.func_tab.h
hack.do.o: hack.do.c hack.h config.h /usr/include/strings.h def.objclass.h
hack.do.o: def.monst.h def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.do.o: def.permonst.h hack.onames.h
hack.do_name.o: hack.do_name.c hack.h config.h /usr/include/strings.h
hack.do_name.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.do_name.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.do_name.o: /usr/include/stdio.h
hack.do_wear.o: hack.do_wear.c hack.h config.h /usr/include/strings.h
hack.do_wear.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.do_wear.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.do_wear.o: /usr/include/stdio.h
hack.dog.o: hack.dog.c hack.h config.h /usr/include/strings.h def.objclass.h
hack.dog.o: def.monst.h def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.dog.o: def.permonst.h hack.onames.h hack.mfndpos.h def.edog.h def.mkroom.h
hack.eat.o: hack.eat.c hack.h config.h /usr/include/strings.h def.objclass.h
hack.eat.o: def.monst.h def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.eat.o: def.permonst.h hack.onames.h
hack.end.o: hack.end.c hack.h config.h /usr/include/strings.h def.objclass.h
hack.end.o: def.monst.h def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.end.o: def.permonst.h hack.onames.h /usr/include/stdio.h
hack.end.o: /usr/include/signal.h
hack.engrave.o: hack.engrave.c hack.h config.h /usr/include/strings.h
hack.engrave.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.engrave.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.fight.o: hack.fight.c hack.h config.h /usr/include/strings.h
hack.fight.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.fight.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.invent.o: hack.invent.c hack.h config.h /usr/include/strings.h
hack.invent.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.invent.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.invent.o: /usr/include/stdio.h def.wseg.h
hack.ioctl.o: hack.ioctl.c /usr/include/stdio.h config.h /usr/include/sgtty.h
hack.ioctl.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
hack.ioctl.o: /usr/include/sys/ttydev.h /usr/include/signal.h
hack.lev.o: hack.lev.c hack.h config.h /usr/include/strings.h def.objclass.h
hack.lev.o: def.monst.h def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.lev.o: def.permonst.h hack.onames.h def.mkroom.h /usr/include/stdio.h
hack.lev.o: def.wseg.h
hack.main.o: hack.main.c /usr/include/stdio.h /usr/include/signal.h hack.h
hack.main.o: config.h /usr/include/strings.h def.objclass.h def.monst.h
hack.main.o: def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h def.permonst.h
hack.main.o: hack.onames.h
hack.makemon.o: hack.makemon.c hack.h config.h /usr/include/strings.h
hack.makemon.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.makemon.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.mhitu.o: hack.mhitu.c hack.h config.h /usr/include/strings.h
hack.mhitu.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.mhitu.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.mklev.o: hack.mklev.c hack.h config.h /usr/include/strings.h
hack.mklev.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.mklev.o: def.flag.h def.rm.h def.permonst.h hack.onames.h def.mkroom.h
hack.mkmaze.o: hack.mkmaze.c hack.h config.h /usr/include/strings.h
hack.mkmaze.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.mkmaze.o: def.flag.h def.rm.h def.permonst.h hack.onames.h def.mkroom.h
hack.mkobj.o: hack.mkobj.c hack.h config.h /usr/include/strings.h
hack.mkobj.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.mkobj.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.mkshop.o: hack.mkshop.c hack.h config.h /usr/include/strings.h
hack.mkshop.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.mkshop.o: def.flag.h def.rm.h def.permonst.h hack.onames.h def.mkroom.h
hack.mkshop.o: def.eshk.h
hack.mon.o: hack.mon.c hack.h config.h /usr/include/strings.h def.objclass.h
hack.mon.o: def.monst.h def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.mon.o: def.permonst.h hack.onames.h hack.mfndpos.h
hack.monst.o: hack.monst.c hack.h config.h /usr/include/strings.h
hack.monst.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.monst.o: def.flag.h def.rm.h def.permonst.h hack.onames.h def.eshk.h
hack.o_init.o: hack.o_init.c config.h def.objects.h config.h def.objclass.h
hack.o_init.o: hack.onames.h
hack.objnam.o: hack.objnam.c hack.h config.h /usr/include/strings.h
hack.objnam.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.objnam.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.options.o: hack.options.c config.h hack.h config.h /usr/include/strings.h
hack.options.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.options.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.pager.o: hack.pager.c /usr/include/stdio.h /usr/include/signal.h hack.h
hack.pager.o: config.h /usr/include/strings.h def.objclass.h def.monst.h
hack.pager.o: def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.pager.o: def.permonst.h hack.onames.h /usr/include/sys/wait.h
hack.pager.o: /usr/include/machine/machparam.h
hack.potion.o: hack.potion.c hack.h config.h /usr/include/strings.h
hack.potion.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.potion.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.pri.o: hack.pri.c hack.h config.h /usr/include/strings.h def.objclass.h
hack.pri.o: def.monst.h def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.pri.o: def.permonst.h hack.onames.h /usr/include/stdio.h def.wseg.h
hack.read.o: hack.read.c hack.h config.h /usr/include/strings.h def.objclass.h
hack.read.o: def.monst.h def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.read.o: def.permonst.h hack.onames.h
hack.rip.o: hack.rip.c /usr/include/stdio.h hack.h config.h
hack.rip.o: /usr/include/strings.h def.objclass.h def.monst.h def.gold.h
hack.rip.o: def.trap.h def.obj.h def.flag.h def.rm.h def.permonst.h
hack.rip.o: hack.onames.h
hack.rumors.o: hack.rumors.c /usr/include/stdio.h hack.h config.h
hack.rumors.o: /usr/include/strings.h def.objclass.h def.monst.h def.gold.h
hack.rumors.o: def.trap.h def.obj.h def.flag.h def.rm.h def.permonst.h
hack.rumors.o: hack.onames.h
hack.save.o: hack.save.c hack.h config.h /usr/include/strings.h def.objclass.h
hack.save.o: def.monst.h def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.save.o: def.permonst.h hack.onames.h /usr/include/signal.h
hack.search.o: hack.search.c hack.h config.h /usr/include/strings.h
hack.search.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.search.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.shk.o: hack.shk.c hack.h config.h /usr/include/strings.h def.objclass.h
hack.shk.o: def.monst.h def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.shk.o: def.permonst.h hack.onames.h hack.mfndpos.h def.mkroom.h def.eshk.h
hack.shknam.o: hack.shknam.c hack.h config.h /usr/include/strings.h
hack.shknam.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.shknam.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.steal.o: hack.steal.c hack.h config.h /usr/include/strings.h
hack.steal.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.steal.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.termcap.o: hack.termcap.c /usr/include/stdio.h config.h def.flag.h
hack.timeout.o: hack.timeout.c hack.h config.h /usr/include/strings.h
hack.timeout.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.timeout.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.topl.o: hack.topl.c hack.h config.h /usr/include/strings.h def.objclass.h
hack.topl.o: def.monst.h def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.topl.o: def.permonst.h hack.onames.h /usr/include/stdio.h
hack.track.o: hack.track.c hack.h config.h /usr/include/strings.h
hack.track.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.track.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.trap.o: hack.trap.c hack.h config.h /usr/include/strings.h def.objclass.h
hack.trap.o: def.monst.h def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.trap.o: def.permonst.h hack.onames.h def.mkroom.h
hack.tty.o: hack.tty.c hack.h config.h /usr/include/strings.h def.objclass.h
hack.tty.o: def.monst.h def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.tty.o: def.permonst.h hack.onames.h /usr/include/stdio.h
hack.tty.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
hack.tty.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
hack.unix.o: hack.unix.c /usr/include/stdio.h /usr/include/errno.h hack.h
hack.unix.o: config.h /usr/include/strings.h def.objclass.h def.monst.h
hack.unix.o: def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h def.permonst.h
hack.unix.o: hack.onames.h /usr/include/sys/types.h /usr/include/sys/stat.h
hack.unix.o: /usr/include/sys/time.h /usr/include/time.h
hack.u_init.o: hack.u_init.c hack.h config.h /usr/include/strings.h
hack.u_init.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.u_init.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.u_init.o: /usr/include/stdio.h /usr/include/signal.h
hack.vault.o: hack.vault.c hack.h config.h /usr/include/strings.h
hack.vault.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.vault.o: def.flag.h def.rm.h def.permonst.h hack.onames.h def.mkroom.h
hack.wield.o: hack.wield.c hack.h config.h /usr/include/strings.h
hack.wield.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.wield.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.wizard.o: hack.wizard.c hack.h config.h /usr/include/strings.h
hack.wizard.o: def.objclass.h def.monst.h def.gold.h def.trap.h def.obj.h
hack.wizard.o: def.flag.h def.rm.h def.permonst.h hack.onames.h
hack.worm.o: hack.worm.c hack.h config.h /usr/include/strings.h def.objclass.h
hack.worm.o: def.monst.h def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.worm.o: def.permonst.h hack.onames.h def.wseg.h
hack.worn.o: hack.worn.c hack.h config.h /usr/include/strings.h def.objclass.h
hack.worn.o: def.monst.h def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.worn.o: def.permonst.h hack.onames.h
hack.zap.o: hack.zap.c hack.h config.h /usr/include/strings.h def.objclass.h
hack.zap.o: def.monst.h def.gold.h def.trap.h def.obj.h def.flag.h def.rm.h
hack.zap.o: def.permonst.h hack.onames.h
hack.version.o: hack.version.c date.h
rnd.o: rnd.c
alloc.o: alloc.c
makedefs.o: makedefs.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
