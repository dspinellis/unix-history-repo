#	Makefile	1.4	87/11/23
#
DESTDIR=
COPTS=	-DSTANDALONE -DTAHOE -I../../h -I../..
CFLAGS=	-O ${COPTS}
RELOC=	800
LDTOPT= -T ${RELOC} -e _entry -x
SRCS=	cmd.c cons.c config.c correct.c delete.c disklabel.c exercise.c help.c \
	format.c info.c io.c list.c maps.c proc_cmd.c profile.c relocate.c \
	reset.c smd.c smd_e.c start.c status.c strpbrk.c strspn.c strtok.c \
	util.c vdfmt.c verify.c
OBJS=	cmd.o cons.o config.o correct.o delete.o disklabel.o exercise.o help.o \
	format.o info.o io.o list.o maps.o proc_cmd.o profile.o relocate.o \
	reset.o smd.o smd_e.o start.o status.o strpbrk.o strspn.o strtok.o \
	util.o vdfmt.o verify.o
ALL=	vdformat

all: ${ALL}

vdformat: ${OBJS} ../srt0.o ../libsa.a
	ld -o vdformat ${LDTOPT} ../srt0.o ${OBJS} ../libsa.a -lc

clean:
	rm -f ${OBJS} ${ALL} a.out core t.c errs tags Makefile.bak

../srt0.o: 
	@(cd ..; make srt0.o)

install:
	install -m 644 vdformat ${DESTDIR}/stand/vdformat

depend:
	${CC} -M ${COPTS} ${SRCS} | \
	sed -e ':loop' \
	    -e 's/\.\.\/[^ /]*\/\.\./../' \
	    -e 't loop' | \
	awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$2 } } \
	      END { print rec } ' > makedep
	echo '/^# DO NOT DELETE THIS LINE/+1,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ex - Makefile < eddep
	rm eddep makedep

# DO NOT DELETE THIS LINE -- make depend uses it

cmd.o: cmd.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h ../../h/param.h
cmd.o: /usr/include/sys/types.h ../../h/signal.h ../../machine/machparam.h
cmd.o: ../../h/buf.h ../../h/disklabel.h ../../h/inode.h ../../h/fs.h
cmd.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
cmd.o: ./cmd.h
cons.o: cons.c ../../machine/mtpr.h ../../h/param.h /usr/include/sys/types.h
cons.o: ../../h/signal.h ../../machine/machparam.h ../../tahoe/cp.h
config.o: config.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
config.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
config.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
config.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
config.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h
correct.o: correct.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
correct.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
correct.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
correct.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
correct.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h ./cmd.h
delete.o: delete.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
delete.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
delete.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
delete.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
delete.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h ./cmd.h
disklabel.o: disklabel.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
disklabel.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
disklabel.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
disklabel.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
disklabel.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h ./cmd.h
exercise.o: exercise.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
exercise.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
exercise.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
exercise.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
exercise.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h
help.o: help.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
help.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
help.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
help.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
help.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h ./cmd.h
format.o: format.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
format.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
format.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
format.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
format.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h
info.o: info.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
info.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
info.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
info.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
info.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h
io.o: io.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h ../../h/param.h
io.o: /usr/include/sys/types.h ../../h/signal.h ../../machine/machparam.h
io.o: ../../h/buf.h ../../h/disklabel.h ../../h/inode.h ../../h/fs.h
io.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h ./cmd.h
list.o: list.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
list.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
list.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
list.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
list.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h
maps.o: maps.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
maps.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
maps.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
maps.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
maps.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h
proc_cmd.o: proc_cmd.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
proc_cmd.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
proc_cmd.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
proc_cmd.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
proc_cmd.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h ./cmd.h
profile.o: profile.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
profile.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
profile.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
profile.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
profile.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h
relocate.o: relocate.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
relocate.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
relocate.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
relocate.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
relocate.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h ./cmd.h
reset.o: reset.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
reset.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
reset.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
reset.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
reset.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h
smd.o: smd.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h ../../h/param.h
smd.o: /usr/include/sys/types.h ../../h/signal.h ../../machine/machparam.h
smd.o: ../../h/buf.h ../../h/disklabel.h ../../h/inode.h ../../h/fs.h
smd.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
smd_e.o: smd_e.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
smd_e.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
smd_e.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
smd_e.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
smd_e.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h
start.o: start.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
start.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
start.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
start.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
start.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h
status.o: status.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
status.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
status.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
status.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
status.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h
strpbrk.o: strpbrk.c
strspn.o: strspn.c
strtok.o: strtok.c
util.o: util.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
util.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
util.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
util.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
util.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h ./cmd.h
vdfmt.o: vdfmt.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
vdfmt.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
vdfmt.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
vdfmt.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
vdfmt.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h
verify.o: verify.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
verify.o: ../../h/param.h /usr/include/sys/types.h ../../h/signal.h
verify.o: ../../machine/machparam.h ../../h/buf.h ../../h/disklabel.h
verify.o: ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
verify.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h
