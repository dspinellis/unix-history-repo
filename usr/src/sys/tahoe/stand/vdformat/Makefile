#	Makefile	1.2	86/11/04
#
COPTS=	-DSTANDALONE -DTAHOE -I../../h -I../..
CFLAGS=	-O ${COPTS}
RELOC=	800
LDTOPT= -T ${RELOC} -e _entry -x
SRCS=	cmd.c cons.c config.c correct.c delete.c exercise.c format.c help.c \
	info.c io.c list.c maps.c proc_cmd.c profile.c relocate.c \
	reset.c smd.c smd_e.c start.c status.c strpbrk.c strspn.c strtok.c \
	util.c vdfmt.c verify.c
OBJS=	cmd.o cons.o config.o correct.o delete.o exercise.o format.o help.o \
	info.o io.o list.o maps.o proc_cmd.o profile.o relocate.o \
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
cmd.o: ../../machine/machparam.h ../../h/signal.h /usr/include/sys/types.h
cmd.o: ../../h/buf.h ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
cmd.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h ./cmd.h
config.o: config.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
config.o: ../../h/param.h ../../machine/machparam.h ../../h/signal.h
config.o: /usr/include/sys/types.h ../../h/buf.h ../../h/inode.h ../../h/fs.h
config.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
correct.o: correct.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
correct.o: ../../h/param.h ../../machine/machparam.h ../../h/signal.h
correct.o: /usr/include/sys/types.h ../../h/buf.h ../../h/inode.h ../../h/fs.h
correct.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
correct.o: ./cmd.h
delete.o: delete.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
delete.o: ../../h/param.h ../../machine/machparam.h ../../h/signal.h
delete.o: /usr/include/sys/types.h ../../h/buf.h ../../h/inode.h ../../h/fs.h
delete.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
delete.o: ./cmd.h
exercise.o: exercise.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
exercise.o: ../../h/param.h ../../machine/machparam.h ../../h/signal.h
exercise.o: /usr/include/sys/types.h ../../h/buf.h ../../h/inode.h ../../h/fs.h
exercise.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
format.o: format.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
format.o: ../../h/param.h ../../machine/machparam.h ../../h/signal.h
format.o: /usr/include/sys/types.h ../../h/buf.h ../../h/inode.h ../../h/fs.h
format.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
help.o: help.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
help.o: ../../h/param.h ../../machine/machparam.h ../../h/signal.h
help.o: /usr/include/sys/types.h ../../h/buf.h ../../h/inode.h ../../h/fs.h
help.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
help.o: ./cmd.h
info.o: info.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
info.o: ../../h/param.h ../../machine/machparam.h ../../h/signal.h
info.o: /usr/include/sys/types.h ../../h/buf.h ../../h/inode.h ../../h/fs.h
info.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
io.o: io.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h ../../h/param.h
io.o: ../../machine/machparam.h ../../h/signal.h /usr/include/sys/types.h
io.o: ../../h/buf.h ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
io.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h ./cmd.h
list.o: list.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
list.o: ../../h/param.h ../../machine/machparam.h ../../h/signal.h
list.o: /usr/include/sys/types.h ../../h/buf.h ../../h/inode.h ../../h/fs.h
list.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
maps.o: maps.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
maps.o: ../../h/param.h ../../machine/machparam.h ../../h/signal.h
maps.o: /usr/include/sys/types.h ../../h/buf.h ../../h/inode.h ../../h/fs.h
maps.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
proc_cmd.o: proc_cmd.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
proc_cmd.o: ../../h/param.h ../../machine/machparam.h ../../h/signal.h
proc_cmd.o: /usr/include/sys/types.h ../../h/buf.h ../../h/inode.h ../../h/fs.h
proc_cmd.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
proc_cmd.o: ./cmd.h
profile.o: profile.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
profile.o: ../../h/param.h ../../machine/machparam.h ../../h/signal.h
profile.o: /usr/include/sys/types.h ../../h/buf.h ../../h/inode.h ../../h/fs.h
profile.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
relocate.o: relocate.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
relocate.o: ../../h/param.h ../../machine/machparam.h ../../h/signal.h
relocate.o: /usr/include/sys/types.h ../../h/buf.h ../../h/inode.h ../../h/fs.h
relocate.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
relocate.o: ./cmd.h
reset.o: reset.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
reset.o: ../../h/param.h ../../machine/machparam.h ../../h/signal.h
reset.o: /usr/include/sys/types.h ../../h/buf.h ../../h/inode.h ../../h/fs.h
reset.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
smd.o: smd.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h ../../h/param.h
smd.o: ../../machine/machparam.h ../../h/signal.h /usr/include/sys/types.h
smd.o: ../../h/buf.h ../../h/inode.h ../../h/fs.h ../../tahoevba/vbaparam.h
smd.o: ../../tahoevba/vdreg.h ../../tahoe/cp.h
smd_e.o: smd_e.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
smd_e.o: ../../h/param.h ../../machine/machparam.h ../../h/signal.h
smd_e.o: /usr/include/sys/types.h ../../h/buf.h ../../h/inode.h ../../h/fs.h
smd_e.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
start.o: start.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
start.o: ../../h/param.h ../../machine/machparam.h ../../h/signal.h
start.o: /usr/include/sys/types.h ../../h/buf.h ../../h/inode.h ../../h/fs.h
start.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
status.o: status.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
status.o: ../../h/param.h ../../machine/machparam.h ../../h/signal.h
status.o: /usr/include/sys/types.h ../../h/buf.h ../../h/inode.h ../../h/fs.h
status.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
strpbrk.o: strpbrk.c
strspn.o: strspn.c
strtok.o: strtok.c
util.o: util.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
util.o: ../../h/param.h ../../machine/machparam.h ../../h/signal.h
util.o: /usr/include/sys/types.h ../../h/buf.h ../../h/inode.h ../../h/fs.h
util.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
util.o: ./cmd.h
vdfmt.o: vdfmt.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
vdfmt.o: ../../h/param.h ../../machine/machparam.h ../../h/signal.h
vdfmt.o: /usr/include/sys/types.h ../../h/buf.h ../../h/inode.h ../../h/fs.h
vdfmt.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
verify.o: verify.c ./vdfmt.h /usr/include/setjmp.h ../../tahoe/mtpr.h
verify.o: ../../h/param.h ../../machine/machparam.h ../../h/signal.h
verify.o: /usr/include/sys/types.h ../../h/buf.h ../../h/inode.h ../../h/fs.h
verify.o: ../../tahoevba/vbaparam.h ../../tahoevba/vdreg.h ../../tahoe/cp.h
