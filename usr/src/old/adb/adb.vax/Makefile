#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1	(Berkeley)	%G%
#
LIBC=	/lib/libc.a
DFLAGS=	-DADB
CFLAGS=	-O -w ${DFLAGS}
#	path to the directory containing as
AS =	../../as/as.vax
SRCS=	access.c command.c expr.c format.c input.c main.c message.c \
	opset.c optab.c output.c pcs.c print.c runpcs.c setup.c sym.c
OBJS=	access.o command.o expr.o format.o input.o main.o message.o \
	opset.o optab.o output.o pcs.o print.o runpcs.o setup.o sym.o

all: adb

adb:	${OBJS} ${LIBC}
	${CC} -o $@ ${OBJS}

message.o:
	${CC} -c -R message.c

opset.o:
	${CC} ${CFLAGS} -c -I. -I${AS} opset.c

optab.o: instrs.adb
	${CC} ${CFLAGS} -c -R -I. -I${AS} optab.c

instrs.adb: ${AS}/instrs
	(echo FLAVOR ADB; cat ${AS}/instrs) | awk -f ${AS}/instrs > instrs.adb

clean: FRC
	rm -f ${OBJS} core adb

depend: FRC
	mkdep ${CFLAGS} -I. -I${AS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 adb ${DESTDIR}/bin/adb

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

access.o: access.c defs.h /usr/include/machine/psl.h /usr/include/machine/pte.h
access.o: /usr/include/sys/param.h /usr/include/sys/types.h
access.o: /usr/include/signal.h /usr/include/machine/machparam.h
access.o: /usr/include/sys/dir.h /usr/include/sys/user.h
access.o: /usr/include/machine/pcb.h /usr/include/sys/dmap.h
access.o: /usr/include/sys/time.h /usr/include/time.h
access.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
access.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/ctype.h
access.o: /usr/include/a.out.h /usr/include/sys/exec.h
access.o: /usr/include/sys/ptrace.h mac.h mode.h machine.h
access.o: /usr/include/sys/vm.h /usr/include/sys/vmparam.h
access.o: /usr/include/machine/vmparam.h /usr/include/sys/vmmac.h
access.o: /usr/include/sys/vmmeter.h /usr/include/sys/vmsystm.h head.h
command.o: command.c defs.h /usr/include/machine/psl.h
command.o: /usr/include/machine/pte.h /usr/include/sys/param.h
command.o: /usr/include/sys/types.h /usr/include/signal.h
command.o: /usr/include/machine/machparam.h /usr/include/sys/dir.h
command.o: /usr/include/sys/user.h /usr/include/machine/pcb.h
command.o: /usr/include/sys/dmap.h /usr/include/sys/time.h /usr/include/time.h
command.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
command.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/ctype.h
command.o: /usr/include/a.out.h /usr/include/sys/exec.h
command.o: /usr/include/sys/ptrace.h mac.h mode.h machine.h
command.o: /usr/include/sys/vm.h /usr/include/sys/vmparam.h
command.o: /usr/include/machine/vmparam.h /usr/include/sys/vmmac.h
command.o: /usr/include/sys/vmmeter.h /usr/include/sys/vmsystm.h head.h
expr.o: expr.c defs.h /usr/include/machine/psl.h /usr/include/machine/pte.h
expr.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
expr.o: /usr/include/machine/machparam.h /usr/include/sys/dir.h
expr.o: /usr/include/sys/user.h /usr/include/machine/pcb.h
expr.o: /usr/include/sys/dmap.h /usr/include/sys/time.h /usr/include/time.h
expr.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
expr.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/ctype.h
expr.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/sys/ptrace.h
expr.o: mac.h mode.h machine.h /usr/include/sys/vm.h /usr/include/sys/vmparam.h
expr.o: /usr/include/machine/vmparam.h /usr/include/sys/vmmac.h
expr.o: /usr/include/sys/vmmeter.h /usr/include/sys/vmsystm.h head.h
format.o: format.c defs.h /usr/include/machine/psl.h /usr/include/machine/pte.h
format.o: /usr/include/sys/param.h /usr/include/sys/types.h
format.o: /usr/include/signal.h /usr/include/machine/machparam.h
format.o: /usr/include/sys/dir.h /usr/include/sys/user.h
format.o: /usr/include/machine/pcb.h /usr/include/sys/dmap.h
format.o: /usr/include/sys/time.h /usr/include/time.h
format.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
format.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/ctype.h
format.o: /usr/include/a.out.h /usr/include/sys/exec.h
format.o: /usr/include/sys/ptrace.h mac.h mode.h machine.h
format.o: /usr/include/sys/vm.h /usr/include/sys/vmparam.h
format.o: /usr/include/machine/vmparam.h /usr/include/sys/vmmac.h
format.o: /usr/include/sys/vmmeter.h /usr/include/sys/vmsystm.h head.h
input.o: input.c defs.h /usr/include/machine/psl.h /usr/include/machine/pte.h
input.o: /usr/include/sys/param.h /usr/include/sys/types.h
input.o: /usr/include/signal.h /usr/include/machine/machparam.h
input.o: /usr/include/sys/dir.h /usr/include/sys/user.h
input.o: /usr/include/machine/pcb.h /usr/include/sys/dmap.h
input.o: /usr/include/sys/time.h /usr/include/time.h
input.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
input.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/ctype.h
input.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/sys/ptrace.h
input.o: mac.h mode.h machine.h /usr/include/sys/vm.h
input.o: /usr/include/sys/vmparam.h /usr/include/machine/vmparam.h
input.o: /usr/include/sys/vmmac.h /usr/include/sys/vmmeter.h
input.o: /usr/include/sys/vmsystm.h head.h
main.o: main.c defs.h /usr/include/machine/psl.h /usr/include/machine/pte.h
main.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
main.o: /usr/include/machine/machparam.h /usr/include/sys/dir.h
main.o: /usr/include/sys/user.h /usr/include/machine/pcb.h
main.o: /usr/include/sys/dmap.h /usr/include/sys/time.h /usr/include/time.h
main.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
main.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/ctype.h
main.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/sys/ptrace.h
main.o: mac.h mode.h machine.h /usr/include/sys/vm.h /usr/include/sys/vmparam.h
main.o: /usr/include/machine/vmparam.h /usr/include/sys/vmmac.h
main.o: /usr/include/sys/vmmeter.h /usr/include/sys/vmsystm.h head.h
message.o: message.c mac.h mode.h machine.h /usr/include/sys/vm.h
message.o: /usr/include/sys/vmparam.h /usr/include/machine/vmparam.h
message.o: /usr/include/sys/vmmac.h /usr/include/sys/vmmeter.h
message.o: /usr/include/sys/vmsystm.h
opset.o: opset.c defs.h /usr/include/machine/psl.h /usr/include/machine/pte.h
opset.o: /usr/include/sys/param.h /usr/include/sys/types.h
opset.o: /usr/include/signal.h /usr/include/machine/machparam.h
opset.o: /usr/include/sys/dir.h /usr/include/sys/user.h
opset.o: /usr/include/machine/pcb.h /usr/include/sys/dmap.h
opset.o: /usr/include/sys/time.h /usr/include/time.h
opset.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
opset.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/ctype.h
opset.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/sys/ptrace.h
opset.o: mac.h mode.h machine.h /usr/include/sys/vm.h
opset.o: /usr/include/sys/vmparam.h /usr/include/machine/vmparam.h
opset.o: /usr/include/sys/vmmac.h /usr/include/sys/vmmeter.h
opset.o: /usr/include/sys/vmsystm.h head.h ../../as/as.vax/instrs.h
opset.o: ../../as/as.vax/asnumber.h
optab.o: optab.c defs.h /usr/include/machine/psl.h /usr/include/machine/pte.h
optab.o: /usr/include/sys/param.h /usr/include/sys/types.h
optab.o: /usr/include/signal.h /usr/include/machine/machparam.h
optab.o: /usr/include/sys/dir.h /usr/include/sys/user.h
optab.o: /usr/include/machine/pcb.h /usr/include/sys/dmap.h
optab.o: /usr/include/sys/time.h /usr/include/time.h
optab.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
optab.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/ctype.h
optab.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/sys/ptrace.h
optab.o: mac.h mode.h machine.h /usr/include/sys/vm.h
optab.o: /usr/include/sys/vmparam.h /usr/include/machine/vmparam.h
optab.o: /usr/include/sys/vmmac.h /usr/include/sys/vmmeter.h
optab.o: /usr/include/sys/vmsystm.h head.h ../../as/as.vax/instrs.h instrs.adb
optab.o: ../../as/as.vax/assizetab.c
output.o: output.c defs.h /usr/include/machine/psl.h /usr/include/machine/pte.h
output.o: /usr/include/sys/param.h /usr/include/sys/types.h
output.o: /usr/include/signal.h /usr/include/machine/machparam.h
output.o: /usr/include/sys/dir.h /usr/include/sys/user.h
output.o: /usr/include/machine/pcb.h /usr/include/sys/dmap.h
output.o: /usr/include/sys/time.h /usr/include/time.h
output.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
output.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/ctype.h
output.o: /usr/include/a.out.h /usr/include/sys/exec.h
output.o: /usr/include/sys/ptrace.h mac.h mode.h machine.h
output.o: /usr/include/sys/vm.h /usr/include/sys/vmparam.h
output.o: /usr/include/machine/vmparam.h /usr/include/sys/vmmac.h
output.o: /usr/include/sys/vmmeter.h /usr/include/sys/vmsystm.h head.h
output.o: /usr/include/stdio.h
pcs.o: pcs.c defs.h /usr/include/machine/psl.h /usr/include/machine/pte.h
pcs.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
pcs.o: /usr/include/machine/machparam.h /usr/include/sys/dir.h
pcs.o: /usr/include/sys/user.h /usr/include/machine/pcb.h
pcs.o: /usr/include/sys/dmap.h /usr/include/sys/time.h /usr/include/time.h
pcs.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
pcs.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/ctype.h
pcs.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/sys/ptrace.h
pcs.o: mac.h mode.h machine.h /usr/include/sys/vm.h /usr/include/sys/vmparam.h
pcs.o: /usr/include/machine/vmparam.h /usr/include/sys/vmmac.h
pcs.o: /usr/include/sys/vmmeter.h /usr/include/sys/vmsystm.h head.h
print.o: print.c defs.h /usr/include/machine/psl.h /usr/include/machine/pte.h
print.o: /usr/include/sys/param.h /usr/include/sys/types.h
print.o: /usr/include/signal.h /usr/include/machine/machparam.h
print.o: /usr/include/sys/dir.h /usr/include/sys/user.h
print.o: /usr/include/machine/pcb.h /usr/include/sys/dmap.h
print.o: /usr/include/sys/time.h /usr/include/time.h
print.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
print.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/ctype.h
print.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/sys/ptrace.h
print.o: mac.h mode.h machine.h /usr/include/sys/vm.h
print.o: /usr/include/sys/vmparam.h /usr/include/machine/vmparam.h
print.o: /usr/include/sys/vmmac.h /usr/include/sys/vmmeter.h
print.o: /usr/include/sys/vmsystm.h head.h
runpcs.o: runpcs.c defs.h /usr/include/machine/psl.h /usr/include/machine/pte.h
runpcs.o: /usr/include/sys/param.h /usr/include/sys/types.h
runpcs.o: /usr/include/signal.h /usr/include/machine/machparam.h
runpcs.o: /usr/include/sys/dir.h /usr/include/sys/user.h
runpcs.o: /usr/include/machine/pcb.h /usr/include/sys/dmap.h
runpcs.o: /usr/include/sys/time.h /usr/include/time.h
runpcs.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
runpcs.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/ctype.h
runpcs.o: /usr/include/a.out.h /usr/include/sys/exec.h
runpcs.o: /usr/include/sys/ptrace.h mac.h mode.h machine.h
runpcs.o: /usr/include/sys/vm.h /usr/include/sys/vmparam.h
runpcs.o: /usr/include/machine/vmparam.h /usr/include/sys/vmmac.h
runpcs.o: /usr/include/sys/vmmeter.h /usr/include/sys/vmsystm.h head.h
setup.o: setup.c defs.h /usr/include/machine/psl.h /usr/include/machine/pte.h
setup.o: /usr/include/sys/param.h /usr/include/sys/types.h
setup.o: /usr/include/signal.h /usr/include/machine/machparam.h
setup.o: /usr/include/sys/dir.h /usr/include/sys/user.h
setup.o: /usr/include/machine/pcb.h /usr/include/sys/dmap.h
setup.o: /usr/include/sys/time.h /usr/include/time.h
setup.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
setup.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/ctype.h
setup.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/sys/ptrace.h
setup.o: mac.h mode.h machine.h /usr/include/sys/vm.h
setup.o: /usr/include/sys/vmparam.h /usr/include/machine/vmparam.h
setup.o: /usr/include/sys/vmmac.h /usr/include/sys/vmmeter.h
setup.o: /usr/include/sys/vmsystm.h head.h /usr/include/frame.h
setup.o: /usr/include/ctype.h /usr/include/sys/stat.h /usr/include/sys/file.h
sym.o: sym.c defs.h /usr/include/machine/psl.h /usr/include/machine/pte.h
sym.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
sym.o: /usr/include/machine/machparam.h /usr/include/sys/dir.h
sym.o: /usr/include/sys/user.h /usr/include/machine/pcb.h
sym.o: /usr/include/sys/dmap.h /usr/include/sys/time.h /usr/include/time.h
sym.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
sym.o: /usr/include/sys/uio.h /usr/include/errno.h /usr/include/ctype.h
sym.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/sys/ptrace.h
sym.o: mac.h mode.h machine.h /usr/include/sys/vm.h /usr/include/sys/vmparam.h
sym.o: /usr/include/machine/vmparam.h /usr/include/sys/vmmac.h
sym.o: /usr/include/sys/vmmeter.h /usr/include/sys/vmsystm.h head.h
sym.o: /usr/include/stab.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
