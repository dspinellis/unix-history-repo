IDENT=-DGENERIC -DVAX730 -DVAX750 -DVAX780 -DINET -DQUOTA
PARAM=-DTIMEZONE=480 -DDST=1 -DMAXUSERS=8
#	makefile.vax	6.3	9/25/83
#
# Makefile for 4.2 BSD
#
# This makefile is constructed from a machine description:
#	config machine
# Most changes should be made in the machine description
#	/sys/conf/``machineid''
# after which you should do
#	 config machineid
# Generic makefile changes should be made in
#	/sys/conf/makefile
# after which config should be rerun for all machines.
#
# N.B.: NO DEPENDENCIES ON FOLLOWING FLAGS ARE VISIBLE TO MAKEFILE
#	IF YOU CHANGE THE DEFINITION OF ANY OF THESE RECOMPILE EVERYTHING
#
# -DTRACE	compile in kernel tracing hooks
# -DQUOTA	compile in file system quotas
# -DUUDMA	compile in unibus tu58 pseudo-dma code
# -DSWABIPS	compile in code to byte swap ip packets on 3Mb/s Ethernet
#
C2=	/lib/c2
LD=	/bin/ld
TOUCH=	touch -f -c

COPTS=	${IDENT} -DKERNEL
CFLAGS=	-O ${COPTS}

AHEADS=	../vax/pcb.m

OBJS=af.o if.o raw_cb.o raw_usrreq.o route.o if_loop.o in.o in_pcb.o \
	in_proto.o ip_icmp.o ip_input.o ip_output.o raw_ip.o tcp_debug.o \
	tcp_input.o tcp_output.o tcp_subr.o tcp_timer.o tcp_usrreq.o \
	udp_usrreq.o init_main.o init_sysent.o kern_acct.o kern_clock.o \
	kern_descrip.o kern_exec.o kern_exit.o kern_fork.o kern_mman.o \
	kern_proc.o kern_prot.o kern_resource.o kern_sig.o kern_subr.o \
	kern_synch.o kern_time.o kern_xxx.o quota_kern.o quota_subr.o \
	quota_sys.o quota_ufs.o subr_prf.o subr_rmap.o subr_xxx.o \
	sys_generic.o sys_inode.o sys_process.o sys_socket.o tty.o \
	tty_conf.o tty_pty.o tty_subr.o tty_tty.o ufs_alloc.o ufs_bio.o \
	ufs_bmap.o ufs_dsort.o ufs_fio.o ufs_inode.o ufs_mount.o \
	ufs_nami.o ufs_subr.o ufs_syscalls.o ufs_tables.o ufs_xxx.o \
	uipc_domain.o uipc_mbuf.o uipc_pipe.o uipc_proto.o uipc_socket.o \
	uipc_socket2.o uipc_syscalls.o uipc_usrreq.o vm_drum.o vm_mem.o \
	vm_mon.o vm_page.o vm_proc.o vm_pt.o vm_sched.o vm_subr.o \
	vm_sw.o vm_swap.o vm_swp.o vm_text.o autoconf.o clock.o conf.o \
	cons.o cpudata.o dkbad.o flp.o in_cksum.o machdep.o mem.o \
	sys_machdep.o trap.o tu.o udiv.o ufs_machdep.o urem.o \
	vm_machdep.o if_uba.o hp.o ht.o mba.o mt.o dh.o dmf.o dz.o idc.o \
	lp.o rk.o rl.o tm.o ts.o uba.o uda.o up.o ut.o 

CFILES=../net/af.c ../net/if.c ../net/raw_cb.c ../net/raw_usrreq.c \
	../net/route.c ../netinet/if_loop.c ../netinet/in.c \
	../netinet/in_pcb.c ../netinet/in_proto.c ../netinet/ip_icmp.c \
	../netinet/ip_input.c ../netinet/ip_output.c ../netinet/raw_ip.c \
	../netinet/tcp_debug.c ../netinet/tcp_input.c \
	../netinet/tcp_output.c ../netinet/tcp_subr.c \
	../netinet/tcp_timer.c ../netinet/tcp_usrreq.c \
	../netinet/udp_usrreq.c ../sys/init_main.c ../sys/init_sysent.c \
	../sys/kern_acct.c ../sys/kern_clock.c ../sys/kern_descrip.c \
	../sys/kern_exec.c ../sys/kern_exit.c ../sys/kern_fork.c \
	../sys/kern_mman.c ../sys/kern_proc.c ../sys/kern_prot.c \
	../sys/kern_resource.c ../sys/kern_sig.c ../sys/kern_subr.c \
	../sys/kern_synch.c ../sys/kern_time.c ../sys/kern_xxx.c \
	../sys/quota_kern.c ../sys/quota_subr.c ../sys/quota_sys.c \
	../sys/quota_ufs.c ../sys/subr_prf.c ../sys/subr_rmap.c \
	../sys/subr_xxx.c ../sys/sys_generic.c ../sys/sys_inode.c \
	../sys/sys_process.c ../sys/sys_socket.c ../sys/tty.c \
	../sys/tty_conf.c ../sys/tty_pty.c ../sys/tty_subr.c \
	../sys/tty_tty.c ../sys/ufs_alloc.c ../sys/ufs_bio.c \
	../sys/ufs_bmap.c ../sys/ufs_dsort.c ../sys/ufs_fio.c \
	../sys/ufs_inode.c ../sys/ufs_mount.c ../sys/ufs_nami.c \
	../sys/ufs_subr.c ../sys/ufs_syscalls.c ../sys/ufs_tables.c \
	../sys/ufs_xxx.c ../sys/uipc_domain.c ../sys/uipc_mbuf.c \
	../sys/uipc_pipe.c ../sys/uipc_proto.c ../sys/uipc_socket.c \
	../sys/uipc_socket2.c ../sys/uipc_syscalls.c \
	../sys/uipc_usrreq.c ../sys/vm_drum.c ../sys/vm_mem.c \
	../sys/vm_mon.c ../sys/vm_page.c ../sys/vm_proc.c ../sys/vm_pt.c \
	../sys/vm_sched.c ../sys/vm_subr.c ../sys/vm_sw.c \
	../sys/vm_swap.c ../sys/vm_swp.c ../sys/vm_text.c \
	../vax/autoconf.c ../vax/clock.c ../vax/conf.c ../vax/cons.c \
	../vax/cpudata.c ../vax/dkbad.c ../vax/flp.c ../vax/in_cksum.c \
	../vax/machdep.c ../vax/mem.c ../vax/sys_machdep.c ../vax/trap.c \
	../vax/tu.c ../vax/ufs_machdep.c ../vax/vm_machdep.c \
	../vaxif/if_uba.c ../vaxmba/hp.c ../vaxmba/ht.c ../vaxmba/mba.c \
	../vaxmba/mt.c ../vaxuba/dh.c ../vaxuba/dmf.c ../vaxuba/dz.c \
	../vaxuba/idc.c ../vaxuba/lp.c ../vaxuba/rk.c ../vaxuba/rl.c \
	../vaxuba/tm.c ../vaxuba/ts.c ../vaxuba/uba.c ../vaxuba/uda.c \
	../vaxuba/up.c ../vaxuba/ut.c 

vmunix: makefile locore.o ${OBJS} param.o ioconf.o swapgeneric.o
	@echo loading vmunix
	@rm -f vmunix
	@sh ../conf/newvers.sh
	@${CC} $(CFLAGS) -c vers.c
	@${LD} -n -o vmunix -e start -x -T 80000000 locore.o ${OBJS} vers.o ioconf.o param.o swapgeneric.o
	@echo rearranging symbols
	@-symorder ../vax/symbols.sort vmunix
	@size vmunix
	@chmod 755 vmunix

swapgeneric.o: ../vax/swapgeneric.c
	${CC} -I. -c -S ${COPTS} ../vax/swapgeneric.c
	${C2} swapgeneric.s | sed -f ../vax/asm.sed | ${AS} -o swapgeneric.o
	rm -f swapgeneric.s

hkvmunix: makefile locore.o ${OBJS} param.o ioconf.o swaphkvmunix.o
	@echo loading hkvmunix
	@rm -f hkvmunix
	@${LD} -n -o hkvmunix -e start -x -T 80000000 locore.o ${OBJS} vers.o ioconf.o param.o swaphkvmunix.o
	@echo rearranging symbols
	@-symorder ../vax/symbols.sort hkvmunix
	@size hkvmunix
	@chmod 755 hkvmunix

swaphkvmunix.o: swaphkvmunix.c
	${CC} -I. -c -O ${COPTS} swaphkvmunix.c

hpvmunix: makefile locore.o ${OBJS} param.o ioconf.o swaphpvmunix.o
	@echo loading hpvmunix
	@rm -f hpvmunix
	@${LD} -n -o hpvmunix -e start -x -T 80000000 locore.o ${OBJS} vers.o ioconf.o param.o swaphpvmunix.o
	@echo rearranging symbols
	@-symorder ../vax/symbols.sort hpvmunix
	@size hpvmunix
	@chmod 755 hpvmunix

swaphpvmunix.o: swaphpvmunix.c
	${CC} -I. -c -O ${COPTS} swaphpvmunix.c

all: vmunix hkvmunix hpvmunix

clean:
	rm -f eddep *vmunix tags *.o locore.i [a-tv-z]*.s \
		errs linterrs makelinks

lint: /tmp
	@lint -hbxn -I. -DGENERIC ${COPTS} ../vax/Locore.c \
	  ${CFILES} ../vax/swapgeneric.c ioconf.c | \
	    grep -v 'struct/union .* never defined' | \
	    grep -v 'possible pointer alignment problem'

../vax/symbols.sort: ../vax/symbols.raw
	grep -v '^#' ../vax/symbols.raw \
	    | sed 's/^	//' | sort -u > ../vax/symbols.sort

locore.o: assym.s ${AHEADS} ../vax/rpb.s ../vax/scb.s ../vax/locore.s \
    ubglue.s ../vax/mtpr.h ../vax/trap.h ../machine/psl.h \
    ../machine/pte.h ../vax/cpu.h mba.h
	cat assym.s ../vax/rpb.s ../vax/scb.s ../vax/locore.s \
	    ubglue.s > locore.c
	cc -E -I. -DLOCORE ${COPTS} locore.c > locore.i
	@echo 'as -o locore.o $${AHEADS} locore.i'
	@as -o locore.o ${AHEADS} locore.i
	@rm locore.i

# the following is necessary because autoconf.o depends on #if GENERIC
autoconf.o tu.o: makefile

# the following are necessary because the files depend on the types of
# vax cpu's included in the system configuration
clock.o machdep.o autoconf.o conf.o cons.o flp.o mba.o uba.o vaxcpu.o : makefile
# depend on network configuration
af.o : makefile
# depend on maxusers
assym.s: makefile

assym.s: ../h/param.h ../machine/pte.h ../h/buf.h ../h/vmparam.h \
    ../h/vmmeter.h ../h/dir.h ../h/cmap.h ../h/map.h ../vaxuba/ubavar.h \
    ../h/proc.h ../h/msgbuf.h
	cc ${IDENT} ${PARAM} ../vax/genassym.c; ./a.out >assym.s; rm -f a.out

../h/param.h: /usr/include/signal.h
	${TOUCH} ../h/param.h
../h/tty.h:  ../h/ttychars.h ../h/ttydev.h
	${TOUCH} ../h/tty.h
../h/ioctl.h: /usr/include/sgtty.h ../h/ttychars.h ../h/ttydev.h
	${TOUCH} ../h/ioctl.h
../h/user.h: ../vax/pcb.h ../h/dmap.h ../h/types.h \
    /usr/include/errno.h
	${TOUCH} ../h/user.h
../h/vm.h: ../h/vmmac.h ../h/vmmeter.h ../h/vmparam.h ../h/vmsystm.h
	${TOUCH} ../h/vm.h

depend:
	grep '^#include' ${CFILES} | grep -v '<' | \
	sed -e 's/:[^"]*"\([^"]*\)".*/: \1/' \
	    -e 's/\.c/.o/' \
	    -e 's,../[a-zA-Z]*/,,' | \
	awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$2 } } \
	      END { print rec } ' > makedep
	echo '$$r makedep' >>eddep
	echo '/^# DO NOT DELETE THIS LINE/+1,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp makefile makefile.bak
	ed - makefile < eddep
	rm eddep makedep


links:
	egrep '#if' ${CFILES} | sed -f ../conf/defines | \
	  sed -e 's/:.*//' -e 's/\.c/.o/' | sort -u > dontlink
	echo ${CFILES} | tr -s ' ' '\12' | sed 's/\.c/.o/' | \
	  sort -u | comm -23 - dontlink | \
	  sed 's,../.*/\(.*.o\),rm -f \1;ln -s ../GENERIC/\1 \1,' > makelinks
	echo 'rm -f udiv.o;ln -s ../GENERIC/udiv.o udiv.o' >> makelinks
	echo 'rm -f urem.o;ln -s ../GENERIC/urem.o urem.o' >> makelinks
	sh makelinks && rm -f dontlink

tags:
	/usr/ucb/ctags ${CFILES}

print:
	@pr -f makefile ../sys/TODO linterrs
	@/usr/ucb/ctags -x ${CFILES} | pr -f -h XREF
	@(size vmunix *.o) | pr -f -h sizes
	@ls -ls | pr -f
	@cd ../h; ls -ls | pr -f ; pr -f *.m *.h
	@echo 'once upon a time ...'
	@cd ../sys; pr -f asm.sed Locore.c scb.s locore.s
	@pr -f ${CFILES}

ioconf.o: ioconf.c ../h/param.h ../machine/pte.h ../h/buf.h ../h/map.h \
    ../vaxmba/mbavar.h ../h/vm.h ../vaxuba/ubavar.h
	${CC} -I. -c -S ${COPTS} ioconf.c
	${C2} ioconf.s | sed -f ../vax/asm.sed | ${AS} -o ioconf.o
	rm -f ioconf.s

param.c: ../conf/param.c
	cp ../conf/param.c .

param.o: param.c makefile
	${CC} -I. -c ${CFLAGS} ${PARAM} param.c

af.o: ../net/af.c
	${CC} -I. -c -S ${COPTS} ../net/af.c
	${C2} af.s | sed -f ../vax/asm.sed | ${AS} -o af.o
	rm -f af.s

if.o: ../net/if.c
	${CC} -I. -c -S ${COPTS} ../net/if.c
	${C2} if.s | sed -f ../vax/asm.sed | ${AS} -o if.o
	rm -f if.s

raw_cb.o: ../net/raw_cb.c
	${CC} -I. -c -S ${COPTS} ../net/raw_cb.c
	${C2} raw_cb.s | sed -f ../vax/asm.sed | ${AS} -o raw_cb.o
	rm -f raw_cb.s

raw_usrreq.o: ../net/raw_usrreq.c
	${CC} -I. -c -S ${COPTS} ../net/raw_usrreq.c
	${C2} raw_usrreq.s | sed -f ../vax/asm.sed | ${AS} -o raw_usrreq.o
	rm -f raw_usrreq.s

route.o: ../net/route.c
	${CC} -I. -c -S ${COPTS} ../net/route.c
	${C2} route.s | sed -f ../vax/asm.sed | ${AS} -o route.o
	rm -f route.s

if_loop.o: ../netinet/if_loop.c
	${CC} -I. -c -S ${COPTS} ../netinet/if_loop.c
	${C2} if_loop.s | sed -f ../vax/asm.sed | ${AS} -o if_loop.o
	rm -f if_loop.s

in.o: ../netinet/in.c
	${CC} -I. -c -S ${COPTS} ../netinet/in.c
	${C2} in.s | sed -f ../vax/asm.sed | ${AS} -o in.o
	rm -f in.s

in_pcb.o: ../netinet/in_pcb.c
	${CC} -I. -c -S ${COPTS} ../netinet/in_pcb.c
	${C2} in_pcb.s | sed -f ../vax/asm.sed | ${AS} -o in_pcb.o
	rm -f in_pcb.s

in_proto.o: ../netinet/in_proto.c
	${CC} -I. -c -S ${COPTS} ../netinet/in_proto.c
	${C2} in_proto.s | sed -f ../vax/asm.sed | ${AS} -o in_proto.o
	rm -f in_proto.s

ip_icmp.o: ../netinet/ip_icmp.c
	${CC} -I. -c -S ${COPTS} ../netinet/ip_icmp.c
	${C2} ip_icmp.s | sed -f ../vax/asm.sed | ${AS} -o ip_icmp.o
	rm -f ip_icmp.s

ip_input.o: ../netinet/ip_input.c
	${CC} -I. -c -S ${COPTS} ../netinet/ip_input.c
	${C2} ip_input.s | sed -f ../vax/asm.sed | ${AS} -o ip_input.o
	rm -f ip_input.s

ip_output.o: ../netinet/ip_output.c
	${CC} -I. -c -S ${COPTS} ../netinet/ip_output.c
	${C2} ip_output.s | sed -f ../vax/asm.sed | ${AS} -o ip_output.o
	rm -f ip_output.s

raw_ip.o: ../netinet/raw_ip.c
	${CC} -I. -c -S ${COPTS} ../netinet/raw_ip.c
	${C2} raw_ip.s | sed -f ../vax/asm.sed | ${AS} -o raw_ip.o
	rm -f raw_ip.s

tcp_debug.o: ../netinet/tcp_debug.c
	${CC} -I. -c -S ${COPTS} ../netinet/tcp_debug.c
	${C2} tcp_debug.s | sed -f ../vax/asm.sed | ${AS} -o tcp_debug.o
	rm -f tcp_debug.s

tcp_input.o: ../netinet/tcp_input.c
	${CC} -I. -c -S ${COPTS} ../netinet/tcp_input.c
	${C2} tcp_input.s | sed -f ../vax/asm.sed | ${AS} -o tcp_input.o
	rm -f tcp_input.s

tcp_output.o: ../netinet/tcp_output.c
	${CC} -I. -c -S ${COPTS} ../netinet/tcp_output.c
	${C2} tcp_output.s | sed -f ../vax/asm.sed | ${AS} -o tcp_output.o
	rm -f tcp_output.s

tcp_subr.o: ../netinet/tcp_subr.c
	${CC} -I. -c -S ${COPTS} ../netinet/tcp_subr.c
	${C2} tcp_subr.s | sed -f ../vax/asm.sed | ${AS} -o tcp_subr.o
	rm -f tcp_subr.s

tcp_timer.o: ../netinet/tcp_timer.c
	${CC} -I. -c -S ${COPTS} ../netinet/tcp_timer.c
	${C2} tcp_timer.s | sed -f ../vax/asm.sed | ${AS} -o tcp_timer.o
	rm -f tcp_timer.s

tcp_usrreq.o: ../netinet/tcp_usrreq.c
	${CC} -I. -c -S ${COPTS} ../netinet/tcp_usrreq.c
	${C2} tcp_usrreq.s | sed -f ../vax/asm.sed | ${AS} -o tcp_usrreq.o
	rm -f tcp_usrreq.s

udp_usrreq.o: ../netinet/udp_usrreq.c
	${CC} -I. -c -S ${COPTS} ../netinet/udp_usrreq.c
	${C2} udp_usrreq.s | sed -f ../vax/asm.sed | ${AS} -o udp_usrreq.o
	rm -f udp_usrreq.s

init_main.o: ../sys/init_main.c
	${CC} -I. -c -S ${COPTS} ../sys/init_main.c
	${C2} init_main.s | sed -f ../vax/asm.sed | ${AS} -o init_main.o
	rm -f init_main.s

init_sysent.o: ../sys/init_sysent.c
	${CC} -I. -c -S ${COPTS} ../sys/init_sysent.c
	${C2} init_sysent.s | sed -f ../vax/asm.sed | ${AS} -o init_sysent.o
	rm -f init_sysent.s

kern_acct.o: ../sys/kern_acct.c
	${CC} -I. -c -S ${COPTS} ../sys/kern_acct.c
	${C2} kern_acct.s | sed -f ../vax/asm.sed | ${AS} -o kern_acct.o
	rm -f kern_acct.s

kern_clock.o: ../sys/kern_clock.c
	${CC} -I. -c -S ${COPTS} ../sys/kern_clock.c
	${C2} kern_clock.s | sed -f ../vax/asm.sed | ${AS} -o kern_clock.o
	rm -f kern_clock.s

kern_descrip.o: ../sys/kern_descrip.c
	${CC} -I. -c -S ${COPTS} ../sys/kern_descrip.c
	${C2} kern_descrip.s | sed -f ../vax/asm.sed | ${AS} -o kern_descrip.o
	rm -f kern_descrip.s

kern_exec.o: ../sys/kern_exec.c
	${CC} -I. -c -S ${COPTS} ../sys/kern_exec.c
	${C2} kern_exec.s | sed -f ../vax/asm.sed | ${AS} -o kern_exec.o
	rm -f kern_exec.s

kern_exit.o: ../sys/kern_exit.c
	${CC} -I. -c -S ${COPTS} ../sys/kern_exit.c
	${C2} kern_exit.s | sed -f ../vax/asm.sed | ${AS} -o kern_exit.o
	rm -f kern_exit.s

kern_fork.o: ../sys/kern_fork.c
	${CC} -I. -c -S ${COPTS} ../sys/kern_fork.c
	${C2} kern_fork.s | sed -f ../vax/asm.sed | ${AS} -o kern_fork.o
	rm -f kern_fork.s

kern_mman.o: ../sys/kern_mman.c
	${CC} -I. -c -S ${COPTS} ../sys/kern_mman.c
	${C2} kern_mman.s | sed -f ../vax/asm.sed | ${AS} -o kern_mman.o
	rm -f kern_mman.s

kern_proc.o: ../sys/kern_proc.c
	${CC} -I. -c -S ${COPTS} ../sys/kern_proc.c
	${C2} kern_proc.s | sed -f ../vax/asm.sed | ${AS} -o kern_proc.o
	rm -f kern_proc.s

kern_prot.o: ../sys/kern_prot.c
	${CC} -I. -c -S ${COPTS} ../sys/kern_prot.c
	${C2} kern_prot.s | sed -f ../vax/asm.sed | ${AS} -o kern_prot.o
	rm -f kern_prot.s

kern_resource.o: ../sys/kern_resource.c
	${CC} -I. -c -S ${COPTS} ../sys/kern_resource.c
	${C2} kern_resource.s | sed -f ../vax/asm.sed | ${AS} -o kern_resource.o
	rm -f kern_resource.s

kern_sig.o: ../sys/kern_sig.c
	${CC} -I. -c -S ${COPTS} ../sys/kern_sig.c
	${C2} kern_sig.s | sed -f ../vax/asm.sed | ${AS} -o kern_sig.o
	rm -f kern_sig.s

kern_subr.o: ../sys/kern_subr.c
	${CC} -I. -c -S ${COPTS} ../sys/kern_subr.c
	${C2} kern_subr.s | sed -f ../vax/asm.sed | ${AS} -o kern_subr.o
	rm -f kern_subr.s

kern_synch.o: ../sys/kern_synch.c
	${CC} -I. -c -S ${COPTS} ../sys/kern_synch.c
	${C2} kern_synch.s | sed -f ../vax/asm.sed | ${AS} -o kern_synch.o
	rm -f kern_synch.s

kern_time.o: ../sys/kern_time.c
	${CC} -I. -c -S ${COPTS} ../sys/kern_time.c
	${C2} kern_time.s | sed -f ../vax/asm.sed | ${AS} -o kern_time.o
	rm -f kern_time.s

kern_xxx.o: ../sys/kern_xxx.c
	${CC} -I. -c -S ${COPTS} ../sys/kern_xxx.c
	${C2} kern_xxx.s | sed -f ../vax/asm.sed | ${AS} -o kern_xxx.o
	rm -f kern_xxx.s

quota_kern.o: ../sys/quota_kern.c
	${CC} -I. -c -S ${COPTS} ../sys/quota_kern.c
	${C2} quota_kern.s | sed -f ../vax/asm.sed | ${AS} -o quota_kern.o
	rm -f quota_kern.s

quota_subr.o: ../sys/quota_subr.c
	${CC} -I. -c -S ${COPTS} ../sys/quota_subr.c
	${C2} quota_subr.s | sed -f ../vax/asm.sed | ${AS} -o quota_subr.o
	rm -f quota_subr.s

quota_sys.o: ../sys/quota_sys.c
	${CC} -I. -c -S ${COPTS} ../sys/quota_sys.c
	${C2} quota_sys.s | sed -f ../vax/asm.sed | ${AS} -o quota_sys.o
	rm -f quota_sys.s

quota_ufs.o: ../sys/quota_ufs.c
	${CC} -I. -c -S ${COPTS} ../sys/quota_ufs.c
	${C2} quota_ufs.s | sed -f ../vax/asm.sed | ${AS} -o quota_ufs.o
	rm -f quota_ufs.s

subr_prf.o: ../sys/subr_prf.c
	${CC} -I. -c -S ${COPTS} ../sys/subr_prf.c
	${C2} subr_prf.s | sed -f ../vax/asm.sed | ${AS} -o subr_prf.o
	rm -f subr_prf.s

subr_rmap.o: ../sys/subr_rmap.c
	${CC} -I. -c -S ${COPTS} ../sys/subr_rmap.c
	${C2} subr_rmap.s | sed -f ../vax/asm.sed | ${AS} -o subr_rmap.o
	rm -f subr_rmap.s

subr_xxx.o: ../sys/subr_xxx.c
	${CC} -I. -c -S ${COPTS} ../sys/subr_xxx.c
	${C2} subr_xxx.s | sed -f ../vax/asm.sed | ${AS} -o subr_xxx.o
	rm -f subr_xxx.s

sys_generic.o: ../sys/sys_generic.c
	${CC} -I. -c -S ${COPTS} ../sys/sys_generic.c
	${C2} sys_generic.s | sed -f ../vax/asm.sed | ${AS} -o sys_generic.o
	rm -f sys_generic.s

sys_inode.o: ../sys/sys_inode.c
	${CC} -I. -c -S ${COPTS} ../sys/sys_inode.c
	${C2} sys_inode.s | sed -f ../vax/asm.sed | ${AS} -o sys_inode.o
	rm -f sys_inode.s

sys_process.o: ../sys/sys_process.c
	${CC} -I. -c -S ${COPTS} ../sys/sys_process.c
	${C2} sys_process.s | sed -f ../vax/asm.sed | ${AS} -o sys_process.o
	rm -f sys_process.s

sys_socket.o: ../sys/sys_socket.c
	${CC} -I. -c -S ${COPTS} ../sys/sys_socket.c
	${C2} sys_socket.s | sed -f ../vax/asm.sed | ${AS} -o sys_socket.o
	rm -f sys_socket.s

tty.o: ../sys/tty.c
	${CC} -I. -c -S ${COPTS} ../sys/tty.c
	${C2} tty.s | sed -f ../vax/asm.sed | ${AS} -o tty.o
	rm -f tty.s

tty_conf.o: ../sys/tty_conf.c
	${CC} -I. -c -S ${COPTS} ../sys/tty_conf.c
	${C2} tty_conf.s | sed -f ../vax/asm.sed | ${AS} -o tty_conf.o
	rm -f tty_conf.s

tty_pty.o: ../sys/tty_pty.c
	${CC} -I. -c -S ${COPTS} ../sys/tty_pty.c
	${C2} tty_pty.s | sed -f ../vax/asm.sed | ${AS} -o tty_pty.o
	rm -f tty_pty.s

tty_subr.o: ../sys/tty_subr.c
	${CC} -I. -c -S ${COPTS} ../sys/tty_subr.c
	${C2} tty_subr.s | sed -f ../vax/asm.sed | ${AS} -o tty_subr.o
	rm -f tty_subr.s

tty_tty.o: ../sys/tty_tty.c
	${CC} -I. -c -S ${COPTS} ../sys/tty_tty.c
	${C2} tty_tty.s | sed -f ../vax/asm.sed | ${AS} -o tty_tty.o
	rm -f tty_tty.s

ufs_alloc.o: ../sys/ufs_alloc.c
	${CC} -I. -c -S ${COPTS} ../sys/ufs_alloc.c
	${C2} ufs_alloc.s | sed -f ../vax/asm.sed | ${AS} -o ufs_alloc.o
	rm -f ufs_alloc.s

ufs_bio.o: ../sys/ufs_bio.c
	${CC} -I. -c -S ${COPTS} ../sys/ufs_bio.c
	${C2} ufs_bio.s | sed -f ../vax/asm.sed | ${AS} -o ufs_bio.o
	rm -f ufs_bio.s

ufs_bmap.o: ../sys/ufs_bmap.c
	${CC} -I. -c -S ${COPTS} ../sys/ufs_bmap.c
	${C2} ufs_bmap.s | sed -f ../vax/asm.sed | ${AS} -o ufs_bmap.o
	rm -f ufs_bmap.s

ufs_dsort.o: ../sys/ufs_dsort.c
	${CC} -I. -c -S ${COPTS} ../sys/ufs_dsort.c
	${C2} ufs_dsort.s | sed -f ../vax/asm.sed | ${AS} -o ufs_dsort.o
	rm -f ufs_dsort.s

ufs_fio.o: ../sys/ufs_fio.c
	${CC} -I. -c -S ${COPTS} ../sys/ufs_fio.c
	${C2} ufs_fio.s | sed -f ../vax/asm.sed | ${AS} -o ufs_fio.o
	rm -f ufs_fio.s

ufs_inode.o: ../sys/ufs_inode.c
	${CC} -I. -c -S ${COPTS} ../sys/ufs_inode.c
	${C2} ufs_inode.s | sed -f ../vax/asm.sed | ${AS} -o ufs_inode.o
	rm -f ufs_inode.s

ufs_mount.o: ../sys/ufs_mount.c
	${CC} -I. -c -S ${COPTS} ../sys/ufs_mount.c
	${C2} ufs_mount.s | sed -f ../vax/asm.sed | ${AS} -o ufs_mount.o
	rm -f ufs_mount.s

ufs_nami.o: ../sys/ufs_nami.c
	${CC} -I. -c -S ${COPTS} ../sys/ufs_nami.c
	${C2} ufs_nami.s | sed -f ../vax/asm.sed | ${AS} -o ufs_nami.o
	rm -f ufs_nami.s

ufs_subr.o: ../sys/ufs_subr.c
	${CC} -I. -c -S ${COPTS} ../sys/ufs_subr.c
	${C2} ufs_subr.s | sed -f ../vax/asm.sed | ${AS} -o ufs_subr.o
	rm -f ufs_subr.s

ufs_syscalls.o: ../sys/ufs_syscalls.c
	${CC} -I. -c -S ${COPTS} ../sys/ufs_syscalls.c
	${C2} ufs_syscalls.s | sed -f ../vax/asm.sed | ${AS} -o ufs_syscalls.o
	rm -f ufs_syscalls.s

ufs_tables.o: ../sys/ufs_tables.c
	${CC} -I. -c -S ${COPTS} ../sys/ufs_tables.c
	${C2} ufs_tables.s | sed -f ../vax/asm.sed | ${AS} -o ufs_tables.o
	rm -f ufs_tables.s

ufs_xxx.o: ../sys/ufs_xxx.c
	${CC} -I. -c -S ${COPTS} ../sys/ufs_xxx.c
	${C2} ufs_xxx.s | sed -f ../vax/asm.sed | ${AS} -o ufs_xxx.o
	rm -f ufs_xxx.s

uipc_domain.o: ../sys/uipc_domain.c
	${CC} -I. -c -S ${COPTS} ../sys/uipc_domain.c
	${C2} uipc_domain.s | sed -f ../vax/asm.sed | ${AS} -o uipc_domain.o
	rm -f uipc_domain.s

uipc_mbuf.o: ../sys/uipc_mbuf.c
	${CC} -I. -c -S ${COPTS} ../sys/uipc_mbuf.c
	${C2} uipc_mbuf.s | sed -f ../vax/asm.sed | ${AS} -o uipc_mbuf.o
	rm -f uipc_mbuf.s

uipc_pipe.o: ../sys/uipc_pipe.c
	${CC} -I. -c -S ${COPTS} ../sys/uipc_pipe.c
	${C2} uipc_pipe.s | sed -f ../vax/asm.sed | ${AS} -o uipc_pipe.o
	rm -f uipc_pipe.s

uipc_proto.o: ../sys/uipc_proto.c
	${CC} -I. -c -S ${COPTS} ../sys/uipc_proto.c
	${C2} uipc_proto.s | sed -f ../vax/asm.sed | ${AS} -o uipc_proto.o
	rm -f uipc_proto.s

uipc_socket.o: ../sys/uipc_socket.c
	${CC} -I. -c -S ${COPTS} ../sys/uipc_socket.c
	${C2} uipc_socket.s | sed -f ../vax/asm.sed | ${AS} -o uipc_socket.o
	rm -f uipc_socket.s

uipc_socket2.o: ../sys/uipc_socket2.c
	${CC} -I. -c -S ${COPTS} ../sys/uipc_socket2.c
	${C2} uipc_socket2.s | sed -f ../vax/asm.sed | ${AS} -o uipc_socket2.o
	rm -f uipc_socket2.s

uipc_syscalls.o: ../sys/uipc_syscalls.c
	${CC} -I. -c -S ${COPTS} ../sys/uipc_syscalls.c
	${C2} uipc_syscalls.s | sed -f ../vax/asm.sed | ${AS} -o uipc_syscalls.o
	rm -f uipc_syscalls.s

uipc_usrreq.o: ../sys/uipc_usrreq.c
	${CC} -I. -c -S ${COPTS} ../sys/uipc_usrreq.c
	${C2} uipc_usrreq.s | sed -f ../vax/asm.sed | ${AS} -o uipc_usrreq.o
	rm -f uipc_usrreq.s

vm_drum.o: ../sys/vm_drum.c
	${CC} -I. -c -S ${COPTS} ../sys/vm_drum.c
	${C2} vm_drum.s | sed -f ../vax/asm.sed | ${AS} -o vm_drum.o
	rm -f vm_drum.s

vm_mem.o: ../sys/vm_mem.c
	${CC} -I. -c -S ${COPTS} ../sys/vm_mem.c
	${C2} vm_mem.s | sed -f ../vax/asm.sed | ${AS} -o vm_mem.o
	rm -f vm_mem.s

vm_mon.o: ../sys/vm_mon.c
	${CC} -I. -c -S ${COPTS} ../sys/vm_mon.c
	${C2} vm_mon.s | sed -f ../vax/asm.sed | ${AS} -o vm_mon.o
	rm -f vm_mon.s

vm_page.o: ../sys/vm_page.c
	${CC} -I. -c -S ${COPTS} ../sys/vm_page.c
	${C2} vm_page.s | sed -f ../vax/asm.sed | ${AS} -o vm_page.o
	rm -f vm_page.s

vm_proc.o: ../sys/vm_proc.c
	${CC} -I. -c -S ${COPTS} ../sys/vm_proc.c
	${C2} vm_proc.s | sed -f ../vax/asm.sed | ${AS} -o vm_proc.o
	rm -f vm_proc.s

vm_pt.o: ../sys/vm_pt.c
	${CC} -I. -c -S ${COPTS} ../sys/vm_pt.c
	${C2} vm_pt.s | sed -f ../vax/asm.sed | ${AS} -o vm_pt.o
	rm -f vm_pt.s

vm_sched.o: ../sys/vm_sched.c
	${CC} -I. -c -S ${COPTS} ../sys/vm_sched.c
	${C2} vm_sched.s | sed -f ../vax/asm.sed | ${AS} -o vm_sched.o
	rm -f vm_sched.s

vm_subr.o: ../sys/vm_subr.c
	${CC} -I. -c -S ${COPTS} ../sys/vm_subr.c
	${C2} vm_subr.s | sed -f ../vax/asm.sed | ${AS} -o vm_subr.o
	rm -f vm_subr.s

vm_sw.o: ../sys/vm_sw.c
	${CC} -I. -c -S ${COPTS} ../sys/vm_sw.c
	${C2} vm_sw.s | sed -f ../vax/asm.sed | ${AS} -o vm_sw.o
	rm -f vm_sw.s

vm_swap.o: ../sys/vm_swap.c
	${CC} -I. -c -S ${COPTS} ../sys/vm_swap.c
	${C2} vm_swap.s | sed -f ../vax/asm.sed | ${AS} -o vm_swap.o
	rm -f vm_swap.s

vm_swp.o: ../sys/vm_swp.c
	${CC} -I. -c -S ${COPTS} ../sys/vm_swp.c
	${C2} vm_swp.s | sed -f ../vax/asm.sed | ${AS} -o vm_swp.o
	rm -f vm_swp.s

vm_text.o: ../sys/vm_text.c
	${CC} -I. -c -S ${COPTS} ../sys/vm_text.c
	${C2} vm_text.s | sed -f ../vax/asm.sed | ${AS} -o vm_text.o
	rm -f vm_text.s

autoconf.o: ../vax/autoconf.c
	${CC} -I. -c -S ${COPTS} ../vax/autoconf.c
	${C2} -i autoconf.s | sed -f ../vax/asm.sed | ${AS} -o autoconf.o
	rm -f autoconf.s

clock.o: ../vax/clock.c
	${CC} -I. -c -S ${COPTS} ../vax/clock.c
	${C2} clock.s | sed -f ../vax/asm.sed | ${AS} -o clock.o
	rm -f clock.s

conf.o: ../vax/conf.c
	${CC} -I. -c -S ${COPTS} ../vax/conf.c
	${C2} conf.s | sed -f ../vax/asm.sed | ${AS} -o conf.o
	rm -f conf.s

cons.o: ../vax/cons.c
	${CC} -I. -c -S ${COPTS} ../vax/cons.c
	${C2} cons.s | sed -f ../vax/asm.sed | ${AS} -o cons.o
	rm -f cons.s

cpudata.o: ../vax/cpudata.c
	${CC} -I. -c -S ${COPTS} ../vax/cpudata.c
	${C2} cpudata.s | sed -f ../vax/asm.sed | ${AS} -o cpudata.o
	rm -f cpudata.s

dkbad.o: ../vax/dkbad.c
	${CC} -I. -c -S ${COPTS} ../vax/dkbad.c
	${C2} dkbad.s | sed -f ../vax/asm.sed | ${AS} -o dkbad.o
	rm -f dkbad.s

flp.o: ../vax/flp.c
	${CC} -I. -c -S ${COPTS} ../vax/flp.c
	${C2} flp.s | sed -f ../vax/asm.sed | ${AS} -o flp.o
	rm -f flp.s

in_cksum.o: ../vax/in_cksum.c
	${CC} -I. -c -S ${COPTS} ../vax/in_cksum.c
	${C2} in_cksum.s | sed -f ../vax/asm.sed | ${AS} -o in_cksum.o
	rm -f in_cksum.s

machdep.o: ../vax/machdep.c
	${CC} -I. -c -S ${COPTS} ${PARAM} ../vax/machdep.c
	${C2} machdep.s | sed -f ../vax/asm.sed | ${AS} -o machdep.o
	rm -f machdep.s

mem.o: ../vax/mem.c
	${CC} -I. -c -S ${COPTS} ../vax/mem.c
	${C2} mem.s | sed -f ../vax/asm.sed | ${AS} -o mem.o
	rm -f mem.s

sys_machdep.o: ../vax/sys_machdep.c
	${CC} -I. -c -S ${COPTS} ../vax/sys_machdep.c
	${C2} sys_machdep.s | sed -f ../vax/asm.sed | ${AS} -o sys_machdep.o
	rm -f sys_machdep.s

trap.o: ../vax/trap.c
	${CC} -I. -c -S ${COPTS} ../vax/trap.c
	${C2} trap.s | sed -f ../vax/asm.sed | ${AS} -o trap.o
	rm -f trap.s

tu.o: ../vax/tu.c
	${CC} -I. -c -S ${COPTS} ../vax/tu.c
	${C2} tu.s | sed -f ../vax/asm.sed | ${AS} -o tu.o
	rm -f tu.s

udiv.o: ../vax/udiv.s
	${AS} -o udiv.o ../vax/udiv.s

ufs_machdep.o: ../vax/ufs_machdep.c
	${CC} -I. -c -S ${COPTS} ../vax/ufs_machdep.c
	${C2} ufs_machdep.s | sed -f ../vax/asm.sed | ${AS} -o ufs_machdep.o
	rm -f ufs_machdep.s

urem.o: ../vax/urem.s
	${AS} -o urem.o ../vax/urem.s

vm_machdep.o: ../vax/vm_machdep.c
	${CC} -I. -c -S ${COPTS} ../vax/vm_machdep.c
	${C2} vm_machdep.s | sed -f ../vax/asm.sed | ${AS} -o vm_machdep.o
	rm -f vm_machdep.s

if_uba.o: ../vaxif/if_uba.c
	${CC} -I. -c -S ${COPTS} ../vaxif/if_uba.c
	${C2} -i if_uba.s | sed -f ../vax/asm.sed | ${AS} -o if_uba.o
	rm -f if_uba.s

hp.o: ../vaxmba/hp.c
	${CC} -I. -c -S ${COPTS} ../vaxmba/hp.c
	${C2} -i hp.s | sed -f ../vax/asm.sed | ${AS} -o hp.o
	rm -f hp.s

ht.o: ../vaxmba/ht.c
	${CC} -I. -c -S ${COPTS} ../vaxmba/ht.c
	${C2} -i ht.s | sed -f ../vax/asm.sed | ${AS} -o ht.o
	rm -f ht.s

mba.o: ../vaxmba/mba.c
	${CC} -I. -c -S ${COPTS} ../vaxmba/mba.c
	${C2} -i mba.s | sed -f ../vax/asm.sed | ${AS} -o mba.o
	rm -f mba.s

mt.o: ../vaxmba/mt.c
	${CC} -I. -c -S ${COPTS} ../vaxmba/mt.c
	${C2} -i mt.s | sed -f ../vax/asm.sed | ${AS} -o mt.o
	rm -f mt.s

dh.o: ../vaxuba/dh.c
	${CC} -I. -c -S ${COPTS} ../vaxuba/dh.c
	${C2} -i dh.s | sed -f ../vax/asm.sed | ${AS} -o dh.o
	rm -f dh.s

dmf.o: ../vaxuba/dmf.c
	${CC} -I. -c -S ${COPTS} ../vaxuba/dmf.c
	${C2} -i dmf.s | sed -f ../vax/asm.sed | ${AS} -o dmf.o
	rm -f dmf.s

dz.o: ../vaxuba/dz.c
	${CC} -I. -c -S ${COPTS} ../vaxuba/dz.c
	${C2} -i dz.s | sed -f ../vax/asm.sed | ${AS} -o dz.o
	rm -f dz.s

idc.o: ../vaxuba/idc.c
	${CC} -I. -c -S ${COPTS} ../vaxuba/idc.c
	${C2} -i idc.s | sed -f ../vax/asm.sed | ${AS} -o idc.o
	rm -f idc.s

lp.o: ../vaxuba/lp.c
	${CC} -I. -c -S ${COPTS} ../vaxuba/lp.c
	${C2} -i lp.s | sed -f ../vax/asm.sed | ${AS} -o lp.o
	rm -f lp.s

rk.o: ../vaxuba/rk.c
	${CC} -I. -c -S ${COPTS} ../vaxuba/rk.c
	${C2} -i rk.s | sed -f ../vax/asm.sed | ${AS} -o rk.o
	rm -f rk.s

rl.o: ../vaxuba/rl.c
	${CC} -I. -c -S ${COPTS} ../vaxuba/rl.c
	${C2} -i rl.s | sed -f ../vax/asm.sed | ${AS} -o rl.o
	rm -f rl.s

tm.o: ../vaxuba/tm.c
	${CC} -I. -c -S ${COPTS} ../vaxuba/tm.c
	${C2} -i tm.s | sed -f ../vax/asm.sed | ${AS} -o tm.o
	rm -f tm.s

ts.o: ../vaxuba/ts.c
	${CC} -I. -c -S ${COPTS} ../vaxuba/ts.c
	${C2} -i ts.s | sed -f ../vax/asm.sed | ${AS} -o ts.o
	rm -f ts.s

uba.o: ../vaxuba/uba.c
	${CC} -I. -c -S ${COPTS} ../vaxuba/uba.c
	${C2} -i uba.s | sed -f ../vax/asm.sed | ${AS} -o uba.o
	rm -f uba.s

uda.o: ../vaxuba/uda.c
	${CC} -I. -c -S ${COPTS} ../vaxuba/uda.c
	${C2} -i uda.s | sed -f ../vax/asm.sed | ${AS} -o uda.o
	rm -f uda.s

up.o: ../vaxuba/up.c
	${CC} -I. -c -S ${COPTS} ../vaxuba/up.c
	${C2} -i up.s | sed -f ../vax/asm.sed | ${AS} -o up.o
	rm -f up.s

ut.o: ../vaxuba/ut.c
	${CC} -I. -c -S ${COPTS} ../vaxuba/ut.c
	${C2} -i ut.s | sed -f ../vax/asm.sed | ${AS} -o ut.o
	rm -f ut.s


# DO NOT DELETE THIS LINE -- make depend uses it

af.o: ../h/param.h ../h/mbuf.h ../h/protosw.h ../h/socket.h ../h/socketvar.h
af.o: ../net/af.h
if.o: ../h/param.h ../h/systm.h ../h/socket.h ../h/protosw.h ../h/dir.h
if.o: ../h/user.h ../h/kernel.h ../h/ioctl.h ../h/errno.h ../net/if.h
if.o: ../net/af.h
raw_cb.o: ../h/param.h ../h/systm.h ../h/mbuf.h ../h/socket.h ../h/socketvar.h
raw_cb.o: ../h/errno.h ../net/if.h ../net/route.h ../net/raw_cb.h
raw_cb.o: ../netinet/in.h ../netpup/pup.h ../vax/mtpr.h
raw_usrreq.o: ../h/param.h ../h/mbuf.h ../h/protosw.h ../h/socket.h
raw_usrreq.o: ../h/socketvar.h ../h/errno.h ../net/if.h ../net/route.h
raw_usrreq.o: ../net/netisr.h ../net/raw_cb.h ../vax/mtpr.h
route.o: ../h/param.h ../h/systm.h ../h/mbuf.h ../h/protosw.h ../h/socket.h
route.o: ../h/dir.h ../h/user.h ../h/ioctl.h ../h/errno.h ../net/if.h
route.o: ../net/af.h ../net/route.h
if_loop.o: ../h/param.h ../h/systm.h ../h/mbuf.h ../h/socket.h ../h/errno.h
if_loop.o: ../h/ioctl.h ../net/if.h ../net/netisr.h ../net/route.h
if_loop.o: ../netinet/in.h ../netinet/in_systm.h ../netinet/ip.h
if_loop.o: ../netinet/ip_var.h ../vax/mtpr.h
in.o: ../h/param.h ../h/mbuf.h ../h/protosw.h ../h/socket.h ../h/socketvar.h
in.o: ../netinet/in.h ../netinet/in_systm.h ../net/if.h ../net/route.h
in.o: ../net/af.h
in_pcb.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/mbuf.h
in_pcb.o: ../h/socket.h ../h/socketvar.h ../netinet/in.h ../netinet/in_systm.h
in_pcb.o: ../net/if.h ../net/route.h ../netinet/in_pcb.h ../h/protosw.h
in_proto.o: ../h/param.h ../h/socket.h ../h/protosw.h ../h/domain.h ../h/mbuf.h
in_proto.o: ../netinet/in.h ../netinet/in_systm.h imp.h nd.h
ip_icmp.o: ../h/param.h ../h/systm.h ../h/mbuf.h ../h/protosw.h ../h/socket.h
ip_icmp.o: ../h/time.h ../h/kernel.h ../net/route.h ../netinet/in.h
ip_icmp.o: ../netinet/in_systm.h ../netinet/ip.h ../netinet/ip_icmp.h
ip_icmp.o: ../netinet/icmp_var.h
ip_input.o: ../h/param.h ../h/systm.h ../h/mbuf.h ../h/domain.h ../h/protosw.h
ip_input.o: ../h/socket.h ../h/errno.h ../h/time.h ../h/kernel.h ../net/if.h
ip_input.o: ../net/route.h ../netinet/in.h ../netinet/in_pcb.h
ip_input.o: ../netinet/in_systm.h ../netinet/ip.h ../netinet/ip_var.h
ip_input.o: ../netinet/ip_icmp.h ../netinet/tcp.h nd.h
ip_output.o: ../h/param.h ../h/mbuf.h ../h/errno.h ../h/socket.h
ip_output.o: ../h/socketvar.h ../net/if.h ../net/route.h ../netinet/in.h
ip_output.o: ../netinet/in_systm.h ../netinet/ip.h ../netinet/ip_var.h
ip_output.o: ../vax/mtpr.h
raw_ip.o: ../h/param.h ../h/mbuf.h ../h/socket.h ../h/protosw.h
raw_ip.o: ../h/socketvar.h ../h/errno.h ../net/if.h ../net/route.h
raw_ip.o: ../net/raw_cb.h ../netinet/in.h ../netinet/in_systm.h ../netinet/ip.h
raw_ip.o: ../netinet/ip_var.h
tcp_debug.o: ../h/param.h ../h/systm.h ../h/mbuf.h ../h/socket.h
tcp_debug.o: ../h/socketvar.h ../h/protosw.h ../h/errno.h ../net/route.h
tcp_debug.o: ../net/if.h ../netinet/in.h ../netinet/in_pcb.h
tcp_debug.o: ../netinet/in_systm.h ../netinet/ip.h ../netinet/ip_var.h
tcp_debug.o: ../netinet/tcp.h ../netinet/tcp_fsm.h ../netinet/tcp_seq.h
tcp_debug.o: ../netinet/tcp_timer.h ../netinet/tcp_var.h ../netinet/tcpip.h
tcp_debug.o: ../netinet/tcp_debug.h
tcp_input.o: ../h/param.h ../h/systm.h ../h/mbuf.h ../h/protosw.h ../h/socket.h
tcp_input.o: ../h/socketvar.h ../h/errno.h ../net/if.h ../net/route.h
tcp_input.o: ../netinet/in.h ../netinet/in_pcb.h ../netinet/in_systm.h
tcp_input.o: ../netinet/ip.h ../netinet/ip_var.h ../netinet/tcp.h
tcp_input.o: ../netinet/tcp_fsm.h ../netinet/tcp_seq.h ../netinet/tcp_timer.h
tcp_input.o: ../netinet/tcp_var.h ../netinet/tcpip.h ../netinet/tcp_debug.h
tcp_output.o: ../h/param.h ../h/systm.h ../h/mbuf.h ../h/protosw.h
tcp_output.o: ../h/socket.h ../h/socketvar.h ../h/errno.h ../net/route.h
tcp_output.o: ../netinet/in.h ../netinet/in_pcb.h ../netinet/in_systm.h
tcp_output.o: ../netinet/ip.h ../netinet/ip_var.h ../netinet/tcp.h
tcp_output.o: ../netinet/tcp_fsm.h ../netinet/tcp_seq.h ../netinet/tcp_timer.h
tcp_output.o: ../netinet/tcp_var.h ../netinet/tcpip.h ../netinet/tcp_debug.h
tcp_subr.o: ../h/param.h ../h/systm.h ../h/mbuf.h ../h/socket.h
tcp_subr.o: ../h/socketvar.h ../h/protosw.h ../h/errno.h ../net/route.h
tcp_subr.o: ../net/if.h ../netinet/in.h ../netinet/in_pcb.h
tcp_subr.o: ../netinet/in_systm.h ../netinet/ip.h ../netinet/ip_var.h
tcp_subr.o: ../netinet/ip_icmp.h ../netinet/tcp.h ../netinet/tcp_fsm.h
tcp_subr.o: ../netinet/tcp_seq.h ../netinet/tcp_timer.h ../netinet/tcp_var.h
tcp_subr.o: ../netinet/tcpip.h
tcp_timer.o: ../h/param.h ../h/systm.h ../h/mbuf.h ../h/socket.h
tcp_timer.o: ../h/socketvar.h ../h/protosw.h ../h/errno.h ../net/if.h
tcp_timer.o: ../net/route.h ../netinet/in.h ../netinet/in_pcb.h
tcp_timer.o: ../netinet/in_systm.h ../netinet/ip.h ../netinet/ip_var.h
tcp_timer.o: ../netinet/tcp.h ../netinet/tcp_fsm.h ../netinet/tcp_seq.h
tcp_timer.o: ../netinet/tcp_timer.h ../netinet/tcp_var.h ../netinet/tcpip.h
tcp_usrreq.o: ../h/param.h ../h/systm.h ../h/mbuf.h ../h/socket.h
tcp_usrreq.o: ../h/socketvar.h ../h/protosw.h ../h/errno.h ../net/if.h
tcp_usrreq.o: ../net/route.h ../netinet/in.h ../netinet/in_pcb.h
tcp_usrreq.o: ../netinet/in_systm.h ../netinet/ip.h ../netinet/ip_var.h
tcp_usrreq.o: ../netinet/tcp.h ../netinet/tcp_fsm.h ../netinet/tcp_seq.h
tcp_usrreq.o: ../netinet/tcp_timer.h ../netinet/tcp_var.h ../netinet/tcpip.h
tcp_usrreq.o: ../netinet/tcp_debug.h
udp_usrreq.o: ../h/param.h ../h/dir.h ../h/user.h ../h/mbuf.h ../h/protosw.h
udp_usrreq.o: ../h/socket.h ../h/socketvar.h ../h/errno.h ../net/if.h
udp_usrreq.o: ../net/route.h ../netinet/in.h ../netinet/in_pcb.h
udp_usrreq.o: ../netinet/in_systm.h ../netinet/ip.h ../netinet/ip_var.h
udp_usrreq.o: ../netinet/ip_icmp.h ../netinet/udp.h ../netinet/udp_var.h
init_main.o: ../machine/pte.h ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h
init_main.o: ../h/kernel.h ../h/fs.h ../h/mount.h ../h/map.h ../h/proc.h
init_main.o: ../h/inode.h ../h/seg.h ../h/conf.h ../h/buf.h ../h/vm.h
init_main.o: ../h/cmap.h ../h/text.h ../h/clist.h ../h/protosw.h ../h/quota.h
init_main.o: ../machine/reg.h ../machine/cpu.h loop.h kg.h
init_sysent.o: ../h/param.h ../h/systm.h
kern_acct.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/inode.h
kern_acct.o: ../h/fs.h ../h/kernel.h ../h/acct.h ../h/nami.h ../h/uio.h
kern_clock.o: ../machine/reg.h ../machine/psl.h ../h/param.h ../h/systm.h
kern_clock.o: ../h/dk.h ../h/callout.h ../h/dir.h ../h/user.h ../h/kernel.h
kern_clock.o: ../h/proc.h ../h/vm.h ../h/text.h ../vax/mtpr.h ../h/gprof.h
kern_descrip.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/kernel.h
kern_descrip.o: ../h/inode.h ../h/proc.h ../h/conf.h ../h/file.h ../h/socket.h
kern_descrip.o: ../h/socketvar.h ../h/mount.h ../h/stat.h ../h/ioctl.h
kern_exec.o: ../machine/reg.h ../machine/pte.h ../machine/psl.h ../h/param.h
kern_exec.o: ../h/systm.h ../h/map.h ../h/dir.h ../h/user.h ../h/kernel.h
kern_exec.o: ../h/proc.h ../h/buf.h ../h/inode.h ../h/seg.h ../h/vm.h
kern_exec.o: ../h/text.h ../h/file.h ../h/uio.h ../h/nami.h ../h/acct.h
kern_exec.o: ../vax/mtpr.h
kern_exit.o: ../machine/reg.h ../machine/psl.h ../h/param.h ../h/systm.h
kern_exit.o: ../h/map.h ../h/dir.h ../h/user.h ../h/kernel.h ../h/proc.h
kern_exit.o: ../h/buf.h ../h/wait.h ../h/vm.h ../h/file.h ../h/mbuf.h
kern_exit.o: ../h/inode.h
kern_fork.o: ../machine/reg.h ../machine/pte.h ../machine/psl.h ../h/param.h
kern_fork.o: ../h/systm.h ../h/map.h ../h/dir.h ../h/user.h ../h/kernel.h
kern_fork.o: ../h/proc.h ../h/inode.h ../h/seg.h ../h/vm.h ../h/text.h
kern_fork.o: ../h/file.h ../h/acct.h ../h/quota.h
kern_mman.o: ../machine/reg.h ../machine/psl.h ../machine/pte.h ../h/param.h
kern_mman.o: ../h/systm.h ../h/map.h ../h/dir.h ../h/user.h ../h/proc.h
kern_mman.o: ../h/buf.h ../h/inode.h ../h/seg.h ../h/acct.h ../h/wait.h
kern_mman.o: ../h/vm.h ../h/text.h ../h/file.h ../h/vadvise.h ../h/cmap.h
kern_mman.o: ../h/trace.h ../h/mman.h ../h/conf.h ../vax/mtpr.h
kern_proc.o: ../machine/reg.h ../machine/pte.h ../machine/psl.h ../h/param.h
kern_proc.o: ../h/systm.h ../h/map.h ../h/dir.h ../h/user.h ../h/kernel.h
kern_proc.o: ../h/proc.h ../h/buf.h ../h/inode.h ../h/seg.h ../h/acct.h
kern_proc.o: ../h/wait.h ../h/vm.h ../h/text.h ../h/file.h ../h/quota.h
kern_proc.o: ../h/uio.h ../h/mbuf.h ../h/nami.h
kern_prot.o: ../machine/reg.h ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h
kern_prot.o: ../h/inode.h ../h/proc.h ../h/timeb.h ../h/times.h ../h/reboot.h
kern_prot.o: ../h/fs.h ../h/conf.h ../h/buf.h ../h/mount.h ../h/quota.h
kern_resource.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/inode.h
kern_resource.o: ../h/proc.h ../h/seg.h ../h/fs.h ../h/uio.h ../h/vm.h
kern_resource.o: ../h/kernel.h
kern_sig.o: ../machine/reg.h ../machine/pte.h ../machine/psl.h ../h/param.h
kern_sig.o: ../h/systm.h ../h/dir.h ../h/user.h ../h/inode.h ../h/proc.h
kern_sig.o: ../h/timeb.h ../h/times.h ../h/conf.h ../h/buf.h ../h/mount.h
kern_sig.o: ../h/text.h ../h/seg.h ../h/vm.h ../h/acct.h ../h/uio.h
kern_sig.o: ../h/kernel.h ../h/nami.h ../vax/mtpr.h
kern_subr.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/uio.h
kern_synch.o: ../machine/pte.h ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h
kern_synch.o: ../h/proc.h ../h/file.h ../h/inode.h ../h/vm.h ../h/kernel.h
kern_synch.o: ../h/buf.h ../vax/mtpr.h
kern_time.o: ../machine/reg.h ../h/param.h ../h/dir.h ../h/user.h ../h/kernel.h
kern_time.o: ../h/inode.h ../h/proc.h
kern_xxx.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/kernel.h
kern_xxx.o: ../h/proc.h ../h/reboot.h ../h/quota.h ../h/times.h ../h/vtimes.h
kern_xxx.o: ../machine/psl.h ../machine/reg.h
quota_kern.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/proc.h
quota_kern.o: ../h/inode.h ../h/quota.h ../h/mount.h ../h/fs.h ../h/uio.h
quota_kern.o: ../h/nami.h
quota_subr.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/proc.h
quota_subr.o: ../h/inode.h ../h/quota.h ../h/mount.h ../h/fs.h ../h/uio.h
quota_subr.o: ../h/nami.h
quota_sys.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/proc.h
quota_sys.o: ../h/inode.h ../h/quota.h ../h/mount.h ../h/fs.h ../h/uio.h
quota_sys.o: ../h/nami.h
quota_ufs.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/proc.h
quota_ufs.o: ../h/inode.h ../h/quota.h ../h/mount.h ../h/fs.h ../h/uio.h
quota_ufs.o: ../h/nami.h
subr_prf.o: ../h/param.h ../h/systm.h ../h/seg.h ../h/buf.h ../h/conf.h
subr_prf.o: ../h/reboot.h ../h/vm.h ../h/msgbuf.h ../h/dir.h ../h/user.h
subr_prf.o: ../h/proc.h ../h/tty.h ../vax/mtpr.h
subr_rmap.o: ../h/param.h ../h/systm.h ../h/map.h ../h/dir.h ../h/user.h
subr_rmap.o: ../h/proc.h ../h/text.h ../h/kernel.h
subr_xxx.o: ../machine/pte.h ../h/param.h ../h/systm.h ../h/conf.h ../h/inode.h
subr_xxx.o: ../h/dir.h ../h/user.h ../h/buf.h ../h/proc.h ../h/fs.h ../h/vm.h
subr_xxx.o: ../h/cmap.h ../h/uio.h
sys_generic.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/ioctl.h
sys_generic.o: ../h/file.h ../h/proc.h ../h/uio.h ../h/kernel.h ../h/stat.h
sys_inode.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/inode.h
sys_inode.o: ../h/proc.h ../h/fs.h ../h/conf.h ../h/buf.h ../h/mount.h
sys_inode.o: ../h/file.h ../h/uio.h ../h/ioctl.h ../h/tty.h ../h/cmap.h
sys_inode.o: ../h/stat.h ../h/kernel.h
sys_process.o: ../machine/reg.h ../machine/psl.h ../machine/pte.h ../h/param.h
sys_process.o: ../h/systm.h ../h/dir.h ../h/user.h ../h/proc.h ../h/inode.h
sys_process.o: ../h/text.h ../h/seg.h ../h/vm.h ../h/buf.h ../h/acct.h
sys_socket.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/file.h
sys_socket.o: ../h/mbuf.h ../h/protosw.h ../h/socket.h ../h/socketvar.h
sys_socket.o: ../h/ioctl.h ../h/uio.h ../h/stat.h ../net/if.h ../net/route.h
tty.o: ../machine/reg.h ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h
tty.o: ../h/ioctl.h ../h/tty.h ../h/proc.h ../h/inode.h ../h/file.h ../h/conf.h
tty.o: ../h/buf.h ../h/dk.h ../h/uio.h ../h/kernel.h
tty_conf.o: ../h/param.h ../h/systm.h ../h/buf.h ../h/tty.h ../h/conf.h bk.h
tty_conf.o: tb.h
tty_pty.o: pty.h ../h/param.h ../h/systm.h ../h/ioctl.h ../h/tty.h ../h/dir.h
tty_pty.o: ../h/user.h ../h/conf.h ../h/file.h ../h/proc.h ../h/uio.h
tty_pty.o: ../h/kernel.h
tty_subr.o: ../h/param.h ../h/systm.h ../h/conf.h ../h/buf.h ../h/tty.h
tty_subr.o: ../h/clist.h
tty_tty.o: ../h/param.h ../h/systm.h ../h/conf.h ../h/dir.h ../h/user.h
tty_tty.o: ../h/ioctl.h ../h/tty.h ../h/proc.h ../h/uio.h
ufs_alloc.o: ../h/param.h ../h/systm.h ../h/mount.h ../h/fs.h ../h/conf.h
ufs_alloc.o: ../h/buf.h ../h/inode.h ../h/dir.h ../h/user.h ../h/quota.h
ufs_alloc.o: ../h/kernel.h
ufs_bio.o: ../machine/pte.h ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h
ufs_bio.o: ../h/buf.h ../h/conf.h ../h/proc.h ../h/seg.h ../h/vm.h ../h/trace.h
ufs_bmap.o: ../h/param.h ../h/systm.h ../h/conf.h ../h/inode.h ../h/dir.h
ufs_bmap.o: ../h/user.h ../h/buf.h ../h/proc.h ../h/fs.h
ufs_dsort.o: ../h/param.h ../h/systm.h ../h/buf.h
ufs_fio.o: ../machine/reg.h ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h
ufs_fio.o: ../h/fs.h ../h/file.h ../h/conf.h ../h/inode.h ../h/acct.h
ufs_fio.o: ../h/mount.h ../h/socket.h ../h/socketvar.h ../h/proc.h ../h/nami.h
ufs_inode.o: ../h/param.h ../h/systm.h ../h/mount.h ../h/dir.h ../h/user.h
ufs_inode.o: ../h/inode.h ../h/fs.h ../h/conf.h ../h/buf.h ../h/quota.h
ufs_inode.o: ../h/kernel.h
ufs_mount.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/inode.h
ufs_mount.o: ../h/proc.h ../h/fs.h ../h/buf.h ../h/mount.h ../h/file.h
ufs_mount.o: ../h/nami.h ../h/conf.h
ufs_nami.o: ../h/param.h ../h/systm.h ../h/inode.h ../h/fs.h ../h/mount.h
ufs_nami.o: ../h/dir.h ../h/user.h ../h/buf.h ../h/conf.h ../h/uio.h
ufs_nami.o: ../h/nami.h
ufs_subr.o: ../h/param.h ../h/systm.h ../h/mount.h ../h/fs.h ../h/conf.h
ufs_subr.o: ../h/buf.h ../h/inode.h ../h/dir.h ../h/user.h ../h/quota.h
ufs_subr.o: ../h/kernel.h
ufs_syscalls.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/kernel.h
ufs_syscalls.o: ../h/file.h ../h/stat.h ../h/inode.h ../h/fs.h ../h/buf.h
ufs_syscalls.o: ../h/proc.h ../h/quota.h ../h/uio.h ../h/socket.h
ufs_syscalls.o: ../h/socketvar.h ../h/nami.h ../h/mount.h
ufs_tables.o: ../h/param.h
ufs_xxx.o: ../h/param.h ../h/systm.h ../h/inode.h ../h/fs.h ../h/mount.h
ufs_xxx.o: ../h/dir.h ../h/user.h ../h/buf.h ../h/conf.h ../h/file.h
ufs_xxx.o: ../h/nami.h ../h/kernel.h
uipc_domain.o: ../h/param.h ../h/socket.h ../h/protosw.h ../h/domain.h
uipc_domain.o: ../h/time.h ../h/kernel.h imp.h
uipc_mbuf.o: ../machine/pte.h ../h/param.h ../h/dir.h ../h/user.h ../h/proc.h
uipc_mbuf.o: ../h/cmap.h ../h/map.h ../h/mbuf.h ../h/vm.h ../h/kernel.h
uipc_pipe.o: ../h/param.h ../h/mbuf.h ../h/protosw.h ../h/socket.h
uipc_pipe.o: ../h/socketvar.h ../h/unpcb.h
uipc_proto.o: ../h/param.h ../h/socket.h ../h/protosw.h ../h/domain.h
uipc_proto.o: ../h/mbuf.h
uipc_socket.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/proc.h
uipc_socket.o: ../h/file.h ../h/inode.h ../h/buf.h ../h/mbuf.h ../h/un.h
uipc_socket.o: ../h/protosw.h ../h/socket.h ../h/socketvar.h ../h/stat.h
uipc_socket.o: ../h/ioctl.h ../h/uio.h ../net/route.h ../netinet/in.h
uipc_socket.o: ../net/if.h
uipc_socket2.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/proc.h
uipc_socket2.o: ../h/file.h ../h/inode.h ../h/buf.h ../h/mbuf.h ../h/protosw.h
uipc_socket2.o: ../h/socket.h ../h/socketvar.h
uipc_syscalls.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/proc.h
uipc_syscalls.o: ../h/file.h ../h/inode.h ../h/buf.h ../h/mbuf.h ../h/protosw.h
uipc_syscalls.o: ../h/socket.h ../h/socketvar.h ../h/uio.h
uipc_usrreq.o: ../h/param.h ../h/dir.h ../h/user.h ../h/mbuf.h ../h/protosw.h
uipc_usrreq.o: ../h/socket.h ../h/socketvar.h ../h/unpcb.h ../h/un.h
uipc_usrreq.o: ../h/inode.h ../h/nami.h ../h/file.h
vm_drum.o: ../machine/pte.h ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h
vm_drum.o: ../h/proc.h ../h/buf.h ../h/text.h ../h/map.h ../h/vm.h ../h/cmap.h
vm_drum.o: ../h/kernel.h
vm_mem.o: ../machine/pte.h ../h/param.h ../h/systm.h ../h/cmap.h ../h/dir.h
vm_mem.o: ../h/user.h ../h/proc.h ../h/text.h ../h/vm.h ../h/file.h
vm_mem.o: ../h/inode.h ../h/buf.h ../h/mount.h ../h/trace.h ../h/map.h
vm_mem.o: ../h/kernel.h
vm_mon.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/vmmeter.h
vm_mon.o: ../h/trace.h
vm_page.o: ../machine/reg.h ../machine/pte.h ../h/param.h ../h/systm.h
vm_page.o: ../h/inode.h ../h/dir.h ../h/user.h ../h/proc.h ../h/buf.h
vm_page.o: ../h/text.h ../h/cmap.h ../h/vm.h ../h/file.h ../h/trace.h
vm_page.o: ../vax/mtpr.h
vm_proc.o: ../machine/pte.h ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h
vm_proc.o: ../h/proc.h ../h/map.h ../h/cmap.h ../h/text.h ../h/vm.h
vm_proc.o: ../vax/mtpr.h
vm_pt.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/proc.h
vm_pt.o: ../h/map.h ../h/cmap.h ../h/vm.h ../h/buf.h ../h/text.h ../h/mount.h
vm_pt.o: ../h/inode.h ../h/kernel.h ../machine/pte.h ../vax/mtpr.h
vm_sched.o: ../h/param.h ../h/systm.h ../h/seg.h ../h/dir.h ../h/user.h
vm_sched.o: ../h/proc.h ../h/text.h ../h/vm.h ../h/cmap.h ../h/kernel.h
vm_subr.o: ../machine/pte.h ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h
vm_subr.o: ../h/vm.h ../h/proc.h ../h/cmap.h ../h/inode.h ../h/buf.h
vm_subr.o: ../h/text.h ../h/fs.h ../vax/mtpr.h
vm_sw.o: ../h/param.h ../h/systm.h ../h/buf.h ../h/conf.h ../h/dir.h
vm_sw.o: ../h/user.h ../h/inode.h ../h/map.h ../h/uio.h ../h/file.h ../h/nami.h
vm_swap.o: ../machine/pte.h ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h
vm_swap.o: ../h/proc.h ../h/text.h ../h/map.h ../h/buf.h ../h/cmap.h ../h/vm.h
vm_swp.o: ../machine/pte.h ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h
vm_swp.o: ../h/buf.h ../h/conf.h ../h/proc.h ../h/seg.h ../h/vm.h ../h/trace.h
vm_swp.o: ../h/map.h ../h/uio.h
vm_text.o: ../machine/pte.h ../h/param.h ../h/systm.h ../h/map.h ../h/dir.h
vm_text.o: ../h/user.h ../h/proc.h ../h/text.h ../h/inode.h ../h/buf.h
vm_text.o: ../h/seg.h ../h/vm.h ../h/cmap.h ../h/uio.h
autoconf.o: mba.h uba.h ../machine/pte.h ../h/param.h ../h/systm.h ../h/map.h
autoconf.o: ../h/buf.h ../h/dk.h ../h/vm.h ../h/conf.h ../h/dmap.h ../vax/cpu.h
autoconf.o: ../vax/mem.h ../vax/mtpr.h ../vax/nexus.h ../vax/scb.h
autoconf.o: ../vaxmba/mbareg.h ../vaxmba/mbavar.h ../vaxuba/ubareg.h
autoconf.o: ../vaxuba/ubavar.h
clock.o: ../h/param.h ../h/time.h ../h/kernel.h ../vax/mtpr.h ../vax/clock.h
conf.o: ../h/param.h ../h/systm.h ../h/buf.h ../h/tty.h ../h/conf.h hp.h tu.h
conf.o: rk.h te.h ts.h mu.h ra.h up.h tj.h rb.h rx.h uu.h rl.h acc.h ct.h dh.h
conf.o: dmf.h dz.h lp.h va.h vp.h pty.h lpa.h dn.h gpib.h ik.h ps.h ib.h ad.h
cons.o: ../h/param.h ../h/conf.h ../h/dir.h ../h/user.h ../h/proc.h
cons.o: ../h/ioctl.h ../h/tty.h ../h/systm.h ../h/uio.h ../vax/cpu.h
cons.o: ../vax/cons.h ../vax/mtpr.h
cpudata.o: ../machine/pte.h ../h/param.h ../vax/cpu.h ../vax/nexus.h
cpudata.o: ../vaxuba/ubareg.h
dkbad.o: ../h/param.h ../h/buf.h ../h/dkbad.h
flp.o: ../h/param.h ../h/systm.h ../h/conf.h ../h/dir.h ../h/user.h ../h/buf.h
flp.o: ../h/uio.h ../vax/cons.h ../vax/cpu.h ../vax/flp.h ../vax/mtpr.h
in_cksum.o: ../h/types.h ../h/mbuf.h ../netinet/in.h ../netinet/in_systm.h
machdep.o: ../machine/reg.h ../machine/pte.h ../machine/psl.h ../h/param.h
machdep.o: ../h/systm.h ../h/dir.h ../h/user.h ../h/kernel.h ../h/map.h
machdep.o: ../h/vm.h ../h/proc.h ../h/buf.h ../h/reboot.h ../h/conf.h
machdep.o: ../h/inode.h ../h/file.h ../h/text.h ../h/clist.h ../h/callout.h
machdep.o: ../h/cmap.h ../h/mbuf.h ../h/msgbuf.h ../h/quota.h ../vax/frame.h
machdep.o: ../vax/cons.h ../vax/cpu.h ../vax/mem.h ../vax/mtpr.h ../vax/rpb.h
machdep.o: ../vaxuba/ubavar.h ../vaxuba/ubareg.h
mem.o: ../machine/pte.h ../h/param.h ../h/dir.h ../h/user.h ../h/conf.h
mem.o: ../h/buf.h ../h/systm.h ../h/vm.h ../h/cmap.h ../h/uio.h ../vax/mtpr.h
sys_machdep.o: ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h ../h/ioctl.h
sys_machdep.o: ../h/file.h ../h/proc.h ../h/uio.h ../h/kernel.h ../h/mtio.h
sys_machdep.o: ../h/buf.h ../vax/dkio.h ../vax/pte.h ../vaxuba/ubareg.h
sys_machdep.o: ../vaxuba/ubavar.h
trap.o: ../machine/psl.h ../machine/reg.h ../machine/pte.h ../h/param.h
trap.o: ../h/systm.h ../h/dir.h ../h/user.h assym.s ../h/proc.h ../h/seg.h
trap.o: ../vax/trap.h ../h/acct.h ../h/kernel.h ../sys/syscalls.c ../vax/mtpr.h
tu.o: ../h/param.h ../h/systm.h ../h/buf.h ../h/conf.h ../h/dir.h ../h/user.h
tu.o: ../h/kernel.h ../vax/cpu.h ../vax/mtpr.h ../vax/rsp.h
ufs_machdep.o: ../machine/pte.h ../h/param.h ../h/systm.h ../h/dir.h
ufs_machdep.o: ../h/user.h ../h/buf.h ../h/conf.h ../h/proc.h ../h/seg.h
ufs_machdep.o: ../h/vm.h
vm_machdep.o: ../machine/pte.h ../h/param.h ../h/systm.h ../h/dir.h ../h/user.h
vm_machdep.o: ../h/proc.h ../h/cmap.h ../h/mount.h ../h/vm.h ../h/text.h
vm_machdep.o: ../vax/mtpr.h
if_uba.o: ../machine/pte.h ../h/param.h ../h/systm.h ../h/mbuf.h ../h/map.h
if_uba.o: ../h/buf.h ../h/cmap.h ../h/vmmac.h ../h/socket.h ../net/if.h
if_uba.o: ../vax/mtpr.h ../vaxif/if_uba.h ../vaxuba/ubareg.h ../vaxuba/ubavar.h
hp.o: hp.h ../machine/pte.h ../h/param.h ../h/systm.h ../h/dk.h ../h/buf.h
hp.o: ../h/conf.h ../h/dir.h ../h/user.h ../h/map.h ../vax/mtpr.h ../h/vm.h
hp.o: ../h/cmap.h ../h/dkbad.h ../h/ioctl.h ../h/uio.h ../vax/dkio.h
hp.o: ../vaxmba/mbareg.h ../vaxmba/mbavar.h ../vaxmba/hpreg.h
ht.o: tu.h ../machine/pte.h ../h/param.h ../h/systm.h ../h/buf.h ../h/conf.h
ht.o: ../h/dir.h ../h/file.h ../h/user.h ../h/map.h ../h/ioctl.h ../h/mtio.h
ht.o: ../h/cmap.h ../h/uio.h ../vax/cpu.h ../vaxmba/mbareg.h ../vaxmba/mbavar.h
ht.o: ../vaxmba/htreg.h
mba.o: mba.h ../machine/pte.h ../h/param.h ../h/systm.h ../h/dk.h ../h/buf.h
mba.o: ../h/conf.h ../h/dir.h ../h/user.h ../h/proc.h ../h/map.h ../vax/mtpr.h
mba.o: ../h/vm.h ../vaxmba/mbareg.h ../vaxmba/mbavar.h
mt.o: mu.h ../machine/pte.h ../h/param.h ../h/systm.h ../h/buf.h ../h/conf.h
mt.o: ../h/dir.h ../h/file.h ../h/user.h ../h/map.h ../h/ioctl.h ../h/mtio.h
mt.o: ../h/cmap.h ../h/uio.h ../vax/cpu.h ../vaxmba/mbareg.h ../vaxmba/mbavar.h
mt.o: ../vaxmba/mtreg.h
dh.o: dh.h ../machine/pte.h bk.h ../h/param.h ../h/conf.h ../h/dir.h
dh.o: ../h/user.h ../h/proc.h ../h/ioctl.h ../h/tty.h ../h/map.h ../h/buf.h
dh.o: ../h/vm.h ../vaxuba/ubareg.h ../vaxuba/ubavar.h ../vaxuba/dhreg.h
dh.o: ../vaxuba/dmreg.h ../h/bk.h ../h/clist.h ../h/file.h ../h/uio.h
dmf.o: dmf.h ../machine/pte.h bk.h ../h/param.h ../h/conf.h ../h/dir.h
dmf.o: ../h/user.h ../h/ioctl.h ../h/tty.h ../h/map.h ../h/buf.h ../h/vm.h
dmf.o: ../h/bk.h ../h/clist.h ../h/file.h ../h/uio.h ../vaxuba/ubareg.h
dmf.o: ../vaxuba/ubavar.h ../vaxuba/dmfreg.h
dz.o: dz.h bk.h ../machine/pte.h ../h/param.h ../h/systm.h ../h/ioctl.h
dz.o: ../h/tty.h ../h/dir.h ../h/user.h ../h/proc.h ../h/map.h ../h/buf.h
dz.o: ../h/vm.h ../h/conf.h ../h/bk.h ../h/file.h ../h/uio.h ../h/kernel.h
dz.o: ../vaxuba/pdma.h ../vaxuba/ubavar.h ../vaxuba/dzreg.h
idc.o: rb.h ../machine/pte.h ../h/param.h ../h/systm.h ../h/buf.h ../h/conf.h
idc.o: ../h/dir.h ../h/user.h ../h/map.h ../h/vm.h ../h/dk.h ../h/cmap.h
idc.o: ../h/dkbad.h ../h/uio.h ../h/kernel.h ../vax/cpu.h ../vaxuba/ubareg.h
idc.o: ../vaxuba/ubavar.h ../vaxuba/idcreg.h
lp.o: lp.h ../machine/pte.h ../h/param.h ../h/dir.h ../h/user.h ../h/buf.h
lp.o: ../h/systm.h ../h/map.h ../h/uio.h ../h/tty.h ../h/kernel.h
lp.o: ../vaxuba/ubavar.h
rk.o: rk.h ../machine/pte.h ../h/param.h ../h/systm.h ../h/buf.h ../h/conf.h
rk.o: ../h/dir.h ../h/user.h ../h/map.h ../h/vm.h ../h/dk.h ../h/cmap.h
rk.o: ../h/dkbad.h ../h/uio.h ../h/kernel.h ../vax/cpu.h ../vaxuba/ubareg.h
rk.o: ../vaxuba/ubavar.h ../vaxuba/rkreg.h
rl.o: rl.h ../machine/pte.h ../h/param.h ../h/systm.h ../h/dk.h ../h/dkbad.h
rl.o: ../h/buf.h ../h/conf.h ../h/dir.h ../h/user.h ../h/map.h ../h/vm.h
rl.o: ../h/cmap.h ../h/uio.h ../h/kernel.h ../vax/cpu.h ../vax/nexus.h
rl.o: ../vaxuba/ubavar.h ../vaxuba/ubareg.h ../vaxuba/rlreg.h
tm.o: te.h ts.h ../machine/pte.h ../h/param.h ../h/systm.h ../h/buf.h
tm.o: ../h/dir.h ../h/conf.h ../h/user.h ../h/file.h ../h/map.h ../h/vm.h
tm.o: ../h/ioctl.h ../h/mtio.h ../h/cmap.h ../h/uio.h ../h/kernel.h
tm.o: ../vax/cpu.h ../vaxuba/ubareg.h ../vaxuba/ubavar.h ../vaxuba/tmreg.h
ts.o: ts.h ../machine/pte.h ../h/param.h ../h/systm.h ../h/buf.h ../h/dir.h
ts.o: ../h/conf.h ../h/user.h ../h/file.h ../h/map.h ../h/vm.h ../h/ioctl.h
ts.o: ../h/mtio.h ../h/cmap.h ../h/uio.h ../vax/cpu.h ../vaxuba/ubareg.h
ts.o: ../vaxuba/ubavar.h ../vaxuba/tsreg.h
uba.o: ../machine/pte.h ../h/param.h ../h/systm.h ../h/map.h ../h/buf.h
uba.o: ../h/vm.h ../h/dir.h ../h/user.h ../h/proc.h ../h/conf.h ../h/dk.h
uba.o: ../h/kernel.h ../vax/cpu.h ../vax/mtpr.h ../vax/nexus.h
uba.o: ../vaxuba/ubareg.h ../vaxuba/ubavar.h ik.h
uda.o: ra.h ../machine/pte.h ../h/param.h ../h/systm.h ../h/buf.h ../h/conf.h
uda.o: ../h/dir.h ../h/user.h ../h/map.h ../h/vm.h ../h/dk.h ../h/cmap.h
uda.o: ../h/uio.h ../vax/cpu.h ../vaxuba/ubareg.h ../vaxuba/ubavar.h
uda.o: ../vaxuba/udareg.h ../vax/mscp.h
up.o: up.h ../machine/pte.h ../h/param.h ../h/systm.h ../h/dk.h ../h/dkbad.h
up.o: ../h/buf.h ../h/conf.h ../h/dir.h ../h/user.h ../h/map.h ../h/vm.h
up.o: ../h/cmap.h ../h/uio.h ../h/kernel.h ../vax/cpu.h ../vax/nexus.h
up.o: ../vaxuba/ubavar.h ../vaxuba/ubareg.h ../vaxuba/upreg.h
ut.o: tj.h ../machine/pte.h ../h/param.h ../h/systm.h ../h/buf.h ../h/conf.h
ut.o: ../h/dir.h ../h/file.h ../h/user.h ../h/map.h ../h/ioctl.h ../h/mtio.h
ut.o: ../h/cmap.h ../h/uio.h ../h/kernel.h ../vax/cpu.h ../vaxuba/ubareg.h
ut.o: ../vaxuba/ubavar.h ../vaxuba/utreg.h
