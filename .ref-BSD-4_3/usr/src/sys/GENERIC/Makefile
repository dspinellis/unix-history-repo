IDENT=-DGENERIC -DVAX730 -DVAX750 -DVAX780 -DVAX8600 -DTCP_COMPAT_42 -DNS -DINET -DQUOTA
PARAM=-DTIMEZONE=480 -DDST=1 -DMAXUSERS=8
#
# Copyright (c) 1980, 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile.vax	7.1 (Berkeley) 6/5/86
#
# Makefile for 4.3 BSD
#
# This makefile is constructed from a machine description:
#	config machineid
# Most changes should be made in the machine description
#	/sys/conf/``machineid''
# after which you should do
#	 config machineid
# Generic makefile changes should be made in
#	/sys/conf/Makefile.``machinetype''
# after which config should be rerun for all machines.
#
# N.B.: NO DEPENDENCIES ON FOLLOWING FLAGS ARE VISIBLE TO MAKEFILE
#	IF YOU CHANGE THE DEFINITION OF ANY OF THESE RECOMPILE EVERYTHING
#
# -DTRACE	compile in kernel tracing hooks
# -DQUOTA	compile in file system quotas
# -DUUDMA	compile in unibus tu58 pseudo-dma code
#
C2=	/lib/c2
LD=	/bin/ld
INLINECMD= ../vax/inline/inline
INLINE=	${INLINECMD} ${INLINEOPTS}
TOUCH=	touch -f -c

INCLUDES= -I. -I../h
COPTS=	${INCLUDES} ${IDENT} -DKERNEL
CFLAGS=	-O ${COPTS}

AHEADS=	../vax/pcb.m

OBJS=af.o if.o if_loop.o raw_cb.o raw_usrreq.o route.o if_ether.o in.o \
	in_pcb.o in_proto.o ip_icmp.o ip_input.o ip_output.o raw_ip.o \
	tcp_debug.o tcp_input.o tcp_output.o tcp_subr.o tcp_timer.o \
	tcp_usrreq.o udp_usrreq.o idp_usrreq.o ns.o ns_error.o ns_ip.o \
	ns_input.o ns_output.o ns_pcb.o ns_proto.o spp_debug.o \
	spp_usrreq.o init_main.o init_sysent.o kern_acct.o kern_clock.o \
	kern_descrip.o kern_exec.o kern_exit.o kern_fork.o kern_mman.o \
	kern_proc.o kern_prot.o kern_resource.o kern_sig.o kern_subr.o \
	kern_synch.o kern_time.o kern_xxx.o quota_kern.o quota_subr.o \
	quota_sys.o quota_ufs.o subr_log.o subr_prf.o subr_rmap.o \
	subr_xxx.o sys_generic.o sys_inode.o sys_process.o sys_socket.o \
	tty.o tty_conf.o tty_pty.o tty_subr.o tty_tty.o ufs_alloc.o \
	ufs_bio.o ufs_bmap.o ufs_dsort.o ufs_fio.o ufs_inode.o \
	ufs_mount.o ufs_namei.o ufs_subr.o ufs_syscalls.o ufs_tables.o \
	ufs_xxx.o uipc_domain.o uipc_mbuf.o uipc_proto.o uipc_socket.o \
	uipc_socket2.o uipc_syscalls.o uipc_usrreq.o vm_drum.o vm_mem.o \
	vm_mon.o vm_page.o vm_proc.o vm_pt.o vm_sched.o vm_subr.o \
	vm_sw.o vm_swap.o vm_swp.o vm_text.o autoconf.o clock.o conf.o \
	cons.o cpudata.o crl.o dkbad.o flp.o in_cksum.o ka630.o \
	ns_cksum.o machdep.o mem.o sys_machdep.o trap.o tu.o udiv.o \
	ufs_machdep.o urem.o vm_machdep.o if_de.o if_ec.o if_en.o \
	if_ex.o if_il.o if_uba.o hp.o ht.o mba.o mt.o dh.o dhu.o dmf.o \
	dmz.o dz.o idc.o lp.o rk.o rl.o tm.o tmscp.o ts.o uba.o uda.o \
	up.o ut.o 

CFILES=../net/af.c ../net/if.c ../net/if_loop.c ../net/raw_cb.c \
	../net/raw_usrreq.c ../net/route.c ../netinet/if_ether.c \
	../netinet/in.c ../netinet/in_pcb.c ../netinet/in_proto.c \
	../netinet/ip_icmp.c ../netinet/ip_input.c \
	../netinet/ip_output.c ../netinet/raw_ip.c \
	../netinet/tcp_debug.c ../netinet/tcp_input.c \
	../netinet/tcp_output.c ../netinet/tcp_subr.c \
	../netinet/tcp_timer.c ../netinet/tcp_usrreq.c \
	../netinet/udp_usrreq.c ../netns/idp_usrreq.c ../netns/ns.c \
	../netns/ns_error.c ../netns/ns_ip.c ../netns/ns_input.c \
	../netns/ns_output.c ../netns/ns_pcb.c ../netns/ns_proto.c \
	../netns/spp_debug.c ../netns/spp_usrreq.c ../sys/init_main.c \
	../sys/init_sysent.c ../sys/kern_acct.c ../sys/kern_clock.c \
	../sys/kern_descrip.c ../sys/kern_exec.c ../sys/kern_exit.c \
	../sys/kern_fork.c ../sys/kern_mman.c ../sys/kern_proc.c \
	../sys/kern_prot.c ../sys/kern_resource.c ../sys/kern_sig.c \
	../sys/kern_subr.c ../sys/kern_synch.c ../sys/kern_time.c \
	../sys/kern_xxx.c ../sys/quota_kern.c ../sys/quota_subr.c \
	../sys/quota_sys.c ../sys/quota_ufs.c ../sys/subr_log.c \
	../sys/subr_prf.c ../sys/subr_rmap.c ../sys/subr_xxx.c \
	../sys/sys_generic.c ../sys/sys_inode.c ../sys/sys_process.c \
	../sys/sys_socket.c ../sys/tty.c ../sys/tty_conf.c \
	../sys/tty_pty.c ../sys/tty_subr.c ../sys/tty_tty.c \
	../sys/ufs_alloc.c ../sys/ufs_bio.c ../sys/ufs_bmap.c \
	../sys/ufs_dsort.c ../sys/ufs_fio.c ../sys/ufs_inode.c \
	../sys/ufs_mount.c ../sys/ufs_namei.c ../sys/ufs_subr.c \
	../sys/ufs_syscalls.c ../sys/ufs_tables.c ../sys/ufs_xxx.c \
	../sys/uipc_domain.c ../sys/uipc_mbuf.c ../sys/uipc_proto.c \
	../sys/uipc_socket.c ../sys/uipc_socket2.c \
	../sys/uipc_syscalls.c ../sys/uipc_usrreq.c ../sys/vm_drum.c \
	../sys/vm_mem.c ../sys/vm_mon.c ../sys/vm_page.c \
	../sys/vm_proc.c ../sys/vm_pt.c ../sys/vm_sched.c \
	../sys/vm_subr.c ../sys/vm_sw.c ../sys/vm_swap.c ../sys/vm_swp.c \
	../sys/vm_text.c ../vax/autoconf.c ../vax/clock.c ../vax/conf.c \
	../vax/cons.c ../vax/cpudata.c ../vax/crl.c ../vax/dkbad.c \
	../vax/flp.c ../vax/in_cksum.c ../vax/ka630.c ../vax/ns_cksum.c \
	../vax/machdep.c ../vax/mem.c ../vax/sys_machdep.c ../vax/trap.c \
	../vax/tu.c ../vax/ufs_machdep.c ../vax/vm_machdep.c \
	../vaxif/if_de.c ../vaxif/if_ec.c ../vaxif/if_en.c \
	../vaxif/if_ex.c ../vaxif/if_il.c ../vaxif/if_uba.c \
	../vaxmba/hp.c ../vaxmba/ht.c ../vaxmba/mba.c ../vaxmba/mt.c \
	../vaxuba/dh.c ../vaxuba/dhu.c ../vaxuba/dmf.c ../vaxuba/dmz.c \
	../vaxuba/dz.c ../vaxuba/idc.c ../vaxuba/lp.c ../vaxuba/rk.c \
	../vaxuba/rl.c ../vaxuba/tm.c ../vaxuba/tmscp.c ../vaxuba/ts.c \
	../vaxuba/uba.c ../vaxuba/uda.c ../vaxuba/up.c ../vaxuba/ut.c 

vmunix: Makefile ${INLINECMD} locore.o emulate.o ${OBJS} param.o ioconf.o swapgeneric.o
	@echo loading vmunix
	@rm -f vmunix
	@sh ../conf/newvers.sh
	@${CC} $(CFLAGS) -c vers.c
	@${LD} -n -o vmunix -e start -x -T 80000000 locore.o emulate.o ${OBJS} vers.o ioconf.o param.o swapgeneric.o
	@echo rearranging symbols
	@-symorder ../vax/symbols.sort vmunix
	@size vmunix
	@chmod 755 vmunix

swapgeneric.o: ../vax/swapgeneric.c
	${CC} -c -S ${COPTS} ../vax/swapgeneric.c
	${C2} swapgeneric.s | ${INLINE} | ${AS} -o swapgeneric.o
	rm -f swapgeneric.s

hkvmunix: Makefile ${INLINECMD} locore.o emulate.o ${OBJS} param.o ioconf.o swaphkvmunix.o
	@echo loading hkvmunix
	@rm -f hkvmunix
	@${LD} -n -o hkvmunix -e start -x -T 80000000 locore.o emulate.o ${OBJS} vers.o ioconf.o param.o swaphkvmunix.o
	@echo rearranging symbols
	@-symorder ../vax/symbols.sort hkvmunix
	@size hkvmunix
	@chmod 755 hkvmunix

swaphkvmunix.o: swaphkvmunix.c
	${CC} -c -O ${COPTS} swaphkvmunix.c

hpvmunix: Makefile ${INLINECMD} locore.o emulate.o ${OBJS} param.o ioconf.o swaphpvmunix.o
	@echo loading hpvmunix
	@rm -f hpvmunix
	@${LD} -n -o hpvmunix -e start -x -T 80000000 locore.o emulate.o ${OBJS} vers.o ioconf.o param.o swaphpvmunix.o
	@echo rearranging symbols
	@-symorder ../vax/symbols.sort hpvmunix
	@size hpvmunix
	@chmod 755 hpvmunix

swaphpvmunix.o: swaphpvmunix.c
	${CC} -c -O ${COPTS} swaphpvmunix.c

all: vmunix hkvmunix hpvmunix

clean:
	rm -f eddep *vmunix tags *.o locore.i [a-tv-z]*.s \
		errs linterrs makelinks

lint: /tmp param.c
	@lint -hbxn -DGENERIC ${COPTS} ${PARAM} ../vax/Locore.c \
	  ${CFILES} ../vax/swapgeneric.c ioconf.c param.c | \
	    grep -v 'struct/union .* never defined' | \
	    grep -v 'possible pointer alignment problem'

../vax/symbols.sort: ../vax/symbols.raw
	grep -v '^#' ../vax/symbols.raw \
	    | sed 's/^	//' | sort -u > ../vax/symbols.sort

../vax/inline/inline: ../vax/inline/inline.h ../vax/inline/langpats.c
../vax/inline/inline: ../vax/inline/libcpats.c ../vax/inline/machdep.c
../vax/inline/inline: ../vax/inline/machpats.c ../vax/inline/main.c
	cd ../vax/inline; make

locore.o: assym.s ${AHEADS} ../vax/rpb.s ../vax/scb.s ubvec.s \
    ../vax/locore.s ubglue.s ../vax/mtpr.h ../vax/trap.h ../machine/psl.h \
    ../machine/pte.h ../vax/cpu.h mba.h
	cat assym.s ../vax/rpb.s ../vax/scb.s ubvec.s ../vax/locore.s \
	    ubglue.s > locore.c
	cc -E -DLOCORE -I../vax ${COPTS} locore.c > locore.i
	@echo 'as -o locore.o $${AHEADS} locore.i'
	@as -o locore.o ${AHEADS} locore.i
	@rm locore.i

emulate.o: ../vax/emulate.s
	/lib/cpp -I. ${COPTS} ../vax/emulate.s | ${AS} -o emulate.o

# the following is necessary because autoconf.o depends on #if GENERIC
autoconf.o tu.o: Makefile

# the following are necessary because the files depend on the types of
# vax cpu's included in the system configuration
clock.o machdep.o autoconf.o conf.o cons.o cpudata.o : Makefile
crl.o flp.o mba.o uba.o vaxcpu.o : Makefile

# depend on network configuration
af.o uipc_proto.o locore.o: Makefile

# depend on maxusers
assym.s: Makefile

assym.s: ../h/param.h ../machine/pte.h ../h/buf.h ../h/vmparam.h \
    ../h/vmmeter.h ../h/dir.h ../h/cmap.h ../h/map.h ../vaxuba/ubavar.h \
    ../vaxuba/ubareg.h ../h/proc.h ../h/msgbuf.h ../h/text.h ../h/user.h \
    ../vax/rpb.h
	cc ${INCLUDES} ${IDENT} ${PARAM} ../vax/genassym.c
	./a.out >assym.s; rm -f a.out

depend:	assym.s
	${CC} -M ${COPTS} ${CFILES} | \
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
	@pr -f Makefile ../sys/TODO linterrs
	@/usr/ucb/ctags -x ${CFILES} | pr -f -h XREF
	@(size vmunix *.o) | pr -f -h sizes
	@ls -ls | pr -f
	@cd ../h; ls -ls | pr -f ; pr -f *.m *.h
	@echo 'once upon a time ...'
	@cd ../vax; pr -f Locore.c scb.s locore.s
	@cd ../vax/inline; make print
	@pr -f ${CFILES}

ioconf.o: ioconf.c ../h/param.h ../machine/pte.h ../h/buf.h ../h/map.h \
    ../vaxmba/mbavar.h ../h/vm.h ../vaxuba/ubavar.h
	${CC} -c -S ${COPTS} ioconf.c
	${C2} ioconf.s | ../vax/inline/inline | ${AS} -o ioconf.o
	rm -f ioconf.s

param.c: ../conf/param.c
	-rm -f param.c
	cp ../conf/param.c .

param.o: param.c Makefile
	${CC} -c ${CFLAGS} ${PARAM} param.c

af.o: ../net/af.c
	${CC} -c -S ${COPTS} ../net/af.c
	${C2} af.s | ${INLINE} | ${AS} -o af.o
	rm -f af.s

if.o: ../net/if.c
	${CC} -c -S ${COPTS} ../net/if.c
	${C2} if.s | ${INLINE} | ${AS} -o if.o
	rm -f if.s

if_loop.o: ../net/if_loop.c
	${CC} -c -S ${COPTS} ../net/if_loop.c
	${C2} if_loop.s | ${INLINE} | ${AS} -o if_loop.o
	rm -f if_loop.s

raw_cb.o: ../net/raw_cb.c
	${CC} -c -S ${COPTS} ../net/raw_cb.c
	${C2} raw_cb.s | ${INLINE} | ${AS} -o raw_cb.o
	rm -f raw_cb.s

raw_usrreq.o: ../net/raw_usrreq.c
	${CC} -c -S ${COPTS} ../net/raw_usrreq.c
	${C2} raw_usrreq.s | ${INLINE} | ${AS} -o raw_usrreq.o
	rm -f raw_usrreq.s

route.o: ../net/route.c
	${CC} -c -S ${COPTS} ../net/route.c
	${C2} route.s | ${INLINE} | ${AS} -o route.o
	rm -f route.s

if_ether.o: ../netinet/if_ether.c
	${CC} -c -S ${COPTS} ../netinet/if_ether.c
	${C2} if_ether.s | ${INLINE} | ${AS} -o if_ether.o
	rm -f if_ether.s

in.o: ../netinet/in.c
	${CC} -c -S ${COPTS} ../netinet/in.c
	${C2} in.s | ${INLINE} | ${AS} -o in.o
	rm -f in.s

in_pcb.o: ../netinet/in_pcb.c
	${CC} -c -S ${COPTS} ../netinet/in_pcb.c
	${C2} in_pcb.s | ${INLINE} | ${AS} -o in_pcb.o
	rm -f in_pcb.s

in_proto.o: ../netinet/in_proto.c
	${CC} -c -S ${COPTS} ../netinet/in_proto.c
	${C2} in_proto.s | ${INLINE} | ${AS} -o in_proto.o
	rm -f in_proto.s

ip_icmp.o: ../netinet/ip_icmp.c
	${CC} -c -S ${COPTS} ../netinet/ip_icmp.c
	${C2} ip_icmp.s | ${INLINE} | ${AS} -o ip_icmp.o
	rm -f ip_icmp.s

ip_input.o: ../netinet/ip_input.c
	${CC} -c -S ${COPTS} ../netinet/ip_input.c
	${C2} ip_input.s | ${INLINE} | ${AS} -o ip_input.o
	rm -f ip_input.s

ip_output.o: ../netinet/ip_output.c
	${CC} -c -S ${COPTS} ../netinet/ip_output.c
	${C2} ip_output.s | ${INLINE} | ${AS} -o ip_output.o
	rm -f ip_output.s

raw_ip.o: ../netinet/raw_ip.c
	${CC} -c -S ${COPTS} ../netinet/raw_ip.c
	${C2} raw_ip.s | ${INLINE} | ${AS} -o raw_ip.o
	rm -f raw_ip.s

tcp_debug.o: ../netinet/tcp_debug.c
	${CC} -c -S ${COPTS} ../netinet/tcp_debug.c
	${C2} tcp_debug.s | ${INLINE} | ${AS} -o tcp_debug.o
	rm -f tcp_debug.s

tcp_input.o: ../netinet/tcp_input.c
	${CC} -c -S ${COPTS} ../netinet/tcp_input.c
	${C2} tcp_input.s | ${INLINE} | ${AS} -o tcp_input.o
	rm -f tcp_input.s

tcp_output.o: ../netinet/tcp_output.c
	${CC} -c -S ${COPTS} ../netinet/tcp_output.c
	${C2} tcp_output.s | ${INLINE} | ${AS} -o tcp_output.o
	rm -f tcp_output.s

tcp_subr.o: ../netinet/tcp_subr.c
	${CC} -c -S ${COPTS} ../netinet/tcp_subr.c
	${C2} tcp_subr.s | ${INLINE} | ${AS} -o tcp_subr.o
	rm -f tcp_subr.s

tcp_timer.o: ../netinet/tcp_timer.c
	${CC} -c -S ${COPTS} ../netinet/tcp_timer.c
	${C2} tcp_timer.s | ${INLINE} | ${AS} -o tcp_timer.o
	rm -f tcp_timer.s

tcp_usrreq.o: ../netinet/tcp_usrreq.c
	${CC} -c -S ${COPTS} ../netinet/tcp_usrreq.c
	${C2} tcp_usrreq.s | ${INLINE} | ${AS} -o tcp_usrreq.o
	rm -f tcp_usrreq.s

udp_usrreq.o: ../netinet/udp_usrreq.c
	${CC} -c -S ${COPTS} ../netinet/udp_usrreq.c
	${C2} udp_usrreq.s | ${INLINE} | ${AS} -o udp_usrreq.o
	rm -f udp_usrreq.s

idp_usrreq.o: ../netns/idp_usrreq.c
	${CC} -c -S ${COPTS} ../netns/idp_usrreq.c
	${C2} idp_usrreq.s | ${INLINE} | ${AS} -o idp_usrreq.o
	rm -f idp_usrreq.s

ns.o: ../netns/ns.c
	${CC} -c -S ${COPTS} ../netns/ns.c
	${C2} ns.s | ${INLINE} | ${AS} -o ns.o
	rm -f ns.s

ns_error.o: ../netns/ns_error.c
	${CC} -c -S ${COPTS} ../netns/ns_error.c
	${C2} ns_error.s | ${INLINE} | ${AS} -o ns_error.o
	rm -f ns_error.s

ns_ip.o: ../netns/ns_ip.c
	${CC} -c -S ${COPTS} ../netns/ns_ip.c
	${C2} ns_ip.s | ${INLINE} | ${AS} -o ns_ip.o
	rm -f ns_ip.s

ns_input.o: ../netns/ns_input.c
	${CC} -c -S ${COPTS} ../netns/ns_input.c
	${C2} ns_input.s | ${INLINE} | ${AS} -o ns_input.o
	rm -f ns_input.s

ns_output.o: ../netns/ns_output.c
	${CC} -c -S ${COPTS} ../netns/ns_output.c
	${C2} ns_output.s | ${INLINE} | ${AS} -o ns_output.o
	rm -f ns_output.s

ns_pcb.o: ../netns/ns_pcb.c
	${CC} -c -S ${COPTS} ../netns/ns_pcb.c
	${C2} ns_pcb.s | ${INLINE} | ${AS} -o ns_pcb.o
	rm -f ns_pcb.s

ns_proto.o: ../netns/ns_proto.c
	${CC} -c -S ${COPTS} ../netns/ns_proto.c
	${C2} ns_proto.s | ${INLINE} | ${AS} -o ns_proto.o
	rm -f ns_proto.s

spp_debug.o: ../netns/spp_debug.c
	${CC} -c -S ${COPTS} ../netns/spp_debug.c
	${C2} spp_debug.s | ${INLINE} | ${AS} -o spp_debug.o
	rm -f spp_debug.s

spp_usrreq.o: ../netns/spp_usrreq.c
	${CC} -c -S ${COPTS} ../netns/spp_usrreq.c
	${C2} spp_usrreq.s | ${INLINE} | ${AS} -o spp_usrreq.o
	rm -f spp_usrreq.s

init_main.o: ../sys/init_main.c
	${CC} -c -S ${COPTS} ../sys/init_main.c
	${C2} init_main.s | ${INLINE} | ${AS} -o init_main.o
	rm -f init_main.s

init_sysent.o: ../sys/init_sysent.c
	${CC} -c -S ${COPTS} ../sys/init_sysent.c
	${C2} init_sysent.s | ${INLINE} | ${AS} -o init_sysent.o
	rm -f init_sysent.s

kern_acct.o: ../sys/kern_acct.c
	${CC} -c -S ${COPTS} ../sys/kern_acct.c
	${C2} kern_acct.s | ${INLINE} | ${AS} -o kern_acct.o
	rm -f kern_acct.s

kern_clock.o: ../sys/kern_clock.c
	${CC} -c -S ${COPTS} ../sys/kern_clock.c
	${C2} kern_clock.s | ${INLINE} | ${AS} -o kern_clock.o
	rm -f kern_clock.s

kern_descrip.o: ../sys/kern_descrip.c
	${CC} -c -S ${COPTS} ../sys/kern_descrip.c
	${C2} kern_descrip.s | ${INLINE} | ${AS} -o kern_descrip.o
	rm -f kern_descrip.s

kern_exec.o: ../sys/kern_exec.c
	${CC} -c -S ${COPTS} ../sys/kern_exec.c
	${C2} kern_exec.s | ${INLINE} | ${AS} -o kern_exec.o
	rm -f kern_exec.s

kern_exit.o: ../sys/kern_exit.c
	${CC} -c -S ${COPTS} ../sys/kern_exit.c
	${C2} kern_exit.s | ${INLINE} | ${AS} -o kern_exit.o
	rm -f kern_exit.s

kern_fork.o: ../sys/kern_fork.c
	${CC} -c -S ${COPTS} ../sys/kern_fork.c
	${C2} kern_fork.s | ${INLINE} | ${AS} -o kern_fork.o
	rm -f kern_fork.s

kern_mman.o: ../sys/kern_mman.c
	${CC} -c -S ${COPTS} ../sys/kern_mman.c
	${C2} kern_mman.s | ${INLINE} | ${AS} -o kern_mman.o
	rm -f kern_mman.s

kern_proc.o: ../sys/kern_proc.c
	${CC} -c -S ${COPTS} ../sys/kern_proc.c
	${C2} kern_proc.s | ${INLINE} | ${AS} -o kern_proc.o
	rm -f kern_proc.s

kern_prot.o: ../sys/kern_prot.c
	${CC} -c -S ${COPTS} ../sys/kern_prot.c
	${C2} kern_prot.s | ${INLINE} | ${AS} -o kern_prot.o
	rm -f kern_prot.s

kern_resource.o: ../sys/kern_resource.c
	${CC} -c -S ${COPTS} ../sys/kern_resource.c
	${C2} kern_resource.s | ${INLINE} | ${AS} -o kern_resource.o
	rm -f kern_resource.s

kern_sig.o: ../sys/kern_sig.c
	${CC} -c -S ${COPTS} ../sys/kern_sig.c
	${C2} kern_sig.s | ${INLINE} | ${AS} -o kern_sig.o
	rm -f kern_sig.s

kern_subr.o: ../sys/kern_subr.c
	${CC} -c -S ${COPTS} ../sys/kern_subr.c
	${C2} kern_subr.s | ${INLINE} | ${AS} -o kern_subr.o
	rm -f kern_subr.s

kern_synch.o: ../sys/kern_synch.c
	${CC} -c -S ${COPTS} ../sys/kern_synch.c
	${C2} kern_synch.s | ${INLINE} | ${AS} -o kern_synch.o
	rm -f kern_synch.s

kern_time.o: ../sys/kern_time.c
	${CC} -c -S ${COPTS} ../sys/kern_time.c
	${C2} kern_time.s | ${INLINE} | ${AS} -o kern_time.o
	rm -f kern_time.s

kern_xxx.o: ../sys/kern_xxx.c
	${CC} -c -S ${COPTS} ../sys/kern_xxx.c
	${C2} kern_xxx.s | ${INLINE} | ${AS} -o kern_xxx.o
	rm -f kern_xxx.s

quota_kern.o: ../sys/quota_kern.c
	${CC} -c -S ${COPTS} ../sys/quota_kern.c
	${C2} quota_kern.s | ${INLINE} | ${AS} -o quota_kern.o
	rm -f quota_kern.s

quota_subr.o: ../sys/quota_subr.c
	${CC} -c -S ${COPTS} ../sys/quota_subr.c
	${C2} quota_subr.s | ${INLINE} | ${AS} -o quota_subr.o
	rm -f quota_subr.s

quota_sys.o: ../sys/quota_sys.c
	${CC} -c -S ${COPTS} ../sys/quota_sys.c
	${C2} quota_sys.s | ${INLINE} | ${AS} -o quota_sys.o
	rm -f quota_sys.s

quota_ufs.o: ../sys/quota_ufs.c
	${CC} -c -S ${COPTS} ../sys/quota_ufs.c
	${C2} quota_ufs.s | ${INLINE} | ${AS} -o quota_ufs.o
	rm -f quota_ufs.s

subr_log.o: ../sys/subr_log.c
	${CC} -c -S ${COPTS} ../sys/subr_log.c
	${C2} subr_log.s | ${INLINE} | ${AS} -o subr_log.o
	rm -f subr_log.s

subr_prf.o: ../sys/subr_prf.c
	${CC} -c -S ${COPTS} ../sys/subr_prf.c
	${C2} subr_prf.s | ${INLINE} | ${AS} -o subr_prf.o
	rm -f subr_prf.s

subr_rmap.o: ../sys/subr_rmap.c
	${CC} -c -S ${COPTS} ../sys/subr_rmap.c
	${C2} subr_rmap.s | ${INLINE} | ${AS} -o subr_rmap.o
	rm -f subr_rmap.s

subr_xxx.o: ../sys/subr_xxx.c
	${CC} -c -S ${COPTS} ../sys/subr_xxx.c
	${C2} subr_xxx.s | ${INLINE} | ${AS} -o subr_xxx.o
	rm -f subr_xxx.s

sys_generic.o: ../sys/sys_generic.c
	${CC} -c -S ${COPTS} ../sys/sys_generic.c
	${C2} sys_generic.s | ${INLINE} | ${AS} -o sys_generic.o
	rm -f sys_generic.s

sys_inode.o: ../sys/sys_inode.c
	${CC} -c -S ${COPTS} ../sys/sys_inode.c
	${C2} sys_inode.s | ${INLINE} | ${AS} -o sys_inode.o
	rm -f sys_inode.s

sys_process.o: ../sys/sys_process.c
	${CC} -c -S ${COPTS} ../sys/sys_process.c
	${C2} sys_process.s | ${INLINE} | ${AS} -o sys_process.o
	rm -f sys_process.s

sys_socket.o: ../sys/sys_socket.c
	${CC} -c -S ${COPTS} ../sys/sys_socket.c
	${C2} sys_socket.s | ${INLINE} | ${AS} -o sys_socket.o
	rm -f sys_socket.s

tty.o: ../sys/tty.c
	${CC} -c -S ${COPTS} ../sys/tty.c
	${C2} tty.s | ${INLINE} | ${AS} -o tty.o
	rm -f tty.s

tty_conf.o: ../sys/tty_conf.c
	${CC} -c -S ${COPTS} ../sys/tty_conf.c
	${C2} tty_conf.s | ${INLINE} | ${AS} -o tty_conf.o
	rm -f tty_conf.s

tty_pty.o: ../sys/tty_pty.c
	${CC} -c -S ${COPTS} ../sys/tty_pty.c
	${C2} tty_pty.s | ${INLINE} | ${AS} -o tty_pty.o
	rm -f tty_pty.s

tty_subr.o: ../sys/tty_subr.c
	${CC} -c -S ${COPTS} ../sys/tty_subr.c
	${C2} tty_subr.s | ${INLINE} | ${AS} -o tty_subr.o
	rm -f tty_subr.s

tty_tty.o: ../sys/tty_tty.c
	${CC} -c -S ${COPTS} ../sys/tty_tty.c
	${C2} tty_tty.s | ${INLINE} | ${AS} -o tty_tty.o
	rm -f tty_tty.s

ufs_alloc.o: ../sys/ufs_alloc.c
	${CC} -c -S ${COPTS} ../sys/ufs_alloc.c
	${C2} ufs_alloc.s | ${INLINE} | ${AS} -o ufs_alloc.o
	rm -f ufs_alloc.s

ufs_bio.o: ../sys/ufs_bio.c
	${CC} -c -S ${COPTS} ../sys/ufs_bio.c
	${C2} ufs_bio.s | ${INLINE} | ${AS} -o ufs_bio.o
	rm -f ufs_bio.s

ufs_bmap.o: ../sys/ufs_bmap.c
	${CC} -c -S ${COPTS} ../sys/ufs_bmap.c
	${C2} ufs_bmap.s | ${INLINE} | ${AS} -o ufs_bmap.o
	rm -f ufs_bmap.s

ufs_dsort.o: ../sys/ufs_dsort.c
	${CC} -c -S ${COPTS} ../sys/ufs_dsort.c
	${C2} ufs_dsort.s | ${INLINE} | ${AS} -o ufs_dsort.o
	rm -f ufs_dsort.s

ufs_fio.o: ../sys/ufs_fio.c
	${CC} -c -S ${COPTS} ../sys/ufs_fio.c
	${C2} ufs_fio.s | ${INLINE} | ${AS} -o ufs_fio.o
	rm -f ufs_fio.s

ufs_inode.o: ../sys/ufs_inode.c
	${CC} -c -S ${COPTS} ../sys/ufs_inode.c
	${C2} ufs_inode.s | ${INLINE} | ${AS} -o ufs_inode.o
	rm -f ufs_inode.s

ufs_mount.o: ../sys/ufs_mount.c
	${CC} -c -S ${COPTS} ../sys/ufs_mount.c
	${C2} ufs_mount.s | ${INLINE} | ${AS} -o ufs_mount.o
	rm -f ufs_mount.s

ufs_namei.o: ../sys/ufs_namei.c
	${CC} -c -S ${COPTS} ../sys/ufs_namei.c
	${C2} ufs_namei.s | ${INLINE} | ${AS} -o ufs_namei.o
	rm -f ufs_namei.s

ufs_subr.o: ../sys/ufs_subr.c
	${CC} -c -S ${COPTS} ../sys/ufs_subr.c
	${C2} ufs_subr.s | ${INLINE} | ${AS} -o ufs_subr.o
	rm -f ufs_subr.s

ufs_syscalls.o: ../sys/ufs_syscalls.c
	${CC} -c -S ${COPTS} ../sys/ufs_syscalls.c
	${C2} ufs_syscalls.s | ${INLINE} | ${AS} -o ufs_syscalls.o
	rm -f ufs_syscalls.s

ufs_tables.o: ../sys/ufs_tables.c
	${CC} -c -S ${COPTS} ../sys/ufs_tables.c
	${C2} ufs_tables.s | ${INLINE} | ${AS} -o ufs_tables.o
	rm -f ufs_tables.s

ufs_xxx.o: ../sys/ufs_xxx.c
	${CC} -c -S ${COPTS} ../sys/ufs_xxx.c
	${C2} ufs_xxx.s | ${INLINE} | ${AS} -o ufs_xxx.o
	rm -f ufs_xxx.s

uipc_domain.o: ../sys/uipc_domain.c
	${CC} -c -S ${COPTS} ../sys/uipc_domain.c
	${C2} uipc_domain.s | ${INLINE} | ${AS} -o uipc_domain.o
	rm -f uipc_domain.s

uipc_mbuf.o: ../sys/uipc_mbuf.c
	${CC} -c -S ${COPTS} ../sys/uipc_mbuf.c
	${C2} uipc_mbuf.s | ${INLINE} | ${AS} -o uipc_mbuf.o
	rm -f uipc_mbuf.s

uipc_proto.o: ../sys/uipc_proto.c
	${CC} -c -S ${COPTS} ../sys/uipc_proto.c
	${C2} uipc_proto.s | ${INLINE} | ${AS} -o uipc_proto.o
	rm -f uipc_proto.s

uipc_socket.o: ../sys/uipc_socket.c
	${CC} -c -S ${COPTS} ../sys/uipc_socket.c
	${C2} uipc_socket.s | ${INLINE} | ${AS} -o uipc_socket.o
	rm -f uipc_socket.s

uipc_socket2.o: ../sys/uipc_socket2.c
	${CC} -c -S ${COPTS} ../sys/uipc_socket2.c
	${C2} uipc_socket2.s | ${INLINE} | ${AS} -o uipc_socket2.o
	rm -f uipc_socket2.s

uipc_syscalls.o: ../sys/uipc_syscalls.c
	${CC} -c -S ${COPTS} ../sys/uipc_syscalls.c
	${C2} uipc_syscalls.s | ${INLINE} | ${AS} -o uipc_syscalls.o
	rm -f uipc_syscalls.s

uipc_usrreq.o: ../sys/uipc_usrreq.c
	${CC} -c -S ${COPTS} ../sys/uipc_usrreq.c
	${C2} uipc_usrreq.s | ${INLINE} | ${AS} -o uipc_usrreq.o
	rm -f uipc_usrreq.s

vm_drum.o: ../sys/vm_drum.c
	${CC} -c -S ${COPTS} ../sys/vm_drum.c
	${C2} vm_drum.s | ${INLINE} | ${AS} -o vm_drum.o
	rm -f vm_drum.s

vm_mem.o: ../sys/vm_mem.c
	${CC} -c -S ${COPTS} ../sys/vm_mem.c
	${C2} vm_mem.s | ${INLINE} | ${AS} -o vm_mem.o
	rm -f vm_mem.s

vm_mon.o: ../sys/vm_mon.c
	${CC} -c -S ${COPTS} ../sys/vm_mon.c
	${C2} vm_mon.s | ${INLINE} | ${AS} -o vm_mon.o
	rm -f vm_mon.s

vm_page.o: ../sys/vm_page.c
	${CC} -c -S ${COPTS} ../sys/vm_page.c
	${C2} vm_page.s | ${INLINE} | ${AS} -o vm_page.o
	rm -f vm_page.s

vm_proc.o: ../sys/vm_proc.c
	${CC} -c -S ${COPTS} ../sys/vm_proc.c
	${C2} vm_proc.s | ${INLINE} | ${AS} -o vm_proc.o
	rm -f vm_proc.s

vm_pt.o: ../sys/vm_pt.c
	${CC} -c -S ${COPTS} ../sys/vm_pt.c
	${C2} vm_pt.s | ${INLINE} | ${AS} -o vm_pt.o
	rm -f vm_pt.s

vm_sched.o: ../sys/vm_sched.c
	${CC} -c -S ${COPTS} ../sys/vm_sched.c
	${C2} vm_sched.s | ${INLINE} | ${AS} -o vm_sched.o
	rm -f vm_sched.s

vm_subr.o: ../sys/vm_subr.c
	${CC} -c -S ${COPTS} ../sys/vm_subr.c
	${C2} vm_subr.s | ${INLINE} | ${AS} -o vm_subr.o
	rm -f vm_subr.s

vm_sw.o: ../sys/vm_sw.c
	${CC} -c -S ${COPTS} ../sys/vm_sw.c
	${C2} vm_sw.s | ${INLINE} | ${AS} -o vm_sw.o
	rm -f vm_sw.s

vm_swap.o: ../sys/vm_swap.c
	${CC} -c -S ${COPTS} ../sys/vm_swap.c
	${C2} vm_swap.s | ${INLINE} | ${AS} -o vm_swap.o
	rm -f vm_swap.s

vm_swp.o: ../sys/vm_swp.c
	${CC} -c -S ${COPTS} ../sys/vm_swp.c
	${C2} vm_swp.s | ${INLINE} | ${AS} -o vm_swp.o
	rm -f vm_swp.s

vm_text.o: ../sys/vm_text.c
	${CC} -c -S ${COPTS} ../sys/vm_text.c
	${C2} vm_text.s | ${INLINE} | ${AS} -o vm_text.o
	rm -f vm_text.s

autoconf.o: ../vax/autoconf.c
	${CC} -c -S ${COPTS} ../vax/autoconf.c
	${C2} -i autoconf.s | ${INLINE} | ${AS} -o autoconf.o
	rm -f autoconf.s

clock.o: ../vax/clock.c
	${CC} -c -S ${COPTS} ../vax/clock.c
	${C2} clock.s | ${INLINE} | ${AS} -o clock.o
	rm -f clock.s

conf.o: ../vax/conf.c
	${CC} -c -S ${COPTS} ../vax/conf.c
	${C2} conf.s | ${INLINE} | ${AS} -o conf.o
	rm -f conf.s

cons.o: ../vax/cons.c
	${CC} -c -S ${COPTS} ../vax/cons.c
	${C2} cons.s | ${INLINE} | ${AS} -o cons.o
	rm -f cons.s

cpudata.o: ../vax/cpudata.c
	${CC} -c -S ${COPTS} ../vax/cpudata.c
	${C2} cpudata.s | ${INLINE} | ${AS} -o cpudata.o
	rm -f cpudata.s

crl.o: ../vax/crl.c
	${CC} -c -S ${COPTS} ../vax/crl.c
	${C2} crl.s | ${INLINE} | ${AS} -o crl.o
	rm -f crl.s

dkbad.o: ../vax/dkbad.c
	${CC} -c -S ${COPTS} ../vax/dkbad.c
	${C2} dkbad.s | ${INLINE} | ${AS} -o dkbad.o
	rm -f dkbad.s

flp.o: ../vax/flp.c
	${CC} -c -S ${COPTS} ../vax/flp.c
	${C2} flp.s | ${INLINE} | ${AS} -o flp.o
	rm -f flp.s

in_cksum.o: ../vax/in_cksum.c
	${CC} -c -S ${COPTS} ../vax/in_cksum.c
	${C2} in_cksum.s | ${INLINE} | ${AS} -o in_cksum.o
	rm -f in_cksum.s

ka630.o: ../vax/ka630.c
	${CC} -c -S ${COPTS} ../vax/ka630.c
	${C2} ka630.s | ${INLINE} | ${AS} -o ka630.o
	rm -f ka630.s

ns_cksum.o: ../vax/ns_cksum.c
	${CC} -c -S ${COPTS} ../vax/ns_cksum.c
	${C2} ns_cksum.s | ${INLINE} | ${AS} -o ns_cksum.o
	rm -f ns_cksum.s

machdep.o: ../vax/machdep.c
	${CC} -c -S ${COPTS} ${PARAM} ../vax/machdep.c
	${C2} machdep.s | ${INLINE} | ${AS} -o machdep.o
	rm -f machdep.s

mem.o: ../vax/mem.c
	${CC} -c -S ${COPTS} ../vax/mem.c
	${C2} mem.s | ${INLINE} | ${AS} -o mem.o
	rm -f mem.s

sys_machdep.o: ../vax/sys_machdep.c
	${CC} -c -S ${COPTS} ../vax/sys_machdep.c
	${C2} sys_machdep.s | ${INLINE} | ${AS} -o sys_machdep.o
	rm -f sys_machdep.s

trap.o: ../vax/trap.c
	${CC} -c -S ${COPTS} ../vax/trap.c
	${C2} trap.s | ${INLINE} | ${AS} -o trap.o
	rm -f trap.s

tu.o: ../vax/tu.c
	${CC} -c -S ${COPTS} ../vax/tu.c
	${C2} tu.s | ${INLINE} | ${AS} -o tu.o
	rm -f tu.s

udiv.o: ../vax/udiv.s
	-ln -s ../vax/udiv.s udiv.c
	${CC} -E ${COPTS} udiv.c | ${AS} -o udiv.o
	rm -f udiv.c

ufs_machdep.o: ../vax/ufs_machdep.c
	${CC} -c -S ${COPTS} ../vax/ufs_machdep.c
	${C2} ufs_machdep.s | ${INLINE} | ${AS} -o ufs_machdep.o
	rm -f ufs_machdep.s

urem.o: ../vax/urem.s
	-ln -s ../vax/urem.s urem.c
	${CC} -E ${COPTS} urem.c | ${AS} -o urem.o
	rm -f urem.c

vm_machdep.o: ../vax/vm_machdep.c
	${CC} -c -S ${COPTS} ../vax/vm_machdep.c
	${C2} vm_machdep.s | ${INLINE} | ${AS} -o vm_machdep.o
	rm -f vm_machdep.s

if_de.o: ../vaxif/if_de.c
	${CC} -c -S ${COPTS} ../vaxif/if_de.c
	${C2} -i if_de.s | ${INLINE} | ${AS} -o if_de.o
	rm -f if_de.s

if_ec.o: ../vaxif/if_ec.c
	${CC} -c -S ${COPTS} ../vaxif/if_ec.c
	${C2} -i if_ec.s | ${INLINE} | ${AS} -o if_ec.o
	rm -f if_ec.s

if_en.o: ../vaxif/if_en.c
	${CC} -c -S ${COPTS} ../vaxif/if_en.c
	${C2} -i if_en.s | ${INLINE} | ${AS} -o if_en.o
	rm -f if_en.s

if_ex.o: ../vaxif/if_ex.c
	${CC} -c -S ${COPTS} ../vaxif/if_ex.c
	${C2} -i if_ex.s | ${INLINE} | ${AS} -o if_ex.o
	rm -f if_ex.s

if_il.o: ../vaxif/if_il.c
	${CC} -c -S ${COPTS} ../vaxif/if_il.c
	${C2} -i if_il.s | ${INLINE} | ${AS} -o if_il.o
	rm -f if_il.s

if_uba.o: ../vaxif/if_uba.c
	${CC} -c -S ${COPTS} ../vaxif/if_uba.c
	${C2} -i if_uba.s | ${INLINE} | ${AS} -o if_uba.o
	rm -f if_uba.s

hp.o: ../vaxmba/hp.c
	${CC} -c -S ${COPTS} ../vaxmba/hp.c
	${C2} -i hp.s | ${INLINE} | ${AS} -o hp.o
	rm -f hp.s

ht.o: ../vaxmba/ht.c
	${CC} -c -S ${COPTS} ../vaxmba/ht.c
	${C2} -i ht.s | ${INLINE} | ${AS} -o ht.o
	rm -f ht.s

mba.o: ../vaxmba/mba.c
	${CC} -c -S ${COPTS} ../vaxmba/mba.c
	${C2} -i mba.s | ${INLINE} | ${AS} -o mba.o
	rm -f mba.s

mt.o: ../vaxmba/mt.c
	${CC} -c -S ${COPTS} ../vaxmba/mt.c
	${C2} -i mt.s | ${INLINE} | ${AS} -o mt.o
	rm -f mt.s

dh.o: ../vaxuba/dh.c
	${CC} -c -S ${COPTS} ../vaxuba/dh.c
	${C2} -i dh.s | ${INLINE} | ${AS} -o dh.o
	rm -f dh.s

dhu.o: ../vaxuba/dhu.c
	${CC} -c -S ${COPTS} ../vaxuba/dhu.c
	${C2} -i dhu.s | ${INLINE} | ${AS} -o dhu.o
	rm -f dhu.s

dmf.o: ../vaxuba/dmf.c
	${CC} -c -S ${COPTS} ../vaxuba/dmf.c
	${C2} -i dmf.s | ${INLINE} | ${AS} -o dmf.o
	rm -f dmf.s

dmz.o: ../vaxuba/dmz.c
	${CC} -c -S ${COPTS} ../vaxuba/dmz.c
	${C2} -i dmz.s | ${INLINE} | ${AS} -o dmz.o
	rm -f dmz.s

dz.o: ../vaxuba/dz.c
	${CC} -c -S ${COPTS} ../vaxuba/dz.c
	${C2} -i dz.s | ${INLINE} | ${AS} -o dz.o
	rm -f dz.s

idc.o: ../vaxuba/idc.c
	${CC} -c -S ${COPTS} ../vaxuba/idc.c
	${C2} -i idc.s | ${INLINE} | ${AS} -o idc.o
	rm -f idc.s

lp.o: ../vaxuba/lp.c
	${CC} -c -S ${COPTS} ../vaxuba/lp.c
	${C2} -i lp.s | ${INLINE} | ${AS} -o lp.o
	rm -f lp.s

rk.o: ../vaxuba/rk.c
	${CC} -c -S ${COPTS} ../vaxuba/rk.c
	${C2} -i rk.s | ${INLINE} | ${AS} -o rk.o
	rm -f rk.s

rl.o: ../vaxuba/rl.c
	${CC} -c -S ${COPTS} ../vaxuba/rl.c
	${C2} -i rl.s | ${INLINE} | ${AS} -o rl.o
	rm -f rl.s

tm.o: ../vaxuba/tm.c
	${CC} -c -S ${COPTS} ../vaxuba/tm.c
	${C2} -i tm.s | ${INLINE} | ${AS} -o tm.o
	rm -f tm.s

tmscp.o: ../vaxuba/tmscp.c
	${CC} -c -S ${COPTS} ../vaxuba/tmscp.c
	${C2} -i tmscp.s | ${INLINE} | ${AS} -o tmscp.o
	rm -f tmscp.s

ts.o: ../vaxuba/ts.c
	${CC} -c -S ${COPTS} ../vaxuba/ts.c
	${C2} -i ts.s | ${INLINE} | ${AS} -o ts.o
	rm -f ts.s

uba.o: ../vaxuba/uba.c
	${CC} -c -S ${COPTS} ../vaxuba/uba.c
	${C2} -i uba.s | ${INLINE} | ${AS} -o uba.o
	rm -f uba.s

uda.o: ../vaxuba/uda.c
	${CC} -c -S ${COPTS} ../vaxuba/uda.c
	${C2} -i uda.s | ${INLINE} | ${AS} -o uda.o
	rm -f uda.s

up.o: ../vaxuba/up.c
	${CC} -c -S ${COPTS} ../vaxuba/up.c
	${C2} -i up.s | ${INLINE} | ${AS} -o up.o
	rm -f up.s

ut.o: ../vaxuba/ut.c
	${CC} -c -S ${COPTS} ../vaxuba/ut.c
	${C2} -i ut.s | ${INLINE} | ${AS} -o ut.o
	rm -f ut.s


# DO NOT DELETE THIS LINE -- make depend uses it

af.o: ../net/af.c ../h/param.h ../machine/machparam.h ../h/signal.h
af.o: ../h/types.h ../h/mbuf.h ../h/protosw.h ../h/socket.h ../h/socketvar.h
af.o: ../net/af.h
if.o: ../net/if.c ../h/param.h ../machine/machparam.h ../h/signal.h
if.o: ../h/types.h ../h/systm.h ../h/socket.h ../h/socketvar.h ../h/protosw.h
if.o: ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
if.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/kernel.h
if.o: ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/errno.h ../net/if.h
if.o: ../net/if_arp.h ../net/af.h ./ether.h
if_loop.o: ../net/if_loop.c ../h/param.h ../machine/machparam.h ../h/signal.h
if_loop.o: ../h/types.h ../h/systm.h ../h/mbuf.h ../h/socket.h ../h/errno.h
if_loop.o: ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../net/if.h
if_loop.o: ../net/if_arp.h ../net/netisr.h ../net/route.h ../netinet/in.h
if_loop.o: ../netinet/in_systm.h ../netinet/in_var.h ../netinet/ip.h
if_loop.o: ../vax/mtpr.h ../netns/ns.h ../netns/ns_if.h
raw_cb.o: ../net/raw_cb.c ../h/param.h ../machine/machparam.h ../h/signal.h
raw_cb.o: ../h/types.h ../h/systm.h ../h/mbuf.h ../h/socket.h ../h/socketvar.h
raw_cb.o: ../h/domain.h ../h/protosw.h ../h/errno.h ../net/if.h ../net/if_arp.h
raw_cb.o: ../net/route.h ../net/raw_cb.h ../netinet/in.h ../vax/mtpr.h
raw_usrreq.o: ../net/raw_usrreq.c ../h/param.h ../machine/machparam.h
raw_usrreq.o: ../h/signal.h ../h/types.h ../h/mbuf.h ../h/domain.h
raw_usrreq.o: ../h/protosw.h ../h/socket.h ../h/socketvar.h ../h/errno.h
raw_usrreq.o: ../net/if.h ../net/if_arp.h ../net/route.h ../net/netisr.h
raw_usrreq.o: ../net/raw_cb.h ../vax/mtpr.h
route.o: ../net/route.c ../h/param.h ../machine/machparam.h ../h/signal.h
route.o: ../h/types.h ../h/systm.h ../h/mbuf.h ../h/protosw.h ../h/socket.h
route.o: ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
route.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/ioctl.h
route.o: ../h/ttychars.h ../h/ttydev.h ../h/errno.h ../net/if.h ../net/if_arp.h
route.o: ../net/af.h ../net/route.h
if_ether.o: ../netinet/if_ether.c ../h/param.h ../machine/machparam.h
if_ether.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/mbuf.h ../h/socket.h
if_ether.o: ../h/time.h ../h/kernel.h ../h/errno.h ../h/ioctl.h ../h/ttychars.h
if_ether.o: ../h/ttydev.h ../h/syslog.h ../net/if.h ../net/if_arp.h
if_ether.o: ../netinet/in.h ../netinet/in_systm.h ../netinet/ip.h
if_ether.o: ../netinet/if_ether.h
in.o: ../netinet/in.c ../h/param.h ../machine/machparam.h ../h/signal.h
in.o: ../h/types.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/mbuf.h
in.o: ../h/protosw.h ../h/socket.h ../h/socketvar.h ../h/uio.h ../h/dir.h
in.o: ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
in.o: ../h/namei.h ../h/uio.h ../h/errno.h ../netinet/in_systm.h ../net/if.h
in.o: ../net/if_arp.h ../net/route.h ../net/af.h ../netinet/in.h
in.o: ../netinet/in_var.h
in_pcb.o: ../netinet/in_pcb.c ../h/param.h ../machine/machparam.h ../h/signal.h
in_pcb.o: ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h ../machine/pcb.h
in_pcb.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
in_pcb.o: ../h/errno.h ../h/mbuf.h ../h/socket.h ../h/socketvar.h ../h/ioctl.h
in_pcb.o: ../h/ttychars.h ../h/ttydev.h ../netinet/in.h ../netinet/in_systm.h
in_pcb.o: ../net/if.h ../net/if_arp.h ../net/route.h ../netinet/in_pcb.h
in_pcb.o: ../netinet/in_var.h ../h/protosw.h
in_proto.o: ../netinet/in_proto.c ../h/param.h ../machine/machparam.h
in_proto.o: ../h/signal.h ../h/types.h ../h/socket.h ../h/protosw.h
in_proto.o: ../h/domain.h ../h/mbuf.h ../netinet/in.h ../netinet/in_systm.h
in_proto.o: ./imp.h ./hy.h
ip_icmp.o: ../netinet/ip_icmp.c ../h/param.h ../machine/machparam.h
ip_icmp.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/mbuf.h ../h/protosw.h
ip_icmp.o: ../h/socket.h ../h/time.h ../h/kernel.h ../net/route.h ../net/if.h
ip_icmp.o: ../net/if_arp.h ../netinet/in.h ../netinet/in_systm.h
ip_icmp.o: ../netinet/in_var.h ../netinet/ip.h ../netinet/ip_icmp.h
ip_icmp.o: ../netinet/icmp_var.h
ip_input.o: ../netinet/ip_input.c ../h/param.h ../machine/machparam.h
ip_input.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/mbuf.h ../h/domain.h
ip_input.o: ../h/protosw.h ../h/socket.h ../h/errno.h ../h/time.h ../h/kernel.h
ip_input.o: ../net/if.h ../net/if_arp.h ../net/route.h ../netinet/in.h
ip_input.o: ../netinet/in_pcb.h ../netinet/in_systm.h ../netinet/in_var.h
ip_input.o: ../netinet/ip.h ../netinet/ip_var.h ../netinet/ip_icmp.h
ip_input.o: ../netinet/tcp.h
ip_output.o: ../netinet/ip_output.c ../h/param.h ../machine/machparam.h
ip_output.o: ../h/signal.h ../h/types.h ../h/mbuf.h ../h/errno.h ../h/protosw.h
ip_output.o: ../h/socket.h ../h/socketvar.h ../net/if.h ../net/if_arp.h
ip_output.o: ../net/route.h ../netinet/in.h ../netinet/in_pcb.h
ip_output.o: ../netinet/in_systm.h ../netinet/in_var.h ../netinet/ip.h
ip_output.o: ../netinet/ip_var.h ../vax/mtpr.h
raw_ip.o: ../netinet/raw_ip.c ../h/param.h ../machine/machparam.h ../h/signal.h
raw_ip.o: ../h/types.h ../h/mbuf.h ../h/socket.h ../h/protosw.h
raw_ip.o: ../h/socketvar.h ../h/errno.h ../net/if.h ../net/if_arp.h
raw_ip.o: ../net/route.h ../net/raw_cb.h ../netinet/in.h ../netinet/in_systm.h
raw_ip.o: ../netinet/ip.h ../netinet/ip_var.h
tcp_debug.o: ../netinet/tcp_debug.c ../h/param.h ../machine/machparam.h
tcp_debug.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/mbuf.h ../h/socket.h
tcp_debug.o: ../h/socketvar.h ../h/protosw.h ../h/errno.h ../net/route.h
tcp_debug.o: ../net/if.h ../net/if_arp.h ../netinet/in.h ../netinet/in_pcb.h
tcp_debug.o: ../netinet/in_systm.h ../netinet/ip.h ../netinet/ip_var.h
tcp_debug.o: ../netinet/tcp.h ../netinet/tcp_fsm.h ../netinet/tcp_seq.h
tcp_debug.o: ../netinet/tcp_timer.h ../netinet/tcp_var.h ../netinet/tcpip.h
tcp_debug.o: ../netinet/tcp_debug.h
tcp_input.o: ../netinet/tcp_input.c ../h/param.h ../machine/machparam.h
tcp_input.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/mbuf.h ../h/protosw.h
tcp_input.o: ../h/socket.h ../h/socketvar.h ../h/errno.h ../net/if.h
tcp_input.o: ../net/if_arp.h ../net/route.h ../netinet/in.h ../netinet/in_pcb.h
tcp_input.o: ../netinet/in_systm.h ../netinet/ip.h ../netinet/ip_var.h
tcp_input.o: ../netinet/tcp.h ../netinet/tcp_fsm.h ../netinet/tcp_seq.h
tcp_input.o: ../netinet/tcp_timer.h ../netinet/tcp_var.h ../netinet/tcpip.h
tcp_input.o: ../netinet/tcp_debug.h
tcp_output.o: ../netinet/tcp_output.c ../h/param.h ../machine/machparam.h
tcp_output.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/mbuf.h
tcp_output.o: ../h/protosw.h ../h/socket.h ../h/socketvar.h ../h/errno.h
tcp_output.o: ../net/route.h ../netinet/in.h ../netinet/in_pcb.h
tcp_output.o: ../netinet/in_systm.h ../netinet/ip.h ../netinet/ip_var.h
tcp_output.o: ../netinet/tcp.h ../netinet/tcp_fsm.h ../netinet/tcp_seq.h
tcp_output.o: ../netinet/tcp_timer.h ../netinet/tcp_var.h ../netinet/tcpip.h
tcp_output.o: ../netinet/tcp_debug.h
tcp_subr.o: ../netinet/tcp_subr.c ../h/param.h ../machine/machparam.h
tcp_subr.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/mbuf.h ../h/socket.h
tcp_subr.o: ../h/socketvar.h ../h/protosw.h ../h/errno.h ../net/route.h
tcp_subr.o: ../net/if.h ../net/if_arp.h ../netinet/in.h ../netinet/in_pcb.h
tcp_subr.o: ../netinet/in_systm.h ../netinet/ip.h ../netinet/ip_var.h
tcp_subr.o: ../netinet/ip_icmp.h ../netinet/tcp.h ../netinet/tcp_fsm.h
tcp_subr.o: ../netinet/tcp_seq.h ../netinet/tcp_timer.h ../netinet/tcp_var.h
tcp_subr.o: ../netinet/tcpip.h
tcp_timer.o: ../netinet/tcp_timer.c ../h/param.h ../machine/machparam.h
tcp_timer.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/mbuf.h ../h/socket.h
tcp_timer.o: ../h/socketvar.h ../h/protosw.h ../h/errno.h ../net/if.h
tcp_timer.o: ../net/if_arp.h ../net/route.h ../netinet/in.h ../netinet/in_pcb.h
tcp_timer.o: ../netinet/in_systm.h ../netinet/ip.h ../netinet/ip_var.h
tcp_timer.o: ../netinet/tcp.h ../netinet/tcp_fsm.h ../netinet/tcp_seq.h
tcp_timer.o: ../netinet/tcp_timer.h ../netinet/tcp_var.h ../netinet/tcpip.h
tcp_usrreq.o: ../netinet/tcp_usrreq.c ../h/param.h ../machine/machparam.h
tcp_usrreq.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/mbuf.h ../h/socket.h
tcp_usrreq.o: ../h/socketvar.h ../h/protosw.h ../h/errno.h ../h/stat.h
tcp_usrreq.o: ../net/if.h ../net/if_arp.h ../net/route.h ../netinet/in.h
tcp_usrreq.o: ../netinet/in_pcb.h ../netinet/in_systm.h ../netinet/ip.h
tcp_usrreq.o: ../netinet/ip_var.h ../netinet/tcp.h ../netinet/tcp_fsm.h
tcp_usrreq.o: ../netinet/tcp_seq.h ../netinet/tcp_timer.h ../netinet/tcp_var.h
tcp_usrreq.o: ../netinet/tcpip.h ../netinet/tcp_debug.h
udp_usrreq.o: ../netinet/udp_usrreq.c ../h/param.h ../machine/machparam.h
udp_usrreq.o: ../h/signal.h ../h/types.h ../h/dir.h ../h/user.h
udp_usrreq.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
udp_usrreq.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/mbuf.h ../h/protosw.h
udp_usrreq.o: ../h/socket.h ../h/socketvar.h ../h/errno.h ../h/stat.h
udp_usrreq.o: ../net/if.h ../net/if_arp.h ../net/route.h ../netinet/in.h
udp_usrreq.o: ../netinet/in_pcb.h ../netinet/in_systm.h ../netinet/ip.h
udp_usrreq.o: ../netinet/ip_var.h ../netinet/ip_icmp.h ../netinet/udp.h
udp_usrreq.o: ../netinet/udp_var.h
idp_usrreq.o: ../netns/idp_usrreq.c ../h/param.h ../machine/machparam.h
idp_usrreq.o: ../h/signal.h ../h/types.h ../h/dir.h ../h/user.h
idp_usrreq.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
idp_usrreq.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/mbuf.h ../h/protosw.h
idp_usrreq.o: ../h/socket.h ../h/socketvar.h ../h/errno.h ../h/stat.h
idp_usrreq.o: ../net/if.h ../net/if_arp.h ../net/route.h ../netns/ns.h
idp_usrreq.o: ../netns/ns_pcb.h ../netns/ns_if.h ../netns/idp.h
idp_usrreq.o: ../netns/idp_var.h ../netns/ns_error.h
ns.o: ../netns/ns.c ../h/param.h ../machine/machparam.h ../h/signal.h
ns.o: ../h/types.h ../h/mbuf.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h
ns.o: ../h/protosw.h ../h/socket.h ../h/socketvar.h ../h/uio.h ../h/dir.h
ns.o: ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
ns.o: ../h/namei.h ../h/uio.h ../h/errno.h ../net/if.h ../net/if_arp.h
ns.o: ../net/route.h ../net/af.h ../netns/ns.h ../netns/ns_if.h
ns_error.o: ../netns/ns_error.c ../h/param.h ../machine/machparam.h
ns_error.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/mbuf.h ../h/protosw.h
ns_error.o: ../h/socket.h ../h/time.h ../h/kernel.h ../net/route.h
ns_error.o: ../netns/ns.h ../netns/ns_pcb.h ../netns/idp.h ../netns/ns_error.h
ns_ip.o: ../netns/ns_ip.c
ns_input.o: ../netns/ns_input.c ../h/param.h ../machine/machparam.h
ns_input.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/mbuf.h ../h/domain.h
ns_input.o: ../h/protosw.h ../h/socket.h ../h/socketvar.h ../h/errno.h
ns_input.o: ../h/time.h ../h/kernel.h ../net/if.h ../net/if_arp.h
ns_input.o: ../net/route.h ../net/raw_cb.h ../netns/ns.h ../netns/ns_if.h
ns_input.o: ../netns/ns_pcb.h ../netns/idp.h ../netns/idp_var.h
ns_input.o: ../netns/ns_error.h
ns_output.o: ../netns/ns_output.c ../h/param.h ../machine/machparam.h
ns_output.o: ../h/signal.h ../h/types.h ../h/mbuf.h ../h/errno.h ../h/socket.h
ns_output.o: ../h/socketvar.h ../net/if.h ../net/if_arp.h ../net/route.h
ns_output.o: ../netns/ns.h ../netns/ns_if.h ../netns/idp.h ../netns/idp_var.h
ns_output.o: ../vax/mtpr.h
ns_pcb.o: ../netns/ns_pcb.c ../h/param.h ../machine/machparam.h ../h/signal.h
ns_pcb.o: ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h ../machine/pcb.h
ns_pcb.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
ns_pcb.o: ../h/errno.h ../h/mbuf.h ../h/socket.h ../h/socketvar.h ../net/if.h
ns_pcb.o: ../net/if_arp.h ../net/route.h ../h/protosw.h ../netns/ns.h
ns_pcb.o: ../netns/ns_if.h ../netns/ns_pcb.h
ns_proto.o: ../netns/ns_proto.c ../h/param.h ../machine/machparam.h
ns_proto.o: ../h/signal.h ../h/types.h ../h/socket.h ../h/protosw.h
ns_proto.o: ../h/domain.h ../h/mbuf.h ../netns/ns.h
spp_debug.o: ../netns/spp_debug.c ../h/param.h ../machine/machparam.h
spp_debug.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/mbuf.h ../h/socket.h
spp_debug.o: ../h/socketvar.h ../h/protosw.h ../h/errno.h ../net/route.h
spp_debug.o: ../net/if.h ../net/if_arp.h ../netinet/tcp_fsm.h
spp_debug.o: ../netinet/tcp_timer.h ../netns/ns.h ../netns/ns_pcb.h
spp_debug.o: ../netns/idp.h ../netns/idp_var.h ../netns/sp.h ../netns/spidp.h
spp_debug.o: ../netns/spp_var.h ../netns/spp_debug.h
spp_usrreq.o: ../netns/spp_usrreq.c ../h/param.h ../machine/machparam.h
spp_usrreq.o: ../h/signal.h ../h/types.h ../h/dir.h ../h/user.h
spp_usrreq.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
spp_usrreq.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/mbuf.h ../h/protosw.h
spp_usrreq.o: ../h/socket.h ../h/socketvar.h ../h/errno.h ../net/if.h
spp_usrreq.o: ../net/if_arp.h ../net/route.h ../netinet/tcp_fsm.h
spp_usrreq.o: ../netinet/tcp_timer.h ../netns/ns.h ../netns/ns_pcb.h
spp_usrreq.o: ../netns/idp.h ../netns/idp_var.h ../netns/ns_error.h
spp_usrreq.o: ../netns/sp.h ../netns/spidp.h ../netns/spp_var.h
spp_usrreq.o: ../netns/spp_debug.h
init_main.o: ../sys/init_main.c ../machine/pte.h ../h/param.h
init_main.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
init_main.o: ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
init_main.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/kernel.h
init_main.o: ../h/fs.h ../h/mount.h ../h/map.h ../h/proc.h ../h/inode.h
init_main.o: ../h/seg.h ../h/conf.h ../h/buf.h ../h/vm.h ../h/vmparam.h
init_main.o: ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
init_main.o: ../h/cmap.h ../h/text.h ../h/clist.h ../h/protosw.h ../h/quota.h
init_main.o: ../machine/reg.h ../machine/cpu.h ./loop.h ./kg.h ./sl.h
init_sysent.o: ../sys/init_sysent.c ../h/param.h ../machine/machparam.h
init_sysent.o: ../h/signal.h ../h/types.h ../h/systm.h
kern_acct.o: ../sys/kern_acct.c ../h/param.h ../machine/machparam.h
kern_acct.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
kern_acct.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
kern_acct.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/inode.h ../h/fs.h
kern_acct.o: ../h/kernel.h ../h/acct.h ../h/uio.h
kern_clock.o: ../sys/kern_clock.c ../machine/reg.h ../machine/psl.h
kern_clock.o: ../h/param.h ../machine/machparam.h ../h/signal.h ../h/types.h
kern_clock.o: ../h/systm.h ../h/dk.h ../h/callout.h ../h/dir.h ../h/user.h
kern_clock.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
kern_clock.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/kernel.h ../h/proc.h
kern_clock.o: ../h/vm.h ../h/vmparam.h ../machine/vmparam.h ../h/vmmac.h
kern_clock.o: ../h/vmmeter.h ../h/vmsystm.h ../h/text.h ../vax/mtpr.h
kern_clock.o: ../vax/clock.h
kern_descrip.o: ../sys/kern_descrip.c ../h/param.h ../machine/machparam.h
kern_descrip.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
kern_descrip.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
kern_descrip.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/kernel.h ../h/inode.h
kern_descrip.o: ../h/proc.h ../h/file.h ../h/socket.h ../h/socketvar.h
kern_descrip.o: ../h/mount.h ../h/stat.h ../h/ioctl.h ../h/ttychars.h
kern_descrip.o: ../h/ttydev.h
kern_exec.o: ../sys/kern_exec.c ../machine/reg.h ../machine/pte.h
kern_exec.o: ../machine/psl.h ../h/param.h ../machine/machparam.h ../h/signal.h
kern_exec.o: ../h/types.h ../h/systm.h ../h/map.h ../h/dir.h ../h/user.h
kern_exec.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
kern_exec.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/kernel.h ../h/proc.h
kern_exec.o: ../h/buf.h ../h/inode.h ../h/seg.h ../h/vm.h ../h/vmparam.h
kern_exec.o: ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
kern_exec.o: ../h/text.h ../h/file.h ../h/uio.h ../h/acct.h ../h/exec.h
kern_exec.o: ../vax/mtpr.h
kern_exit.o: ../sys/kern_exit.c ../machine/reg.h ../machine/psl.h ../h/param.h
kern_exit.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
kern_exit.o: ../h/map.h ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h
kern_exit.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
kern_exit.o: ../h/kernel.h ../h/proc.h ../h/buf.h ../h/wait.h ../h/vm.h
kern_exit.o: ../h/vmparam.h ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
kern_exit.o: ../h/vmsystm.h ../h/file.h ../h/mbuf.h ../h/inode.h ../h/syslog.h
kern_fork.o: ../sys/kern_fork.c ../machine/reg.h ../machine/pte.h
kern_fork.o: ../machine/psl.h ../h/param.h ../machine/machparam.h ../h/signal.h
kern_fork.o: ../h/types.h ../h/systm.h ../h/map.h ../h/dir.h ../h/user.h
kern_fork.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
kern_fork.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/kernel.h ../h/proc.h
kern_fork.o: ../h/inode.h ../h/seg.h ../h/vm.h ../h/vmparam.h
kern_fork.o: ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
kern_fork.o: ../h/text.h ../h/file.h ../h/acct.h ../h/quota.h
kern_mman.o: ../sys/kern_mman.c ../machine/reg.h ../machine/psl.h
kern_mman.o: ../machine/pte.h ../h/param.h ../machine/machparam.h ../h/signal.h
kern_mman.o: ../h/types.h ../h/systm.h ../h/map.h ../h/dir.h ../h/user.h
kern_mman.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
kern_mman.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/buf.h
kern_mman.o: ../h/inode.h ../h/seg.h ../h/acct.h ../h/wait.h ../h/vm.h
kern_mman.o: ../h/vmparam.h ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
kern_mman.o: ../h/vmsystm.h ../h/text.h ../h/file.h ../h/vadvise.h ../h/cmap.h
kern_mman.o: ../h/trace.h ../h/mman.h ../h/conf.h ../vax/mtpr.h
kern_proc.o: ../sys/kern_proc.c ../machine/reg.h ../machine/pte.h
kern_proc.o: ../machine/psl.h ../h/param.h ../machine/machparam.h ../h/signal.h
kern_proc.o: ../h/types.h ../h/systm.h ../h/map.h ../h/dir.h ../h/user.h
kern_proc.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
kern_proc.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/kernel.h ../h/proc.h
kern_proc.o: ../h/buf.h ../h/inode.h ../h/seg.h ../h/acct.h ../h/wait.h
kern_proc.o: ../h/vm.h ../h/vmparam.h ../machine/vmparam.h ../h/vmmac.h
kern_proc.o: ../h/vmmeter.h ../h/vmsystm.h ../h/text.h ../h/file.h ../h/quota.h
kern_proc.o: ../h/uio.h ../h/mbuf.h
kern_prot.o: ../sys/kern_prot.c ../machine/reg.h ../h/param.h
kern_prot.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
kern_prot.o: ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
kern_prot.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/inode.h
kern_prot.o: ../h/proc.h ../h/timeb.h ../h/times.h ../h/reboot.h ../h/fs.h
kern_prot.o: ../h/buf.h ../h/mount.h ../h/quota.h
kern_resource.o: ../sys/kern_resource.c ../h/param.h ../machine/machparam.h
kern_resource.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
kern_resource.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
kern_resource.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/inode.h ../h/proc.h
kern_resource.o: ../h/seg.h ../h/fs.h ../h/uio.h ../h/vm.h ../h/vmparam.h
kern_resource.o: ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
kern_resource.o: ../h/vmsystm.h ../h/kernel.h
kern_sig.o: ../sys/kern_sig.c ../machine/reg.h ../machine/pte.h
kern_sig.o: ../machine/psl.h ../h/param.h ../machine/machparam.h ../h/signal.h
kern_sig.o: ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h ../machine/pcb.h
kern_sig.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
kern_sig.o: ../h/errno.h ../h/inode.h ../h/proc.h ../h/timeb.h ../h/times.h
kern_sig.o: ../h/buf.h ../h/mount.h ../h/text.h ../h/seg.h ../h/vm.h
kern_sig.o: ../h/vmparam.h ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
kern_sig.o: ../h/vmsystm.h ../h/acct.h ../h/uio.h ../h/kernel.h ../vax/mtpr.h
kern_subr.o: ../sys/kern_subr.c ../h/param.h ../machine/machparam.h
kern_subr.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
kern_subr.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
kern_subr.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/uio.h
kern_synch.o: ../sys/kern_synch.c ../machine/pte.h ../h/param.h
kern_synch.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
kern_synch.o: ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
kern_synch.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h
kern_synch.o: ../h/file.h ../h/inode.h ../h/vm.h ../h/vmparam.h
kern_synch.o: ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
kern_synch.o: ../h/kernel.h ../h/buf.h ../vax/mtpr.h
kern_time.o: ../sys/kern_time.c ../machine/reg.h ../h/param.h
kern_time.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/dir.h
kern_time.o: ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
kern_time.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/kernel.h
kern_time.o: ../h/inode.h ../h/proc.h
kern_xxx.o: ../sys/kern_xxx.c ../h/param.h ../machine/machparam.h ../h/signal.h
kern_xxx.o: ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h ../machine/pcb.h
kern_xxx.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
kern_xxx.o: ../h/errno.h ../h/kernel.h ../h/proc.h ../h/reboot.h
quota_kern.o: ../sys/quota_kern.c ../h/param.h ../machine/machparam.h
quota_kern.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
quota_kern.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
quota_kern.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/inode.h
quota_kern.o: ../h/quota.h ../h/mount.h ../h/fs.h ../h/uio.h
quota_subr.o: ../sys/quota_subr.c ../h/param.h ../machine/machparam.h
quota_subr.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
quota_subr.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
quota_subr.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/inode.h
quota_subr.o: ../h/quota.h ../h/mount.h ../h/fs.h ../h/uio.h
quota_sys.o: ../sys/quota_sys.c ../h/param.h ../machine/machparam.h
quota_sys.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
quota_sys.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
quota_sys.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/inode.h
quota_sys.o: ../h/quota.h ../h/mount.h ../h/fs.h ../h/uio.h
quota_ufs.o: ../sys/quota_ufs.c ../h/param.h ../machine/machparam.h
quota_ufs.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
quota_ufs.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
quota_ufs.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/inode.h
quota_ufs.o: ../h/quota.h ../h/mount.h ../h/fs.h ../h/uio.h
subr_log.o: ../sys/subr_log.c ../h/param.h ../machine/machparam.h ../h/signal.h
subr_log.o: ../h/types.h ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h
subr_log.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
subr_log.o: ../h/proc.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h
subr_log.o: ../h/msgbuf.h ../h/file.h ../h/errno.h
subr_prf.o: ../sys/subr_prf.c ../h/param.h ../machine/machparam.h ../h/signal.h
subr_prf.o: ../h/types.h ../h/systm.h ../h/seg.h ../h/buf.h ../h/conf.h
subr_prf.o: ../h/reboot.h ../h/vm.h ../h/vmparam.h ../machine/vmparam.h
subr_prf.o: ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h ../h/msgbuf.h ../h/dir.h
subr_prf.o: ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
subr_prf.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h
subr_prf.o: ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/tty.h
subr_prf.o: ../h/ttychars.h ../h/ttydev.h ../h/syslog.h ../vax/mtpr.h
subr_rmap.o: ../sys/subr_rmap.c ../h/param.h ../machine/machparam.h
subr_rmap.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/map.h ../h/dir.h
subr_rmap.o: ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
subr_rmap.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h
subr_rmap.o: ../h/text.h ../h/kernel.h
subr_xxx.o: ../sys/subr_xxx.c ../machine/pte.h ../h/param.h
subr_xxx.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
subr_xxx.o: ../h/conf.h ../h/inode.h ../h/dir.h ../h/user.h ../machine/pcb.h
subr_xxx.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
subr_xxx.o: ../h/errno.h ../h/buf.h ../h/proc.h ../h/fs.h ../h/vm.h
subr_xxx.o: ../h/vmparam.h ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
subr_xxx.o: ../h/vmsystm.h ../h/cmap.h ../h/uio.h
sys_generic.o: ../sys/sys_generic.c ../h/param.h ../machine/machparam.h
sys_generic.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
sys_generic.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
sys_generic.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/ioctl.h
sys_generic.o: ../h/ttychars.h ../h/ttydev.h ../h/file.h ../h/proc.h ../h/uio.h
sys_generic.o: ../h/kernel.h ../h/stat.h
sys_inode.o: ../sys/sys_inode.c ../h/param.h ../machine/machparam.h
sys_inode.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
sys_inode.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
sys_inode.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/inode.h ../h/proc.h
sys_inode.o: ../h/fs.h ../h/conf.h ../h/buf.h ../h/mount.h ../h/file.h
sys_inode.o: ../h/uio.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/tty.h
sys_inode.o: ../h/ttychars.h ../h/ttydev.h ../h/cmap.h ../h/stat.h
sys_inode.o: ../h/kernel.h
sys_process.o: ../sys/sys_process.c ../machine/reg.h ../machine/psl.h
sys_process.o: ../machine/pte.h ../h/param.h ../machine/machparam.h
sys_process.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
sys_process.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
sys_process.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/inode.h
sys_process.o: ../h/text.h ../h/seg.h ../h/vm.h ../h/vmparam.h
sys_process.o: ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
sys_process.o: ../h/buf.h ../h/acct.h ../h/ptrace.h
sys_socket.o: ../sys/sys_socket.c ../h/param.h ../machine/machparam.h
sys_socket.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
sys_socket.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
sys_socket.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/file.h ../h/mbuf.h
sys_socket.o: ../h/protosw.h ../h/socket.h ../h/socketvar.h ../h/ioctl.h
sys_socket.o: ../h/ttychars.h ../h/ttydev.h ../h/uio.h ../h/stat.h ../net/if.h
sys_socket.o: ../net/if_arp.h ../net/route.h
tty.o: ../sys/tty.c ../machine/reg.h ../h/param.h ../machine/machparam.h
tty.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
tty.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h
tty.o: ../h/uio.h ../h/errno.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h
tty.o: ../h/tty.h ../h/ttychars.h ../h/ttydev.h ../h/proc.h ../h/inode.h
tty.o: ../h/file.h ../h/conf.h ../h/buf.h ../h/dk.h ../h/uio.h ../h/kernel.h
tty_conf.o: ../sys/tty_conf.c ../h/param.h ../machine/machparam.h ../h/signal.h
tty_conf.o: ../h/types.h ../h/systm.h ../h/buf.h ../h/ioctl.h ../h/ttychars.h
tty_conf.o: ../h/ttydev.h ../h/tty.h ../h/ttychars.h ../h/ttydev.h ../h/conf.h
tty_conf.o: ./bk.h ./tb.h ./sl.h
tty_pty.o: ../sys/tty_pty.c ./pty.h ../h/param.h ../machine/machparam.h
tty_pty.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/ioctl.h ../h/ttychars.h
tty_pty.o: ../h/ttydev.h ../h/tty.h ../h/ttychars.h ../h/ttydev.h ../h/dir.h
tty_pty.o: ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
tty_pty.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/conf.h ../h/file.h
tty_pty.o: ../h/proc.h ../h/uio.h ../h/kernel.h
tty_subr.o: ../sys/tty_subr.c ../h/param.h ../machine/machparam.h ../h/signal.h
tty_subr.o: ../h/types.h ../h/systm.h ../h/buf.h ../h/ioctl.h ../h/ttychars.h
tty_subr.o: ../h/ttydev.h ../h/tty.h ../h/ttychars.h ../h/ttydev.h ../h/clist.h
tty_tty.o: ../sys/tty_tty.c ../h/param.h ../machine/machparam.h ../h/signal.h
tty_tty.o: ../h/types.h ../h/systm.h ../h/conf.h ../h/dir.h ../h/user.h
tty_tty.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
tty_tty.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/ioctl.h ../h/ttychars.h
tty_tty.o: ../h/ttydev.h ../h/tty.h ../h/ttychars.h ../h/ttydev.h ../h/proc.h
tty_tty.o: ../h/uio.h
ufs_alloc.o: ../sys/ufs_alloc.c ../h/param.h ../machine/machparam.h
ufs_alloc.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/mount.h ../h/fs.h
ufs_alloc.o: ../h/buf.h ../h/inode.h ../h/dir.h ../h/user.h ../machine/pcb.h
ufs_alloc.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
ufs_alloc.o: ../h/errno.h ../h/quota.h ../h/kernel.h ../h/syslog.h ../h/cmap.h
ufs_bio.o: ../sys/ufs_bio.c ../machine/pte.h ../h/param.h
ufs_bio.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
ufs_bio.o: ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
ufs_bio.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/buf.h
ufs_bio.o: ../h/conf.h ../h/proc.h ../h/seg.h ../h/vm.h ../h/vmparam.h
ufs_bio.o: ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
ufs_bio.o: ../h/trace.h
ufs_bmap.o: ../sys/ufs_bmap.c ../h/param.h ../machine/machparam.h ../h/signal.h
ufs_bmap.o: ../h/types.h ../h/systm.h ../h/inode.h ../h/dir.h ../h/user.h
ufs_bmap.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
ufs_bmap.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/buf.h ../h/proc.h
ufs_bmap.o: ../h/fs.h
ufs_dsort.o: ../sys/ufs_dsort.c ../h/param.h ../machine/machparam.h
ufs_dsort.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/buf.h
ufs_fio.o: ../sys/ufs_fio.c ../machine/reg.h ../h/param.h
ufs_fio.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
ufs_fio.o: ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
ufs_fio.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/fs.h
ufs_fio.o: ../h/file.h ../h/conf.h ../h/inode.h ../h/acct.h ../h/mount.h
ufs_fio.o: ../h/socket.h ../h/socketvar.h ../h/proc.h
ufs_inode.o: ../sys/ufs_inode.c ../h/param.h ../machine/machparam.h
ufs_inode.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/mount.h ../h/dir.h
ufs_inode.o: ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
ufs_inode.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/inode.h
ufs_inode.o: ../h/fs.h ../h/buf.h ../h/cmap.h ../h/quota.h ../h/kernel.h
ufs_mount.o: ../sys/ufs_mount.c ../h/param.h ../machine/machparam.h
ufs_mount.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
ufs_mount.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
ufs_mount.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/inode.h ../h/proc.h
ufs_mount.o: ../h/fs.h ../h/buf.h ../h/mount.h ../h/file.h ../h/conf.h
ufs_namei.o: ../sys/ufs_namei.c ../h/param.h ../machine/machparam.h
ufs_namei.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/inode.h ../h/fs.h
ufs_namei.o: ../h/mount.h ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h
ufs_namei.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
ufs_namei.o: ../h/buf.h ../h/conf.h ../h/uio.h ../h/kernel.h
ufs_subr.o: ../sys/ufs_subr.c ../h/param.h ../machine/machparam.h ../h/signal.h
ufs_subr.o: ../h/types.h ../h/systm.h ../h/mount.h ../h/fs.h ../h/buf.h
ufs_subr.o: ../h/inode.h ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h
ufs_subr.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
ufs_subr.o: ../h/quota.h ../h/kernel.h
ufs_syscalls.o: ../sys/ufs_syscalls.c ../h/param.h ../machine/machparam.h
ufs_syscalls.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
ufs_syscalls.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
ufs_syscalls.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/kernel.h ../h/file.h
ufs_syscalls.o: ../h/stat.h ../h/inode.h ../h/fs.h ../h/buf.h ../h/proc.h
ufs_syscalls.o: ../h/quota.h ../h/uio.h ../h/socket.h ../h/socketvar.h
ufs_syscalls.o: ../h/mount.h
ufs_tables.o: ../sys/ufs_tables.c ../h/param.h ../machine/machparam.h
ufs_tables.o: ../h/signal.h ../h/types.h
ufs_xxx.o: ../sys/ufs_xxx.c ../h/param.h ../machine/machparam.h ../h/signal.h
ufs_xxx.o: ../h/types.h ../h/systm.h ../h/inode.h ../h/fs.h ../h/mount.h
ufs_xxx.o: ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
ufs_xxx.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/buf.h
ufs_xxx.o: ../h/conf.h
uipc_domain.o: ../sys/uipc_domain.c ../h/param.h ../machine/machparam.h
uipc_domain.o: ../h/signal.h ../h/types.h ../h/socket.h ../h/protosw.h
uipc_domain.o: ../h/domain.h ../h/time.h ../h/kernel.h ./imp.h
uipc_mbuf.o: ../sys/uipc_mbuf.c ../machine/pte.h ../h/param.h
uipc_mbuf.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/dir.h
uipc_mbuf.o: ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
uipc_mbuf.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h
uipc_mbuf.o: ../h/cmap.h ../h/map.h ../h/mbuf.h ../h/vm.h ../h/vmparam.h
uipc_mbuf.o: ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
uipc_mbuf.o: ../h/kernel.h
uipc_proto.o: ../sys/uipc_proto.c ../h/param.h ../machine/machparam.h
uipc_proto.o: ../h/signal.h ../h/types.h ../h/socket.h ../h/protosw.h
uipc_proto.o: ../h/domain.h ../h/mbuf.h
uipc_socket.o: ../sys/uipc_socket.c ../h/param.h ../machine/machparam.h
uipc_socket.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
uipc_socket.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
uipc_socket.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/file.h
uipc_socket.o: ../h/inode.h ../h/buf.h ../h/mbuf.h ../h/un.h ../h/domain.h
uipc_socket.o: ../h/protosw.h ../h/socket.h ../h/socketvar.h ../h/stat.h
uipc_socket.o: ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/uio.h
uipc_socket.o: ../net/route.h ../netinet/in.h ../net/if.h ../net/if_arp.h
uipc_socket2.o: ../sys/uipc_socket2.c ../h/param.h ../machine/machparam.h
uipc_socket2.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
uipc_socket2.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
uipc_socket2.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/file.h
uipc_socket2.o: ../h/inode.h ../h/buf.h ../h/mbuf.h ../h/protosw.h
uipc_socket2.o: ../h/socket.h ../h/socketvar.h
uipc_syscalls.o: ../sys/uipc_syscalls.c ../h/param.h ../machine/machparam.h
uipc_syscalls.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
uipc_syscalls.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
uipc_syscalls.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/file.h
uipc_syscalls.o: ../h/inode.h ../h/buf.h ../h/mbuf.h ../h/protosw.h
uipc_syscalls.o: ../h/socket.h ../h/socketvar.h ../h/uio.h
uipc_usrreq.o: ../sys/uipc_usrreq.c ../h/param.h ../machine/machparam.h
uipc_usrreq.o: ../h/signal.h ../h/types.h ../h/dir.h ../h/user.h
uipc_usrreq.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
uipc_usrreq.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/mbuf.h ../h/domain.h
uipc_usrreq.o: ../h/protosw.h ../h/socket.h ../h/socketvar.h ../h/unpcb.h
uipc_usrreq.o: ../h/un.h ../h/inode.h ../h/file.h ../h/stat.h
vm_drum.o: ../sys/vm_drum.c ../machine/pte.h ../h/param.h
vm_drum.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
vm_drum.o: ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
vm_drum.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h
vm_drum.o: ../h/buf.h ../h/text.h ../h/map.h ../h/vm.h ../h/vmparam.h
vm_drum.o: ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
vm_drum.o: ../h/cmap.h ../h/kernel.h
vm_mem.o: ../sys/vm_mem.c ../machine/pte.h ../h/param.h ../machine/machparam.h
vm_mem.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/cmap.h ../h/dir.h
vm_mem.o: ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
vm_mem.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/text.h
vm_mem.o: ../h/vm.h ../h/vmparam.h ../machine/vmparam.h ../h/vmmac.h
vm_mem.o: ../h/vmmeter.h ../h/vmsystm.h ../h/file.h ../h/inode.h ../h/buf.h
vm_mem.o: ../h/mount.h ../h/trace.h ../h/map.h ../h/kernel.h
vm_mon.o: ../sys/vm_mon.c ../h/param.h ../machine/machparam.h ../h/signal.h
vm_mon.o: ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h ../machine/pcb.h
vm_mon.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
vm_mon.o: ../h/errno.h ../h/vmmeter.h ../h/trace.h ../h/kernel.h
vm_page.o: ../sys/vm_page.c ../machine/reg.h ../machine/pte.h ../h/param.h
vm_page.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
vm_page.o: ../h/inode.h ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h
vm_page.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
vm_page.o: ../h/proc.h ../h/buf.h ../h/text.h ../h/cmap.h ../h/vm.h
vm_page.o: ../h/vmparam.h ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
vm_page.o: ../h/vmsystm.h ../h/file.h ../h/trace.h
vm_proc.o: ../sys/vm_proc.c ../machine/pte.h ../h/param.h
vm_proc.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
vm_proc.o: ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
vm_proc.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h
vm_proc.o: ../h/map.h ../h/cmap.h ../h/text.h ../h/vm.h ../h/vmparam.h
vm_proc.o: ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
vm_proc.o: ../vax/mtpr.h
vm_pt.o: ../sys/vm_pt.c ../h/param.h ../machine/machparam.h ../h/signal.h
vm_pt.o: ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h ../machine/pcb.h
vm_pt.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
vm_pt.o: ../h/errno.h ../h/proc.h ../h/map.h ../h/cmap.h ../h/vm.h
vm_pt.o: ../h/vmparam.h ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
vm_pt.o: ../h/vmsystm.h ../h/buf.h ../h/text.h ../h/mount.h ../h/inode.h
vm_pt.o: ../h/kernel.h ../machine/pte.h ../vax/mtpr.h
vm_sched.o: ../sys/vm_sched.c ../h/param.h ../machine/machparam.h ../h/signal.h
vm_sched.o: ../h/types.h ../h/systm.h ../h/seg.h ../h/dir.h ../h/user.h
vm_sched.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
vm_sched.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/text.h
vm_sched.o: ../h/vm.h ../h/vmparam.h ../machine/vmparam.h ../h/vmmac.h
vm_sched.o: ../h/vmmeter.h ../h/vmsystm.h ../h/cmap.h ../h/kernel.h
vm_subr.o: ../sys/vm_subr.c ../machine/pte.h ../h/param.h
vm_subr.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
vm_subr.o: ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
vm_subr.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/vm.h
vm_subr.o: ../h/vmparam.h ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
vm_subr.o: ../h/vmsystm.h ../h/proc.h ../h/cmap.h ../h/inode.h ../h/buf.h
vm_subr.o: ../h/text.h ../h/fs.h ../vax/mtpr.h
vm_sw.o: ../sys/vm_sw.c ../h/param.h ../machine/machparam.h ../h/signal.h
vm_sw.o: ../h/types.h ../h/systm.h ../h/buf.h ../h/conf.h ../h/dir.h
vm_sw.o: ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
vm_sw.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/inode.h ../h/map.h
vm_sw.o: ../h/uio.h ../h/file.h
vm_swap.o: ../sys/vm_swap.c ../machine/pte.h ../h/param.h
vm_swap.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
vm_swap.o: ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
vm_swap.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h
vm_swap.o: ../h/text.h ../h/map.h ../h/buf.h ../h/cmap.h ../h/vm.h
vm_swap.o: ../h/vmparam.h ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
vm_swap.o: ../h/vmsystm.h
vm_swp.o: ../sys/vm_swp.c ../machine/pte.h ../h/param.h ../machine/machparam.h
vm_swp.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
vm_swp.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h
vm_swp.o: ../h/uio.h ../h/errno.h ../h/buf.h ../h/conf.h ../h/proc.h ../h/seg.h
vm_swp.o: ../h/vm.h ../h/vmparam.h ../machine/vmparam.h ../h/vmmac.h
vm_swp.o: ../h/vmmeter.h ../h/vmsystm.h ../h/trace.h ../h/map.h ../h/uio.h
vm_text.o: ../sys/vm_text.c ../machine/pte.h ../h/param.h
vm_text.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
vm_text.o: ../h/map.h ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h
vm_text.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
vm_text.o: ../h/proc.h ../h/text.h ../h/inode.h ../h/buf.h ../h/seg.h ../h/vm.h
vm_text.o: ../h/vmparam.h ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
vm_text.o: ../h/vmsystm.h ../h/cmap.h ../h/uio.h ../h/exec.h
autoconf.o: ../vax/autoconf.c ./mba.h ./uba.h ../vax/pte.h ../h/param.h
autoconf.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
autoconf.o: ../h/map.h ../h/buf.h ../h/dk.h ../h/vm.h ../h/vmparam.h
autoconf.o: ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
autoconf.o: ../h/conf.h ../h/dmap.h ../h/reboot.h ../vax/cpu.h ../vax/mem.h
autoconf.o: ../vax/mtpr.h ../vax/nexus.h ../vax/scb.h ../vax/ioa.h
autoconf.o: ../vaxmba/mbareg.h ../vaxmba/mbavar.h ../vaxuba/ubareg.h
autoconf.o: ../vaxuba/ubavar.h
clock.o: ../vax/clock.c ../h/param.h ../machine/machparam.h ../h/signal.h
clock.o: ../h/types.h ../h/time.h ../h/kernel.h ../vax/mtpr.h ../vax/clock.h
clock.o: ../vax/cpu.h
conf.o: ../vax/conf.c ../h/param.h ../machine/machparam.h ../h/signal.h
conf.o: ../h/types.h ../h/systm.h ../h/buf.h ../h/ioctl.h ../h/ttychars.h
conf.o: ../h/ttydev.h ../h/tty.h ../h/ttychars.h ../h/ttydev.h ../h/conf.h
conf.o: ./hp.h ./tu.h ./rk.h ./te.h ./tms.h ./ts.h ./mu.h ./ra.h ./up.h ./tj.h
conf.o: ./rb.h ./rx.h ./uu.h ./rl.h ./acc.h ./ct.h ./dh.h ./dmf.h ./dz.h ./lp.h
conf.o: ./va.h ./vp.h ./pty.h ./lpa.h ./dn.h ./ik.h ./ps.h ./ad.h ./dhu.h
conf.o: ./vs.h ./dmz.h
cons.o: ../vax/cons.c ../h/param.h ../machine/machparam.h ../h/signal.h
cons.o: ../h/types.h ../h/conf.h ../h/dir.h ../h/user.h ../machine/pcb.h
cons.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
cons.o: ../h/errno.h ../h/proc.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h
cons.o: ../h/tty.h ../h/ttychars.h ../h/ttydev.h ../h/systm.h ../h/uio.h
cons.o: ../vax/cpu.h ../vax/cons.h ../vax/mtpr.h
cpudata.o: ../vax/cpudata.c ../vax/pte.h ../h/param.h ../machine/machparam.h
cpudata.o: ../h/signal.h ../h/types.h ../vax/cpu.h ../vax/nexus.h ../vax/ioa.h
cpudata.o: ../vaxuba/ubareg.h
crl.o: ../vax/crl.c ../h/param.h ../machine/machparam.h ../h/signal.h
crl.o: ../h/types.h ../h/systm.h ../h/conf.h ../h/dir.h ../h/user.h
crl.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h
crl.o: ../h/uio.h ../h/errno.h ../h/buf.h ../h/uio.h ../vax/cons.h ../vax/cpu.h
crl.o: ../vax/crl.h ../vax/mtpr.h
dkbad.o: ../vax/dkbad.c ../h/param.h ../machine/machparam.h ../h/signal.h
dkbad.o: ../h/types.h ../h/buf.h ../h/dkbad.h
flp.o: ../vax/flp.c ../h/param.h ../machine/machparam.h ../h/signal.h
flp.o: ../h/types.h ../h/systm.h ../h/conf.h ../h/dir.h ../h/user.h
flp.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h
flp.o: ../h/uio.h ../h/errno.h ../h/buf.h ../h/uio.h ../vax/cons.h ../vax/cpu.h
flp.o: ../vax/flp.h ../vax/mtpr.h
in_cksum.o: ../vax/in_cksum.c ../h/types.h ../h/mbuf.h ../netinet/in.h
in_cksum.o: ../netinet/in_systm.h
ka630.o: ../vax/ka630.c
ns_cksum.o: ../vax/ns_cksum.c ../h/types.h ../h/mbuf.h
machdep.o: ../vax/machdep.c ../vax/reg.h ../vax/pte.h ../vax/psl.h ../h/param.h
machdep.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
machdep.o: ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
machdep.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/kernel.h
machdep.o: ../h/map.h ../h/vm.h ../h/vmparam.h ../machine/vmparam.h
machdep.o: ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h ../h/proc.h ../h/buf.h
machdep.o: ../h/reboot.h ../h/conf.h ../h/inode.h ../h/file.h ../h/text.h
machdep.o: ../h/clist.h ../h/callout.h ../h/cmap.h ../h/mbuf.h ../h/msgbuf.h
machdep.o: ../h/quota.h ../vax/frame.h ../vax/clock.h ../vax/cons.h
machdep.o: ../vax/cpu.h ../vax/mem.h ../vax/mtpr.h ../vax/rpb.h ../vax/ka630.h
machdep.o: ../vaxuba/ubavar.h ../vaxuba/ubareg.h
mem.o: ../vax/mem.c ../vax/pte.h ../h/param.h ../machine/machparam.h
mem.o: ../h/signal.h ../h/types.h ../h/dir.h ../h/user.h ../machine/pcb.h
mem.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
mem.o: ../h/errno.h ../h/conf.h ../h/buf.h ../h/systm.h ../h/vm.h
mem.o: ../h/vmparam.h ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
mem.o: ../h/vmsystm.h ../h/cmap.h ../h/uio.h ../vax/mtpr.h
sys_machdep.o: ../vax/sys_machdep.c ../h/param.h ../machine/machparam.h
sys_machdep.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/dir.h ../h/user.h
sys_machdep.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
sys_machdep.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/ioctl.h
sys_machdep.o: ../h/ttychars.h ../h/ttydev.h ../h/file.h ../h/proc.h ../h/uio.h
sys_machdep.o: ../h/kernel.h ../h/mtio.h ../h/buf.h ../h/trace.h ../vax/dkio.h
sys_machdep.o: ../vax/pte.h ../vaxuba/ubareg.h ../vaxuba/ubavar.h
trap.o: ../vax/trap.c ../vax/psl.h ../vax/reg.h ../vax/pte.h ../h/param.h
trap.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
trap.o: ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
trap.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ./assym.s
trap.o: ../h/proc.h ../h/seg.h ../vax/trap.h ../h/acct.h ../h/kernel.h
trap.o: ../vax/mtpr.h
tu.o: ../vax/tu.c ../h/param.h ../machine/machparam.h ../h/signal.h
tu.o: ../h/types.h ../h/systm.h ../h/buf.h ../h/conf.h ../h/dir.h ../h/user.h
tu.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h
tu.o: ../h/uio.h ../h/errno.h ../h/kernel.h ../vax/cpu.h ../vax/mtpr.h
tu.o: ../vax/rsp.h
ufs_machdep.o: ../vax/ufs_machdep.c ../vax/pte.h ../h/param.h
ufs_machdep.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
ufs_machdep.o: ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
ufs_machdep.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/buf.h
ufs_machdep.o: ../h/conf.h ../h/proc.h ../h/seg.h ../h/vm.h ../h/vmparam.h
ufs_machdep.o: ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
vm_machdep.o: ../vax/vm_machdep.c ../vax/pte.h ../h/param.h
vm_machdep.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
vm_machdep.o: ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h
vm_machdep.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h
vm_machdep.o: ../h/cmap.h ../h/mount.h ../h/vm.h ../h/vmparam.h
vm_machdep.o: ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
vm_machdep.o: ../h/text.h ../vax/mtpr.h
if_de.o: ../vaxif/if_de.c ./de.h ../machine/pte.h ../h/param.h
if_de.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
if_de.o: ../h/mbuf.h ../h/buf.h ../h/protosw.h ../h/socket.h ../h/vmmac.h
if_de.o: ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/errno.h ../h/syslog.h
if_de.o: ../net/if.h ../net/if_arp.h ../net/netisr.h ../net/route.h
if_de.o: ../netinet/in.h ../netinet/in_systm.h ../netinet/in_var.h
if_de.o: ../netinet/ip.h ../netinet/if_ether.h ../netns/ns.h ../netns/ns_if.h
if_de.o: ../vax/cpu.h ../vax/mtpr.h ../vaxif/if_dereg.h ../vaxif/if_uba.h
if_de.o: ../vaxuba/ubareg.h ../vaxuba/ubavar.h
if_ec.o: ../vaxif/if_ec.c ./ec.h ../machine/pte.h ../h/param.h
if_ec.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
if_ec.o: ../h/mbuf.h ../h/buf.h ../h/protosw.h ../h/socket.h ../h/syslog.h
if_ec.o: ../h/vmmac.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/errno.h
if_ec.o: ../net/if.h ../net/if_arp.h ../net/netisr.h ../net/route.h
if_ec.o: ../netinet/in.h ../netinet/in_systm.h ../netinet/in_var.h
if_ec.o: ../netinet/ip.h ../netinet/if_ether.h ../netns/ns.h ../netns/ns_if.h
if_ec.o: ../vax/cpu.h ../vax/mtpr.h ../vaxif/if_ecreg.h ../vaxif/if_uba.h
if_ec.o: ../vaxuba/ubareg.h ../vaxuba/ubavar.h
if_en.o: ../vaxif/if_en.c ./en.h ../machine/pte.h ../h/param.h
if_en.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
if_en.o: ../h/mbuf.h ../h/buf.h ../h/protosw.h ../h/socket.h ../h/vmmac.h
if_en.o: ../h/errno.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../net/if.h
if_en.o: ../net/if_arp.h ../net/netisr.h ../net/route.h ../netinet/in.h
if_en.o: ../netinet/in_systm.h ../netinet/in_var.h ../netinet/ip.h
if_en.o: ../netns/ns.h ../netns/ns_if.h ../vax/cpu.h ../vax/mtpr.h
if_en.o: ../vaxif/if_en.h ../vaxif/if_enreg.h ../vaxif/if_uba.h
if_en.o: ../vaxuba/ubareg.h ../vaxuba/ubavar.h
if_ex.o: ../vaxif/if_ex.c ./ex.h ../machine/pte.h ../h/param.h
if_ex.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
if_ex.o: ../h/mbuf.h ../h/buf.h ../h/protosw.h ../h/socket.h ../h/vmmac.h
if_ex.o: ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/errno.h ../net/if.h
if_ex.o: ../net/if_arp.h ../net/netisr.h ../net/route.h ../netinet/in.h
if_ex.o: ../netinet/in_systm.h ../netinet/in_var.h ../netinet/ip.h
if_ex.o: ../netinet/if_ether.h ../netns/ns.h ../netns/ns_if.h ../vax/cpu.h
if_ex.o: ../vax/mtpr.h ../vaxif/if_exreg.h ../vaxif/if_uba.h ../vaxuba/ubareg.h
if_ex.o: ../vaxuba/ubavar.h
if_il.o: ../vaxif/if_il.c ./il.h ../machine/pte.h ../h/param.h
if_il.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
if_il.o: ../h/mbuf.h ../h/buf.h ../h/protosw.h ../h/socket.h ../h/vmmac.h
if_il.o: ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/errno.h ../net/if.h
if_il.o: ../net/if_arp.h ../net/netisr.h ../net/route.h ../netinet/in.h
if_il.o: ../netinet/in_systm.h ../netinet/in_var.h ../netinet/ip.h
if_il.o: ../netinet/if_ether.h ../netns/ns.h ../netns/ns_if.h ../vax/cpu.h
if_il.o: ../vax/mtpr.h ../vaxif/if_il.h ../vaxif/if_ilreg.h ../vaxif/if_uba.h
if_il.o: ../vaxuba/ubareg.h ../vaxuba/ubavar.h
if_uba.o: ../vaxif/if_uba.c ../machine/pte.h ../h/param.h
if_uba.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
if_uba.o: ../h/mbuf.h ../h/map.h ../h/buf.h ../h/cmap.h ../h/vmmac.h
if_uba.o: ../h/socket.h ../h/syslog.h ../net/if.h ../net/if_arp.h ../vax/mtpr.h
if_uba.o: ../vaxif/if_uba.h ../vaxuba/ubareg.h ../vaxuba/ubavar.h
hp.o: ../vaxmba/hp.c ./hp.h ../machine/pte.h ../h/param.h
hp.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h ../h/dk.h
hp.o: ../h/buf.h ../h/conf.h ../h/dir.h ../h/user.h ../machine/pcb.h
hp.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
hp.o: ../h/errno.h ../h/map.h ../vax/mtpr.h ../h/vm.h ../h/vmparam.h
hp.o: ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
hp.o: ../h/cmap.h ../h/dkbad.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h
hp.o: ../h/uio.h ../h/syslog.h ../vax/dkio.h ../vaxmba/mbareg.h
hp.o: ../vaxmba/mbavar.h ../vaxmba/hpreg.h
ht.o: ../vaxmba/ht.c ./tu.h ../machine/pte.h ../h/param.h
ht.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h ../h/buf.h
ht.o: ../h/conf.h ../h/dir.h ../h/file.h ../h/user.h ../machine/pcb.h
ht.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
ht.o: ../h/errno.h ../h/map.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h
ht.o: ../h/mtio.h ../h/cmap.h ../h/uio.h ../h/tty.h ../h/ttychars.h
ht.o: ../h/ttydev.h ../vax/cpu.h ../vaxmba/mbareg.h ../vaxmba/mbavar.h
ht.o: ../vaxmba/htreg.h
mba.o: ../vaxmba/mba.c ./mba.h ../machine/pte.h ../h/param.h
mba.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h ../h/dk.h
mba.o: ../h/buf.h ../h/conf.h ../h/dir.h ../h/user.h ../machine/pcb.h
mba.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
mba.o: ../h/errno.h ../h/proc.h ../h/map.h ../vax/mtpr.h ../h/vm.h
mba.o: ../h/vmparam.h ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
mba.o: ../h/vmsystm.h ../vaxmba/mbareg.h ../vaxmba/mbavar.h
mt.o: ../vaxmba/mt.c ./mu.h ../machine/pte.h ../h/param.h
mt.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h ../h/buf.h
mt.o: ../h/conf.h ../h/dir.h ../h/file.h ../h/user.h ../machine/pcb.h
mt.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
mt.o: ../h/errno.h ../h/map.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h
mt.o: ../h/mtio.h ../h/cmap.h ../h/uio.h ../h/tty.h ../h/ttychars.h
mt.o: ../h/ttydev.h ../vax/cpu.h ../vaxmba/mbareg.h ../vaxmba/mbavar.h
mt.o: ../vaxmba/mtreg.h
dh.o: ../vaxuba/dh.c ./dh.h ../machine/pte.h ./bk.h ./uba.h ../h/param.h
dh.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/conf.h ../h/dir.h
dh.o: ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
dh.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/ioctl.h
dh.o: ../h/ttychars.h ../h/ttydev.h ../h/tty.h ../h/ttychars.h ../h/ttydev.h
dh.o: ../h/map.h ../h/buf.h ../h/vm.h ../h/vmparam.h ../machine/vmparam.h
dh.o: ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h ../h/kernel.h ../h/syslog.h
dh.o: ../vaxuba/ubareg.h ../vaxuba/ubavar.h ../vaxuba/dhreg.h ../vaxuba/dmreg.h
dh.o: ../h/bkmac.h ../h/clist.h ../h/file.h ../h/uio.h
dhu.o: ../vaxuba/dhu.c ./dhu.h ../machine/pte.h ./bk.h ../h/param.h
dhu.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/conf.h ../h/dir.h
dhu.o: ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
dhu.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/ioctl.h
dhu.o: ../h/ttychars.h ../h/ttydev.h ../h/tty.h ../h/ttychars.h ../h/ttydev.h
dhu.o: ../h/map.h ../h/buf.h ../h/vm.h ../h/vmparam.h ../machine/vmparam.h
dhu.o: ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h ../h/kernel.h ../h/syslog.h
dhu.o: ./uba.h ../vaxuba/ubareg.h ../vaxuba/ubavar.h ../vaxuba/dhureg.h
dhu.o: ../h/bkmac.h ../h/clist.h ../h/file.h ../h/uio.h
dmf.o: ../vaxuba/dmf.c ./dmf.h ../machine/pte.h ./bk.h ./uba.h ../h/param.h
dmf.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/conf.h ../h/dir.h
dmf.o: ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
dmf.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/ioctl.h
dmf.o: ../h/ttychars.h ../h/ttydev.h ../h/tty.h ../h/ttychars.h ../h/ttydev.h
dmf.o: ../h/map.h ../h/buf.h ../h/vm.h ../h/vmparam.h ../machine/vmparam.h
dmf.o: ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h ../h/bkmac.h ../h/clist.h
dmf.o: ../h/file.h ../h/uio.h ../h/kernel.h ../h/syslog.h ../vaxuba/ubareg.h
dmf.o: ../vaxuba/ubavar.h ../vaxuba/dmfreg.h
dmz.o: ../vaxuba/dmz.c ./dmz.h ../machine/pte.h ./bk.h ./uba.h ../h/param.h
dmz.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/conf.h ../h/dir.h
dmz.o: ../h/user.h ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
dmz.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/ioctl.h
dmz.o: ../h/ttychars.h ../h/ttydev.h ../h/tty.h ../h/ttychars.h ../h/ttydev.h
dmz.o: ../h/map.h ../h/buf.h ../h/vm.h ../h/vmparam.h ../machine/vmparam.h
dmz.o: ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h ../h/bkmac.h ../h/clist.h
dmz.o: ../h/file.h ../h/uio.h ../h/kernel.h ../h/syslog.h ../vaxuba/ubareg.h
dmz.o: ../vaxuba/ubavar.h ../vaxuba/dmzreg.h ../vaxuba/dmreg.h
dz.o: ../vaxuba/dz.c ./dz.h ./bk.h ../machine/pte.h ../h/param.h
dz.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
dz.o: ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/tty.h ../h/ttychars.h
dz.o: ../h/ttydev.h ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h
dz.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
dz.o: ../h/proc.h ../h/map.h ../h/buf.h ../h/vm.h ../h/vmparam.h
dz.o: ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
dz.o: ../h/conf.h ../h/bkmac.h ../h/file.h ../h/uio.h ../h/kernel.h
dz.o: ../h/syslog.h ../vaxuba/pdma.h ../vaxuba/ubavar.h ../vaxuba/dzreg.h
idc.o: ../vaxuba/idc.c ./rb.h ../machine/pte.h ../h/param.h
idc.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
idc.o: ../h/buf.h ../h/conf.h ../h/dir.h ../h/user.h ../machine/pcb.h
idc.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
idc.o: ../h/errno.h ../h/map.h ../h/vm.h ../h/vmparam.h ../machine/vmparam.h
idc.o: ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h ../h/dk.h ../h/cmap.h
idc.o: ../h/dkbad.h ../h/uio.h ../h/kernel.h ../h/syslog.h ../vax/cpu.h
idc.o: ../vaxuba/ubareg.h ../vaxuba/ubavar.h ../vaxuba/idcreg.h
lp.o: ../vaxuba/lp.c ./lp.h ../machine/pte.h ../h/param.h
lp.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/dir.h ../h/user.h
lp.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h
lp.o: ../h/uio.h ../h/errno.h ../h/buf.h ../h/systm.h ../h/map.h ../h/uio.h
lp.o: ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/tty.h ../h/ttychars.h
lp.o: ../h/ttydev.h ../h/kernel.h ../vaxuba/ubavar.h
rk.o: ../vaxuba/rk.c ./rk.h ../machine/pte.h ../h/param.h
rk.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h ../h/buf.h
rk.o: ../h/conf.h ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h
rk.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
rk.o: ../h/map.h ../h/vm.h ../h/vmparam.h ../machine/vmparam.h ../h/vmmac.h
rk.o: ../h/vmmeter.h ../h/vmsystm.h ../h/dk.h ../h/cmap.h ../h/dkbad.h
rk.o: ../h/uio.h ../h/kernel.h ../h/syslog.h ../vax/cpu.h ../vaxuba/ubareg.h
rk.o: ../vaxuba/ubavar.h ../vaxuba/rkreg.h
rl.o: ../vaxuba/rl.c ./rl.h ../machine/pte.h ../h/param.h
rl.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h ../h/dk.h
rl.o: ../h/dkbad.h ../h/buf.h ../h/conf.h ../h/dir.h ../h/user.h
rl.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h
rl.o: ../h/uio.h ../h/errno.h ../h/map.h ../h/vm.h ../h/vmparam.h
rl.o: ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
rl.o: ../h/cmap.h ../h/uio.h ../h/kernel.h ../vax/cpu.h ../vax/nexus.h
rl.o: ../vaxuba/ubavar.h ../vaxuba/ubareg.h ../vaxuba/rlreg.h
tm.o: ../vaxuba/tm.c ./te.h ./ts.h ../machine/pte.h ../h/param.h
tm.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h ../h/buf.h
tm.o: ../h/dir.h ../h/conf.h ../h/user.h ../machine/pcb.h ../h/dmap.h
tm.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
tm.o: ../h/file.h ../h/map.h ../h/vm.h ../h/vmparam.h ../machine/vmparam.h
tm.o: ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h ../h/ioctl.h ../h/ttychars.h
tm.o: ../h/ttydev.h ../h/mtio.h ../h/cmap.h ../h/uio.h ../h/kernel.h ../h/tty.h
tm.o: ../h/ttychars.h ../h/ttydev.h ../vax/cpu.h ../vaxuba/ubareg.h
tm.o: ../vaxuba/ubavar.h ../vaxuba/tmreg.h
tmscp.o: ../vaxuba/tmscp.c ./tms.h ../vax/pte.h ../h/param.h
tmscp.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
tmscp.o: ../h/buf.h ../h/conf.h ../h/dir.h ../h/user.h ../machine/pcb.h
tmscp.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
tmscp.o: ../h/errno.h ../h/file.h ../h/map.h ../h/vm.h ../h/vmparam.h
tmscp.o: ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
tmscp.o: ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/syslog.h ../h/mtio.h
tmscp.o: ../h/cmap.h ../h/uio.h ../h/tty.h ../h/ttychars.h ../h/ttydev.h
tmscp.o: ../vax/cpu.h ../vax/mtpr.h ../vaxuba/ubareg.h ../vaxuba/ubavar.h
tmscp.o: ../vaxuba/tmscpreg.h ../vax/tmscp.h
ts.o: ../vaxuba/ts.c ./ts.h ../machine/pte.h ../h/param.h
ts.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h ../h/buf.h
ts.o: ../h/dir.h ../h/conf.h ../h/user.h ../machine/pcb.h ../h/dmap.h
ts.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
ts.o: ../h/file.h ../h/map.h ../h/vm.h ../h/vmparam.h ../machine/vmparam.h
ts.o: ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h ../h/ioctl.h ../h/ttychars.h
ts.o: ../h/ttydev.h ../h/mtio.h ../h/cmap.h ../h/uio.h ../h/tty.h
ts.o: ../h/ttychars.h ../h/ttydev.h ../vax/cpu.h ../vaxuba/ubareg.h
ts.o: ../vaxuba/ubavar.h ../vaxuba/tsreg.h
uba.o: ../vaxuba/uba.c ../machine/pte.h ../h/param.h ../machine/machparam.h
uba.o: ../h/signal.h ../h/types.h ../h/systm.h ../h/map.h ../h/buf.h ../h/vm.h
uba.o: ../h/vmparam.h ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
uba.o: ../h/vmsystm.h ../h/dir.h ../h/user.h ../machine/pcb.h ../h/dmap.h
uba.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
uba.o: ../h/proc.h ../h/conf.h ../h/dk.h ../h/kernel.h ../vax/cpu.h
uba.o: ../vax/mtpr.h ../vax/nexus.h ../vaxuba/ubareg.h ../vaxuba/ubavar.h
uba.o: ./ik.h ./vs.h
uda.o: ../vaxuba/uda.c ./ra.h ../machine/pte.h ../h/param.h
uda.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h
uda.o: ../h/buf.h ../h/conf.h ../h/dir.h ../h/user.h ../machine/pcb.h
uda.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
uda.o: ../h/errno.h ../h/map.h ../h/vm.h ../h/vmparam.h ../machine/vmparam.h
uda.o: ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h ../h/dk.h ../h/cmap.h
uda.o: ../h/uio.h ../vax/cpu.h ../vaxuba/ubareg.h ../vaxuba/ubavar.h
uda.o: ../vax/mtpr.h ../vaxuba/udareg.h ../vax/mscp.h
up.o: ../vaxuba/up.c ./up.h ../machine/pte.h ../h/param.h
up.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h ../h/dk.h
up.o: ../h/dkbad.h ../h/buf.h ../h/conf.h ../h/dir.h ../h/user.h
up.o: ../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h
up.o: ../h/uio.h ../h/errno.h ../h/map.h ../h/vm.h ../h/vmparam.h
up.o: ../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
up.o: ../h/cmap.h ../h/uio.h ../h/kernel.h ../h/syslog.h ../vax/cpu.h
up.o: ../vax/nexus.h ../vaxuba/ubavar.h ../vaxuba/ubareg.h ../vaxuba/upreg.h
ut.o: ../vaxuba/ut.c ./tj.h ../machine/pte.h ../h/param.h
ut.o: ../machine/machparam.h ../h/signal.h ../h/types.h ../h/systm.h ../h/buf.h
ut.o: ../h/conf.h ../h/dir.h ../h/file.h ../h/user.h ../machine/pcb.h
ut.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
ut.o: ../h/errno.h ../h/map.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h
ut.o: ../h/mtio.h ../h/cmap.h ../h/uio.h ../h/kernel.h ../h/tty.h
ut.o: ../h/ttychars.h ../h/ttydev.h ../vax/cpu.h ../vaxuba/ubareg.h
ut.o: ../vaxuba/ubavar.h ../vaxuba/utreg.h
