IDENT=-DGENERICISA -Di386 -DTCP_COMPAT_42 -DCOMPAT_43 -DNFS -DISOFS -DINET
PARAM=-DTIMEZONE=480 -DDST=1 -DMAXUSERS=10
# Copyright 1990 W. Jolitz
# @(#)Makefile.i386	7.1 5/10/91
# Makefile for 4.3 BSD-Reno
#
# This makefile is constructed from a machine description:
#	config machineid
# Most changes should be made in the machine description
#	/sys/i386/conf/``machineid''
# after which you should do
#	 config machineid
# Generic makefile changes should be made in
#	/sys/i386/conf/Makefile.i386
# after which config should be rerun for all machines.
#
# N.B.: NO DEPENDENCIES ON FOLLOWING FLAGS ARE INVISIBLE TO MAKEFILE
#	IF YOU CHANGE THE DEFINITION OF ANY OF THESE RECOMPILE EVERYTHING
#
# -DTRACE	compile in kernel tracing hooks
# -DQUOTA	compile in file system quotas
#
TOUCH=	touch -f -c
LD=	/usr/bin/ld
CC=	cc 
CPP=	cpp

S=	../..
I386=	../../i386

INCLUDES= -I. -I$S -I$S/sys
COPTS=	${INCLUDES} ${IDENT} -DKERNEL -Di386 -DNPX
ASFLAGS=
CFLAGS=	-O ${COPTS}

NORMAL_C= ${CC} -c ${CFLAGS} ${PROF} $<
NORMAL_C_C= ${CC} -c ${CFLAGS} ${PROF} ${PARAM} $<
DRIVER_C= ${CC} -c ${CFLAGS} ${PROF} $<
DRIVER_C_C= ${CC} -c ${CFLAGS} ${PROF} ${PARAM} $<
SYSTEM_OBJS=locore.o ${OBJS} param.o ioconf.o conf.o vers.o
SYSTEM_DEP=Makefile symbols.sort ${SYSTEM_OBJS}
SYSTEM_LD_HEAD= 	@echo loading $@; rm -f $@ ; /sys/conf/newvers.sh ; cc -c vers.c
SYSTEM_LD= @${LD} -z -T FE000000 -o $@ -X ${SYSTEM_OBJS}
SYSTEM_LD_TAIL= @echo rearranging symbols; cp $@ orig.$@ ;symorder -t symbols.sort $@; size $@; chmod 755 $@

OBJS=init_main.o init_sysent.o kern_acct.o kern_clock.o kern_descrip.o \
	kern_execve.o kern_exit.o kern_fork.o kern_kinfo.o kern_ktrace.o \
	kern_malloc.o kern__physio.o kern_proc.o kern_prot.o \
	kern_resource.o kern_sig.o kern_subr.o kern_synch.o kern_time.o \
	kern_xxx.o dead_vnops.o spec_vnops.o subr_log.o subr_prf.o \
	subr_rlist.o subr_xxx.o sys_generic.o sys_process.o sys_socket.o \
	tty.o tty_compat.o tty_conf.o tty_pty.o tty_ring.o tty_tty.o \
	vfs__bio.o vfs_cache.o vfs_conf.o vfs_lookup.o vfs_subr.o \
	vfs_syscalls.o vfs_vnops.o uipc_domain.o uipc_mbuf.o \
	uipc_proto.o uipc_socket.o uipc_socket2.o uipc_syscalls.o \
	uipc_usrreq.o kern_lock.o vm_fault.o vm_glue.o vm_init.o \
	vm_kern.o vm_map.o vm_meter.o vm_mmap.o vm_object.o vm_page.o \
	vm_pageout.o vm_pager.o vm_swap.o vm_unix.o vm_user.o \
	device_pager.o swap_pager.o vnode_pager.o ufs_alloc.o ufs_bmap.o \
	ufs_disksubr.o ufs_inode.o ufs_lockf.o ufs_lookup.o ufs_subr.o \
	ufs_tables.o ufs_vfsops.o ufs_vnops.o nfs_bio.o nfs_node.o \
	nfs_serv.o nfs_socket.o nfs_srvcache.o nfs_subs.o nfs_syscalls.o \
	nfs_vfsops.o nfs_vnops.o af.o if.o if_ethersubr.o if_loop.o \
	if_sl.o raw_cb.o raw_usrreq.o radix.o route.o rtsock.o \
	slcompress.o if_ether.o in.o in_pcb.o in_proto.o ip_icmp.o \
	ip_input.o ip_output.o raw_ip.o tcp_debug.o tcp_input.o \
	tcp_output.o tcp_subr.o tcp_timer.o tcp_usrreq.o udp_usrreq.o \
	isofs_bmap.o isofs_lookup.o isofs_node.o isofs_util.o \
	isofs_vfsops.o isofs_vnops.o autoconf.o cons.o pccons.o clock.o \
	in_cksum.o machdep.o math_emulate.o mem.o pmap.o sys_machdep.o \
	trap.o vm_machdep.o if_ne.o if_we.o if_ec.o if_is.o wd.o fd.o \
	wt.o isa.o com.o npx.o as.o 

CFILES=$S/kern/init_main.c $S/kern/init_sysent.c $S/kern/kern_acct.c \
	$S/kern/kern_clock.c $S/kern/kern_descrip.c \
	$S/kern/kern_execve.c $S/kern/kern_exit.c $S/kern/kern_fork.c \
	$S/kern/kern_kinfo.c $S/kern/kern_ktrace.c $S/kern/kern_malloc.c \
	$S/kern/kern__physio.c $S/kern/kern_proc.c $S/kern/kern_prot.c \
	$S/kern/kern_resource.c $S/kern/kern_sig.c $S/kern/kern_subr.c \
	$S/kern/kern_synch.c $S/kern/kern_time.c $S/kern/kern_xxx.c \
	$S/kern/dead_vnops.c $S/kern/spec_vnops.c $S/kern/subr_log.c \
	$S/kern/subr_prf.c $S/kern/subr_rlist.c $S/kern/subr_xxx.c \
	$S/kern/sys_generic.c $S/kern/sys_process.c $S/kern/sys_socket.c \
	$S/kern/tty.c $S/kern/tty_compat.c $S/kern/tty_conf.c \
	$S/kern/tty_pty.c $S/kern/tty_ring.c $S/kern/tty_tty.c \
	$S/kern/vfs__bio.c $S/kern/vfs_cache.c $S/kern/vfs_conf.c \
	$S/kern/vfs_lookup.c $S/kern/vfs_subr.c $S/kern/vfs_syscalls.c \
	$S/kern/vfs_vnops.c $S/kern/uipc_domain.c $S/kern/uipc_mbuf.c \
	$S/kern/uipc_proto.c $S/kern/uipc_socket.c \
	$S/kern/uipc_socket2.c $S/kern/uipc_syscalls.c \
	$S/kern/uipc_usrreq.c $S/vm/kern_lock.c $S/vm/vm_fault.c \
	$S/vm/vm_glue.c $S/vm/vm_init.c $S/vm/vm_kern.c $S/vm/vm_map.c \
	$S/vm/vm_meter.c $S/vm/vm_mmap.c $S/vm/vm_object.c \
	$S/vm/vm_page.c $S/vm/vm_pageout.c $S/vm/vm_pager.c \
	$S/vm/vm_swap.c $S/vm/vm_unix.c $S/vm/vm_user.c \
	$S/vm/device_pager.c $S/vm/swap_pager.c $S/vm/vnode_pager.c \
	$S/ufs/ufs_alloc.c $S/ufs/ufs_bmap.c $S/ufs/ufs_disksubr.c \
	$S/ufs/ufs_inode.c $S/ufs/ufs_lockf.c $S/ufs/ufs_lookup.c \
	$S/ufs/ufs_subr.c $S/ufs/ufs_tables.c $S/ufs/ufs_vfsops.c \
	$S/ufs/ufs_vnops.c $S/nfs/nfs_bio.c $S/nfs/nfs_node.c \
	$S/nfs/nfs_serv.c $S/nfs/nfs_socket.c $S/nfs/nfs_srvcache.c \
	$S/nfs/nfs_subs.c $S/nfs/nfs_syscalls.c $S/nfs/nfs_vfsops.c \
	$S/nfs/nfs_vnops.c $S/net/af.c $S/net/if.c $S/net/if_ethersubr.c \
	$S/net/if_loop.c $S/net/if_sl.c $S/net/raw_cb.c \
	$S/net/raw_usrreq.c $S/net/radix.c $S/net/route.c \
	$S/net/rtsock.c $S/net/slcompress.c $S/netinet/if_ether.c \
	$S/netinet/in.c $S/netinet/in_pcb.c $S/netinet/in_proto.c \
	$S/netinet/ip_icmp.c $S/netinet/ip_input.c \
	$S/netinet/ip_output.c $S/netinet/raw_ip.c \
	$S/netinet/tcp_debug.c $S/netinet/tcp_input.c \
	$S/netinet/tcp_output.c $S/netinet/tcp_subr.c \
	$S/netinet/tcp_timer.c $S/netinet/tcp_usrreq.c \
	$S/netinet/udp_usrreq.c $S/isofs/isofs_bmap.c \
	$S/isofs/isofs_lookup.c $S/isofs/isofs_node.c \
	$S/isofs/isofs_util.c $S/isofs/isofs_vfsops.c \
	$S/isofs/isofs_vnops.c $S/i386/i386/autoconf.c \
	$S/i386/i386/cons.c $S/i386/isa/pccons.c $S/i386/isa/clock.c \
	$S/i386/i386/in_cksum.c $S/i386/i386/machdep.c \
	$S/i386/i386/math_emulate.c $S/i386/i386/mem.c \
	$S/i386/i386/pmap.c $S/i386/i386/sys_machdep.c \
	$S/i386/i386/trap.c $S/i386/i386/vm_machdep.c \
	$S/i386/isa/if_ne.c $S/i386/isa/if_we.c $S/i386/isa/if_ec.c \
	$S/i386/isa/if_is.c $S/i386/isa/wd.c $S/i386/isa/fd.c \
	$S/i386/isa/wt.c $S/i386/isa/isa.c $S/i386/isa/com.c \
	$S/i386/isa/npx.c $S/i386/isa/as.c swap386bsd.c 

386bsd: ${SYSTEM_DEP} swap386bsd.o
	${SYSTEM_LD_HEAD}
	${SYSTEM_LD} swap386bsd.o
	${SYSTEM_LD_TAIL}

swap386bsd.o: swap386bsd.c
	${NORMAL_C}

all: 386bsd

clean:
	rm -f eddep *386bsd tags *.o locore.i [a-uw-z]*.s \
		errs linterrs makelinks

lint: /tmp param.c
	@lint -hbxn -I. -DGENERIC -Dvolatile= ${COPTS} ${PARAM} \
	  ${I386}/i386/Locore.c ${CFILES} ioconf.c param.c | \
	    grep -v 'struct/union .* never defined' | \
	    grep -v 'possible pointer alignment problem'

symbols.sort: ${I386}/i386/symbols.raw
	grep -v '^#' ${I386}/i386/symbols.raw \
	    | sed 's/^	//' | sort -u > symbols.sort

locore.o: assym.s ${I386}/i386/locore.s machine/trap.h machine/psl.h \
	machine/pte.h vector.s ${I386}/isa/icu.s ${I386}/isa/isa.h \
	${I386}/isa/icu.h
	${CPP} -I. -DLOCORE ${COPTS} ${I386}/i386/locore.s | \
		${AS} ${ASFLAGS} -o locore.o

# the following is necessary because autoconf.o depends on #if GENERIC
autoconf.o: Makefile

# depend on network configuration
af.o uipc_proto.o locore.o: Makefile

# depend on maxusers
assym.s machdep.o: Makefile

# depends on KDB (cons.o also depends on GENERIC)
trap.o cons.o: Makefile

assym.s: $S/sys/param.h machine/pte.h $S/sys/buf.h \
    $S/sys/vmmeter.h \
    $S/sys/proc.h $S/sys/msgbuf.h machine/vmparam.h

assym.s: genassym
	./genassym >assym.s

genassym:
	${CC} ${INCLUDES} -DKERNEL ${IDENT} ${PARAM} \
		 ${I386}/i386/genassym.c -o genassym

depend: assym.s param.c
	sh /usr/bin/mkdep ${COPTS} ${CFILES} ioconf.c
	sh /usr/bin/mkdep -a -p ${INCLUDES} ${IDENT} ${PARAM} ${I386}/i386/genassym.c

links:
	egrep '#if' ${CFILES} | sed -f $S/conf/defines | \
	  sed -e 's/:.*//' -e 's/\.c/.o/' | sort -u > dontlink
	echo ${CFILES} | tr -s ' ' '\12' | sed 's/\.c/.o/' | \
	  sort -u | comm -23 - dontlink | \
	  sed 's,../.*/\(.*.o\),rm -f \1;ln -s ../GENERIC/\1 \1,' > makelinks
	sh makelinks && rm -f dontlink

tags:
	@echo "see $S/kern/Makefile for tags"

ioconf.o: ioconf.c $S/sys/param.h machine/pte.h $S/sys/buf.h \
    ${I386}/isa/isa_device.h ${I386}/isa/isa.h ${I386}/isa/icu.h
	${CC} -c ${CFLAGS} ioconf.c

conf.o: $S/sys/param.h $S/sys/systm.h $S/sys/buf.h $S/sys/ioctl.h \
	$S/sys/tty.h $S/sys/conf.h ${I386}/i386/conf.c
	${CC} -traditional -c ${CFLAGS} ${I386}/i386/conf.c

param.c: $S/conf/param.c
	-rm -f param.c
	cp $S/conf/param.c .

param.o: param.c Makefile
	${CC} -c ${CFLAGS} ${PARAM} param.c

init_main.o: $S/kern/init_main.c
	${NORMAL_C}

init_sysent.o: $S/kern/init_sysent.c
	${NORMAL_C}

kern_acct.o: $S/kern/kern_acct.c
	${NORMAL_C}

kern_clock.o: $S/kern/kern_clock.c
	${NORMAL_C}

kern_descrip.o: $S/kern/kern_descrip.c
	${NORMAL_C}

kern_execve.o: $S/kern/kern_execve.c
	${NORMAL_C}

kern_exit.o: $S/kern/kern_exit.c
	${NORMAL_C}

kern_fork.o: $S/kern/kern_fork.c
	${NORMAL_C}

kern_kinfo.o: $S/kern/kern_kinfo.c
	${NORMAL_C}

kern_ktrace.o: $S/kern/kern_ktrace.c
	${NORMAL_C}

kern_malloc.o: $S/kern/kern_malloc.c
	${NORMAL_C}

kern__physio.o: $S/kern/kern__physio.c
	${NORMAL_C}

kern_proc.o: $S/kern/kern_proc.c
	${NORMAL_C}

kern_prot.o: $S/kern/kern_prot.c
	${NORMAL_C}

kern_resource.o: $S/kern/kern_resource.c
	${NORMAL_C}

kern_sig.o: $S/kern/kern_sig.c
	${NORMAL_C}

kern_subr.o: $S/kern/kern_subr.c
	${NORMAL_C}

kern_synch.o: $S/kern/kern_synch.c
	${NORMAL_C}

kern_time.o: $S/kern/kern_time.c
	${NORMAL_C}

kern_xxx.o: $S/kern/kern_xxx.c
	${NORMAL_C}

dead_vnops.o: $S/kern/dead_vnops.c
	${NORMAL_C}

spec_vnops.o: $S/kern/spec_vnops.c
	${NORMAL_C}

subr_log.o: $S/kern/subr_log.c
	${NORMAL_C}

subr_prf.o: $S/kern/subr_prf.c
	${NORMAL_C}

subr_rlist.o: $S/kern/subr_rlist.c
	${NORMAL_C}

subr_xxx.o: $S/kern/subr_xxx.c
	${NORMAL_C}

sys_generic.o: $S/kern/sys_generic.c
	${NORMAL_C}

sys_process.o: $S/kern/sys_process.c
	${NORMAL_C}

sys_socket.o: $S/kern/sys_socket.c
	${NORMAL_C}

tty.o: $S/kern/tty.c
	${NORMAL_C}

tty_compat.o: $S/kern/tty_compat.c
	${NORMAL_C}

tty_conf.o: $S/kern/tty_conf.c
	${NORMAL_C}

tty_pty.o: $S/kern/tty_pty.c
	${NORMAL_C}

tty_ring.o: $S/kern/tty_ring.c
	${NORMAL_C}

tty_tty.o: $S/kern/tty_tty.c
	${NORMAL_C}

vfs__bio.o: $S/kern/vfs__bio.c
	${NORMAL_C}

vfs_cache.o: $S/kern/vfs_cache.c
	${NORMAL_C}

vfs_conf.o: $S/kern/vfs_conf.c
	${NORMAL_C}

vfs_lookup.o: $S/kern/vfs_lookup.c
	${NORMAL_C}

vfs_subr.o: $S/kern/vfs_subr.c
	${NORMAL_C}

vfs_syscalls.o: $S/kern/vfs_syscalls.c
	${NORMAL_C}

vfs_vnops.o: $S/kern/vfs_vnops.c
	${NORMAL_C}

uipc_domain.o: $S/kern/uipc_domain.c
	${NORMAL_C}

uipc_mbuf.o: $S/kern/uipc_mbuf.c
	${NORMAL_C}

uipc_proto.o: $S/kern/uipc_proto.c
	${NORMAL_C}

uipc_socket.o: $S/kern/uipc_socket.c
	${NORMAL_C}

uipc_socket2.o: $S/kern/uipc_socket2.c
	${NORMAL_C}

uipc_syscalls.o: $S/kern/uipc_syscalls.c
	${NORMAL_C}

uipc_usrreq.o: $S/kern/uipc_usrreq.c
	${NORMAL_C}

kern_lock.o: $S/vm/kern_lock.c
	${NORMAL_C}

vm_fault.o: $S/vm/vm_fault.c
	${NORMAL_C}

vm_glue.o: $S/vm/vm_glue.c
	${NORMAL_C}

vm_init.o: $S/vm/vm_init.c
	${NORMAL_C}

vm_kern.o: $S/vm/vm_kern.c
	${NORMAL_C}

vm_map.o: $S/vm/vm_map.c
	${NORMAL_C}

vm_meter.o: $S/vm/vm_meter.c
	${NORMAL_C}

vm_mmap.o: $S/vm/vm_mmap.c
	${NORMAL_C}

vm_object.o: $S/vm/vm_object.c
	${NORMAL_C}

vm_page.o: $S/vm/vm_page.c
	${NORMAL_C}

vm_pageout.o: $S/vm/vm_pageout.c
	${NORMAL_C}

vm_pager.o: $S/vm/vm_pager.c
	${NORMAL_C}

vm_swap.o: $S/vm/vm_swap.c
	${NORMAL_C}

vm_unix.o: $S/vm/vm_unix.c
	${NORMAL_C}

vm_user.o: $S/vm/vm_user.c
	${NORMAL_C}

device_pager.o: $S/vm/device_pager.c
	${NORMAL_C}

swap_pager.o: $S/vm/swap_pager.c
	${NORMAL_C}

vnode_pager.o: $S/vm/vnode_pager.c
	${NORMAL_C}

ufs_alloc.o: $S/ufs/ufs_alloc.c
	${NORMAL_C}

ufs_bmap.o: $S/ufs/ufs_bmap.c
	${NORMAL_C}

ufs_disksubr.o: $S/ufs/ufs_disksubr.c
	${NORMAL_C}

ufs_inode.o: $S/ufs/ufs_inode.c
	${NORMAL_C}

ufs_lockf.o: $S/ufs/ufs_lockf.c
	${NORMAL_C}

ufs_lookup.o: $S/ufs/ufs_lookup.c
	${NORMAL_C}

ufs_subr.o: $S/ufs/ufs_subr.c
	${NORMAL_C}

ufs_tables.o: $S/ufs/ufs_tables.c
	${NORMAL_C}

ufs_vfsops.o: $S/ufs/ufs_vfsops.c
	${NORMAL_C}

ufs_vnops.o: $S/ufs/ufs_vnops.c
	${NORMAL_C}

nfs_bio.o: $S/nfs/nfs_bio.c
	${NORMAL_C}

nfs_node.o: $S/nfs/nfs_node.c
	${NORMAL_C}

nfs_serv.o: $S/nfs/nfs_serv.c
	${NORMAL_C}

nfs_socket.o: $S/nfs/nfs_socket.c
	${NORMAL_C}

nfs_srvcache.o: $S/nfs/nfs_srvcache.c
	${NORMAL_C}

nfs_subs.o: $S/nfs/nfs_subs.c
	${NORMAL_C}

nfs_syscalls.o: $S/nfs/nfs_syscalls.c
	${NORMAL_C}

nfs_vfsops.o: $S/nfs/nfs_vfsops.c
	${NORMAL_C}

nfs_vnops.o: $S/nfs/nfs_vnops.c
	${NORMAL_C}

af.o: $S/net/af.c
	${NORMAL_C}

if.o: $S/net/if.c
	${NORMAL_C}

if_ethersubr.o: $S/net/if_ethersubr.c
	${NORMAL_C}

if_loop.o: $S/net/if_loop.c
	${NORMAL_C}

if_sl.o: $S/net/if_sl.c
	${NORMAL_C}

raw_cb.o: $S/net/raw_cb.c
	${NORMAL_C}

raw_usrreq.o: $S/net/raw_usrreq.c
	${NORMAL_C}

radix.o: $S/net/radix.c
	${NORMAL_C}

route.o: $S/net/route.c
	${NORMAL_C}

rtsock.o: $S/net/rtsock.c
	${NORMAL_C}

slcompress.o: $S/net/slcompress.c
	${NORMAL_C}

if_ether.o: $S/netinet/if_ether.c
	${NORMAL_C}

in.o: $S/netinet/in.c
	${NORMAL_C}

in_pcb.o: $S/netinet/in_pcb.c
	${NORMAL_C}

in_proto.o: $S/netinet/in_proto.c
	${NORMAL_C}

ip_icmp.o: $S/netinet/ip_icmp.c
	${NORMAL_C}

ip_input.o: $S/netinet/ip_input.c
	${NORMAL_C}

ip_output.o: $S/netinet/ip_output.c
	${NORMAL_C}

raw_ip.o: $S/netinet/raw_ip.c
	${NORMAL_C}

tcp_debug.o: $S/netinet/tcp_debug.c
	${NORMAL_C}

tcp_input.o: $S/netinet/tcp_input.c
	${NORMAL_C}

tcp_output.o: $S/netinet/tcp_output.c
	${NORMAL_C}

tcp_subr.o: $S/netinet/tcp_subr.c
	${NORMAL_C}

tcp_timer.o: $S/netinet/tcp_timer.c
	${NORMAL_C}

tcp_usrreq.o: $S/netinet/tcp_usrreq.c
	${NORMAL_C}

udp_usrreq.o: $S/netinet/udp_usrreq.c
	${NORMAL_C}

isofs_bmap.o: $S/isofs/isofs_bmap.c
	${NORMAL_C}

isofs_lookup.o: $S/isofs/isofs_lookup.c
	${NORMAL_C}

isofs_node.o: $S/isofs/isofs_node.c
	${NORMAL_C}

isofs_util.o: $S/isofs/isofs_util.c
	${NORMAL_C}

isofs_vfsops.o: $S/isofs/isofs_vfsops.c
	${NORMAL_C}

isofs_vnops.o: $S/isofs/isofs_vnops.c
	${NORMAL_C}

autoconf.o: $S/i386/i386/autoconf.c
	${DRIVER_C}

cons.o: $S/i386/i386/cons.c
	${NORMAL_C}

pccons.o: $S/i386/isa/pccons.c
	${DRIVER_C}

clock.o: $S/i386/isa/clock.c
	${NORMAL_C}

in_cksum.o: $S/i386/i386/in_cksum.c
	${NORMAL_C}

machdep.o: $S/i386/i386/machdep.c
	${NORMAL_C_C}

math_emulate.o: $S/i386/i386/math_emulate.c
	${NORMAL_C}

mem.o: $S/i386/i386/mem.c
	${NORMAL_C}

pmap.o: $S/i386/i386/pmap.c
	${NORMAL_C}

sys_machdep.o: $S/i386/i386/sys_machdep.c
	${NORMAL_C}

trap.o: $S/i386/i386/trap.c
	${NORMAL_C}

vm_machdep.o: $S/i386/i386/vm_machdep.c
	${NORMAL_C}

if_ne.o: $S/i386/isa/if_ne.c
	${DRIVER_C}

if_we.o: $S/i386/isa/if_we.c
	${DRIVER_C}

if_ec.o: $S/i386/isa/if_ec.c
	${DRIVER_C}

if_is.o: $S/i386/isa/if_is.c
	${DRIVER_C}

wd.o: $S/i386/isa/wd.c
	${DRIVER_C}

fd.o: $S/i386/isa/fd.c
	${DRIVER_C}

wt.o: $S/i386/isa/wt.c
	${DRIVER_C}

isa.o: $S/i386/isa/isa.c
	${DRIVER_C}

com.o: $S/i386/isa/com.c
	${DRIVER_C}

npx.o: $S/i386/isa/npx.c
	${DRIVER_C}

as.o: $S/i386/isa/as.c
	${DRIVER_C}


# DO NOT DELETE THIS LINE -- make depend uses it

