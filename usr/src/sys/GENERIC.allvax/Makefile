IDENT=-DGENERIC -DVAX650 -DVAX630 -DVAX730 -DVAX750 -DVAX780 -DVAX8200 -DVAX8600 -DKTRACE -DCOMPAT_43 -DTCP_COMPAT_42 -DDIAGNOSTIC -DMFS -DNFS -DFIFO -DQUOTA -DKADB -DNS -DINET
PARAM=-DTIMEZONE=480 -DDST=1 -DMAXUSERS=8
INLINEOPTS=-s
#
# Copyright (c) 1980, 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile.vax	7.16 (Berkeley) 6/24/90
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
C2=	/usr/libexec/c2
LD=	/usr/bin/ld
INLINECMD= ../vax/inline/inline
INLINE=	${INLINECMD} ${INLINEOPTS}
TOUCH=	touch -f -c

INCLUDES= -I. -I../sys
COPTS=	${INCLUDES} ${IDENT} -DKERNEL
CFLAGS=	-O ${COPTS}

AHEADS=	../vax/pcb.m

OBJS=init_main.o init_sysent.o kern_acct.o kern_clock.o kern_descrip.o \
	kern_exec.o kern_exit.o kern_fork.o kern_kinfo.o kern_ktrace.o \
	kern_malloc.o kern_mman.o kern_proc.o kern_prot.o \
	kern_resource.o kern_sig.o kern_subr.o kern_synch.o kern_time.o \
	kern_xxx.o dead_vnops.o spec_vnops.o fifo_vnops.o subr_log.o \
	subr_prf.o subr_rmap.o subr_xxx.o sys_generic.o sys_process.o \
	sys_socket.o tty.o tty_compat.o tty_conf.o tty_pty.o tty_subr.o \
	tty_tty.o vfs_bio.o vfs_cache.o vfs_conf.o vfs_lookup.o \
	vfs_subr.o vfs_syscalls.o vfs_vnops.o uipc_domain.o uipc_mbuf.o \
	uipc_proto.o uipc_socket.o uipc_socket2.o uipc_syscalls.o \
	uipc_usrreq.o vm_drum.o vm_mem.o vm_mon.o vm_page.o vm_proc.o \
	vm_pt.o vm_sched.o vm_subr.o vm_sw.o vm_swap.o vm_swp.o \
	vm_text.o ufs_alloc.o ufs_bmap.o ufs_disksubr.o ufs_inode.o \
	ufs_lookup.o ufs_subr.o ufs_tables.o ufs_vfsops.o ufs_vnops.o \
	ufs_quota.o mfs_vnops.o mfs_vfsops.o nfs_bio.o nfs_node.o \
	nfs_serv.o nfs_socket.o nfs_srvcache.o nfs_subs.o nfs_syscalls.o \
	nfs_vfsops.o nfs_vnops.o kdb_access.o kdb_command.o kdb_ctype.o \
	kdb_expr.o kdb_format.o kdb_input.o kdb_message.o kdb_output.o \
	kdb_pcs.o kdb_print.o kdb_runpcs.o kdb_sym.o kdb_trap.o af.o \
	if.o if_ethersubr.o if_loop.o raw_cb.o raw_usrreq.o radix.o \
	route.o rtsock.o if_ether.o in.o in_pcb.o in_proto.o ip_icmp.o \
	ip_input.o ip_output.o raw_ip.o tcp_debug.o tcp_input.o \
	tcp_output.o tcp_subr.o tcp_timer.o tcp_usrreq.o udp_usrreq.o \
	idp_usrreq.o ns.o ns_error.o ns_ip.o ns_input.o ns_output.o \
	ns_pcb.o ns_proto.o spp_debug.o spp_usrreq.o autoconf.o clock.o \
	conf.o cons.o cpudata.o crl.o dkbad.o flp.o in_cksum.o ka650.o \
	ka630.o ka730.o ka750.o ka780.o ka820.o ka860.o kdb_machdep.o \
	kdb_opset.o machdep.o mem.o mscp.o ns_cksum.o rx50.o \
	sys_machdep.o trap.o tu.o udiv.o ufs_machdep.o urem.o \
	vm_machdep.o bi.o kdb.o if_de.o if_ec.o if_ex.o if_il.o if_qe.o \
	if_uba.o hp.o ht.o mba.o mt.o dh.o dhu.o dmf.o dmx.o dmz.o dz.o \
	idc.o lp.o rk.o rl.o tm.o tmscp.o ts.o uba.o uda.o up.o ut.o \
	qv.o qfont.o qd.o 

CFILES=../kern/init_main.c ../kern/init_sysent.c ../kern/kern_acct.c \
	../kern/kern_clock.c ../kern/kern_descrip.c ../kern/kern_exec.c \
	../kern/kern_exit.c ../kern/kern_fork.c ../kern/kern_kinfo.c \
	../kern/kern_ktrace.c ../kern/kern_malloc.c ../kern/kern_mman.c \
	../kern/kern_proc.c ../kern/kern_prot.c ../kern/kern_resource.c \
	../kern/kern_sig.c ../kern/kern_subr.c ../kern/kern_synch.c \
	../kern/kern_time.c ../kern/kern_xxx.c ../kern/dead_vnops.c \
	../kern/spec_vnops.c ../kern/fifo_vnops.c ../kern/subr_log.c \
	../kern/subr_prf.c ../kern/subr_rmap.c ../kern/subr_xxx.c \
	../kern/sys_generic.c ../kern/sys_process.c ../kern/sys_socket.c \
	../kern/tty.c ../kern/tty_compat.c ../kern/tty_conf.c \
	../kern/tty_pty.c ../kern/tty_subr.c ../kern/tty_tty.c \
	../kern/vfs_bio.c ../kern/vfs_cache.c ../kern/vfs_conf.c \
	../kern/vfs_lookup.c ../kern/vfs_subr.c ../kern/vfs_syscalls.c \
	../kern/vfs_vnops.c ../kern/uipc_domain.c ../kern/uipc_mbuf.c \
	../kern/uipc_proto.c ../kern/uipc_socket.c \
	../kern/uipc_socket2.c ../kern/uipc_syscalls.c \
	../kern/uipc_usrreq.c ../kern/vm_drum.c ../kern/vm_mem.c \
	../kern/vm_mon.c ../kern/vm_page.c ../kern/vm_proc.c \
	../kern/vm_pt.c ../kern/vm_sched.c ../kern/vm_subr.c \
	../kern/vm_sw.c ../kern/vm_swap.c ../kern/vm_swp.c \
	../kern/vm_text.c ../ufs/ufs_alloc.c ../ufs/ufs_bmap.c \
	../ufs/ufs_disksubr.c ../ufs/ufs_inode.c ../ufs/ufs_lookup.c \
	../ufs/ufs_subr.c ../ufs/ufs_tables.c ../ufs/ufs_vfsops.c \
	../ufs/ufs_vnops.c ../ufs/ufs_quota.c ../ufs/mfs_vnops.c \
	../ufs/mfs_vfsops.c ../nfs/nfs_bio.c ../nfs/nfs_node.c \
	../nfs/nfs_serv.c ../nfs/nfs_socket.c ../nfs/nfs_srvcache.c \
	../nfs/nfs_subs.c ../nfs/nfs_syscalls.c ../nfs/nfs_vfsops.c \
	../nfs/nfs_vnops.c ../kdb/kdb_access.c ../kdb/kdb_command.c \
	../kdb/kdb_ctype.c ../kdb/kdb_expr.c ../kdb/kdb_format.c \
	../kdb/kdb_input.c ../kdb/kdb_message.c ../kdb/kdb_output.c \
	../kdb/kdb_pcs.c ../kdb/kdb_print.c ../kdb/kdb_runpcs.c \
	../kdb/kdb_sym.c ../kdb/kdb_trap.c ../net/af.c ../net/if.c \
	../net/if_ethersubr.c ../net/if_loop.c ../net/raw_cb.c \
	../net/raw_usrreq.c ../net/radix.c ../net/route.c \
	../net/rtsock.c ../netinet/if_ether.c ../netinet/in.c \
	../netinet/in_pcb.c ../netinet/in_proto.c ../netinet/ip_icmp.c \
	../netinet/ip_input.c ../netinet/ip_output.c ../netinet/raw_ip.c \
	../netinet/tcp_debug.c ../netinet/tcp_input.c \
	../netinet/tcp_output.c ../netinet/tcp_subr.c \
	../netinet/tcp_timer.c ../netinet/tcp_usrreq.c \
	../netinet/udp_usrreq.c ../netns/idp_usrreq.c ../netns/ns.c \
	../netns/ns_error.c ../netns/ns_ip.c ../netns/ns_input.c \
	../netns/ns_output.c ../netns/ns_pcb.c ../netns/ns_proto.c \
	../netns/spp_debug.c ../netns/spp_usrreq.c ../vax/autoconf.c \
	../vax/clock.c ../vax/conf.c ../vax/cons.c ../vax/cpudata.c \
	../vax/crl.c ../vax/dkbad.c ../vax/flp.c ../vax/in_cksum.c \
	../vax/ka650.c ../vax/ka630.c ../vax/ka730.c ../vax/ka750.c \
	../vax/ka780.c ../vax/ka820.c ../vax/ka860.c \
	../vax/kdb_machdep.c ../vax/kdb_opset.c ../vax/machdep.c \
	../vax/mem.c ../vax/mscp.c ../vax/ns_cksum.c ../vax/rx50.c \
	../vax/sys_machdep.c ../vax/trap.c ../vax/tu.c \
	../vax/ufs_machdep.c ../vax/vm_machdep.c ../vaxbi/bi.c \
	../vaxbi/kdb.c ../vaxif/if_de.c ../vaxif/if_ec.c \
	../vaxif/if_ex.c ../vaxif/if_il.c ../vaxif/if_qe.c \
	../vaxif/if_uba.c ../vaxmba/hp.c ../vaxmba/ht.c ../vaxmba/mba.c \
	../vaxmba/mt.c ../vaxuba/dh.c ../vaxuba/dhu.c ../vaxuba/dmf.c \
	../vaxuba/dmx.c ../vaxuba/dmz.c ../vaxuba/dz.c ../vaxuba/idc.c \
	../vaxuba/lp.c ../vaxuba/rk.c ../vaxuba/rl.c ../vaxuba/tm.c \
	../vaxuba/tmscp.c ../vaxuba/ts.c ../vaxuba/uba.c ../vaxuba/uda.c \
	../vaxuba/up.c ../vaxuba/ut.c ../vaxuba/qv.c ../vaxuba/qfont.c \
	../vaxuba/qd.c ../vax/swapgeneric.c swaphkvmunix.c \
	swaphpvmunix.c 

vmunix: Makefile machine/symbols.sort ${INLINECMD} locore.o emulate.o ${OBJS} param.o ioconf.o swapgeneric.o
	@echo loading vmunix
	@rm -f vmunix
	@sh ../conf/newvers.sh
	@${CC} ${CFLAGS} -c vers.c
	@${LD} -n -o vmunix -e start -x -T 80000000 locore.o emulate.o ${OBJS} vers.o ioconf.o param.o swapgeneric.o
	@echo rearranging symbols
	@-symorder machine/symbols.sort vmunix
	@size vmunix
	@chmod 755 vmunix

swapgeneric.o: ../vax/swapgeneric.c
	${CC} -c -S ${COPTS} ../vax/swapgeneric.c
	${C2} swapgeneric.s | ${INLINE} | ${AS} -o swapgeneric.o
	rm -f swapgeneric.s

hkvmunix: Makefile machine/symbols.sort ${INLINECMD} locore.o emulate.o ${OBJS} param.o ioconf.o swaphkvmunix.o
	@echo loading hkvmunix
	@rm -f hkvmunix
	@${LD} -n -o hkvmunix -e start -x -T 80000000 locore.o emulate.o ${OBJS} vers.o ioconf.o param.o swaphkvmunix.o
	@echo rearranging symbols
	@-symorder machine/symbols.sort hkvmunix
	@size hkvmunix
	@chmod 755 hkvmunix

swaphkvmunix.o: swaphkvmunix.c
	${CC} -c -O ${COPTS} swaphkvmunix.c

hpvmunix: Makefile machine/symbols.sort ${INLINECMD} locore.o emulate.o ${OBJS} param.o ioconf.o swaphpvmunix.o
	@echo loading hpvmunix
	@rm -f hpvmunix
	@${LD} -n -o hpvmunix -e start -x -T 80000000 locore.o emulate.o ${OBJS} vers.o ioconf.o param.o swaphpvmunix.o
	@echo rearranging symbols
	@-symorder machine/symbols.sort hpvmunix
	@size hpvmunix
	@chmod 755 hpvmunix

swaphpvmunix.o: swaphpvmunix.c
	${CC} -c -O ${COPTS} swaphpvmunix.c

all: vmunix hkvmunix hpvmunix

clean:
	rm -f eddep *vmunix tags *.o locore.i [a-tv-z]*.s \
		errs linterrs makelinks

lint: /tmp param.c
	@lint -hbxn -DGENERIC ${COPTS} ${PARAM} \
	  ../vax/Locore.c ${CFILES} ioconf.c param.c | \
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
    ../vax/locore.s ubglue.s ../vax/mtpr.h ../vax/trap.h machine/psl.h \
    machine/pte.h ../vax/cpu.h mba.h
	cat assym.s ../vax/rpb.s ../vax/scb.s ubvec.s ../vax/locore.s \
	    ubglue.s > locore.c
	cc -E -DLOCORE -I../vax ${COPTS} locore.c > locore.i
	@echo 'as -o locore.o $${AHEADS} locore.i'
	@as -o locore.o ${AHEADS} locore.i
	@rm locore.i

emulate.o: ../vax/emulate.s
	cpp -I. ${COPTS} ../vax/emulate.s | ${AS} -o emulate.o

# the following is necessary because autoconf.o depends on #if GENERIC
autoconf.o tu.o: Makefile

# the following are necessary because the files depend on the types of
# vax cpu's included in the system configuration
clock.o machdep.o autoconf.o conf.o cons.o cpudata.o : Makefile
crl.o flp.o mba.o uba.o vaxcpu.o : Makefile

# depend on network configuration
af.o uipc_proto.o uipc_domain.o locore.o: Makefile

# depends on kdb being configured
trap.o:	Makefile

# depend on maxusers
assym.s: Makefile

assym.s: genassym
	./genassym >assym.s

genassym:
	cc ${INCLUDES} ${IDENT} ${PARAM} -o genassym ../vax/genassym.c

depend: assym.s
	mkdep ${COPTS} ${CFILES} ioconf.c
	mkdep -a -p ${INCLUDES} ${IDENT} ${PARAM} ../vax/genassym.c

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
	@echo "see ../kern/Makefile for tags"

print:
	@pr -f Makefile ../kern/TODO linterrs
	@/usr/ucb/ctags -x ${CFILES} | pr -f -h XREF
	@(size vmunix *.o) | pr -f -h sizes
	@ls -ls | pr -f
	@cd ../kern; ls -ls | pr -f ; pr -f *.m *.h
	@echo 'once upon a time ...'
	@cd ../vax; pr -f Locore.c scb.s locore.s
	@cd ../vax/inline; make print
	@pr -f ${CFILES}

ioconf.o: ioconf.c ../sys/param.h machine/pte.h ../sys/buf.h ../sys/map.h \
    ../vaxmba/mbavar.h ../sys/vm.h ../vaxuba/ubavar.h
	${CC} -c -S ${COPTS} ioconf.c
	${C2} ioconf.s | ../vax/inline/inline | ${AS} -o ioconf.o
	rm -f ioconf.s

param.c: ../conf/param.c
	-rm -f param.c
	cp ../conf/param.c .

param.o: param.c Makefile
	${CC} -c ${CFLAGS} ${PARAM} param.c

init_main.o: ../kern/init_main.c
	${CC} -c -S ${COPTS} ../kern/init_main.c
	${C2} init_main.s | ${INLINE} | ${AS} -o init_main.o
	rm -f init_main.s

init_sysent.o: ../kern/init_sysent.c
	${CC} -c -S ${COPTS} ../kern/init_sysent.c
	${C2} init_sysent.s | ${INLINE} | ${AS} -o init_sysent.o
	rm -f init_sysent.s

kern_acct.o: ../kern/kern_acct.c
	${CC} -c -S ${COPTS} ../kern/kern_acct.c
	${C2} kern_acct.s | ${INLINE} | ${AS} -o kern_acct.o
	rm -f kern_acct.s

kern_clock.o: ../kern/kern_clock.c
	${CC} -c -S ${COPTS} ../kern/kern_clock.c
	${C2} kern_clock.s | ${INLINE} | ${AS} -o kern_clock.o
	rm -f kern_clock.s

kern_descrip.o: ../kern/kern_descrip.c
	${CC} -c -S ${COPTS} ../kern/kern_descrip.c
	${C2} kern_descrip.s | ${INLINE} | ${AS} -o kern_descrip.o
	rm -f kern_descrip.s

kern_exec.o: ../kern/kern_exec.c
	${CC} -c -S ${COPTS} ../kern/kern_exec.c
	${C2} kern_exec.s | ${INLINE} | ${AS} -o kern_exec.o
	rm -f kern_exec.s

kern_exit.o: ../kern/kern_exit.c
	${CC} -c -S ${COPTS} ../kern/kern_exit.c
	${C2} kern_exit.s | ${INLINE} | ${AS} -o kern_exit.o
	rm -f kern_exit.s

kern_fork.o: ../kern/kern_fork.c
	${CC} -c -S ${COPTS} ../kern/kern_fork.c
	${C2} kern_fork.s | ${INLINE} | ${AS} -o kern_fork.o
	rm -f kern_fork.s

kern_kinfo.o: ../kern/kern_kinfo.c
	${CC} -c -S ${COPTS} ../kern/kern_kinfo.c
	${C2} kern_kinfo.s | ${INLINE} | ${AS} -o kern_kinfo.o
	rm -f kern_kinfo.s

kern_ktrace.o: ../kern/kern_ktrace.c
	${CC} -c -S ${COPTS} ../kern/kern_ktrace.c
	${C2} kern_ktrace.s | ${INLINE} | ${AS} -o kern_ktrace.o
	rm -f kern_ktrace.s

kern_malloc.o: ../kern/kern_malloc.c
	${CC} -c -S ${COPTS} ../kern/kern_malloc.c
	${C2} kern_malloc.s | ${INLINE} | ${AS} -o kern_malloc.o
	rm -f kern_malloc.s

kern_mman.o: ../kern/kern_mman.c
	${CC} -c -S ${COPTS} ../kern/kern_mman.c
	${C2} kern_mman.s | ${INLINE} | ${AS} -o kern_mman.o
	rm -f kern_mman.s

kern_proc.o: ../kern/kern_proc.c
	${CC} -c -S ${COPTS} ../kern/kern_proc.c
	${C2} kern_proc.s | ${INLINE} | ${AS} -o kern_proc.o
	rm -f kern_proc.s

kern_prot.o: ../kern/kern_prot.c
	${CC} -c -S ${COPTS} ../kern/kern_prot.c
	${C2} kern_prot.s | ${INLINE} | ${AS} -o kern_prot.o
	rm -f kern_prot.s

kern_resource.o: ../kern/kern_resource.c
	${CC} -c -S ${COPTS} ../kern/kern_resource.c
	${C2} kern_resource.s | ${INLINE} | ${AS} -o kern_resource.o
	rm -f kern_resource.s

kern_sig.o: ../kern/kern_sig.c
	${CC} -c -S ${COPTS} ../kern/kern_sig.c
	${C2} kern_sig.s | ${INLINE} | ${AS} -o kern_sig.o
	rm -f kern_sig.s

kern_subr.o: ../kern/kern_subr.c
	${CC} -c -S ${COPTS} ../kern/kern_subr.c
	${C2} kern_subr.s | ${INLINE} | ${AS} -o kern_subr.o
	rm -f kern_subr.s

kern_synch.o: ../kern/kern_synch.c
	${CC} -c -S ${COPTS} ../kern/kern_synch.c
	${C2} kern_synch.s | ${INLINE} | ${AS} -o kern_synch.o
	rm -f kern_synch.s

kern_time.o: ../kern/kern_time.c
	${CC} -c -S ${COPTS} ../kern/kern_time.c
	${C2} kern_time.s | ${INLINE} | ${AS} -o kern_time.o
	rm -f kern_time.s

kern_xxx.o: ../kern/kern_xxx.c
	${CC} -c -S ${COPTS} ../kern/kern_xxx.c
	${C2} kern_xxx.s | ${INLINE} | ${AS} -o kern_xxx.o
	rm -f kern_xxx.s

dead_vnops.o: ../kern/dead_vnops.c
	${CC} -c -S ${COPTS} ../kern/dead_vnops.c
	${C2} dead_vnops.s | ${INLINE} | ${AS} -o dead_vnops.o
	rm -f dead_vnops.s

spec_vnops.o: ../kern/spec_vnops.c
	${CC} -c -S ${COPTS} ../kern/spec_vnops.c
	${C2} spec_vnops.s | ${INLINE} | ${AS} -o spec_vnops.o
	rm -f spec_vnops.s

fifo_vnops.o: ../kern/fifo_vnops.c
	${CC} -c -S ${COPTS} ../kern/fifo_vnops.c
	${C2} fifo_vnops.s | ${INLINE} | ${AS} -o fifo_vnops.o
	rm -f fifo_vnops.s

subr_log.o: ../kern/subr_log.c
	${CC} -c -S ${COPTS} ../kern/subr_log.c
	${C2} subr_log.s | ${INLINE} | ${AS} -o subr_log.o
	rm -f subr_log.s

subr_prf.o: ../kern/subr_prf.c
	${CC} -c -S ${COPTS} ../kern/subr_prf.c
	${C2} subr_prf.s | ${INLINE} | ${AS} -o subr_prf.o
	rm -f subr_prf.s

subr_rmap.o: ../kern/subr_rmap.c
	${CC} -c -S ${COPTS} ../kern/subr_rmap.c
	${C2} subr_rmap.s | ${INLINE} | ${AS} -o subr_rmap.o
	rm -f subr_rmap.s

subr_xxx.o: ../kern/subr_xxx.c
	${CC} -c -S ${COPTS} ../kern/subr_xxx.c
	${C2} subr_xxx.s | ${INLINE} | ${AS} -o subr_xxx.o
	rm -f subr_xxx.s

sys_generic.o: ../kern/sys_generic.c
	${CC} -c -S ${COPTS} ../kern/sys_generic.c
	${C2} sys_generic.s | ${INLINE} | ${AS} -o sys_generic.o
	rm -f sys_generic.s

sys_process.o: ../kern/sys_process.c
	${CC} -c -S ${COPTS} ../kern/sys_process.c
	${C2} sys_process.s | ${INLINE} | ${AS} -o sys_process.o
	rm -f sys_process.s

sys_socket.o: ../kern/sys_socket.c
	${CC} -c -S ${COPTS} ../kern/sys_socket.c
	${C2} sys_socket.s | ${INLINE} | ${AS} -o sys_socket.o
	rm -f sys_socket.s

tty.o: ../kern/tty.c
	${CC} -c -S ${COPTS} ../kern/tty.c
	${C2} tty.s | ${INLINE} | ${AS} -o tty.o
	rm -f tty.s

tty_compat.o: ../kern/tty_compat.c
	${CC} -c -S ${COPTS} ../kern/tty_compat.c
	${C2} tty_compat.s | ${INLINE} | ${AS} -o tty_compat.o
	rm -f tty_compat.s

tty_conf.o: ../kern/tty_conf.c
	${CC} -c -S ${COPTS} ../kern/tty_conf.c
	${C2} tty_conf.s | ${INLINE} | ${AS} -o tty_conf.o
	rm -f tty_conf.s

tty_pty.o: ../kern/tty_pty.c
	${CC} -c -S ${COPTS} ../kern/tty_pty.c
	${C2} tty_pty.s | ${INLINE} | ${AS} -o tty_pty.o
	rm -f tty_pty.s

tty_subr.o: ../kern/tty_subr.c
	${CC} -c -S ${COPTS} ../kern/tty_subr.c
	${C2} tty_subr.s | ${INLINE} | ${AS} -o tty_subr.o
	rm -f tty_subr.s

tty_tty.o: ../kern/tty_tty.c
	${CC} -c -S ${COPTS} ../kern/tty_tty.c
	${C2} tty_tty.s | ${INLINE} | ${AS} -o tty_tty.o
	rm -f tty_tty.s

vfs_bio.o: ../kern/vfs_bio.c
	${CC} -c -S ${COPTS} ../kern/vfs_bio.c
	${C2} vfs_bio.s | ${INLINE} | ${AS} -o vfs_bio.o
	rm -f vfs_bio.s

vfs_cache.o: ../kern/vfs_cache.c
	${CC} -c -S ${COPTS} ../kern/vfs_cache.c
	${C2} vfs_cache.s | ${INLINE} | ${AS} -o vfs_cache.o
	rm -f vfs_cache.s

vfs_conf.o: ../kern/vfs_conf.c
	${CC} -c -S ${COPTS} ../kern/vfs_conf.c
	${C2} vfs_conf.s | ${INLINE} | ${AS} -o vfs_conf.o
	rm -f vfs_conf.s

vfs_lookup.o: ../kern/vfs_lookup.c
	${CC} -c -S ${COPTS} ../kern/vfs_lookup.c
	${C2} vfs_lookup.s | ${INLINE} | ${AS} -o vfs_lookup.o
	rm -f vfs_lookup.s

vfs_subr.o: ../kern/vfs_subr.c
	${CC} -c -S ${COPTS} ../kern/vfs_subr.c
	${C2} vfs_subr.s | ${INLINE} | ${AS} -o vfs_subr.o
	rm -f vfs_subr.s

vfs_syscalls.o: ../kern/vfs_syscalls.c
	${CC} -c -S ${COPTS} ../kern/vfs_syscalls.c
	${C2} vfs_syscalls.s | ${INLINE} | ${AS} -o vfs_syscalls.o
	rm -f vfs_syscalls.s

vfs_vnops.o: ../kern/vfs_vnops.c
	${CC} -c -S ${COPTS} ../kern/vfs_vnops.c
	${C2} vfs_vnops.s | ${INLINE} | ${AS} -o vfs_vnops.o
	rm -f vfs_vnops.s

uipc_domain.o: ../kern/uipc_domain.c
	${CC} -c -S ${COPTS} ../kern/uipc_domain.c
	${C2} uipc_domain.s | ${INLINE} | ${AS} -o uipc_domain.o
	rm -f uipc_domain.s

uipc_mbuf.o: ../kern/uipc_mbuf.c
	${CC} -c -S ${COPTS} ../kern/uipc_mbuf.c
	${C2} uipc_mbuf.s | ${INLINE} | ${AS} -o uipc_mbuf.o
	rm -f uipc_mbuf.s

uipc_proto.o: ../kern/uipc_proto.c
	${CC} -c -S ${COPTS} ../kern/uipc_proto.c
	${C2} uipc_proto.s | ${INLINE} | ${AS} -o uipc_proto.o
	rm -f uipc_proto.s

uipc_socket.o: ../kern/uipc_socket.c
	${CC} -c -S ${COPTS} ../kern/uipc_socket.c
	${C2} uipc_socket.s | ${INLINE} | ${AS} -o uipc_socket.o
	rm -f uipc_socket.s

uipc_socket2.o: ../kern/uipc_socket2.c
	${CC} -c -S ${COPTS} ../kern/uipc_socket2.c
	${C2} uipc_socket2.s | ${INLINE} | ${AS} -o uipc_socket2.o
	rm -f uipc_socket2.s

uipc_syscalls.o: ../kern/uipc_syscalls.c
	${CC} -c -S ${COPTS} ../kern/uipc_syscalls.c
	${C2} uipc_syscalls.s | ${INLINE} | ${AS} -o uipc_syscalls.o
	rm -f uipc_syscalls.s

uipc_usrreq.o: ../kern/uipc_usrreq.c
	${CC} -c -S ${COPTS} ../kern/uipc_usrreq.c
	${C2} uipc_usrreq.s | ${INLINE} | ${AS} -o uipc_usrreq.o
	rm -f uipc_usrreq.s

vm_drum.o: ../kern/vm_drum.c
	${CC} -c -S ${COPTS} ../kern/vm_drum.c
	${C2} vm_drum.s | ${INLINE} | ${AS} -o vm_drum.o
	rm -f vm_drum.s

vm_mem.o: ../kern/vm_mem.c
	${CC} -c -S ${COPTS} ../kern/vm_mem.c
	${C2} vm_mem.s | ${INLINE} | ${AS} -o vm_mem.o
	rm -f vm_mem.s

vm_mon.o: ../kern/vm_mon.c
	${CC} -c -S ${COPTS} ../kern/vm_mon.c
	${C2} vm_mon.s | ${INLINE} | ${AS} -o vm_mon.o
	rm -f vm_mon.s

vm_page.o: ../kern/vm_page.c
	${CC} -c -S ${COPTS} ../kern/vm_page.c
	${C2} vm_page.s | ${INLINE} | ${AS} -o vm_page.o
	rm -f vm_page.s

vm_proc.o: ../kern/vm_proc.c
	${CC} -c -S ${COPTS} ../kern/vm_proc.c
	${C2} vm_proc.s | ${INLINE} | ${AS} -o vm_proc.o
	rm -f vm_proc.s

vm_pt.o: ../kern/vm_pt.c
	${CC} -c -S ${COPTS} ../kern/vm_pt.c
	${C2} vm_pt.s | ${INLINE} | ${AS} -o vm_pt.o
	rm -f vm_pt.s

vm_sched.o: ../kern/vm_sched.c
	${CC} -c -S ${COPTS} ../kern/vm_sched.c
	${C2} vm_sched.s | ${INLINE} | ${AS} -o vm_sched.o
	rm -f vm_sched.s

vm_subr.o: ../kern/vm_subr.c
	${CC} -c -S ${COPTS} ../kern/vm_subr.c
	${C2} vm_subr.s | ${INLINE} | ${AS} -o vm_subr.o
	rm -f vm_subr.s

vm_sw.o: ../kern/vm_sw.c
	${CC} -c -S ${COPTS} ../kern/vm_sw.c
	${C2} vm_sw.s | ${INLINE} | ${AS} -o vm_sw.o
	rm -f vm_sw.s

vm_swap.o: ../kern/vm_swap.c
	${CC} -c -S ${COPTS} ../kern/vm_swap.c
	${C2} vm_swap.s | ${INLINE} | ${AS} -o vm_swap.o
	rm -f vm_swap.s

vm_swp.o: ../kern/vm_swp.c
	${CC} -c -S ${COPTS} ../kern/vm_swp.c
	${C2} vm_swp.s | ${INLINE} | ${AS} -o vm_swp.o
	rm -f vm_swp.s

vm_text.o: ../kern/vm_text.c
	${CC} -c -S ${COPTS} ../kern/vm_text.c
	${C2} vm_text.s | ${INLINE} | ${AS} -o vm_text.o
	rm -f vm_text.s

ufs_alloc.o: ../ufs/ufs_alloc.c
	${CC} -c -S ${COPTS} ../ufs/ufs_alloc.c
	${C2} ufs_alloc.s | ${INLINE} | ${AS} -o ufs_alloc.o
	rm -f ufs_alloc.s

ufs_bmap.o: ../ufs/ufs_bmap.c
	${CC} -c -S ${COPTS} ../ufs/ufs_bmap.c
	${C2} ufs_bmap.s | ${INLINE} | ${AS} -o ufs_bmap.o
	rm -f ufs_bmap.s

ufs_disksubr.o: ../ufs/ufs_disksubr.c
	${CC} -c -S ${COPTS} ../ufs/ufs_disksubr.c
	${C2} ufs_disksubr.s | ${INLINE} | ${AS} -o ufs_disksubr.o
	rm -f ufs_disksubr.s

ufs_inode.o: ../ufs/ufs_inode.c
	${CC} -c -S ${COPTS} ../ufs/ufs_inode.c
	${C2} ufs_inode.s | ${INLINE} | ${AS} -o ufs_inode.o
	rm -f ufs_inode.s

ufs_lookup.o: ../ufs/ufs_lookup.c
	${CC} -c -S ${COPTS} ../ufs/ufs_lookup.c
	${C2} ufs_lookup.s | ${INLINE} | ${AS} -o ufs_lookup.o
	rm -f ufs_lookup.s

ufs_subr.o: ../ufs/ufs_subr.c
	${CC} -c -S ${COPTS} ../ufs/ufs_subr.c
	${C2} ufs_subr.s | ${INLINE} | ${AS} -o ufs_subr.o
	rm -f ufs_subr.s

ufs_tables.o: ../ufs/ufs_tables.c
	${CC} -c -S ${COPTS} ../ufs/ufs_tables.c
	${C2} ufs_tables.s | ${INLINE} | ${AS} -o ufs_tables.o
	rm -f ufs_tables.s

ufs_vfsops.o: ../ufs/ufs_vfsops.c
	${CC} -c -S ${COPTS} ../ufs/ufs_vfsops.c
	${C2} ufs_vfsops.s | ${INLINE} | ${AS} -o ufs_vfsops.o
	rm -f ufs_vfsops.s

ufs_vnops.o: ../ufs/ufs_vnops.c
	${CC} -c -S ${COPTS} ../ufs/ufs_vnops.c
	${C2} ufs_vnops.s | ${INLINE} | ${AS} -o ufs_vnops.o
	rm -f ufs_vnops.s

ufs_quota.o: ../ufs/ufs_quota.c
	${CC} -c -S ${COPTS} ../ufs/ufs_quota.c
	${C2} ufs_quota.s | ${INLINE} | ${AS} -o ufs_quota.o
	rm -f ufs_quota.s

mfs_vnops.o: ../ufs/mfs_vnops.c
	${CC} -c -S ${COPTS} ../ufs/mfs_vnops.c
	${C2} mfs_vnops.s | ${INLINE} | ${AS} -o mfs_vnops.o
	rm -f mfs_vnops.s

mfs_vfsops.o: ../ufs/mfs_vfsops.c
	${CC} -c -S ${COPTS} ../ufs/mfs_vfsops.c
	${C2} mfs_vfsops.s | ${INLINE} | ${AS} -o mfs_vfsops.o
	rm -f mfs_vfsops.s

nfs_bio.o: ../nfs/nfs_bio.c
	${CC} -c -S ${COPTS} ../nfs/nfs_bio.c
	${C2} nfs_bio.s | ${INLINE} | ${AS} -o nfs_bio.o
	rm -f nfs_bio.s

nfs_node.o: ../nfs/nfs_node.c
	${CC} -c -S ${COPTS} ../nfs/nfs_node.c
	${C2} nfs_node.s | ${INLINE} | ${AS} -o nfs_node.o
	rm -f nfs_node.s

nfs_serv.o: ../nfs/nfs_serv.c
	${CC} -c -S ${COPTS} ../nfs/nfs_serv.c
	${C2} nfs_serv.s | ${INLINE} | ${AS} -o nfs_serv.o
	rm -f nfs_serv.s

nfs_socket.o: ../nfs/nfs_socket.c
	${CC} -c -S ${COPTS} ../nfs/nfs_socket.c
	${C2} nfs_socket.s | ${INLINE} | ${AS} -o nfs_socket.o
	rm -f nfs_socket.s

nfs_srvcache.o: ../nfs/nfs_srvcache.c
	${CC} -c -S ${COPTS} ../nfs/nfs_srvcache.c
	${C2} nfs_srvcache.s | ${INLINE} | ${AS} -o nfs_srvcache.o
	rm -f nfs_srvcache.s

nfs_subs.o: ../nfs/nfs_subs.c
	${CC} -c -S ${COPTS} ../nfs/nfs_subs.c
	${C2} nfs_subs.s | ${INLINE} | ${AS} -o nfs_subs.o
	rm -f nfs_subs.s

nfs_syscalls.o: ../nfs/nfs_syscalls.c
	${CC} -c -S ${COPTS} ../nfs/nfs_syscalls.c
	${C2} nfs_syscalls.s | ${INLINE} | ${AS} -o nfs_syscalls.o
	rm -f nfs_syscalls.s

nfs_vfsops.o: ../nfs/nfs_vfsops.c
	${CC} -c -S ${COPTS} ../nfs/nfs_vfsops.c
	${C2} nfs_vfsops.s | ${INLINE} | ${AS} -o nfs_vfsops.o
	rm -f nfs_vfsops.s

nfs_vnops.o: ../nfs/nfs_vnops.c
	${CC} -c -S ${COPTS} ../nfs/nfs_vnops.c
	${C2} nfs_vnops.s | ${INLINE} | ${AS} -o nfs_vnops.o
	rm -f nfs_vnops.s

kdb_access.o: ../kdb/kdb_access.c
	${CC} -c -S ${COPTS} ../kdb/kdb_access.c
	${C2} kdb_access.s | ${INLINE} | ${AS} -o kdb_access.o
	rm -f kdb_access.s

kdb_command.o: ../kdb/kdb_command.c
	${CC} -c -S ${COPTS} ../kdb/kdb_command.c
	${C2} kdb_command.s | ${INLINE} | ${AS} -o kdb_command.o
	rm -f kdb_command.s

kdb_ctype.o: ../kdb/kdb_ctype.c
	${CC} -c -S ${COPTS} ../kdb/kdb_ctype.c
	${C2} kdb_ctype.s | ${INLINE} | ${AS} -o kdb_ctype.o
	rm -f kdb_ctype.s

kdb_expr.o: ../kdb/kdb_expr.c
	${CC} -c -S ${COPTS} ../kdb/kdb_expr.c
	${C2} kdb_expr.s | ${INLINE} | ${AS} -o kdb_expr.o
	rm -f kdb_expr.s

kdb_format.o: ../kdb/kdb_format.c
	${CC} -c -S ${COPTS} ../kdb/kdb_format.c
	${C2} kdb_format.s | ${INLINE} | ${AS} -o kdb_format.o
	rm -f kdb_format.s

kdb_input.o: ../kdb/kdb_input.c
	${CC} -c -S ${COPTS} ../kdb/kdb_input.c
	${C2} kdb_input.s | ${INLINE} | ${AS} -o kdb_input.o
	rm -f kdb_input.s

kdb_message.o: ../kdb/kdb_message.c
	${CC} -c -S ${COPTS} ../kdb/kdb_message.c
	${C2} kdb_message.s | ${INLINE} | ${AS} -o kdb_message.o
	rm -f kdb_message.s

kdb_output.o: ../kdb/kdb_output.c
	${CC} -c -S ${COPTS} ../kdb/kdb_output.c
	${C2} kdb_output.s | ${INLINE} | ${AS} -o kdb_output.o
	rm -f kdb_output.s

kdb_pcs.o: ../kdb/kdb_pcs.c
	${CC} -c -S ${COPTS} ../kdb/kdb_pcs.c
	${C2} kdb_pcs.s | ${INLINE} | ${AS} -o kdb_pcs.o
	rm -f kdb_pcs.s

kdb_print.o: ../kdb/kdb_print.c
	${CC} -c -S ${COPTS} ../kdb/kdb_print.c
	${C2} kdb_print.s | ${INLINE} | ${AS} -o kdb_print.o
	rm -f kdb_print.s

kdb_runpcs.o: ../kdb/kdb_runpcs.c
	${CC} -c -S ${COPTS} ../kdb/kdb_runpcs.c
	${C2} kdb_runpcs.s | ${INLINE} | ${AS} -o kdb_runpcs.o
	rm -f kdb_runpcs.s

kdb_sym.o: ../kdb/kdb_sym.c
	${CC} -c -S ${COPTS} ../kdb/kdb_sym.c
	${C2} kdb_sym.s | ${INLINE} | ${AS} -o kdb_sym.o
	rm -f kdb_sym.s

kdb_trap.o: ../kdb/kdb_trap.c
	${CC} -c -S ${COPTS} ../kdb/kdb_trap.c
	${C2} kdb_trap.s | ${INLINE} | ${AS} -o kdb_trap.o
	rm -f kdb_trap.s

af.o: ../net/af.c
	${CC} -c -S ${COPTS} ../net/af.c
	${C2} af.s | ${INLINE} | ${AS} -o af.o
	rm -f af.s

if.o: ../net/if.c
	${CC} -c -S ${COPTS} ../net/if.c
	${C2} if.s | ${INLINE} | ${AS} -o if.o
	rm -f if.s

if_ethersubr.o: ../net/if_ethersubr.c
	${CC} -c -S ${COPTS} ../net/if_ethersubr.c
	${C2} if_ethersubr.s | ${INLINE} | ${AS} -o if_ethersubr.o
	rm -f if_ethersubr.s

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

radix.o: ../net/radix.c
	${CC} -c -S ${COPTS} ../net/radix.c
	${C2} radix.s | ${INLINE} | ${AS} -o radix.o
	rm -f radix.s

route.o: ../net/route.c
	${CC} -c -S ${COPTS} ../net/route.c
	${C2} route.s | ${INLINE} | ${AS} -o route.o
	rm -f route.s

rtsock.o: ../net/rtsock.c
	${CC} -c -S ${COPTS} ../net/rtsock.c
	${C2} rtsock.s | ${INLINE} | ${AS} -o rtsock.o
	rm -f rtsock.s

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

ka650.o: ../vax/ka650.c
	${CC} -c -S ${COPTS} ../vax/ka650.c
	${C2} ka650.s | ${INLINE} | ${AS} -o ka650.o
	rm -f ka650.s

ka630.o: ../vax/ka630.c
	${CC} -c -S ${COPTS} ../vax/ka630.c
	${C2} ka630.s | ${INLINE} | ${AS} -o ka630.o
	rm -f ka630.s

ka730.o: ../vax/ka730.c
	${CC} -c -S ${COPTS} ../vax/ka730.c
	${C2} ka730.s | ${INLINE} | ${AS} -o ka730.o
	rm -f ka730.s

ka750.o: ../vax/ka750.c
	${CC} -c -S ${COPTS} ../vax/ka750.c
	${C2} ka750.s | ${INLINE} | ${AS} -o ka750.o
	rm -f ka750.s

ka780.o: ../vax/ka780.c
	${CC} -c -S ${COPTS} ../vax/ka780.c
	${C2} ka780.s | ${INLINE} | ${AS} -o ka780.o
	rm -f ka780.s

ka820.o: ../vax/ka820.c
	${CC} -c -S ${COPTS} ../vax/ka820.c
	${C2} ka820.s | ${INLINE} | ${AS} -o ka820.o
	rm -f ka820.s

ka860.o: ../vax/ka860.c
	${CC} -c -S ${COPTS} ../vax/ka860.c
	${C2} ka860.s | ${INLINE} | ${AS} -o ka860.o
	rm -f ka860.s

kdb_machdep.o: ../vax/kdb_machdep.c
	${CC} -c -S ${COPTS} ../vax/kdb_machdep.c
	${C2} kdb_machdep.s | ${INLINE} | ${AS} -o kdb_machdep.o
	rm -f kdb_machdep.s

kdb_opset.o: ../vax/kdb_opset.c
	${CC} -c -S ${COPTS} ../vax/kdb_opset.c
	${C2} kdb_opset.s | ${INLINE} | ${AS} -o kdb_opset.o
	rm -f kdb_opset.s

machdep.o: ../vax/machdep.c
	${CC} -c -S ${COPTS} ${PARAM} ../vax/machdep.c
	${C2} machdep.s | ${INLINE} | ${AS} -o machdep.o
	rm -f machdep.s

mem.o: ../vax/mem.c
	${CC} -c -S ${COPTS} ../vax/mem.c
	${C2} mem.s | ${INLINE} | ${AS} -o mem.o
	rm -f mem.s

mscp.o: ../vax/mscp.c
	${CC} -c -S ${COPTS} ../vax/mscp.c
	${C2} mscp.s | ${INLINE} | ${AS} -o mscp.o
	rm -f mscp.s

ns_cksum.o: ../vax/ns_cksum.c
	${CC} -c -S ${COPTS} ../vax/ns_cksum.c
	${C2} ns_cksum.s | ${INLINE} | ${AS} -o ns_cksum.o
	rm -f ns_cksum.s

rx50.o: ../vax/rx50.c
	${CC} -c -S ${COPTS} ../vax/rx50.c
	${C2} -i rx50.s | ${INLINE} | ${AS} -o rx50.o
	rm -f rx50.s

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

bi.o: ../vaxbi/bi.c
	${CC} -c -S ${COPTS} ../vaxbi/bi.c
	${C2} -i bi.s | ${INLINE} | ${AS} -o bi.o
	rm -f bi.s

kdb.o: ../vaxbi/kdb.c
	${CC} -c -S ${COPTS} ../vaxbi/kdb.c
	${C2} -i kdb.s | ${INLINE} | ${AS} -o kdb.o
	rm -f kdb.s

if_de.o: ../vaxif/if_de.c
	${CC} -c -S ${COPTS} ../vaxif/if_de.c
	${C2} -i if_de.s | ${INLINE} | ${AS} -o if_de.o
	rm -f if_de.s

if_ec.o: ../vaxif/if_ec.c
	${CC} -c -S ${COPTS} ../vaxif/if_ec.c
	${C2} -i if_ec.s | ${INLINE} | ${AS} -o if_ec.o
	rm -f if_ec.s

if_ex.o: ../vaxif/if_ex.c
	${CC} -c -S ${COPTS} ../vaxif/if_ex.c
	${C2} -i if_ex.s | ${INLINE} | ${AS} -o if_ex.o
	rm -f if_ex.s

if_il.o: ../vaxif/if_il.c
	${CC} -c -S ${COPTS} ../vaxif/if_il.c
	${C2} -i if_il.s | ${INLINE} | ${AS} -o if_il.o
	rm -f if_il.s

if_qe.o: ../vaxif/if_qe.c
	${CC} -c -S ${COPTS} ../vaxif/if_qe.c
	${C2} -i if_qe.s | ${INLINE} | ${AS} -o if_qe.o
	rm -f if_qe.s

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

dmx.o: ../vaxuba/dmx.c
	${CC} -c -S ${COPTS} ../vaxuba/dmx.c
	${C2} -i dmx.s | ${INLINE} | ${AS} -o dmx.o
	rm -f dmx.s

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

qv.o: ../vaxuba/qv.c
	${CC} -c -S ${COPTS} ../vaxuba/qv.c
	${C2} -i qv.s | ${INLINE} | ${AS} -o qv.o
	rm -f qv.s

qfont.o: ../vaxuba/qfont.c
	${CC} -c -S ${COPTS} ../vaxuba/qfont.c
	${C2} -i qfont.s | ${INLINE} | ${AS} -o qfont.o
	rm -f qfont.s

qd.o: ../vaxuba/qd.c
	${CC} -c -S ${COPTS} ../vaxuba/qd.c
	${C2} -i qd.s | ${INLINE} | ${AS} -o qd.o
	rm -f qd.s


# DO NOT DELETE THIS LINE -- make depend uses it

