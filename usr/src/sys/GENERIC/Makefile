IDENT=-DGENERIC -DTAHOE -DKADB -DFPE -DNS -DINET -DTCP_COMPAT_42 -DQUOTA
PARAM=-DTIMEZONE=480 -DDST=1 -DMAXUSERS=32
#	@(#)Makefile.tahoe	1.14 (Berkeley) 10/22/87
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
# N.B.: NO DEPENDENCIES ON FOLLOWING FLAGS ARE INVISIBLE TO MAKEFILE
#	IF YOU CHANGE THE DEFINITION OF ANY OF THESE RECOMPILE EVERYTHING
#
# -DTRACE	compile in kernel tracing hooks
# -DQUOTA	compile in file system quotas
#
C2=	/lib/c2
LD=	/bin/ld
TOUCH=	touch -f -c
INDIR=	../tahoe/inline
INLINE=	${INDIR}/inline

INCLUDES= -I. -I../h
COPTS=	${INCLUDES} ${IDENT} -DKERNEL
CFLAGS=	-O ${COPTS}

OBJS=kdb_access.o kdb_command.o kdb_ctype.o kdb_expr.o kdb_format.o \
	kdb_input.o kdb_message.o kdb_output.o kdb_pcs.o kdb_print.o \
	kdb_runpcs.o kdb_sym.o kdb_trap.o af.o if.o if_loop.o raw_cb.o \
	raw_usrreq.o route.o if_ether.o in.o in_pcb.o in_proto.o \
	ip_icmp.o ip_input.o ip_output.o raw_ip.o tcp_debug.o \
	tcp_input.o tcp_output.o tcp_subr.o tcp_timer.o tcp_usrreq.o \
	udp_usrreq.o idp_usrreq.o ns.o ns_error.o ns_ip.o ns_input.o \
	ns_output.o ns_pcb.o ns_proto.o spp_debug.o spp_usrreq.o \
	init_main.o init_sysent.o kern_acct.o kern_clock.o \
	kern_descrip.o kern_exec.o kern_exit.o kern_fork.o kern_malloc.o \
	kern_mman.o kern_proc.o kern_prot.o kern_resource.o kern_sig.o \
	kern_subr.o kern_synch.o kern_time.o kern_xxx.o quota_kern.o \
	quota_subr.o quota_sys.o quota_ufs.o subr_log.o subr_prf.o \
	subr_rmap.o subr_xxx.o sys_generic.o sys_inode.o sys_process.o \
	sys_socket.o tty.o tty_conf.o tty_pty.o tty_subr.o tty_tty.o \
	ufs_alloc.o ufs_bio.o ufs_bmap.o ufs_disksubr.o ufs_fio.o \
	ufs_inode.o ufs_mount.o ufs_namei.o ufs_subr.o ufs_syscalls.o \
	ufs_tables.o ufs_xxx.o uipc_domain.o uipc_mbuf.o uipc_proto.o \
	uipc_socket.o uipc_socket2.o uipc_syscalls.o uipc_usrreq.o \
	vm_drum.o vm_mem.o vm_mon.o vm_page.o vm_proc.o vm_pt.o \
	vm_sched.o vm_subr.o vm_sw.o vm_swap.o vm_swp.o vm_text.o \
	autoconf.o conf.o cons.o clock.o kdb_machdep.o kdb_opset.o \
	in_cksum.o machdep.o mem.o ns_cksum.o sys_machdep.o trap.o \
	ufs_machdep.o vm_machdep.o udiv.o urem.o if_ex.o fpe.o Kaddd.o \
	Kaddf.o Katanf.o Kcvtld.o Kcvtlf.o Kdivd.o Kdivf.o Kexpf.o \
	Kfnorm.o Kfp_exp.o Kfp_mvtoacc.o Kfrexpf.o Kldexpf.o Klogf.o \
	Kmodf.o Kmuld.o Kmulf.o Ksfnorm.o Ksfp_exp.o Ksinfcosf.o \
	Ksqrtf.o Ksubd.o Ksubf.o vba.o vd.o vx.o hd.o 

CFILES=../kdb/kdb_access.c ../kdb/kdb_command.c ../kdb/kdb_ctype.c \
	../kdb/kdb_expr.c ../kdb/kdb_format.c ../kdb/kdb_input.c \
	../kdb/kdb_message.c ../kdb/kdb_output.c ../kdb/kdb_pcs.c \
	../kdb/kdb_print.c ../kdb/kdb_runpcs.c ../kdb/kdb_sym.c \
	../kdb/kdb_trap.c ../net/af.c ../net/if.c ../net/if_loop.c \
	../net/raw_cb.c ../net/raw_usrreq.c ../net/route.c \
	../netinet/if_ether.c ../netinet/in.c ../netinet/in_pcb.c \
	../netinet/in_proto.c ../netinet/ip_icmp.c ../netinet/ip_input.c \
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
	../sys/kern_fork.c ../sys/kern_malloc.c ../sys/kern_mman.c \
	../sys/kern_proc.c ../sys/kern_prot.c ../sys/kern_resource.c \
	../sys/kern_sig.c ../sys/kern_subr.c ../sys/kern_synch.c \
	../sys/kern_time.c ../sys/kern_xxx.c ../sys/quota_kern.c \
	../sys/quota_subr.c ../sys/quota_sys.c ../sys/quota_ufs.c \
	../sys/subr_log.c ../sys/subr_prf.c ../sys/subr_rmap.c \
	../sys/subr_xxx.c ../sys/sys_generic.c ../sys/sys_inode.c \
	../sys/sys_process.c ../sys/sys_socket.c ../sys/tty.c \
	../sys/tty_conf.c ../sys/tty_pty.c ../sys/tty_subr.c \
	../sys/tty_tty.c ../sys/ufs_alloc.c ../sys/ufs_bio.c \
	../sys/ufs_bmap.c ../sys/ufs_disksubr.c ../sys/ufs_fio.c \
	../sys/ufs_inode.c ../sys/ufs_mount.c ../sys/ufs_namei.c \
	../sys/ufs_subr.c ../sys/ufs_syscalls.c ../sys/ufs_tables.c \
	../sys/ufs_xxx.c ../sys/uipc_domain.c ../sys/uipc_mbuf.c \
	../sys/uipc_proto.c ../sys/uipc_socket.c ../sys/uipc_socket2.c \
	../sys/uipc_syscalls.c ../sys/uipc_usrreq.c ../sys/vm_drum.c \
	../sys/vm_mem.c ../sys/vm_mon.c ../sys/vm_page.c \
	../sys/vm_proc.c ../sys/vm_pt.c ../sys/vm_sched.c \
	../sys/vm_subr.c ../sys/vm_sw.c ../sys/vm_swap.c ../sys/vm_swp.c \
	../sys/vm_text.c ../tahoe/autoconf.c ../tahoe/conf.c \
	../tahoe/cons.c ../tahoe/clock.c ../tahoe/kdb_machdep.c \
	../tahoe/kdb_opset.c ../tahoe/in_cksum.c ../tahoe/machdep.c \
	../tahoe/mem.c ../tahoe/ns_cksum.c ../tahoe/sys_machdep.c \
	../tahoe/trap.c ../tahoe/ufs_machdep.c ../tahoe/vm_machdep.c \
	../tahoeif/if_ex.c ../tahoemath/fpe.c ../tahoevba/vba.c \
	../tahoevba/vd.c ../tahoevba/vx.c ../tahoevba/hd.c \
	../tahoe/swapgeneric.c 

vmunix: Makefile ../machine/symbols.sort ${INLINE} locore.o ${OBJS} param.o ioconf.o swapgeneric.o
	@echo loading vmunix
	@rm -f vmunix
	@sh ../conf/newvers.sh
	@${CC} $(CFLAGS) -c vers.c
	@${LD} -n -o vmunix -e start -x -T C0000800 locore.o ${OBJS} vers.o ioconf.o param.o swapgeneric.o
	@echo rearranging symbols
	@-symorder ../tahoe/symbols.sort vmunix
	@size vmunix
	@chmod 755 vmunix

swapgeneric.o: ../tahoe/swapgeneric.c
	${CC} -c -S ${COPTS} ../tahoe/swapgeneric.c
	${C2} swapgeneric.s | ${INLINE} | ${AS} -o swapgeneric.o
	rm -f swapgeneric.s

all: vmunix

clean:
	rm -f eddep *vmunix tags *.o locore.i [a-uw-z]*.s \
		errs linterrs makelinks

lint: /tmp param.c
	@lint -hbxn -I. -DGENERIC ${COPTS} ${PARAM} \
	  ../tahoe/Locore.c ${CFILES} ioconf.c param.c | \
	    grep -v 'struct/union .* never defined' | \
	    grep -v 'possible pointer alignment problem'

../tahoe/symbols.sort: ../tahoe/symbols.raw
	grep -v '^#' ../tahoe/symbols.raw \
	    | sed 's/^	//' | sort -u > ../tahoe/symbols.sort

${INLINE}: ${INDIR}/inline.h ${INDIR}/langpats.c
${INLINE}: ${INDIR}/libcpats.c ${INDIR}/machdep.c
${INLINE}: ${INDIR}/machpats.c ${INDIR}/main.c
	cd ${INDIR}; make

locore.o: assym.s ../tahoe/scb.s vbvec.s ../tahoe/locore.s \
    vbglue.s ../tahoe/mtpr.h ../tahoe/trap.h ../tahoe/psl.h \
    ../tahoe/pte.h ../tahoe/cp.h ../tahoe/mem.h
	cat assym.s ../tahoe/scb.s vbvec.s ../tahoe/locore.s \
	    vbglue.s > locore.c
	${CC} -E -I. -DLOCORE ${COPTS} locore.c > locore.i
	@echo 'as -o locore.o locore.i'
	@${AS} -o locore.o locore.i
	@rm locore.i

# the following is necessary because autoconf.o depends on #if GENERIC
autoconf.o: Makefile

# depend on network configuration
af.o uipc_proto.o locore.o: Makefile

# depend on maxusers
assym.s machdep.o: Makefile

# depends on KDB (cons.o also depends on GENERIC)
trap.o cons.o: Makefile

assym.s: ../h/param.h ../tahoe/pte.h ../h/buf.h ../h/vmparam.h \
    ../h/vmmeter.h ../h/dir.h ../h/cmap.h ../h/map.h ../tahoevba/vbaparam.h \
    ../h/proc.h ../h/msgbuf.h ../tahoe/scb.h ../machine/vmparam.h
	${CC} ${INCLUDES} -DKERNEL ${IDENT} ${PARAM} ../tahoe/genassym.c
	./a.out >assym.s; rm -f a.out

depend:	assym.s
	mkdep ${COPTS} ${CFILES}

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
	@echo "see ../sys/Makefile for tags"

print:
	@pr -f Makefile ../sys/TODO linterrs
	@/usr/ucb/ctags -x ${CFILES} | pr -f -h XREF
	@(size vmunix *.o) | pr -f -h sizes
	@ls -ls | pr -f
	@cd ../h; ls -ls | pr -f ; pr -f *.m *.h
	@echo 'once upon a time ...'
	@cd ../tahoe; pr -f Locore.c scb.s locore.s
	@cd ${INDIR}; make print
	@pr -f ${CFILES}

ioconf.o: ioconf.c ../h/param.h ../tahoe/pte.h ../h/buf.h ../h/map.h \
    ../h/vm.h ../tahoevba/vbavar.h

param.c: ../conf/param.c
	-rm -f param.c
	cp ../conf/param.c .

param.o: param.c Makefile
	${CC} -c ${CFLAGS} ${PARAM} param.c

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

kern_malloc.o: ../sys/kern_malloc.c
	${CC} -c -S ${COPTS} ../sys/kern_malloc.c
	${C2} kern_malloc.s | ${INLINE} | ${AS} -o kern_malloc.o
	rm -f kern_malloc.s

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

ufs_disksubr.o: ../sys/ufs_disksubr.c
	${CC} -c -S ${COPTS} ../sys/ufs_disksubr.c
	${C2} ufs_disksubr.s | ${INLINE} | ${AS} -o ufs_disksubr.o
	rm -f ufs_disksubr.s

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

autoconf.o: ../tahoe/autoconf.c
	${CC} -c -S ${COPTS} ../tahoe/autoconf.c
	${C2} -i autoconf.s | ${INLINE} | ${AS} -o autoconf.o
	rm -f autoconf.s

conf.o: ../tahoe/conf.c
	${CC} -c -S ${COPTS} ../tahoe/conf.c
	${C2} conf.s | ${INLINE} | ${AS} -o conf.o
	rm -f conf.s

cons.o: ../tahoe/cons.c
	${CC} -c -S ${COPTS} ../tahoe/cons.c
	${C2} cons.s | ${INLINE} | ${AS} -o cons.o
	rm -f cons.s

clock.o: ../tahoe/clock.c
	${CC} -c -S ${COPTS} ../tahoe/clock.c
	${C2} clock.s | ${INLINE} | ${AS} -o clock.o
	rm -f clock.s

kdb_machdep.o: ../tahoe/kdb_machdep.c
	${CC} -c -S ${COPTS} ../tahoe/kdb_machdep.c
	${C2} kdb_machdep.s | ${INLINE} | ${AS} -o kdb_machdep.o
	rm -f kdb_machdep.s

kdb_opset.o: ../tahoe/kdb_opset.c
	${CC} -c -S ${COPTS} ../tahoe/kdb_opset.c
	${C2} kdb_opset.s | ${INLINE} | ${AS} -o kdb_opset.o
	rm -f kdb_opset.s

in_cksum.o: ../tahoe/in_cksum.c
	${CC} -c -S ${COPTS} ../tahoe/in_cksum.c
	${C2} in_cksum.s | ${INLINE} | ${AS} -o in_cksum.o
	rm -f in_cksum.s

machdep.o: ../tahoe/machdep.c
	${CC} -c -S ${COPTS} ${PARAM} ../tahoe/machdep.c
	${C2} machdep.s | ${INLINE} | ${AS} -o machdep.o
	rm -f machdep.s

mem.o: ../tahoe/mem.c
	${CC} -c -S ${COPTS} ../tahoe/mem.c
	${C2} mem.s | ${INLINE} | ${AS} -o mem.o
	rm -f mem.s

ns_cksum.o: ../tahoe/ns_cksum.c
	${CC} -c -S ${COPTS} ../tahoe/ns_cksum.c
	${C2} ns_cksum.s | ${INLINE} | ${AS} -o ns_cksum.o
	rm -f ns_cksum.s

sys_machdep.o: ../tahoe/sys_machdep.c
	${CC} -c -S ${COPTS} ../tahoe/sys_machdep.c
	${C2} sys_machdep.s | ${INLINE} | ${AS} -o sys_machdep.o
	rm -f sys_machdep.s

trap.o: ../tahoe/trap.c
	${CC} -c -S ${COPTS} ../tahoe/trap.c
	${C2} trap.s | ${INLINE} | ${AS} -o trap.o
	rm -f trap.s

ufs_machdep.o: ../tahoe/ufs_machdep.c
	${CC} -c -S ${COPTS} ../tahoe/ufs_machdep.c
	${C2} ufs_machdep.s | ${INLINE} | ${AS} -o ufs_machdep.o
	rm -f ufs_machdep.s

vm_machdep.o: ../tahoe/vm_machdep.c
	${CC} -c -S ${COPTS} ../tahoe/vm_machdep.c
	${C2} vm_machdep.s | ${INLINE} | ${AS} -o vm_machdep.o
	rm -f vm_machdep.s

udiv.o: ../tahoe/udiv.s
	-ln -s ../tahoe/udiv.s udiv.c
	${CC} -E ${COPTS} udiv.c | ${AS} -o udiv.o
	rm -f udiv.c

urem.o: ../tahoe/urem.s
	-ln -s ../tahoe/urem.s urem.c
	${CC} -E ${COPTS} urem.c | ${AS} -o urem.o
	rm -f urem.c

if_ex.o: ../tahoeif/if_ex.c
	${CC} -c -S ${COPTS} ../tahoeif/if_ex.c
	${C2} -i if_ex.s | ${INLINE} | ${AS} -o if_ex.o
	rm -f if_ex.s

fpe.o: ../tahoemath/fpe.c
	${CC} -c -S ${COPTS} ../tahoemath/fpe.c
	${C2} fpe.s | ${INLINE} | ${AS} -o fpe.o
	rm -f fpe.s

Kaddd.o: ../tahoemath/Kaddd.s
	-ln -s ../tahoemath/Kaddd.s Kaddd.c
	${CC} -E ${COPTS} Kaddd.c | ${AS} -o Kaddd.o
	rm -f Kaddd.c

Kaddf.o: ../tahoemath/Kaddf.s
	-ln -s ../tahoemath/Kaddf.s Kaddf.c
	${CC} -E ${COPTS} Kaddf.c | ${AS} -o Kaddf.o
	rm -f Kaddf.c

Katanf.o: ../tahoemath/Katanf.s
	-ln -s ../tahoemath/Katanf.s Katanf.c
	${CC} -E ${COPTS} Katanf.c | ${AS} -o Katanf.o
	rm -f Katanf.c

Kcvtld.o: ../tahoemath/Kcvtld.s
	-ln -s ../tahoemath/Kcvtld.s Kcvtld.c
	${CC} -E ${COPTS} Kcvtld.c | ${AS} -o Kcvtld.o
	rm -f Kcvtld.c

Kcvtlf.o: ../tahoemath/Kcvtlf.s
	-ln -s ../tahoemath/Kcvtlf.s Kcvtlf.c
	${CC} -E ${COPTS} Kcvtlf.c | ${AS} -o Kcvtlf.o
	rm -f Kcvtlf.c

Kdivd.o: ../tahoemath/Kdivd.s
	-ln -s ../tahoemath/Kdivd.s Kdivd.c
	${CC} -E ${COPTS} Kdivd.c | ${AS} -o Kdivd.o
	rm -f Kdivd.c

Kdivf.o: ../tahoemath/Kdivf.s
	-ln -s ../tahoemath/Kdivf.s Kdivf.c
	${CC} -E ${COPTS} Kdivf.c | ${AS} -o Kdivf.o
	rm -f Kdivf.c

Kexpf.o: ../tahoemath/Kexpf.s
	-ln -s ../tahoemath/Kexpf.s Kexpf.c
	${CC} -E ${COPTS} Kexpf.c | ${AS} -o Kexpf.o
	rm -f Kexpf.c

Kfnorm.o: ../tahoemath/Kfnorm.s
	-ln -s ../tahoemath/Kfnorm.s Kfnorm.c
	${CC} -E ${COPTS} Kfnorm.c | ${AS} -o Kfnorm.o
	rm -f Kfnorm.c

Kfp_exp.o: ../tahoemath/Kfp_exp.s
	-ln -s ../tahoemath/Kfp_exp.s Kfp_exp.c
	${CC} -E ${COPTS} Kfp_exp.c | ${AS} -o Kfp_exp.o
	rm -f Kfp_exp.c

Kfp_mvtoacc.o: ../tahoemath/Kfp_mvtoacc.s
	-ln -s ../tahoemath/Kfp_mvtoacc.s Kfp_mvtoacc.c
	${CC} -E ${COPTS} Kfp_mvtoacc.c | ${AS} -o Kfp_mvtoacc.o
	rm -f Kfp_mvtoacc.c

Kfrexpf.o: ../tahoemath/Kfrexpf.s
	-ln -s ../tahoemath/Kfrexpf.s Kfrexpf.c
	${CC} -E ${COPTS} Kfrexpf.c | ${AS} -o Kfrexpf.o
	rm -f Kfrexpf.c

Kldexpf.o: ../tahoemath/Kldexpf.s
	-ln -s ../tahoemath/Kldexpf.s Kldexpf.c
	${CC} -E ${COPTS} Kldexpf.c | ${AS} -o Kldexpf.o
	rm -f Kldexpf.c

Klogf.o: ../tahoemath/Klogf.s
	-ln -s ../tahoemath/Klogf.s Klogf.c
	${CC} -E ${COPTS} Klogf.c | ${AS} -o Klogf.o
	rm -f Klogf.c

Kmodf.o: ../tahoemath/Kmodf.s
	-ln -s ../tahoemath/Kmodf.s Kmodf.c
	${CC} -E ${COPTS} Kmodf.c | ${AS} -o Kmodf.o
	rm -f Kmodf.c

Kmuld.o: ../tahoemath/Kmuld.s
	-ln -s ../tahoemath/Kmuld.s Kmuld.c
	${CC} -E ${COPTS} Kmuld.c | ${AS} -o Kmuld.o
	rm -f Kmuld.c

Kmulf.o: ../tahoemath/Kmulf.s
	-ln -s ../tahoemath/Kmulf.s Kmulf.c
	${CC} -E ${COPTS} Kmulf.c | ${AS} -o Kmulf.o
	rm -f Kmulf.c

Ksfnorm.o: ../tahoemath/Ksfnorm.s
	-ln -s ../tahoemath/Ksfnorm.s Ksfnorm.c
	${CC} -E ${COPTS} Ksfnorm.c | ${AS} -o Ksfnorm.o
	rm -f Ksfnorm.c

Ksfp_exp.o: ../tahoemath/Ksfp_exp.s
	-ln -s ../tahoemath/Ksfp_exp.s Ksfp_exp.c
	${CC} -E ${COPTS} Ksfp_exp.c | ${AS} -o Ksfp_exp.o
	rm -f Ksfp_exp.c

Ksinfcosf.o: ../tahoemath/Ksinfcosf.s
	-ln -s ../tahoemath/Ksinfcosf.s Ksinfcosf.c
	${CC} -E ${COPTS} Ksinfcosf.c | ${AS} -o Ksinfcosf.o
	rm -f Ksinfcosf.c

Ksqrtf.o: ../tahoemath/Ksqrtf.s
	-ln -s ../tahoemath/Ksqrtf.s Ksqrtf.c
	${CC} -E ${COPTS} Ksqrtf.c | ${AS} -o Ksqrtf.o
	rm -f Ksqrtf.c

Ksubd.o: ../tahoemath/Ksubd.s
	-ln -s ../tahoemath/Ksubd.s Ksubd.c
	${CC} -E ${COPTS} Ksubd.c | ${AS} -o Ksubd.o
	rm -f Ksubd.c

Ksubf.o: ../tahoemath/Ksubf.s
	-ln -s ../tahoemath/Ksubf.s Ksubf.c
	${CC} -E ${COPTS} Ksubf.c | ${AS} -o Ksubf.o
	rm -f Ksubf.c

vba.o: ../tahoevba/vba.c
	${CC} -c -S ${COPTS} ../tahoevba/vba.c
	${C2} -i vba.s | ${INLINE} | ${AS} -o vba.o
	rm -f vba.s

vd.o: ../tahoevba/vd.c
	${CC} -c -S ${COPTS} ../tahoevba/vd.c
	${C2} -i vd.s | ${INLINE} | ${AS} -o vd.o
	rm -f vd.s

vx.o: ../tahoevba/vx.c
	${CC} -c -S ${COPTS} ../tahoevba/vx.c
	${C2} -i vx.s | ${INLINE} | ${AS} -o vx.o
	rm -f vx.s

hd.o: ../tahoevba/hd.c
	${CC} -c -S ${COPTS} ../tahoevba/hd.c
	${C2} -i hd.s | ${INLINE} | ${AS} -o hd.o
	rm -f hd.s


# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

kdb_access.o: ../kdb/kdb_access.c ../kdb/../kdb/defs.h ../h/param.h
kdb_access.o: ../h/types.h ../h/signal.h ../h/../machine/trap.h
kdb_access.o: ../h/../machine/machparam.h machine/endian.h ../h/dir.h
kdb_access.o: ../kdb/../kdb/../machine/psl.h ../kdb/../kdb/../machine/pte.h
kdb_access.o: ../h/user.h ../h/../machine/pcb.h ../h/dmap.h ../h/time.h
kdb_access.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h
kdb_access.o: /usr/include/a.out.h sys/exec.h /usr/include/ctype.h
kdb_access.o: ../kdb/../kdb/redef.h ../kdb/../kdb/../machine/kdbparam.h
kdb_command.o: ../kdb/kdb_command.c ../kdb/../kdb/defs.h ../h/param.h
kdb_command.o: ../h/types.h ../h/signal.h ../h/../machine/trap.h
kdb_command.o: ../h/../machine/machparam.h machine/endian.h ../h/dir.h
kdb_command.o: ../kdb/../kdb/../machine/psl.h ../kdb/../kdb/../machine/pte.h
kdb_command.o: ../h/user.h ../h/../machine/pcb.h ../h/dmap.h ../h/time.h
kdb_command.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h
kdb_command.o: /usr/include/a.out.h sys/exec.h /usr/include/ctype.h
kdb_command.o: ../kdb/../kdb/redef.h ../kdb/../kdb/../machine/kdbparam.h
kdb_ctype.o: ../kdb/kdb_ctype.c /usr/include/ctype.h
kdb_expr.o: ../kdb/kdb_expr.c ../kdb/../kdb/defs.h ../h/param.h ../h/types.h
kdb_expr.o: ../h/signal.h ../h/../machine/trap.h ../h/../machine/machparam.h
kdb_expr.o: machine/endian.h ../h/dir.h ../kdb/../kdb/../machine/psl.h
kdb_expr.o: ../kdb/../kdb/../machine/pte.h ../h/user.h ../h/../machine/pcb.h
kdb_expr.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
kdb_expr.o: ../h/errno.h ../h/proc.h /usr/include/a.out.h sys/exec.h
kdb_expr.o: /usr/include/ctype.h ../kdb/../kdb/redef.h
kdb_expr.o: ../kdb/../kdb/../machine/kdbparam.h
kdb_format.o: ../kdb/kdb_format.c ../kdb/../kdb/defs.h ../h/param.h
kdb_format.o: ../h/types.h ../h/signal.h ../h/../machine/trap.h
kdb_format.o: ../h/../machine/machparam.h machine/endian.h ../h/dir.h
kdb_format.o: ../kdb/../kdb/../machine/psl.h ../kdb/../kdb/../machine/pte.h
kdb_format.o: ../h/user.h ../h/../machine/pcb.h ../h/dmap.h ../h/time.h
kdb_format.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h
kdb_format.o: /usr/include/a.out.h sys/exec.h /usr/include/ctype.h
kdb_format.o: ../kdb/../kdb/redef.h ../kdb/../kdb/../machine/kdbparam.h
kdb_input.o: ../kdb/kdb_input.c ../kdb/../kdb/defs.h ../h/param.h ../h/types.h
kdb_input.o: ../h/signal.h ../h/../machine/trap.h ../h/../machine/machparam.h
kdb_input.o: machine/endian.h ../h/dir.h ../kdb/../kdb/../machine/psl.h
kdb_input.o: ../kdb/../kdb/../machine/pte.h ../h/user.h ../h/../machine/pcb.h
kdb_input.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
kdb_input.o: ../h/errno.h ../h/proc.h /usr/include/a.out.h sys/exec.h
kdb_input.o: /usr/include/ctype.h ../kdb/../kdb/redef.h
kdb_input.o: ../kdb/../kdb/../machine/kdbparam.h
kdb_message.o: ../kdb/kdb_message.c ../kdb/../kdb/redef.h
kdb_output.o: ../kdb/kdb_output.c ../kdb/../kdb/defs.h ../h/param.h
kdb_output.o: ../h/types.h ../h/signal.h ../h/../machine/trap.h
kdb_output.o: ../h/../machine/machparam.h machine/endian.h ../h/dir.h
kdb_output.o: ../kdb/../kdb/../machine/psl.h ../kdb/../kdb/../machine/pte.h
kdb_output.o: ../h/user.h ../h/../machine/pcb.h ../h/dmap.h ../h/time.h
kdb_output.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h
kdb_output.o: /usr/include/a.out.h sys/exec.h /usr/include/ctype.h
kdb_output.o: ../kdb/../kdb/redef.h ../kdb/../kdb/../machine/kdbparam.h
kdb_pcs.o: ../kdb/kdb_pcs.c ../kdb/../kdb/defs.h ../h/param.h ../h/types.h
kdb_pcs.o: ../h/signal.h ../h/../machine/trap.h ../h/../machine/machparam.h
kdb_pcs.o: machine/endian.h ../h/dir.h ../kdb/../kdb/../machine/psl.h
kdb_pcs.o: ../kdb/../kdb/../machine/pte.h ../h/user.h ../h/../machine/pcb.h
kdb_pcs.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
kdb_pcs.o: ../h/errno.h ../h/proc.h /usr/include/a.out.h sys/exec.h
kdb_pcs.o: /usr/include/ctype.h ../kdb/../kdb/redef.h
kdb_pcs.o: ../kdb/../kdb/../machine/kdbparam.h
kdb_print.o: ../kdb/kdb_print.c ../kdb/../kdb/defs.h ../h/param.h ../h/types.h
kdb_print.o: ../h/signal.h ../h/../machine/trap.h ../h/../machine/machparam.h
kdb_print.o: machine/endian.h ../h/dir.h ../kdb/../kdb/../machine/psl.h
kdb_print.o: ../kdb/../kdb/../machine/pte.h ../h/user.h ../h/../machine/pcb.h
kdb_print.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
kdb_print.o: ../h/errno.h ../h/proc.h /usr/include/a.out.h sys/exec.h
kdb_print.o: /usr/include/ctype.h ../kdb/../kdb/redef.h
kdb_print.o: ../kdb/../kdb/../machine/kdbparam.h
kdb_runpcs.o: ../kdb/kdb_runpcs.c ../kdb/../kdb/defs.h ../h/param.h
kdb_runpcs.o: ../h/types.h ../h/signal.h ../h/../machine/trap.h
kdb_runpcs.o: ../h/../machine/machparam.h machine/endian.h ../h/dir.h
kdb_runpcs.o: ../kdb/../kdb/../machine/psl.h ../kdb/../kdb/../machine/pte.h
kdb_runpcs.o: ../h/user.h ../h/../machine/pcb.h ../h/dmap.h ../h/time.h
kdb_runpcs.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h
kdb_runpcs.o: /usr/include/a.out.h sys/exec.h /usr/include/ctype.h
kdb_runpcs.o: ../kdb/../kdb/redef.h ../kdb/../kdb/../machine/kdbparam.h
kdb_sym.o: ../kdb/kdb_sym.c ../kdb/../kdb/defs.h ../h/param.h ../h/types.h
kdb_sym.o: ../h/signal.h ../h/../machine/trap.h ../h/../machine/machparam.h
kdb_sym.o: machine/endian.h ../h/dir.h ../kdb/../kdb/../machine/psl.h
kdb_sym.o: ../kdb/../kdb/../machine/pte.h ../h/user.h ../h/../machine/pcb.h
kdb_sym.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
kdb_sym.o: ../h/errno.h ../h/proc.h /usr/include/a.out.h sys/exec.h
kdb_sym.o: /usr/include/ctype.h ../kdb/../kdb/redef.h
kdb_sym.o: ../kdb/../kdb/../machine/kdbparam.h /usr/include/stab.h
kdb_trap.o: ../kdb/kdb_trap.c ../kdb/../kdb/defs.h ../h/param.h ../h/types.h
kdb_trap.o: ../h/signal.h ../h/../machine/trap.h ../h/../machine/machparam.h
kdb_trap.o: machine/endian.h ../h/dir.h ../kdb/../kdb/../machine/psl.h
kdb_trap.o: ../kdb/../kdb/../machine/pte.h ../h/user.h ../h/../machine/pcb.h
kdb_trap.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
kdb_trap.o: ../h/errno.h ../h/proc.h /usr/include/a.out.h sys/exec.h
kdb_trap.o: /usr/include/ctype.h ../kdb/../kdb/redef.h
kdb_trap.o: ../kdb/../kdb/../machine/kdbparam.h
af.o: ../net/af.c ../h/param.h ../h/types.h ../h/signal.h
af.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
af.o: ../h/mbuf.h ../h/protosw.h ../h/socket.h ../h/socketvar.h ../net/af.h
if.o: ../net/if.c ../h/param.h ../h/types.h ../h/signal.h
if.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
if.o: ../h/mbuf.h ../h/systm.h ../h/socket.h ../h/socketvar.h ../h/protosw.h
if.o: ../h/dir.h ../h/user.h ../h/../machine/pcb.h ../h/dmap.h ../h/time.h
if.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/kernel.h
if.o: ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/errno.h ../net/if.h
if.o: ../net/../net/if_arp.h ../net/af.h ether.h
if_loop.o: ../net/if_loop.c ../h/param.h ../h/types.h ../h/signal.h
if_loop.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
if_loop.o: ../h/systm.h ../h/mbuf.h ../h/socket.h ../h/errno.h ../h/ioctl.h
if_loop.o: ../h/ttychars.h ../h/ttydev.h ../net/../net/if.h
if_loop.o: ../net/../net/../net/if_arp.h ../net/../net/netisr.h
if_loop.o: ../net/../net/route.h ../net/../machine/mtpr.h
if_loop.o: ../net/../netinet/in.h ../net/../netinet/in_systm.h
if_loop.o: ../net/../netinet/in_var.h ../net/../netinet/ip.h
if_loop.o: ../net/../netns/ns.h ../net/../netns/ns_if.h
raw_cb.o: ../net/raw_cb.c ../h/param.h ../h/types.h ../h/signal.h
raw_cb.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
raw_cb.o: ../h/systm.h ../h/mbuf.h ../h/socket.h ../h/socketvar.h ../h/domain.h
raw_cb.o: ../h/protosw.h ../h/errno.h ../net/if.h ../net/../net/if_arp.h
raw_cb.o: ../net/route.h ../net/raw_cb.h ../net/../netinet/in.h
raw_cb.o: ../net/../machine/mtpr.h
raw_usrreq.o: ../net/raw_usrreq.c ../h/param.h ../h/types.h ../h/signal.h
raw_usrreq.o: ../h/../machine/trap.h ../h/../machine/machparam.h
raw_usrreq.o: machine/endian.h ../h/mbuf.h ../h/domain.h ../h/protosw.h
raw_usrreq.o: ../h/socket.h ../h/socketvar.h ../h/errno.h ../net/if.h
raw_usrreq.o: ../net/../net/if_arp.h ../net/route.h ../net/netisr.h
raw_usrreq.o: ../net/raw_cb.h ../net/../machine/mtpr.h
route.o: ../net/route.c ../h/param.h ../h/types.h ../h/signal.h
route.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
route.o: ../h/systm.h ../h/mbuf.h ../h/protosw.h ../h/socket.h ../h/dir.h
route.o: ../h/user.h ../h/../machine/pcb.h ../h/dmap.h ../h/time.h
route.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/ioctl.h
route.o: ../h/ttychars.h ../h/ttydev.h ../h/errno.h ../net/if.h
route.o: ../net/../net/if_arp.h ../net/af.h ../net/route.h
if_ether.o: ../netinet/if_ether.c ../h/param.h ../h/types.h ../h/signal.h
if_ether.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
if_ether.o: ../h/systm.h ../h/mbuf.h ../h/socket.h ../h/time.h ../h/kernel.h
if_ether.o: ../h/errno.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h
if_ether.o: ../h/syslog.h ../netinet/../net/if.h
if_ether.o: ../netinet/../net/../net/if_arp.h ../netinet/in.h
if_ether.o: ../netinet/in_systm.h ../netinet/ip.h ../netinet/if_ether.h
in.o: ../netinet/in.c ../h/param.h ../h/types.h ../h/signal.h
in.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
in.o: ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/mbuf.h ../h/protosw.h
in.o: ../h/socket.h ../h/socketvar.h ../h/uio.h ../h/dir.h ../h/user.h
in.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
in.o: ../h/namei.h ../h/uio.h ../h/errno.h ../netinet/in_systm.h
in.o: ../netinet/../net/if.h ../netinet/../net/../net/if_arp.h
in.o: ../netinet/../net/route.h ../netinet/../net/af.h ../netinet/in.h
in.o: ../netinet/in_var.h
in_pcb.o: ../netinet/in_pcb.c ../h/param.h ../h/types.h ../h/signal.h
in_pcb.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
in_pcb.o: ../h/systm.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h ../h/dmap.h
in_pcb.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
in_pcb.o: ../h/mbuf.h ../h/socket.h ../h/socketvar.h ../h/ioctl.h
in_pcb.o: ../h/ttychars.h ../h/ttydev.h ../netinet/in.h ../netinet/in_systm.h
in_pcb.o: ../netinet/../net/if.h ../netinet/../net/../net/if_arp.h
in_pcb.o: ../netinet/../net/route.h ../netinet/in_pcb.h ../netinet/in_var.h
in_pcb.o: ../h/protosw.h
in_proto.o: ../netinet/in_proto.c ../h/param.h ../h/types.h ../h/signal.h
in_proto.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
in_proto.o: ../h/socket.h ../h/protosw.h ../h/domain.h ../h/mbuf.h
in_proto.o: ../netinet/in.h ../netinet/in_systm.h imp.h hy.h
ip_icmp.o: ../netinet/ip_icmp.c ../h/param.h ../h/types.h ../h/signal.h
ip_icmp.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
ip_icmp.o: ../h/systm.h ../h/mbuf.h ../h/protosw.h ../h/socket.h ../h/time.h
ip_icmp.o: ../h/kernel.h ../netinet/../net/route.h ../netinet/../net/if.h
ip_icmp.o: ../netinet/../net/../net/if_arp.h ../netinet/in.h
ip_icmp.o: ../netinet/in_systm.h ../netinet/in_var.h ../netinet/ip.h
ip_icmp.o: ../netinet/ip_icmp.h ../netinet/icmp_var.h
ip_input.o: ../netinet/ip_input.c ../h/param.h ../h/types.h ../h/signal.h
ip_input.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
ip_input.o: ../h/systm.h ../h/mbuf.h ../h/domain.h ../h/protosw.h ../h/socket.h
ip_input.o: ../h/errno.h ../h/time.h ../h/kernel.h ../netinet/../net/if.h
ip_input.o: ../netinet/../net/../net/if_arp.h ../netinet/../net/route.h
ip_input.o: ../netinet/in.h ../netinet/in_pcb.h ../netinet/in_systm.h
ip_input.o: ../netinet/in_var.h ../netinet/ip.h ../netinet/ip_var.h
ip_input.o: ../netinet/ip_icmp.h ../netinet/tcp.h
ip_output.o: ../netinet/ip_output.c ../h/param.h ../h/types.h ../h/signal.h
ip_output.o: ../h/../machine/trap.h ../h/../machine/machparam.h
ip_output.o: machine/endian.h ../h/mbuf.h ../h/errno.h ../h/protosw.h
ip_output.o: ../h/socket.h ../h/socketvar.h ../netinet/../net/if.h
ip_output.o: ../netinet/../net/../net/if_arp.h ../netinet/../net/route.h
ip_output.o: ../netinet/in.h ../netinet/in_pcb.h ../netinet/in_systm.h
ip_output.o: ../netinet/in_var.h ../netinet/ip.h ../netinet/ip_var.h
raw_ip.o: ../netinet/raw_ip.c ../h/param.h ../h/types.h ../h/signal.h
raw_ip.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
raw_ip.o: ../h/mbuf.h ../h/socket.h ../h/protosw.h ../h/socketvar.h
raw_ip.o: ../h/errno.h ../netinet/../net/if.h ../netinet/../net/../net/if_arp.h
raw_ip.o: ../netinet/../net/route.h ../netinet/../net/raw_cb.h ../netinet/in.h
raw_ip.o: ../netinet/in_systm.h ../netinet/ip.h ../netinet/ip_var.h
tcp_debug.o: ../netinet/tcp_debug.c ../h/param.h ../h/types.h ../h/signal.h
tcp_debug.o: ../h/../machine/trap.h ../h/../machine/machparam.h
tcp_debug.o: machine/endian.h ../h/systm.h ../h/mbuf.h ../h/socket.h
tcp_debug.o: ../h/socketvar.h ../h/protosw.h ../h/errno.h
tcp_debug.o: ../netinet/../net/route.h ../netinet/../net/if.h
tcp_debug.o: ../netinet/../net/../net/if_arp.h ../netinet/in.h
tcp_debug.o: ../netinet/in_pcb.h ../netinet/in_systm.h ../netinet/ip.h
tcp_debug.o: ../netinet/ip_var.h ../netinet/tcp.h ../netinet/tcp_fsm.h
tcp_debug.o: ../netinet/tcp_seq.h ../netinet/tcp_timer.h ../netinet/tcp_var.h
tcp_debug.o: ../netinet/tcpip.h ../netinet/tcp_debug.h
tcp_input.o: ../netinet/tcp_input.c ../h/param.h ../h/types.h ../h/signal.h
tcp_input.o: ../h/../machine/trap.h ../h/../machine/machparam.h
tcp_input.o: machine/endian.h ../h/systm.h ../h/mbuf.h ../h/protosw.h
tcp_input.o: ../h/socket.h ../h/socketvar.h ../h/errno.h ../netinet/../net/if.h
tcp_input.o: ../netinet/../net/../net/if_arp.h ../netinet/../net/route.h
tcp_input.o: ../netinet/in.h ../netinet/in_pcb.h ../netinet/in_systm.h
tcp_input.o: ../netinet/ip.h ../netinet/ip_var.h ../netinet/tcp.h
tcp_input.o: ../netinet/tcp_fsm.h ../netinet/tcp_seq.h ../netinet/tcp_timer.h
tcp_input.o: ../netinet/tcp_var.h ../netinet/tcpip.h ../netinet/tcp_debug.h
tcp_output.o: ../netinet/tcp_output.c ../h/param.h ../h/types.h ../h/signal.h
tcp_output.o: ../h/../machine/trap.h ../h/../machine/machparam.h
tcp_output.o: machine/endian.h ../h/systm.h ../h/mbuf.h ../h/protosw.h
tcp_output.o: ../h/socket.h ../h/socketvar.h ../h/errno.h
tcp_output.o: ../netinet/../net/route.h ../netinet/in.h ../netinet/in_pcb.h
tcp_output.o: ../netinet/in_systm.h ../netinet/ip.h ../netinet/ip_var.h
tcp_output.o: ../netinet/tcp.h ../netinet/tcp_fsm.h ../netinet/tcp_seq.h
tcp_output.o: ../netinet/tcp_timer.h ../netinet/tcp_var.h ../netinet/tcpip.h
tcp_output.o: ../netinet/tcp_debug.h
tcp_subr.o: ../netinet/tcp_subr.c ../h/param.h ../h/types.h ../h/signal.h
tcp_subr.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
tcp_subr.o: ../h/systm.h ../h/mbuf.h ../h/socket.h ../h/socketvar.h
tcp_subr.o: ../h/protosw.h ../h/errno.h ../netinet/../net/route.h
tcp_subr.o: ../netinet/../net/if.h ../netinet/../net/../net/if_arp.h
tcp_subr.o: ../netinet/in.h ../netinet/in_pcb.h ../netinet/in_systm.h
tcp_subr.o: ../netinet/ip.h ../netinet/ip_var.h ../netinet/ip_icmp.h
tcp_subr.o: ../netinet/tcp.h ../netinet/tcp_fsm.h ../netinet/tcp_seq.h
tcp_subr.o: ../netinet/tcp_timer.h ../netinet/tcp_var.h ../netinet/tcpip.h
tcp_timer.o: ../netinet/tcp_timer.c ../h/param.h ../h/types.h ../h/signal.h
tcp_timer.o: ../h/../machine/trap.h ../h/../machine/machparam.h
tcp_timer.o: machine/endian.h ../h/systm.h ../h/mbuf.h ../h/socket.h
tcp_timer.o: ../h/socketvar.h ../h/protosw.h ../h/errno.h
tcp_timer.o: ../netinet/../net/if.h ../netinet/../net/../net/if_arp.h
tcp_timer.o: ../netinet/../net/route.h ../netinet/in.h ../netinet/in_pcb.h
tcp_timer.o: ../netinet/in_systm.h ../netinet/ip.h ../netinet/ip_var.h
tcp_timer.o: ../netinet/tcp.h ../netinet/tcp_fsm.h ../netinet/tcp_seq.h
tcp_timer.o: ../netinet/tcp_timer.h ../netinet/tcp_var.h ../netinet/tcpip.h
tcp_usrreq.o: ../netinet/tcp_usrreq.c ../h/param.h ../h/types.h ../h/signal.h
tcp_usrreq.o: ../h/../machine/trap.h ../h/../machine/machparam.h
tcp_usrreq.o: machine/endian.h ../h/systm.h ../h/mbuf.h ../h/socket.h
tcp_usrreq.o: ../h/socketvar.h ../h/protosw.h ../h/errno.h ../h/stat.h
tcp_usrreq.o: ../netinet/../net/if.h ../netinet/../net/../net/if_arp.h
tcp_usrreq.o: ../netinet/../net/route.h ../netinet/in.h ../netinet/in_pcb.h
tcp_usrreq.o: ../netinet/in_systm.h ../netinet/ip.h ../netinet/ip_var.h
tcp_usrreq.o: ../netinet/tcp.h ../netinet/tcp_fsm.h ../netinet/tcp_seq.h
tcp_usrreq.o: ../netinet/tcp_timer.h ../netinet/tcp_var.h ../netinet/tcpip.h
tcp_usrreq.o: ../netinet/tcp_debug.h
udp_usrreq.o: ../netinet/udp_usrreq.c ../h/param.h ../h/types.h ../h/signal.h
udp_usrreq.o: ../h/../machine/trap.h ../h/../machine/machparam.h
udp_usrreq.o: machine/endian.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h
udp_usrreq.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
udp_usrreq.o: ../h/errno.h ../h/mbuf.h ../h/protosw.h ../h/socket.h
udp_usrreq.o: ../h/socketvar.h ../h/errno.h ../h/stat.h ../netinet/../net/if.h
udp_usrreq.o: ../netinet/../net/../net/if_arp.h ../netinet/../net/route.h
udp_usrreq.o: ../netinet/in.h ../netinet/in_pcb.h ../netinet/in_systm.h
udp_usrreq.o: ../netinet/ip.h ../netinet/ip_var.h ../netinet/ip_icmp.h
udp_usrreq.o: ../netinet/udp.h ../netinet/udp_var.h
idp_usrreq.o: ../netns/idp_usrreq.c ../h/param.h ../h/types.h ../h/signal.h
idp_usrreq.o: ../h/../machine/trap.h ../h/../machine/machparam.h
idp_usrreq.o: machine/endian.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h
idp_usrreq.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
idp_usrreq.o: ../h/errno.h ../h/mbuf.h ../h/protosw.h ../h/socket.h
idp_usrreq.o: ../h/socketvar.h ../h/errno.h ../h/stat.h ../netns/../net/if.h
idp_usrreq.o: ../netns/../net/../net/if_arp.h ../netns/../net/route.h
idp_usrreq.o: ../netns/ns.h ../netns/ns_pcb.h ../netns/ns_if.h ../netns/idp.h
idp_usrreq.o: ../netns/idp_var.h ../netns/ns_error.h
ns.o: ../netns/ns.c ../h/param.h ../h/types.h ../h/signal.h
ns.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
ns.o: ../h/mbuf.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/protosw.h
ns.o: ../h/socket.h ../h/socketvar.h ../h/uio.h ../h/dir.h ../h/user.h
ns.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
ns.o: ../h/namei.h ../h/uio.h ../h/errno.h ../netns/../net/if.h
ns.o: ../netns/../net/../net/if_arp.h ../netns/../net/route.h
ns.o: ../netns/../net/af.h ../netns/ns.h ../netns/ns_if.h
ns_error.o: ../netns/ns_error.c ../h/param.h ../h/types.h ../h/signal.h
ns_error.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
ns_error.o: ../h/systm.h ../h/mbuf.h ../h/protosw.h ../h/socket.h ../h/time.h
ns_error.o: ../h/kernel.h ../netns/../net/route.h ../netns/ns.h
ns_error.o: ../netns/ns_pcb.h ../netns/idp.h ../netns/ns_error.h
ns_ip.o: ../netns/ns_ip.c
ns_input.o: ../netns/ns_input.c ../h/param.h ../h/types.h ../h/signal.h
ns_input.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
ns_input.o: ../h/systm.h ../h/mbuf.h ../h/domain.h ../h/protosw.h ../h/socket.h
ns_input.o: ../h/socketvar.h ../h/errno.h ../h/time.h ../h/kernel.h
ns_input.o: ../netns/../net/if.h ../netns/../net/../net/if_arp.h
ns_input.o: ../netns/../net/route.h ../netns/../net/raw_cb.h ../netns/ns.h
ns_input.o: ../netns/ns_if.h ../netns/ns_pcb.h ../netns/idp.h
ns_input.o: ../netns/idp_var.h ../netns/ns_error.h
ns_output.o: ../netns/ns_output.c ../h/param.h ../h/types.h ../h/signal.h
ns_output.o: ../h/../machine/trap.h ../h/../machine/machparam.h
ns_output.o: machine/endian.h ../h/mbuf.h ../h/errno.h ../h/socket.h
ns_output.o: ../h/socketvar.h ../netns/../net/if.h
ns_output.o: ../netns/../net/../net/if_arp.h ../netns/../net/route.h
ns_output.o: ../netns/ns.h ../netns/ns_if.h ../netns/idp.h ../netns/idp_var.h
ns_pcb.o: ../netns/ns_pcb.c ../h/param.h ../h/types.h ../h/signal.h
ns_pcb.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
ns_pcb.o: ../h/systm.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h ../h/dmap.h
ns_pcb.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
ns_pcb.o: ../h/mbuf.h ../h/socket.h ../h/socketvar.h ../netns/../net/if.h
ns_pcb.o: ../netns/../net/../net/if_arp.h ../netns/../net/route.h
ns_pcb.o: ../h/protosw.h ../netns/ns.h ../netns/ns_if.h ../netns/ns_pcb.h
ns_proto.o: ../netns/ns_proto.c ../h/param.h ../h/types.h ../h/signal.h
ns_proto.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
ns_proto.o: ../h/socket.h ../h/protosw.h ../h/domain.h ../h/mbuf.h
ns_proto.o: ../netns/ns.h
spp_debug.o: ../netns/spp_debug.c ../h/param.h ../h/types.h ../h/signal.h
spp_debug.o: ../h/../machine/trap.h ../h/../machine/machparam.h
spp_debug.o: machine/endian.h ../h/systm.h ../h/mbuf.h ../h/socket.h
spp_debug.o: ../h/socketvar.h ../h/protosw.h ../h/errno.h
spp_debug.o: ../netns/../net/route.h ../netns/../net/if.h
spp_debug.o: ../netns/../net/../net/if_arp.h ../netns/../netinet/tcp_fsm.h
spp_debug.o: ../netns/ns.h ../netns/ns_pcb.h ../netns/idp.h ../netns/idp_var.h
spp_debug.o: ../netns/sp.h ../netns/spidp.h ../netns/spp_timer.h
spp_debug.o: ../netns/spp_var.h ../netns/spp_debug.h
spp_usrreq.o: ../netns/spp_usrreq.c ../h/param.h ../h/types.h ../h/signal.h
spp_usrreq.o: ../h/../machine/trap.h ../h/../machine/machparam.h
spp_usrreq.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
spp_usrreq.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
spp_usrreq.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/mbuf.h ../h/protosw.h
spp_usrreq.o: ../h/socket.h ../h/socketvar.h ../h/errno.h ../netns/../net/if.h
spp_usrreq.o: ../netns/../net/../net/if_arp.h ../netns/../net/route.h
spp_usrreq.o: ../netns/../netinet/tcp_fsm.h ../netns/ns.h ../netns/ns_pcb.h
spp_usrreq.o: ../netns/idp.h ../netns/idp_var.h ../netns/ns_error.h
spp_usrreq.o: ../netns/sp.h ../netns/spidp.h ../netns/spp_timer.h
spp_usrreq.o: ../netns/spp_var.h ../netns/spp_debug.h
init_main.o: ../sys/init_main.c ../sys/../machine/pte.h ../h/param.h
init_main.o: ../h/types.h ../h/signal.h ../h/../machine/trap.h
init_main.o: ../h/../machine/machparam.h machine/endian.h ../h/systm.h
init_main.o: ../h/dir.h ../h/user.h ../h/../machine/pcb.h ../h/dmap.h
init_main.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
init_main.o: ../h/kernel.h ../h/fs.h ../h/mount.h ../h/map.h ../h/proc.h
init_main.o: ../h/inode.h ../h/seg.h ../h/conf.h ../h/buf.h ../h/vm.h
init_main.o: ../h/vmparam.h ../h/../machine/vmparam.h ../h/vmmac.h
init_main.o: ../h/vmmeter.h ../h/vmsystm.h ../h/cmap.h ../h/text.h ../h/clist.h
init_main.o: ../h/protosw.h ../h/quota.h ../h/reboot.h ../sys/../machine/reg.h
init_main.o: ../sys/../machine/cpu.h loop.h sl.h
init_sysent.o: ../sys/init_sysent.c ../h/param.h ../h/types.h ../h/signal.h
init_sysent.o: ../h/../machine/trap.h ../h/../machine/machparam.h
init_sysent.o: machine/endian.h ../h/systm.h
kern_acct.o: ../sys/kern_acct.c ../h/param.h ../h/types.h ../h/signal.h
kern_acct.o: ../h/../machine/trap.h ../h/../machine/machparam.h
kern_acct.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
kern_acct.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
kern_acct.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/inode.h ../h/fs.h
kern_acct.o: ../h/kernel.h ../h/acct.h ../h/uio.h
kern_clock.o: ../sys/kern_clock.c ../sys/../machine/reg.h
kern_clock.o: ../sys/../machine/psl.h ../h/param.h ../h/types.h ../h/signal.h
kern_clock.o: ../h/../machine/trap.h ../h/../machine/machparam.h
kern_clock.o: machine/endian.h ../h/systm.h ../h/dkstat.h ../h/callout.h
kern_clock.o: ../h/dir.h ../h/user.h ../h/../machine/pcb.h ../h/dmap.h
kern_clock.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
kern_clock.o: ../h/kernel.h ../h/proc.h ../h/vm.h ../h/vmparam.h
kern_clock.o: ../h/../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
kern_clock.o: ../h/vmsystm.h ../h/text.h ../sys/../machine/mtpr.h
kern_clock.o: ../sys/../machine/clock.h
kern_descrip.o: ../sys/kern_descrip.c ../h/param.h ../h/types.h ../h/signal.h
kern_descrip.o: ../h/../machine/trap.h ../h/../machine/machparam.h
kern_descrip.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
kern_descrip.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
kern_descrip.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/kernel.h ../h/inode.h
kern_descrip.o: ../h/proc.h ../h/file.h ../h/socket.h ../h/socketvar.h
kern_descrip.o: ../h/mount.h ../h/stat.h ../h/ioctl.h ../h/ttychars.h
kern_descrip.o: ../h/ttydev.h
kern_exec.o: ../sys/kern_exec.c ../sys/../machine/reg.h ../sys/../machine/pte.h
kern_exec.o: ../sys/../machine/psl.h ../sys/../machine/mtpr.h ../h/param.h
kern_exec.o: ../h/types.h ../h/signal.h ../h/../machine/trap.h
kern_exec.o: ../h/../machine/machparam.h machine/endian.h ../h/systm.h
kern_exec.o: ../h/map.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h
kern_exec.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
kern_exec.o: ../h/errno.h ../h/kernel.h ../h/proc.h ../h/buf.h ../h/inode.h
kern_exec.o: ../h/seg.h ../h/vm.h ../h/vmparam.h ../h/../machine/vmparam.h
kern_exec.o: ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h ../h/text.h ../h/file.h
kern_exec.o: ../h/uio.h ../h/acct.h ../h/exec.h
kern_exit.o: ../sys/kern_exit.c ../sys/../machine/reg.h ../sys/../machine/psl.h
kern_exit.o: ../h/param.h ../h/types.h ../h/signal.h ../h/../machine/trap.h
kern_exit.o: ../h/../machine/machparam.h machine/endian.h ../h/systm.h
kern_exit.o: ../h/map.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h
kern_exit.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
kern_exit.o: ../h/errno.h ../h/kernel.h ../h/proc.h ../h/buf.h ../h/wait.h
kern_exit.o: ../h/vm.h ../h/vmparam.h ../h/../machine/vmparam.h ../h/vmmac.h
kern_exit.o: ../h/vmmeter.h ../h/vmsystm.h ../h/file.h ../h/inode.h
kern_exit.o: ../h/syslog.h ../h/malloc.h
kern_fork.o: ../sys/kern_fork.c ../sys/../machine/reg.h ../sys/../machine/pte.h
kern_fork.o: ../sys/../machine/psl.h ../h/param.h ../h/types.h ../h/signal.h
kern_fork.o: ../h/../machine/trap.h ../h/../machine/machparam.h
kern_fork.o: machine/endian.h ../h/systm.h ../h/map.h ../h/dir.h ../h/user.h
kern_fork.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
kern_fork.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/kernel.h ../h/proc.h
kern_fork.o: ../h/inode.h ../h/seg.h ../h/vm.h ../h/vmparam.h
kern_fork.o: ../h/../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
kern_fork.o: ../h/vmsystm.h ../h/text.h ../h/file.h ../h/acct.h ../h/quota.h
kern_malloc.o: ../sys/kern_malloc.c ../h/param.h ../h/types.h ../h/signal.h
kern_malloc.o: ../h/../machine/trap.h ../h/../machine/machparam.h
kern_malloc.o: machine/endian.h ../h/vm.h ../h/vmparam.h
kern_malloc.o: ../h/../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
kern_malloc.o: ../h/vmsystm.h ../h/cmap.h ../h/time.h ../h/proc.h ../h/map.h
kern_malloc.o: ../h/kernel.h ../h/malloc.h ../sys/../machine/pte.h
kern_mman.o: ../sys/kern_mman.c ../h/param.h ../h/types.h ../h/signal.h
kern_mman.o: ../h/../machine/trap.h ../h/../machine/machparam.h
kern_mman.o: machine/endian.h ../h/systm.h ../h/map.h ../h/dir.h ../h/user.h
kern_mman.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
kern_mman.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/buf.h
kern_mman.o: ../h/inode.h ../h/seg.h ../h/acct.h ../h/wait.h ../h/vm.h
kern_mman.o: ../h/vmparam.h ../h/../machine/vmparam.h ../h/vmmac.h
kern_mman.o: ../h/vmmeter.h ../h/vmsystm.h ../h/text.h ../h/file.h
kern_mman.o: ../h/vadvise.h ../h/cmap.h ../h/trace.h ../h/mman.h ../h/conf.h
kern_mman.o: ../sys/../machine/cpu.h ../sys/../machine/reg.h
kern_mman.o: ../sys/../machine/psl.h ../sys/../machine/pte.h
kern_mman.o: ../sys/../machine/mtpr.h
kern_proc.o: ../sys/kern_proc.c ../sys/../machine/reg.h ../sys/../machine/pte.h
kern_proc.o: ../sys/../machine/psl.h ../h/param.h ../h/types.h ../h/signal.h
kern_proc.o: ../h/../machine/trap.h ../h/../machine/machparam.h
kern_proc.o: machine/endian.h ../h/systm.h ../h/map.h ../h/dir.h ../h/user.h
kern_proc.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
kern_proc.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/kernel.h ../h/proc.h
kern_proc.o: ../h/buf.h ../h/seg.h ../h/acct.h ../h/wait.h ../h/vm.h
kern_proc.o: ../h/vmparam.h ../h/../machine/vmparam.h ../h/vmmac.h
kern_proc.o: ../h/vmmeter.h ../h/vmsystm.h ../h/text.h ../h/file.h ../h/quota.h
kern_proc.o: ../h/uio.h ../h/mbuf.h
kern_prot.o: ../sys/kern_prot.c ../sys/../machine/reg.h ../h/param.h
kern_prot.o: ../h/types.h ../h/signal.h ../h/../machine/trap.h
kern_prot.o: ../h/../machine/machparam.h machine/endian.h ../h/systm.h
kern_prot.o: ../h/dir.h ../h/user.h ../h/../machine/pcb.h ../h/dmap.h
kern_prot.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
kern_prot.o: ../h/inode.h ../h/proc.h ../h/timeb.h ../h/times.h ../h/reboot.h
kern_prot.o: ../h/fs.h ../h/buf.h ../h/mount.h ../h/quota.h
kern_resource.o: ../sys/kern_resource.c ../h/param.h ../h/types.h ../h/signal.h
kern_resource.o: ../h/../machine/trap.h ../h/../machine/machparam.h
kern_resource.o: machine/endian.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h
kern_resource.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h
kern_resource.o: ../h/uio.h ../h/errno.h ../h/proc.h
kern_sig.o: ../sys/kern_sig.c ../sys/../machine/reg.h ../sys/../machine/pte.h
kern_sig.o: ../sys/../machine/psl.h ../sys/../machine/mtpr.h ../h/param.h
kern_sig.o: ../h/types.h ../h/signal.h ../h/../machine/trap.h
kern_sig.o: ../h/../machine/machparam.h machine/endian.h ../h/systm.h
kern_sig.o: ../h/dir.h ../h/user.h ../h/../machine/pcb.h ../h/dmap.h
kern_sig.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
kern_sig.o: ../h/inode.h ../h/proc.h ../h/timeb.h ../h/times.h ../h/buf.h
kern_sig.o: ../h/mount.h ../h/text.h ../h/seg.h ../h/vm.h ../h/vmparam.h
kern_sig.o: ../h/../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
kern_sig.o: ../h/vmsystm.h ../h/acct.h ../h/uio.h ../h/kernel.h
kern_subr.o: ../sys/kern_subr.c ../h/param.h ../h/types.h ../h/signal.h
kern_subr.o: ../h/../machine/trap.h ../h/../machine/machparam.h
kern_subr.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
kern_subr.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
kern_subr.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/uio.h
kern_synch.o: ../sys/kern_synch.c ../sys/../machine/pte.h
kern_synch.o: ../sys/../machine/psl.h ../sys/../machine/mtpr.h ../h/param.h
kern_synch.o: ../h/types.h ../h/signal.h ../h/../machine/trap.h
kern_synch.o: ../h/../machine/machparam.h machine/endian.h ../h/systm.h
kern_synch.o: ../h/dir.h ../h/user.h ../h/../machine/pcb.h ../h/dmap.h
kern_synch.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
kern_synch.o: ../h/proc.h ../h/vm.h ../h/vmparam.h ../h/../machine/vmparam.h
kern_synch.o: ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h ../h/kernel.h
kern_synch.o: ../h/buf.h
kern_time.o: ../sys/kern_time.c ../h/param.h ../h/types.h ../h/signal.h
kern_time.o: ../h/../machine/trap.h ../h/../machine/machparam.h
kern_time.o: machine/endian.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h
kern_time.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
kern_time.o: ../h/errno.h ../h/kernel.h ../h/proc.h ../sys/../machine/reg.h
kern_time.o: ../sys/../machine/cpu.h
kern_xxx.o: ../sys/kern_xxx.c ../h/param.h ../h/types.h ../h/signal.h
kern_xxx.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
kern_xxx.o: ../h/systm.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h
kern_xxx.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
kern_xxx.o: ../h/errno.h ../h/kernel.h ../h/proc.h ../h/reboot.h
quota_kern.o: ../sys/quota_kern.c ../h/param.h ../h/types.h ../h/signal.h
quota_kern.o: ../h/../machine/trap.h ../h/../machine/machparam.h
quota_kern.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
quota_kern.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
quota_kern.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/inode.h
quota_kern.o: ../h/quota.h ../h/mount.h ../h/fs.h ../h/uio.h
quota_subr.o: ../sys/quota_subr.c ../h/param.h ../h/types.h ../h/signal.h
quota_subr.o: ../h/../machine/trap.h ../h/../machine/machparam.h
quota_subr.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
quota_subr.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
quota_subr.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/inode.h
quota_subr.o: ../h/quota.h ../h/mount.h ../h/fs.h ../h/uio.h
quota_sys.o: ../sys/quota_sys.c ../h/param.h ../h/types.h ../h/signal.h
quota_sys.o: ../h/../machine/trap.h ../h/../machine/machparam.h
quota_sys.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
quota_sys.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
quota_sys.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/inode.h
quota_sys.o: ../h/quota.h ../h/mount.h ../h/fs.h ../h/uio.h
quota_ufs.o: ../sys/quota_ufs.c ../h/param.h ../h/types.h ../h/signal.h
quota_ufs.o: ../h/../machine/trap.h ../h/../machine/machparam.h
quota_ufs.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
quota_ufs.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
quota_ufs.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/inode.h
quota_ufs.o: ../h/quota.h ../h/mount.h ../h/fs.h ../h/uio.h
subr_log.o: ../sys/subr_log.c ../h/param.h ../h/types.h ../h/signal.h
subr_log.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
subr_log.o: ../h/dir.h ../h/user.h ../h/../machine/pcb.h ../h/dmap.h
subr_log.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
subr_log.o: ../h/proc.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h
subr_log.o: ../h/msgbuf.h ../h/file.h ../h/errno.h
subr_prf.o: ../sys/subr_prf.c ../h/param.h ../h/types.h ../h/signal.h
subr_prf.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
subr_prf.o: ../h/systm.h ../h/seg.h ../h/buf.h ../h/conf.h ../h/reboot.h
subr_prf.o: ../h/vm.h ../h/vmparam.h ../h/../machine/vmparam.h ../h/vmmac.h
subr_prf.o: ../h/vmmeter.h ../h/vmsystm.h ../h/msgbuf.h ../h/dir.h ../h/user.h
subr_prf.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
subr_prf.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/ioctl.h
subr_prf.o: ../h/ttychars.h ../h/ttydev.h ../h/tty.h ../h/ttychars.h
subr_prf.o: ../h/ttydev.h ../h/syslog.h ../sys/../machine/mtpr.h
subr_prf.o: ../sys/../machine/kdbparam.h
subr_rmap.o: ../sys/subr_rmap.c ../h/param.h ../h/types.h ../h/signal.h
subr_rmap.o: ../h/../machine/trap.h ../h/../machine/machparam.h
subr_rmap.o: machine/endian.h ../h/systm.h ../h/map.h ../h/dir.h ../h/user.h
subr_rmap.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
subr_rmap.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/text.h
subr_rmap.o: ../h/kernel.h
subr_xxx.o: ../sys/subr_xxx.c ../h/errno.h
sys_generic.o: ../sys/sys_generic.c ../h/param.h ../h/types.h ../h/signal.h
sys_generic.o: ../h/../machine/trap.h ../h/../machine/machparam.h
sys_generic.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
sys_generic.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
sys_generic.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/ioctl.h
sys_generic.o: ../h/ttychars.h ../h/ttydev.h ../h/file.h ../h/proc.h ../h/uio.h
sys_generic.o: ../h/kernel.h ../h/stat.h ../h/malloc.h
sys_inode.o: ../sys/sys_inode.c ../h/param.h ../h/types.h ../h/signal.h
sys_inode.o: ../h/../machine/trap.h ../h/../machine/machparam.h
sys_inode.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
sys_inode.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
sys_inode.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/inode.h ../h/proc.h
sys_inode.o: ../h/fs.h ../h/conf.h ../h/buf.h ../h/mount.h ../h/file.h
sys_inode.o: ../h/uio.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/tty.h
sys_inode.o: ../h/ttychars.h ../h/ttydev.h ../h/cmap.h ../h/stat.h
sys_inode.o: ../h/kernel.h ../h/disklabel.h
sys_process.o: ../sys/sys_process.c ../sys/../machine/reg.h
sys_process.o: ../sys/../machine/psl.h ../sys/../machine/pte.h ../h/param.h
sys_process.o: ../h/types.h ../h/signal.h ../h/../machine/trap.h
sys_process.o: ../h/../machine/machparam.h machine/endian.h ../h/systm.h
sys_process.o: ../h/dir.h ../h/user.h ../h/../machine/pcb.h ../h/dmap.h
sys_process.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
sys_process.o: ../h/proc.h ../h/inode.h ../h/text.h ../h/seg.h ../h/vm.h
sys_process.o: ../h/vmparam.h ../h/../machine/vmparam.h ../h/vmmac.h
sys_process.o: ../h/vmmeter.h ../h/vmsystm.h ../h/buf.h ../h/acct.h
sys_process.o: ../h/ptrace.h
sys_socket.o: ../sys/sys_socket.c ../h/param.h ../h/types.h ../h/signal.h
sys_socket.o: ../h/../machine/trap.h ../h/../machine/machparam.h
sys_socket.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
sys_socket.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
sys_socket.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/file.h ../h/mbuf.h
sys_socket.o: ../h/protosw.h ../h/socket.h ../h/socketvar.h ../h/ioctl.h
sys_socket.o: ../h/ttychars.h ../h/ttydev.h ../h/uio.h ../h/stat.h
sys_socket.o: ../sys/../net/if.h ../sys/../net/../net/if_arp.h
sys_socket.o: ../sys/../net/route.h
tty.o: ../sys/tty.c ../sys/../machine/reg.h ../h/param.h ../h/types.h
tty.o: ../h/signal.h ../h/../machine/trap.h ../h/../machine/machparam.h
tty.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
tty.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
tty.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/ioctl.h ../h/ttychars.h
tty.o: ../h/ttydev.h ../h/tty.h ../h/ttychars.h ../h/ttydev.h ../h/proc.h
tty.o: ../h/file.h ../h/conf.h ../h/dkstat.h ../h/uio.h ../h/kernel.h
tty_conf.o: ../sys/tty_conf.c ../h/param.h ../h/types.h ../h/signal.h
tty_conf.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
tty_conf.o: ../h/systm.h ../h/buf.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h
tty_conf.o: ../h/tty.h ../h/ttychars.h ../h/ttydev.h ../h/conf.h bk.h tb.h sl.h
tty_pty.o: ../sys/tty_pty.c pty.h ../h/param.h ../h/types.h ../h/signal.h
tty_pty.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
tty_pty.o: ../h/systm.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/tty.h
tty_pty.o: ../h/ttychars.h ../h/ttydev.h ../h/dir.h ../h/user.h
tty_pty.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
tty_pty.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/conf.h ../h/file.h
tty_pty.o: ../h/proc.h ../h/uio.h ../h/kernel.h
tty_subr.o: ../sys/tty_subr.c ../h/param.h ../h/types.h ../h/signal.h
tty_subr.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
tty_subr.o: ../h/systm.h ../h/buf.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h
tty_subr.o: ../h/tty.h ../h/ttychars.h ../h/ttydev.h ../h/clist.h
tty_tty.o: ../sys/tty_tty.c ../h/param.h ../h/types.h ../h/signal.h
tty_tty.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
tty_tty.o: ../h/systm.h ../h/conf.h ../h/dir.h ../h/user.h
tty_tty.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
tty_tty.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/ioctl.h ../h/ttychars.h
tty_tty.o: ../h/ttydev.h ../h/tty.h ../h/ttychars.h ../h/ttydev.h ../h/proc.h
tty_tty.o: ../h/uio.h
ufs_alloc.o: ../sys/ufs_alloc.c ../h/param.h ../h/types.h ../h/signal.h
ufs_alloc.o: ../h/../machine/trap.h ../h/../machine/machparam.h
ufs_alloc.o: machine/endian.h ../h/systm.h ../h/mount.h ../h/fs.h ../h/buf.h
ufs_alloc.o: ../h/inode.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h
ufs_alloc.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
ufs_alloc.o: ../h/errno.h ../h/quota.h ../h/kernel.h ../h/syslog.h ../h/cmap.h
ufs_bio.o: ../sys/ufs_bio.c ../sys/../machine/pte.h ../h/param.h ../h/types.h
ufs_bio.o: ../h/signal.h ../h/../machine/trap.h ../h/../machine/machparam.h
ufs_bio.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
ufs_bio.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
ufs_bio.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/buf.h ../h/conf.h
ufs_bio.o: ../h/proc.h ../h/seg.h ../h/vm.h ../h/vmparam.h
ufs_bio.o: ../h/../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
ufs_bio.o: ../h/trace.h
ufs_bmap.o: ../sys/ufs_bmap.c ../h/param.h ../h/types.h ../h/signal.h
ufs_bmap.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
ufs_bmap.o: ../h/systm.h ../h/inode.h ../h/dir.h ../h/user.h
ufs_bmap.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
ufs_bmap.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/buf.h ../h/proc.h
ufs_bmap.o: ../h/fs.h
ufs_disksubr.o: ../sys/ufs_disksubr.c ../h/param.h ../h/types.h ../h/signal.h
ufs_disksubr.o: ../h/../machine/trap.h ../h/../machine/machparam.h
ufs_disksubr.o: machine/endian.h ../h/systm.h ../h/buf.h ../h/disklabel.h
ufs_disksubr.o: ../h/syslog.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h
ufs_disksubr.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
ufs_disksubr.o: ../h/errno.h
ufs_fio.o: ../sys/ufs_fio.c ../sys/../machine/reg.h ../h/param.h ../h/types.h
ufs_fio.o: ../h/signal.h ../h/../machine/trap.h ../h/../machine/machparam.h
ufs_fio.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
ufs_fio.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
ufs_fio.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/fs.h ../h/file.h
ufs_fio.o: ../h/conf.h ../h/inode.h ../h/acct.h ../h/mount.h ../h/socket.h
ufs_fio.o: ../h/socketvar.h ../h/proc.h
ufs_inode.o: ../sys/ufs_inode.c ../h/param.h ../h/types.h ../h/signal.h
ufs_inode.o: ../h/../machine/trap.h ../h/../machine/machparam.h
ufs_inode.o: machine/endian.h ../h/systm.h ../h/mount.h ../h/dir.h ../h/user.h
ufs_inode.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
ufs_inode.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/inode.h ../h/fs.h
ufs_inode.o: ../h/buf.h ../h/cmap.h ../h/quota.h ../h/kernel.h ../h/malloc.h
ufs_mount.o: ../sys/ufs_mount.c ../h/param.h ../h/types.h ../h/signal.h
ufs_mount.o: ../h/../machine/trap.h ../h/../machine/machparam.h
ufs_mount.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
ufs_mount.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
ufs_mount.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/inode.h ../h/proc.h
ufs_mount.o: ../h/fs.h ../h/buf.h ../h/mount.h ../h/file.h ../h/conf.h
ufs_mount.o: ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/disklabel.h
ufs_mount.o: ../h/stat.h ../h/malloc.h
ufs_namei.o: ../sys/ufs_namei.c ../h/param.h ../h/types.h ../h/signal.h
ufs_namei.o: ../h/../machine/trap.h ../h/../machine/machparam.h
ufs_namei.o: machine/endian.h ../h/systm.h ../h/inode.h ../h/fs.h ../h/mount.h
ufs_namei.o: ../h/dir.h ../h/user.h ../h/../machine/pcb.h ../h/dmap.h
ufs_namei.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
ufs_namei.o: ../h/buf.h ../h/conf.h ../h/uio.h ../h/kernel.h ../h/malloc.h
ufs_subr.o: ../sys/ufs_subr.c ../h/param.h ../h/types.h ../h/signal.h
ufs_subr.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
ufs_subr.o: ../h/systm.h ../h/mount.h ../h/fs.h ../h/buf.h ../h/inode.h
ufs_subr.o: ../h/dir.h ../h/user.h ../h/../machine/pcb.h ../h/dmap.h
ufs_subr.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
ufs_subr.o: ../h/quota.h ../h/kernel.h
ufs_syscalls.o: ../sys/ufs_syscalls.c ../h/param.h ../h/types.h ../h/signal.h
ufs_syscalls.o: ../h/../machine/trap.h ../h/../machine/machparam.h
ufs_syscalls.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
ufs_syscalls.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
ufs_syscalls.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/kernel.h ../h/file.h
ufs_syscalls.o: ../h/stat.h ../h/inode.h ../h/fs.h ../h/buf.h ../h/proc.h
ufs_syscalls.o: ../h/quota.h ../h/uio.h ../h/socket.h ../h/socketvar.h
ufs_syscalls.o: ../h/mount.h
ufs_tables.o: ../sys/ufs_tables.c ../h/param.h ../h/types.h ../h/signal.h
ufs_tables.o: ../h/../machine/trap.h ../h/../machine/machparam.h
ufs_tables.o: machine/endian.h
ufs_xxx.o: ../sys/ufs_xxx.c ../h/param.h ../h/types.h ../h/signal.h
ufs_xxx.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
ufs_xxx.o: ../h/systm.h ../h/inode.h ../h/fs.h ../h/mount.h ../h/dir.h
ufs_xxx.o: ../h/user.h ../h/../machine/pcb.h ../h/dmap.h ../h/time.h
ufs_xxx.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/buf.h
ufs_xxx.o: ../h/conf.h
uipc_domain.o: ../sys/uipc_domain.c ../h/param.h ../h/types.h ../h/signal.h
uipc_domain.o: ../h/../machine/trap.h ../h/../machine/machparam.h
uipc_domain.o: machine/endian.h ../h/socket.h ../h/protosw.h ../h/domain.h
uipc_domain.o: ../h/time.h ../h/kernel.h imp.h
uipc_mbuf.o: ../sys/uipc_mbuf.c ../sys/../machine/pte.h ../h/param.h
uipc_mbuf.o: ../h/types.h ../h/signal.h ../h/../machine/trap.h
uipc_mbuf.o: ../h/../machine/machparam.h machine/endian.h ../h/dir.h
uipc_mbuf.o: ../h/user.h ../h/../machine/pcb.h ../h/dmap.h ../h/time.h
uipc_mbuf.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h
uipc_mbuf.o: ../h/cmap.h ../h/map.h ../h/mbuf.h ../h/vm.h ../h/vmparam.h
uipc_mbuf.o: ../h/../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
uipc_mbuf.o: ../h/vmsystm.h ../h/kernel.h ../h/syslog.h ../h/domain.h
uipc_mbuf.o: ../h/protosw.h
uipc_proto.o: ../sys/uipc_proto.c ../h/param.h ../h/types.h ../h/signal.h
uipc_proto.o: ../h/../machine/trap.h ../h/../machine/machparam.h
uipc_proto.o: machine/endian.h ../h/socket.h ../h/protosw.h ../h/domain.h
uipc_proto.o: ../h/mbuf.h
uipc_socket.o: ../sys/uipc_socket.c ../h/param.h ../h/types.h ../h/signal.h
uipc_socket.o: ../h/../machine/trap.h ../h/../machine/machparam.h
uipc_socket.o: machine/endian.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h
uipc_socket.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
uipc_socket.o: ../h/errno.h ../h/proc.h ../h/file.h ../h/mbuf.h ../h/domain.h
uipc_socket.o: ../h/protosw.h ../h/socket.h ../h/socketvar.h
uipc_socket2.o: ../sys/uipc_socket2.c ../h/param.h ../h/types.h ../h/signal.h
uipc_socket2.o: ../h/../machine/trap.h ../h/../machine/machparam.h
uipc_socket2.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
uipc_socket2.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
uipc_socket2.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/file.h
uipc_socket2.o: ../h/inode.h ../h/buf.h ../h/mbuf.h ../h/protosw.h
uipc_socket2.o: ../h/socket.h ../h/socketvar.h
uipc_syscalls.o: ../sys/uipc_syscalls.c ../h/param.h ../h/types.h ../h/signal.h
uipc_syscalls.o: ../h/../machine/trap.h ../h/../machine/machparam.h
uipc_syscalls.o: machine/endian.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h
uipc_syscalls.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h
uipc_syscalls.o: ../h/uio.h ../h/errno.h ../h/file.h ../h/buf.h ../h/mbuf.h
uipc_syscalls.o: ../h/protosw.h ../h/socket.h ../h/socketvar.h
uipc_usrreq.o: ../sys/uipc_usrreq.c ../h/param.h ../h/types.h ../h/signal.h
uipc_usrreq.o: ../h/../machine/trap.h ../h/../machine/machparam.h
uipc_usrreq.o: machine/endian.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h
uipc_usrreq.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
uipc_usrreq.o: ../h/errno.h ../h/mbuf.h ../h/domain.h ../h/protosw.h
uipc_usrreq.o: ../h/socket.h ../h/socketvar.h ../h/unpcb.h ../h/un.h
uipc_usrreq.o: ../h/inode.h ../h/file.h ../h/stat.h
vm_drum.o: ../sys/vm_drum.c ../sys/../machine/pte.h ../h/param.h ../h/types.h
vm_drum.o: ../h/signal.h ../h/../machine/trap.h ../h/../machine/machparam.h
vm_drum.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
vm_drum.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
vm_drum.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/buf.h
vm_drum.o: ../h/text.h ../h/map.h ../h/vm.h ../h/vmparam.h
vm_drum.o: ../h/../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
vm_drum.o: ../h/cmap.h ../h/kernel.h
vm_mem.o: ../sys/vm_mem.c ../h/param.h ../h/types.h ../h/signal.h
vm_mem.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
vm_mem.o: ../h/systm.h ../h/cmap.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h
vm_mem.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
vm_mem.o: ../h/errno.h ../h/proc.h ../h/text.h ../h/vm.h ../h/vmparam.h
vm_mem.o: ../h/../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
vm_mem.o: ../h/file.h ../h/buf.h ../h/mount.h ../h/trace.h ../h/map.h
vm_mem.o: ../sys/../machine/cpu.h ../sys/../machine/pte.h
vm_mem.o: ../sys/../machine/mtpr.h
vm_mon.o: ../sys/vm_mon.c ../h/param.h ../h/types.h ../h/signal.h
vm_mon.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
vm_mon.o: ../h/systm.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h ../h/dmap.h
vm_mon.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
vm_mon.o: ../h/vmmeter.h ../h/trace.h ../h/kernel.h
vm_page.o: ../sys/vm_page.c ../h/param.h ../h/types.h ../h/signal.h
vm_page.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
vm_page.o: ../h/systm.h ../h/inode.h ../h/dir.h ../h/user.h
vm_page.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
vm_page.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/buf.h
vm_page.o: ../h/text.h ../h/cmap.h ../h/vm.h ../h/vmparam.h
vm_page.o: ../h/../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
vm_page.o: ../h/file.h ../h/trace.h ../sys/../machine/cpu.h
vm_page.o: ../sys/../machine/reg.h ../sys/../machine/pte.h
vm_page.o: ../sys/../machine/mtpr.h
vm_proc.o: ../sys/vm_proc.c ../h/param.h ../h/types.h ../h/signal.h
vm_proc.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
vm_proc.o: ../h/systm.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h
vm_proc.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
vm_proc.o: ../h/errno.h ../h/proc.h ../h/map.h ../h/cmap.h ../h/text.h
vm_proc.o: ../h/vm.h ../h/vmparam.h ../h/../machine/vmparam.h ../h/vmmac.h
vm_proc.o: ../h/vmmeter.h ../h/vmsystm.h ../sys/../machine/pte.h
vm_proc.o: ../sys/../machine/mtpr.h ../sys/../machine/cpu.h
vm_pt.o: ../sys/vm_pt.c ../sys/../machine/pte.h ../sys/../machine/mtpr.h
vm_pt.o: ../h/param.h ../h/types.h ../h/signal.h ../h/../machine/trap.h
vm_pt.o: ../h/../machine/machparam.h machine/endian.h ../h/systm.h ../h/dir.h
vm_pt.o: ../h/user.h ../h/../machine/pcb.h ../h/dmap.h ../h/time.h
vm_pt.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h
vm_pt.o: ../h/map.h ../h/cmap.h ../h/vm.h ../h/vmparam.h
vm_pt.o: ../h/../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
vm_pt.o: ../h/buf.h ../h/text.h ../h/mount.h ../h/inode.h ../h/kernel.h
vm_sched.o: ../sys/vm_sched.c ../h/param.h ../h/types.h ../h/signal.h
vm_sched.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
vm_sched.o: ../h/systm.h ../h/seg.h ../h/dir.h ../h/user.h
vm_sched.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
vm_sched.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/text.h
vm_sched.o: ../h/vm.h ../h/vmparam.h ../h/../machine/vmparam.h ../h/vmmac.h
vm_sched.o: ../h/vmmeter.h ../h/vmsystm.h ../h/cmap.h ../h/kernel.h
vm_subr.o: ../sys/vm_subr.c ../sys/../machine/pte.h ../sys/../machine/mtpr.h
vm_subr.o: ../h/param.h ../h/types.h ../h/signal.h ../h/../machine/trap.h
vm_subr.o: ../h/../machine/machparam.h machine/endian.h ../h/systm.h ../h/dir.h
vm_subr.o: ../h/user.h ../h/../machine/pcb.h ../h/dmap.h ../h/time.h
vm_subr.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/vm.h
vm_subr.o: ../h/vmparam.h ../h/../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
vm_subr.o: ../h/vmsystm.h ../h/proc.h ../h/cmap.h ../h/inode.h ../h/buf.h
vm_subr.o: ../h/text.h ../h/fs.h
vm_sw.o: ../sys/vm_sw.c ../h/param.h ../h/types.h ../h/signal.h
vm_sw.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
vm_sw.o: ../h/systm.h ../h/buf.h ../h/conf.h ../h/dir.h ../h/user.h
vm_sw.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
vm_sw.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/inode.h ../h/map.h
vm_sw.o: ../h/uio.h ../h/file.h ../h/stat.h
vm_swap.o: ../sys/vm_swap.c ../h/param.h ../h/types.h ../h/signal.h
vm_swap.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
vm_swap.o: ../h/systm.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h
vm_swap.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
vm_swap.o: ../h/errno.h ../h/proc.h ../h/text.h ../h/map.h ../h/buf.h
vm_swap.o: ../h/cmap.h ../h/vm.h ../h/vmparam.h ../h/../machine/vmparam.h
vm_swap.o: ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h ../sys/../machine/cpu.h
vm_swap.o: ../sys/../machine/pte.h ../sys/../machine/mtpr.h
vm_swp.o: ../sys/vm_swp.c ../sys/../machine/pte.h ../h/param.h ../h/types.h
vm_swp.o: ../h/signal.h ../h/../machine/trap.h ../h/../machine/machparam.h
vm_swp.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
vm_swp.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
vm_swp.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/buf.h ../h/conf.h
vm_swp.o: ../h/proc.h ../h/seg.h ../h/vm.h ../h/vmparam.h
vm_swp.o: ../h/../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
vm_swp.o: ../h/trace.h ../h/map.h ../h/uio.h
vm_text.o: ../sys/vm_text.c ../h/param.h ../h/types.h ../h/signal.h
vm_text.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
vm_text.o: ../h/systm.h ../h/map.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h
vm_text.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
vm_text.o: ../h/errno.h ../h/proc.h ../h/text.h ../h/inode.h ../h/buf.h
vm_text.o: ../h/seg.h ../h/vm.h ../h/vmparam.h ../h/../machine/vmparam.h
vm_text.o: ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h ../h/cmap.h ../h/uio.h
vm_text.o: ../h/exec.h ../sys/../machine/pte.h ../sys/../machine/cpu.h
autoconf.o: ../tahoe/autoconf.c ../h/param.h ../h/types.h ../h/signal.h
autoconf.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
autoconf.o: ../h/systm.h ../h/map.h ../h/buf.h ../h/dkstat.h ../h/vm.h
autoconf.o: ../h/vmparam.h ../h/../machine/vmparam.h ../h/vmmac.h
autoconf.o: ../h/vmmeter.h ../h/vmsystm.h ../h/conf.h ../h/dmap.h ../h/reboot.h
autoconf.o: ../h/malloc.h ../tahoe/pte.h ../tahoe/mem.h ../tahoe/mtpr.h
autoconf.o: ../tahoe/scb.h vba.h ../tahoe/../tahoevba/vbavar.h
autoconf.o: ../tahoe/../tahoevba/vbaparam.h
conf.o: ../tahoe/conf.c ../h/param.h ../h/types.h ../h/signal.h
conf.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
conf.o: ../h/systm.h ../h/buf.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h
conf.o: ../h/tty.h ../h/ttychars.h ../h/ttydev.h ../h/conf.h dk.h hd.h yc.h
conf.o: vx.h pty.h mp.h enp.h dr.h ik.h
cons.o: ../tahoe/cons.c ../h/param.h ../h/types.h ../h/signal.h
cons.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
cons.o: ../h/conf.h ../h/dir.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h
cons.o: ../h/user.h ../h/../machine/pcb.h ../h/dmap.h ../h/time.h
cons.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h
cons.o: ../h/tty.h ../h/ttychars.h ../h/ttydev.h ../h/uio.h ../h/callout.h
cons.o: ../h/systm.h ../h/kernel.h ../h/syslog.h ../tahoe/cp.h ../tahoe/cpu.h
cons.o: ../tahoe/mtpr.h
clock.o: ../tahoe/clock.c ../h/param.h ../h/types.h ../h/signal.h
clock.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
clock.o: ../h/time.h ../h/kernel.h ../tahoe/pte.h ../tahoe/cpu.h
clock.o: ../tahoe/mtpr.h ../tahoe/clock.h ../tahoe/cp.h
kdb_machdep.o: ../tahoe/kdb_machdep.c ../h/param.h ../h/types.h ../h/signal.h
kdb_machdep.o: ../h/../machine/trap.h ../h/../machine/machparam.h
kdb_machdep.o: machine/endian.h ../h/conf.h ../h/dir.h ../h/user.h
kdb_machdep.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
kdb_machdep.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/uio.h
kdb_machdep.o: ../h/systm.h ../h/reboot.h ../h/vmmac.h ../h/ioctl.h
kdb_machdep.o: ../h/ttychars.h ../h/ttydev.h ../h/tty.h ../h/ttychars.h
kdb_machdep.o: ../h/ttydev.h ../tahoe/cpu.h ../tahoe/mtpr.h ../tahoe/psl.h
kdb_machdep.o: ../tahoe/pte.h ../tahoe/reg.h ../tahoe/trap.h
kdb_machdep.o: ../tahoe/kdbparam.h
kdb_opset.o: ../tahoe/kdb_opset.c ../tahoe/../kdb/defs.h ../h/param.h
kdb_opset.o: ../h/types.h ../h/signal.h ../h/../machine/trap.h
kdb_opset.o: ../h/../machine/machparam.h machine/endian.h ../h/dir.h
kdb_opset.o: ../tahoe/../kdb/../machine/psl.h ../tahoe/../kdb/../machine/pte.h
kdb_opset.o: ../h/user.h ../h/../machine/pcb.h ../h/dmap.h ../h/time.h
kdb_opset.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h
kdb_opset.o: /usr/include/a.out.h sys/exec.h /usr/include/ctype.h
kdb_opset.o: ../tahoe/../kdb/redef.h ../tahoe/../kdb/../machine/kdbparam.h
kdb_opset.o: ../tahoe/../tahoe/kdb_instrs
in_cksum.o: ../tahoe/in_cksum.c ../h/types.h ../h/mbuf.h
machdep.o: ../tahoe/machdep.c ../h/param.h ../h/types.h ../h/signal.h
machdep.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
machdep.o: ../h/systm.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h
machdep.o: ../h/dmap.h ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h
machdep.o: ../h/errno.h ../h/kernel.h ../h/map.h ../h/vm.h ../h/vmparam.h
machdep.o: ../h/../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
machdep.o: ../h/proc.h ../h/buf.h ../h/reboot.h ../h/conf.h ../h/inode.h
machdep.o: ../h/file.h ../h/text.h ../h/clist.h ../h/callout.h ../h/cmap.h
machdep.o: ../h/malloc.h ../h/mbuf.h ../h/msgbuf.h ../h/quota.h ../tahoe/cpu.h
machdep.o: ../tahoe/reg.h ../tahoe/pte.h ../tahoe/psl.h ../tahoe/mem.h
machdep.o: ../tahoe/mtpr.h ../tahoe/cp.h ../tahoe/../tahoevba/vbavar.h yc.h
mem.o: ../tahoe/mem.c ../h/param.h ../h/types.h ../h/signal.h
mem.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
mem.o: ../h/dir.h ../h/user.h ../h/../machine/pcb.h ../h/dmap.h ../h/time.h
mem.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/conf.h
mem.o: ../h/buf.h ../h/systm.h ../h/vm.h ../h/vmparam.h
mem.o: ../h/../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h
mem.o: ../h/cmap.h ../h/uio.h ../tahoe/pte.h ../tahoe/mtpr.h
ns_cksum.o: ../tahoe/ns_cksum.c ../h/types.h ../h/mbuf.h
sys_machdep.o: ../tahoe/sys_machdep.c ../h/param.h ../h/types.h ../h/signal.h
sys_machdep.o: ../h/../machine/trap.h ../h/../machine/machparam.h
sys_machdep.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
sys_machdep.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
sys_machdep.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/ioctl.h
sys_machdep.o: ../h/ttychars.h ../h/ttydev.h ../h/file.h ../h/proc.h ../h/uio.h
sys_machdep.o: ../h/kernel.h ../h/mtio.h ../h/buf.h ../tahoe/pte.h
trap.o: ../tahoe/trap.c ../h/param.h ../h/types.h ../h/signal.h
trap.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
trap.o: ../h/systm.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h ../h/dmap.h
trap.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
trap.o: ../h/proc.h ../h/seg.h ../h/acct.h ../h/kernel.h ../tahoe/psl.h
trap.o: ../tahoe/reg.h ../tahoe/pte.h ../tahoe/mtpr.h
trap.o: ../tahoe/../sys/syscalls.c ../tahoe/../tahoe/trap.h
ufs_machdep.o: ../tahoe/ufs_machdep.c ../h/param.h ../h/types.h ../h/signal.h
ufs_machdep.o: ../h/../machine/trap.h ../h/../machine/machparam.h
ufs_machdep.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
ufs_machdep.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
ufs_machdep.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/buf.h ../h/conf.h
ufs_machdep.o: ../h/proc.h ../h/seg.h ../h/vm.h ../h/vmparam.h
ufs_machdep.o: ../h/../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
ufs_machdep.o: ../h/vmsystm.h ../tahoe/pte.h
vm_machdep.o: ../tahoe/vm_machdep.c ../h/param.h ../h/types.h ../h/signal.h
vm_machdep.o: ../h/../machine/trap.h ../h/../machine/machparam.h
vm_machdep.o: machine/endian.h ../h/systm.h ../h/dir.h ../h/user.h
vm_machdep.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
vm_machdep.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h ../h/cmap.h
vm_machdep.o: ../h/mount.h ../h/vm.h ../h/vmparam.h ../h/../machine/vmparam.h
vm_machdep.o: ../h/vmmac.h ../h/vmmeter.h ../h/vmsystm.h ../h/text.h
vm_machdep.o: ../h/kernel.h ../tahoe/pte.h ../tahoe/cpu.h ../tahoe/mtpr.h
if_ex.o: ../tahoeif/if_ex.c ex.h ../h/param.h ../h/types.h ../h/signal.h
if_ex.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
if_ex.o: ../h/systm.h ../h/mbuf.h ../h/malloc.h ../h/buf.h ../h/protosw.h
if_ex.o: ../h/socket.h ../h/vmmac.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h
if_ex.o: ../h/errno.h ../h/vmparam.h ../h/../machine/vmparam.h ../h/syslog.h
if_ex.o: ../h/uio.h ../tahoeif/../net/if.h ../tahoeif/../net/../net/if_arp.h
if_ex.o: ../tahoeif/../net/netisr.h ../tahoeif/../net/route.h
if_ex.o: ../tahoeif/../netinet/in.h ../tahoeif/../netinet/in_systm.h
if_ex.o: ../tahoeif/../netinet/in_var.h ../tahoeif/../netinet/ip.h
if_ex.o: ../tahoeif/../netinet/if_ether.h ../tahoeif/../netns/ns.h
if_ex.o: ../tahoeif/../netns/ns_if.h ../tahoeif/../tahoe/cpu.h
if_ex.o: ../tahoeif/../tahoe/pte.h ../tahoeif/../tahoe/mtpr.h
if_ex.o: ../tahoeif/../tahoevba/vbavar.h ../tahoeif/if_exreg.h
fpe.o: ../tahoemath/fpe.c ../tahoemath/../tahoe/psl.h
fpe.o: ../tahoemath/../tahoe/reg.h ../tahoemath/../tahoe/pte.h
fpe.o: ../tahoemath/../tahoe/mtpr.h ../tahoemath/../tahoemath/Kfp.h
fpe.o: ../h/param.h ../h/types.h ../h/signal.h ../h/../machine/trap.h
fpe.o: ../h/../machine/machparam.h machine/endian.h ../h/systm.h ../h/dir.h
fpe.o: ../h/user.h ../h/../machine/pcb.h ../h/dmap.h ../h/time.h
fpe.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/proc.h
fpe.o: ../h/seg.h ../h/acct.h ../h/kernel.h
vba.o: ../tahoevba/vba.c ../h/param.h ../h/types.h ../h/signal.h
vba.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
vba.o: ../h/buf.h ../h/cmap.h ../h/conf.h dk.h ../h/map.h ../h/systm.h
vba.o: ../h/dir.h ../h/user.h ../h/../machine/pcb.h ../h/dmap.h ../h/time.h
vba.o: ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h ../h/vmparam.h
vba.o: ../h/../machine/vmparam.h ../h/vmmac.h ../h/proc.h ../h/syslog.h
vba.o: ../h/malloc.h ../tahoevba/../tahoe/mtpr.h ../tahoevba/../tahoe/pte.h
vba.o: ../tahoevba/../tahoevba/vbavar.h dk.h ../tahoevba/vdreg.h hd.h
vba.o: ../tahoevba/hdreg.h
vd.o: ../tahoevba/vd.c dk.h ../h/param.h ../h/types.h ../h/signal.h
vd.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
vd.o: ../h/buf.h ../h/cmap.h ../h/conf.h ../h/dir.h ../h/dkstat.h
vd.o: ../h/disklabel.h ../h/map.h ../h/file.h ../h/systm.h ../h/user.h
vd.o: ../h/../machine/pcb.h ../h/dmap.h ../h/time.h ../h/resource.h
vd.o: ../h/namei.h ../h/uio.h ../h/errno.h ../h/vmmac.h ../h/proc.h ../h/uio.h
vd.o: ../h/syslog.h ../h/kernel.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h
vd.o: ../h/stat.h ../tahoevba/../tahoe/cpu.h ../tahoevba/../tahoe/mtpr.h
vd.o: ../tahoevba/../tahoe/pte.h ../tahoevba/../tahoevba/vbavar.h
vd.o: ../tahoevba/../tahoevba/vdreg.h
vx.o: ../tahoevba/vx.c vx.h ../h/param.h ../h/types.h ../h/signal.h
vx.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
vx.o: ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h ../h/tty.h ../h/ttychars.h
vx.o: ../h/ttydev.h ../h/dir.h ../h/user.h ../h/../machine/pcb.h ../h/dmap.h
vx.o: ../h/time.h ../h/resource.h ../h/namei.h ../h/uio.h ../h/errno.h
vx.o: ../h/map.h ../h/buf.h ../h/conf.h ../h/file.h ../h/uio.h ../h/proc.h
vx.o: ../h/vm.h ../h/vmparam.h ../h/../machine/vmparam.h ../h/vmmac.h
vx.o: ../h/vmmeter.h ../h/vmsystm.h ../h/kernel.h ../h/syslog.h
vx.o: ../tahoevba/../tahoe/pte.h ../tahoevba/../tahoevba/vbavar.h
vx.o: ../tahoevba/../tahoevba/vbaparam.h ../tahoevba/../tahoevba/vxreg.h
vx.o: ../tahoevba/../tahoevba/scope.h
hd.o: ../tahoevba/hd.c hd.h ../h/param.h ../h/types.h ../h/signal.h
hd.o: ../h/../machine/trap.h ../h/../machine/machparam.h machine/endian.h
hd.o: ../h/buf.h ../h/conf.h ../h/dir.h ../h/dkstat.h ../h/disklabel.h
hd.o: ../h/file.h ../h/systm.h ../h/vmmac.h ../h/time.h ../h/proc.h ../h/uio.h
hd.o: ../h/syslog.h ../h/kernel.h ../h/ioctl.h ../h/ttychars.h ../h/ttydev.h
hd.o: ../h/stat.h ../h/errno.h ../tahoevba/../tahoe/cpu.h
hd.o: ../tahoevba/../tahoe/mtpr.h ../tahoevba/../tahoevba/vbavar.h
hd.o: ../tahoevba/../tahoevba/hdreg.h
swapgeneric.o: ../tahoe/swapgeneric.c ../h/param.h ../h/types.h ../h/signal.h
swapgeneric.o: ../h/../machine/trap.h ../h/../machine/machparam.h
swapgeneric.o: machine/endian.h ../h/conf.h ../h/buf.h ../h/vm.h ../h/vmparam.h
swapgeneric.o: ../h/../machine/vmparam.h ../h/vmmac.h ../h/vmmeter.h
swapgeneric.o: ../h/vmsystm.h ../h/systm.h ../h/reboot.h ../tahoe/pte.h
swapgeneric.o: ../tahoe/cpu.h ../tahoe/cp.h ../tahoe/mtpr.h
swapgeneric.o: ../tahoe/../tahoevba/vbavar.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
