/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)locore.s	6.26 (Berkeley) %G%
 */

#include "psl.h"
#include "pte.h"

#include "errno.h"
#include "cmap.h"

#include "mtpr.h"
#include "trap.h"
#include "cpu.h"
#include "nexus.h"
#include "cons.h"
#include "clock.h"
#include "../vaxuba/ubareg.h"

#include "dz.h"
#include "uu.h"
#include "ps.h"
#include "mba.h"
#include "uba.h"

	.set	HIGH,0x1f	# mask for total disable
	.set	MCKVEC,4	# offset into scb of machine check vector
	.set	NBPG,512
	.set	PGSHIFT,9

	.set	NISP,3		# number of interrupt stack pages

/*
 * User structure is UPAGES at top of user space.
 */
	.globl	_u
	.set	_u,0x80000000 - UPAGES*NBPG

	.globl	_intstack
_intstack:
	.space	NISP*NBPG
eintstack:

/*
 * Do a dump.
 * Called by auto-restart.
 * May be called manually.
 */
	.align	2
	.globl	_doadump
_doadump:
	nop; nop				# .word 0x0101
#define	_rpbmap	_Sysmap				# rpb, scb, UNI*vec, istack*4
	bicl2	$PG_PROT,_rpbmap
	bisl2	$PG_KW,_rpbmap
	mtpr	$0,$TBIA
	tstl	_rpb+RP_FLAG			# dump only once!
	bneq	1f
	incl	_rpb+RP_FLAG
	movl	sp,erpb
	movab	erpb,sp
	mfpr	$PCBB,-(sp)
	mfpr	$MAPEN,-(sp)
	mfpr	$IPL,-(sp)
	mtpr	$0,$MAPEN
	mtpr	$HIGH,$IPL
	pushr	$0x3fff
	calls	$0,_dumpsys
1:
	mfpr	$TXCS,r0
	bitl	$TXCS_RDY,r0
	beql	1b
	mtpr	$TXDB_BOOT,$TXDB
	halt

/*
 * Interrupt vector routines
 */ 
	.globl	_waittime

#define	SCBVEC(name)	.align 2; .globl _X/**/name; _X/**/name
#define	PANIC(msg)	clrl _waittime; pushab 1f; \
			calls $1,_panic; 1: .asciz msg
#define	PRINTF(n,msg)	pushab 1f; calls $n+1,_printf; MSG(msg)
#define	MSG(msg)	.data; 1: .asciz msg; .text
#define	PUSHR		pushr $0x3f
#define	POPR		popr $0x3f

	.data
nofault: .long	0	# where to go on predicted machcheck
	.text
SCBVEC(machcheck):
	tstl	nofault
	bneq	1f
	PUSHR; pushab 6*4(sp); calls $1,_machinecheck; POPR;
	addl2 (sp)+,sp; rei
	.align	2
1:
	casel	_cpu,$1,$VAX_MAX
0:
	.word	8f-0b		# 1 is 780
	.word	5f-0b		# 2 is 750
	.word	5f-0b		# 3 is 730
5:
#if defined(VAX750) || defined(VAX730)
	mtpr	$0xf,$MCESR
#endif
	brb	1f
8:
#if VAX780
	mtpr	$0,$SBIFS
#endif
1:
	addl2	(sp)+,sp		# discard mchchk trash
	movl	nofault,(sp)
	rei
SCBVEC(kspnotval):
	PUSHR; PANIC("KSP not valid");
SCBVEC(powfail):
	halt
SCBVEC(chme): SCBVEC(chms): SCBVEC(chmu):
	PUSHR; PANIC("CHM? in kernel");
SCBVEC(stray):
	PUSHR; PRINTF(0, "stray scb interrupt\n"); POPR;
	rei
SCBVEC(nexzvec):
	PUSHR; mfpr $IPL,-(sp); PRINTF(1, "nexus stray intr ipl%x\n"); POPR; rei
SCBVEC(cmrd):
	PUSHR; calls $0,_memerr; POPR; rei
SCBVEC(wtime):
	PUSHR; pushl 6*4(sp); PRINTF(1,"write timeout %x\n"); POPR;
	PANIC("wtimo");

#if NMBA > 0
SCBVEC(mba3int):
	PUSHR; incl _intrcnt+I_MBA3; pushl $3; brb 1f
SCBVEC(mba2int):
	PUSHR; incl _intrcnt+I_MBA2; pushl $2; brb 1f
SCBVEC(mba1int):
	PUSHR; incl _intrcnt+I_MBA1; pushl $1; brb 1f
SCBVEC(mba0int):
	PUSHR; incl _intrcnt+I_MBA0; pushl $0
1:	calls $1,_mbintr
	POPR
	incl	_cnt+V_INTR
	rei
#endif

#if VAX780
/*
 * Registers for the uba handling code
 */
#define	rUBANUM	r0
#define	rUBAHD	r1
#define	rUVEC	r3
#define	rUBA	r4
/* r2,r5 are scratch */

SCBVEC(ua3int):
	PUSHR; movl $3,rUBANUM; moval _uba_hd+(3*UH_SIZE),rUBAHD; brb 1f
SCBVEC(ua2int):
	PUSHR; movl $2,rUBANUM; moval _uba_hd+(2*UH_SIZE),rUBAHD; brb 1f
SCBVEC(ua1int):
	PUSHR; movl $1,rUBANUM; moval _uba_hd+(1*UH_SIZE),rUBAHD; brb 1f
SCBVEC(ua0int):
	PUSHR; movl $0,rUBANUM; moval _uba_hd+(0*UH_SIZE),rUBAHD;
1:
	mfpr	$IPL,r2				/* r2 = mfpr(IPL); */
	movl	UH_UBA(rUBAHD),rUBA		/* uba = uhp->uh_uba; */
	movl	UBA_BRRVR-0x14*4(rUBA)[r2],rUVEC
					/* uvec = uba->uba_brrvr[r2-0x14] */
ubanorm:
	bleq	ubaerror 
	addl2	UH_VEC(rUBAHD),rUVEC		/* uvec += uh->uh_vec */
	bicl3	$3,(rUVEC),r1 
	jmp	2(r1)				/* 2 skips ``pushr $0x3f'' */
ubaerror:
	PUSHR; calls $0,_ubaerror; POPR		/* ubaerror r/w's r0-r5 */
	tstl rUVEC; jneq ubanorm		/* rUVEC contains result */
	incl _intrcnt+I_UBA[rUBANUM]
	incl	_cnt+V_INTR
	POPR
	rei
#endif
SCBVEC(cnrint):
	PUSHR; calls $0,_cnrint; POPR
	incl _cnt+V_INTR
	incl _intrcnt+I_CNR
	rei
SCBVEC(cnxint):
	PUSHR; calls $0,_cnxint; POPR
	incl _cnt+V_INTR
	incl _intrcnt+I_CNX
	rei
SCBVEC(hardclock):
	PUSHR
	mtpr $ICCS_RUN|ICCS_IE|ICCS_INT|ICCS_ERR,$ICCS
#if NPS > 0
	pushl	4+6*4(sp); pushl 4+6*4(sp);
	calls	$2,_psextsync
#endif
	pushl 4+6*4(sp); pushl 4+6*4(sp);
	calls $2,_hardclock			# hardclock(pc,psl)
	POPR;
	incl	_cnt+V_INTR
	incl	_intrcnt+I_CLOCK
	rei
SCBVEC(softclock):
	PUSHR
	pushl	4+6*4(sp); pushl 4+6*4(sp);
	calls	$2,_softclock			# softclock(pc,psl)
	POPR; 
	incl	_cnt+V_SOFT
	rei
#include "../net/netisr.h"
	.globl	_netisr
SCBVEC(netintr):
	PUSHR
	bbcc	$NETISR_RAW,_netisr,1f; calls $0,_rawintr; 1:
#ifdef INET
#include "../netinet/in_systm.h"
	bbcc	$NETISR_IP,_netisr,1f; calls $0,_ipintr; 1:
#endif
#ifdef NS
	bbcc	$NETISR_NS,_netisr,1f; calls $0,_nsintr; 1:
#endif
	POPR
	incl	_cnt+V_SOFT
	rei
#if defined(VAX750) || defined(VAX730)
SCBVEC(consdin):
	PUSHR;
	incl _intrcnt+I_TUR
#if defined(VAX750) && !defined(VAX730) && !defined(MRSP)
	jsb	tudma
#endif
	calls $0,_turintr;
	POPR;
	incl _cnt+V_INTR;
	rei
SCBVEC(consdout):
	PUSHR; calls $0,_tuxintr; POPR
	incl _cnt+V_INTR
	incl _intrcnt+I_TUX
	rei
#else
SCBVEC(consdin):
	halt
SCBVEC(consdout):
	halt
#endif

#if NDZ > 0
/*
 * DZ pseudo dma routine:
 *	r0 - controller number
 */
	.align	1
	.globl	dzdma
dzdma:
	mull2	$8*20,r0
	movab	_dzpdma(r0),r3		# pdma structure base
					# for this controller
dzploop:
	movl	r3,r0	
	movl	(r0)+,r1		# device register address
	movzbl	1(r1),r2		# get line number
	bitb	$0x80,r2		# TRDY on?
	beql	dzprei			# no	
	bicb2	$0xf8,r2		# clear garbage bits
	mull2	$20,r2
	addl2	r2,r0			# point at line's pdma structure
	movl	(r0)+,r2		# p_mem
	cmpl	r2,(r0)+		# p_mem < p_end ?
	bgequ	dzpcall			# no, go call dzxint
	movb	(r2)+,6(r1)		# dztbuf = *p_mem++
	movl	r2,-8(r0)
	brb 	dzploop			# check for another line
dzprei:
	POPR
	incl	_cnt+V_PDMA
	rei

dzpcall:
	pushl	r3
	pushl	(r0)+			# push tty address
	calls	$1,*(r0)		# call interrupt rtn
	movl	(sp)+,r3
	brb 	dzploop			# check for another line
#endif

#if NUU > 0 && defined(UUDMA)
/*
 * Pseudo DMA routine for tu58 (on DL11)
 *	r0 - controller number
 */
	.align	1
	.globl	uudma
uudma:
	movl	_uudinfo[r0],r2
	movl	16(r2),r2		# r2 = uuaddr
	mull3	$48,r0,r3
	movab	_uu_softc(r3),r5	# r5 = uuc

	cvtwl	2(r2),r1		# c = uuaddr->rdb
	bbc	$15,r1,1f		# if (c & UUDB_ERROR)
	movl	$13,16(r5)		#	uuc->tu_state = TUC_RCVERR;
	rsb				#	let uurintr handle it
1:
	tstl	4(r5)			# if (uuc->tu_rcnt) {
	beql	1f
	movb	r1,*0(r5)		#	*uuc->tu_rbptr++ = r1
	incl	(r5)
	decl	4(r5)			#	if (--uuc->tu_rcnt)
	beql	2f			#		done
	tstl	(sp)+
	POPR				# 	registers saved in ubglue.s
	rei				# }
2:
	cmpl	16(r5),$8		# if (uuc->tu_state != TUS_GETH)
	beql	2f			# 	let uurintr handle it
1:
	rsb
2:
	mull2	$14,r0			# sizeof(uudata[ctlr]) = 14
	movab	_uudata(r0),r4		# data = &uudata[ctlr];
	cmpb	$1,(r4)			# if (data->pk_flag != TUF_DATA)
	bneq	1b
#ifdef notdef
	/* this is for command packets */
	beql	1f			# 	r0 = uuc->tu_rbptr
	movl	(r5),r0
	brb	2f
1:					# else
#endif
	movl	24(r5),r0		# 	r0 = uuc->tu_addr
2:
	movzbl	1(r4),r3		# counter to r3 (data->pk_count)
	movzwl	(r4),r1			# first word of checksum (=header)
	mfpr	$IPL,-(sp)		# s = spl5();
	mtpr	$0x15,$IPL		# to keep disk interrupts out
	clrw	(r2)			# disable receiver interrupts
3:	bbc	$7,(r2),3b		# while ((uuaddr->rcs & UUCS_READY)==0);
	cvtwb	2(r2),(r0)+		# *buffer = uuaddr->rdb & 0xff
	sobgtr	r3,1f			# continue with next byte ...
	addw2	2(r2),r1		# unless this was the last (odd count)
	brb	2f

1:	bbc	$7,(r2),1b		# while ((uuaddr->rcs & UUCS_READY)==0);
	cvtwb	2(r2),(r0)+		# *buffer = uuaddr->rdb & 0xff
	addw2	-2(r0),r1		# add to checksum..
2:
	adwc	$0,r1			# get the carry
	sobgtr	r3,3b			# loop while r3 > 0
/*
 * We're ready to get the checksum
 */
1:	bbc	$7,(r2),1b		# while ((uuaddr->rcs & UUCS_READY)==0);
	cvtwb	2(r2),12(r4)		# get first (lower) byte
1:	bbc	$7,(r2),1b
	cvtwb	2(r2),13(r4)		# ..and second
	cmpw	12(r4),r1		# is checksum ok?
	beql	1f
	movl	$14,16(r5)		# uuc->tu_state = TUS_CHKERR
	brb	2f			# exit
1:
	movl	$11,16(r5)		# uuc->tu_state = TUS_GET (ok)
2:
	movw	$0x40,(r2)		# enable receiver interrupts
	mtpr	(sp)+,$IPL		# splx(s);
	rsb				# continue processing in uurintr
#endif

#if defined(VAX750) && !defined(VAX730) && !defined(MRSP)
/*
 * Pseudo DMA routine for VAX-11/750 console tu58 
 *   	    (without MRSP)
 */
	.align	1
	.globl	tudma
tudma:
	movab	_tu,r5			# r5 = tu
	tstl	4(r5)			# if (tu.tu_rcnt) {
	beql	3f
	mfpr	$CSRD,r1		# get data from tu58
	movb	r1,*0(r5)		#	*tu.tu_rbptr++ = r1
	incl	(r5)
	decl	4(r5)			#	if (--tu.tu_rcnt)
	beql	1f			#		done
	tstl	(sp)+
	POPR				# 	registers saved in ubglue.s
	rei				# 	data handled, done
1:					# }
	cmpl	16(r5),$8		# if (tu.tu_state != TUS_GETH)
	beql	2f			# 	let turintr handle it
3:
	rsb
2:
	movab	_tudata,r4		# r4 = tudata
	cmpb	$1,(r4)			# if (tudata.pk_flag != TUF_DATA)
	bneq	3b			# 	let turintr handle it
1:					# else
	movl	24(r5),r1		# get buffer pointer to r1
	movzbl	1(r4),r3		# counter to r3
	movzwl	(r4),r0			# first word of checksum (=header)
	mtpr	$0,$CSRS		# disable receiver interrupts
3:
	bsbw	5f			# wait for next byte
	mfpr	$CSRD,r5
	movb	r5,(r1)+		# *buffer = rdb
	sobgtr	r3,1f			# continue with next byte ...
	mfpr	$CSRD,r2		# unless this was the last (odd count)
	brb	2f

1:	bsbw	5f			# wait for next byte
	mfpr	$CSRD,r5
	movb	r5,(r1)+		# *buffer = rdb
	movzwl	-2(r1),r2		# get the last word back from memory
2:
	addw2	r2,r0			# add to checksum..
	adwc	$0,r0			# get the carry
	sobgtr	r3,3b			# loop while r3 > 0
/*
 * We're ready to get the checksum.
 */
	bsbw	5f
	movab	_tudata,r4
	mfpr	$CSRD,r5
	movb	r5,12(r4)		# get first (lower) byte
	bsbw	5f
	mfpr	$CSRD,r5
	movb	r5,13(r4)		# ..and second
	movab	_tu,r5
	cmpw	12(r4),r0		# is checksum ok?
	beql	1f
	movl	$14,16(r5)		# tu.tu_state = TUS_CHKERR
	brb	2f			# exit
1:
	movl	$11,16(r5)		# tu.tu_state = TUS_GET
2:
	mtpr	$0x40,$CSRS		# enable receiver interrupts
	rsb				# continue processing in turintr
/*
 * Loop until a new byte is ready from
 * the tu58, make sure we don't loop forever
 */
5:
	movl	$5000,r5		# loop max 5000 times
1:
	mfpr	$CSRS,r2
	bbs	$7,r2,1f
	sobgtr	r5,1b
	movab	_tu,r5
	movl	$13,16(r5)		# return TUS_RCVERR
	tstl	(sp)+			# and let turintr handle it
1:
	rsb
#endif

/*
 * Stray UNIBUS interrupt catch routines
 */
	.data
	.align	2
#define	PJ	PUSHR;jsb _Xustray
	.globl	_catcher
_catcher:
	PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ
	PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ
	PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ
	PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ
	PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ
	PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ
	PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ
	PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ

	.globl	_cold
_cold:	.long	1
	.data

	.text
SCBVEC(ustray):
	blbc	_cold,1f
	mfpr	$IPL,r11
	subl3	$_catcher+8,(sp)+,r10
	ashl	$-1,r10,r10
	POPR
	rei
1:
	subl3	$_catcher+8,(sp)+,r0
	ashl	$-1,r0,-(sp)
	mfpr	$IPL,-(sp)
	PRINTF(2, "uba?: stray intr ipl %x vec %o\n")
	POPR
	rei

/*
 * Trap and fault vector routines
 */ 
#define	TRAP(a)	pushl $T_/**/a; jbr alltraps

/*
 * Ast delivery (profiling and/or reschedule)
 */
SCBVEC(astflt):
	pushl $0; TRAP(ASTFLT)
SCBVEC(privinflt):
	pushl $0; TRAP(PRIVINFLT)
SCBVEC(xfcflt):
	pushl $0; TRAP(XFCFLT)
SCBVEC(resopflt):
	pushl $0; TRAP(RESOPFLT)
SCBVEC(resadflt):
	pushl $0; TRAP(RESADFLT)
SCBVEC(bptflt):
	pushl $0; TRAP(BPTFLT)
SCBVEC(compatflt):
	TRAP(COMPATFLT);
SCBVEC(tracep):
	pushl $0; TRAP(TRCTRAP)
SCBVEC(arithtrap):
	TRAP(ARITHTRAP)
SCBVEC(protflt):
	blbs	(sp)+,segflt
	TRAP(PROTFLT)
segflt:
	TRAP(SEGFLT)
SCBVEC(transflt):
	bitl	$2,(sp)+
	bnequ	tableflt
 	jsb	Fastreclaim		# try and avoid pagein
	TRAP(PAGEFLT)
tableflt: 
	TRAP(TABLEFLT)

alltraps:
	mfpr	$USP,-(sp); calls $0,_trap; mtpr (sp)+,$USP
	incl	_cnt+V_TRAP
	addl2	$8,sp			# pop type, code
	mtpr	$HIGH,$IPL		## dont go to a higher IPL (GROT)
	rei

SCBVEC(syscall):
	pushl	$T_SYSCALL
	mfpr	$USP,-(sp); calls $0,_syscall; mtpr (sp)+,$USP
	incl	_cnt+V_SYSCALL
	addl2	$8,sp			# pop type, code
	mtpr	$HIGH,$IPL		## dont go to a higher IPL (GROT)
	rei

/*
 * System page table
 */ 
#define	vaddr(x)	((((x)-_Sysmap)/4)*NBPG+0x80000000)
#define	SYSMAP(mname, vname, npte)			\
_/**/mname:	.globl	_/**/mname;		\
	.space	npte*4;				\
	.globl	_/**/vname;			\
	.set	_/**/vname,vaddr(_/**/mname)

	.data
	.align	2
	SYSMAP(Sysmap	,Sysbase	,SYSPTSIZE	)
	SYSMAP(UMBAbeg	,umbabeg	,0		)
	SYSMAP(Nexmap	,nexus		,16*MAXNNEXUS	)
	SYSMAP(UMEMmap	,umem		,512*NUBA	)
	SYSMAP(UMBAend	,umbaend	,0		)
	SYSMAP(Usrptmap	,usrpt		,USRPTSIZE	)
	SYSMAP(Forkmap	,forkutl	,UPAGES		)
	SYSMAP(Xswapmap	,xswaputl	,UPAGES		)
	SYSMAP(Xswap2map,xswap2utl	,UPAGES		)
	SYSMAP(Swapmap	,swaputl	,UPAGES		)
	SYSMAP(Pushmap	,pushutl	,UPAGES		)
	SYSMAP(Vfmap	,vfutl		,UPAGES		)
	SYSMAP(CMAP1	,CADDR1		,1		)
	SYSMAP(CMAP2	,CADDR2		,1		)
	SYSMAP(mcrmap	,mcr		,1		)
	SYSMAP(mmap	,vmmap		,1		)
	SYSMAP(msgbufmap,msgbuf		,MSGBUFPTECNT	)
	SYSMAP(camap	,cabase		,16*CLSIZE	)
#ifdef	GPROF
	SYSMAP(profmap	,profbase	,600*CLSIZE	)
#endif
	SYSMAP(ecamap	,calimit	,0		)
	SYSMAP(Mbmap	,mbutl		,NMBCLUSTERS*CLSIZE)

eSysmap:
	.globl	_Syssize
	.set	_Syssize,(eSysmap-_Sysmap)/4
	.text

/*
 * Initialization
 *
 * ipl 0x1f; mapen 0; scbb, pcbb, sbr, slr, isp, ksp not set
 */
	.data
	.globl	_cpu
_cpu:	.long	0
	.text
	.globl	start
start:
	.word	0
/* set system control block base and system page table params */
	mtpr	$_scb-0x80000000,$SCBB
	mtpr	$_Sysmap-0x80000000,$SBR
	mtpr	$_Syssize,$SLR
/* double map the kernel into the virtual user addresses of phys mem */
	mtpr	$_Sysmap,$P0BR
	mtpr	$_Syssize,$P0LR
/* set ISP and get cpu type */
	movl	$_intstack+NISP*NBPG,sp
	mfpr	$SID,r0
	movab	_cpu,r1
	extzv	$24,$8,r0,(r1)
/* init RPB */
	movab	_rpb,r0
	movl	r0,(r0)+			# rp_selfref
	movab	_doadump,r1
	movl	r1,(r0)+			# rp_dumprout
	movl	$0x1f,r2
	clrl	r3
1:	addl2	(r1)+,r3; sobgtr r2,1b
	movl	r3,(r0)+			# rp_chksum
/* count up memory */
	clrl	r7
1:	pushl	$4; pushl r7; calls $2,_badaddr; tstl r0; bneq 9f
	acbl	$MAXMEM*1024-1,$64*1024,r7,1b
9:
/* clear memory from kernel bss and pages for proc 0 u. and page table */
	movab	_edata,r6
	movab	_end,r5
	bbcc	$31,r5,0f; 0:
	addl2	$(UPAGES*NBPG)+NBPG+NBPG,r5
1:	clrq	(r6); acbl r5,$8,r6,1b
/* trap() and syscall() save r0-r11 in the entry mask (per ../h/reg.h) */
	bisw2	$0x0fff,_trap
	bisw2	$0x0fff,_syscall
	calls	$0,_fixctlrmask
/* initialize system page table: uba vectors and int stack writeable */
	clrl	r2
	movab	eintstack,r1; bbcc $31,r1,0f; 0: ashl $-PGSHIFT,r1,r1
1:	bisl3	$PG_V|PG_KW,r2,_Sysmap[r2]; aoblss r1,r2,1b
/* make rpb, scb read-only as red zone for interrupt stack */
	bicl2	$PG_PROT,_rpbmap
	bisl2	$PG_KR,_rpbmap
/* make kernel text space read-only */
	movab	_etext+NBPG-1,r1; bbcc $31,r1,0f; 0: ashl $-PGSHIFT,r1,r1
1:	bisl3	$PG_V|PG_KR,r2,_Sysmap[r2]; aoblss r1,r2,1b
/* make kernel data, bss, read-write */
	movab	_end+NBPG-1,r1; bbcc $31,r1,0f; 0:; ashl $-PGSHIFT,r1,r1
1:	bisl3	$PG_V|PG_KW,r2,_Sysmap[r2]; aoblss r1,r2,1b
/* now go to mapped mode */
	mtpr	$1,$TBIA; mtpr $1,$MAPEN; jmp *$0f; 0:
/* init mem sizes */
	ashl	$-PGSHIFT,r7,_maxmem
	movl	_maxmem,_physmem
	movl	_maxmem,_freemem
/* setup context for proc[0] == Scheduler */
	movab	_end+NBPG-1,r6
	bicl2	$NBPG-1,r6		# make page boundary
/* setup page table for proc[0] */
	bbcc	$31,r6,0f; 0:
	ashl	$-PGSHIFT,r6,r3			# r3 = btoc(r6)
	bisl3	$PG_V|PG_KW,r3,_Usrptmap	# init first upt entry
	incl	r3
	movab	_usrpt,r0
	mtpr	r0,$TBIS
/* init p0br, p0lr */
	mtpr	r0,$P0BR
	mtpr	$0,$P0LR
/* init p1br, p1lr */
	movab	NBPG(r0),r0
	movl	$0x200000-UPAGES,r1
	mtpr	r1,$P1LR
	mnegl	r1,r1
	moval	-4*UPAGES(r0)[r1],r2
	mtpr	r2,$P1BR
/* setup mapping for UPAGES of _u */
	movl	$UPAGES,r2; movab _u+NBPG*UPAGES,r1; addl2 $UPAGES,r3; jbr 2f
1:	decl	r3
	moval	-NBPG(r1),r1;
	bisl3	$PG_V|PG_URKW,r3,-(r0)
	mtpr	r1,$TBIS
2:	sobgeq	r2,1b
/* initialize (slightly) the pcb */
	movab	UPAGES*NBPG(r1),PCB_KSP(r1)
	mnegl	$1,PCB_ESP(r1)
	mnegl	$1,PCB_SSP(r1)
	movl	r1,PCB_USP(r1)
	mfpr	$P0BR,PCB_P0BR(r1)
	mfpr	$P0LR,PCB_P0LR(r1)
	movb	$4,PCB_P0LR+3(r1)		# disable ast
	mfpr	$P1BR,PCB_P1BR(r1)
	mfpr	$P1LR,PCB_P1LR(r1)
	movl	$CLSIZE,PCB_SZPT(r1)		# init u.u_pcb.pcb_szpt
	movl	r11,PCB_R11(r1)
	movab	1f,PCB_PC(r1)			# initial pc
	clrl	PCB_PSL(r1)			# mode(k,k), ipl=0
	ashl	$PGSHIFT,r3,r3
	mtpr	r3,$PCBB			# first pcbb
/* set regs, p0br, p0lr, p1br, p1lr, astlvl, ksp and change to kernel mode */
	ldpctx
	rei
/* put signal trampoline code in u. area */
1:	movab	_u,r0
	movc3	$19,sigcode,PCB_SIGC(r0)
/* save reboot flags in global _boothowto */
	movl	r11,_boothowto
/* calculate firstaddr, and call main() */
	movab	_end+NBPG-1,r0; bbcc $31,r0,0f; 0:; ashl $-PGSHIFT,r0,-(sp)
	addl2	$UPAGES+1,(sp); calls $1,_main
/* proc[1] == /etc/init now running here; run icode */
	pushl	$PSL_CURMOD|PSL_PRVMOD; pushl $0; rei

/* signal trampoline code: it is known that this code takes exactly 19 bytes */
/* in ../vax/pcb.h and in the movc3 above */
sigcode:
	calls	$4,8(pc)	# params pushed by sendsig
	movl	sp,ap		# calls frame built by sendsig
	chmk	$103		# cleanup mask and onsigstack
	halt			# sigreturn() does not return!
	.word	0x3f		# registers 0-5
	callg	(ap),*16(ap)	# call the signal handler
	ret			# return to code above

	.set	exec,11
	.set	exit,1
	.globl	_icode
	.globl	_initflags
	.globl	_szicode
/*
 * Icode is copied out to process 1 to exec /etc/init.
 * If the exec fails, process 1 exits.
 */
_icode:
	pushab	b`argv-l0(pc)
l0:	pushab	b`init-l1(pc)
l1:	pushl	$2
	movl	sp,ap
	chmk	$exec
	chmk	$exit

init:	.asciz	"/etc/init"
	.align	2
_initflags:
	.long	0
argv:	.long	init+5-_icode
	.long	_initflags-_icode
	.long	0
_szicode:
	.long	_szicode-_icode

/*
 * Primitives
 */ 

#ifdef GPROF
#define	ENTRY(name, regs) \
	.globl _/**/name; .align 1; _/**/name: .word regs; jsb mcount
#define	JSBENTRY(name, regs) \
	.globl _/**/name; _/**/name: \
	movl fp,-(sp); movab -12(sp),fp; pushr $(regs); jsb mcount; \
	popr $(regs); movl (sp)+,fp
#else
#define	ENTRY(name, regs) \
	.globl _/**/name; .align 1; _/**/name: .word regs
#define	JSBENTRY(name, regs) \
	.globl _/**/name; _/**/name:
#endif GPROF
#define R0 0x01
#define R1 0x02
#define R2 0x04
#define R3 0x08
#define R4 0x10
#define R5 0x20
#define R6 0x40

/*
 * badaddr(addr, len)
 *	see if access addr with a len type instruction causes a machine check
 *	len is length of access (1=byte, 2=short, 4=long)
 */
	.globl	_badaddr
_badaddr:
	.word	0
	movl	$1,r0
	mfpr	$IPL,r1
	mtpr	$HIGH,$IPL
	movl	4(ap),r3
	movl	8(ap),r4
	movab	2f,nofault		# jump to 2f on machcheck
	bbc	$0,r4,1f; tstb	(r3)
1:	bbc	$1,r4,1f; tstw	(r3)
1:	bbc	$2,r4,1f; tstl	(r3)
1:	clrl	r0			# made it w/o machine checks
2:	clrl	nofault
	mtpr	r1,$IPL
	ret

/*
 * update profiling information for the user
 * addupc(pc, &u.u_prof, ticks)
 */
ENTRY(addupc, 0)
	movl	8(ap),r2		# &u.u_prof
	subl3	8(r2),4(ap),r0		# corrected pc
	blss	9f
	extzv	$1,$31,r0,r0		# logical right shift
	extzv	$1,$31,12(r2),r1	# ditto for scale
	emul	r1,r0,$0,r0
	ashq	$-14,r0,r0
	tstl	r1
	bneq	9f
	bicl2	$1,r0
	cmpl	r0,4(r2)		# length
	bgequ	9f
	addl2	(r2),r0			# base
	probew	$3,$2,(r0)
	beql	8f
	addw2	12(ap),(r0)
9:
	ret
8:
	clrl	12(r2)
	ret

/*
 * Copy a null terminated string from the user address space into
 * the kernel address space.
 *
 * copyinstr(fromaddr, toaddr, maxlength, &lencopied)
 */
ENTRY(copyinstr, R6)
	movl	12(ap),r6		# r6 = max length
	jlss	8f
	movl	4(ap),r1		# r1 = user address
	bicl3	$~(NBPG*CLSIZE-1),r1,r2	# r2 = bytes on first page
	subl3	r2,$NBPG*CLSIZE,r2
	movl	8(ap),r3		# r3 = kernel address
1:
	cmpl	r6,r2			# r2 = min(bytes on page, length left);
	jgeq	2f
	movl	r6,r2
2:
	prober	$3,r2,(r1)		# bytes accessible?
	jeql	8f
	subl2	r2,r6			# update bytes left count
	locc	$0,r2,(r1)		# null byte found?
	jneq	3f
	subl2	r2,r1			# back up pointer updated by `locc'
	movc3	r2,(r1),(r3)		# copy in next piece
	movl	$(NBPG*CLSIZE),r2	# check next page
	tstl	r6			# run out of space?
	jneq	1b
	movl	$ENOENT,r0		# set error code and return
	jbr	9f
3:
	tstl	16(ap)			# return length?
	beql	4f
	subl3	r6,12(ap),r6		# actual len = maxlen - unused pages
	subl2	r0,r6			#	- unused on this page
	addl3	$1,r6,*16(ap)		#	+ the null byte
4:
	subl2	r0,r2			# r2 = number of bytes to move
	subl2	r2,r1			# back up pointer updated by `locc'
	incl	r2			# copy null byte as well
	movc3	r2,(r1),(r3)		# copy in last piece
	clrl	r0			# redundant
	ret
8:
	movl	$EFAULT,r0
9:
	tstl	16(ap)
	beql	1f
	subl3	r6,12(ap),*16(ap)
1:
	ret

/*
 * Copy a null terminated string from the kernel
 * address space to the user address space.
 *
 * copyoutstr(fromaddr, toaddr, maxlength, &lencopied)
 */
ENTRY(copyoutstr, R6)
	movl	12(ap),r6		# r6 = max length
	jlss	8b
	movl	4(ap),r1		# r1 = kernel address
	movl	8(ap),r3		# r3 = user address
	bicl3	$~(NBPG*CLSIZE-1),r3,r2	# r2 = bytes on first page
	subl3	r2,$NBPG*CLSIZE,r2
1:
	cmpl	r6,r2			# r2 = min(bytes on page, length left);
	jgeq	2f
	movl	r6,r2
2:
	probew	$3,r2,(r3)		# bytes accessible?
	jeql	8b
	subl2	r2,r6			# update bytes left count
	locc	$0,r2,(r1)		# null byte found?
	jneq	3b
	subl2	r2,r1			# back up pointer updated by `locc'
	movc3	r2,(r1),(r3)		# copy in next piece
	movl	$(NBPG*CLSIZE),r2	# check next page
	tstl	r6			# run out of space?
	jneq	1b
	movl	$ENOENT,r0		# set error code and return
	jbr	9b

/*
 * Copy a null terminated string from one point to another in
 * the kernel address space.
 *
 * copystr(fromaddr, toaddr, maxlength, &lencopied)
 */
ENTRY(copystr, R6)
	movl	12(ap),r6		# r6 = max length
	jlss	8b
	movl	4(ap),r1		# r1 = src address
	movl	8(ap),r3		# r3 = dest address
1:
	movzwl	$65535,r2		# r2 = bytes in first chunk
	cmpl	r6,r2			# r2 = min(bytes in chunk, length left);
	jgeq	2f
	movl	r6,r2
2:
	subl2	r2,r6			# update bytes left count
	locc	$0,r2,(r1)		# null byte found?
	jneq	3b
	subl2	r2,r1			# back up pointer updated by `locc'
	movc3	r2,(r1),(r3)		# copy in next piece
	tstl	r6			# run out of space?
	jneq	1b
	movl	$ENOENT,r0		# set error code and return
	jbr	9b

/* 
 * Copy specified amount of data from user space into the kernel
 * Copyin(from, to, len)
 *	r1 == from (user source address)
 *	r3 == to (kernel destination address)
 *	r5 == length
 */
	.align	1
JSBENTRY(Copyin, R1|R3|R5)
	cmpl	r5,$(NBPG*CLSIZE)	# probing one page or less ?
	bgtru	1f			# no
	prober	$3,r5,(r1)		# bytes accessible ?
	beql	ersb			# no
	movc3	r5,(r1),(r3)
/*	clrl	r0			# redundant */
	rsb
1:
	blss	ersb			# negative length?
	pushl	r6			# r6 = length
	movl	r5,r6
	bicl3	$~(NBPG*CLSIZE-1),r1,r0	# r0 = bytes on first page
	subl3	r0,$(NBPG*CLSIZE),r0
	addl2	$(NBPG*CLSIZE),r0	# plus one additional full page
	jbr	2f

ciloop:
	movc3	r0,(r1),(r3)
	movl	$(2*NBPG*CLSIZE),r0	# next amount to move
2:
	cmpl	r0,r6
	bleq	3f
	movl	r6,r0
3:
	prober	$3,r0,(r1)		# bytes accessible ?
	beql	ersb1			# no
	subl2	r0,r6			# last move?
	bneq	ciloop			# no

	movc3	r0,(r1),(r3)
/*	clrl	r0			# redundant */
	movl	(sp)+,r6		# restore r6
	rsb

ersb1:
	movl	(sp)+,r6		# restore r6
ersb:
	movl	$EFAULT,r0
	rsb

/* 
 * Copy specified amount of data from kernel to the user space
 * Copyout(from, to, len)
 *	r1 == from (kernel source address)
 *	r3 == to (user destination address)
 *	r5 == length
 */
	.align	1
JSBENTRY(Copyout, R1|R3|R5)
	cmpl	r5,$(NBPG*CLSIZE)	# moving one page or less ?
	bgtru	1f			# no
	probew	$3,r5,(r3)		# bytes writeable?
	beql	ersb			# no
	movc3	r5,(r1),(r3)
/*	clrl	r0			# redundant */
	rsb
1:
	blss	ersb			# negative length?
	pushl	r6			# r6 = length
	movl	r5,r6
	bicl3	$~(NBPG*CLSIZE-1),r3,r0	# r0 = bytes on first page
	subl3	r0,$(NBPG*CLSIZE),r0
	addl2	$(NBPG*CLSIZE),r0	# plus one additional full page
	jbr	2f

coloop:
	movc3	r0,(r1),(r3)
	movl	$(2*NBPG*CLSIZE),r0	# next amount to move
2:
	cmpl	r0,r6
	bleq	3f
	movl	r6,r0
3:
	probew	$3,r0,(r3)		# bytes writeable?
	beql	ersb1			# no
	subl2	r0,r6			# last move?
	bneq	coloop			# no

	movc3	r0,(r1),(r3)
/*	clrl	r0			# redundant */
	movl	(sp)+,r6		# restore r6
	rsb

/*
 * non-local goto's
 */
#ifdef notdef		/* this is now expanded completely inline */
	.align	1
JSBENTRY(Setjmp, R0)
	movl	fp,(r0)+	# current stack frame
	movl	(sp),(r0)	# resuming pc
	clrl	r0
	rsb
#endif

#define PCLOC 16	/* location of pc in calls frame */
#define APLOC 8		/* location of ap,fp in calls frame */
	.align	1
JSBENTRY(Longjmp, R0)
	movl	(r0)+,newfp	# must save parameters in memory as all
	movl	(r0),newpc	# registers may be clobbered.
1:
	cmpl	fp,newfp	# are we there yet?
	bgequ	2f		# yes
	moval	1b,PCLOC(fp)	# redirect return pc to us!
	ret			# pop next frame
2:
	beql	3f		# did we miss our frame?
	pushab	4f		# yep ?!?
	calls	$1,_panic
3:
	movl	newpc,r0	# all done, just return to the `setjmp'
	jmp	(r0)		# ``rsb''

	.data
newpc:	.space	4
newfp:	.space	4
4:	.asciz	"longjmp"
	.text
/*
 * setjmp that saves all registers as the call frame may not
 * be available to recover them in the usual mannor by longjmp.
 * Called before swapping out the u. area, restored by resume()
 * below.
 */
ENTRY(savectx, 0)
	movl	4(ap),r0
	movq	r6,(r0)+
	movq	r8,(r0)+
	movq	r10,(r0)+
	movq	APLOC(fp),(r0)+	# save ap, fp
	addl3	$8,ap,(r0)+	# save sp
	movl	PCLOC(fp),(r0)	# save pc
	clrl	r0
	ret

	.globl	_whichqs
	.globl	_qs
	.globl	_cnt

	.globl	_noproc
	.comm	_noproc,4
	.globl	_runrun
	.comm	_runrun,4

/*
 * The following primitives use the fancy VAX instructions
 * much like VMS does.  _whichqs tells which of the 32 queues _qs
 * have processes in them.  Setrq puts processes into queues, Remrq
 * removes them from queues.  The running process is on no queue,
 * other processes are on a queue related to p->p_pri, divided by 4
 * actually to shrink the 0-127 range of priorities into the 32 available
 * queues.
 */

/*
 * Setrq(p), using fancy VAX instructions.
 *
 * Call should be made at spl6(), and p->p_stat should be SRUN
 */
	.align	1
 JSBENTRY(Setrq, R0)
	tstl	P_RLINK(r0)		## firewall: p->p_rlink must be 0
	beql	set1			##
	pushab	set3			##
	calls	$1,_panic		##
set1:
	movzbl	P_PRI(r0),r1		# put on queue which is p->p_pri / 4
	ashl	$-2,r1,r1
	movaq	_qs[r1],r2
	insque	(r0),*4(r2)		# at end of queue
	bbss	r1,_whichqs,set2	# mark queue non-empty
set2:
	rsb

set3:	.asciz	"setrq"

/*
 * Remrq(p), using fancy VAX instructions
 *
 * Call should be made at spl6().
 */
	.align	1
 JSBENTRY(Remrq, R0)
	movzbl	P_PRI(r0),r1
	ashl	$-2,r1,r1
	bbsc	r1,_whichqs,rem1
	pushab	rem3			# it wasn't recorded to be on its q
	calls	$1,_panic
rem1:
	remque	(r0),r2
	beql	rem2
	bbss	r1,_whichqs,rem2
rem2:
	clrl	P_RLINK(r0)		## for firewall checking
	rsb

rem3:	.asciz	"remrq"

/*
 * Masterpaddr is the p->p_addr of the running process on the master
 * processor.  When a multiprocessor system, the slave processors will have
 * an array of slavepaddr's.
 */
	.globl	_masterpaddr
	.data
_masterpaddr:
	.long	0

	.set	ASTLVL_NONE,4
	.text
sw0:	.asciz	"swtch"

/*
 * When no processes are on the runq, Swtch branches to idle
 * to wait for something to come ready.
 */
	.globl	Idle
Idle: idle:
	mtpr	$0,$IPL			# must allow interrupts here
	tstl	_whichqs		# look for non-empty queue
	bneq	sw1
	brb	idle

badsw:	pushab	sw0
	calls	$1,_panic
	/*NOTREACHED*/

/*
 * Swtch(), using fancy VAX instructions
 */
	.align	1
JSBENTRY(Swtch, 0)
	movl	$1,_noproc
	incl	_cnt+V_SWTCH
sw1:	ffs	$0,$32,_whichqs,r0	# look for non-empty queue
	beql	idle			# if none, idle
	mtpr	$0x18,$IPL		# lock out all so _whichqs==_qs
	bbcc	r0,_whichqs,sw1		# proc moved via lbolt interrupt
	movaq	_qs[r0],r1
	remque	*(r1),r2		# r2 = p = highest pri process
	bvs	badsw			# make sure something was there
sw2:	beql	sw3
	insv	$1,r0,$1,_whichqs	# still more procs in this queue
sw3:
	clrl	_noproc
	clrl	_runrun
	tstl	P_WCHAN(r2)		## firewalls
	bneq	badsw			##
	cmpb	P_STAT(r2),$SRUN	##
	bneq	badsw			##
	clrl	P_RLINK(r2)		##
	movl	*P_ADDR(r2),r0
#ifdef notdef
	cmpl	r0,_masterpaddr		# resume of current proc is easy
	beql	res0
#endif
	movl	r0,_masterpaddr
	ashl	$PGSHIFT,r0,r0		# r0 = pcbb(p)
/* fall into... */

/*
 * Resume(pf)
 */
JSBENTRY(Resume, R0)
	mtpr	$HIGH,$IPL			# no interrupts, please
	movl	_CMAP2,_u+PCB_CMAP2	# yech
	svpctx
	mtpr	r0,$PCBB
	ldpctx
	movl	_u+PCB_CMAP2,_CMAP2	# yech
	mtpr	$_CADDR2,$TBIS
res0:
	tstl	_u+PCB_SSWAP
	bneq	res1
	rei
res1:
	movl	_u+PCB_SSWAP,r0			# longjmp to saved context
	clrl	_u+PCB_SSWAP
	movq	(r0)+,r6
	movq	(r0)+,r8
	movq	(r0)+,r10
	movq	(r0)+,r12
	movl	(r0)+,r1
	cmpl	r1,sp				# must be a pop
	bgequ	1f
	pushab	2f
	calls	$1,_panic
	/* NOTREACHED */
1:
	movl	r1,sp
	movl	(r0),(sp)			# address to return to
	movl	$PSL_PRVMOD,4(sp)		# ``cheating'' (jfr)
	rei

2:	.asciz	"ldctx"

/*
 * {fu,su},{byte,word}, all massaged by asm.sed to jsb's
 */
	.align	1
JSBENTRY(Fuword, R0)
	prober	$3,$4,(r0)
	beql	fserr
	movl	(r0),r0
	rsb
fserr:
	mnegl	$1,r0
	rsb

	.align	1
JSBENTRY(Fubyte, R0)
	prober	$3,$1,(r0)
	beql	fserr
	movzbl	(r0),r0
	rsb

	.align	1
JSBENTRY(Suword, R0|R1)
	probew	$3,$4,(r0)
	beql	fserr
	movl	r1,(r0)
	clrl	r0
	rsb

	.align	1
JSBENTRY(Subyte, R0|R1)
	probew	$3,$1,(r0)
	beql	fserr
	movb	r1,(r0)
	clrl	r0
	rsb

/*
 * Copy 1 relocation unit (NBPG bytes)
 * from user virtual address to physical address
 */
ENTRY(copyseg, 0)
	bisl3	$PG_V|PG_KW,8(ap),_CMAP2
	mtpr	$_CADDR2,$TBIS	# invalidate entry for copy 
	movc3	$NBPG,*4(ap),_CADDR2
	ret

/*
 * zero out physical memory
 * specified in relocation units (NBPG bytes)
 */
ENTRY(clearseg, 0)
	bisl3	$PG_V|PG_KW,4(ap),_CMAP1
	mtpr	$_CADDR1,$TBIS
	movc5	$0,(sp),$0,$NBPG,_CADDR1
	ret

/*
 * Check address.
 * Given virtual address, byte count, and rw flag
 * returns 0 on no access.
 */
ENTRY(useracc, 0)
	movl	4(ap),r0		# get va
	movl	8(ap),r1		# count
	tstl	12(ap)			# test for read access ?
	bneq	userar			# yes
	cmpl	$NBPG,r1			# can we do it in one probe ?
	bgeq	uaw2			# yes
uaw1:
	probew	$3,$NBPG,(r0)
	beql	uaerr			# no access
	addl2	$NBPG,r0
	acbl	$NBPG+1,$-NBPG,r1,uaw1
uaw2:
	probew	$3,r1,(r0)
	beql	uaerr
	movl	$1,r0
	ret

userar:
	cmpl	$NBPG,r1
	bgeq	uar2
uar1:
	prober	$3,$NBPG,(r0)
	beql	uaerr
	addl2	$NBPG,r0
	acbl	$NBPG+1,$-NBPG,r1,uar1
uar2:
	prober	$3,r1,(r0)
	beql	uaerr
	movl	$1,r0
	ret
uaerr:
	clrl	r0
	ret

/*
 * kernacc - check for kernel access privileges
 *
 * We can't use the probe instruction directly because
 * it ors together current and previous mode.
 */
 ENTRY(kernacc, 0)
	movl	4(ap),r0	# virtual address
	bbcc	$31,r0,kacc1
	bbs	$30,r0,kacerr
	mfpr	$SBR,r2		# address and length of page table (system)
	bbss	$31,r2,0f; 0:
	mfpr	$SLR,r3
	brb	kacc2
kacc1:
	bbsc	$30,r0,kacc3
	mfpr	$P0BR,r2	# user P0
	mfpr	$P0LR,r3
	brb	kacc2
kacc3:
	mfpr	$P1BR,r2	# user P1 (stack)
	mfpr	$P1LR,r3
kacc2:
	addl3	8(ap),r0,r1	# ending virtual address
	addl2	$NBPG-1,r1
	ashl	$-PGSHIFT,r0,r0
	ashl	$-PGSHIFT,r1,r1
	bbs	$31,4(ap),kacc6
	bbc	$30,4(ap),kacc6
	cmpl	r0,r3		# user stack
	blss	kacerr		# address too low
	brb	kacc4
kacc6:
	cmpl	r1,r3		# compare last page to P0LR or SLR
	bgtr	kacerr		# address too high
kacc4:	
	movl	(r2)[r0],r3
	bbc	$31,4(ap),kacc4a
	bbc	$31,r3,kacerr	# valid bit is off
kacc4a:
	cmpzv	$27,$4,r3,$1	# check protection code
	bleq	kacerr		# no access allowed
	tstb	12(ap)
	bneq	kacc5		# only check read access
	cmpzv	$27,$2,r3,$3	# check low 2 bits of prot code
	beql	kacerr		# no write access
kacc5:
	aoblss	r1,r0,kacc4	# next page
	movl	$1,r0		# no errors
	ret
kacerr:
	clrl	r0		# error
	ret
/*
 * Extracted and unrolled most common case of pagein (hopefully):
 *	resident and not on free list (reclaim of page is purely
 *	for the purpose of simulating a reference bit)
 *
 * Built in constants:
 *	CLSIZE of 2, any bit fields in pte's
 */
	.text
	.globl	Fastreclaim
Fastreclaim:
	PUSHR
#ifdef GPROF
	movl	fp,-(sp)
	movab	12(sp),fp
	jsb	mcount
	movl	(sp)+,fp
#endif GPROF
	extzv	$9,$23,28(sp),r3	# virtual address
	bicl2	$1,r3			# v = clbase(btop(virtaddr)); 
	movl	_u+U_PROCP,r5		# p = u.u_procp 
					# from vtopte(p, v) ...
	movl	$1,r2			# type = CTEXT;
	cmpl	r3,P_TSIZE(r5)
	jlssu	1f			# if (isatsv(p, v)) {
	addl3	P_TSIZE(r5),P_DSIZE(r5),r0
	cmpl	r3,r0
	jgequ	2f
	clrl	r2			#	type = !CTEXT;
1:
	ashl	$2,r3,r4
	addl2	P_P0BR(r5),r4		#	tptopte(p, vtotp(p, v));
	jbr	3f
2:
	cvtwl	P_SZPT(r5),r4		# } else (isassv(p, v)) {
	ashl	$7,r4,r4
	subl2	$0x400000,r4
	addl2	r3,r4
	ashl	$2,r4,r4
	addl2	P_P0BR(r5),r4		#	sptopte(p, vtosp(p, v));
	clrl	r2			# 	type = !CTEXT;
3:					# }
	bitb	$0x82,3(r4)
	beql	2f			# if (pte->pg_v || pte->pg_fod)
	POPR; rsb			#	let pagein handle it
2:
	bicl3	$0xffe00000,(r4),r0
	jneq	2f			# if (pte->pg_pfnum == 0)
	POPR; rsb			# 	let pagein handle it 
2:
	subl2	_firstfree,r0
	ashl	$-1,r0,r0	
	incl	r0			# pgtocm(pte->pg_pfnum) 
	mull2	$SZ_CMAP,r0
	addl2	_cmap,r0		# &cmap[pgtocm(pte->pg_pfnum)] 
	tstl	r2
	jeql	2f			# if (type == CTEXT &&
	jbc	$C_INTRANS,(r0),2f	#     c_intrans)
	POPR; rsb			# 	let pagein handle it
2:
	jbc	$C_FREE,(r0),2f		# if (c_free)
	POPR; rsb			# 	let pagein handle it 
2:
	bisb2	$0x80,3(r4)		# pte->pg_v = 1;
	jbc	$26,4(r4),2f		# if (anycl(pte, pg_m) 
	bisb2	$0x04,3(r4)		#	pte->pg_m = 1;
2:
	bicw3	$0x7f,2(r4),r0
	bicw3	$0xff80,6(r4),r1
	bisw3	r0,r1,6(r4)		# distcl(pte);
	ashl	$PGSHIFT,r3,r0
	mtpr	r0,$TBIS
	addl2	$NBPG,r0
	mtpr	r0,$TBIS		# tbiscl(v); 
	tstl	r2
	jeql	2f			# if (type == CTEXT) 
	movl	P_TEXTP(r5),r0
	movl	X_CADDR(r0),r5		# for (p = p->p_textp->x_caddr; p; ) {
	jeql	2f
	ashl	$2,r3,r3
3:
	addl3	P_P0BR(r5),r3,r0	#	tpte = tptopte(p, tp);
	bisb2	$1,P_FLAG+3(r5)		#	p->p_flag |= SPTECHG;
	movl	(r4),(r0)+		#	for (i = 0; i < CLSIZE; i++)
	movl	4(r4),(r0)		#		tpte[i] = pte[i];
	movl	P_XLINK(r5),r5		#	p = p->p_xlink;
	jneq	3b			# }
2:					# collect a few statistics...
	incl	_u+U_RU+RU_MINFLT	# u.u_ru.ru_minflt++;
	moval	_cnt,r0
	incl	V_FAULTS(r0)		# cnt.v_faults++; 
	incl	V_PGREC(r0)		# cnt.v_pgrec++;
	incl	V_FASTPGREC(r0)		# cnt.v_fastpgrec++;
	incl	V_TRAP(r0)		# cnt.v_trap++;
	POPR
	addl2	$8,sp			# pop pc, code
	mtpr	$HIGH,$IPL		## dont go to a higher IPL (GROT)
	rei
