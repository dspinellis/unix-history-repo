/*	locore.s	1.7	85/05/15	*/

#include "../tahoe/mtpr.h"
#include "../tahoe/trap.h"
#include "../tahoe/psl.h"
#include "../tahoe/pte.h"
#include "../tahoe/cp.h"
#include "../tahoe/mem.h"
#include "../tahoe/fp.h"

#include "../h/errno.h"

	.set	HIGH,0x1f	# mask for total disable
	.set	BERVEC,0x80	# offset into scb of the bus error vector 
	.set	RESTVEC,0x8	# offset into scb of the restart vector 
	.set	MAXPHYSMEM,8*1024*1024-1 # max physical memory 
					# while we work on CMD/32M !
					# look at vmsched.c to see why.
	.set	MEMUNIT,64*1024 # minimum memory increment

	.set	NISP,3		# number of interrupt stack pages
	.set	SYSTEM,0xC0000000 # virtual address of system start
	.set	PPAGES,0x100000  # Number of possible pages in P0,P1, etc.

/* ACBL for non-negative '_add' */
#define ACBL(_limit,_add,_index,_displ) \
	addl2	_add,_index; \
	cmpl	_index,_limit; \
	bleq	_displ

/* _ACBL for negative '_add' */
#define _ACBL(_limit,_add,_index,_displ) \
	addl2	_add,_index; \
	cmpl	_index,_limit; \
	bgeq	_displ

#define	MOVC3(_len,_srcaddr,_dstaddr) \
	movl	_srcaddr,r0; \
	movl	_dstaddr,r1; \
	movl	_len,r2; \
	movblk

/* Keep address of psl if coming from user mode */
#define CHECK_SFE(_delta) \
	bitl	$PSL_CURMOD,_delta(sp); \
	jeql	1f; \
	moval	_delta(sp),_user_psl; \
1:

/*
 * User structure is UPAGES at top of user space.
 */
	.globl	_u
	.set	_u,SYSTEM - UPAGES*NBPG

/*
 * Restart stack. Used on power recovery or panic.
 * Takes a core-dump and then halts.
 */ 
	.globl	_rsstk

_rsstk: 	.space	1024-8
	.globl	pwfl_stk	
pwfl_stk:	.space  4
dumpflag:	.space	4

	.globl	_intstack
_intstack:
	.space	NISP*NBPG
eintstack:

	.data
	.globl	pwfl_r0
pwfl_r0:	.space	14*4		# Enough for r0 - r13
	.globl	pwfl_sp
pwfl_sp:	.long	0x12345678	# r14
	.globl	pwfl_SCBB
pwfl_SCBB:	.long	0x12345678
	.globl	pwfl_SBR
pwfl_SBR:	.long	0x12345678
	.globl	pwfl_SLR
pwfl_SLR:	.long	0x12345678
	.globl	pwfl_P0BR
pwfl_P0BR:	.long	0x12345678
	.globl	pwfl_P0LR
pwfl_P0LR:	.long	0x12345678
	.globl	pwfl_P1BR
pwfl_P1BR:	.long	0x12345678
	.globl	pwfl_P1LR
pwfl_P1LR:	.long	0x12345678
	.globl	pwfl_P2BR
pwfl_P2BR:	.long	0x12345678
	.globl	pwfl_P2LR
pwfl_P2LR:	.long	0x12345678
	.globl	pwfl_IPL
pwfl_IPL:	.long	0x12345678
	.globl	pwfl_DCK
pwfl_DCK:	.long	0x12345678
	.globl	pwfl_CCK
pwfl_CCK:	.long	0x12345678
	.globl	pwfl_PCBB
pwfl_PCBB:	.long	0x12345678
	.globl	pwfl_ISP
pwfl_ISP:	.long	0x12345678
	.globl	pwfl_KSP
pwfl_KSP:	.long	0x12345678
	.globl	pwfl_USP
pwfl_USP:	.long	0x12345678
	.globl	pwfl_MME
pwfl_MME:	.long	0x12345678
	.globl	pwfl_PSL
pwfl_PSL:	.long	0x12345678

/*
 * Do a dump.
 * Called by auto-restart.
 * May be called manually.
 */
	.align	2
	.text
	.globl	_Xdoadump
	.globl	_doadump
_Xdoadump:			# CP brings Tahoe here on power recovery
	loadr	$0x3fff,pwfl_r0	# Restore r0 - r13
	movl	pwfl_sp,sp	# Restore sp ( = r14 )
	mtpr	pwfl_SCBB,$SCBB
	mtpr	pwfl_SBR,$SBR	# Restore all re_loadable registers
	mtpr	pwfl_SLR,$SLR
	mtpr	pwfl_P0BR,$P0BR
	mtpr	pwfl_P0LR,$P0LR
	mtpr	pwfl_P1BR,$P1BR
	mtpr	pwfl_P1LR,$P1LR
	mtpr	pwfl_P2BR,$P2BR
	mtpr	pwfl_P2LR,$P2LR
	mtpr	pwfl_IPL,$IPL
	mtpr	pwfl_DCK,$DCK
	mtpr	pwfl_CCK,$CCK
	mtpr	pwfl_PCBB,$PCBB
	mtpr	pwfl_ISP,$ISP
	mtpr	pwfl_KSP,$KSP
	mtpr	pwfl_USP,$USP

	bicpsw	$0xff		# Restore PSW.
	bispsw	pwfl_PSL+2	# Set original bits back (just in case..)
/* now go to mapped mode */
/* Have to change PC to system addresses */
	mtpr	$1,$PACC	# Thoroughly clean up caches.
	mtpr	$1,$PADC
	mtpr	$1,$TBIA
	mtpr	pwfl_MME,$MME  	# Restore MME. Last thing to be done.
	jmp 	*$0f
_doadump:
	.word 0
#define	_rsstkmap	_Sysmap+12			
			# Tahoe storage, scb, rsstk, interrupt stack
0:
	mtpr	$HIGH,$IPL
	andl2	$0!PG_PROT,_rsstkmap
	orl2	$PG_KW,_rsstkmap		# Make dump stack r/w
	tstl	dumpflag			# dump only once!
	bneq	1f
	movl	$1,dumpflag
	mtpr	$0,$TBIA
	movab	dumpflag,sp
	callf	$4,_dumpsys
1:
	halt

/*
 * Interrupt vector routines
 */ 
	.globl	_waittime

#define	SCBVEC(name) \
	.align 2; \
	.globl _X/**/name; \
_X/**/name
#define	PANIC(msg)	clrl _waittime; pushab 1f; \
			callf $8,_panic; 1: .asciz msg
#define	PRINTF(n,msg)	pushab 1f; callf $(n+2)*4,_printf; MSG(msg)
#define	MSG(msg)	.data; 1: .asciz msg; .text

			# these registers are not restored by the C-compiler.
#define	PUSHR		pushl r0; pushl r1;
#define	POPR		movl (sp)+, r1; movl (sp)+, r0;
			# mask for error code on stack.

#define SAVE_FPSTAT(_delta)	bitl	$PSL_DBL,_delta(sp); \
				beql	1f; \
				pushl	$1; \
				pushd; \
				jmp	2f; \
			1:	pushl	$0; \
				pushl	$0; \
				stf	-(sp); \
			2:	tstl	_u+PCB_SAVACC; \
				bneq	3f; \
				moval	0(sp),_u+PCB_SAVACC; \
				orl2	$2,8(sp);\
			3:	pushl	$0;

#define REST_FPSTAT		tstl	(sp)+; \
				bitl	$2,8(sp);\
				beql	1f;\
				movl	$0,_u+PCB_SAVACC; \
			1:	bitl	$1,8(sp); \
				beql	2f; \
				ldd	(sp); \
				jmp	3f; \
			2:	ldf	(sp); \
			3:	moval	12(sp),sp;

#define REST_ACC		tstl	_u+PCB_SAVACC; \
				beql	2f; \
				movl	_u+PCB_SAVACC,r1; \
				andl3	$(EXPMASK|SIGNBIT),(r1),-(sp); \
				cmpl	$0x80000000,(sp)+; \
				bneq	3f; \
				clrl	(r1); \
			3:	bitl	$1,8(r1); \
				beql	1f; \
				ldd	(r1); \
				jmp	2f; \
			1:	ldf	(r1); \
			2:	;

#define PUSHBPAR	pushab	6*4(sp) /* Push address of buserr paramters */
#define BPAR1		28		/* Offset to first hardware parameter */

SCBVEC(buserr):
	CHECK_SFE(12)
	SAVE_FPSTAT(12);
	PUSHR
	andl3	BPAR1(sp),$ERRCD,r0
	jeql	go_on
	cmpl	r0,$APE
	jneq	1f
	halt		# Address parity error !!!
1:	cmpl	r0,$VBE
	jneq	go_on
	halt		# Versabus error !!!
go_on:
	PUSHBPAR	# Pointer to parameters
	callf	$8,_buserror
	POPR
	REST_FPSTAT;
	movab	8(sp),sp	# Remove hardware parameters
	rei

SCBVEC(powfail):		# We should be on interrupt stack now.
	movpsl	pwfl_PSL	# Keeps all flags, etc.
	storer	$0x3fff,pwfl_r0	# Saves r0 - r13
	moval	0(sp),pwfl_sp	# Saves sp ( = r14 )
	mfpr	$SBR,pwfl_SBR	# Save all re_loadable registers
	mfpr	$SLR,pwfl_SLR
	mfpr	$P0BR,pwfl_P0BR
	mfpr	$P0LR,pwfl_P0LR
	mfpr	$P1BR,pwfl_P1BR
	mfpr	$P1LR,pwfl_P1LR
	mfpr	$P2BR,pwfl_P2BR
	mfpr	$P2LR,pwfl_P2LR
	mfpr	$IPL,pwfl_IPL
	mfpr	$MME,pwfl_MME  
	mfpr	$DCK,pwfl_DCK
	mfpr	$CCK,pwfl_CCK
	mfpr	$PCBB,pwfl_PCBB
	mfpr	$ISP,pwfl_ISP
	mfpr	$SCBB,pwfl_SCBB
	mfpr	$KSP,pwfl_KSP
	mfpr	$USP,pwfl_USP
	moval	_Xdoadump-SYSTEM,_scb+RESTVEC
	halt

SCBVEC(stray):
	PUSHR; PRINTF(0, "******* Undefined interrupt *******\n"); POPR;
	rei

#include "../net/netisr.h"
	.globl	_netisr
SCBVEC(netintr):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR

	bbc	$NETISR_RAW,_netisr,1f
	andl2	$(0!(1<<NETISR_RAW)),_netisr	
	callf	$4,_rawintr	
1:
#ifdef INET
#include "../netinet/in_systm.h"
	bbc	$NETISR_IP,_netisr,1f	
	andl2	$(0!(1<<NETISR_IP)),_netisr
	callf	$4,_ipintr	
1:
#endif
#ifdef NS
	bbc	$NETISR_NS,_netisr,1f	
	andl2	$(0!(1<<NETISR_NS)),_netisr
	callf	$4,_nsintr	
1:
#endif
	POPR; 
	REST_FPSTAT
	rei
SCBVEC(soft15):
SCBVEC(soft14):
SCBVEC(soft13):
SCBVEC(soft11):
SCBVEC(soft10):
#ifndef SIMIO
SCBVEC(soft9):
#endif
SCBVEC(soft7):
SCBVEC(soft6):
SCBVEC(soft5):
SCBVEC(soft4):
#ifndef SIMIO
SCBVEC(soft3):
SCBVEC(soft2):
SCBVEC(soft1):
#endif
	PUSHR
	PRINTF(0, "******* Undefined software interrupt *******\n")
	POPR;
	rei

#ifdef SIMIO
SCBVEC(soft2):
#endif
SCBVEC(cnrint):
	CHECK_SFE(4)
	SAVE_FPSTAT(4);PUSHR; 
	pushl $CPCONS; callf $8,_cnrint; POPR; incl _cnt+V_INTR;
	REST_FPSTAT; rei
#ifdef SIMIO
SCBVEC(soft3):
#endif
SCBVEC(cnxint):
	CHECK_SFE(4)
	SAVE_FPSTAT(4);PUSHR; 
	pushl $CPCONS; callf $8,_cnxint; POPR; REST_FPSTAT;
	incl _cnt+V_INTR; rei
SCBVEC(rmtrint):
	CHECK_SFE(4)
	SAVE_FPSTAT(4); PUSHR; 
	pushl $CPREMOT; callf $8,_cnrint; POPR; REST_FPSTAT;
	incl _cnt+V_INTR; rei
SCBVEC(rmtxint):
	CHECK_SFE(4)
	SAVE_FPSTAT(4); PUSHR; 
	pushl $CPREMOT; callf $8,_cnxint; POPR; REST_FPSTAT;
	incl _cnt+V_INTR; rei
#ifdef SIMIO
SCBVEC(soft9):
#endif

#define PUSHPCPSL	pushl 5*4+2*4(sp); pushl 5*4+2*4(sp);

SCBVEC(hardclock):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	PUSHPCPSL				# push pc and psl
	callf $12,_hardclock			# hardclock(pc,psl)
	POPR;
	REST_FPSTAT
	incl	_cnt+V_INTR		## temp so not to break vmstat -= HZ
	rei
SCBVEC(softclock):
	CHECK_SFE(4)
	SAVE_FPSTAT(4)
	PUSHR
	PUSHPCPSL				# push pc and psl
	callf $12,_softclock			# softclock(pc,psl)
	POPR; 
	REST_FPSTAT
	rei

/*
 * Trap and fault vector routines
 */ 
#define	TRAP(a)	pushl $T_/**/a; jbr alltraps

/*
 * Ast delivery (profiling and/or reschedule)
 */
/*
 * When we want to reschedule we will force a memory fault by setting the m.s.b
 *  of P0LR. Then , on memory fault if m.s.b of P0LR is on we will clear it and
 *  TRAP(astflt).
 *
 */
 
					# if this bit is on it is an ast.
#define 	ASTBIT	0x00100000  	
#define		P0MASK	0xc0000000

SCBVEC(kspnotval):
	CHECK_SFE(4)
	pushl $0;
	SAVE_FPSTAT(8)
	TRAP(KSPNOTVAL)
SCBVEC(privinflt):
	CHECK_SFE(4)
	pushl $0;
	SAVE_FPSTAT(8)
	TRAP(PRIVINFLT)
SCBVEC(resopflt):
	CHECK_SFE(4)
	pushl $0;
	SAVE_FPSTAT(8)
	TRAP(RESOPFLT)
SCBVEC(resadflt):
	CHECK_SFE(4)
	pushl $0;
	SAVE_FPSTAT(8)
	TRAP(RESADFLT)
SCBVEC(bptflt):
	CHECK_SFE(4)
	pushl $0;
	SAVE_FPSTAT(8)
	TRAP(BPTFLT)
SCBVEC(tracep):
	CHECK_SFE(4)
	pushl $0;
	SAVE_FPSTAT(8)
	TRAP(TRCTRAP)
SCBVEC(alignflt):
	CHECK_SFE(4)
	pushl $0;
	SAVE_FPSTAT(8)
	TRAP(ALIGNFLT)
SCBVEC(arithtrap):
	CHECK_SFE(8)
	SAVE_FPSTAT(8)
	TRAP(ARITHTRAP)

SCBVEC(protflt):
	CHECK_SFE(12)
	bitl	$1,(sp)+
	jneq	segflt
	SAVE_FPSTAT(8)
	TRAP(PROTFLT)
segflt:
	SAVE_FPSTAT(8)
	TRAP(SEGFLT)

SCBVEC(fpm):			# Floating Point eMulation
	.globl	_fpemulate
	CHECK_SFE(16)
	SAVE_FPSTAT(16)
	callf	$4,_fpemulate
	REST_FPSTAT
	moval	8(sp),sp	# Pop operand
	tstl	(sp)		# Stack= PSL, PC, return_code
	jneq	_Xarithtrap	# If not OK, emulate F.P. exception
	movab	4(sp),sp	# Else remove return_code and
	rei

SCBVEC(sfexcep):
	CHECK_SFE(4)
	pushl $0
	SAVE_FPSTAT(8)
	TRAP(ASTFLT)

SCBVEC(transflt):
	CHECK_SFE(12)
	bitl	$1,(sp)+
	bneq	tableflt
pageflt:
	SAVE_FPSTAT(8)
	TRAP(PAGEFLT)
tableflt:
	SAVE_FPSTAT(8)
	TRAP(TABLEFLT)

#define REST_STACK	movab 4(sp), sp; REST_FPSTAT; movab 4(sp), sp

alltraps:
	mfpr	$USP,-(sp); 
	callf $4,_trap; mtpr (sp)+,$USP
	incl	_cnt+V_TRAP
	REST_STACK			# pop type, code, an fp stuff
	mtpr	$HIGH,$IPL		## dont go to a higher IPL (GROT)
	rei

SCBVEC(syscall):
	CHECK_SFE(8)
	SAVE_FPSTAT(8)
	pushl	$T_SYSCALL
	mfpr	$USP,-(sp); callf $4,_syscall; mtpr (sp)+,$USP
	incl	_cnt+V_SYSCALL
	REST_STACK			# pop type, code, an fp stuff
	mtpr	$HIGH,$IPL		## dont go to a higher IPL (GROT)
	rei

/*
 * System page table
 */ 
#define	vaddr(x)	((((x)-_Sysmap)/4)*NBPG+SYSTEM)
#define	SYSMAP(mname, vname, npte)			\
_/**/mname:	.globl	_/**/mname;		\
	.space	npte*4;				\
	.globl	_/**/vname;			\
	.set	_/**/vname,vaddr(_/**/mname)

	.data
	.align	2
	SYSMAP(Sysmap	,Sysbase	,SYSPTSIZE	)
	SYSMAP(VMEMbeg	,vmembeg	,0		)
	SYSMAP(VMEMmap	,vmem		,IOSIZE 	)
	SYSMAP(ACE0map	,ace0utl	,(ACEBPTE+1)	)
	SYSMAP(ACE1map	,ace1utl	,(ACEBPTE+1)	)
	SYSMAP(VMEMend	,vmemend	,0		)
	SYSMAP(Usrptmap	,usrpt		,USRPTSIZE	)
	SYSMAP(Forkmap	,forkutl	,UPAGES		)
	SYSMAP(Xswapmap	,xswaputl	,UPAGES		)
	SYSMAP(Xswap2map,xswap2utl	,UPAGES		)
	SYSMAP(Swapmap	,swaputl	,UPAGES		)
	SYSMAP(Pushmap	,pushutl	,UPAGES		)
	SYSMAP(Vfmap	,vfutl		,UPAGES		)
	SYSMAP(VD0map	,vd0utl		,(MAXBPTE+1)	)
	SYSMAP(VD1map	,vd1utl		,(MAXBPTE+1)	)
	SYSMAP(VD2map	,vd2utl		,(MAXBPTE+1)	)
	SYSMAP(VD3map	,vd3utl		,(MAXBPTE+1)	)
	SYSMAP(CYmap	,cyutl		,(TBUFSIZ+1)	)
	SYSMAP(CMAP1	,CADDR1		,1		)
	SYSMAP(CMAP2	,CADDR2		,1		)
	SYSMAP(mmap	,vmmap		,1		)
	SYSMAP(msgbufmap,msgbuf		,MSGBUFPTECNT	)
	SYSMAP(camap	,cabase		,16*CLSIZE	)
	SYSMAP(ecamap	,calimit	,0		)
	SYSMAP(Mbmap	,mbutl		,NMBCLUSTERS*CLSIZE)
eSysmap:
	.globl	_Syssize
	.set	_Syssize,(eSysmap-_Sysmap)/4

	.text
/*
 * Initialization
 *
 * IPL 0x1f; MME 0; scbb, pcbb, sbr, slr, isp, ksp not set
 *
 */

	.align	2
	.globl	start
start:
	.word	0

/* set system control block base and system page table params */

	mtpr	$_scb-SYSTEM,$SCBB
	mtpr	$_Sysmap-SYSTEM,$SBR
	mtpr	$_Syssize,$SLR

/* double map the kernel into the virtual user addresses of phys mem */
/* (to be on the safe side.This is supposed to run in system sace. )  */
	mtpr	$_Sysmap,$P0BR
	mtpr	$_Syssize,$P0LR

	mtpr	$_Sysmap,$P1BR		# Against Murphy
	mtpr	$_Syssize,$P1LR

/* set ISP */
	movl	$_intstack-SYSTEM+NISP*NBPG,sp	# Still physical !
	mtpr	$_intstack+NISP*NBPG,$ISP

/* count up memory */

	clrl	r7
1:	pushl	$1; pushl r7; callf $12,_badaddr; tstl r0; bneq 9f
	addl2	$MEMUNIT,r7
	cmpl	r7,$MAXPHYSMEM
	bleq	1b
9:
/* clear memory from kernel bss and pages for proc 0 u. and page table */
	movab	_edata,r6
	movab	_end,r5
	andl2	$0!SYSTEM,r6
	andl2	$0!SYSTEM,r5
	addl2	$(UPAGES*NBPG)+NBPG+NBPG,r5
1:	clrl	(r6); ACBL( r5,$4,r6,1b)

/* trap() and syscall() save r0-r13 in the entry mask (per ../h/reg.h) */
/* For floating point emulation, we do same for 'fpemulate' */
	orw2	$0x01fff,_trap
	orw2	$0x01fff,_syscall
	orw2	$0x01fff,_fpemulate

/* initialize system page table: scb and int stack writeable */
	clrl	r2
	movab	eintstack,r1 
	andl2	$0!SYSTEM,r1
	shrl 	$PGSHIFT,r1,r1		# r1-page number of eintstack.

/* Start by making the processor storage read/only */

	orl3	$PG_V|PG_KR,r2,_Sysmap[r2]; incl r2;
	orl3	$PG_V|PG_KR,r2,_Sysmap[r2]; incl r2;

/* Other parts of the system are read/write for kernel */

1:	orl3	$PG_V|PG_KW,r2,_Sysmap[r2]; # data:kernel write + phys=virtual
	aoblss r1,r2,1b

/* make rsstk read-only as red zone for interrupt stack */
	andl2	$0!PG_PROT,_rsstkmap
	orl2	$PG_V|PG_KR,_rsstkmap		# Make dump stack r/w

/* make kernel text space read-only */
/*
 * HAVE TO CHECK ALL THE MAGIC CONSTANTS USED HERE : $xxxxxx */

	movab	_etext+NBPG-1,r1
	andl2	$0!SYSTEM,r1
	shrl 	$PGSHIFT,r1,r1
1:	orl3	$PG_V|PG_KR,r2,_Sysmap[r2]
	aoblss r1,r2,1b

/* make kernel data, bss, read-write */
	movab	_end+NBPG-1,r1
	andl2	$0!SYSTEM,r1
	shrl 	$PGSHIFT,r1,r1
1:	orl3	$PG_V|PG_KW,r2,_Sysmap[r2]
	aoblss r1,r2,1b

/* now go to mapped mode */
/* Have to change both PC and SP to system addresses */
	mtpr	$1,$TBIA
	mtpr	$1,$PADC	/* needed by HW parity & ECC logic */
	mtpr	$1,$PACC	/* just in case */
	mtpr 	$1,$MME
	movab	SYSTEM(sp),sp
	.globl	go_virt
go_virt:	
	jmp 	*$0f
0:

/* disable any interrupts */
	movl	$0,_intenable
/* init mem sizes */
	shrl	$PGSHIFT,r7,_maxmem
	movl	_maxmem,_physmem
	movl	_maxmem,_freemem

/* setup context for proc[0] == Scheduler */
	movab	_end-SYSTEM+NBPG-1,r6
	andl2	$0!(NBPG-1),r6		# make page boundary

/* setup page table for proc[0] */
	shrl	$PGSHIFT,r6,r3			# r3 = btoc(r6)
	orl3	$PG_V|PG_KW,r3,_Usrptmap	# init first upt entry
	incl	r3				# r3 - next page
	movab	_usrpt,r0			# r0 - first user page
	mtpr	r0,$TBIS

/* init p0br, p0lr */
	mtpr	r0,$P0BR	# No P0 for proc[0]
	mtpr	$0,$P0LR

	mtpr	r0,$P1BR	# No P1 either
	mtpr	$0,$P1LR


/* init p2br, p2lr */
	movab	NBPG(r0),r0
	movl	$PPAGES-UPAGES,r1
	mtpr	r1,$P2LR
	moval	-4*PPAGES(r0),r2
	mtpr	r2,$P2BR

/* setup mapping for UPAGES of _u */
	clrl	r2; 
	movl 	$SYSTEM,r1
	addl2 	$UPAGES,r3
	jbr 2f
1:	decl	r3
	moval	-NBPG(r1),r1;	# r1 = virtual add of next (downward) _u page
	subl2	$4,r0		# r0 = pte address
	orl3	$PG_V|PG_URKW,r3,(r0)
	mtpr	r1,$TBIS
2:	aobleq	$UPAGES,r2,1b

/* initialize (slightly) the pcb */
	movab	UPAGES*NBPG(r1),PCB_KSP(r1)	# KSP starts at end of _u
	movl	r1,PCB_USP(r1)			# USP starts just below _u
	mfpr	$P0BR,PCB_P0BR(r1)
	mfpr	$P0LR,PCB_P0LR(r1)
	mfpr	$P1BR,PCB_P1BR(r1)
	mfpr	$P1LR,PCB_P1LR(r1)
	mfpr	$P2BR,PCB_P2BR(r1)
	mfpr	$P2LR,PCB_P2LR(r1)
	movl	$CLSIZE,PCB_SZPT(r1)		# init u.u_pcb.pcb_szpt
	movl	r11,PCB_R11(r1)			# r11 obtained from CP on boot
	movab	1f,PCB_PC(r1)			# initial pc
	clrl	PCB_PSL(r1)			# kernel mode, ipl=0
	movw	$0xff,PCB_CKEY(r1)	# give him a code key
	movw	$0xff,PCB_DKEY(r1)	# give him a data key
	shll	$PGSHIFT,r3,r3
	mtpr	r3,$PCBB			# first pcbb (physical ! )

/* Go to a 'normal' process mode (kernel) */

	ldpctx
	rei		# Actually 'returns' to the next instruction:

/* put signal trampoline code in u. area */
1:	movab	_u,r0
	movl	sigcode+0,PCB_SIGC+0(r0)
	movl	sigcode+4,PCB_SIGC+4(r0)
	movl	sigcode+8,PCB_SIGC+8(r0)
	movl	sigcode+12,PCB_SIGC+12(r0)

/* save reboot flags in global _boothowto */
	movl	r11,_boothowto

/* calculate firstaddr, and call main() */
	movab	_end-SYSTEM+NBPG-1,r0
	shrl	$PGSHIFT,r0,-(sp)
	addl2	$UPAGES+1,(sp)		# First physical unused page number
	callf 	$8,_main

/* proc[1] == /etc/init now running here in kernel mode; run icode */
	pushl	$PSL_CURMOD		# User mode PSL
	pushl $0			# PC = 0 (virtual now)
	rei

/* signal trampoline code: it is known that this code takes up to 16    */
/* bytes in pcb.h and in the code above 				*/
/*  The following user stack layout was set up by machdep.c/sendsig 	*/
/*	routine:							*/
/*									*/
/*	+---------------+						*/
/*	|  Last PSL	|\						*/
/*	+---------------+ > for rei					*/
/*  :-->|  Last PC	|/						*/
/*  |	+---------------+						*/
/*  :___|__*   SP	|\						*/
/*	+---------------+ |						*/
/*	|  sigmask	| |						*/
/*	+---------------+  > cleaned by kcall $139 (sigcleanup)		*/
/*  :-->|  u.u_onstack	| |						*/
/*  |	+---------------+ |						*/
/*  :___|_* copy of SCP	|/						*/
/*  |	+---------------+						*/
/*  |	|  Process' r0	| 						*/
/*  |	+---------------+						*/
/*  |	|  Process' r1	|						*/
/*  |	+---------------+						*/
/*  |	| Handler addr. |\						*/
/*  |	+---------------+ |						*/
/*  :___|_*   SCP	| |						*/
/*	+---------------+  > cleaned by ret from calls			*/
/*	|  u.u_code	| |						*/
/*	+---------------+ |						*/
/*	|  signal number|/						*/
/*	+---------------+						*/
/*									*/
/*   * Stack when entering sigcode; setup by sendsig();			*/
/*									*/
	.align	2
sigcode:			# When the process executes this code
				#  (located in its u. structure), it
				#  is in user mode !
	calls	$4*4+4,*12(sp)	# 4 words popped from stack when this call returns
	movl	(sp)+,r1
	movl	(sp)+,r0
	kcall	$139		# Signal cleanup
	rei			# From user mode to user mode !

/*
 * Primitives
 */ 

/*
 * badaddr(addr, len)
 *	see if access addr with a len type instruction causes a machine check
 *	len is length of access (1=byte, 2=short, 4=long)
 *	r0 = 0 means good(exists); r0 =1 means does not exist.
 */
	.globl	_badaddr
_badaddr:
	.word	0x1c	# Keep r4,r3,r2
	mfpr	$IPL,r1
	mtpr	$HIGH,$IPL
	movl	_scb+BERVEC,r2
	movl	4(fp),r3
	movl	8(fp),r4
	movab	9f,_scb+BERVEC
	bbc	$0,r4,1f; tstb	(r3)
1:	bbc	$1,r4,1f; tstw	(r3)
1:	bbc	$2,r4,1f; tstl	(r3)
1:	clrl	r0			# made it w/o machine checks
2:	movl	r2,_scb+BERVEC
	mtpr	r1,$IPL
	ret

	.align	2
9:			# Here we catch buss error (if it comes)
	andl3	4(sp),$ERRCD,r0
	cmpl	r0,$APE
	jneq	1f
	halt			# Address parity error !!!
1:	cmpl	r0,$VBE
	jneq	1f
	halt			# Versabus error
1:
	movl	$1,r0		# Anything else = bad address
	movab	8(sp),sp	# discard buss error trash
	movab	2b,(sp)		# new program counter on stack.
	rei


/*
 * badcyaddr(addr)
 *	see if access tape master controller addr causes a bus error
 *	r0 = 0: no error; r0 = 1: timeout error.
 */
	.globl	_badcyaddr
_badcyaddr:
	.word	0x0c	# Keep r3,r2
	mfpr	$IPL,r1
	mtpr	$HIGH,$IPL
	movl	_scb+BERVEC,r2
	clrl	r3
	movab	9f,_scb+BERVEC
	movob	$-1, *4(fp)
1:	aobleq	$1000, r3, 1b
	clrl	r0			# made it w/o machine checks
2:	movl	r2,_scb+BERVEC
	mtpr	r1,$IPL
	ret

	.align	2
9:			# Here we catch buss error (if it comes)
	andl3	4(sp),$ERRCD,r0
	cmpl	r0,$APE
	jneq	1f
	halt			# Address parity error !!!
1:	cmpl	r0,$VBE
	jneq	1f
	halt			# Versabus error
1:
	movl	$1,r0		# Anything else = timeout
	movab	8(sp),sp	# discard buss error trash
	movab	2b,(sp)		# new program counter on stack.
	rei

_bcopy:	.globl	_bcopy
	.word	0x4		# save r2
				# Called by ovbcopy(from,dst,len)
	movl	4(fp),r0
	movl	8(fp),r1
	movl	12(fp),r2
	movblk
	ret

_ovbcopy:	.globl	_ovbcopy
	.word	0x001c
	movl	4(fp),r0
	movl	8(fp),r1
	movl	12(fp),r2
	cmpl	r0,r1
	bgtru	1f		# normal forward case
	beql	2f		# equal, nothing to do
	addl2	r2,r0		# may be overlapping
	cmpl	r0,r1
	bgtru	3f
	subl2	r2,r0		# normal forward case
1:
	movblk
2:
	ret
3:
	addl2	r2,r1		# overlapping, must do backwards
	subl3	r0,r1,r3
	movl	r2,r4
	jbr	5f
4:
	subl2	r3,r0
	subl2	r3,r1
	movl	r3,r2
	movblk
	subl2	r3,r0
	subl2	r3,r1
	subl2	r3,r4
5:
	cmpl	r4,r3
	jgtr	4b
	movl	r4,r2
	subl2	r2,r0
	subl2	r2,r1
	movblk
	ret

/*
 * bzero (base, count)
 * zero out a block starting at 'base', size 'count'.
 */
_bzero:	
	.globl	_bzero
	.word	0x4
	movl	$1f,r0				# r0 = null source string
	movl	4(fp),r1			# r1 = destination address
	movl	8(fp),r2			# r2 = count
	movs3
	ret
1:
	.byte 0


_copyin:	.globl	_copyin		# the _Copyin subroutine in VAX
					# became _copyin procedure for TAHOE
	.word	0x0004
	movl	12(fp),r0		# copy length
	blss	ersb
	movl	4(fp),r1		# copy user address
	cmpl	$NBPG,r0		# probing one page or less ?
	bgeq	cishort			# yes
ciloop:
	prober	$1,(r1),$NBPG		# bytes accessible ?
	beql	ersb			# no
	addl2	$NBPG,r1		# incr user address ptr
	_ACBL	($NBPG+1,$-NBPG,r0,ciloop)	# reduce count and loop
cishort:
	prober	$1,(r1),r0		# bytes accessible ?
	beql	ersb			# no
	MOVC3	(12(fp),4(fp),8(fp))
	clrl	r0
	ret

ersb:
	movl	$EFAULT,r0
	ret

_copyout:	.globl	_copyout	# the _Copyout subroutine in VAX
					# became _copyout procedure for TAHOE
	.word	0x04
	movl	12(fp),r0		# get count
	blss	ersb
	movl	8(fp),r1		# get user address
	cmpl	$NBPG,r0		# can do in one probew?
	bgeq	coshort			# yes
coloop:
	probew	$1,(r1),$NBPG		# bytes accessible?
	beql	ersb			# no 
	addl2	$NBPG,r1		# increment user address
	_ACBL	($NBPG+1,$-NBPG,r0,coloop)	# reduce count and loop
coshort:
	probew	$1,(r1),r0		# bytes accessible?
	beql	ersb			# no
	MOVC3	(12(fp),4(fp),8(fp))
	clrl	r0
	ret

/*
 * non-local goto's
 */
	.globl	_setjmp			# the _Setjmp subroutine in VAX
					# became _setjmp procedure for TAHOE
_setjmp:
	.word	0x0
	movl	4(fp),r2
	storer	$0x1ff8,(r2)
	addl2	$40,r2
	movl	(fp),(r2)
	addl2	$4,r2
	movab	8(fp),(r2)
	addl2	$4,r2
	movl	-8(fp),(r2)
	clrl	r0
	ret

	.globl	_longjmp		# the _Longjmp subroutine in VAX
					# became _longjmp procedure for TAHOE
_longjmp:
	.word	0x0000
	movl	4(fp),r2
_Longjmp:			# called from swtch
	loadr	$0x3ff8,(r2)
	addl2	$44,r2
	movl	(r2),r1
	addl2	$4,r2
	movab	(sp),r0
	cmpl	r1,r0				# must be a pop
	bgequ	lj2
	pushab	lj1
	callf	$8,_panic
lj2:
	movl	r1,sp
	jmp	*(r2)

lj1:	.asciz	"longjmp"


	.globl	_whichqs
	.globl	_qs
	.globl	_cnt

	.globl	_noproc
	.comm	_noproc,4
	.globl	_runrun
	.comm	_runrun,4

/*
 * The following primitives use the fancy TAHOE instructions.
 * _whichqs tells which of the 32 queues _qs
 * have processes in them.  setrq puts processes into queues, remrq
 * removes them from queues.  The running process is on no queue,
 * other processes are on a queue related to p->p_pri, divided by 4
 * actually to shrink the 0-127 range of priorities into the 32 available
 * queues.
 */

/*
 * setrq(p), using fancy TAHOE instructions.
 *
 * Call should be made at spl8(), and p->p_stat should be SRUN
 */
	.globl	_setrq
_setrq:
	.word	0x4
	movl	4(fp),r0
	tstl	P_RLINK(r0)		## firewall: p->p_rlink must be 0
	beql	set1			##
	pushab	set3			##
	callf	$8,_panic		##
set1:
	movzbl	P_PRI(r0),r1		# put on queue which is p->p_pri / 4
	shar	$2,r1,r1
	shal	$1,r1,r2
	moval	_qs[r2],r2
	insque	(r0),*4(r2)		# at end of queue
	shal	r1,$1,r1
	orl2	r1,_whichqs		# mark queue non-empty
	ret

set3:	.asciz	"setrq"

/*
 * remrq(p), using fancy TAHOE instructions
 *
 * Call should be made at spl8().
 */
	.globl	_remrq
_remrq:
	.word	0x0
	movl	4(fp),r0
	movzbl	P_PRI(r0),r1
	shar	$2,r1,r1
	bbs	r1,_whichqs,rem1
	pushab	rem3			# it wasn't recorded to be on its q
	callf	$8,_panic
rem1:
	remque	(r0)
	bneq	rem2			# q not empty yet
	shal	r1,$1,r1
	mcoml	r1,r1
	andl2	r1,_whichqs		# mark queue empty
rem2:
	clrl	P_RLINK(r0)		## for firewall checking
	ret

rem3:	.asciz	"remrq"

	.globl __insque
__insque:
	.word 0
	insque	*4(fp), *8(fp)
	ret


	.globl __remque
__remque:
	.word 0
	remque	*4(fp)
	ret

/*
 * Masterpaddr is the p->p_addr of the running process on the master
 * processor.  When a multiprocessor system, the slave processors will have
 * an array of slavepaddr's.
 */
	.globl	_masterpaddr
	.data
_masterpaddr:
	.long	0

	.text
sw0:	.asciz	"swtch"
/*
 * swtch(), using fancy TAHOE instructions
 */
	.globl	_swtch
_swtch:
	.word	0x0
	movl	(fp),fp			# prepare for rei
	movl	(sp),4(sp)		# saved pc
	tstl	(sp)+
	movpsl	4(sp)
	movl	$1,_noproc
	clrl	_runrun
mtpr0:	mtpr	$0,$IPL			# must allow interrupts here
sw1:	ffs	_whichqs,r0		# look for non-empty queue
	bgeq	sw1a
	brb	sw1			# this is an idle loop!
sw1a:	mtpr	$0x18,$IPL		# lock out all so _whichqs==_qs
	bbc	r0,_whichqs,mtpr0	# proc moved via lbolt interrupt
	shal	$1,r0,r1
	moval	_qs[r1],r1
	movl	(r1),r2			# r2 = p = highest pri process
	remque	*(r1)
	bvc	sw2			# make sure something was there
sw1b:	pushab	sw0
	callf	$8,_panic
sw2:	bneq	sw3
	shal	r0,$1,r1
	mcoml	r1,r1
	andl2	r1,_whichqs		# no more procs in this queue
sw3:
	clrl	_noproc
	tstl	P_WCHAN(r2)		## firewalls
	bneq	sw1b			##
	movzbl	P_STAT(r2),r3		##
	cmpl	$SRUN,r3		##
	bneq	sw1b			##
	clrl	P_RLINK(r2)		##
	movl	*P_ADDR(r2),r0
	movl	r0,_masterpaddr
	shal	$PGSHIFT,r0,r0		# r0 = pcbb(p)
/*	mfpr	$PCBB,r1		# resume of current proc is easy
 *	cmpl	r0,r1
 */	beql	res0
	incl	_cnt+V_SWTCH
	brb	swresume
/* fall into... */

/*
 * global cache key used if cant use proc index as key
 * (e.g. NPROC>=255)
 * otherwise used for temporary storage of key
 */
	.data
	.globl	_globkey
_globkey:
	.long	0
	.globl	_prevpcb
_prevpcb:
	.long	0
	.text
/*
 * resume(pf)
 */
	.globl	_resume
_resume:
	.word	0x0
	shal	$PGSHIFT,4(fp),r0	# r0 = pcbb(p)
	movl	(fp),fp			# prepare for rei
	movl	(sp)+,4(sp)		# saved pc
	tstl	(sp)+
	movpsl	4(sp)
swresume:
	mtpr	$0x18,$IPL			# no interrupts, please
	movl	_CMAP2,_u+PCB_CMAP2	# yech
	REST_ACC			# restore original accumulator
	svpctx
	mtpr	r0,$PCBB
	ldpctx
	movl	_u+PCB_CMAP2,_CMAP2	# yech
	mtpr	$_CADDR2,$TBIS
res0:
	tstl	_u+PCB_SSWAP
	beql	res1
	movl	_u+PCB_SSWAP,r2
	clrl	_u+PCB_SSWAP
	movab	_Longjmp,(sp)
	clrl	4(sp)			# PSL = kernel mode, IPL=0
res1:
#ifdef GLOBKEY
	aoblss	$256,_globkey,res2	# define new cache key
	mtpr	$0,$PACC		# out of keys, purge all
	mtpr	$0,$PADC
	movl	$1,_globkey
res2:
	mtpr	_globkey,$CCK
#else
res3:
	movzwl	_u+PCB_CKEY,_globkey
	mtpr	_globkey,$CCK
res4:
	movzwl	_u+PCB_DKEY,_globkey
#endif
	mtpr	_globkey,$DCK
	rei

/*
 * {fu,su},{byte,word}
 */
	.globl	_fuiword
	.globl	_fuword
_fuiword:
_fuword:
	.word	0x0
	movl	4(fp), r1
	prober	$1,(r1),$4		# check access
	beql	fserr			# page unreadable
	bitl	$1,r1			# check byte alignment
	bneq	2f			# odd, do byte-word-byte
	bitl	$2,r1			# check word alignment
	bneq	1f			# odd, do in 2 words
	movl	(r1),r0			# move longword
	ret
1:
	movw	(r1),r0			# move two words
	shal	$16,r0,r0
	movzwl	2(r1),r1		# orw2 sign extends
	orl2	r1,r0
	ret
2:
	movb	(r1),r0			# move byte-word-byte
	shal	$24,r0,r0
	movzwl	1(r1),r2		# orw2 sign extends
	shal	$8,r2,r2
	movzbl	3(r1),r1		# orb2 sign extends
	orl2	r2,r1
	orl2	r1,r0
	ret
fserr:
	mnegl	$1,r0
	ret

	.globl	_fuibyte
	.globl	_fubyte
_fuibyte:
_fubyte:
	.word	0x0
	prober	$1,*4(fp),$1
	beql	fserr
	movzbl	*4(fp),r0
	ret

	.globl	_suiword
	.globl	_suword
_suiword:
_suword:
	.word	0x0
	movl	4(fp), r0
	probew	$1,(r0),$4		# check access
	beql	fserr			# page unwritable
	bitl	$1,r0			# check byte alignment
	bneq	1f			# odd byte boundary
	bitl	$2,r0			# check word alignment
	beql	2f			# longword aligned
	movw	8(fp),(r0)		# move two words
	movw	10(fp),2(r0)
	jbr	3f
1:
	movb	8(fp),(r0)
	movb	9(fp),1(r0)
	movb	10(fp),2(r0)
	movb	11(fp),3(r0)
	jbr	3f
2:
	movl	8(fp),(r0)
3:
	clrl	r0
	ret

	.globl	_suibyte
	.globl	_subyte
_suibyte:
_subyte:
	.word	0x0
	probew	$1,*4(fp),$1
	beql	fserr
	movb	11(fp),*4(fp)
	clrl	r0
	ret

/*
 * Copy 1 relocation unit (NBPG bytes)
 * from user virtual address to physical address
 */
_copyseg: 	.globl	_copyseg
	.word	0x4
	orl3	$PG_V|PG_KW,8(fp),_CMAP2
	mtpr	$_CADDR2,$TBIS	# invalidate entry for copy 
	MOVC3	($NBPG,4(fp),$_CADDR2)
	ret

/*
 * clearseg(physical_page_number);
 *
 * zero out physical memory
 * specified in relocation units (NBPG bytes)
 * This routine was optimized for speed on Tahoe.
 */
_clearseg: 	.globl	_clearseg
	.word	0
	orl3	$PG_V|PG_KW,4(fp),_CMAP1	# Maps to virtual addr CADDR1
	mtpr	$_CADDR1,$TBIS
	movl	$255,r0				# r0 = limit
	clrl	r1				# r1 = index of cleared long
1:
	clrl	_CADDR1[r1]
	aobleq	r0,r1,1b
	ret

/*
 * if ( useracc(address, count, mode) ) ....
 * Check address.
 * Given virtual address, byte count, and rw flag
 * returns 0 on no access.
 * Note : it is assumed that on all calls to this routine,
 *  mode=0 means write access, mode=1 means read access.
 */
_useracc:	.globl	_useracc
	.word	0x4
	movl	$1,r2			# r2 = 'user mode' for probew/probew
probes:
	movl	4(fp),r0		# get va
	movl	8(fp),r1		# count
	tstl	12(fp)			# test for read access ?
	bneq	userar			# yes
	cmpl	$NBPG,r1			# can we do it in one probe ?
	bgeq	uaw2			# yes
uaw1:
	probew	r2,(r0),$NBPG
	beql	uaerr			# no access
	addl2	$NBPG,r0
	_ACBL($NBPG+1,$-NBPG,r1,uaw1)
uaw2:
	probew	r2,(r0),r1
	beql	uaerr
	movl	$1,r0
	ret

userar:
	cmpl	$NBPG,r1
	bgeq	uar2
uar1:
	prober	r2,(r0),$NBPG
	beql	uaerr
	addl2	$NBPG,r0
	_ACBL($NBPG+1,$-NBPG,r1,uar1)
uar2:
	prober	r2,(r0),r1
	beql	uaerr
	movl	$1,r0
	ret
uaerr:
	clrl	r0
	ret

/*
 * if ( kernacc(address, count, mode) ) ....
 * Check address.
 * Given virtual address, byte count, and rw flag
 * returns 0 on no access.
 * Same as useracc routine but checks for kernel access rights.
 */

_kernacc:	.globl	_kernacc
	.word	0x4
	clrl	r2		# r2 = 0 means kernel mode probe.
	jbr	probes		# Dijkstra would get gastric distress here.

/*
 * addupc - increment some histogram counter
 *	in the profiling buffer
 *
 * addupc(pc, prof, counts)
 * long	pc , counts;	Only least significant word of 'counts' is added.
 * struct	uprof *prof;
 * 
 * struct uprof {		# profile arguments 
 * 	short	*r_base;	# buffer base 
 * 	unsigned pr_size;	# buffer size 
 * 	unsigned pr_off;	# pc offset 
 * 	unsigned pr_scale;	# pc scaling 
 * }
 */
	.globl	_addupc
_addupc:
	.word	4
	movl	8(fp),r2	# r2 points to structure
	subl3	8(r2),4(fp),r0	# r0 = PC - lowpc
	jlss	9f		# PC < lowpc , out of range !
	shrl	$1,r0,r0	# the unit is words
	shrl	$1,12(r2),r1	# ditto for scale
	emul	r1,r0,$0,r0
	shrq	$14,r0,r0
	tstl	r0		# too big
	jneq	9f
	cmpl	r1,4(r2)	# Check buffer overflow
	jgequ	9f
	probew	$1,*0(r2)[r1],$2	# counter accessible?
	jeql	9f
	shrl	$1,r1,r1	# make r1 word index
	addw2	14(fp),*0(r2)[r1]
9:	ret
