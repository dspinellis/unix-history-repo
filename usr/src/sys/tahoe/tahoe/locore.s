/*	locore.s	1.22	88/01/27	*/

#include "../tahoe/mtpr.h"
#include "../tahoe/trap.h"
#include "../tahoe/psl.h"
#include "../tahoe/pte.h"
#include "../tahoe/cp.h"
#include "../tahoe/mem.h"
#include "../tahoe/SYS.h"
#include "../tahoemath/fp.h"

#include "errno.h"
#include "syscall.h"
#include "cmap.h"

	.set	HIGH,0x1f		# mask for total disable
	.set	NISP,3			# number of interrupt stack pages
	.set	SYSTEM,0xC0000000	# virtual address of system start
	.set	PPAGES,0x100000  	# possible pages in P0,P1, etc.

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

#define	MOVC3(_srcaddr,_dstaddr,_len) \
	movl	_srcaddr,r0; \
	movl	_dstaddr,r1; \
	movl	_len,r2; \
	movblk

/* keep address of psl if coming from user mode */
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
	.globl	pwfl_stk	
_rsstk:
	.space	1024-8
pwfl_stk:
	.space	4
dumpflag:
	.space	4

	.globl	_intstack
_intstack:
	.space	NISP*NBPG
eintstack:

/*
 * Power failure storage block and
 * macros for saving and restoring.
 */
#define	POWERFAIL(id,longs) \
	.globl	pwfl_/**/id \
pwfl_/**/id: .space longs*4
	.data
	POWERFAIL(r0,	14)		# r0-r13
	POWERFAIL(sp,	1)		# r14
	POWERFAIL(SCBB,	1)		# system control block base
	POWERFAIL(SBR,	1)		# system pte base
	POWERFAIL(SLR,	1)		# system pte length
	POWERFAIL(P0BR,	1)		# p0 pte base
	POWERFAIL(P0LR,	1)		# p0 pte length
	POWERFAIL(P1BR,	1)		# p1 pte base
	POWERFAIL(P1LR,	1)		# p1 pte length
	POWERFAIL(P2BR,	1)		# p2 pte base
	POWERFAIL(P2LR,	1)		# p2 pte length
	POWERFAIL(IPL,	1)		# interrupt priority level
	POWERFAIL(DCK,	1)		# data cache key
	POWERFAIL(CCK,	1)		# code cache key
	POWERFAIL(PCBB,	1)		# process control block base
	POWERFAIL(ISP,	1)		# interrupt stack pointer
	POWERFAIL(KSP,	1)		# kernel mode stack pointer
	POWERFAIL(USP,	1)		# user mode stack pointer
	POWERFAIL(MME,	1)		# memory management enable
	POWERFAIL(PSL,	1)		# processor status longword

/*
 * Save current state in power fail storage block.
 */
#define	SAVEpwfl() \
	movpsl	pwfl_PSL	# Keeps all flags, etc. \
	storer	$0x3fff,pwfl_r0	# Saves r0-r13 \
	moval	0(sp),pwfl_sp	# Saves sp (=r14) \
	mfpr	$SBR,pwfl_SBR	# Save all re_loadable registers \
	mfpr	$SLR,pwfl_SLR \
	mfpr	$P0BR,pwfl_P0BR \
	mfpr	$P0LR,pwfl_P0LR \
	mfpr	$P1BR,pwfl_P1BR \
	mfpr	$P1LR,pwfl_P1LR \
	mfpr	$P2BR,pwfl_P2BR \
	mfpr	$P2LR,pwfl_P2LR \
	mfpr	$IPL,pwfl_IPL \
	mfpr	$MME,pwfl_MME \
	mfpr	$DCK,pwfl_DCK \
	mfpr	$CCK,pwfl_CCK \
	mfpr	$PCBB,pwfl_PCBB \
	mfpr	$ISP,pwfl_ISP \
	mfpr	$SCBB,pwfl_SCBB \
	mfpr	$KSP,pwfl_KSP \
	mfpr	$USP,pwfl_USP

/*
 * Restore state saved in power fail block and
 * jmp to location specified after (possibly)
 * enabling memory management.
 */
#define	RESTOREpwfl(loc) \
	loadr	$0x3fff,pwfl_r0	# Restore r0-r13 \
	movl	pwfl_sp,sp	# Restore sp (=r14) \
	mtpr	pwfl_SCBB,$SCBB \
	mtpr	pwfl_SBR,$SBR	# Restore all re_loadable registers \
	mtpr	pwfl_SLR,$SLR \
	mtpr	pwfl_P0BR,$P0BR \
	mtpr	pwfl_P0LR,$P0LR \
	mtpr	pwfl_P1BR,$P1BR \
	mtpr	pwfl_P1LR,$P1LR \
	mtpr	pwfl_P2BR,$P2BR \
	mtpr	pwfl_P2LR,$P2LR \
	mtpr	pwfl_IPL,$IPL \
	mtpr	pwfl_DCK,$DCK \
	mtpr	pwfl_CCK,$CCK \
	mtpr	pwfl_PCBB,$PCBB \
	mtpr	pwfl_ISP,$ISP \
	mtpr	pwfl_KSP,$KSP \
	mtpr	pwfl_USP,$USP \
\
	bicpsw	$0xff		# Restore PSW. \
	bispsw	pwfl_PSL+2	# Set original bits back (just in case..) \
# now go to mapped mode \
# Have to change PC to system addresses \
	mtpr	$1,$PACC	# Thoroughly clean up caches. \
	mtpr	$1,$PADC \
	mtpr	$1,$TBIA \
	mtpr	pwfl_MME,$MME  	# Restore MME. Last thing to be done. \
	jmp 	loc

/*
 * Do a dump.
 * Called by auto-restart.
 * May be called manually.
 */
	.align	2
	.text
	.globl	_Xdoadump
	.globl	_doadump
_Xdoadump:					# CP comes here after power fail
	RESTOREpwfl(*0f)			# restore state
_doadump:
	.word 0
0:	mtpr	$HIGH,$IPL
#define	_rsstkmap _Sysmap+12	# powerfail storage, scb, rsstk, int stack
	tstl	dumpflag			# dump only once!
	bneq	1f
	andl2	$~PG_PROT,_rsstkmap
	orl2	$PG_KW,_rsstkmap		# Make dump stack r/w
	mtpr	$0,$TBIA
	movl	$1,dumpflag
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
#define	PANIC(msg) \
	clrl _waittime; pushab 1f; callf $8,_panic; 1: .asciz msg
#define	PRINTF(n,msg) \
	pushab 1f; callf $(n+2)*4,_printf; MSG(msg)
#define	MSG(msg) .data; 1: .asciz msg; .text
/*
 * r0-r2 are saved across all faults and interrupts.
 * Routines below and those hidden in vbglue.s (device
 * interrupts) invoke the PUSHR/POPR macros to execute
 * this.  Also, certain stack frame offset calculations
 * (such as in hardclock) understand this, using the
 * REGSPC definition (and FPSPC defined below).
 * Finally, many routines, including those expanded
 * inline depend on this!  Should probably save all
 * live C compiler temp registers to eliminate potentially
 * grievous problems caused by incorrect register save masks.
 */
#define	REGSPC	(3*4)
#define	PUSHR	pushl r0; pushl r1; pushl r2;
#define	POPR	movl (sp)+,r2; movl (sp)+,r1; movl (sp)+,r0;

/*
 * Floating point state is saved across faults and
 * interrupts.  The state occupies 4 longwords on
 * the stack:
 *	precision indicator (single = 0/double = 1)
 *	double representation of accumulator
 *	save accumulator status flag (pcb_savacc)
 */
#define	FPSPC	(4*4)

#define SAVE_FPSTAT(_delta) \
	bitl	$PSL_DBL,_delta(sp); \
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

#define REST_FPSTAT \
	tstl	(sp)+; \
	bitl	$2,8(sp);\
	beql	1f;\
	movl	$0,_u+PCB_SAVACC; \
1:	bitl	$1,8(sp); \
	beql	2f; \
	ldd	(sp); \
	jmp	3f; \
2:	ldf	(sp); \
3:	moval	12(sp),sp;

#define REST_ACC \
	tstl	_u+PCB_SAVACC; \
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

	.data
nofault: .space	4			# bus error non-local goto label

	.text
SCBVEC(buserr):
	CHECK_SFE(12)
	SAVE_FPSTAT(12)
	incl	_intrcnt+I_BUSERR	# keep stats...
	pushl	r0			# must save
	andl3	24(sp),$ERRCD,r0	# grab pushed MER value
	cmpl	r0,$APE			# address parity error?
	jneq	1f
	halt	
1:	cmpl	r0,$VBE			# versabus error?
	jneq	2f
	halt
2:
	movl	(sp)+,r0		# restore r0 and...
	bitl	$PSL_CURMOD,4*4+3*4(sp)	# check if happened in user mode?
	jeql	3f			# yes, then shift stack up for trap...
	movl	12(sp),16(sp)		# sorry, no space for which-buss...
	movl	8(sp),12(sp)
	movl	4(sp),8(sp)
	movl	0(sp),4(sp)
	movl	$T_BUSERR,0(sp)		# push trap type code and...
	jbr	alltraps		# ...merge with all other traps
3:					# kernel mode, check to see if...
	tstl	nofault			# ...doing peek/poke?
	jeql	4f			# nofault set? if so, jump to it...
	movl	nofault,4*4+2*4(sp)	# ...setup for non-local goto
	clrl	nofault
	jbr	5f
4:
	PUSHR
	pushab	7*4(sp)			# address of bus error parameters
	callf	$8,_buserror
	POPR
5:
	REST_FPSTAT
	movab	8(sp),sp		# remove bus error parameters
	rei

SCBVEC(powfail):			# We should be on interrupt stack now.
	SAVEpwfl()			# save machine state
	moval	_Xdoadump-SYSTEM,_scb+SCB_DOADUMP
	halt

SCBVEC(stray):
	incl	_cnt+V_INTR		# add to statistics
	rei

#include "../net/netisr.h"
	.globl	_netisr
SCBVEC(netintr):
	CHECK_SFE(4)
	SAVE_FPSTAT(4); PUSHR
#include "imp.h"
#if NIMP > 0
	bbc	$NETISR_IMP,_netisr,1f;
	andl2	$~(1<<NETISR_IMP),_netisr
	callf	$4,_impintr;
1:
#endif
#ifdef INET
	bbc	$NETISR_IP,_netisr,1f	
	andl2	$~(1<<NETISR_IP),_netisr
	callf	$4,_ipintr	
1:
#endif
#ifdef NS
	bbc	$NETISR_NS,_netisr,1f	
	andl2	$~(1<<NETISR_NS),_netisr
	callf	$4,_nsintr	
1:
#endif
	bbc	$NETISR_RAW,_netisr,1f
	andl2	$~(1<<NETISR_RAW),_netisr	
	callf	$4,_rawintr	
1:
	incl	_cnt+V_SOFT
	POPR; REST_FPSTAT
	rei

SCBVEC(cnrint):
	CHECK_SFE(4)
	SAVE_FPSTAT(4); PUSHR; 
	pushl $CPCONS; callf $8,_cnrint;
	incl	_intrcnt+I_CNR
	incl	_cnt+V_INTR
	POPR; REST_FPSTAT;
	rei
SCBVEC(cnxint):
	CHECK_SFE(4)
	SAVE_FPSTAT(4); PUSHR; 
	pushl $CPCONS; callf $8,_cnxint;
	incl	_intrcnt+I_CNX
	incl	_cnt+V_INTR
	POPR; REST_FPSTAT;
	rei
SCBVEC(rmtrint):
	CHECK_SFE(4)
	SAVE_FPSTAT(4); PUSHR; 
	pushl $CPREMOT; callf $8,_cnrint;
	incl	_intrcnt+I_RMTR
	incl	_cnt+V_INTR
	POPR; REST_FPSTAT;
	rei
SCBVEC(rmtxint):
	CHECK_SFE(4)
	SAVE_FPSTAT(4); PUSHR; 
	pushl $CPREMOT; callf $8,_cnxint;
	incl	_intrcnt+I_RMTX
	incl	_cnt+V_INTR
	POPR; REST_FPSTAT;
	rei

#define PUSHPCPSL	pushl 4+FPSPC+REGSPC(sp); pushl 4+FPSPC+REGSPC(sp);

SCBVEC(hardclock):
	tstl	_clk_enable
	bneq	1f
	rei
1:
	CHECK_SFE(4)
	SAVE_FPSTAT(4); PUSHR
	PUSHPCPSL			# push pc and psl
	callf	$12,_hardclock		# hardclock(pc,psl)
	incl	_intrcnt+I_CLOCK
	incl	_cnt+V_INTR		## temp so not to break vmstat -= HZ
	POPR; REST_FPSTAT
	rei
SCBVEC(softclock):
	CHECK_SFE(4)
	SAVE_FPSTAT(4); PUSHR;
	PUSHPCPSL				# push pc and psl
	callf	$12,_softclock			# softclock(pc,psl)
	incl	_cnt+V_SOFT
	POPR; REST_FPSTAT
	rei

/*
 * Stray VERSAbus interrupt catch routines
 */
	.data
#define	PJ	.align 2; callf $4,_Xvstray
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
	PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ
	PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ
	PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ
	PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ;PJ

	.align	2
	.globl	_cold
_cold:	.long	0x3

	.text
SCBVEC(vstray):
	.word	0
	bbc	$0,_cold,2f		# system running?
	bbc	$1,_cold,1f		# doing autoconfig?
	jbr	3f			# random interrupt, ignore
1:
	mfpr	$IPL,r12		# ...setup br and cvec
	subl3	$_catcher+7,-8(fp),r11; shar $3,r11,r11
	addl2	$SCB_DEVBASE,r11
	jbr	3f
2:
	PUSHR
	subl3	$_catcher+7,-8(fp),r0; shar $3,r0,r0
	addl3	$SCB_DEVBASE,r0,-(sp);
	mfpr	$IPL,-(sp)
	PRINTF(2, "stray intr ipl %x vec %x\n")
	POPR
3:	moval	0f,-8(fp); ret		# pop callf frame...
0:	rei				# ...and return

/*
 * Trap and fault vector routines
 */ 
#define	TRAP(a)	pushl $T_/**/a; jbr alltraps

/*
 * Ast delivery (profiling and/or reschedule)
 */

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
SCBVEC(kdbintr):
	CHECK_SFE(4);
	pushl $0;
	SAVE_FPSTAT(8);
	TRAP(KDBTRAP);
SCBVEC(tracep):
	CHECK_SFE(4)
	pushl $0;
	SAVE_FPSTAT(8)
	TRAP(TRCTRAP)
SCBVEC(alignflt):
#ifdef ALIGN
	bitl	$PSL_CURMOD,4(sp)
	jeql	align_excp		# Can't emulate for kernel mode !
	jbr	non_aligned		# Only emulated for user mode.
align_excp:
#else
	CHECK_SFE(4)
#endif
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

SCBVEC(fpm):			# Floating Point Emulation
#ifdef FPE
	CHECK_SFE(16)
	SAVE_FPSTAT(16)
	incl	_cnt+V_FPE	# count emulation traps
	callf	$4,_fpemulate
	REST_FPSTAT
#endif
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
	bitl	$2,(sp)+
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
	callf	$4,_trap;
	mtpr	(sp)+,$USP
	incl	_cnt+V_TRAP
	REST_STACK			# pop type, code, and fp stuff
	mtpr	$HIGH,$IPL		## dont go to a higher IPL (GROT)
	rei

SCBVEC(syscall):
	CHECK_SFE(8)
	SAVE_FPSTAT(8)
	pushl	$T_SYSCALL
	mfpr	$USP,-(sp);
	callf	$4,_syscall;
	mtpr	(sp)+,$USP
	incl	_cnt+V_SYSCALL
	REST_STACK			# pop type, code, and fp stuff
	mtpr	$HIGH,$IPL		## dont go to a higher IPL (GROT)
	rei

/*
 * System page table.
 *
 * Mbmap and Usrptmap are enlarged by CLSIZE entries
 * as they are managed by resource maps starting with index 1 or CLSIZE.
 */ 
#define	vaddr(x)	((((x)-_Sysmap)/4)*NBPG+SYSTEM)
#define	SYSMAP(mname, vname, npte)			\
_/**/mname:	.globl	_/**/mname;		\
	.space	(npte)*4;			\
	.globl	_/**/vname;			\
	.set	_/**/vname,vaddr(_/**/mname)

	.data
	.align	2
	SYSMAP(Sysmap	,Sysbase	,SYSPTSIZE	)
	SYSMAP(Forkmap	,forkutl	,UPAGES		)
	SYSMAP(Xswapmap	,xswaputl	,UPAGES		)
	SYSMAP(Xswap2map,xswap2utl	,UPAGES		)
	SYSMAP(Swapmap	,swaputl	,UPAGES		)
	SYSMAP(Pushmap	,pushutl	,UPAGES		)
	SYSMAP(Vfmap	,vfutl		,UPAGES		)
	SYSMAP(CMAP1	,CADDR1		,1		)
	SYSMAP(CMAP2	,CADDR2		,1		)
	SYSMAP(mmap	,vmmap		,1		)
	SYSMAP(alignmap	,alignutl	,1		)	/* XXX */
	SYSMAP(msgbufmap,msgbuf		,MSGBUFPTECNT	)
	SYSMAP(Mbmap	,mbutl		,NMBCLUSTERS*CLSIZE+CLSIZE )
	SYSMAP(camap	,cabase		,16*CLSIZE	 )
#ifdef	GPROF
	SYSMAP(profmap	,profbase	,600*CLSIZE	)
#endif
	/*
	 * Enlarge kmempt as needed for bounce buffers allocated
	 * by tahoe controllers.
	 */
#include "dk.h"
	SYSMAP(_vdmap	,_vdbase	,NVD*(MAXPHYS/NBPG+CLSIZE) )
#include "yc.h"
#include "yc.h"
	SYSMAP(_cymap	,_cybase	,NCY*(MAXPHYS/NBPG+CLSIZE) )
	SYSMAP(ecamap	,calimit	,0		)
	SYSMAP(ekmempt	,kmemlimit	,0		)
	SYSMAP(VMEMbeg	,vmembeg	,0		)
	SYSMAP(VMEMmap	,vmem		,VBIOSIZE 	)
	SYSMAP(VMEMmap1	,vmem1		,0		)
#include "ace.h"
	SYSMAP(_acemap1	,_acemem	,NACE*32	)
	SYSMAP(VMEMend	,vmemend	,0		)
	SYSMAP(VBmap	,vbbase		,CLSIZE		)
	SYSMAP(_vdbmap	,_vdbbase	,NVD*(MAXPHYS/NBPG+CLSIZE) )
	SYSMAP(_cybmap	,_cybbase	,NCY*(MAXPHYS/NBPG+CLSIZE) )
	SYSMAP(eVBmap	,vbend		,0		)
	SYSMAP(Usrptmap	,usrpt		,USRPTSIZE+CLSIZE )
eSysmap:
	.globl	_Syssize
	.set	_Syssize,(eSysmap-_Sysmap)/4

	.text
/*
 * Initialization
 *
 * IPL 0x1f; MME 0; scbb, pcbb, sbr, slr, isp, ksp not set
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
	mtpr	$_Sysmap,$P0BR
	mtpr	$_Syssize,$P0LR
	mtpr	$_Sysmap,$P1BR			# against Murphy
	mtpr	$_Syssize,$P1LR
/* set ISP */
	movl	$_intstack-SYSTEM+NISP*NBPG,sp	# still physical
	mtpr	$_intstack+NISP*NBPG,$ISP
/* count up memory */
	clrl	r7
1:	pushl	$1; pushl r7; callf $12,_badaddr; tstl r0; bneq 9f
	ACBL($MAXMEM*1024-1,$64*1024,r7,1b)
9:
/* clear memory from kernel bss and pages for proc 0 u. and page table */
	movab	_edata,r6; andl2 $~SYSTEM,r6
	movab	_end,r5; andl2 $~SYSTEM,r5
#ifdef KDB
	subl2	$4,r5
1:	clrl	(r6); ACBL(r5,$4,r6,1b)		# clear just bss
	addl2	$4,r5
	bbc	$6,r11,0f			# check RB_KDB
	andl3	$~SYSTEM,r9,r5			# skip symbol & string tables
	andl3	$~SYSTEM,r9,r6
#endif
0:	orl3	$SYSTEM,r5,r9			# convert to virtual address
	addl2	$NBPG-1,r9			# roundup to next page
	addl2	$(UPAGES*NBPG)+NBPG+NBPG,r5
1:	clrl	(r6); ACBL(r5,$4,r6,1b)
/* trap(), syscall(), and fpemulate() save r0-r12 in the entry mask */
	orw2	$0x01fff,_trap
	orw2	$0x01fff,_syscall
#ifdef FPE
	orw2	$0x01fff,_fpemulate
#endif
	orw2	$0x01ffc,_panic			# for debugging (no r0|r1)
	callf	$4,_fixctlrmask			# setup for autoconfig
/* initialize system page table: scb and int stack writeable */
	clrl	r2
	movab	eintstack,r1 
	andl2	$~SYSTEM,r1
	shrl 	$PGSHIFT,r1,r1			# r1-page number of eintstack
/* make 1st page processor storage read/only, 2nd read/write */
	orl3	$PG_V|PG_KR,r2,_Sysmap[r2]; incl r2;
	orl3	$PG_V|PG_KW,r2,_Sysmap[r2]; incl r2;
/* other parts of the system are read/write for kernel */
1:	orl3	$PG_V|PG_KW,r2,_Sysmap[r2];	# data:kernel write+phys=virtual
	aoblss r1,r2,1b
/* make rsstk read-only as red zone for interrupt stack */
	andl2	$~PG_PROT,_rsstkmap
	orl2	$PG_V|PG_KR,_rsstkmap
/* make kernel text space read-only */
	movab	_etext+NBPG-1,r1
	andl2	$~SYSTEM,r1
	shrl 	$PGSHIFT,r1,r1
1:	orl3	$PG_V|PG_KR,r2,_Sysmap[r2]
	aoblss r1,r2,1b
/* make kernel data, bss, read-write */
	andl3	$~SYSTEM,r9,r1
	shrl 	$PGSHIFT,r1,r1
1:	orl3	$PG_V|PG_KW,r2,_Sysmap[r2]
	aoblss r1,r2,1b
/* go to mapped mode, have to change both pc and sp to system addresses */
	mtpr	$1,$TBIA
	mtpr	$1,$PADC			# needed by HW parity&ECC logic
	mtpr	$1,$PACC			# just in case
	mtpr 	$1,$MME
	movab	SYSTEM(sp),sp
	jmp 	*$0f
0:
/* disable any interrupts */
	movl	$0,_intenable
/* init mem sizes */
	shrl	$PGSHIFT,r7,_maxmem
	movl	_maxmem,_physmem
	movl	_maxmem,_freemem
/* setup context for proc[0] == scheduler */
	andl3	$~SYSTEM,r9,r6			# convert to physical
	andl2	$~(NBPG-1),r6			# make page boundary
/* setup page table for proc[0] */
	shrl	$PGSHIFT,r6,r3			# r3 = btoc(r6)
	orl3	$PG_V|PG_KW,r3,_Usrptmap	# init first upt entry
	incl	r3				# r3 - next page
	movab	_usrpt,r0			# r0 - first user page
	mtpr	r0,$TBIS
/* init p0br, p0lr */
	mtpr	r0,$P0BR			# no p0 for proc[0]
	mtpr	$0,$P0LR
	mtpr	r0,$P1BR			# no p1 either
	mtpr	$0,$P1LR
/* init p2br, p2lr */
	movab	NBPG(r0),r0
	movl	$PPAGES-UPAGES,r1
	mtpr	r1,$P2LR
	moval	-4*PPAGES(r0),r2
	mtpr	r2,$P2BR
/* setup mapping for UPAGES of _u */
	clrl	r2
	movl 	$SYSTEM,r1
	addl2 	$UPAGES,r3
	jbr 2f
1:	decl	r3
	moval	-NBPG(r1),r1	# r1 = virtual add of next (downward) _u page
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
	movl	r9,PCB_R9(r1)			# r9 obtained from boot
	movl	r10,PCB_R10(r1)			# r10 obtained from boot
	movl	r11,PCB_R11(r1)			# r11 obtained from CP on boot
	movab	1f,PCB_PC(r1)			# initial pc
	clrl	PCB_PSL(r1)			# kernel mode, ipl=0
	shll	$PGSHIFT,r3,r3
	mtpr	r3,$PCBB			# first pcbb (physical)
/* go to kernel mode */
	ldpctx
	rei					# Actually next instruction:
/* put signal trampoline code in u. area */
1:	movab	sigcode,r0
	movab	_u+PCB_SIGC,r1
	movl	$19,r2
	movblk
/* save boot device in global _bootdev */
	movl	r10,_bootdev
/* save reboot flags in global _boothowto */
	movl	r11,_boothowto
/* save end of symbol & string table in global _bootesym */
	subl3	$NBPG-1,r9,_bootesym
/* calculate firstaddr, and call main() */
	andl3	$~SYSTEM,r9,r0
	shrl	$PGSHIFT,r0,-(sp)
	addl2	$UPAGES+1,(sp)			# first physical unused page
	callf 	$8,_main
/* proc[1] == /etc/init now running here in kernel mode; run icode */
	pushl	$PSL_CURMOD			# User mode PSL
	pushl $0				# PC = 0 (virtual now)
	rei

/*
 * Mask for saving/restoring registers on entry to
 * a user signal handler.  Space for the registers
 * is reserved in sendsig, so beware if you want
 * to change the mask.
 */
#define	SIGREGS	(R0|R1|R2|R3|R4|R5)
	.align	2
	.globl	sigcode
sigcode:
	storer	$SIGREGS,16(sp)	# save volatile registers
	calls	$4*3+4,*12(sp)	# params pushed by sendsig for handler
	loadr	$SIGREGS,4(sp)	# restore volatile registers
	movab	24(sp),fp	# use parameter list set up in sendsig
	kcall	$SYS_sigreturn	# cleanup mask and onsigstack
	halt			# sigreturn does not return!

	.globl	_icode
	.globl	_initflags
	.globl	_szicode
/*
 * Icode is copied out to process 1 to exec /etc/init.
 * If the exec fails, process 1 exits.
 */
	.align	2
_icode:
	pushab	b`argv-l0(pc)
l0:	pushab	b`init-l1(pc)
l1:	pushl	$2
	movab	(sp),fp
	kcall	$SYS_execv
	pushl	r0
	kcall	$SYS_exit

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

/*
 * badaddr(addr, len)
 *	see if access addr with a len type instruction causes a machine check
 *	len is length of access (1=byte, 2=short, 4=long)
 *	r0 = 0 means good(exists); r0 =1 means does not exist.
 */
ENTRY(badaddr, R3|R4)
	mfpr	$IPL,r1
	mtpr	$HIGH,$IPL
	movl	_scb+SCB_BUSERR,r2
	movl	4(fp),r3
	movl	8(fp),r4
	movab	9f,_scb+SCB_BUSERR
	bbc	$0,r4,1f; tstb	(r3)
1:	bbc	$1,r4,1f; tstw	(r3)
1:	bbc	$2,r4,1f; tstl	(r3)
1:	clrl	r0
2:	movl	r2,_scb+SCB_BUSERR
	mtpr	r1,$IPL
	ret

	.align	2
9:				# catch buss error (if it comes)
	andl3	4(sp),$ERRCD,r0
	cmpl	r0,$APE
	jneq	1f
	halt			# address parity error
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
ENTRY(badcyaddr, 0)
	mfpr	$IPL,r1
	mtpr	$HIGH,$IPL
	clrl	r2
	movab	2f,nofault
	movob	$-1, *4(fp)
1:	aobleq	$1000, r2, 1b
	clrl	nofault			# made it w/o bus error
	clrl	r0
	jbr	3f
2:	movl	$1,r0
3:	mtpr	r1,$IPL
	ret

/*
 * peek(addr)
 *	fetch word and catch any bus error
 */
ENTRY(peek, 0)
	mfpr	$IPL,r1
	mtpr	$0x18,$IPL	# not reentrant
	movl	4(fp),r2
	movab	1f,nofault
	movw	(r2),r0
	clrl	nofault
	andl2	$0xffff,r0
	jbr	2f
1:	movl	$-1,r0		# bus error
2:	mtpr	r1,$IPL
	ret

/*
 * poke(addr, val)
 *	write word and catch any bus error
 */
ENTRY(poke, R3)
	mfpr	$IPL,r1
	mtpr	$0x18,$IPL	# not reentrant
	movl	4(fp),r2
	movl	8(fp),r3
	clrl	r0
	movab	1f,nofault
	movw	r3,(r2)
	clrl	nofault
	jbr	2f
1:	movl	$-1,r0		# bus error
2:	mtpr	r1,$IPL
	ret

/*
 * Copy a potentially overlapping block of memory.
 *
 * ovbcopy(src, dst, count)
 *	caddr_t src, dst; unsigned count;
 */
ENTRY(ovbcopy, R3|R4)
	movl	4(fp),r0
	movl	8(fp),r1
	movl	12(fp),r2
	cmpl	r0,r1
	bgtru	1f			# normal forward case
	beql	2f			# equal, nothing to do
	addl2	r2,r0			# may be overlapping
	cmpl	r0,r1
	bgtru	3f
	subl2	r2,r0			# normal forward case
1:
	movblk
2:
	ret
3:
	addl2	r2,r1			# overlapping, must do backwards
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
 * Copy a null terminated string from the user address space into
 * the kernel address space.
 *
 * copyinstr(fromaddr, toaddr, maxlength, &lencopied)
 */
ENTRY(copyinstr, 0)
	movl	12(fp),r5		# r5 = max length
	jlss	5f
	movl	8(fp),r4		# r4 = kernel address
	movl	4(fp),r0		# r0 = user address
	andl3	$(NBPG*CLSIZE-1),r0,r2	# r2 = bytes on first page
	subl3	r2,$(NBPG*CLSIZE),r2
1:
	cmpl	r5,r2			# r2 = min(bytes on page, length left);
	jgeq	2f
	movl	r5,r2
2:
	prober	$1,(r0),r2		# bytes accessible?
	jeql	5f
	subl2	r2,r5			# update bytes left count
	movl	r2,r3			# r3 = saved count
	movl	r0,r1
	cmps3				# check for null
	tstl	r2
	jneq	3f
	subl2	r3,r0			# back up r0
	movl	r4,r1
	movl	r3,r2
	movblk				# copy in next piece
	movl	r1,r4
	movl	$(NBPG*CLSIZE),r2	# check next page
	tstl	r5			# run out of space?
	jneq	1b
	movl	$ENOENT,r0		# set error code and return
	jbr	6f
3:
	tstl	16(fp)			# return length?
	beql	4f
	subl3	r5,12(fp),r5		# actual len = maxlen - unused pages
	subl2	r2,r5			#	- unused on this page
	addl3	$1,r5,*16(fp)		#	+ the null byte
4:
	movl	r4,r1
	subl3	r2,r3,r2		# calc char cnt
	subl2	r2,r0			# back up r0
	incl	r2			# add on null byte
	movblk				# copy last piece
	clrl	r0
	ret
5:
	movl	$EFAULT,r0
6:
	tstl	16(fp)
	beql	7f
	subl3	r5,12(fp),*16(fp)
7:
	ret

/*
 * Copy a null terminated string from the kernel
 * address space to the user address space.
 *
 * copyoutstr(fromaddr, toaddr, maxlength, &lencopied)
 */
ENTRY(copyoutstr, 0)
	movl	12(fp),r5		# r5 = max length
	jlss	5f
	movl	4(fp),r0		# r0 = kernel address
	movl	8(fp),r4		# r4 = user address
	andl3	$(NBPG*CLSIZE-1),r4,r2	# r2 = bytes on first page
	subl3	r2,$(NBPG*CLSIZE),r2
1:
	cmpl	r5,r2			# r2 = min(bytes on page, length left);
	jgeq	2f
	movl	r5,r2
2:
	probew	$1,(r4),r2		# bytes accessible?
	jeql	5f
	subl2	r2,r5			# update bytes left count
	movl	r2,r3			# r3 = saved count
	movl	r0,r1
/*
 * This is a workaround for a microcode bug that causes
 * a trap type 9 when cmps3/movs3 touches the last byte
 * on a valid page immediately followed by an invalid page.
 */
#ifdef good_cmps3
	cmps3				# check for null
	tstl	r2
	jneq	3b
#else
	decl	r2
	beql	9f			# cannot handle case of r2 == 0!
	cmps3				# check for null up to last byte
9:
	incl	r2
	cmpl	$1,r2			# get to last byte on page?
	bneq	3b
	tstb	(r0)			# last byte on page null?
	beql	3b
	incl	r0			# not null, so bump pointer
#endif not good_cmps3
	subl2	r3,r0			# back up r0
	movl	r4,r1
	movl	r3,r2
	movblk				# copy out next piece
	movl	r1,r4
	movl	$(NBPG*CLSIZE),r2	# check next page
	tstl	r5			# run out of space?
	jneq	1b
	movl	$ENOENT,r0		# set error code and return
	jbr	6b
5:
	clrl	*$0		# this should never execute, if it does
	movl	$EFAULT,r0	#  save me a core dump (mkm - 9/87)
6:
	tstl	16(fp)
	beql	7f
	subl3	r5,12(fp),*16(fp)
7:
	ret


/*
 * Copy a null terminated string from one point to another in
 * the kernel address space.
 *
 * copystr(fromaddr, toaddr, maxlength, &lencopied)
 */
ENTRY(copystr, 0)
	movl	12(fp),r3		# r3 = max length
	jlss	5b
	movl	4(fp),r0		# r0 = src address
	movl	8(fp),r4		# r4 = dest address
	clrl	r5			# r5 = bytes left
	movl	r3,r2			# r2 = max bytes to copy
	movl	r0,r1
	cmps3				# check for null
	tstl	r2
	jneq	3b
	subl2	r3,r0			# back up r0
	movl	r4,r1
	movl	r3,r2
	movblk				# copy next piece
	movl	$ENOENT,r0		# set error code and return
	jbr	6b

/*
 * Copy a block of data from the user address space into
 * the kernel address space.
 *
 * copyin(fromaddr, toaddr, count)
 */
ENTRY(copyin, 0)
	movl	12(fp),r0		# copy length
	blss	9f
	movl	4(fp),r1		# copy user address
	cmpl	$(CLSIZE*NBPG),r0	# probing one page or less ?
	bgeq	2f			# yes
1:
	prober	$1,(r1),$(CLSIZE*NBPG)	# bytes accessible ?
	beql	9f			# no
	addl2	$(CLSIZE*NBPG),r1	# incr user address ptr
	_ACBL($(CLSIZE*NBPG+1),$(-CLSIZE*NBPG),r0,1b)	# reduce count and loop
2:
	prober	$1,(r1),r0		# bytes accessible ?
	beql	9f			# no
	MOVC3(4(fp),8(fp),12(fp))
	clrl	r0
	ret
9:
	movl	$EFAULT,r0
	ret

/*
 * Copy a block of data from the kernel 
 * address space to the user address space.
 *
 * copyout(fromaddr, toaddr, count)
 */
ENTRY(copyout, 0)
	movl	12(fp),r0		# get count
	blss	9b
	movl	8(fp),r1		# get user address
	cmpl	$(CLSIZE*NBPG),r0	# can do in one probew?
	bgeq	2f			# yes
1:
	probew	$1,(r1),$(CLSIZE*NBPG)	# bytes accessible?
	beql	9b			# no 
	addl2	$(CLSIZE*NBPG),r1	# increment user address
	_ACBL($(CLSIZE*NBPG+1),$(-CLSIZE*NBPG),r0,1b)	# reduce count and loop
2:
	probew	$1,(r1),r0		# bytes accessible?
	beql	9b			# no
	MOVC3(4(fp),8(fp),12(fp))
	clrl	r0
	ret

/*
 * non-local goto's
 */
#ifdef notdef
ENTRY(setjmp, 0)
	movl	4(fp),r0
	movl	(fp),(r0); addl2 $4,r0		# save fp
	movl	-8(fp),(r0)			# save pc
	clrl	r0
	ret
#endif

ENTRY(longjmp, 0)
	movl	4(fp),r0
	movl	(r0),newfp; addl2 $4,r0		# must save parameters in memory
	movl	(r0),newpc			# as all regs may be clobbered.
1:
	cmpl	fp,newfp			# are we there yet?
	bgequ	2f				# yes
	moval	1b,-8(fp)			# redirect return pc to us!
	ret					# pop next frame
2:
	beql	3f				# did we miss our frame?
	pushab	4f				# yep ?!?
	callf	$8,_panic
3:
	movl	newpc,r0			# all done, just return
	jmp	(r0)				# to setjmp `ret'

	.data
newpc:	.space	4
newfp:	.space	4
4:	.asciz	"longjmp"
	.text

/*
 * setjmp that saves all registers as the call frame may not
 * be available to recover them in the usual manner by longjmp.
 * Called before swapping out the u. area, restored by resume()
 * below.
 */
ENTRY(savectx, 0)
	movl	4(fp),r2
	storer	$0x1ff8,(r2); addl2 $40,r2	# r3-r12
	movl	(fp),(r2); addl2 $4,r2		# fp
	movab	8(fp),(r2); addl2 $4,r2		# sp
	movl	-8(fp),(r2)			# pc
	clrl	r0
	ret

#ifdef KDB
/*
 * C library -- reset, setexit
 *
 *	reset(x)
 * will generate a "return" from
 * the last call to
 *	setexit()
 * by restoring r2 - r12, fp
 * and doing a return.
 * The returned value is x; on the original
 * call the returned value is 0.
 */
ENTRY(setexit, 0)
	movab	setsav,r0
	storer	$0x1ffc, (r0)
	movl	(fp),44(r0)		# fp
	moval	4(fp),48(r0)		# sp
	movl	-8(fp),52(r0)		# pc
	clrl	r0
	ret

ENTRY(reset, 0)
	movl	4(fp),r0	# returned value
	movab	setsav,r1
	loadr	$0x1ffc,(r1)
	movl	44(r1),fp
	movl	48(r1),sp
	jmp 	*52(r1)

	.data
	.align	2
setsav:	.space	14*4
	.text
#endif

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
ENTRY(setrq, 0)
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
ENTRY(remrq, 0)
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

/*
 * Masterpaddr is the p->p_addr of the running process on the master
 * processor.  When a multiprocessor system, the slave processors will have
 * an array of slavepaddr's.
 */
	.globl	_masterpaddr
	.data
	.align	2
_masterpaddr: .long	0

	.text
sw0:	.asciz	"swtch"

/*
 * When no processes are on the runq, swtch branches to idle
 * to wait for something to come ready.
 */
	.globl  Idle
Idle: idle:
	movl	$1,_noproc
	mtpr	$0,$IPL			# must allow interrupts here
1:
	tstl	_whichqs		# look for non-empty queue
	bneq	sw1
	brb	1b

badsw:	pushab	sw0
	callf	$8,_panic
	/* NOTREACHED */

	.align	2
/*
 * swtch(), using fancy tahoe instructions
 */
ENTRY(swtch, 0)
	movl	(fp),fp			# prepare for rei
	movl	(sp),4(sp)		# saved pc
	tstl	(sp)+
	movpsl	4(sp)
	incl	_cnt+V_SWTCH
sw1:	ffs	_whichqs,r0		# look for non-empty queue
	blss	idle			# if none, idle
	mtpr	$0x18,$IPL		# lock out all so _whichqs==_qs
	bbc	r0,_whichqs,sw1		# proc moved via interrupt
	shal	$1,r0,r1
	moval	_qs[r1],r1
	movl	(r1),r2			# r2 = p = highest pri process
	remque	*(r1)
	bvs	badsw			# make sure something was there
	bneq	sw2
	shal	r0,$1,r1
	mcoml	r1,r1
	andl2	r1,_whichqs		# no more procs in this queue
sw2:
	clrl	_noproc
	clrl	_runrun
#ifdef notdef
	tstl	P_WCHAN(r2)		## firewalls
	bneq	badsw			##
	cmpb	P_STAT(r2),$SRUN	##
	bneq	badsw			##
#endif
	clrl	P_RLINK(r2)		##
	movl	*P_ADDR(r2),r0
#ifdef notdef
	cmpl	r0,_masterpaddr		# resume of current proc is easy
	beql	res0
#endif
	movl	r0,_masterpaddr
	shal	$PGSHIFT,r0,r0		# r0 = pcbb(p)
	brb	swresume

/*
 * resume(pf)
 */
ENTRY(resume, 0)
	shal	$PGSHIFT,4(fp),r0	# r0 = pcbb(pf)
	movl	(fp),fp			# prepare for rei
	movl	(sp)+,4(sp)		# saved pc
	tstl	(sp)+
	movpsl	4(sp)
swresume:
	mtpr	$0x18,$IPL		# no interrupts, please
	movl	_CMAP2,_u+PCB_CMAP2	# yech
	REST_ACC			# restore original accumulator
	svpctx
	mtpr	r0,$PCBB
	ldpctx
	movl	_u+PCB_CMAP2,_CMAP2	# yech
	mtpr	$_CADDR2,$TBIS
res0:
	movl	_u+U_PROCP,r2		# r2 = u.u_procp
	tstl	P_CKEY(r2)		# does proc have code key?
	bneq	1f
	callf	$4,_getcodekey		# no, give him one
	movl	_u+U_PROCP,r2		# r2 = u.u_procp
	movl	r0,P_CKEY(r2)
1:
	tstl	P_DKEY(r2)		# does proc have data key?
	bneq	1f
	callf	$4,_getdatakey		# no, give him one
	movl	_u+U_PROCP,r2		# r2 = u.u_procp
	movl	r0,P_DKEY(r2)
1:
	mtpr	P_CKEY(r2),$CCK		# set code cache key
	mtpr	P_DKEY(r2),$DCK		# set data cache key
	tstl	_u+PCB_SSWAP
	bneq	res1
	rei
res1:					# longjmp to saved context
	movl	_u+PCB_SSWAP,r2
	clrl	_u+PCB_SSWAP
	loadr	$0x3ff8,(r2); addl2 $44,r2	# restore r3-r13 (r13=fp)
	movl	(r2),r1; addl2 $4,r2	# fetch previous sp ...
	movab	(sp),r0			# ... and current sp and
	cmpl	r1,r0			# check for credibility,
	bgequ	1f			# if further up stack ...
	pushab 2f; callf $8,_panic	# ... panic
	/*NOTREACHED*/
1:					# sp ok, complete return
	movl	r1,sp			# restore sp
	pushl	$PSL_PRVMOD		# kernel mode, ipl 0
	pushl	(r2)			# return address
	rei
2:	.asciz	"ldctx"

/*
 * {fu,su},{byte,word}
 */
ENTRY(fuword, 0)
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

ENTRY(fubyte, 0)
	prober	$1,*4(fp),$1
	beql	fserr
	movzbl	*4(fp),r0
	ret

ENTRY(suword, 0)
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

ENTRY(subyte, 0)
	probew	$1,*4(fp),$1
	beql	fserr
	movb	11(fp),*4(fp)
	clrl	r0
	ret

/*
 * Copy 1 relocation unit (NBPG bytes)
 * from user virtual address to physical address
 */
ENTRY(copyseg, 0)
	orl3	$PG_V|PG_KW,8(fp),_CMAP2
	mtpr	$_CADDR2,$TBIS	# invalidate entry for copy 
	MOVC3(4(fp),$_CADDR2,$NBPG)
	ret

/*
 * Clear a page of memory.  The page frame is specified.
 *
 * clearseg(pf);
 */
ENTRY(clearseg, 0)
	orl3	$PG_V|PG_KW,4(fp),_CMAP1	# Maps to virtual addr CADDR1
	mtpr	$_CADDR1,$TBIS
	movl	$255,r0				# r0 = limit
	clrl	r1				# r1 = index of cleared long
1:
	clrl	_CADDR1[r1]
	aobleq	r0,r1,1b
	ret

/*
 * Check user mode read/write access.
 *
 * useracc(addr, count, mode)
 *	caddr_t addr; int count, mode;
 * mode = 0	write access
 * mode = 1	read access
 */
ENTRY(useracc, 0)
	movl	$1,r2			# r2 = 'user mode' for probew/probew
probes:
	movl	4(fp),r0		# get va
	movl	8(fp),r1		# count
	tstl	12(fp)			# test for read access ?
	bneq	userar			# yes
	cmpl	$(CLSIZE*NBPG),r1	# can we do it in one probe ?
	bgeq	uaw2			# yes
uaw1:
	probew	r2,(r0),$(CLSIZE*NBPG)
	beql	uaerr			# no access
	addl2	$(CLSIZE*NBPG),r0
	_ACBL($(CLSIZE*NBPG+1),$(-CLSIZE*NBPG),r1,uaw1)
uaw2:
	probew	r2,(r0),r1
	beql	uaerr
	movl	$1,r0
	ret
userar:
	cmpl	$(CLSIZE*NBPG),r1
	bgeq	uar2
uar1:
	prober	r2,(r0),$(CLSIZE*NBPG)
	beql	uaerr
	addl2	$(CLSIZE*NBPG),r0
	_ACBL($(CLSIZE*NBPG+1),$(-CLSIZE*NBPG),r1,uar1)
uar2:
	prober	r2,(r0),r1
	beql	uaerr
	movl	$1,r0
	ret
uaerr:
	clrl	r0
	ret

/*
 * Check kernel mode read/write access.
 *
 * kernacc(addr, count, mode)
 *	caddr_t addr; int count, mode;
 * mode = 0	write access
 * mode = 1	read access
 */
ENTRY(kernacc, 0)
	clrl	r2		# r2 = 0 means kernel mode probe.
	jbr	probes		# Dijkstra would get gastric distress here.

/*
 * addupc - increment some histogram counter
 *	in the profiling buffer
 *
 * addupc(pc, prof, delta)
 *	long pc; short delta; struct uprof *prof;
 * 
 * struct uprof {		# profile arguments 
 * 	short	*r_base;	# buffer base 
 * 	unsigned pr_size;	# buffer size 
 * 	unsigned pr_off;	# pc offset 
 * 	unsigned pr_scale;	# pc scaling 
 * }
 */
ENTRY(addupc, 0)
	movl	8(fp),r2		# r2 points to structure
	subl3	8(r2),4(fp),r0		# r0 = PC - lowpc
	jlss	9f			# PC < lowpc , out of range !
	shrl	$1,r0,r0		# the unit is words
	shrl	$1,12(r2),r1		# ditto for scale
	emul	r1,r0,$0,r0
	shrq	$14,r0,r0
	tstl	r0			# too big
	jneq	9f
	cmpl	r1,4(r2)		# Check buffer overflow
	jgequ	9f
	probew	$1,*0(r2)[r1],$2	# counter accessible?
	jeql	9f
	shrl	$1,r1,r1		# make r1 word index
	addw2	14(fp),*0(r2)[r1]
9:	ret

/*
 * scanc(size, cp, table, mask)
 */
ENTRY(scanc, R3|R4)
	movl	8(fp),r0		# r0 = cp
	addl3	4(fp),r0,r2		# end = &cp[size]
	movl	12(fp),r1		# r1 = table
	movb	19(fp),r4		# r4 = mask
	decl	r0			# --cp
	jbr	0f			# just like Fortran...
1:					# do {
	movzbl	(r0),r3	
	bitb	r4,(r1)[r3]		#	if (table[*cp] & mask)
	jneq	2f			#		break;
0:	aoblss	r2,r0,1b		# } while (++cp < end);
2:
	subl3	r0,r2,r0; ret		# return (end - cp);

/*
 * skpc(mask, size, cp)
 */
ENTRY(skpc, 0)
	movl	12(fp),r0		# r0 = cp
	addl3	8(fp),r0,r1		# r1 = end = &cp[size];
	movb	7(fp),r2		# r2 = mask
	decl	r0			# --cp;
	jbr	0f
1:					# do
	cmpb	(r0),r2			#	if (*cp != mask)
	jneq	2f			#		break;
0:	aoblss	r1,r0,1b		# while (++cp < end);
2:
	subl3	r0,r1,r0; ret		# return (end - cp);

/*
 * locc(mask, size, cp)
 */
ENTRY(locc, 0)
	movl	12(fp),r0		# r0 = cp
	addl3	8(fp),r0,r1		# r1 = end = &cp[size]
	movb	7(fp),r2		# r2 = mask
	decl	r0			# --cp;
	jbr	0f
1:					# do
	cmpb	(r0),r2			#	if (*cp == mask)
	jeql	2f			#		break;
0:	aoblss	r1,r0,1b		# while (++cp < end);
2:
	subl3	r0,r1,r0; ret		# return (end - cp);

#ifdef ALIGN
#include "../tahoealign/align.h"

	.globl	_alignment
/*
 * There's an intimate relationship between this piece of code
 * and the alignment emulation code (especially the layout
 * of local variables in alignment.c! Don't change unless
 * you update both this, alignment.h and alignment.c !!
 */
non_aligned:
	orb2	$EMULATEALIGN,_u+U_EOSYS
	incl	_cnt+V_TRAP
	incl	_cnt+V_ALIGN		# count emulated alignment traps
	moval	4(sp),_user_psl
	SAVE_FPSTAT(4)			# Also zeroes out ret_exception !
	pushl	$0			# ret_addr
	pushl	$0			# ret_code
	mfpr	$USP,-(sp)		# user sp
	callf	$4,_alignment		# call w/o parms so regs may be modified
	/*
	 * We return here after a successful emulation or an exception.
	 * The registers have been restored and we must not alter them
	 * before returning to the user.
	 */
2:	mtpr	(sp)+,$USP		# restore user sp
	tstl	8(sp)			# Any exception ?
	bneq	got_excp		# Yes, reflect it back to user.
	moval	8(sp),sp		# pop 2 zeroes pushed above
	REST_FPSTAT
	xorb2	$EMULATEALIGN,_u+U_EOSYS

	bitl	$PSL_T,4(sp)		# check for trace bit set
	beql	9f
	CHECK_SFE(4)
	pushl $0
	SAVE_FPSTAT(8)
	TRAP(TRCTRAP)
9:	rei

got_excp:				# decode exception
	casel	8(sp),$ILL_ADDRMOD,$ALIGNMENT
	.align	1
L1:
	.word	ill_addrmod-L1
	.word	ill_access-L1
	.word	ill_oprnd-L1
	.word	arithmetic-L1
	.word	alignment-L1
	brw	alignment		# default - shouldn't come here at all !

ill_addrmod:				# No other parameters. Set up stack as
	moval	8(sp),sp		# the HW would do it in a real case.
	REST_FPSTAT
	jbr	_Xresadflt
ill_oprnd:
	moval	8(sp),sp
	REST_FPSTAT
	jbr	_Xresopflt
alignment:
	moval	8(sp),sp
	REST_FPSTAT
	jbr	align_excp	# NB: going to _Xalignflt would cause loop
ill_access:
	/*
	 * Must restore accumulator w/o modifying sp and w/o using
	 * registers.  Solution: copy things needed by REST_FPSTAT.
	 */
	pushl	20(sp)			# The flags longword
	pushl	20(sp)			# acc_low
	pushl	20(sp)			# acc_high
	pushl	20(sp)			# ret_exception ignored by REST_FPSTAT 
	REST_FPSTAT			# Back where we were with the sp !
	movl	(sp),16(sp)		# code for illegal access
	movl	4(sp),20(sp)		# original virtual address
	moval	16(sp),sp		# Just like the HW would set it up
	jbr	_Xprotflt
arithmetic:				# same trickery as above
	pushl	20(sp)			# The flags longword
	pushl	20(sp)			# acc_low
	pushl	20(sp)			# acc_high
	pushl	20(sp)			# ret_exception ignored by REST_FPSTAT 
	REST_FPSTAT			# Back where we were with the sp !
	movl	(sp),20(sp)		# code for arithmetic exception
	moval	20(sp),sp		# Just like the HW would set it up
	jbr	_Xarithtrap
#endif
