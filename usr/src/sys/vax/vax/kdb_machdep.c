/*	@(#)kdb_machdep.c	7.1 (Berkeley) %G%	*/

#include "param.h"
#include "conf.h"
#include "dir.h"
#include "user.h"
#include "proc.h"
#include "uio.h"
#include "systm.h"
#include "reboot.h"
#include "vmmac.h"
#include "ioctl.h"
#include "tty.h"

#include "../vax/cpu.h"
#include "../vax/mtpr.h"
#include "../vax/psl.h"
#include "../vax/pte.h"
#include "../vax/reg.h"
#include "../vax/trap.h"

#define	KDB_IPL		0xf	/* highest priority software interrupt */
#define	setsoftkdb()	mtpr(SIRR, KDB_IPL)

#define	KDBSPACE	1024	/* 1K of memory for breakpoint table */
static	char kdbbuf[KDBSPACE];
static	char *kdbend = kdbbuf;
/*
 * Dynamically allocate space for the debugger.
 */
char *
kdbmalloc(n)
	u_int n;
{
	char *old = kdbend;

	if (kdbend+n >= kdbbuf+KDBSPACE) {
		printf("kdb: Out of space\n");
		return ((char *)-1);
	}
	kdbend += n;
	return (old);
}

/*
 * Initialize the kernel debugger.
 */
kdb_init()
{
	char *symtab, *strtab;
	int strsize;
	extern int end;

	kdbsetup();
	if (bootesym > (char *)&end) {
		symtab = (char *)&end + sizeof (int);
#define	symsize	*(int *)&end
		strtab = symtab + symsize;
		strsize = roundup(*(int *)strtab, sizeof (int));
		if (strtab + strsize == bootesym) {
			printf("[Preserving %d bytes of symbol information]\n",
			    symsize + strsize);
			kdbsetsym(symtab, strtab, strtab, strsize);
		} else
			printf("kdb_init: bad bootesym %x, calculated %x\n",
			    bootesym, strtab + strsize);
	}
	/*
	 * If boot flags indicate, force entry into the debugger.
	 */
	if ((boothowto&(RB_HALT|RB_KDB)) == (RB_HALT|RB_KDB))
		setsoftkdb();
#undef	symsize
}

int	kdbactive = 0;
#define	ESC	CTRL([)
/*
 * Process a keyboard interrupt from the console.
 * We look for an escape sequence which signals
 * a request to enter the debugger.
 */
kdbrintr(c, tp)
	int c;
	struct tty *tp;
{
	static int escape = 0;

	c &= 0177;			/* strip parity also */
	if (!escape)
		return (c == ESC &&  ++escape);
	escape = 0;
	/*
	 * Transfer control to the debugger only if the
	 * system was booted with RB_KDB and the trap
	 * enable flag (RB_NOYSNC) is set.
	 */
	if ((boothowto&(RB_KDB|RB_NOSYNC)) != (RB_KDB|RB_NOSYNC) ||
	    (c != 'k' && c != 'K' && c != CTRL(k))) {
		(*linesw[tp->t_line].l_rint)(ESC, tp);
		return (0);
	}
	if (!kdbactive)
		setsoftkdb();
	return (1);
}

#define	TYPE	SP+1
#define	CODE	PC-1
#define	USER	040
static	caddr_t kdbnofault;		/* label for peek & poke */
/*
 * Field a kdb-related trap or fault.
 */
kdb_trap(apsl)
	register int *apsl;
{
	register int *locr0, type;
	int code, retval;
	static int prevtype = -1, prevcode;
	extern char *trap_type[];
	extern int TRAP_TYPES;

	/*
	 * Allow panic if the debugger is not enabled.
	 */
	if ((boothowto&RB_KDB) == 0)
		return (0);
	locr0 = apsl - PS;
	type = locr0[TYPE], code = locr0[CODE];
	if (type == T_KDBTRAP && prevtype != -1) {
		type = prevtype, code = prevcode;
		prevtype = -1;
	}
	if (type != T_TRCTRAP && type != T_BPTFLT) {
		/*
		 * Catch traps from kdbpeek and kdbpoke and perform
		 * non-local goto to error label setup in routines.
		 */
		if (kdbnofault) {
			locr0[PC] = (int)kdbnofault;
			return (1);
		}
		type &= ~USER;
	}
	/*
	 * We prefer to run the debugger from the interrupt stack to
	 * avoid overflowing the kernel stack.  Thus, if we're not
	 * currently on the interrupt stack and the ipl is low, schedule
	 * a software interrupt to force reentry on the interrupt stack
	 * immediately after the rei that'll take place on return.
	 */
	if ((movpsl()&PSL_IS) == 0) {
		int s = splhigh();
		if (s < KDB_IPL) {
			prevtype = type, prevcode = code;
			setsoftkdb();
			return (1);
		}
		splx(s);
		printf("(from kernel stack)\n");
	}
	getpcb(locr0);
	/*
	 * Mark debugger active and initiate input
	 * polling in the console device driver.
	 */
	cnpoll(kdbactive = 1);
	retval = kdb(type, code, noproc ? (struct proc *)0 : u.u_procp);
	cnpoll(kdbactive = 0);
	setpcb(locr0);
	return (retval);
}

static	char *codenames[] = {
	"code = 0",
	"integer overflow",
	"integer divide by zero",
	"floating overflow",
	"floating/decimal divide by zero",
	"floating underflow",
	"decimal overflow",
	"subscript out of range",
	"floating overflow",
	"floating divide by zero",
	"floating undeflow"
};
#define	NCODES	(sizeof (codenames) / sizeof (codenames[0]))

/*
 * Announce a trap.
 */
kdbprinttrap(type, code)
	int type, code;
{

	extern int TRAP_TYPES;
	extern char *trap_type[];

	if (type != T_TRCTRAP && type != T_BPTFLT) {
		if (type < TRAP_TYPES && trap_type[type])
			printf(trap_type[type]);
		else
			printf("trap type %d", type);
		if (type == T_ARITHTRAP && (unsigned)code < NCODES)
			printf(", %s", code);
		else if (code)
			printf(", code = %d", code);
		printf("\n");
	}
}

/*
 * Read character from the console.
 */
kdbreadc(cp)
	char *cp;
{

	*cp = cngetc();
	return (1);
}

/*
 * Write characters to the console.
 */
kdbwrite(cp, len)
	register char *cp;
	register int len;
{

	while (len-- > 0)
		cnputc(*cp++);
}

/*
 * Fetch a longword carefully.
 */
kdbpeek(addr)
	register caddr_t addr;
{
	register long v = 0;

	asm("movab 1f,_kdbnofault");
	v = *(long *)addr;
asm("1:");
	kdbnofault = 0;
	return (v);
}

/*
 * Put a longword carefully.
 */
kdbpoke(addr, v)
	register caddr_t addr;
	long v;
{
	register int pn, *pte, opte = 0;
	extern char Sysbase[], etext;

	/*
	 * If we're writing to the kernel's text space,
	 * make the page writeable for the duration of
	 * the access.
	 */
	if ((caddr_t)Sysbase <= addr && addr <= (caddr_t)&etext) {
		pn = btop((int)addr &~ 0x80000000);
		pte = (int *)&Sysmap[pn];
		opte = *pte;
		*pte = (*pte &~ PG_PROT)|PG_KW;
		mtpr(TBIS, addr);
	}
	asm("movab 1f,_kdbnofault");
	*(long *)addr = v;
asm("1:");
	kdbnofault = 0;
	if (opte) {
		*pte = opte;
		mtpr(TBIS, addr);
	}
}

static
getpcb(locr0)
	register int *locr0;
{
	extern struct pcb kdbpcb;
	register struct pcb *pcb = &kdbpcb;

	pcb->pcb_r0 = locr0[R0];
	pcb->pcb_r1 = locr0[R1];
	pcb->pcb_r2 = locr0[R2];
	pcb->pcb_r3 = locr0[R3];
	pcb->pcb_r4 = locr0[R4];
	pcb->pcb_r5 = locr0[R5];
	pcb->pcb_r6 = locr0[R6];
	pcb->pcb_r7 = locr0[R7];
	pcb->pcb_r8 = locr0[R8];
	pcb->pcb_r9 = locr0[R9];
	pcb->pcb_r10 = locr0[R10];
	pcb->pcb_r11 = locr0[R11];
	pcb->pcb_ap = locr0[AP];
	pcb->pcb_fp = locr0[FP];
	pcb->pcb_usp = locr0[SP];
	pcb->pcb_pc = locr0[PC];
	pcb->pcb_psl = locr0[PS];
	pcb->pcb_ksp = mfpr(KSP);
	pcb->pcb_esp = mfpr(ISP);
	pcb->pcb_p0br = (struct pte *)mfpr(P0BR);
	pcb->pcb_p0lr = mfpr(P0LR);
	pcb->pcb_p1br = (struct pte *)mfpr(P1BR);
	pcb->pcb_p1lr = mfpr(P1LR);
}

static
setpcb(locr0)
	register int *locr0;
{
	extern struct pcb kdbpcb;
	register struct pcb *pcb = &kdbpcb;

	locr0[R0] = pcb->pcb_r0;
	locr0[R1] = pcb->pcb_r1;
	locr0[R2] = pcb->pcb_r2;
	locr0[R3] = pcb->pcb_r3;
	locr0[R4] = pcb->pcb_r4;
	locr0[R5] = pcb->pcb_r5;
	locr0[R6] = pcb->pcb_r6;
	locr0[R7] = pcb->pcb_r7;
	locr0[R8] = pcb->pcb_r8;
	locr0[R9] = pcb->pcb_r9;
	locr0[R10] = pcb->pcb_r10;
	locr0[R11] = pcb->pcb_r11;
	locr0[R12] = pcb->pcb_r12;
	locr0[FP] = pcb->pcb_fp;
	locr0[SP] = pcb->pcb_usp;
	locr0[PC] = pcb->pcb_pc;
	locr0[PS] = pcb->pcb_psl;
	locr0[RACH] = pcb->pcb_ach;
	locr0[RACL] = pcb->pcb_acl;
}
