#ifndef lint
static char sccsid[] = "@(#)machdep.c	5.4 (Berkeley) 9/26/89";
#endif

/*
 * adb - miscellaneous machine dependent routines.
 */

#define	RLOCALS			/* enable alternate $C stack trace */

#include "defs.h"
#include "bkpt.h"
#include <machine/pte.h>
#include <machine/frame.h>
#include <machine/reg.h>
#include <machine/vmparam.h>
#include <sys/ptrace.h>
#include <sys/vmmac.h>
#include <stab.h>

struct	pte *sbr;
int	slr;
struct	pcb pcb;
int	masterpcbb;

/*
 * Activation records.
 */

/*
 * Set up a stack frame based on the registers in the core image
 * (or in the kernel core file ... not yet!).
 */
a_init(ap)
	register struct activation *ap;
{

	ap->a_valid = 1;
	if (kcore) {
		ap->a_fp = pcb.pcb_fp;
		ap->a_pc = pcb.pcb_pc;
	} else {
		ap->a_fp = u.u_ar0[FP];
		ap->a_pc = u.u_ar0[PC];
	}
}

/*
 * Back up one stack frame in the call stack.
 * ap points to the activation record from the previous frame.
 * Clear a_valid field if we ran out of frames.
 */
a_back(ap)
	register struct activation *ap;
{
	struct frame fr;

	if (adbread(SP_DATA, ap->a_fp - FRAMEOFF, &fr, sizeof fr) != sizeof fr)
		ap->a_valid = 0;
	else {
		ap->a_fp = fr.fr_savfp;
		ap->a_pc = fr.fr_savpc;
		if (ap->a_fp == 0)
			ap->a_valid = 0;
	}
}

/*
 * Evaluate a local symbol (N_LSYM or N_PSYM) using the activation
 * record pointed to by ap.
 */
addr_t
eval_localsym(sp, ap)
	register struct nlist *sp;
	struct activation *ap;
{

	switch (sp->n_type) {

	case N_LSYM:
		return (ap->a_fp - sp->n_value);

	case N_PSYM:
		return (ap->a_fp + sp->n_value);
	}
	panic("eval_localsym");
	/* NOTREACHED */
}


/* true iff address a is in instruction space */
#define	ispace(a) ((a) < txtmap.m1.e)

/*
 * Delete a (single) breakpoint.  Return 0 on success.
 */
int
clr_bpt(b)
	struct bkpt *b;
{
	addr_t a = b->loc;

	return (adbwrite(ispace(a) ? SP_INSTR : SP_DATA, a, &b->ins, 1) != 1);
}

/*
 * Set a (single) breakpoint.  Return 0 on success.
 */
set_bpt(b)
	struct bkpt *b;
{
	addr_t a = b->loc;
	int space;
	char bpt = 0x30;		/* breakpoint instruction */

	space = ispace(a) ? SP_INSTR : SP_DATA;
	return (adbread(space, a, &b->ins, 1) != 1 ||
		adbwrite(space, a, &bpt, 1) != 1);
}

/*
 * Check a float for `correctness' (reserved patterns, etc).  Return
 * a pointer to a character string to be printed instead of the float,
 * or NULL to print the float as-is.
 *
 * The string returned, if any, should be no longer than 16 characters.
 *
 * On the Tahoe, we can simply check the second two bytes.  Byte two
 * contains one bit of the exponent, and byte 3 has the remaining 7
 * exponent bits and the sign bit.  If the sign bit is set and the
 * exponent is zero, the value is reserved.
 *
 * PLEASE CHECK THE ABOVE, IT IS PROBABLY WRONG
 */
/* ARGSUSED */
char *
checkfloat(fp, isdouble)
	caddr_t fp;
	int isdouble;
{

	return ((((short *)fp)[1] & 0xff80) == 0x8000 ?
		"(reserved oprnd)" : NULL);
}

/*
 * Convert a value in `expr_t' format to float or double.
 */
etofloat(e, fp, isdouble)
	expr_t e;
	caddr_t fp;
	int isdouble;
{

	if (isdouble)
		((int *)fp)[1] = 0;
	*(int *)fp = e;
}

mch_init()
{

	mkioptab();
}

/* quietly read object obj from address addr */
#define	GET(obj, addr)	(void) adbread(SP_DATA, addr, &(obj), sizeof(obj))

/* set `current process' pcb */
setpcb(addr)
	addr_t addr;
{
	int pte;

	GET(pte, addr);
	masterpcbb = (pte & PG_PFNUM) * NBPG;
}

getpcb()
{

	/* maybe use adbread() here ... */
	(void) readcore((off_t)masterpcbb & ~KERNBASE,
		(char *)&pcb, sizeof(struct pcb));
	adbprintf("p0br %R p0lr %R p2br %R p2lr %R\n",
	    pcb.pcb_p0br, pcb.pcb_p0lr, pcb.pcb_p2br, pcb.pcb_p2lr);
}

/*
 * Convert a kernel virtual address to a physical address,
 * a la the Tahoe hardware.  Set *err if the resulting address
 * is invalid.
 */
addr_t
vtophys(addr, err)
	addr_t addr;
	char **err;
{
	register unsigned v = btop(addr & ~KERNBASE);
	register addr_t pteaddr;
	struct pte pte;

	switch ((int)(addr >> 30)) {	/* select space */

	case 3:
		/* system space: get system pte */
		if (v >= slr)
			goto oor;
		pteaddr = (addr_t)(sbr + v) & ~KERNBASE;
		goto direct;

	case 2:
		/* P2 space: must not be in shadow region */
		if (v < pcb.pcb_p2lr)
			goto oor;
		pteaddr = (addr_t)(pcb.pcb_p2br + v);
		break;

	case 1:
		/* P1 space: verboten (for now) */
		goto oor;

	case 0:
		/* P0 space: must not be off end of region */
		if (v >= pcb.pcb_p0lr)
			goto oor;
		pteaddr = (addr_t)(pcb.pcb_p0br + v);
		break;

oor:
		*err = "address out of segment";
		return (0);
	}

	/* in P0/P1/P2 space, pte should be in kernel virtual space */
	if ((pteaddr & KERNBASE) != KERNBASE) {
		*err = "bad p0br, p1br, or p2br in pcb";
		return (0);
	}
	pteaddr = vtophys(pteaddr, err);
	if (*err)
		return (0);

direct:
	/*
	 * Read system pte.  If valid or reclaimable,
	 * physical address is combination of its page number and
	 * the page offset of the original address.
	 */
	if (readcore((off_t)pteaddr, (caddr_t)&pte, 4) != 4) {
		*err = "page table botch";
		return (0);
	}
	/* SHOULD CHECK NOT I/O ADDRESS; NEED CPU TYPE! */
	if (pte.pg_v == 0 && (pte.pg_fod || pte.pg_pfnum == 0)) {
		*err = "page not valid/reclaimable";
		return (0);
	}
	return ((addr_t)(ptob(pte.pg_pfnum) + (addr & PGOFSET)));
}

/*
 * Print a stack trace ($c, $C).  Trace backwards through nback
 * frames; if locals is set, print local variables.
 */
printstack(locals, nback)
	int locals, nback;
{
	register int i;
	register addr_t a;
	struct nlist *sym;
	char *s;
	addr_t callpc;		/* pc that called this frame */
	int narg;		/* number of arguments to this frame */
	struct activation cur;	/* this frame itself */
	struct frame fr;	/* the frame above this frame */
	addr_t dummy;		/* a variable to scribble on */
#define	UNKNOWN	-1

#ifdef RLOCALS
	/* if locals variables are broken, use an alternate strategy */
	register int r;
	addr_t sp, prev_sp;
	int regs[13];
	static char unknown[] = "<unknown>";
#endif

#ifdef RLOCALS
	/* grab registers */
	bcopy((caddr_t)(kcore ? &pcb.pcb_r0 : &u.u_ar0[R0]), (caddr_t)regs,
		sizeof(regs));
#endif

	/* set up the current stack frame */
	if (gavedot) {
		cur.a_fp = dot;
		cur.a_pc = UNKNOWN;
#ifdef RLOCALS
		sp = UNKNOWN;
#endif
	} else if (kcore) {
		cur.a_fp = pcb.pcb_fp;
		cur.a_pc = pcb.pcb_pc;
#ifdef RLOCALS
		sp = pcb.pcb_ksp;
#endif
	} else {
		cur.a_fp = u.u_ar0[FP];
		cur.a_pc = u.u_ar0[PC];
#ifdef RLOCALS
		sp = u.u_ar0[SP];
#endif
	}

	/* now back up through the stack */
	while (nback-- && cur.a_fp != 0) {
		/* read this frame, but defer error check */
		GET(fr, cur.a_fp - FRAMEOFF);

		/* where are we? ... if u. area, signal trampoline code */
		if (cur.a_pc >= USRSTACK && cur.a_pc < KERNBASE) {
			narg = 0;
			GET(callpc, cur.a_fp + 44);	/* XXX magic 44 */
			s = "sigtramp";
		} else {
			narg = (fr.fr_removed >> 2) - 1;
			callpc = fr.fr_savpc;
			if (cur.a_pc != UNKNOWN &&
			    (sym = findsym(cur.a_pc, SP_INSTR, &dummy)) != 0) {
				s = sym->n_un.n_name;
				if (eqstr(s, "start")) {
					errflag = NULL;
					break;
				}
			} else
				s = "?";
		}
		/* safe at last to check for error reading frame */
		checkerr();

		/* arguments */
		adbprintf("%s(", s);
		a = cur.a_fp;
		for (i = narg > 20 ? 20 : narg; i;) {
			prfrom(a += 4, --i ? ',' : 0);
			checkerr();
		}
		printc(')');
		if (cur.a_pc != UNKNOWN) {
			prints(" at ");
			psymoff("%R", cur.a_pc, SP_INSTR, -(addr_t)1, "");
		}
		printc('\n');

		/* local variables */
		if (locals) {
#ifdef busted
			if (cur.a_pc != UNKNOWN) {
				sym = findsym(cur.a_pc, SP_INSTR, &dummy);
				while ((sym = nextlocal(sym)) != NULL) {
					adbprintf("%8t");
					printlsym(sym->n_un.n_name);
					adbprintf(":%12t");
					prfrom(eval_localsym(sym, &cur), '\n');
				}
			}
#endif
#ifdef RLOCALS
			adbprintf("\
fp: %R\%16tsp:  %?s%?R%32tpc:  %?s%?R%48tr0:  %R\n\
r1: %R\%16tr2:  %R\%32tr3:  %R\%48tr4:  %R\n\
r5: %R\%16tr6:  %R\%32tr7:  %R\%48tr8:  %R\n\
r9: %R\%16tr10: %R\%32tr11: %R\%48tr12: %R\n",
#define q(s) s == UNKNOWN, unknown, s != UNKNOWN, s
			    cur.a_fp, q(sp), q(cur.a_pc), regs[0],
#undef q
			    regs[1], regs[2], regs[3], regs[4],
			    regs[5], regs[6], regs[7], regs[8],
			    regs[9], regs[10], regs[11], regs[12]);

			/* update registers, and find previous frame's sp */
			a = cur.a_fp + 4;
			for (r = 0, i = fr.fr_mask; i != 0; r++, i >>= 1)
				if (i & 1)
					GET(regs[r], a += 4);
			a += narg * 4;
			prev_sp = a;

			/* now print automatics */
			if (sp != UNKNOWN) {
#define	MAXPRINT 30		/* max # words to print */
				/* XXX should be settable */
				i = (cur.a_fp - sp) >> 2;
				if (i > MAXPRINT)
					i = MAXPRINT;
				for (a = cur.a_fp; --i >= 0;) {
					a -= 4;
					adbprintf("%R: %V(fp):%24t",
						a, a - cur.a_fp);
					prfrom(a, '\n');
				}
				if (a > sp)
					adbprintf("\
%R: %V(fp) .. %R: %V(fp) not displayed\n",
						a, a - cur.a_fp,
						sp, sp - cur.a_fp);
			}
#endif /* RLOCALS */
		}

		errflag = NULL;		/* clobber any read errors */

		/* back up one frame */
		if (fr.fr_savfp == 0)
			break;
		cur.a_fp = fr.fr_savfp;
#ifdef RLOCALS
		sp = prev_sp;
#endif
		cur.a_pc = callpc;

		if (!gavedot && !INSTACK(cur.a_fp) && !kcore)
			break;

		/* make sure we returned somewhere... */
		(void) adbread(kcore ? SP_DATA : SP_INSTR, cur.a_pc, &dummy, 1);
		checkerr();
	}
}

/*
 * Register offset to u. pointer, and register offset to ptrace value
 */
#define	otoua(o) \
	((int *)(((o) < 0 ? (int)u.u_ar0 : (int)&u.u_pcb) + (o)))
#define	otopt(o) \
	((int *)((o) < 0 ? (o) + ctob(UPAGES) : (o)))

/*
 * Return the value of some register.
 */
expr_t
getreg(reg)
	register struct reglist *reg;
{

	return (kcore ? *reg->r_pcbaddr : *otoua(reg->r_offset));
}


/*
 * Set the value of some register.  Return 0 if all goes well.
 */
setreg(reg, val)
	register struct reglist *reg;
	expr_t val;
{

	if (kcore)
		*reg->r_pcbaddr = val;
	else {
		*otoua(reg->r_offset) = val;
		if (pid) {
			errno = 0;
			if (ptrace(PT_WRITE_U, pid, otopt(reg->r_offset),
					(int)val) == -1 && errno)
				return (-1);
		}
	}
	return (0);
}

/*
 * Read registers from current process.
 */
readregs()
{
	register struct reglist *reg;
	extern struct reglist reglist[];

	for (reg = reglist; reg->r_name != NULL; reg++)
		*otoua(reg->r_offset) =
			ptrace(PT_READ_U, pid, otopt(reg->r_offset), 0);
}

addr_t
getpc()
{

	return (kcore ? pcb.pcb_pc : u.u_ar0[PC]);
}

setpc(where)
	addr_t where;
{

	if (kcore)
		pcb.pcb_pc = where;
	else
		u.u_ar0[PC] = where;
}

/*
 * udot returns true if u.u_pcb appears correct.  More extensive
 * checking is possible....
 */
udot()
{

	/* user stack should be in stack segment */
	if (!INSTACK(u.u_pcb.pcb_usp))
		return (0);
	/* kernel stack should be in u. area */
	if (u.u_pcb.pcb_ksp < USRSTACK || u.u_pcb.pcb_ksp >= KERNBASE)
		return (0);
	/* looks good to us... */
	return (1);
}

sigprint()
{
	extern char *sys_siglist[];
	extern char *illinames[], *fpenames[];
	extern int nillinames, nfpenames;

	if ((u_int)signo - 1 < NSIG - 1)
		prints(sys_siglist[signo]);
	switch (signo) {

	case SIGFPE:
		if ((u_int)sigcode < nfpenames)
			prints(fpenames[sigcode]);
		break;

	case SIGILL:
		if ((u_int)sigcode < nillinames)
			prints(illinames[sigcode]);
		break;
	}
}
