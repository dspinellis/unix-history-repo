#ifndef lint
static char sccsid[] = "@(#)kstack.c	5.1 (Berkeley) 1/16/89";
#endif

/*
 * adb - routines to probe the kernel stack when debugging post-mortem
 * crash dumps.
 */

#include "defs.h"
#include <ctype.h>
#include <machine/pte.h>
#include <machine/frame.h>
#include <machine/rpb.h>

struct	pte *sbr;
int	slr;
struct	pcb pcb;

static caddr_t rpb, erpb;
static caddr_t intstack, eintstack;
static caddr_t ustack, eustack;

char	*malloc();

/*
 * Convert a kernel virtual address to an (off_t) physical offset.
 */
#define	kvtooff(a)	((off_t)(a) & ~KERNBASE)

/*
 * Check if an address is in one of the kernel's stacks:
 * interrupt stack, rpb stack (during restart sequence),
 * or u. stack.
 */
#define	within(a, b, e) \
	((addr_t)(a) >= (addr_t)(b) && (addr_t)(a) < (addr_t)(e))
#define	kstackaddr(a) \
	(within(a, intstack, eintstack) || \
	 within(a, rpb + sizeof(struct rpb), erpb) || \
	 within(a, ustack, eustack))

/*
 * Determine whether we are looking at a kernel core dump, and if so,
 * set sbr and slr and the current pcb.
 */
getkcore() {
	struct nlist *sm, *ss, *mp;

	if ((sm = lookup("_Sysmap")) == NULL ||
	    (ss = lookup("_Syssize")) == NULL ||
	    (mp = lookup("_masterpaddr")) == NULL)
		return (0);	/* a.out is not a vmunix */
	datmap.m1.b = 0;
	datmap.m1.e = -1L;
	/* must set sbr, slr before setpcb() */
	sbr = (struct pte *)sm->n_value;
	slr = ss->n_value;
	adbprintf("sbr %X slr %X\n", sbr, slr);
	setpcb((addr_t)mp->n_value);
	getpcb();
	findstackframe();
	return (1);
}

/*
 * A version of lookup that never returns failure, and which returns
 * the n_value field of the symbol found.
 */
static caddr_t
xlookup(sym)
	char *sym;
{
	struct nlist *sp;

	if ((sp = lookup(sym)) == NULL) {
		adbprintf("symbol %s not found ... bad kernel core?\n", sym);
		exit(1);
	}
	return ((caddr_t)sp->n_value);
}

/*
 * Find the current stack frame when debugging the kernel.
 * If we're looking at a crash dump and this was not a ``clean''
 * crash, then we must search the interrupt stack carefully
 * looking for a valid frame.
 */
findstackframe()
{
	register char *cp;
	register int n;
	caddr_t addr;
	struct frame fr;
	char buf[256];

	if (readcore(kvtooff(xlookup("_panicstr")), (caddr_t)&addr,
			sizeof(addr)) != sizeof(addr) || addr == 0)
		return;
	n = readcore(kvtooff(addr), buf, sizeof(buf));
	for (cp = buf; --n > 0 && *cp != 0; cp++)
		if (!isascii(*cp) || (!isprint(*cp) && !isspace(*cp)))
			*cp = '?';
	*cp = '\0';
	adbprintf("panic: %s\n", buf);

	/*
	 * After a panic, look at the top of the rpb stack to find a stack
	 * frame.  If this was a clean crash, i.e. one which left the
	 * interrupt and kernel stacks in a reasonable state, then we should
	 * find a pointer to the proper stack frame here (at location
	 * intstack-8).  If we don't find a reasonable frame here, then we
	 * must search down through the interrupt stack.
	 */
	intstack = xlookup("_intstack");
	eintstack = xlookup("_Xdoadump");		/* XXX */
	rpb = xlookup("_rsstk");			/* XXX */
	erpb = rpb + NBPG - 2 * sizeof(long);
	ustack = xlookup("_u");
	eustack = ustack + ctob(UPAGES);
	ustack += (int)((struct user *)0)->u_stack;
	(void) readcore(kvtooff(intstack - 8), (caddr_t)&addr, sizeof(addr));
	if (!getframe(addr, &fr) && !checkintstack(&addr, &fr)) {
		/* search kernel stack? */
		prints("can't locate stack frame\n");
		return;
	}
	/* probably shouldn't clobber pcb, but for now this is easy */
	pcb.pcb_fp = (int)addr;
	pcb.pcb_pc = fr.fr_savpc;
}

/*
 * Search interrupt stack for a valid frame.  Return 1 if found,
 * also setting *addr to the kernel address of the frame, and *frame
 * to the frame at that address.
 */
checkintstack(addr, frame)
	caddr_t *addr;
	struct frame *frame;
{
	register int ssize;
	register char *stack;

	ssize = eintstack - intstack;
	if ((stack = malloc((u_int)ssize)) == NULL)
		return (0);
	if (readcore(kvtooff(intstack), stack, ssize) != ssize) {
		free(stack);
		return (0);
	}
	for (ssize -= sizeof(*frame); ssize >= 0; ssize -= 4) {
		if (goodframe((struct frame *)&stack[ssize])) {
			*addr = &intstack[ssize] + FRAMEOFF;
			*frame = *(struct frame *)&stack[ssize];
			free(stack);
			return (1);
		}
	}
	free(stack);
	return (0);
}

/*
 * Get a stack frame and verify that it looks like
 * something which might be on a kernel stack.  Return 1 if OK.
 */
getframe(addr, fp)
	caddr_t addr;
	struct frame *fp;
{
	off_t off;
	char *err = NULL;

	if (!kstackaddr(addr))
		return (0);
	off = vtophys((addr_t)(addr - FRAMEOFF), &err);
	if (err || readcore(off, (caddr_t)fp, sizeof(*fp)) != sizeof(*fp))
		return (0);
	return (goodframe(fp));
}

/*
 * Check a call frame to see if it's ok as a kernel stack frame.
 * It should have its parent frame in the kernel stack, and should
 * return to kernel code.
 */
goodframe(fr)
	register struct frame *fr;
{

	return (kstackaddr(fr->fr_savfp) &&
		within(fr->fr_savpc, txtmap.m1.b, txtmap.m1.e));
}
