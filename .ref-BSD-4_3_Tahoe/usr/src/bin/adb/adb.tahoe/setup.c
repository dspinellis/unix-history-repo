#ifndef lint
static	char sccsid[] = "@(#)setup.c	1.1 (Berkeley) 2/25/86";
#endif

/*
 * adb - routines to read a.out+core at startup
 */
#include "defs.h"
#include <frame.h>
#include <ctype.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <machine/rpb.h>

off_t	datbas;			/* offset of the base of the data segment */
off_t	stksiz;			/* stack size in the core image */
INT	sigcode;	/* belongs in head.h */

char	*symfil	= "a.out";
char	*corfil	= "core";

setsym()
{
	off_t loc;
	struct exec hdr;
	register struct nlist *sp;
	int ssiz;
	char *strtab, *malloc();

	fsym = getfile(symfil, 1);
	txtmap.ufd = fsym;
	if (read(fsym, (char *)&hdr, sizeof hdr) != sizeof hdr ||
	    N_BADMAG(hdr)) {
		txtmap.e1 = MAXFILE;
		return;
	}
	filhdr = hdr;
	loc = filhdr.a_text+filhdr.a_data;
	txtmap.f1 = txtmap.f2 = N_TXTOFF(filhdr);
	txtmap.b1 = 0;
	switch (filhdr.a_magic) {

	case OMAGIC:
		txtmap.b1 = txtmap.e1 = 0;
		txtmap.b2 = datbas = 0;
		txtmap.e2 = loc;
		break;

	case ZMAGIC:
	case NMAGIC:
		txtmap.e1 = filhdr.a_text;
		txtmap.b2 = datbas = round(filhdr.a_text, PAGSIZ);
		txtmap.e2 = datbas + filhdr.a_data;
		txtmap.f2 += txtmap.e1;
	}
	loc = N_SYMOFF(filhdr);
	symtab = (struct nlist *) malloc(filhdr.a_syms);
	esymtab = &symtab[filhdr.a_syms / sizeof (struct nlist)];
	if (symtab == NULL)
		goto nospac;
	lseek(fsym, loc, L_SET);
	if (filhdr.a_syms == 0)
		goto nosymt;
	/* SHOULD SQUISH OUT STABS HERE!!! */
	if (read(fsym, symtab, filhdr.a_syms) != filhdr.a_syms)
		goto readerr;
	if (read(fsym, &ssiz, sizeof (ssiz)) != sizeof (ssiz))
		goto oldfmt;
	strtab = malloc(ssiz);
	if (strtab == 0)
		goto nospac;
	*(int *)strtab = ssiz;
	ssiz -= sizeof (ssiz);
	if (read(fsym, strtab + sizeof (ssiz), ssiz) != ssiz)
		goto readerr;
	for (sp = symtab; sp < esymtab; sp++)
		if (sp->n_un.n_strx)
			/* SHOULD PERFORM RANGE CHECK HERE */
			sp->n_un.n_name = strtab + sp->n_un.n_strx;
nosymt:
	if (INKERNEL(filhdr.a_entry)) {
		txtmap.b1 += KERNOFF;
		txtmap.e1 += KERNOFF;
		txtmap.b2 += KERNOFF;
		txtmap.e2 += KERNOFF;
	}
	return;
readerr:
	printf("Error reading symbol|string table\n");
	exit(1);
nospac:
	printf("Not enough space for symbol|string table\n");
	exit(1);
oldfmt:
	printf("Old format a.out - no string table\n");
	exit(1);
}

setcor()
{

	fcor = datmap.ufd = getfile(corfil,2);
	if (kernel && fcor != -1 && INKERNEL(filhdr.a_entry)) {
		struct stat stb;

		kcore = 1;
		fstat(fcor, &stb);
		datmap.b1 = 0;
		datmap.e1 = -1;
		if (kernel == 0 && (stb.st_mode & S_IFREG))
			datmap.b1 = 0xc0000000;
		lookup("_Sysmap");
		sbr = (struct pte *)cursym->n_value;
		lookup("_Syssize");
		slr = cursym->n_value;
		printf("sbr %X slr %X\n", sbr, slr);
		lookup("_masterpaddr");
		physrw(fcor, cursym->n_value&~0xc0000000, &masterpcbb, 1);
		masterpcbb = (masterpcbb&PG_PFNUM)*NBPG;
		getpcb();
		findstackframe();
		return;
	}
	if (read(fcor, (char *)&u, ctob(UPAGES))!=ctob(UPAGES) ||
	   !INUDOT(u.u_pcb.pcb_ksp) || !INSTACK(u.u_pcb.pcb_usp)) {
		datmap.e1 = MAXFILE;
		return;
	}
	signo = u.u_arg[0];
	sigcode = u.u_code;
	filhdr.a_text = ctob(u.u_tsize);
	filhdr.a_data = ctob(u.u_dsize);
	stksiz = ctob(u.u_ssize);
	switch (filhdr.a_magic) {

	case OMAGIC:
		datmap.b1 = 0;
		datmap.e1 = filhdr.a_text+filhdr.a_data;
		datmap.f2 = ctob(UPAGES) + datmap.e1;
		break;

	case NMAGIC:
	case ZMAGIC:
		datmap.b1 = round(filhdr.a_text, PAGSIZ);
		datmap.e1 = datmap.b1 + filhdr.a_data;
		datmap.f2 = ctob(UPAGES) + filhdr.a_data;
		break;
	}
	datbas = datmap.b1;
	datmap.f1 = ctob(UPAGES);
	datmap.b2 = MAXSTOR - stksiz;
	datmap.e2 = MAXSTOR;
}

getpcb()
{

	lseek(fcor, KVTOPH(masterpcbb), L_SET);
	read(fcor, &pcb, sizeof (struct pcb));
	printf("p0br %X p0lr %X p2br %X p2lr %X\n",
	    pcb.pcb_p0br, pcb.pcb_p0lr, pcb.pcb_p2br, pcb.pcb_p2lr);
}

caddr_t	rpb, erpb;
caddr_t	intstack, eintstack;
caddr_t	ustack, eustack;
struct	frame *getframe();
struct	frame *checkintstack();

/*
 * Find the current stack frame when debugging the kernel.
 * If we're looking at a crash dump and this was not a ``clean''
 * crash, then we must search the interrupt stack carefully
 * looking for a valid frame.
 */
findstackframe()
{
	char *panicstr, buf[256];
	register struct frame *fp;
	caddr_t addr;
	register char *cp;
	int mask;

	if (lookup("_panicstr") == 0)
		return;
	lseek(fcor, cursym->n_value&~0xc0000000, L_SET);
	read(fcor, &panicstr, sizeof (panicstr));
	if (panicstr == 0)
		return;
	lseek(fcor, ((off_t)panicstr)&~0xc0000000, L_SET);
	read(fcor, buf, sizeof (buf));
	for (cp = buf; cp < &buf[sizeof (buf)] && *cp; cp++)
		if (!isascii(*cp) || (!isprint(*cp) && !isspace(*cp)))
			*cp = '?';
	if (*cp)
		*cp = '\0';
	printf("panic: %s\n", buf);
	/*
	 * After a panic, look at the top of the rpb stack to
	 * find a stack frame.  If this was a clean crash,
	 * i.e. one which left the interrupt and kernel stacks
	 * in a reasonable state, then we should find a pointer
	 * to the proper stack frame here (at location intstack-4).
	 * If we don't find a reasonable frame here, then we
	 * must search down through the interrupt stack.
	 */
	intstack = (caddr_t)lookup("_intstack")->n_value;
#define	NISP	3			/* from locore.s */
	eintstack = intstack + NISP*NBPG;
	rpb = (caddr_t)lookup("_rsstk")->n_value;	/* XXX */
	erpb = rpb + NBPG - 2*sizeof (long);
	lookup("_u");
	ustack = 
	    (caddr_t)(cursym->n_value + (int)&((struct user *)0)->u_stack[0]);
	eustack = (caddr_t)(cursym->n_value + ctob(UPAGES));
	physrw(fcor, KVTOPH((int)intstack-2*sizeof (caddr_t)), &addr, 1);
	fp = getframe(fcor, addr);
	if (fp == 0)
		fp = checkintstack();
	/* search kernel stack? */
	if (fp == 0) {
		printf("can't locate stack frame\n");
		return;
	}
	/* probably shouldn't clobber pcb, but for now this is easy */
	pcb.pcb_fp = (int)addr;
	pcb.pcb_pc = fp->fr_savpc;
}

/*
 * Search interrupt stack for a valid frame.
 */
struct frame *
checkintstack(fcor)
{
	char stack[NISP*NBPG];
	off_t off = vtophys(intstack);
	struct frame *fp;
	register caddr_t addr;

	if (off == -1 || lseek(fcor, off, L_SET) != off ||
	    read(fcor, stack, sizeof (stack)) != sizeof (stack))
		return ((struct frame *)0);
	addr = eintstack;
	do {
		addr -= sizeof (caddr_t);
		fp = (struct frame *)&stack[addr - intstack];
	} while (addr >= intstack + sizeof (struct frame) - sizeof (caddr_t) &&
	    !checkframe(fp));
	return (addr < intstack+sizeof (struct frame) ? (struct frame *)0 : fp);
}

/*
 * Get a stack frame and verify it looks like
 * something which might be on a kernel stack.
 */
struct frame *
getframe(fcor, fp)
	int fcor;
	caddr_t fp;
{
	static struct frame frame;
	off_t off;

	if (!kstackaddr(fp) || (off = vtophys(fp)) == -1)
		return ((struct frame *)0);
	off -= sizeof (struct frame) -  sizeof (caddr_t);
	if (lseek(fcor, off, L_SET) != off ||
	    read(fcor, &frame, sizeof (frame)) != sizeof (frame))
		return ((struct frame *)0);
	if (!checkframe(&frame))
		return ((struct frame *)0);
	return (&frame);
}

/*
 * Check a call frame to see if it's ok as
 * a kernel stack frame.
 */
checkframe(fp)
	register struct frame *fp;
{

	if (!kstackaddr(fp->fr_savfp))
		return (0);
	return (within(fp->fr_savpc, txtmap.b1, txtmap.e1));
}

/*
 * Check if an address is in one of the kernel's stacks:
 * interrupt stack, rpb stack (during restart sequence),
 * or u. stack.
 */
kstackaddr(addr)
	caddr_t addr;
{

	return (within(addr, intstack, eintstack) ||
	    within(addr, rpb + sizeof (struct rpb), erpb) ||
	    within(addr, ustack, eustack));
}

create(f)
	char *f;
{
	register int fd;

	fd = creat(f, 0644);
	if (fd < 0)
		return (-1);
	close(fd);
	return (open(f, wtflag));
}

getfile(filnam, cnt)
	char *filnam;
{
	register int fsym;

	if (eqstr(filnam, "-"))
		return (-1);
	fsym = open(filnam, wtflag);
	if (fsym < 0 && xargc > cnt) {
		if (wtflag)
			fsym = create(filnam);
		if (fsym < 0)
			printf("cannot open `%s'\n", filnam);
	}
	return (fsym);
}

setvar()
{

	var[varchk('b')] = datbas;
	var[varchk('d')] = filhdr.a_data;
	var[varchk('e')] = filhdr.a_entry;
	var[varchk('m')] = filhdr.a_magic;
	var[varchk('s')] = stksiz;
	var[varchk('t')] = filhdr.a_text;
}
