#ifndef lint
static	char sccsid[] = "@(#)access.c	4.8 10/13/84";
#endif
/*
 * Adb: access data in file/process address space.
 *
 * The routines in this file access referenced data using
 * the maps to access files, ptrace to access subprocesses,
 * or the system page tables when debugging the kernel,
 * to translate virtual to physical addresses.
 */

#include "defs.h"


MAP		txtmap;
MAP		datmap;
INT		wtflag;
STRING		errflg;
int		errno;

INT		pid;

/*
 * Primitives: put a value in a space, get a value from a space
 * and get a word or byte not returning if an error occurred.
 */
put(addr, space, value) 
    off_t addr; { (void) access(WT, addr, space, value); }

#if vax || pdp11
u_int
get(addr, space)
    off_t addr; { return (access(RD, addr, space, 0)); }

u_int
lchkget(addr, space)
    off_t addr; { u_int w = get(addr, space); chkerr(); return(w); }
#endif

#ifdef mc68000
/*
 * 68000 Unices don't like odd addresses.
 */
u_int
get(addr, space)
    off_t addr;
{
    u_int data;

    if (addr & 1) {
	data = access(RD, addr - 1, space, 0);
	return ((data >> 8) & 0xffff);
    }
    data = access(RD, addr, space, 0);
    return ((data >> 16) & 0xffff);
}

u_int
lget(addr, space)
    off_t addr;
{
    if (addr & 1) {
	u_int data = get(addr, space);
	return (get(addr + 2, space) | (data << 16));
    }
    return (access(RD, addr, space, 0));
}

u_int
lchkget(addr, space)
    off_t addr; { u_int w = lget(addr, space); chkerr(); return(w); }
#endif

#if !pdp11 && !vax && !mc68000
help!
#endif

u_int
chkget(addr, space)
    off_t addr; { u_int w = get(addr, space); chkerr(); return(w); }

u_int
bchkget(addr, space) 
    off_t addr; { return(lobyte(chkget(addr, space))); }

/*
 * Read/write according to mode at address addr in i/d space.
 * Value is quantity to be written, if write.
 *
 * This routine decides whether to get the data from the subprocess
 * address space with ptrace, or to get it from the files being
 * debugged.  
 *
 * When the kernel is being debugged with the -k flag we interpret
 * the system page tables for data space, mapping p0 and p1 addresses
 * relative to the ``current'' process (as specified by its p_addr in
 * <p) and mapping system space addresses through the system page tables.
 */
access(mode, addr, space, value)
	int mode, space, value;
	off_t addr;
{
	int rd = mode == RD;
	int file, w;

	if (space == NSP)
		return(0);
	if (pid) {
		int pmode = (space&DSP ?
		    (rd ? PT_READ_D : PT_WRITE_D) :
		    (rd ? PT_READ_I : PT_WRITE_I));

		w = ptrace(pmode, pid, addr, value);
		if (errno)
			rwerr(space);
		return (w);
	}
	w = 0;
	if (mode==WT && wtflag==0)
		error("not in write mode");
	if (!chkmap(&addr, space))
		return (0);
	file = mapptr(space)->ufd;
	if (kernel && space == DSP) {
		addr = vtophys(addr);
		if (addr == -1)
			return (0);
	}
	if (physrw(file, addr, rd ? &w : &value, rd) < 0)
		rwerr(space);
	return (w);
}

#ifdef vax
/*
 * When looking at kernel data space through /dev/mem or
 * with a core file, do virtual memory mapping.
 */
vtophys(addr)
	off_t addr;
{
	int oldaddr = addr;
	int v;
	struct pte pte;

	addr &= ~0xc0000000;
	v = btop(addr);
	switch (oldaddr&0xc0000000) {

	case 0xc0000000:
	case 0x80000000:
		/*
		 * In system space get system pte.  If
		 * valid or reclaimable then physical address
		 * is combination of its page number and the page
		 * offset of the original address.
		 */
		if (v >= slr)
			goto oor;
		addr = ((long)(sbr+v)) &~ 0x80000000;
		goto simple;

	case 0x40000000:
		/*
		 * In p1 space must not be in shadow region.
		 */
		if (v < pcb.pcb_p1lr)
			goto oor;
		addr = pcb.pcb_p1br+v;
		break;

	case 0x00000000:
		/*
		 * In p0 space must not be off end of region.
		 */
		if (v >= pcb.pcb_p0lr)
			goto oor;
		addr = pcb.pcb_p0br+v;
		break;
	oor:
		errflg = "address out of segment";
		return (-1);
	}
	/*
	 * For p0/p1 address, user-level page table should
	 * be in kernel vm.  Do second-level indirect by recursing.
	 */
	if ((addr & 0x80000000) == 0) {
		errflg = "bad p0br or p1br in pcb";
		return (-1);
	}
	addr = vtophys(addr);
simple:
	/*
	 * Addr is now address of the pte of the page we
	 * are interested in; get the pte and paste up the
	 * physical address.
	 */
	if (physrw(fcor, addr, (int *)&pte, 1) < 0) {
		errflg = "page table botch";
		return (-1);
	}
	/* SHOULD CHECK NOT I/O ADDRESS; NEED CPU TYPE! */
	if (pte.pg_v == 0 && (pte.pg_fod || pte.pg_pfnum == 0)) {
		errflg = "page not valid/reclaimable";
		return (-1);
	}
	return (ptob(pte.pg_pfnum) + (oldaddr & PGOFSET));
}
#endif

#ifdef hp300

#ifdef NEWVM
#ifndef btop
#define btop	hp300_btop
#endif
#ifndef ptob
#define ptob	hp300_ptob
#endif
#endif

int is68040 = 0;

/*
 * When looking at kernel data space through /dev/mem or
 * with a core file, do virtual memory mapping.
 */
vtophys(addr)
	off_t addr;
{
	int v;
	struct pte pte;
	int oldaddr = addr;

	if (INKERNEL(addr)) {
		/*
		 * In system space get system pte.  If
		 * valid or reclaimable then physical address
		 * is combination of its page number and the page
		 * offset of the original address.
		 */
#ifdef NEWVM
		/* locate PTE page in segtab */
		if (is68040) {
			int steaddr;

			steaddr = KVTOPH((int)(sbr+(addr>>SG4_SHIFT1)));
			lseek(fcor, (off_t)steaddr, 0);
			read(fcor, &pte, sizeof pte);
#if 0
			printf("va %X: ste1 %X@%X",
			       addr, *(int *)&pte, steaddr);
#endif
			if (*(int *)&pte == SG_NV)
				goto bad;
			steaddr = (int)(((int *)(*(int *)&pte & SG4_ADDR1)) +
					((addr & SG4_MASK2) >> SG4_SHIFT2));
			physrw(fcor, steaddr, (int *)&pte, 1);
#if 0
			printf(" ste2 %X@%X", *(int *)&pte, steaddr);
#endif
		} else {
			lseek(fcor,
			      (off_t)KVTOPH((int)(sbr+(addr>>SG_ISHIFT))), 0);
			read(fcor, &pte, sizeof pte);
#if 0
			printf("va %X: ste %X@%X",
			       addr, *(int *)&pte, sbr+(addr>>SG_ISHIFT));
#endif
		}
		/* see if STE is valid */
		if (*(int *)&pte == SG_NV) {
bad:
			errflg = "address out of segment";
			return(-1);
		}
		/* desired PTE is within that page */
		v = btop(addr & SG_PMASK);
		addr = (pte.pg_pfnum << PGSHIFT) + (v * sizeof pte);
#else
		v = btop(addr - KERNOFF);
		addr = (long)(sbr+v) + lowram;
#endif
	}
	else if (INUDOT(addr)) {
		addr -= kernudot;
		addr += masterpcbb;
		return(vtophys(addr));
	}
	else /* user space */ {
#ifdef NEWVM
		errflg = "cannot translate user addresses";
		return (-1);
#else
		v = btop(addr);
		/*
		 * Must be within bounds of p0 or p1 regions.
		 */
		if (v < pcb.pcb_p0lr)
			addr = pcb.pcb_p0br+v;
		else if (v >= pcb.pcb_p1lr)
			addr = pcb.pcb_p1br+v;
		else {
			errflg = "address out of segment";
			return (-1);
		}
		/*
		 * For p0/p1 address, user-level page table should
		 * be in kernel vm.  Do second-level indirect by recursing.
		 */
		if (!INKERNEL(addr)) {
			errflg = "bad p0br or p1br in pcb";
			return (-1);
		}
		addr = vtophys(addr);
#endif
	}
	/*
	 * Addr is now address of the pte of the page we
	 * are interested in; get the pte and paste up the
	 * physical address.
	 */
	if (physrw(fcor, addr, (int *)&pte, 1) < 0) {
		errflg = "page table botch";
		return (-1);
	}
	if (pte.pg_v == 0 &&
#ifdef NEWVM
	    pte.pg_pfnum == 0
#else
	    (pte.pg_fod || pte.pg_pfnum == 0)
#endif
	) {
		errflg = "page not valid/reclaimable";
		return (-1);
	}
#if 0
	printf(" -> pte %X@%X -> addr %X\n",
	       *(int *)&pte, addr, ptob(pte.pg_pfnum) + (oldaddr & PGOFSET));
#endif
	return (ptob(pte.pg_pfnum) + (oldaddr & PGOFSET));
}
#endif

#if !vax && !hp300
help!
#endif

rwerr(space)
	int space;
{

	if (space & DSP)
		errflg = "data address not found";
	else if (space & PSP)
		errflg = "physical address not found";
	else
		errflg = "text address not found";
}

physrw(file, addr, aw, rd)
	off_t addr;
	int *aw, rd;
{

#ifdef hp300
	if (kcore && !kmem && file == fcor)
		addr -= lowram;
#endif
	if (longseek(file,addr)==0 ||
	    (rd ? read(file,aw,sizeof(int)) : write(file,aw,sizeof(int))) < 1)
		return (-1);
	return (0);
}

chkmap(addr,space)
	REG L_INT	*addr;
	REG INT		space;
{
	REG MAPPTR amap;
	amap = mapptr(space);
	IF space&STAR ORF !within(*addr,amap->b1,amap->e1)
	THEN IF within(*addr,amap->b2,amap->e2)
	     THEN *addr += (amap->f2)-(amap->b2);
	     ELSE rwerr(space); return(0);
	     FI
	ELSE *addr += (amap->f1)-(amap->b1);
	FI
	return(1);
}

within(addr,lbd,ubd)
    u_int addr, lbd, ubd; { return(addr>=lbd && addr<ubd); }

longseek(f, a)
    off_t a; { return(lseek(f, a, 0) != -1); }

#ifdef NEWVM
#undef lseek
#undef off_t
Lseek(f, o, w)
    int f, w; Ooff_t o; { return(lseek(f, (off_t)o, w)); }
#endif
