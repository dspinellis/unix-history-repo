#ifndef lint
static	char sccsid[] = "@(#)access.c	4.8 %G%";
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
INT		errno;

INT		pid;

/*
 * Primitives: put a value in a space, get a value from a space
 * and get a word or byte not returning if an error occurred.
 */
put(addr, space, value) 
    off_t addr; { (void) access(WT, addr, space, value); }

u_int
get(addr, space)
    off_t addr; { return (access(RD, addr, space, 0)); };

u_int
chkget(addr, space)
    off_t addr; { u_int w = get(addr, space); chkerr(); return(w); }

u_int
bchkget(addr, space) 
    off_t addr; { return(chkget(addr, space) & LOBYTE); }

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
	file = (space&DSP) ? datmap.ufd : txtmap.ufd;
	if (kernel && space == DSP) {
		addr = vtophys(addr);
		if (addr < 0)
			return (0);
	}
	if (physrw(file, addr, rd ? &w : &value, rd) < 0)
		rwerr(space);
	return (w);
}

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

rwerr(space)
	int space;
{

	if (space & DSP)
		errflg = "data address not found";
	else
		errflg = "text address not found";
}

physrw(file, addr, aw, rd)
	off_t addr;
	int *aw, rd;
{

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
	amap=((space&DSP?&datmap:&txtmap));
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
