#
/*
 *
 *	UNIX debugger
 *
 */

#define dprintf if (0) printf

#include "defs.h"
static	char sccsid[] = "@(#)access.c 4.2 %G%";

MSG		ODDADR;
MSG		BADDAT;
MSG		BADTXT;
MAP		txtmap;
MAP		datmap;
INT		wtflag;
STRING		errflg;
INT		errno;

INT		pid;

/* file handling and access routines */

put(adr,space,value)
#ifndef EDDT
L_INT	adr;
{
	access(WT,adr,space,value);
}
#else
	L_INT *adr; {*adr=value;}
#endif

POS	get(adr, space)
#ifndef EDDT
L_INT		adr;
{
	return(access(RD,adr,space,0));
}
#else
	L_INT *adr; {return(*adr);}
#endif

POS	chkget(n, space)
L_INT		n;
{
#ifndef vax
	REG INT		w;
#else
	REG L_INT	w;
#endif

	w = get(n, space);
	chkerr();
	return(w);
}

POS bchkget(n, space) 
L_INT	n;
{
	return(chkget(n, space) & LOBYTE);
}

#ifndef EDDT
access(mode,adr,space,value)
long	adr;
{
	INT	pmode,rd,file;
	ADDR	w;
	rd = mode==RD;

	IF space == NSP THEN return(0); FI

	IF pid		/* tracing on? */
	THEN
#ifndef vax
		IF adr&01 ANDF !rd THEN error(ODDADR); FI
#endif
	     pmode = (space&DSP?(rd?RDUSER:WDUSER):(rd?RIUSER:WIUSER));
	     w = ptrace(pmode, pid, adr, value);
#ifndef vax
	     IF adr&01
	     THEN w1 = ptrace(pmode, pid, shorten(adr+1), value);
		  w = (w>>8)&LOBYTE | (w1<<8);
	     FI
#endif
	     IF errno
	     THEN errflg = (space&DSP ? BADDAT : BADTXT);
	     FI
	     return(w);
	FI
	w = 0;
	IF mode==WT ANDF wtflag==0
	THEN	error("not in write mode");
	FI
	IF !chkmap(&adr,space)
	THEN return(0);
	FI
	file=(space&DSP?datmap.ufd:txtmap.ufd);
	IF kernel && space == DSP THEN
	    int oadr = adr;
	    int v;
	    adr &= ~0x80000000;
	    IF oadr&0x80000000 THEN		/* system space */
		v = btop(adr);
		dprintf("system addr %X, v %X\n", adr, v);
		IF v >= slr THEN errflg="bad system space addr"; return (0); FI
		adr = vtoa(file, adr);
		IF adr == -1 THEN
		    errflg="sys page table page not valid"; return (0); FI
	    ELIF adr&0x40000000 THEN		/* p1 space */
		v = btop(adr&~0x40000000);
		dprintf("p1 addr %X, v %X, p1br %X p1lr %X\n", adr, v,
		    pcb.pcb_p1br, pcb.pcb_p1lr);
		IF v < pcb.pcb_p1lr THEN
		    errflg="bad p1 space addr"; return (0); FI
		adr = vtoa(file, pcb.pcb_p1br+v);
		IF adr == -1 THEN
		    errflg="p1 page table page not valid"; return (0); FI
		goto get;
	    ELSE				/* p0 space */
		dprintf("p0 addr %X, v %X, p0br %X p0lr %X\n", adr,
		   v, pcb.pcb_p0br, pcb.pcb_p0lr);
		IF v >= pcb.pcb_p0lr THEN
		    errflg="bad p0 space addr"; return (0); FI
		adr = vtoa(file, pcb.pcb_p0br+v);
		IF adr == -1 THEN
		    errflg="p0 page table page not valid"; return (0); FI
get:
		dprintf("addr for pt page %X\n", adr);
		IF physrw(file, adr, &adr, 1) < 0 THEN
		    errflg = "page tables botched"; return (0); FI
		dprintf("user pte value %X\n", adr);
		IF (adr & PG_V) == 0 &&
		    ((adr & PG_FOD) || (adr & PG_PFNUM) == 0) THEN
		    errflg = "user page not resident"; return (0);
		FI
		adr = ((adr & 0xfffff) << 9) | (oadr & 0x1ff);
	    FI
	FI
	IF physrw(file, adr, &w, rd) < 0 THEN
	    errflg=(space&DSP?BADDAT:BADTXT);
	FI
	return(w);
}
#endif

physrw(file, adr, aw, rd)
int *aw;
{

	dprintf("physrw(%X) %s to %X\n", adr, rd ? "read" : "write", aw);
	IF longseek(file,adr)==0 ORF
	   (rd ? read(file,aw,sizeof(int)) : write(file,aw,sizeof(int))) < 1
	THEN	 return (-1);
	FI
	return (0);
}

vtoa(file, va)
unsigned long va;
{
	struct pte pte;

	physrw(file, ((long)(sbr + btop(va&0x7fffffff)))&~0x80000000, &pte, 1);
	dprintf("vtoa got pte %X\n", pte);
	if (pte.pg_v || (pte.pg_fod == 0 && pte.pg_pfnum))
		return (ptob(pte.pg_pfnum) + (va & PGOFSET));
	errflg = "page not resident";
	return (-1);
}
	
chkmap(adr,space)
	REG L_INT	*adr;
	REG INT		space;
{
	REG MAPPTR amap;
	amap=((space&DSP?&datmap:&txtmap));
	IF space&STAR ORF !within(*adr,amap->b1,amap->e1)
	THEN IF within(*adr,amap->b2,amap->e2)
	     THEN *adr += (amap->f2)-(amap->b2);
	     ELSE errflg=(space&DSP?BADDAT:BADTXT); return(0);
	     FI
	ELSE *adr += (amap->f1)-(amap->b1);
	FI
	return(1);
}

within(adr,lbd,ubd)
POS	adr, lbd, ubd;
{
	return(adr>=lbd && adr<ubd);
}

longseek(f, a)
L_INT a;
{
	return(lseek(f,a,0) != -1);
}
