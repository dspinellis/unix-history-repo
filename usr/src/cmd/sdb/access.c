#
/*
 *
 *	UNIX debugger
 *
 */

#include "head.h"
struct user u;


MSG		BADDAT;
MSG		BADTXT;
MAP		txtmap;
MAP		datmap;
STRING		errflg;
int		errno;

INT		pid;




/* file handling and access routines */

int dmask[5] = {0, 0xff, 0xffff, 0xffffff, 0xffffffff};

/* get data at loc using descriptor format d */
long
getval(loc, d)
ADDR loc;
char d; {
	register int val;
	
	val = get(loc, DSP);
	val &= dmask[dtol(d)];
	return(val);
}

/* put value at loc using descriptor format d */
putval(loc, d, value)
ADDR loc; char d; long value; {
	register long val;
	
	val = get(loc, DSP);
	val = (val & !dmask[dtol(d)]) | (value & dmask[dtol(d)]);
	put(loc, DSP, val);
}

/* put value in named register using descriptor format d */
putreg(reg, d, value)
ADDR reg; char d; long value; {
	register long val;
	
	val = *(ADDR *)(((ADDR)&u)+R0+WORDSIZE*reg);
	val = (val & !dmask[dtol(d)]) | (value & dmask[dtol(d)]);
	*(ADDR *)(((ADDR)&u)+R0+WORDSIZE*reg) = val;
}

put(adr,space,value)
L_INT	adr;
{
	access(WT,adr,space,value);
}

POS	get(adr, space)
L_INT		adr;
{
	return(access(RD,adr,space,0));
}


access(mode,adr,space,value)
L_INT	adr;
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
	IF !chkmap(&adr,space)
	THEN return(0);
	FI
	file=(space&DSP?datmap.ufd:txtmap.ufd);
	IF longseek(file,adr)==0 ORF
	   (rd ? read(file,&w,sizeof(w)) : write(file,&value,sizeof(w))) < 1
	THEN	errflg=(space&DSP?BADDAT:BADTXT);
	FI
	return(w);

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
