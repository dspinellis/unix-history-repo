#
/*
 *
 *	UNIX debugger
 *
 */

#include "defs.h"
SCCSID(@(#)access.c	2.2);


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
	IF mode==WT ANDF wtflag==0
	THEN	error("not in write mode");
	FI
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
#endif

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
