#
/*
 *
 *	UNIX debugger
 *
 */

#include "defs.h"
#include "a.out.h"
SCCSID(@(#)setup.c	2.5);


MSG		BADNAM;
MSG		BADMAG;

MAP		txtmap;
MAP		datmap;
SYMSLAVE	*symvec;
INT		wtflag;
INT		fcor;
INT		fsym;
L_INT		maxfile;
L_INT		maxstor;
L_INT		txtsiz;
L_INT		datsiz;
L_INT		datbas;
L_INT		stksiz;
STRING		errflg;
INT		magic;
L_INT		symbas;
L_INT		symnum;
L_INT		entrypt;

INT		argcount;
INT		signo;
struct user u;

STRING		symfil	= "a.out";
STRING		corfil	= "core";

#define TXTHDRSIZ	(sizeof(txthdr))

#ifndef EDDT
readl(f,p,n) int f,n; long * p;{
#ifndef vax
	int t=0;
	do {t += read(f,&(p->loword),2); t += read(f,&(p->hiword),2); p++;} while (--n);
	return(t);
#else
	return(read(f,p,n*sizeof(long)));
#endif
}
#endif

setsym()
{
#ifndef EDDT
	INT		relflg;
	INT		symval, symflg;
	SYMSLAVE	*symptr;
	SYMPTR		symp;
	TXTHDR		txthdr;

	fsym=getfile(symfil,1);
	txtmap.ufd=fsym;
	IF readl(fsym, txthdr, TXTHDRSIZ/sizeof(txthdr[0]))==TXTHDRSIZ
	THEN	magic=txthdr[0];
		IF magic!=0411 ANDF magic!=0410 ANDF magic!=0407 ANDF magic!=0405
		THEN	magic=0;
		ELSE	symnum=txthdr[4]/SYMTABSIZ;
			txtsiz=txthdr[1];
			datsiz=txthdr[2];
			symbas=txtsiz+datsiz;
			txtmap.b1=0;
			txtmap.e1=(magic==0407?symbas:txtsiz);
			txtmap.f1 = TXTHDRSIZ;
			txtmap.b2=datbas=(magic==0410?round(txtsiz,TXTRNDSIZ):0);
			txtmap.e2=txtmap.b2+(magic==0407?symbas:datsiz);
			txtmap.f2 = TXTHDRSIZ+(magic==0407?0:txtmap.e1);
			entrypt=txthdr[5];
	/*		relflg=txthdr[7];
	/*		IF relflg!=1 THEN symbas =<< 1; FI
	*/
			symbas += txthdr[6]+txthdr[7];
			symbas += TXTHDRSIZ;

			/* set up symvec */
			symvec=sbrk(shorten((1+symnum))*sizeof (SYMSLAVE));
			IF (symptr=symvec)==-1
			THEN	printf("%s\n",BADNAM);
				symptr=symvec=sbrk(sizeof (SYMSLAVE));
			ELSE	symset();
				WHILE (symp=symget()) ANDF errflg==0
				DO 
				    symflg=symp->symf;
				    symptr->valslave=symp->symv;
				    symptr->typslave=SYMTYPE(symflg);
				    IF (symflg & STABTYPES) != 0
				    THEN
					symptr->typslave=XSYM;
				    FI
				    symptr++;
				OD
			FI
			symptr->typslave=ESYM;
		FI
	FI
	IF magic==0 THEN txtmap.e1=maxfile; FI
#endif
}

setcor()
{
#ifndef EDDT
	fcor=getfile(corfil,2);
	datmap.ufd=fcor;
	IF read(fcor, &u, ctob(4))==ctob(4)
	   ANDF (u.u_pcb.pcb_ksp & 0xF0000000L)==0x80000000L
	   ANDF (u.u_pcb.pcb_usp & 0xF0000000L)==0x70000000L
	THEN	signo = u.u_arg[0]&017;
		txtsiz = ctob(u.u_tsize);
		datsiz = ctob(u.u_dsize);
		stksiz = ctob(u.u_ssize);
		datmap.b1 = datbas = (magic==0410?round(txtsiz,TXTRNDSIZ):0);
		datmap.e1=(magic==0407?txtsiz:datmap.b1)+datsiz;
		datmap.f1 = ctob(USIZE);
		datmap.b2 = maxstor-stksiz;
		datmap.e2 = maxstor;
		datmap.f2 = ctob(USIZE)+(magic==0410?datsiz:datmap.e1);
		signo = *(ADDR *)(((ADDR)&u)+ctob(4)-4*4);
		IF magic ANDF magic!=u.u_exdata.ux_mag
		THEN	printf("%s\n",BADMAG);
		FI
	ELSE	datmap.e1 = maxfile;
	FI
#endif
}

#ifndef EDDT
create(f)
STRING	f;
{	int fd;
	IF (fd=creat(f,0644))>=0
	THEN close(fd); return(open(f,wtflag));
	ELSE return(-1);
	FI
}

getfile(filnam,cnt)
STRING	filnam;
{
	REG INT		fsym;

	IF !eqstr("-",filnam)
	THEN	fsym=open(filnam,wtflag);
		IF fsym<0 ANDF argcount>cnt
		THEN	IF wtflag
			THEN	fsym=create(filnam);
			FI
			IF fsym<0
			THEN printf("cannot open `%s'\n", filnam);
			FI
		FI
	ELSE	fsym = -1;
	FI
	return(fsym);
}
#endif
