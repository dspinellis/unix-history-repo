static	char sccsid[] = "@(#)setup.c 4.3 %G%";
#
/*
 *
 *	UNIX debugger
 *
 */

#include "head.h"

MSG		BADMAG;

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
	TXTHDR		txthdr;

	fsym=getfile(symfil,1);
	txtmap.ufd=fsym;
	IF readl(fsym, txthdr, TXTHDRSIZ/sizeof(txthdr[0]))==TXTHDRSIZ
	THEN	magic=txthdr[0];
		IF magic!=0410 ANDF magic!=0407 ANDF magic!=0412 ANDF magic!=0413
		THEN	magic=0;
		ELSE
			symnum=txthdr[4]/SYMTABSIZ;
			txtsiz=txthdr[1];
			datsiz=txthdr[2];
			symbas=txtsiz+datsiz;
			txtmap.f1=txtmap.f2=TXTHDRSIZ;
			switch (magic) {

			case 0407:
				txtmap.b1=0;
				txtmap.e1=0;
				txtmap.b2=datbas=0;
				txtmap.e2=symbas;
				break;

			case 0413:
				txtmap.f1=txtmap.f2=(CLSIZE*NBPG);
			case 0410:
				txtmap.b1=0;
				txtmap.e1=txtsiz;
				txtmap.b2=datbas=round(txtsiz,TXTRNDSIZ);
				txtmap.e2=datbas+datsiz;
				txtmap.f2+=txtmap.e1;
			}
			entrypt=txthdr[5];
			symbas += txthdr[6]+txthdr[7];
			symbas += magic==0412||magic==0413 ? (CLSIZE*NBPG) : TXTHDRSIZ;
			ststart = symbas;
#ifdef FLEXNAMES
			gstart = ststart+txthdr[4];
#endif
			/* set up symvec */
		FI
	FI
	IF magic==0 THEN txtmap.e1=maxfile; FI
#endif
}

setcor()
{
	fcor=getfile(corfil,2);
	datmap.ufd=fcor;
	IF read(fcor, &u, ctob(UPAGES))==ctob(UPAGES)
#ifndef STD
	   ANDF (u.u_pcb.pcb_ksp & 0xF0000000L)==0x70000000L
#else
	   ANDF (u.u_pcb.pcb_ksp & 0xF0000000L)==0x80000000L
#endif
	   ANDF (u.u_pcb.pcb_usp & 0xF0000000L)==0x70000000L
	THEN	
		signo = u.u_arg[0]&017;
		txtsiz = ctob(u.u_tsize);
		datsiz = ctob(u.u_dsize);
		stksiz = ctob(u.u_ssize);
		datmap.b1 = datbas = (magic==0410?round(txtsiz,TXTRNDSIZ):0);
		if (magic == 0413)
			datmap.b1 = datbas = txtsiz;
		datmap.e1=(magic==0407?txtsiz:datmap.b1)+datsiz;
#ifdef STD
		datmap.f1 = ctob(USIZE);
#else
		datmap.f1 = ctob(UPAGES);
#endif
		datmap.b2 = maxstor-stksiz;
		datmap.e2 = maxstor;
#ifdef STD
		datmap.f2 = ctob(USIZE)+(magic==0410?datsiz:datmap.e1);
#else
		datmap.f2 = ctob(UPAGES)+((magic==0410 || magic == 0413)
					     ? datsiz : datmap.e1);
#endif
		signo = *(ADDR *)(((ADDR)&u)+ctob(UPAGES)-4*sizeof(int));
		IF magic ANDF magic!=u.u_exdata.ux_mag
		THEN	printf("%s\n",BADMAG);
		FI
	ELSE	datmap.e1 = maxfile;
	FI
}

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
