#
/*
 *
 *	UNIX debugger
 *
 */

#include "defs.h"


MSG		LONGFIL;
MSG		NOTOPEN;
MSG		A68BAD;
MSG		A68LNK;
MSG		BADMOD;

MAP		txtmap;
MAP		datmap;

SYMTAB		symbol;
INT		lastframe;
INT		callpc;

INT		infile;
INT		outfile;
CHAR		*lp;
INT		maxoff;
INT		maxpos;
INT		octal;

/* symbol management */
L_INT		localval;

/* breakpoints */
BKPTR		bkpthead;

REGLIST reglist [] {
		"ps", ps,
		"pc", pc,
		"sp", sp,
		"r5", r5,
		"r4", r4,
		"r3", r3,
		"r2", r2,
		"r1", r1,
		"r0", r0,
};

INT		frnames[] { 0, 3, 4, 5, 1, 2 };

char		lastc;
POS		corhdr[];
POS		*endhdr;

INT		fcor;
STRING		errflg;
INT		signo;


L_INT		dot;
L_INT		var[];
STRING		symfil;
STRING		corfil;
INT		pid;
L_INT		adrval;
INT		adrflg;
L_INT		cntval;
INT		cntflg;

STRING		signals[] {
		"",
		"hangup",
		"interrupt",
		"quit",
		"illegal instruction",
		"trace/BPT",
		"IOT",
		"EMT",
		"floating exception",
		"killed",
		"bus error",
		"memory fault",
		"bad system call",
		"broken pipe",
		"alarm call",
		"terminated",
};




/* general printing routines ($) */

printtrace(modif)
{
	INT		narg, i, stat, name, limit;
	POS		dynam;
	REG BKPTR	bkptr;
	CHAR		hi, lo;
	INT		word;
	STRING		comptr;
	L_INT		argp, frame, link;
	SYMPTR		symp;

	IF cntflg==0 THEN cntval = -1; FI

	switch (modif) {

	    case '<':
	    case '>':
		{CHAR		file[64];
		INT		index;

		index=0;
		IF modif=='<'
		THEN	iclose();
		ELSE	oclose();
		FI
		IF rdc()!=EOR
		THEN	REP file[index++]=lastc;
			    IF index>=63 THEN error(LONGFIL); FI
			PER readchar()!=EOR DONE
			file[index]=0;
			IF modif=='<'
			THEN	infile=open(file,0);
				IF infile<0
				THEN	infile=0; error(NOTOPEN);
				FI
			ELSE	outfile=open(file,1);
				IF outfile<0
				THEN	outfile=creat(file,0644);
				ELSE	lseek(outfile,0L,2);
				FI
			FI

		FI
		lp--;
		}
		break;

	    case 'o':
		octal = TRUE; break;

	    case 'd':
		octal = FALSE; break;

	    case 'q': case 'Q': case '%':
		done();

	    case 'w': case 'W':
		maxpos=(adrflg?adrval:MAXPOS);
		break;

	    case 's': case 'S':
		maxoff=(adrflg?adrval:MAXOFF);
		break;

	    case 'v': case 'V':
		prints("variables\n");
		FOR i=0;i<=35;i++
		DO IF var[i]
		   THEN printc((i<=9 ? '0' : 'a'-10) + i);
			printf(" = %Q\n",var[i]);
		   FI
		OD
		break;

	    case 'm': case 'M':
		printmap("? map",&txtmap);
		printmap("/ map",&datmap);
		break;

	    case 0: case '?':
		IF pid
		THEN printf("pcs id = %d\n",pid);
		ELSE prints("no process\n");
		FI
		sigprint(); flushbuf();

	    case 'r': case 'R':
		printregs();
		return;

	    case 'f': case 'F':
		printfregs(modif=='F');
		return;

	    case 'c': case 'C':
		frame=(adrflg?adrval:endhdr[r5])&EVEN; lastframe=0;
		callpc=(adrflg?get(frame+2,DSP):endhdr[pc]);
		WHILE cntval--
		DO	chkerr();
			narg = findroutine(frame);
			printf("%.8s(", symbol.symc);
			argp = frame+4;
			IF --narg >= 0
			THEN	printf("%o", get(argp, DSP));
			FI
			WHILE --narg >= 0
			DO	argp += 2;
				printf(",%o", get(argp, DSP));
			OD
			prints(")\n");

			IF modif=='C'
			THEN WHILE localsym(frame)
			     DO word=get(localval,DSP);
				printf("%8t%.8s:%10t", symbol.symc);
				IF errflg THEN prints("?\n"); errflg=0; ELSE printf("%o\n",word); FI
			     OD
			FI

			lastframe=frame;
			frame=get(frame, DSP)&EVEN;
			IF frame==0 THEN break; FI
		OD
		break;

	    /*print externals*/
	    case 'e': case 'E':
		symset();
		WHILE (symp=symget())
		DO chkerr();
		   IF (symp->symf)==043 ORF (symp->symf)==044
		   THEN printf("%.8s:%12t%o\n", symp->symc, get(leng(symp->symv),DSP));
		   FI
		OD
		break;

	    case 'a': case 'A':
		frame=(adrflg ? adrval : endhdr[r4]);

		WHILE cntval--
		DO chkerr();
		   stat=get(frame,DSP); dynam=get(frame+2,DSP); link=get(frame+4,DSP);
		   IF modif=='A'
		   THEN printf("%8O:%8t%-8o,%-8o,%-8o",frame,stat,dynam,link);
		   FI
		   IF stat==1 THEN break; FI
		   IF errflg THEN error(A68BAD); FI

		   IF get(link-4,ISP)!=04767
		   THEN IF get(link-2,ISP)!=04775
			THEN error(A68LNK);
			ELSE /*compute entry point of routine*/
			     prints(" ? ");
			FI
		   ELSE printf("%8t");
		        valpr(name=shorten(link)+get(link-2,ISP),ISYM);
			name=get(leng(name-2),ISP);
			printf("%8t\""); limit=8;
			REP word=get(leng(name),DSP); name += 2;
			    lo=word&LOBYTE; hi=(word>>8)&LOBYTE;
			    printc(lo); printc(hi);
			PER lo ANDF hi ANDF limit-- DONE
			printc('"');
		   FI
		   limit=4; i=6; printf("%24targs:%8t");
		   WHILE limit--
		   DO printf("%8t%o",get(frame+i,DSP)); i += 2; OD
		   printc(EOR);

		   frame=dynam;
		OD
		errflg=0;
		flushbuf();
		break;

	    /*set default c frame*/
	    /*print breakpoints*/
	    case 'b': case 'B':
		printf("breakpoints\ncount%8tbkpt%24tcommand\n");
		FOR bkptr=bkpthead; bkptr; bkptr=bkptr->nxtbkpt
		DO IF bkptr->flag
		   THEN printf("%-8.8d",bkptr->count);
			psymoff(leng(bkptr->loc),ISYM,"%24t");
			comptr=bkptr->comm;
			WHILE *comptr DO printc(*comptr++); OD
		   FI
		OD
		break;

	    default: error(BADMOD);
	}

}

printmap(s,amap)
STRING	s; MAP *amap;
{
	int file;
	file=amap->ufd;
	printf("%s%12t`%s'\n",s,(file<0 ? "-" : (file==fcor ? corfil : symfil)));
	printf("b1 = %-16Q",amap->b1);
	printf("e1 = %-16Q",amap->e1);
	printf("f1 = %-16Q",amap->f1);
	printf("\nb2 = %-16Q",amap->b2);
	printf("e2 = %-16Q",amap->e2);
	printf("f2 = %-16Q",amap->f2);
	printc(EOR);
}

printfregs(longpr)
{
	REG i;
	L_REAL f;

	printf("fpsr	%o\n", corhdr[0].fpsr);
	FOR i=0; i<FRMAX; i++
	DO	IF corhdr[0].fpsr&FD ORF longpr	/* long mode */
		THEN	f = corhdr[0].Lfr[frnames[i]];
		ELSE	f = corhdr[0].Sfr[frnames[i]];
		FI
		printf("fr%-8d%-32.18f\n", i, f);
	OD
}

printregs()
{
	REG REGPTR	p;
	INT		v;

	FOR p=reglist; p < &reglist[9]; p++
	DO	printf("%s%8t%o%8t", p->rname, v=endhdr[p->roffs]);
		valpr(v,(p->roffs==pc?ISYM:DSYM));
		printc(EOR);
	OD
	printpc();
}

getreg(regnam)
{
	REG REGPTR	p;
	REG STRING	regptr;
	CHAR		regnxt;
	regnxt=readchar();
	FOR p=reglist; p<&reglist[9]; p++
	DO	regptr=p->rname;
		IF (regnam == *regptr++) ANDF (regnxt == *regptr)
		THEN	return(p->roffs);
		FI
	OD
	lp--;
	return(0);
}

printpc()
{
	dot=endhdr[pc];
	psymoff(dot,ISYM,":%16t"); printins(0,ISP,chkget(dot,ISP));
	printc(EOR);
}

sigprint()
{
	prints(signals[signo]);
}

