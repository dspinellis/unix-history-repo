#
/*
 *
 *	UNIX debugger
 *
 */

#include "defs.h"
SCCSID(@(#)print.c	2.9);
#include "a.out.h"
struct user u;


MSG		LONGFIL;
MSG		NOTOPEN;
MSG		A68BAD;
MSG		A68LNK;
MSG		BADMOD;

MAP		txtmap;
MAP		datmap;

SYMTAB		symbol;
ADDR		lastframe;
ADDR		callpc;

INT		infile;
INT		outfile;
CHAR		*lp;
L_INT		maxoff;
L_INT		maxpos;
INT		radix;

/* symbol management */
L_INT		localval;

/* breakpoints */
BKPTR		bkpthead;

REGLIST reglist [] = {
		"p1lr", P1LR,
		"p1br",P1BR,
		"p0lr", P0LR,
		"p0br",P0BR,
		"ksp",KSP,
		"esp",ESP,
		"ssp",SSP,
		"psl", PSL,
		"pc", PC,
		"usp",USP,
		"fp", FP,
		"ap", AP,
		"r11", R11,
		"r10", R10,
		"r9", R9,
		"r8", R8,
		"r7", R7,
		"r6", R6,
		"r5", R5,
		"r4", R4,
		"r3", R3,
		"r2", R2,
		"r1", R1,
		"r0", R0,
};

char		lastc;

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

STRING		signals[] = {
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
#define MAXSIG 15




/* general printing routines ($) */

printtrace(modif)
{
	INT		narg, i, stat, name, limit;
	POS		dynam;
	REG BKPTR	bkptr;
	CHAR		hi, lo;
	ADDR		word;
	STRING		comptr;
	ADDR		argp, frame, link;
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
#ifndef EDDT
				ELSE	lseek(outfile,0L,2);
#endif
				FI
			FI

		FI
		lp--;
		}
		break;

	    case 'd':
		if (adrflg) {
			if (adrval<2 || adrval>16) {printf("must have 2 <= radix <= 16"); break;}
			printf("radix=%d base ten",radix=adrval);
		}
		break;

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

	    case 'c': case 'C':
		IF adrflg
		THEN frame=adrval;
			word=get(adrval+6,DSP)&0xFFFF;
			IF word&0x2000
			THEN /* 'calls', can figure out argp */
				argp=adrval+20+((word>>14)&3); word &= 0xFFF;
				WHILE word DO IF word&1 THEN argp+=4; FI word>>=1; OD
			ELSE /* 'callg', can't tell where argp is */ argp=frame;
			FI
			callpc=get(frame+16,DSP);
		ELSE argp= *(ADDR *)(((ADDR)&u)+AP);
			frame= *(ADDR *)(((ADDR)&u)+FP);
			callpc= *(ADDR *)(((ADDR)&u)+PC);
		FI
		lastframe=0;
		WHILE cntval--
		DO	chkerr();
			printf("%.8s(", findsym(callpc,ISYM)==-1 ? "?":symbol.symc);
			narg = get(argp,DSP); IF narg&~0xFF THEN narg=0; FI
			LOOP IF narg==0 THEN break; FI
				printf("%R", get(argp += 4, DSP));
				IF --narg!=0 THEN printc(','); FI
			POOL
			prints(")\n");

			IF modif=='C'
			THEN WHILE localsym(frame,argp)
			     DO word=get(localval,DSP);
				printf("%8t%.8s:%10t", symbol.symc);
				IF errflg THEN prints("?\n"); errflg=0; ELSE printf("%R\n",word); FI
			     OD
			FI

			callpc=get(frame+16, DSP);
			argp=get(frame+8, DSP);
			lastframe=frame;
			frame=get(frame+12, DSP)&EVEN;
			IF frame==0 ORF (!adrflg ANDF (frame&0xF0000000)!=0x70000000)
			THEN break;
			FI
		OD
		break;

#ifndef EDDT
	    /*print externals*/
	    case 'e': case 'E':
		symset();
		WHILE symp=symget()
		DO chkerr();
		   IF symp->symf==(N_DATA|N_EXT) ORF symp->symf==(N_BSS|N_EXT)
		   THEN printf("%.8s:%12t%R\n", symp->symc, get(symp->symv,DSP));
		   FI
		OD
		break;

	    case 'a': case 'A':
		frame=(adrflg ? adrval : *(ADDR *)(((ADDR)&u)+FP));

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
#endif

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
	printf("b1 = %-16R",amap->b1);
	printf("e1 = %-16R",amap->e1);
	printf("f1 = %-16R",amap->f1);
	printf("\nb2 = %-16R",amap->b2);
	printf("e2 = %-16R",amap->e2);
	printf("f2 = %-16R",amap->f2);
	printc(EOR);
}

printregs()
{
	REG REGPTR	p;
	L_INT		v;

	FOR p=reglist; p < &reglist[24]; p++
	DO	printf("%s%6t%R %16t", p->rname, v= *(ADDR *)(((ADDR)&u)+p->roffs));
		valpr(v,(p->roffs==PC?ISYM:DSYM));
		printc(EOR);
	OD
	printpc();
}

getreg(regnam)
{
	REG REGPTR	p;
	REG STRING	regptr;
	CHAR	*olp;
	CHAR		regnxt;

	olp=lp;
	FOR p=reglist; p < &reglist[24]; p++
	DO	regptr=p->rname;
		IF (regnam == *regptr++)
		THEN
			WHILE *regptr
			DO IF (regnxt=readchar()) != *regptr++
				THEN --regptr; break;
				FI
			OD
			IF *regptr
			THEN lp=olp;
			ELSE return(p->roffs);
			FI
		FI
	OD
	lp=olp;
	return(0);
}

printpc()
{
	dot= *(ADDR *)(((ADDR)&u)+PC);
	psymoff(dot,ISYM,":%16t"); printins(0,ISP,chkget(dot,ISP));
	printc(EOR);
}

sigprint()
{
	IF (signo>=0) ANDF (signo<=MAXSIG) THEN prints(signals[signo]); FI
}

