static	char sccsid[] = "@(#)print.c 4.11 5/2/85";
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
	"p1lr",	P1LR,	&pcb.pcb_p1lr,
	"p1br",	P1BR,	&pcb.pcb_p1br,
	"p0lr",	P0LR,	&pcb.pcb_p0lr,
	"p0br",	P0BR,	&pcb.pcb_p0br,
	"ksp",	KSP,	&pcb.pcb_ksp,
	"esp",	ESP,	&pcb.pcb_esp,
	"ssp",	SSP,	&pcb.pcb_ssp,
	"psl",	PSL,	&pcb.pcb_psl,
	"pc",	PC,	&pcb.pcb_pc,
	"usp",	USP,	&pcb.pcb_usp,
	"fp",	FP,	&pcb.pcb_fp,
	"ap",	AP,	&pcb.pcb_ap,
	"r11",	R11,	&pcb.pcb_r11,
	"r10",	R10,	&pcb.pcb_r10,
	"r9",	R9,	&pcb.pcb_r9,
	"r8",	R8,	&pcb.pcb_r8,
	"r7",	R7,	&pcb.pcb_r7,
	"r6",	R6,	&pcb.pcb_r6,
	"r5",	R5,	&pcb.pcb_r5,
	"r4",	R4,	&pcb.pcb_r4,
	"r3",	R3,	&pcb.pcb_r3,
	"r2",	R2,	&pcb.pcb_r2,
	"r1",	R1,	&pcb.pcb_r1,
	"r0",	R0,	&pcb.pcb_r0,
};

char		lastc;

INT		fcor;
STRING		errflg;
INT		signo;
INT		sigcode;


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
		"signal 16",
		"stop (signal)",
		"stop (tty)",
		"continue (signal)",
		"child termination",
		"stop (tty input)",
		"stop (tty output)",
		"input available (signal)",
		"cpu timelimit",
		"file sizelimit",
		"signal 26",
		"signal 27",
		"signal 28",
		"signal 29",
		"signal 30",
		"signal 31",
};

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
	register struct nlist *sp;
	INT		stack;
	INT		ntramp;

	IF cntflg==0 THEN cntval = -1; FI

	switch (modif) {

	    case '<':
		IF cntval == 0
		THEN	WHILE readchar() != EOR
			DO OD
			lp--;
			break;
		FI
		IF rdc() == '<'
		THEN	stack = 1;
		ELSE	stack = 0; lp--;
		FI
		/* fall thru... */

	    case '>':
		{CHAR		file[64];
		CHAR		Ifile[128];
		extern CHAR	*Ipath;
		INT		index;

		index=0;
		IF rdc()!=EOR
		THEN	REP file[index++]=lastc;
			    IF index>=63 THEN error(LONGFIL); FI
			PER readchar()!=EOR DONE
			file[index]=0;
			IF modif=='<'
			THEN	IF Ipath THEN
					strcpy(Ifile, Ipath);
					strcat(Ifile, "/");
					strcat(Ifile, file);
				FI
				IF strcmp(file, "-")!=0
				THEN	iclose(stack, 0);
					infile=open(file,0);
					IF infile<0
					THEN	infile=open(Ifile,0);
					FI
				ELSE	lseek(infile, 0L, 0);
				FI
				IF infile<0
				THEN	infile=0; error(NOTOPEN);
				ELSE	IF cntflg
					THEN	var[9] = cntval;
					ELSE	var[9] = 1;
					FI
				FI
			ELSE	oclose();
				outfile=open(file,1);
				IF outfile<0
				THEN	outfile=creat(file,0644);
#ifndef EDDT
				ELSE	lseek(outfile,0L,2);
#endif
				FI
			FI

		ELSE	IF modif == '<'
			THEN	iclose(-1, 0);
			ELSE	oclose();
			FI
		FI
		lp--;
		}
		break;

	    case 'p':
		IF kernel == 0
		THEN	printf("not debugging kernel\n");
		ELSE	IF adrflg
			THEN	int pte = access(RD, dot, DSP, 0);
				masterpcbb = (pte&PG_PFNUM)*512;
			FI
			getpcb();
		FI
		break;

	    case 'd':
		IF adrflg
		THEN	IF adrval < 2 ORF adrval > 16
			THEN	printf("must have 2 <= radix <= 16");
				break;
			FI
			printf("radix=%d base ten",radix=adrval);
		FI
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
		DO	IF var[i]
			THEN printc((i<=9 ? '0' : 'a'-10) + i);
				printf(" = %x\n",var[i]);
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
		THEN	frame=adrval;
			word=get(adrval+6,DSP)&0xFFFF;
			IF word&0x2000
			THEN	/* 'calls', can figure out argp */
				argp=adrval+20+((word>>14)&3); word &= 0xFFF;
				WHILE word
				DO	IF word&1
					THEN	argp+=4;
					FI
					word>>=1;
				OD
			ELSE	/* 'callg', can't tell where argp is */
				argp=frame;
			FI
			callpc=get(frame+16,DSP);
		ELIF kcore
		THEN	argp = pcb.pcb_ap;
			frame = pcb.pcb_fp;
			callpc = pcb.pcb_pc;
		ELSE	argp= *(ADDR *)(((ADDR)&u)+AP);
			frame= *(ADDR *)(((ADDR)&u)+FP);
			callpc= *(ADDR *)(((ADDR)&u)+PC);
		FI
		lastframe=0;
		ntramp = 0;
		WHILE cntval--
		DO	char *name;
			chkerr();
			/* if in extended pcb must be signal trampoline code */
			IF KERNOFF - ctob(UPAGES) < callpc ANDF
			    (unsigned)callpc < KERNOFF
			THEN	name = "sigtramp";
				ntramp++;
			ELSE	ntramp = 0;
				findsym(callpc,ISYM);
				IF cursym ANDF
				    !strcmp(cursym->n_un.n_name, "start")
				THEN break;
				FI
				IF cursym
				THEN name = cursym->n_un.n_name;
				ELSE name = "?";
				FI
			FI
			printf("%s(", name);
			narg = get(argp,DSP); IF narg&~0xFF THEN narg=0; FI
			LOOP	IF narg==0 THEN break; FI
				printf("%R", get(argp += 4, DSP));
				IF --narg!=0 THEN printc(','); FI
			POOL
			IF ntramp == 1
			THEN callpc=get(frame+92, DSP);
			ELSE callpc=get(frame+16, DSP);
			FI
			IF callpc != 0
			THEN	prints(") from ");
				psymoff(callpc, ISYM, "\n");
			ELSE	prints(")\n");
			FI

			IF modif=='C'
			THEN	WHILE localsym(frame,argp)
				DO	word=get(localval,DSP);
					printf("%8t%s:%10t",
					    cursym->n_un.n_name);
					IF errflg
					THEN prints("?\n"); errflg=0;
					ELSE printf("%R\n",word);
					FI
				OD
			FI

			argp=get(frame+8, DSP);
			lastframe=frame;
			frame=get(frame+12, DSP)&EVEN;
			IF frame==0 THEN break; FI
			IF !adrflg ANDF !INSTACK(frame)
			THEN	IF !kcore ORF !kstackaddr(frame)
				THEN break;
				FI
			FI
		OD
		break;

	    /*print externals*/
	    case 'e': case 'E':
		FOR sp = symtab; sp < esymtab; sp++
		DO	IF sp->n_type == (N_DATA|N_EXT) ORF
			   sp->n_type == (N_BSS|N_EXT)
			THEN printf("%s:%12t%R\n", sp->n_un.n_name,
				    get(sp->n_value,DSP));
			FI
		OD
		break;

	    case 'a': case 'A':
		error("No algol 68 on VAX");
		/*NOTREACHED*/

	    /*print breakpoints*/
	    case 'b': case 'B':
		printf("breakpoints\ncount%8tbkpt%24tcommand\n");
		FOR bkptr=bkpthead; bkptr; bkptr=bkptr->nxtbkpt
		DO	IF bkptr->flag
			THEN	printf("%-8.8d",bkptr->count);
				psymoff(leng(bkptr->loc),ISYM,"%24t");
				comptr=bkptr->comm;
				WHILE *comptr DO printc(*comptr++); OD
			FI
		OD
		break;

	    default:
		error(BADMOD);
	}

}

printmap(s,amap)
STRING	s; MAP *amap;
{
	int file;
	file=amap->ufd;
	printf("%s%12t`%s'\n", s,
	    (file<0 ? "-" : (file==fcor ? corfil : symfil)));
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
	DO	v = kcore ? *p->rkern : *(ADDR *)(((ADDR)&u)+p->roffs);
		printf("%s%6t%R %16t", p->rname, v);
		valpr(v,(p->roffs==PC?ISYM:DSYM));
		printc(EOR);
	OD
	printpc();
}

getreg(regnam) {
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
			ELSE
				int i = kcore ? (int)p->rkern : p->roffs;
				return (i);
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

char	*illinames[] = {
	"reserved addressing fault",
	"priviliged instruction fault",
	"reserved operand fault"
};
char	*fpenames[] = {
	0,
	"integer overflow trap",
	"integer divide by zero trap",
	"floating overflow trap",
	"floating/decimal divide by zero trap",
	"floating underflow trap",
	"decimal overflow trap",
	"subscript out of range trap",
	"floating overflow fault",
	"floating divide by zero fault",
	"floating undeflow fault"
};

sigprint()
{
	IF (signo>=0) ANDF (signo<sizeof signals/sizeof signals[0])
	THEN prints(signals[signo]); FI
	switch (signo) {

	case SIGFPE:
		IF (sigcode > 0 &&
		    sigcode < sizeof fpenames / sizeof fpenames[0]) THEN
			prints(" ("); prints(fpenames[sigcode]); prints(")");
		FI
		break;

	case SIGILL:
		IF (sigcode >= 0 &&
		    sigcode < sizeof illinames / sizeof illinames[0]) THEN
			prints(" ("); prints(illinames[sigcode]); prints(")");
		FI
		break;
	}
}
