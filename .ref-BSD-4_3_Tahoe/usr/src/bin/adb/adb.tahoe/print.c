#ifndef lint
static	char sccsid[] = "@(#)print.c	1.2 (Berkeley) 7/25/86";
#endif
/*
 *
 *	UNIX debugger
 *
 */
#include "defs.h"

MSG		LONGFIL;
MSG		NOTOPEN;
MSG		BADMOD;

MAP		txtmap;
MAP		datmap;

ADDR		lastframe;
ADDR		callpc;

INT		infile;
INT		outfile;
CHAR		*lp;
ADDR		maxoff;
L_INT		maxpos;
INT		radix;

/* symbol management */
ADDR		localval;

/* breakpoints */
BKPTR		bkpthead;

REGLIST reglist[] = {
	"p2lr",	P2LR,	&pcb.pcb_p2lr,
	"p2br",	P2BR,	(int *)&pcb.pcb_p2br,
	"p0lr",	P0LR,	&pcb.pcb_p0lr,
	"p0br",	P0BR,	(int *)&pcb.pcb_p0br,
	"ksp",	KSP,	&pcb.pcb_ksp,
	"hfs",	HFS,	&pcb.pcb_hfs,
	"psl",	PSL,	&pcb.pcb_psl,
	"pc",	PC,	&pcb.pcb_pc,
	"ach",	ACHI,	&pcb.pcb_ach,
	"acl",	ACLO,	&pcb.pcb_acl,
	"usp",	USP,	&pcb.pcb_usp,
	"fp",	FP,	&pcb.pcb_fp,
	"r12",	R12,	&pcb.pcb_r12,
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
	0
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
L_INT		pid;
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
	REG		narg, i;
	REG BKPTR	bkptr;
	REG	ADDR	word;
	REG	STRING	comptr;
	REG	ADDR	argp, frame;
	register struct nlist *sp;
	INT		stack, ntramp;

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
							/* fall through */
	    case '>':
		{CHAR		file[64];
		CHAR		Ifile[128];
		extern CHAR	*Ipath;
		INT		index;

		index=0;
		IF modif=='<'
		THEN	iclose(stack, 0);
		ELSE	oclose();
		FI
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
				infile=open(file,0);
				IF infile<0 && (infile=open(Ifile,0))<0
				THEN	infile=0; error(NOTOPEN);
				ELSE	IF cntflg
					THEN	var[9] = cntval;
					ELSE	var[9] = 1;
					FI
				FI
			ELSE	outfile=open(file,1);
				IF outfile<0
				THEN	outfile=creat(file,0644);
#ifndef EDDT
				ELSE	lseek(outfile,0L,2);
#endif
				FI
			FI

		ELSE	IF modif == '<'
			THEN	iclose(-1, 0);
			FI
		FI
		lp--;
		}
		break;

	    case 'p':
		IF kernel == 0 THEN
			printf("not debugging kernel\n");
		ELSE
			IF adrflg THEN
				int pte = access(RD, dot, DSP, 0);
				masterpcbb = (pte&PG_PFNUM)*NBPG;
			FI
			getpcb();
		FI
		break;

	    case 'd':
		if (adrflg) {
			if (!(adrval>=2 && adrval<=16 || adrval<=-2 && adrval>=-16)) {
				printf("illegal radix %d base ten",radix);
				break;
			}
			radix=adrval;
		}
		printf("radix=%d base ten",radix);
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
		printf("variables\n");
		FOR i=0;i<=35;i++
		DO IF var[i]
		   THEN printc((i<=9 ? '0' : 'a'-10) + i);
			printf(" = %R\n",var[i]);
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
		ELSE printf("no process\n");
		FI
		sigprint(); flushbuf();

	    case 'r': case 'R':
		printregs(modif);
		return;

	    case 'c': case 'C':
		IF adrflg
		THEN frame=adrval;
			callpc=get(frame-8,DSP);
		ELIF kcore THEN
			frame = pcb.pcb_fp;
			callpc = pcb.pcb_pc;
		ELSE
			frame = *(ADDR *)(((ADDR)(&u))+FP);
			callpc = *(ADDR *)(((ADDR)(&u))+PC);
		FI
		lastframe=0;
		ntramp = 0;
		WHILE cntval-- ANDF frame!=0
		DO	char *name;
			chkerr();
			/* check for pc in pcb (signal trampoline code) */
			if (MAXSTOR < callpc && callpc < MAXSTOR+ctob(UPAGES)) {
				name = "sigtramp";
				ntramp++;
			} else {
				ntramp = 0;
				findsym(callpc,ISYM);
				if (cursym &&
				    !strcmp(cursym->n_un.n_name, "start")) 
					break;
				if (cursym)
					name = cursym->n_un.n_name;
				else
					name = "?";
			}
			printf("%s(", name);
			narg = ((get(frame-4, DSP)&0xffff)-4)/4;
			argp = frame;
			IF ntramp != 1 THEN
			    LOOP IF narg==0 THEN break; FI
				printf("%R", get(argp += 4, DSP));
				IF --narg!=0 THEN printc(','); FI
			    POOL
			FI
			printf(") at ");
			psymoff(callpc, ISYM, "\n");

			IF modif=='C'
			THEN WHILE localsym(frame,argp)
			     DO word=get(localval,DSP);
				printf("%8t%s:%10t", cursym->n_un.n_name);
				IF errflg THEN printf("?\n"); errflg=0;
				ELSE printf("%R\n",word); FI
			     OD
			FI

			if (ntramp != 1) {
				callpc = get(frame-8, DSP);
				lastframe = frame;
				frame = get(frame, DSP)&ALIGN;
			} else
				callpc = get(lastframe+44, DSP);
			IF !adrflg ANDF !INSTACK(frame)
			THEN break; FI
		OD
		break;

	    /*print externals*/
	    case 'e': case 'E':
		for (sp = symtab; sp < esymtab; sp++) {
		   if (sp->n_type==(N_DATA|N_EXT) ORF sp->n_type==(N_BSS|N_EXT))
		   	printf("%s:%12t%R\n", sp->n_un.n_name, get(sp->n_value,DSP));
		}
		break;

	    /*print breakpoints*/
	    case 'b': case 'B':
		printf("breakpoints\ncount%8tbkpt%24tcommand\n");
		for (bkptr=bkpthead; bkptr; bkptr=bkptr->nxtbkpt)
			if (bkptr->flag) {
		   		printf("%-8.8d",bkptr->count);
				psymoff(bkptr->loc,ISYM,"%24t");
				comptr=bkptr->comm;
				WHILE *comptr DO printc(*comptr++); OD
			}
		break;

	    default: error(BADMOD);
	}

}

printmap(s,amap)
STRING	s; REG	MAP *amap;
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

printregs(c)
{
	REG REGPTR	p;
	ADDR		v;

	FOR p=reglist; p->rname; p++
	DO
		if(c!='R' && p->roffs!=PSL)
			continue;
		c = 'R';
		v = kcore ? *p->rkern : *(ADDR *)(((ADDR)&u)+p->roffs);
		printf("%s%6t%R %16t", p->rname, v);
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

	olp=lp;
	FOR p=reglist; p->rname; p++
	DO	regptr=p->rname;
		IF (regnam == *regptr++)
		THEN
			WHILE *regptr
			DO IF readchar() != *regptr++
				THEN --regptr; break;
				FI
			OD
			IF *regptr
			THEN lp=olp;
			ELSE return(kcore ? (int)p->rkern : p->roffs);
			FI
		FI
	OD
	lp=olp;
	return(-1);
}

printpc()
{
	dot= *(ADDR *)(((ADDR)(&u))+PC);
	psymoff(dot,ISYM,":%16t"); printins(ISP,chkget(dot,ISP));
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
/* not valid
	"floating overflow trap",
	"floating/decimal divide by zero trap",
	"floating underflow trap",
	"decimal overflow trap",
	"subscript out of range trap",
	"floating overflow fault",
	"floating divide by zero fault",
	"floating undeflow fault"
*/
};

sigprint()
{
	IF (signo>=0) ANDF (signo<sizeof signals/sizeof signals[0])
	THEN printf(signals[signo]); FI
	switch (signo) {

	case SIGFPE:
		IF (sigcode > 0 &&
		    sigcode < sizeof fpenames / sizeof fpenames[0]) THEN
			printf(" ("); printf(fpenames[sigcode]); printc(')');
		FI
		break;

	case SIGILL:
		IF (sigcode >= 0 &&
		    sigcode < sizeof illinames / sizeof illinames[0]) THEN
			printf(" ("); printf(illinames[sigcode]); printc(')');
		FI
		break;
	}
}
