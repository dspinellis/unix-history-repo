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
MAP		physmap;

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
#ifdef pdp11
help!
#endif

#ifdef vax
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
#endif

#ifdef hp300
/* not all register info is available from the pcb */
/* for hp (and other 68000's) we add a print format field and a type field */
#ifndef NEWVM
	"p0br",	P0BR,	&pcb.pcb_p0br,		FMTTAB,	HEXINT,
	"p0lr",	P0LR,	&pcb.pcb_p0lr,		FMTNL,	HEXINT,
	"p1br",	P1BR,	&pcb.pcb_p1br,		FMTTAB,	HEXINT,
	"p1lr",	P1LR,	&pcb.pcb_p1lr,		FMTNL,	HEXINT,
#endif
	"psw",	PSW,	&pcb.pcb_flags,		FMTNL,	HEXINT,
	"usp",	USP,	&pcb.pcb_usp,		FMTTAB,	HEXINT,
	"pc",	PC,	0,			FMTNL,	HEXADDR,
	"d7",	D7,	&pcb.pcb_regs[5],	FMTTAB,	HEXINT,
	"sp",	A7,	&pcb.pcb_regs[11],	FMTNL,	HEXADDR,
	"d6",	D6,	&pcb.pcb_regs[4],	FMTTAB,	HEXINT,
	"a6",	A6,	&pcb.pcb_regs[10],	FMTNL,	HEXADDR,
	"d5",	D5,	&pcb.pcb_regs[3],	FMTTAB,	HEXINT,
	"a5",	A5,	&pcb.pcb_regs[9],	FMTNL,	HEXADDR,
	"d4",	D4,	&pcb.pcb_regs[2],	FMTTAB,	HEXINT,
	"a4",	A4,	&pcb.pcb_regs[8],	FMTNL,	HEXADDR,
	"d3",	D3,	&pcb.pcb_regs[1],	FMTTAB,	HEXINT,
	"a3",	A3,	&pcb.pcb_regs[7],	FMTNL,	HEXADDR,
	"d2",	D2,	&pcb.pcb_regs[0],	FMTTAB,	HEXINT,
	"a2",	A2,	&pcb.pcb_regs[6],	FMTNL,	HEXADDR,
	"d1",	D1,	0,			FMTTAB,	HEXINT,
	"a1",	A1,	0,			FMTNL,	HEXADDR,
	"d0",	D0,	0,			FMTTAB,	HEXINT,
	"a0",	A0,	0,			FMTNL,	HEXADDR,
	"fp0",	FP0,	&pcb.pcb_fpregs.fpf_regs[0],	FMTTAB,	XPFLOAT,
	"fp4",	FP4,	&pcb.pcb_fpregs.fpf_regs[12],	FMTNL,	XPFLOAT,
	"fp1",	FP1,	&pcb.pcb_fpregs.fpf_regs[3],	FMTTAB,	XPFLOAT,
	"fp5",	FP5,	&pcb.pcb_fpregs.fpf_regs[15],	FMTNL,	XPFLOAT,
	"fp2",	FP2,	&pcb.pcb_fpregs.fpf_regs[6],	FMTTAB,	XPFLOAT,
	"fp6",	FP6,	&pcb.pcb_fpregs.fpf_regs[18],	FMTNL,	XPFLOAT,
	"fp3",	FP3,	&pcb.pcb_fpregs.fpf_regs[9],	FMTTAB,	XPFLOAT,
	"fp7",	FP7,	&pcb.pcb_fpregs.fpf_regs[21],	FMTNL,	XPFLOAT,
	"fpcr",	FPCR,	&pcb.pcb_fpregs.fpf_fpcr,	FMTTAB,	HEXINT,
	"fpsr",	FPSR,	&pcb.pcb_fpregs.fpf_fpsr,	FMTTAB,	HEXINT,
	"fpiar",FPIAR,	&pcb.pcb_fpregs.fpf_fpiar,	FMTNL,	HEXINT,
#endif

#if !pdp11 && !vax && !hp300

edit this file and add the reglist for your machine

#endif
};

INT		nregs	= sizeof reglist / sizeof reglist[0];

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

extern STRING	sys_siglist[NSIG];

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
	INT		tracefirst;
	L_INT		radj;

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
					IF findifile(file, Ifile) == 0
					THEN error(NOTOPEN);
		 			FI
				FI
				IF strcmp(file, "-")!=0
				THEN	iclose(stack, 0);
					infile=open(file,0);
					IF infile<0
					THEN	infile=open(Ifile,0);
					FI
				ELSE	lseek(infile, (off_t)0, 0);
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
				ELSE	lseek(outfile, (off_t)0, 2);
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
#ifdef vax
		IF kernel == 0
		THEN	printf("not debugging kernel\n");
		ELSE	IF adrflg
			THEN	int pte = access(RD, dot, DSP, 0);
				masterpcbb = (pte&PG_PFNUM)*512;
			FI
			getpcb();
		FI
#endif

#ifdef hp300
		IF kernel == 0
		THEN	printf("not debugging kernel\n");
		ELSE	IF adrflg
			THEN
				masterpcbb = ctob(dot);
#ifndef NEWVM
				IF kcore && !kmem
				THEN
					masterpcbb -= lowram;
				FI
#endif
			FI
			getpcb();
		FI
#endif

#if !vax && !hp300

edit this file and fix it for your kernel

#endif
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
		if (kernel)
			printmap("] map",&physmap);
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
#ifdef vax
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
#endif

#ifdef hp300
		tracefirst = 0;
		IF !kcore THEN radj = getradj(0); FI
		IF adrflg
		THEN	frame=adrval;
			argp=frame+4;	/* base of args minus one word */
			callpc=lget(frame+4, DSP); /* best we can do? */
		ELIF kcore
		THEN	frame=pcb.pcb_regs[10];
			argp=frame+4;
			callpc=lget(frame, DSP);
		ELSE	callpc= *(ADDR *)(((ADDR)&u)+PC-radj);
			IF (*(INT *)(((ADDR)&u)+PC+4-radj) & 07777) == 0200
			THEN	/* system call, fake a stack frame */
				tracefirst = 1;
				frame= *(ADDR *)(((ADDR)&u)+USP-radj)-4;
			ELSE	frame= *(ADDR *)(((ADDR)&u)+FP-radj);
			FI
			argp= frame+4;
		FI
#endif

#if !vax && !hp300

get help!

#endif
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
#ifdef vax
			narg = get(argp,DSP); IF narg&~0xFF THEN narg=0; FI
#endif
#ifdef hp300
			narg = getnargs(frame);
#endif
#if !vax && !hp300
edit this file to calculate arg count for your machine
#endif
			LOOP	IF narg==0 THEN break; FI
#ifdef vax
				printf("%R", get(argp += 4, DSP));
#endif
#ifdef hp300
				printf("%X", lget(argp += 4, DSP));
#endif
				IF --narg!=0 THEN printc(','); FI
			POOL
#ifdef vax
			IF ntramp == 1
			THEN callpc=get(frame+92, DSP);
			ELSE callpc=get(frame+16, DSP);
			FI
#endif
#ifdef hp300
			callpc=lget(frame+4,DSP);
#endif
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

#ifdef vax
			argp=get(frame+8, DSP);
			lastframe=frame;
			frame=get(frame+12, DSP)&EVEN;
#endif
#ifdef hp300
			lastframe=frame;
			IF tracefirst
			THEN	tracefirst = 0;
				frame= *(ADDR *)(((ADDR)&u)+FP-radj);
			ELSE	frame=lget(frame, DSP);
			FI
			argp=frame+4;
#endif
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
				psymoff((long)bkptr->loc,ISYM,"%24t");
				comptr=bkptr->comm;
				WHILE *comptr DO printc(*comptr++); OD
			FI
		OD
		break;

	    default:
		error(BADMOD);
	}

}

#ifdef hp300
#define ADDQ	0x500f
#define ADDL	0xd1fc
#define ADDW	0xd0fc
#define LEA	0x4fef

getnargs(fp)
ADDR fp;
{
	register int narg;
	ADDR rtn = lget(fp + 4, DSP);
	unsigned inst = get(rtn, ISP);

	if ((inst & 0xf13f) == ADDQ) {
		narg = (inst >> 9) & 07;
		if (narg == 0)
			narg = 8;
	} else
	if ((inst & 0xf1fc) == ADDL)
		narg = lget(rtn + 2, ISP);
	else
	if ((inst & 0xf1fc) == ADDW)
		narg = get(rtn + 2, ISP);
	else
	if (inst == LEA)
		narg = get(rtn + 2, ISP);
	else
		narg = 0;
	narg >>= 2;
	return narg;
}
#endif

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

#ifdef hp300
/* convert 68881 extended precision to double precision */
double xptod(vp)
	ADDR *vp;
{
#ifdef __HCC__
	asm("mov.l	8(%a6),%a0");
	asm("fmov.x	(%a0),%fp0");
	asm("fmov.d	%fp0,-(%sp)");
	asm("mov.l	(%sp)+,%d0");
	asm("mov.l	(%sp),%d1");
	asm("unlk	%a6");
	asm("rts");
#else
#ifdef __GNUC__
	asm("movel a6@(8),a0");
	asm("fmovex a0@,fp0");
	asm("fmoved fp0,sp@-");
	asm("movel sp@+,d0");
	asm("movel sp@+,d1");
	asm("unlk a6");
	asm("rts");
#else
	/* hack -- we convert to single precision */
	asm("movl	a6@(8),a0");
	asm(".word	0xf210");	/* fmovx a0@,fp0 */
	asm(".word	0x4800");
	asm(".word	0xf200");	/* fmovf fp0,d0 */
	asm(".word	0x6400");
	asm("clrl	d1");
	asm("unlk	a6");
	asm("rts");
#endif
#endif
}
#endif

printregs()
{
	REG REGPTR	p;
	ADDR		*vp;
	L_INT		radj = getradj(0);
	L_INT		offset;

	FOR p=reglist; p < &reglist[nregs]; p++
	DO	IF kcore
		THEN	IF !p->rkern THEN continue; FI
			vp = p->rkern;
		ELSE	IF (offset = p->roffs) >= sizeof (struct user)
			THEN	offset -= radj;
			FI
			vp = (ADDR *)(((ADDR)&u) + offset);
		FI
#ifndef hp300
		printf("%s%6t%X %16t", p->rname, *(L_INT *)vp);
		valpr(*(L_INT *)vp,(p->roffs==PC?ISYM:DSYM));
		printc(EOR);
#else
		switch (p->rtype) {
		case HEXINT:
			printf("%-8s%8X", p->rname, *(L_INT *)vp);
			break;
		case HEXADDR:
			printf("%-8s%8X%8t", p->rname, *(L_INT *)vp);
			valpr(*(L_INT *)vp,(p->roffs==PC?ISYM:DSYM));
			break;
		case XPFLOAT:
			printf("%-8s%F", p->rname, xptod(vp));
			break;
		default:
			printf("???");
		}
		switch (p->rfmt) {
		case FMTTAB:
			printf("%8t");
			break;

		default:
		case FMTNL:
			printc(EOR);
			break;
		}
#endif
	OD
	printpc();
}

getreg(regnam) {
	REG REGPTR	p;
	REG STRING	regptr;
	CHAR	*olp;
	CHAR		regnxt;

	olp=lp;
	FOR p=reglist; p < &reglist[nregs]; p++
	DO	IF kcore ANDF !p->rkern THEN continue; FI
		regptr=p->rname;
		IF (regnam == *regptr++)
		THEN
			WHILE *regptr
			DO IF (regnxt=readchar()) != *regptr++
				THEN --regptr; break;
				FI
			OD
			IF *regptr
			THEN lp=olp;
			ELSE	IF kcore
				THEN	return ((int) p->rkern);
				FI
				return (p->roffs);
			FI
		FI
	OD
	lp=olp;
	return(0);
}

printpc()
{
#ifdef hp300
	IF kcore THEN return; FI /* no pc in pcb */
#endif
	dot= *(ADDR *)(((ADDR)&u)+PC-getradj(0));
	psymoff(dot,ISYM,":%16t"); printins(0,ISP,chkget(dot,ISP));
	printc(EOR);
}

#ifdef vax
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
#endif

sigprint()
{
	IF (signo>=0) ANDF (signo<sizeof sys_siglist/sizeof sys_siglist[0])
	THEN prints(sys_siglist[signo]); FI
#ifdef vax
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
#endif

#ifdef mc68000
	/* XXX any equivalent on 68000 boxen? */
#endif
}
