/*	kdb_print.c	7.3	86/11/20	*/

#include "../kdb/defs.h"

char	*BADRAD;

ADDR	lastframe;
ADDR	callpc;

char	*BADMOD;
char	*lp;
long	maxpos;
int	radix;
char	lastc;

/* breakpoints */
BKPTR	bkpthead;

REGLIST reglist[] = {
	"p2lr",	&pcb.pcb_p2lr,	"p2br",	(int *)&pcb.pcb_p2br,
	"p1lr",	&pcb.pcb_p1lr,	"p1br",	(int *)&pcb.pcb_p1br,
	"p0lr",	&pcb.pcb_p0lr,	"p0br",	(int *)&pcb.pcb_p0br,
	"ksp",	&pcb.pcb_ksp,	"hfs",	&pcb.pcb_hfs,
	"psl",	&pcb.pcb_psl,	"pc",	&pcb.pcb_pc,
	"ach",	&pcb.pcb_ach,	"acl",	&pcb.pcb_acl,
	"usp",	&pcb.pcb_usp,	"fp",	&pcb.pcb_fp,
	"r12",	&pcb.pcb_r12,	"r11",	&pcb.pcb_r11,
	"r10",	&pcb.pcb_r10,	"r9",	&pcb.pcb_r9,
	"r8",	&pcb.pcb_r8,	"r7",	&pcb.pcb_r7,
	"r6",	&pcb.pcb_r6,	"r5",	&pcb.pcb_r5,
	"r4",	&pcb.pcb_r4,	"r3",	&pcb.pcb_r3,
	"r2",	&pcb.pcb_r2,	"r1",	&pcb.pcb_r1,
	"r0",	&pcb.pcb_r0,
	0
};

/* general printing routines ($) */

printtrace(modif)
{
	register narg, i;
	register BKPTR bkptr;
	register ADDR word;
	register char *comptr;
	register ADDR argp, frame;
	register struct nlist *sp;
	int stack, ntramp;
	register struct  proc *p;
	extern struct proc *allproc;

	if (cntflg==0)
		cntval = -1;
	switch (modif) {

	case 'd':
		if (adrflg) {
			if (adrval < 2 || adrval > 16)
				error(BADRAD);
			radix = adrval;
		}
		printf("radix=%d base ten", radix);
		break;

	case 'w': case 'W':
		printf("maxpos=%d", maxpos=(adrflg?adrval:MAXPOS));
		break;

	case 's': case 'S':
		printf("maxoff=%d", maxoff=(adrflg?adrval:MAXOFF));
		break;

	case 'v': case 'V':
		printf("variables\n");
		for (i=0;i<=35;i++)
			if (var[i]) {
				printc((i<=9 ? '0' : 'a'-10) + i);
				printf(" = %R\n",var[i]);
			}
		break;

	case 0: case '?':
		if (p = (struct proc *)var[varchk('p')])
			printf("pid = %d\n", p->p_pid);
		else
			printf("in idle loop\n");
		printtrap(var[varchk('t')], var[varchk('c')]);
		/* fall thru... */
	case 'r': case 'R':
		printregs(modif);
		return;

	case 'c': case 'C':
		if (adrflg) {
			frame=adrval;
			callpc=get(frame-8,DSP);
		} else {
			frame = pcb.pcb_fp;
			callpc = pcb.pcb_pc;
		}
		lastframe=0;
		ntramp = 0;
		while (cntval-- && frame!=0) {
			char *name;

			chkerr();
			/* check for pc in pcb (signal trampoline code) */
			if (MAXSTOR < callpc &&
			   callpc < MAXSTOR+ctob(UPAGES)) {
				name = "sigtramp";
				ntramp++;
			} else {
				ntramp = 0;
				findsym(callpc,ISYM);
				if (cursym)
					name = cursym->n_un.n_name;
				else
					name = "?";
			}
			printf("%s(", name);
			narg = ((get(frame-4, DSP)&0xffff)-4)/4;
			argp = frame;
			if (ntramp != 1)
				for (;;) {
					if (narg==0)
						break;
					printf("%R", get(argp += 4, DSP));
					if (--narg!=0)
						printc(',');
				}
			printf(") at ");
			psymoff(callpc, ISYM, "\n");

			if (modif=='C') {
				while (localsym(frame,argp)) {
					word=get(localval,DSP);
					printf("%8t%s:%10t",
					    cursym->n_un.n_name);
					if (errflg) {
						printf("?\n");
						errflg=0;
					} else
						printf("%R\n",word);
				}
			}
			if (ntramp != 1) {
				callpc = get(frame-8, DSP);
				lastframe = frame;
				frame = get(frame, DSP)&ALIGN;
			} else
				callpc = get(lastframe+44, DSP);
			if (frame == 0 || (!adrflg && !INSTACK(frame)))
				break;
		}
		break;

		/*print externals*/
	case 'e': case 'E':
		for (sp = symtab; sp < esymtab; sp++)
			if (sp->n_type==(N_DATA|N_EXT) ||
			    sp->n_type==(N_BSS|N_EXT))
				printf("%s:%12t%R\n", sp->n_un.n_name,
					get(sp->n_value,DSP));
		break;

		/*print breakpoints*/
	case 'b': case 'B':
		printf("breakpoints\ncount%8tbkpt%24tcommand\n");
		for (bkptr=bkpthead; bkptr; bkptr=bkptr->nxtbkpt)
			if (bkptr->flag) {
		   		printf("%-8.8d",bkptr->count);
				psymoff(bkptr->loc,ISYM,"%24t");
				comptr=bkptr->comm;
				while (*comptr)
					printc(*comptr++);
			}
		break;

	case 'l':
		for (p = allproc; p; p = p->p_nxt) {
			printf("%X pid %5d %c", p, p->p_pid,
				p->p_stat == SSLEEP ? 'S' :
				p->p_stat == SRUN ? 'R':
				p->p_stat == SIDL ? 'I':
				p->p_stat == SSTOP ? 'T' : '?');
			if (p->p_wchan) {
				printf(" wait ");
				psymoff(p->p_wchan, ISYM, "");
			}
			printc(EOR);
		}
		break;

	default:
		error(BADMOD);
	}
}

static
printregs(c)
{
	register REGPTR	p;
	ADDR v;

	for (p=reglist; p->rname; p++) {
		if (c!='R' && p->rkern!=&pcb.pcb_psl)
			continue;
		c = 'R';
		v = *p->rkern;
		printf("%s%6t%R %16t", p->rname, v);
		valpr(v,(p->rkern==&pcb.pcb_pc?ISYM:DSYM));
		printc(EOR);
	}
	printpc();
}

getreg(regnam)
{
	register REGPTR	p;
	register char *regptr;
	char *olp;

	olp=lp;
	for (p=reglist; p->rname; p++) {
		regptr=p->rname;
		if (regnam == *regptr++) {
			while (*regptr)
				if (readchar() != *regptr++) {
					--regptr;
					break;
				}
			if (*regptr)
				lp=olp;
			else
				return((int)p->rkern);
		}
	}
	lp=olp;
	return (-1);
}

printpc()
{

	psymoff(pcb.pcb_pc, ISYM, ":%16t");
	printins(ISP, chkget(pcb.pcb_pc, ISP));
	printc(EOR);
}
