static	char sccsid[] = "@(#)display.c 4.3 8/17/82";
#include "head.h"
#include <a.out.h>
#include <stab.h>
#include "cdefs.h"
struct user u;
BKPTR	bkpthead;

#ifdef FLEXNAMES
#define	bread(a,b,c)	stread(b,c)
#define	blseek(a,b,c)	stseek(b,c)
#endif

/* initialize frame pointers to top of call stack */
/*  MACHINE DEPENDENT */
struct proct *
initframe() {
	argp = *(ADDR *) (((ADDR) &u) + AP);
	frame = *(ADDR *) (((ADDR) &u) + FP);
	callpc = *(ADDR *) (((ADDR) &u) + PC);
	if ((frame == 0) || (frame & 0xf0000000 != 0x70000000))
		return(badproc);
	return(adrtoprocp(callpc++));  /* ++ because UNIX backs up instrs */
}


struct proct *
nextframe() {
	callpc = get(frame+16, DSP);
	argp = get(frame+8, DSP);
	frame = get(frame+12, DSP) & EVEN;
	if (callpc > 0x70000000) {
		/* error handler kludge */
		callpc = get(frame+64, DSP);
		argp = get(frame+8, DSP);
		frame = get(frame+12, DSP) & EVEN;
	}
	if ((frame == 0) || (frame & 0xf0000000 != 0x70000000))
		return(badproc);
	return(adrtoprocp(callpc-1));
}

/* returns core image address for variable */
/*  MACHINE DEPENDENT */
ADDR
formaddr(class, addr)
u_char class;
ADDR addr; {
if (debug) printf("formaddr(%o, %d)\n", class & 0377, addr);
	switch(class & STABMASK) {
	case N_RSYM:
		return(stackreg(addr));
	case N_GSYM:
	case N_SSYM:
	case N_STSYM:
	case N_LCSYM:
		return(addr);
		
	case N_PSYM:
		return(argp+addr);
		
	case N_LSYM:
		return(frame+addr);

	default:
		printf("Bad class in formaddr: 0%o",
			class & 0377);
		return(0);
	}
}

char class;

/*
 *  stackreg(reg):
 * If the register for the current frame is somewhere on the stack
 * then return the address of where it is, otherwise its still in
 * the register so return the register number.
 * We distinguish the two by noting that register numbers are less
 * than 16 and that stack addresses are greater.
 *
 *  MACHINE DEPENDENT
 */
ADDR
stackreg(reg) {
	register int curframe, regfl, mask, i;
	struct proct *procp;
	ADDR regaddr;

	curframe = frame;
	regaddr = reg;
	regfl = 0x10000 << reg;
	for (procp=initframe(); frame!=curframe; procp=nextframe()) {
		if (procp == badproc) {
			error("Stackreg error: frame");
			return(-1);
		}
		mask = get(frame+4, DSP);
		if (mask & regfl) {
			regaddr = frame + 20;
			for (i=0; i<reg; i++) {
				if (mask & 0x10000)
					regaddr += WORDSIZE;
				mask = mask >> 1;
			}
			if (!(mask & 0x10000)) {
				error("Stackreg error: contents");
				return(-1);
			}
		}
	}
	return(regaddr);
}

/* returns address of proc:var. Sets externals class and subflag */
ADDR
varaddr(proc, var)
char *proc, *var; {
	return(findvar(proc, var, "", 0));
}

/*
 * displays values of variables matching proc:var, 
 * returns its address
 */
ADDR
dispvar(proc, var, fmt)
char *proc, *var, *fmt; {
	return(findvar(proc, var, fmt, 1));
}

/*
 * Find and print values of all variables matching proc:var
 *	using specified format.
 * Returns address of last matching variable.
 *
 * prvar==0 => no output,
 * prvar==1 => output value,
 * prvar==2 => output addr
 */
ADDR
findvar(proc, var, fmt, prvar)
char *proc, *var, *fmt; {
	ADDR addr = -1, a = -1;
	int metaflag = 0, match=0, nullflag=0, depthcnt = -1;
	char *comblk;
	register struct proct *procp;

	if (percentflag) {	/* kludge for register names */
		return(regout(var, prvar, fmt));
	}

	if (var[0] == '\0') {
		error("Unexpected null variable name");
		return(-1);
	}

	metaflag = eqany('*', proc) || eqany('?', proc) ||
		eqany('*', var) || eqany('?', var);
	
	if (proc[0] == '\0') {
		nullflag++;
		proc = curproc()->pname;
	}

	comblk = colonflag ? "" : "*";

	if (integ && !eqany(var[0], "->.[")) {	
		depthcnt = integ;
	}
	if (integ) {
		if (eqany(var[0], "->.[")) 
			match++;
		else
			depthcnt = integ;
	}

	procp = initframe();
	if (!eqany(var[0], "->.[") && !(nullflag && colonflag)) {
		do {
			if (eqpat(proc, procp->pname)) {
				match++;
				if (--depthcnt==0 || integ==0) {
					a = outvar(procp->pname, var, fmt,
						metaflag, integ, N_GSYM, 
						0, prname, comblk, prvar);
					if (a != -1)
						addr = a;
					if (depthcnt == 0)
						break;
				}
			}
		} while ((procp=nextframe()) != badproc);
	}

	if ((colonflag || metaflag || a == -1) && 
			(nullflag || eqpat(proc, ""))) {
		a = outvar("", var, fmt, metaflag, integ,
			N_GSYM, 0, prname, comblk, prvar);
		if (a != -1) {
			addr = a;
			match++;
		}
	}

	if (match==0 && colonflag) {
		procp = initframe();
		do {
			if (eqstr(curproc()->pname, procp->pname))
				break;
		} while ((procp=nextframe()) != badproc);
		a = outvar(curproc()->pname, var, fmt, metaflag,
			integ, N_GSYM, 0, prname, 
			nullflag ? "_BLNK_" : proc, prvar);
		if (a != -1) {
			addr = a;
			match++;
		}
	}

	if (addr == -1 && match == 0) {
		addr = extoutvar(var, fmt, metaflag, prvar);
		if (addr != -1)
			return(addr);
	}
	if (match == 0) {
		printf("%s not an active procedure\n", proc);
		return(-1);
	}
	if (addr == -1) {
		if (var[0] == '.')
			var++;
		if (proc[0])
#ifndef FLEXNAMES
			printf("%.16s:%s not found\n", proc, var);
#else
			printf("%s:%s not found\n", proc, var);
#endif
		else
			printf("%s not found\n", var);
		return(-1);
	}
	return(addr);
}

char *
typetodesc(type, subflag)
short type; {
	register int ptr, ftn, ary;
	register char *desc;
	
	static char *typedesc[] = {
		"d",  /* undef */
		"d",  /* farg */
		"c",  /* char */
		"hd",  /* short */
		"d",  /* int */
		"ld",  /* long */
		"f",  /* float */
		"g",  /* double */
		"d",  /* strty */
		"d",  /* unionty */
		"d",  /* enumty */
		"d",  /* moety */
		"bu",  /* uchar */
		"hu",  /* ushort */
		"u",  /* unsigned */
		"lu",  /* ulong */
		"d"   /* ? */
	};
	
	ptr = ftn = ary = 0;
	
	desc = typedesc[type&BTMASK];
	for (; type & TMASK; type = DECREF(type)) {
		if (ISPTR(type)) ptr++;
		else if (ISFTN(type)) ftn++;
		else if (ISARY(type)) ary++;
	}
	
	if ((ptr-subflag == 1  || ary-subflag == 1)  &&  desc[0] == 'c')
		return("s");
	if (debug)
		printf ("PTR %d; FTN %d; ARY %d; DESC %s\n",ptr,ftn,ary,desc);
	if (ptr + ary ==  subflag)
		return(desc);
	if (ptr) return("x");
	if (ptr==1 && ftn==1) return("p");
	return(desc);
}

typetosize(type, stsize)
short type; {
	register int ptr, ftn, ary;
	register int size;
	
	static char typesize[] = {
		4,  /* undef */
		4,  /* farg */
		1,  /* char */
		2,  /* short */
		WORDSIZE,  /* int */
		4,  /* long */
		4,  /* float */
		8,  /* double */
		0,  /* strty */
		0,  /* unionty */
		4,  /* enumty */
		4,  /* moety */
		1,  /* uchar */
		2,  /* ushort */
		4,  /* unsigned */
		4,  /* ulong */
		4   /* ? */
	};
	
	ptr = ftn = ary = 0;
	
	size = typesize[type&BTMASK];
	for (; type & TMASK; type = DECREF(type)) {
		if (ISPTR(type)) ptr++;
		else if (ISFTN(type)) ftn++;
		else if (ISARY(type)) ary++;
	}
	
	if (debug)
		printf ("PTR %d; FTN %d; ARY %d; SIZE %d; STSIZE %d\n",
				ptr,ftn,ary,size,stsize);
	if (ptr>1) return(4);
	if (size == 0) return(stsize);
	else return(size);
}


/* print breakpoints */
prbkpt() {
	register BKPTR bkptr;
	register int cnt;
	char *cmdp;

	cnt = 0;
	
	for (bkptr = bkpthead; bkptr; bkptr=bkptr->nxtbkpt)
		if (bkptr->flag) {
			cnt++;
			printbkpt("", adrtoprocp(bkptr->loc), bkptr->loc);
			cmdp = bkptr->comm;
			if (*cmdp != '\n') {
				printf("   <");
				while (*cmdp != '\n')
					printf("%c", *cmdp++);
				printf(">\n");
			}
			else
				printf("\n");
		}
	if (cnt == 0) 
		printf("No breakpoints set\n");
}

/* interactively delete breakpoints */

idbkpt() {
	register BKPTR bkptr;
	register int yesflg, cnt;
	char c;
	
	cnt = 0;

	for (bkptr = bkpthead; bkptr; bkptr=bkptr->nxtbkpt)
		if (bkptr->flag) {
			printbkpt(" ? ", adrtoprocp(bkptr->loc), bkptr->loc);
			yesflg = 0;
			cnt++;
			do {
				c = getchar();
				if (c == 'y' || c == 'd') yesflg++;
			} while (c != '\n');
			if (yesflg)
				bkptr->flag = 0;
		}
	if (cnt == 0)
		printf("No breakpoints set\n");
}

/* delete all breakpoints */

dabkpt() {
	register BKPTR bkptr;
	
	for (bkptr = bkpthead; bkptr; bkptr=bkptr->nxtbkpt)
		bkptr->flag = 0;
}
 
/* 
 * Print name of breakpoint for a, b, d commands:
 */
printbkpt(s, procp, dot)
char *s; struct proct *procp; ADDR dot; {
	adrtolineno(dot);
	if (dot != lnfaddr)
		printf("0x%x (", dot);
	prlnoff(procp, dot);
	if (dot != lnfaddr)
		printf(")");
	printf("%s", s);
}

/* print call frame */
prframe() {
	prfrx(0);
}

/* set top to print just the top procedure */
prfrx(top) {
	int narg;
	long offset;
	u_char class;
	register int endflg;
	char *p;
	struct proct *procp;
	struct nlist stentry;
	
	if ((procp = initframe()) == badproc) return;
	do {
		if (get(frame+12, DSP) == 0) return;
		p = procp->pname;
		if (eqstr("__dbsubc", p)) return;
		if (p[0] == '_') {
			endflg = 1;
#ifndef FLEXNAMES
			printf("%.15s(", p+1);
#else
			printf("%s(", p+1);
#endif
		}
		else {
#ifndef FLEXNAMES
			printf("%.16s(", p);
#else
			printf("%s(", p);
#endif
			endflg = 0;
		}
		if (endflg == 0) {
			offset = procp->st_offset;
			blseek(&sbuf, offset, 0);
			do {
				if (bread(&sbuf, &stentry, sizeof stentry) <
							sizeof stentry) {
					endflg++;
					break;
				}
				class = stentry.n_type & STABMASK;
			} while (class == N_FUN);
			while (class != N_PSYM) {
				if (bread(&sbuf, &stentry, sizeof stentry) <
							sizeof stentry) {
					endflg++;
					break;
				}
				class = stentry.n_type & STABMASK;
				if (class == N_FUN) {
					endflg++;
					break;
				}
			}
		}
		narg = get(argp, DSP);
		if (narg & ~0xff) narg = 0;
		argp += WORDSIZE;
		while (narg) {
			if (endflg) {
				printf("%d", get(argp, DSP));
				argp += 4;
			} else {
				int length;
#ifndef FLEXNAMES
				printf("%.16s=", stentry.n_name);
#else
				printf("%s=", stentry.n_un.n_name);
#endif
				dispx(argp, "", N_GSYM, stentry.n_desc,
					0, 0, DSP);
				length = typetosize(stentry.n_desc, 0);
				if (length > WORDSIZE)
					argp += length;
				else
					argp += WORDSIZE;
			}
			do {
				if (endflg) break;
				if (bread(&sbuf, &stentry, sizeof stentry) <
							sizeof stentry) {
					endflg++;
					break;
				}
				class = stentry.n_type & STABMASK;
				if (class == N_FUN) {
					endflg++;
					break;
				}
			} while (class != N_PSYM);
		l1:	if (--narg != 0) printf(",");
		}
		printf(")");
		if (debug) printf("  @ 0x%x ", callpc);
		if (procp->sfptr != badfile)
			printf("   [%s:%d]", adrtofilep(callpc-1)->sfilename,
				adrtolineno(callpc-1));
		printf("\n");
	} while (((procp = nextframe()) != badproc) && !top);
}

INT		signo;
STRING		signals[];
extern	nsig;
sigprint()
{

	if (signo < nsig)
		printf("%s", signals[signo]);
	else
		printf("signal %d???", signals[signo]);
}
