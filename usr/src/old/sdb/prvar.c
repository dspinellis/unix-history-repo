static	char sccsid[] = "@(#)prvar.c 4.3 %G%";
#include "head.h"
#include <a.out.h>
#include <stab.h>
#include "cdefs.h"
struct user u;
BKPTR	bkpthead;
STRING	errflg;

/* 
 *  outvar():
 * Prints named variable, recursing once for each structure member or
 *  subscript.
 * proc:var: variable name
 * fmt: print format
 * metaflag: set iff var contains metacharacters * or ?
 * addr: partial address of variable, initally 0
 * class: type class of variable
 * subflag: number of levels of subscript indirection
 * prnamep: pointer to end of partially formed print name of variable
 * comblk: name of common block containing variable, if any
 * prvar: as in findvar
 *
 * Here and elsewhere we assume that -1 is an invalid address, and
 *	its is used to indicate error.
 */
outvar(proc, var, fmt, metaflag, addr, class, subflag, prnamep,
		comblk, prvar)
ADDR addr; char *proc, *var, *fmt, *prnamep, *comblk; u_char class; {
	char *p, *q, *r, *oldpr;
	register int match;
	long soffset, goffset;
	register ADDR newaddr = -1, arrowaddr;
	register enum {INIT, ARROW, DOT} typeflag;

	switch (var[0]) {
	case '\0':
		if (prvar == 0) return(addr);
		if (metaflag) {
			if (comblk[0] && !(eqstr(comblk, "*")))
#ifndef FLEXNAMES
				printf("%.8s:%.8s", comblk, prname);
#else
				printf("%s:%s", comblk, prname);
#endif
			else if (proc[0])
#ifndef FLEXNAMES
				printf("%.8s:%.8s", proc, prname);
#else
				printf("%s:%s", proc, prname);
#endif
			else
				printf("%s", prname);
		}
		printit(metaflag, prvar, addr, fmt, class, sl_type,
			sl_size, subflag, DSP);
		return(addr);

	case '[':
		*prnamep++ = *var++;
		p = var;
		for (;;) {
			*prnamep++ = *var;
			if (*var == '\0' || *var == ']') break;
			var++;
		}
		newaddr = getindir(class, addr, sl_type);
		newaddr += typetosize(sl_type, sl_size) * readint(&p);
		return(outvar(proc, var+1, fmt, metaflag, newaddr, N_GSYM,
			subflag+1, prnamep, comblk, prvar));
		
	case '-':
	case '>':
		typeflag = ARROW;
		while (eqany(*var, "->"))
			*prnamep++ = *var++;
		subflag++;
		arrowaddr = getindir(class, addr, sl_type);
		if (errflg) {
			printf("%s\n", errflg);
			errflg = 0;
			return(0);
		}
		class = N_GSYM;
		if (var[0] == '\0') {
			p = var;
			newaddr = arrowaddr;
			goto recurse;
		}
		break;

	case '.':
		typeflag = DOT;
		if (class == N_RSYM) {
			error("Not with a register variable");
			return(0);
		}
		*prnamep++ = *var++;
		subflag = 0;
		break;

	default:
		typeflag = INIT;
		break;
	}

	if (typeflag == INIT) {
		soffset = proc[0] ? adrtostoffset(callpc-1) : -1;
		goffset = proc[0] ? -1 : findfile(curfile)->stf_offset;
	} else {
		soffset = proc[0] ? adrtostoffset(callpc-1) : -1;
		goffset = findfile(curfile)->stf_offset;
	}

	p = var;
	oldpr = prnamep;
	while (!eqany(*p, "->.[") && *p != '\0')
		*prnamep++ = *p++;
	*prnamep = '\0';

	match = 0;
	slookinit(); 

	for (;;) {
		if (soffset != -1)
			if ((soffset = slooknext(var, soffset, typeflag!=INIT,
				comblk)) != -1)
				goto found;
		if (goffset != -1)
			if ((goffset = globallookup(var, goffset, 
				typeflag!=INIT)) != -1)
				goto found;
		return(newaddr);

	found:
		r = sl_name;
		q = oldpr;
		while (*r) *q++ = *r++;
		*q ='\0';

		switch(typeflag) {
		case INIT:
			class = sl_class & STABMASK;
			if (!varclass(class) || class == N_SSYM)
				goto l;
			newaddr = (class == N_LSYM) ? -sl_addr : sl_addr;
			newaddr = formaddr(class, newaddr);
			break;

		case ARROW:
			class = sl_class & STABMASK;
			if (!varclass(class) || class != N_SSYM)
				goto l;
			newaddr = arrowaddr + sl_addr;
			break;

		case DOT:
			class = sl_class & STABMASK;
			if (!varclass(class) || class != N_SSYM)
				goto l;
			newaddr = addr + sl_addr;
			break;
		}

	recurse:
		newaddr = outvar(proc, p, fmt, metaflag, newaddr, 
			class, subflag, prnamep, comblk, prvar);
		
		if (!metaflag)
			return(newaddr);
l:;	}
}

/* Output external variables.  Arguments as in outvar() */
extoutvar(var, fmt, metaflag, prvar)
char *var, *fmt; {
	long offset;
	ADDR addr = -1;

	offset = extstart;
	sl_addr = -1;

	for (;;) {
		offset = extlookup(var, offset);
		addr = sl_addr;
		if (offset == -1)
			return(addr);
		if (metaflag) 
#ifndef FLEXNAMES
			printf("%.7s", sl_name);
#else
			printf("%s", sl_name);
#endif
		printit(metaflag, prvar, addr, fmt[0] ? fmt : "d", 
			N_GSYM, 0, 0, 0, DSP);
		if (!metaflag)
			return(addr);
	}
}

prdebug() {
	register struct proct *procp;
	register struct filet *filep;
	
	printf("dot=%d\n", dot);
	printf("extstart = %d\n", extstart);
	printf("firstdata = %d\n", firstdata);
	for(filep=files;filep->sfilename[0];filep++)
		printf("%s offs %d @ %d flag %d addr 0x%x\n", filep->sfilename, filep->stf_offset, filep, filep->lineflag, filep->faddr);
	for(procp=procs;procp->pname[0];procp++) {
#ifndef FLEXNAMES
		printf("%s addr 0x%x; offs %d; sfptr %d; line %d",
#else
		printf("%8.8s addr 0x%x; offs %d; sfptr %d; line %d",
#endif
			procp->pname, procp->paddr, procp->st_offset,
			procp->sfptr, procp->lineno);
		if (procp->entrypt) printf(" entrypoint");
		printf("\n");
	}
}

/*
 * display addr in data space using format desc or class s
 *  type == 1 => use addr for value to print
 */
dispf(addr, desc, class, type, size, subflag, space)
u_char class;
char *desc; short type; ADDR addr; {
	dispx(addr, desc, class, type, size, subflag, DSP);
	printf("\n");
}

/* display addr in instruction space using format desc or class s */
/*  returns -1 if bad address */
dispi(addr, desc, class, type, size, subflag, space)
u_char class;
char *desc; short type; ADDR addr; {
	register i;
	i = dispx(addr, desc, class, type, size, subflag, ISP);
	printf("\n");
	return(i);
}

char	pd[3];
dispx(addr, desc, class, type, size, subflag, space)
u_char class;
char *desc; short type; ADDR addr; {
	int i, sflag;
	char *p;
	char dlen, dfmt;
	long value;
	union {
		char c[WORDSIZE]; 
		int w;
		float f;
	} word;
	union {
		struct{
			int w1, w2;
		} ww;
		double d;
	} dbl;

	class &= STABMASK;
	if (desc[0]  == '\0') desc = typetodesc(type, subflag);
	cpstr(odesc, desc);
	otype = type;
	oclass = class;
	oaddr = addr;
	oincr = 0;
	if (debug) printf("dispx(addr=%d,desc=%s,class=%d,type=%d,size=%d,subflg=%d,space=%d)\n",
		addr, desc, class, type, size, subflag, space);
	pd[0] = '%';
	pd[1] = dfmt = 'd';
	dlen = '\0';
	for (p = desc; *p; p++) {
		if (*p>= '0' && *p<'9') {
			size = readint(&p);
			p--;
		} else switch (*p) {
			case 'l':
			case 'h':
			case 'b':
				dlen = *p;
				break;

			case 'a':
			case 'c':
			case 'd':
			case 'f':
			case 'g':
			case 'i':
			case 'I':
			case 'o':
			case 'p':
			case 's':
			case 'u':
			case 'x':
				pd[1] = dfmt = *p;
				break;

			default:
				printf("Illegal descriptor: %c\n", *p);
				return(1);
		}
	}

	if (type == -1)
		value = addr;
	else if (class == N_RSYM && addr < 16) {
		/* MACHINE DEPENDENT */
		if ((addr > 0 && addr < 6) || addr > 11) {
			printf("Bad register var %d\n", addr);
			return(-1);
		}
		value = *(ADDR *)(((ADDR) &u) + R0 + (WORDSIZE)*addr);
	}
	else {
		value = getval(addr, dfmt == 'g' ? 'd' : dfmt, space);
	}

	if (errflg) {
		printf("%s", errflg);
		errflg = 0;
		return(-1);
	}

	switch (dfmt) {
	default:
		switch (dfmt) {
		case 'u':
		case 'x':
		case 'o':
			switch (dlen) {
			case 'h':
				value = (unsigned short) value;
				oincr = 2;
				break;
			case 'b':
				value = (unsigned char) value;
				oincr = 1;
				break;
			case 'l':
				value = (unsigned long) value;
				oincr = 4;
				break;
			default:
				oincr = WORDSIZE;
				break;
			}
			break;

		default:
			switch (dlen) {
			case 'h':
				value = (short) value;
				oincr = 2;
				break;
			case 'b':
				value = (char) value;
				oincr = 1;
				break;
			case 'l':
				value = (long) value;
				oincr = 4;
				break;
			default:
				oincr = WORDSIZE;
				break;
			}
		}
		if (dfmt == 'x' && (value > 9 || value < 0))
			printf("0x");
		else if (dfmt == 'o' && (value > 7 || value < 0))
			printf("0");
		printf(pd, value);
		return(1);
		
	case 'f':
		pd[1] = 'g';
		word.w = value;
		printf(pd, word.f);
		return(1);

	case 'g':
		dbl.ww.w1 = value;
		dbl.ww.w2 = (class == N_RSYM) ?
			*(ADDR *)(((ADDR) &u)+R0+(WORDSIZE)*(addr+1)) :
			getval(addr+WORDSIZE, 'd', space);
		printf("%.13g", dbl.d);
		return(1);

	case 'p':
		printf("%s:%d", adrtoprocp(value)->pname,
			adrtolineno(value));
		return(1);

	case 's':
		addr = getindir(class, addr, type);
		goto aa;
			
	case 'c':
		if (size <= 1) {
			oincr = 1;
			printchar(value);
			return(1);
		} else 
			goto aa;

	case 'a':
	aa:	sflag = size == 0;
		if (sflag)
			size = 128;  /* maximum length for s and a */
		else
			oincr = size;
		for (;;) {
			word.w = getval(addr, 'd', space);
			for (i=0; i<WORDSIZE; i++) {
				if (sflag && word.c[i] == 0) 
					return(1);
				if (size-- == 0)
					return(1);
				printchar(word.c[i]);
			}
			addr += WORDSIZE;
		}
		break;

	case 'i':
	case 'I':
		value = chkget(dot, space);
		if (errflg) {
			printf("%s", errflg);
			errflg = 0;
			return(-1);
		}
		printins(dfmt, space, value);
		break;

	}
	return(1);
}

/* print variable as in prvar */
printit(metaflag, prvar, addr, desc, class, type, size, subflag, space) 
u_char class;
char *desc; short type; ADDR addr; {
	if (prvar == 0)
		return;
	if (metaflag) {
		if (prvar == 1)
			printf("/ ");
		else
			printf("= ");
	}
	if (prvar == 1)
		dispf(addr, desc, class, type, size,
			subflag, space);
	else
		dispf(addr, desc, 0, -1, 0, 0, DSP);
}

printchar(c) {
	if ((c & 0177) < ' ') 
		printf("^%c", c + ('A' - 1));
	else if ((c & 0177) == 0177)
		printf("^?");
	else
		printf("%c", c);
}

INT fcor;
printmap(s,amap)
STRING	s; MAP *amap;
{
	int file;
	file=amap->ufd;
	printf("%s\t`%s'\n",s,(file<0 ? "-" : (file==fcor ? corfil : symfil)));
	printf("b1 = 0x%-16x",amap->b1);
	printf("e1 = 0x%-16x",amap->e1);
	printf("f1 = 0x%-x",amap->f1);
	printf("\nb2 = 0x%-16x",amap->b2);
	printf("e2 = 0x%-16x",amap->e2);
	printf("f2 = 0x%-x",amap->f2);
	printf("\n");
}

#define NUMREGS 24	/* number of hardware registers */
REGLIST reglist[];

printregs()
{
	REG REGPTR	p;

	for (p=reglist; p < &reglist[NUMREGS/2]; p++) {
		printf("%4.4s/  ", p->rname);
		prhex12(*(ADDR *)(((ADDR)&u)+p->roffs));
		printf("      %4.4s/  ",(p+NUMREGS/2)->rname);
		prhex(*(ADDR *)(((ADDR)&u)+(p+NUMREGS/2)->roffs));
		printf("\n");
	}
	printpc();
}

printpc()
{
	dot= *(ADDR *)(((ADDR)&u)+PC);
	prisploc();
	printins('i',ISP,chkget(dot,ISP));
	printf("\n");
}
	
/* print register */
REGLIST reglist[];
regout(name, prvar, fmt) 
char *name, *fmt; {
	REG REGPTR p;
	for (p=reglist; p< &reglist[24]; p++) {
		if (eqstr(name, p->rname)) {
			printit(0, prvar, *(ADDR *)(((ADDR)&u)+p->roffs),
				fmt[0] ? fmt : "d", N_GSYM, -1, 0, 0, DSP);
			return(p->roffs);
		}
	}
	error("Unknown register variable");
	return(-1);
}
/* Print symbolic location of dot */
prisploc() {
	struct proct *procp;
	int lineno;

	printf("0x%x", dot);
	procp = adrtoprocp(dot);
	if (procp != badproc) {
		printf(" (");
		prlnoff(procp, dot);
		printf("):  \t");
	} else
		printf(":  \t");
}
