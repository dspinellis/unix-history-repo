#include "head.h"
#include <a.out.h>
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
 * prflag: as in findvar
 *
 * Here and elsewhere we assume that -1 is an invalid address, and
 *	its is used to indicate error.
 */
outvar(proc, var, fmt, metaflag, addr, class, subflag, prnamep, comblk, prflag)
ADDR addr; char *proc, *var, *fmt, class, *prnamep, *comblk; {
	char *p, *q, *r, *oldpr;
	register int match;
	long soffset, goffset;
	register ADDR newaddr = -1, arrowaddr;
	register enum {INIT, ARROW, DOT} typeflag;

	switch (var[0]) {
	case '\0':
		if (prflag == 0) return(addr);
		if (metaflag) {
			if (comblk[0] && !(eqstr(comblk, "*")))
				printf("%.8s:%.8s", comblk, prname);
			else if (proc[0])
				printf("%.8s:%.8s", proc, prname);
			else
				printf("%s", prname);
		if (prflag == 1)
			printf("/ ");
		else
			printf("= ");
		}
		if (prflag == 1)
			dispf(addr, fmt, class, sl_type, sl_size, subflag);
		else
			dispf(addr, fmt, 0, -1, 0, 0);
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
			subflag+1, prnamep, comblk, prflag));
		
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
			class, subflag, prnamep, comblk, prflag);
		
		if (!metaflag)
			return(newaddr);
l:;	}
}

prdebug() {
	register struct proct *procp;
	register struct filet *filep;
	
	printf("dot=%d\n", dot);
	printf("extstart = %d\n", extstart);
	for(filep=files;filep->sfilename[0];filep++)
		printf("%s offs %d @ %d flag %d addr 0x%x\n", filep->sfilename, filep->stf_offset, filep, filep->lineflag, filep->faddr);
	for(procp=procs;procp->pname[0];procp++) {
		printf("%8.8s addr 0x%x; offs %d; sfptr %d; line %d",
			procp->pname, procp->paddr, procp->st_offset,
			procp->sfptr, procp->lineno);
		if (procp->entrypt) printf(" entrypoint");
		printf("\n");
	}
}

/* display addr using format desc or class s */
dispf(addr, desc, class, type, size, subflag)
char *desc; short type; ADDR addr; {
	dispx(addr, desc, class, type, size, subflag);
	printf("\n");
}

char pd[] = "%x";
dispx(addr, desc, class, type, size, subflag)
char *desc; short type; ADDR addr; {
	int i, sflag;
	char *p;
	char dlen, dfmt;
	long value;
	union {
		struct {
			char c[WORDSIZE]; 
			};
		struct {
			int w;
		};
		struct {
			float f;
		}
	} word;
	union {
		struct{
			int w1, w2;
		};
		struct {
			double d;
		};
	} dbl;

	class &= STABMASK;
	if (desc[0]  == '\0') desc = typetodesc(type, subflag);
	cpstr(odesc, desc);
	otype = type;
	oclass = class;
	oaddr = addr;
	oincr = 0;
	if (debug) printf("dispf(%d,%s,0%o,0%o)\n", addr,desc,class,type);
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
			case 'o':
			case 'p':
			case 's':
			case 'u':
			case 'x':
				pd[1] = dfmt = *p;
				break;

			default:
				printf("Illegal descriptor: %c\n", *p);
				return;
			}
		}

		if (type == -1)
			value = addr;
		else if (class == N_RSYM && addr < 16) {
			/* MACHINE DEPENDENT */
			if ((addr > 0 && addr < 6) || addr > 11) {
				printf("Bad register var %d\n", addr);
				return;
			}
			value = *(ADDR *)(((ADDR) &u) + R0 + (WORDSIZE)*addr);
		}
		else {
			value = getval(addr, dfmt == 'g' ? 'd' : dfmt);
		}

		if (errflg) {
			printf("%s", errflg);
			errflg = 0;
			return;
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
				}
			}
			if (value > 0) {
				if (value > 9  &&  dfmt == 'x')
					printf("0x");
				else if (value > 7  &&  dfmt == 'o')
					printf("0");
			}
			printf(pd, value);
			return;
			
		case 'f':
			pd[1] = 'g';
			word.w = value;
			printf(pd, word.f);
			return;

		case 'g':
			dbl.w1 = value;
			dbl.w2 = (class == (char) N_RSYM) ?
				*(ADDR *)(((ADDR) &u)+R0+(WORDSIZE)*(addr+1)) :
				getval(addr+WORDSIZE, 'd');
			printf("%.13g", dbl.d);
			return;

		case 'p':
			printf("%s:%d", adrtoprocp(value)->pname,
				adrtolineno(value));
			return;

		case 's':
			addr = getindir(class, addr, type);
			goto aa;
				
		case 'c':
			if (size <= 1) {
				oincr = 1;
				printchar(value);
				return;
			} else 
				goto aa;

		case 'a':
		aa:	sflag = size == 0;
			if (sflag)
				size = 128;  /* maximum length for s and a */
			else
				oincr = size;
			for (;;) {
				word.w = getval(addr, 'd');
				for (i=0; i<WORDSIZE; i++) {
					if (sflag && word.c[i] == 0) 
						return;
					if (size-- == 0)
						return;
					printchar(word.c[i]);
				}
				addr += WORDSIZE;
			}

	}
}

printchar(c) {
	if ((c & 0177) < ' ') 
		printf("^%c", c + ('A' - 1));
	else if ((c & 0177) == 0177)
		printf("^?");
	else
		printf("%c", c);
}

