#include "head.h"
#include <a.out.h>
struct user u;
#include <stdio.h>
INT signo;
INT adrflg;
INT pid;
L_INT cntval;


/* service routines for sub process control */


/*
 * single step until loc with descriptor format d is modified
 * return its new value.
 */
monex(loc, d)
ADDR loc; char d; {
	register ADDR oldval;
	
	oldval = getval(loc, d);
	do {
		subpcs('s');
	} while (oldval == getval(loc, d));
	return(getval(loc, d));
}

/* single step count source stmts */
singstep(count) {
	register int thisline, curline;
	register ADDR dot;
	register struct proct *thisproc;
	
	dot = *(ADDR *) (((ADDR) &u) + PC);
	thisproc = adrtoprocp(dot);
	thisline = adrtolineno(dot);
	if (count == 0) count = 1;
	for(; count; count--) {
		do {
			subpcs('s');
			dot = *(ADDR *) (((ADDR) &u) + PC);
			curline = adrtolineno(dot);
		} while (!signo && 
			((thisproc == adrtoprocp(dot) && thisline == curline) ||
			curline == -1));
		if (signo) return;
	}
}

doscall() {
	int subargs[NUMARGS];	     /* subargs[0]    = address, 
					subargs[1]    = number of arguments
					subargs[2:NUMARGS] = actual arguments */
	union {
		struct {
			int w[128-NUMARGS];
		};
		struct {
			char c[4*(128-NUMARGS)];
		};
	}substr;
	
	register int i, numchars, *subargp;
	register char ch;
	ADDR straddr, adr, praddr;
	ADDR j;
	
	praddr = extaddr(proc);
	if (praddr == -1) {
		printf("Cannot find %s\n", proc);
		return;
	}
	straddr = extaddr("_dbargs");
	if (straddr == -1) {
		error("Program not loaded with -lg");
		return;
	}
	
	numchars = 0;
	subargp = subargs;
	argsp++;
	*subargp++ = praddr;
	subargp++;
	
	for (i=0; i<NUMARGS - 2; i++) {  /* process an argument */
		ch = *argsp;
		if (ch == '\'') {
			*subargp++ = *(argsp+1);
			argsp += 2;
		} else if (ch == '"') {
			*subargp++ = straddr + sizeof subargs + numchars;
			argsp++;
			for (;;) {
				substr.c[numchars++] = ch = *argsp++;
				if (ch == '"') {
					substr.c[numchars-1] = '\0';
					break;
				}
				if (ch == '\0') {
					error("Unterminated string constant");
					return;
				}
				if (numchars > sizeof substr.c) {
					error("Too many string constants");
					return;
				}
			}
		} else if ((ch >= '0' && ch <= '9') || ch == '-') {
			*subargp++ = readint(&argsp);
		} else if ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') ||
			ch == '_') {
			cpname(var, argsp);
			j = varaddr(curproc()->pname, var);
			if (j == -1) {
				printf("Unknown variable: %s\n", argsp);
				return;
			}
			*subargp++ = getval(j, typetodesc(sl_type, 0)[0]);
			do {
				argsp++;
			} while (varchar(*argsp) || number(*argsp));
		} else if (ch != ')') {
			printf("Unexpected character %c\n", ch);
			return;
		}
	
		do {
			ch = *argsp++;
		} while(ch == ' ');
		if (ch == ')') {
 			dot = *(ADDR *)(((ADDR)&u)+PC) = extaddr("_dbsubc");
			if (dot == -1) {
				error("Internal error - cannot find _dbsubc");
				return;
			}
			adrflg = 1;
			cntval = 1;
			if (pid == 0 || signo) subpcs('r');
			subargs[1] = (subargp - subargs) - 2;
			adr = straddr;
			for (j=0; j<=(subargp-subargs); j++) {
				put(adr, DSP, subargs[j]);
				adr += WORDSIZE;
			}
			adr = straddr + sizeof subargs;
			for (j=0; j<(numchars+WORDSIZE-1)/WORDSIZE; j++) {
				put(adr, DSP, substr.w[j]);
				adr += WORDSIZE;
			}
			errflg = 0;
			subpcs('c');
 			dot = *(ADDR *)(((ADDR)&u)+PC);
			if (dot != extaddr("_dbsubn")) {
				if (!signo) printf("Breakpoint\n");
				else printf(" at\n");
				return;
			}
			if (*argsp++ == '/') {
				dispf((ADDR) 0, *argsp ? *argsp : 'd', N_RSYM, 0, 0);
			}
			else
				printf("Procedure returned normally\n");
			reset();
		}
		while (*argsp == ' ' || *argsp == ',')
			argsp++;
	}
	
	error ("Too many arguments");

}
