static	char sccsid[] = "@(#)xeq.c 4.3 %G%";
#include "head.h"
#include <a.out.h>
#include <stab.h>
struct user u;
#include <stdio.h>
INT signo;
INT adrflg;
INT pid;
ADDR userpc;
L_INT cntval;


/* service routines for sub process control */


/*
 * single step until loc with descriptor format d is modified
 * return its new value.
 */
monex(loc, d)
ADDR loc; char d; {
	register ADDR oldval;
	
	oldval = getval(loc, d, DSP);
	do {
		subpcs('s');
	} while (oldval == getval(loc, d, DSP));
	return(getval(loc, d, DSP));
}

/* single step count source stmts */
singstep(count, cmd)
	char cmd; {
	register int thisline, curline;
	register struct proct *thisproc;
	
	if (sdbttym.sg_flags != userttym.sg_flags)
		stty(2, &userttym);
	dot = *(ADDR *) (((ADDR) &u) + PC);
	thisproc = adrtoprocp(dot);
	thisline = adrtolineno(dot);
	if (count == 0) count = 1;
	for(; count; count--) {
		do {
			if (cmd == 'S') {  /* MACHINE DEPENDENT */
				dot = *(ADDR *) (((ADDR) &u) + PC);
				if ((get(dot,ISP) & 0xff) == 0xfb){ /* calls */
					int retaddr;
					subpcs('s');
					retaddr =
					   *(ADDR *) (((ADDR) &u) + USP) + 16;
					retaddr = dot = get(retaddr, DSP);
					subpcs('b');
					subpcs('c');
					dot = retaddr;
					subpcs('d');
					dot = *(ADDR *) (((ADDR) &u) + PC);
					if (retaddr != dot && signo == 0) {
						gtty(2, &userttym);
						if (sdbttym.sg_flags !=
							userttym.sg_flags)
							stty(2, &sdbttym);
						printf("Breakpoint at \n");
						return;
					}
					continue;
				}
			}

			subpcs('s');
			dot = *(ADDR *) (((ADDR) &u) + PC);
			curline = adrtolineno(dot);
		} while (!signo && 
			((thisproc == adrtoprocp(dot) && thisline == curline) ||
			curline == -1));
		gtty(2, &userttym);
		if (sdbttym.sg_flags != userttym.sg_flags)
			stty(2, &sdbttym);
		if (signo) return;
	}
}

doscall() {
	int subargs[NUMARGS];	 
		/* subargs[0]    = address, 
		 * subargs[1]    = number of arguments
		 * subargs[2:NUMARGS] = actual arguments
		 */
	union {
		int w[128-NUMARGS];
		char c[4*(128-NUMARGS)];
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
				ch = *argsp++;
				if (ch == '\\') {
					switch(ch = *argsp++) {
					case 'r':
						ch = '\015';
						break;
					case 'n':
						ch = '\012';
						break;
					case '\\':
						ch = '\\';
						break;
					}
				}
				substr.c[numchars++] = ch;
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
		} else if ((ch >= 'a' && ch <= 'z') ||
			(ch >= 'A' && ch <= 'Z') || ch == '_') {
			cpname(var, argsp);
			j = varaddr(curproc()->pname, var);
			if (j == -1) {
				return;
			}
			*subargp++ =
				sl_class == N_RSYM ?
				  *(ADDR *)(((ADDR) &u) + R0 + (WORDSIZE)*j) :
				  getval(j, typetodesc(sl_type, 0)[0], DSP);
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
			if (scallx == 0) {
				scallx = 1;
 				pcs = *(ADDR *)(((ADDR)&u)+PC);
 				fps = *(ADDR *)(((ADDR)&u)+FP);
 				aps = *(ADDR *)(((ADDR)&u)+AP);
				if (bkpts = scanbkpt(userpc)) {
					if (flagss = bkpts->flag) {
						bkpts->flag = BKPTSET;
					}
				}
			}
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
			dschar = *argsp++;
			errflg = 0;
			dopcs('c');
			if (!signo) printf("Breakpoint");
			printf(" at\n");
			return;
		}
		while (*argsp == ' ' || *argsp == ',')
			argsp++;
	}
	
	error ("Too many arguments");

}


/* get arguments from core file, place them in args */
getargs() {
	struct proct *procp;
	ADDR p, av;
	int ac, i;
	char *argsp = args;
	union {
		char c[WORDSIZE]; 
		int w;
		float f;
	} word;

	if ((procp = initframe()) == badproc) goto old1;
	do {
		if (eqstr("main", procp->pname))
			goto fnd;
	} while ((procp = nextframe()) != badproc);

old1:	cpstr(args, oldargs);
	printf("%s %s\n", symfil, args);
	return;

fnd:	ac = get(argp, DSP);
	if ((ac == 0) || (ac & 0xff)) goto old1;
	ac = get(argp+4, DSP);
	av = (ADDR) get(argp+8, DSP);

	av += WORDSIZE;
	ac--;

	for (; ac; ac--) {
		p = (ADDR) get(av, DSP);
		av += WORDSIZE;
		for (;;) {
			word.w = get(p, DSP);
			for (i=0; i<WORDSIZE; i++) {
				if (word.c[i] == '\0') goto l1;
				*argsp++ = word.c[i];
			}
			p += WORDSIZE;
		}
l1:		*argsp++ = ' ';
	}
	*argsp == '\0';
	printf("%s %s\n", symfil, args);
	return;


}

dopcs(c) 
char c; {
	if (c != 'r' && c != 'R' && sdbttym.sg_flags != userttym.sg_flags)
		stty(2, &userttym);
	subpcs(c);
	gtty(2, &userttym);
	if (sdbttym.sg_flags != userttym.sg_flags)
		stty(2, &sdbttym);
 	
	if (eqany(c, "cCsS") && 
	     *(ADDR *)(((ADDR)&u)+PC) == extaddr("_dbsubn")) {
		if (dschar == '/') {
			dispf((ADDR) 0, *argsp ? argsp : "d", N_RSYM, 0, 0, DSP);
		}
		else
			printf("Procedure returned normally\n");
 		userpc = dot = *(ADDR *)(((ADDR)&u)+PC) = pcs;
 		*(ADDR *)(((ADDR)&u)+FP) = fps;
 		*(ADDR *)(((ADDR)&u)+AP) = aps;
		if (bkpts)
			bkpts->flag = flagss;
		scallx = 0;
		longjmp(env, 0);
	}
}

/* execute commands from a breakpoint */
acommand(cmds)
char *cmds; {
	char *p = cmds;
	int endflg = 0;

	setcur(0);
	do { 		/* process a command */
		for (;;) {
			if (*p == ';') {
				*p = '\n';
				break;
			}
			if (*p == '\n') {
				endflg++;
				break;
			}
			p++;
		}
		if (decode(cmds) == 1) {
			printf("Bad command: ");
			do {
				printf("%c", *cmds);
			} while (*cmds++ != '\n');
			return;
		}
		docommand();
		if (!endflg)
			*p = ';';
		p = cmds = p + 1;
	} while (!endflg);
}
