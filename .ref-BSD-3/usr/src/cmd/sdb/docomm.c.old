#include <signal.h>
#include "head.h"
#include <a.out.h>

struct user u;
L_INT 	cntval;
INT	signo;
INT	adrflg;
INT	pid;
ADDR	userpc;
char	*s;
enum	{NOCOM, PRCOM, DSCOM} lastcom;

docommand() {
	register char	*p;
	register int	i;
	register ADDR	addr, bkaddr;
	struct proct 	*procp;
	
	cntval = 1;
	adrflg = 0;
	errflg = 0;

	if (scallf) {
		doscall();
		setcur(1);
		lastcom = NOCOM;
		return;
	}
	
	if (reflag) {  /* search for regular expression */
		dore();
		lastcom = PRCOM;
		return;
	}
	
	if (cmd == '\0') {
		if (integ != 0 && var[0] != '\0') {
			error("Invalid command (1)");
			return;
		}
		if (integ != 0) { /* print line number */
			ffind(integ);
			fprint();
			lastcom = PRCOM;
			return;
		}
		if (var[0] != 0) {
			printf("Unexpected null command\n");
			return;
		}
	}
		
	switch (cmd) {
	
	case 'Y':
		debug = !debug;
		break;

	case 'V':
		version();
		break;

	case 'a':
		if (integ) {
			cpstr(args, "l\n");
		} else if (proc[0]) {
			cpall(args, "T\n");
		} else {
			error("Bad arguments");
			break;
		}
		goto setbrk;
		break;	

	case 'l':
		setcur(1);
		lastcom = NOCOM;
		break;
		
	case 'T':
		prfrx(1);
		lastcom = NOCOM;
		break;
		
	case 't':
		prframe();
		lastcom = NOCOM;
		break;
		
	case 'e':
		p = args;
		if (*p == '\0') {
			printf("%.8s() in \"%s\"\n",
				curproc()->pname, curfile);
			break;
		}

		while (*p != '\0')
			if (*p++ == '.') goto l1;
		/* argument is procedure name */
		procp = findproc(args);
		if ((procp->pname[0] != '\0') && (procp->sfptr != badfile)) {
			finit(adrtofilep(procp->paddr)->sfilename);
			ffind(procp->lineno);
		}
		else printf("Can't find %s\n", args);
		printf("%.8s() in \"%s\"\n", curproc()->pname, curfile);
		lastcom = NOCOM;
		break;
		
	l1:	/* argument is filename */
		finit(args);
		printf("\"%s\"\n", curfile);
		lastcom = NOCOM;
		break;
		
	case 'p':
		if (integ) ffind(integ);
		fprint();
		lastcom = PRCOM;
		break;
		
	case 'q':
		exit(0);
		
	case 'w':
		if (integ) ffind(integ);
		i = fline;
		fback(WINDOW/2);
		fprintn(WINDOW);
		ffind(i);
		lastcom = PRCOM;
		break;
		
	case 'X':
		prdebug();
		break;

	case 'z':
		if (integ) ffind(integ);
		fprintn(WINDOW);
		lastcom = PRCOM;
		break;

	case '-':
		fback(integ ? integ : 1);
		fpargs();
		lastcom = PRCOM;
		break;

	case '+':
		fforward(integ ? integ : 1);
		fpargs();
		lastcom = PRCOM;
		break;

	case '\n':
		switch (lastcom) {
		case PRCOM:
			fforward(1);
			fprint();
			break;
		case DSCOM:
			oaddr += oincr ? oincr : typetosize(otype, WORDSIZE);
			printf("0x%x/ ", oaddr);
			dispf((ADDR) oaddr, odesc,
			    oclass == N_RSYM ? oclass : N_GSYM, otype, 0, 0);
			break;
		}
		break;

	case '\004':
		fforward(1);
		printf("\b");
		fprintn(WINDOW);
		lastcom = PRCOM;
		break;

	case 'r':
		if (args[0] == '\0') getargs();
	case 'R':
		signo = 0;
		cpstr(oldargs, args);
		if (debug) error("calling dopcs");
		if (integ) cntval = integ;
		if (!executing) {
			executing = TRUE;
			if (integ) cntval = integ;
			dopcs('r');
			executing = FALSE;
		}
		if (debug) error("exiting dopcs");
		bkaddr = -1;
		goto f1;

	case 'c':
		signo = 0;
	case 'C':
		if (proc[0] != '\0' || integ != 0) {
			dot = getaddr(proc, integ);
			if (dot == -1) {
				error("Cannot set temporary breakpoint");
				break;
			}
			dopcs('b');
			bkaddr = dot;
		} else
			bkaddr = -1;
		integ = atoi(args);

f1:		if (debug) error("calling dopcs");
		if (integ) cntval = integ;
		dopcs('c');
		if (debug) error("exiting dopcs");
		if (bkaddr != -1) {
			ADDR dotsave;
			dotsave = dot;
			dot = bkaddr;
			dopcs('d');
			dot = dotsave;
		}
		if (!signo) printf("Breakpoint");
		printf(" at\n");
		setcur(1);
		lastcom = NOCOM;
		break;
		
	case 'S':
	case 's':
		signo = 0;
		singstep(integ ? integ : 1, cmd);
		if (signo) printf("\n");
		setcur(1);
		lastcom = NOCOM;
		break;
		
	case 'g':
		if (pid == 0  ||  signo) {
			error("Not stopped at breakpoint");
		}
		dot = getaddr(proc, integ);
		if (dot == -1) {
			error("Bad address");
			break;
		}
		adrflg = 1;
		integ = atoi(args);
		if (integ) cntval = integ;
		dopcs('c');
		if (!signo) printf("Breakpoint");
		printf(" at\n");
		setcur(1);
		lastcom = NOCOM;
		break;

	case 'k':
		if (scallx) {
	 		userpc = dot = *(ADDR *)(((ADDR)&u)+PC) = pcs;
	 		*(ADDR *)(((ADDR)&u)+FP) = fps;
	 		*(ADDR *)(((ADDR)&u)+AP) = aps;
			if (bkpts)
				bkpts->flag = flagss;
			scallx = 0;
			error("Procedure killed");
			longjmp(env, 0);
		} else {
			dopcs('k');
			printf("\n");
			lastcom = NOCOM;
			break;
		}

	case 'B':
		prbkpt();
		break;

	case 'b':
	setbrk:
		if (proc[0] == '\0' && integ == 0) {
			integ = fline;
		}
		dot = getaddr(proc,integ);
		if (dot == -1 || dot == 0) {
			error("Cannot set breakpoint");
			break;
		}
		dopcs('b');
		s = " b\n";
		s[1] = cmd;
		printbkpt(s, adrtoprocp(dot)->pname,
			adrtolineno(dot));
		break;
		
	case 'd':
		if (proc[0] == '\0' && integ == 0) {
			idbkpt();
			break;
		}
		dot = getaddr(proc,integ);
		if (dot == -1) {
			error("Non existent breakpoint");
			break;
		}
		dopcs('d');
		break;
		
	case 'D':
		dabkpt();
		error("All breakpoints deleted");
		break;

	case 'm':
		addr = varaddr(proc[0] ? proc : curproc()->pname, var);
		printf("stopped with value %d\n", monex(addr, 'd'));
		setcur(1);
		lastcom = NOCOM;
		break;
		
	case '/':
		if (var[0] == '.' && var[1] == '\0') {
			if (integ == 0) integ = oaddr;
			dispf((ADDR) integ, args[0] ? args : odesc,
			    oclass == N_RSYM ? oclass : N_GSYM, otype, 0, 0);
			oaddr = integ;
		} else
		if (integ && (var[0] == '\0')) {
			dispf((ADDR) integ, args, N_GSYM, 0, 0, 0);
			oaddr = integ;
			cpstr(odesc, args);
			oclass = N_GSYM;
			otype = 0;
		} else
			dispvar(proc, var, args);
		lastcom = DSCOM;
		break;
		
	case '=':
		if (var[0] == '\0') {
			if (proc[0]) {
				addr = getaddr(proc, integ);
				if (addr == -1) {
					error("Unknown address");
					break;
				}
			}
			else
				addr = integ;
			dispf(addr, args[0] ? args : "x", 0, -1, 0, 0);
		} else 
			findvar(proc, var, args[0] ? args : "x", 2);
		break;

	case '!':
		if (var[0] == '\0')
			addr = getaddr(proc, integ);
		else
			addr = varaddr(proc, var);
		if (addr == -1) 
			error("Unknown variable");
		else {
			if (number(args[0]) || eqany(args[0], ".-")) {
				char *p;
				double atof();
				union {
					struct{
						int w1, w2;
					};
					struct {
						double d;
					};
				} dbl;

				p = (args[0] == '-') ? args+1 : args;
				for (; *p != '.' && *p != 'e'; p++) {
					if (!number(*p)) goto l2;
				}
				dbl.d = atof(args);
				putval(addr, 'd', dbl.w1);
				if (typetodesc(sl_type,0)[0] == 'g')
					putval(addr+WORDSIZE, 'd', dbl.w2);
				break;
			}
l2: 			if (sl_class == N_RSYM && addr < 16)
				putreg(addr,typetodesc(sl_type,subflag)[0],
						argvalue(args));
			else
				putval(addr,typetodesc(sl_type,subflag)[0],
						argvalue(args));
		}
		lastcom = NOCOM;
		break;

	case '"':
		printf(args);
		break;
	}
}

fpargs() {
	register int i;
	
	switch(args[0]) {
	case 'p':
	case '\0':
		fprint();
		break;
case 'w':
		i = fline;
		fback(WINDOW/2);
		fprintn(WINDOW);
		ffind(i);
		break;
	case 'z':
		fprintn(WINDOW);
		break;
	}
}
