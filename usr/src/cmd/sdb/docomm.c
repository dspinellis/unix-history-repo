#include <signal.h>
#include "head.h"
#include <a.out.h>

struct user u;
L_INT 	cntval;
INT	signo;
INT	adrflg;
long	oldaddr = -1;

docommand() {
	register char	*p;
	register int	i;
	register ADDR	addr, odot;
	struct proct 	*procp;
	
	if (signo == SIGINT) signo = 0;
	cntval = 1;
	adrflg = 0;
	errflg = 0;

	if (scallf) {
		doscall();
		setcur();
		return;
	}
	
	if (reflag) {  /* search for regular expression */
		dore();
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
			return;
		}
		if (var[0] != 0) {
			printf("Unexpected null command\n");
			return;
		}
	}
		
	switch (cmd) {
	
	case 'a':
		debug = !debug;
		break;
		
	case 't':
		prframe();
		break;
		
	case 'e':
		p = args;
		if (*p == '\0') {
			printf("%.8s() in \"%s\"\n", curproc()->pname, curfile);
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
		break;
		
	l1:	/* argument is filename */
		finit(args);
		printf("\"%s\"\n", curfile);
		break;
		
	case 'p':
		if (integ) ffind(integ);
		fprint();
		break;
		
	case 'q':
		exit(0);
		
	case 'w':
		if (integ) ffind(integ);
		i = fline;
		fback(WINDOW/2);
		fprintn(WINDOW);
		ffind(i);
		break;
		
	case 'x':
		prdebug();
		break;

	case 'z':
		if (integ) ffind(integ);
		fprintn(WINDOW);
		break;

	case '-':
		fback(integ ? integ : 1);
		fpargs();
		break;

	case '+':
		fforward(integ ? integ : 1);
		fpargs();
		break;

	case '\n':
		fforward(1);
		fprint();
		break;

	case '\004':
		fforward(1);
		printf("\b");
		fprintn(WINDOW);
		break;

	case 'r':
		if (debug) error("calling subpcs");
		if (integ) cntval = integ;
		if (!executing) {
			executing = TRUE;
			if (integ) cntval = integ;
			subpcs('r');
			executing = FALSE;
		}
		if (debug) error("exiting subpcs");
/*
		setcur();
		break;
*/

	case 'c':
		if (debug) error("calling subpcs");
		if (integ) cntval = integ;
		subpcs('c');
		if (debug) error("exiting subpcs");
		if (!signo) printf("Breakpoint");
		printf(" at\n");
		setcur();
		break;
		
	case 's':
		singstep(integ ? integ : 1);
		if (signo) printf("\n");
		setcur();
		break;
		
	case 'n':
		odot = *(ADDR *) (((ADDR) &u) + PC);
		for (i=1; i<100; i++) {
			dot = getaddr(adrtoprocp(odot)->pname, adrtolineno(odot)+i);
			if (dot != odot || dot == -1) break;
		}
		if (odot == dot  ||  dot == -1) {
			error("Cannot find next line");
			break;
		}
		if (debug) printf("Setting bkpt with i=%d at %d, odot = %d\n", i, dot, odot);
		odot = dot;
		subpcs('b');
		subpcs('c');
		if (!signo) printf("Next statement\n");
		else printf(" at\n");
		setcur();
		dot = odot;
		subpcs('d');
		break;
		
	case 'b':
		if (proc[0] == '\0' && integ == 0) {
			prbkpt();
		}
		else {
			dot = getaddr(proc,integ);
			if (dot == -1) {
				error("Cannot set breakpoint");
				break;
			}
			subpcs('b');
			printf("%.8s:%d b\n",adrtoprocp(dot)->pname,
				adrtolineno(dot));
		}
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
		subpcs('d');
		break;
		
	case 'm':
		addr = varaddr(proc[0] ? proc : curproc()->pname, var);
		printf("stopped with value %d\n", monex(addr, 'd'));
		setcur();
		break;
		
	case '/':
		if (var[0] == '.' && var[1] == '\0') {
			if (integ == 0) integ = oldaddr;
			dispf((ADDR) integ, args[0] ? args : odesc,
				oclass == N_RSYM ? oclass : N_GSYM, otype, 0);
			oldaddr = integ;
			break;
		}
		if (integ) {
			dispf((ADDR) integ, args, N_GSYM, 0, 0);
			break;
		}
		oldaddr = dispvar(proc[0] ? proc : curproc()->pname, var, args);
		break;
		
	case '=':
		if (var[0] == '\0')
			addr = getaddr(proc, integ);
		else
			addr = varaddr(proc[0] ? proc : curproc()->pname, var);
		if (addr == -1)
			error("Unknown address");
		else
			printf("0x%x\n", addr);
		break;

	case '!':
		if (var[0] == '\0')
			addr = getaddr(proc, integ);
		else
			addr = varaddr(proc[0] ? proc : curproc()->pname, var);
		if (addr == -1) 
			error("Unknown variable");
		else
			if (sl_class == N_RSYM)
				putreg(addr,typetodesc(sl_type,subflag)[0],argvalue(args));
			else
				putval(addr,typetodesc(sl_type,subflag)[0],argvalue(args));
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
