/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)int.c	5.2 (Berkeley) %G%";
#endif not lint

/*
 * px - interpreter for Berkeley Pascal
 * Version 3.0 Winter 1979
 *
 * Original version for the PDP 11/70 authored by:
 * Bill Joy, Charles Haley, Ken Thompson
 *
 * Rewritten for VAX 11/780 by Kirk McKusick
 */

#include	<signal.h>
#include	"whoami.h"
#include	"vars.h"
#include	"libpc.h"
#include	"objfmt.h"

/*
 * New stuff for pdx
 */

extern char *end;
extern loopaddr();
extern union progcntr pdx_pc;	/* address of interpreter program cntr */
static void inittrap();

main(ac,av)

	int	ac;
	char	**av;

{
	register char *objprog, *file;
	char *name;
	register long bytesread, bytestoread, block;
	register FILE *prog;
	struct	 pxhdr pxhd;
#	define	 pipe 3

	/*
	 * Initialize everything
	 */
	_argc = ac;
	_argv = av;
	_nodump = FALSE;

	/*
	 * Determine how PX was invoked, and how to process the program 
	 */
	file = _argv[1];
	if (!strcmp(_argv[0], "pdx")) {
		_mode = PDX;
		_argv += 2; _argc -= 2;
		name = _argv[0];
	} else if (!strcmp(_argv[0], "pix")) {
		_mode = PIX;
		_argv++; _argc--;
		name = _argv[0];
	} else if (!strcmp(_argv[0], "pipe")) {
		_mode = PIPE;
		file = "PIPE";
		_argv++; _argc--;
		name = _argv[0];
	} else {
		_mode = PX;
		if (_argc <= 1)
			file = "obj";
		name = file;
	}

	/*
	 * kludge to check for old style objs.
	 */
	if (_mode == PX && !strcmp(file, "-")) {
		fprintf(stderr, "%s is obsolete and must be recompiled\n",
		    _argv[0]);
		exit(1);
	}
	/*
	 * Process program header information
	 */
	if (_mode == PIPE) {
		read(pipe,&pxhd,sizeof(struct pxhdr));
	} else {
		prog = fopen(file,"r");
		if (prog == NULL) {
			perror(file);
			exit(1);
		}
		fread(&pxhd,sizeof(struct pxhdr),1,prog);
		if (pxhd.magicnum != MAGICNUM) {
			fseek(prog,(long)(HEADER_BYTES-sizeof(struct pxhdr)),0);
			fread(&pxhd,sizeof(struct pxhdr),1,prog);
		}
	}
	if (pxhd.magicnum != MAGICNUM) {
		fprintf(stderr,"%s is not a Pascal interpreter file\n",name);
		exit(1);
	}
	if (pxhd.maketime < createtime) {
		fprintf(stderr,"%s is obsolete and must be recompiled\n",name);
		exit(1);
	}

	/*
	 * Load program into memory
	 */
	objprog = malloc((int)pxhd.objsize);
	if (_mode == PIPE) {
		bytestoread = pxhd.objsize;
		bytesread = 0;
		do	{
			block = read(pipe,(int)(objprog+bytesread),bytestoread);
			if (block > 0) {
				bytesread += block;
				bytestoread -= block;
			}
		} while (block > 0);
	} else {
		bytesread = fread(objprog,1,(int)pxhd.objsize,prog);
		fclose(prog);
	}
	if (bytesread != pxhd.objsize) {
		fprintf(stderr,"Read error occurred while loading %s\n",file);
		exit(1);
	}
	if (_mode == PIX)
		fputs("Execution begins...\n",stderr);
	/*
	 * set interpreter to catch expected signals and begin interpretation
	 */
	signal(SIGILL,syserr);
	signal(SIGBUS,syserr);
	signal(SIGSYS,syserr);
	if (signal(SIGINT,SIG_IGN) != SIG_IGN)
		signal(SIGINT,intr);
	signal(SIGSEGV,memsize);
	signal(SIGFPE,EXCEPT);
	signal(SIGTRAP,liberr);

	/*
	 * See if we're being watched by the debugger, if so set a trap.
	 */
	if (_mode == PDX || (_mode == PIX && pxhd.symtabsize > 0)) {
		inittrap(&_display, &_dp, objprog, &pdx_pc, loopaddr);
	}

	/*
	 * do it
	 */
	interpreter(objprog);
	/*
	 * reset signals, deallocate memory, and exit normally
	 */
	signal(SIGINT,SIG_IGN);
	signal(SIGSEGV,SIG_DFL);
	signal(SIGFPE,SIG_DFL);
	signal(SIGTRAP,SIG_DFL);
	signal(SIGILL,SIG_DFL);
	signal(SIGBUS,SIG_DFL);
	signal(SIGSYS,SIG_DFL);
	PFLUSH();
	psexit(0);
}

/*
 * Generate an IOT trap to tell the debugger that the object code
 * has been read in.  Parameters are there for debugger to look at,
 * not the procedure.
 */

static void
inittrap(dispaddr, dpaddr, endaddr, pcaddr, loopaddrp)
union disply *dispaddr;
struct disp *dpaddr;
char *endaddr;
union progcntr *pcaddr;
char **loopaddrp;
{
	kill(getpid(), SIGIOT);
}
