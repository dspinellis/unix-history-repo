/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)int.c 1.1 %G%";

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
#include	"vars.h"
#include	"objfmt.h"

main(ac,av)

long	ac;
char	**av;

{
register long bytesread, block;
register char *objprog, *file;
register FILE *prog;
struct	 pxhdr pxhd;
#define	 pipe 3
#define	 pipesize 4096

/*
 * Initialize everything
 */
_argc = ac;
_argv = av;
_nodump = 0;

/*
 * Determine how PX was invoked, and how to process the program 
 */
if (_argv[0][0] == '-' && _argv[0][1] == 'o')
	{
	file = &_argv[0][2];
	_mode = PIX;
	}
else if (_argc <= 1)
	{
	file = "obj";
	_mode = PX;
	}
else if (_argv[1][0] != '-')
	{
	file = _argv[1];
	_mode = PX;
	}
else if (_argv[1][1] == 0)
	{
	file = _argv[0];
	_mode = PIPE;
	_argc -= 1;
	_argv[1] = _argv[0];
	_argv = &_argv[1];
	}
else
	{
	fputs("Improper specification of object file to PX\n",stderr);
	exit(1);
	}

/*
 * Process program header information
 */
if (_mode == PIPE)
	read(pipe,&pxhd,sizeof(struct pxhdr));
else
	{
	prog = fopen(file,"r");
	if (prog == NULL)
		{
		perror(file);
		exit(1);
		}
	fseek(prog,HEADER_BYTES-sizeof(struct pxhdr),0);
	fread(&pxhd,sizeof(struct pxhdr),1,prog);
	}
if (pxhd.maketime < createtime)
	{
	fprintf(stderr,"%s is obsolete and must be recompiled\n",file);
	exit(1);
	}
if (pxhd.magicnum != 0403)
	{
	fprintf(stderr,"%s is not a Pascal program\n",file);
	exit(1);
	}

/*
 * Load program into memory
 */
objprog = malloc(pxhd.objsize);
if (_mode == PIPE)
	{
	bytesread = 0;
	do
		{
		block = read(pipe,objprog+bytesread,pipesize);
		bytesread += block;
		}
		while (block);
	}
else
	{
	bytesread = fread(objprog,1,pxhd.objsize,prog);
	fclose(prog);
	if (_mode == PIX)
		unlink(file);
	}
if (bytesread != pxhd.objsize)
	{
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
signal(SIGFPE,except);
signal(SIGTRAP,liberr);
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
/* pfree(objprog); */
psexit(0);
}
