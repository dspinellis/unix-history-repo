/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)int.c 4.1 10/10/80";

/*
 * px - interpreter for Berkeley Pascal
 * Version 2.0 Winter 1979
 *
 * Original version for the PDP 11/70 authored by:
 * Bill Joy, Charles Haley, Ken Thompson
 *
 * Rewritten for VAX 11/780 by Kirk McKusick
 */

#include	"stdio.h"
#include	"signal.h"
#include	"h00vars.h"
#include	"objfmt.h"

main(ac,av)

long	ac;
char	**av;

{
extern	 intr();
extern	 memsize();
extern	 except();
extern	 syserr();
extern	 char *malloc();
extern   long createtime;
struct	 pxhdr pxhd;
register long bytesread, block;
register char *objprog;
register FILE *prog;
#define	 pipe 3
#define	 pipesize 4096

/*
 * Initialize everything
 */
argc = ac;
argv = av;
stcnt = 0;
stlim = 500000;
llimit = 0x7fffffff;	/* set to unlimited */
nodump = 0;

/*
 * Determine how PX was invoked, and how to process the program 
 */
if (argv[0][0] == '-' && argv[0][1] == 'o')
	{
	file = &argv[0][2];
	mode = PIX;
	}
else if (argc <= 1)
	{
	file = "obj";
	mode = PX;
	}
else if (argv[1][0] != '-')
	{
	file = argv[1];
	mode = PX;
	}
else if (argv[1][1] == 0)
	{
	file = argv[0];
	mode = PIPE;
	argc -= 1;
	argv[1] = argv[0];
	argv = &argv[1];
	}
else
	{
	fputs("Improper specification of object file to PX\n",stderr);
	exit(1);
	}

/*
 * Process program header information
 */
if (mode == PIPE)
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
if (mode == PIPE)
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
	if (mode == PIX)
		unlink(file);
	}
if (bytesread != pxhd.objsize)
	{
	fprintf(stderr,"Read error occurred while loading %s\n",file);
	exit(1);
	}
if (mode == PIX)
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
#ifdef profile
interpret(objprog,1);
#else
interpret(objprog,0);
#endif
/*
 * reset signals, deallocate memory, and exit normally
 */
signal(SIGINT,SIG_IGN);
signal(SIGSEGV,SIG_DFL);
signal(SIGFPE,SIG_DFL);
pflush();
/* pfree(objprog); */
psexit(0);
}
