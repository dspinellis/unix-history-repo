#
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

main(ac,av)

long	ac;
char	**av;

{
extern	 intr();
extern	 memsize();
extern	 except();
extern	 char *malloc();
extern	 char _sibuf[], _sobuf[];
	 long stats[8];
	 short unsigned magicnum;
	 long size;
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
bufopt = 1;	/* default to line buffering */

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
	{
	read(pipe,(char *)(&size),4);
	read(pipe,(char *)(&magicnum),2);
	}
else
	{
	prog = fopen(file,"r");
	if (prog == NULL)
		{
		perror(file);
		exit(1);
		}
	fstat(fileno(prog),&stats[0]);
	size = stats[4];
	fread((char *)(&magicnum),2,1,prog);
	if (magicnum == 0407)
		{
		fseek(prog,1024,0);
		fread((char *)(&magicnum),2,1,prog);
		size -= 1024;
		}
	}
size -=2;
if (magicnum == 0404)
	/* maintain compatability with 11/70 */
	addrsze = 8;
else if (magicnum == 0403)
	/* normal case */
	addrsze = 4;
else
	{
	fprintf(stderr,"%s is not a Pascal program\n",file);
	exit(1);
	}

/*
 * Load program into memory
 */
objprog = malloc(size);
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
	bytesread = fread(objprog,1,size,prog);
	fclose(prog);
	if (mode == PIX)
		unlink(file);
	}
if (bytesread != size)
	{
	fprintf(stderr,"Read error occurred while loading %s\n",file);
	exit(1);
	}
setbuf(stdin,&_sibuf[0]);
setbuf(stdout,&_sobuf[0]);
if (mode == PIX)
	fputs("Execution begins...\n",stderr);
/*
 * set interpreter to catch expected signals and begin interpretation
 */
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
/*
pfree(objprog);
*/
psexit(0);
}
