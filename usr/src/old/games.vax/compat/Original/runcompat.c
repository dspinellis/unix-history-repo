#

static char sccsid[] = "	runcompat.c	1.1	82/05/12	";

/*
 *	Compatability mode support under UNIX-32V
 *	written by Art Wetzel during August 1979
 *	at the Interdisciplinary Dept of Information Science
 *	Room 711, LIS Bldg
 *	University of Pittsburgh
 *	Pittsburgh, Pa 15260
 *
 *	No claims are made on the completeness of the support of any
 *	of the systems simulated under this package
 */
#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "defs.h"
#ifdef UNIX
#include "unixhdr.h"
#endif
#ifdef RT11
#include "rt11.h"
#endif
struct	stat	stat32v;
unsigned short regs[8];
unsigned long psl;
unsigned short *pc;
int incompat;
char	*progname;
char	*nameend;
main(argc, argv, envp) int argc; char **argv, **envp; {
	if(argc < 2){
		printf(stderr,"Usage: %s [-rootdir] file args...\n",argv[0]);
		exit(1);
	}
	/* remember where the program name etc should go for using ps */
	progname = argv[0];
	nameend = envp[0]-1;
	argv++;
	/* set up alternate root directory if flagged for */
	if(*argv[0] == '-') {
		if(chroot(argv[0]+1)) {
			fprintf(stderr,"Can't change root to %s\n",argv[0]+1);
			exit(-1);
		}
		argv++;
	}
	/* check out file stats of file to run */
	if(stat(argv[0], &stat32v)) {
		fprintf(stderr,"%s does not exist\n",argv[0]);
		exit(1);
	}
	/* a version of SETUID and SETGID file executions */
	/* the binary of this program should be SETUID root for this to work */
	/* requires nonstandard seteuid and setegid sys calls */
	if(!(stat32v.st_mode & S_ISGID) || setegid(stat32v.st_gid))
		/* if not SETGID file or error, drop back to real group */
		setgid(getgid());
	if(!(stat32v.st_mode & S_ISUID) || seteuid(stat32v.st_uid))
		/* if not SETUID file or error, drop back to real uid */
		setuid(getuid());
#ifdef V6UNIX
	/* no umasks in version 6 */
	umask(0);
#endif
	/* go try to execute , passing along args and environment */
	execute(argv[0], argv, envp);
	/* only get here if execute fails */
	fprintf(stderr,"Execution failure on %s\n",argv[0]);
	exit(1);
}
execute(file, argv, envp) char *file, **argv, **envp; {
	int fd, n, tloadpt, dloadpt, tloadsize, dloadsize, stacksize;
	register short *p;
	extern illtrap();
	/* file to run should be readable */
	if((fd = open(file, 0)) == -1) {
		fprintf(stderr,"Can't open %s for read access\n",file);
		return(-1);
	}
#ifdef UNIX
	if((n = read(fd, &header, sizeof header)) != sizeof header)
		return(ENOEXEC);
	/* check to see if really unix file */
	if(header.magic != MAGIC1 && header.magic != MAGIC2 &&
		header.magic != MAGIC3 && header.magic != MAGIC4) {
		return(ENOEXEC);
	}
	/* if a UNIX-32V file run it */
	if(header.textsize == 0) {
		close(fd);
		return(execv(file, argv,  envp));
	}
	/* checks out OK as PDP-11 UNIX file */
	if(header.magic == MAGIC3) {
		fprintf(stderr,"%s compiled for separate I/D space\n",argv[0]);
		return(-1);
	}
	/* unix text loads at 0 */
	tloadpt = 0;
	/* set starting pc value */
	pc = (unsigned short *)header.entry;
	/* figure out where to load initialized data */
	dloadpt = tloadsize = header.textsize;
	/* check if alignment of data segment to 8k byte boundary */
	if(header.magic == MAGIC2)
		dloadpt = (dloadpt+8191) & (~8191);
	/* how much data */
	dloadsize = header.datasize;
	stacksize = header.bsssize;
#endif
#ifdef RT11
	if((n = read(fd, shortspace, RTHDRSIZ)) != RTHDRSIZ) {
		fprintf(stderr,"Error reading 1st block\n");
		return(-1);
	}
	/* rt11 files are 0 aligned including the header */
	tloadpt = RTHDRSIZ;
	/* set starting pc value */
	pc = (unsigned short *)shortspace[RTPC];
	/* initialize stack location */
	regs[6] = shortspace[RTSP];
	/* figure how much to load */
	dloadpt = tloadsize = shortspace[RTHGH]-RTHDRSIZ;
	/* no separate data as in unix */
	dloadsize = 0;
	stacksize = 0;
#endif
	/* see if it all fits into available memory space */
	if((dloadpt+dloadsize+stacksize) > (int)memsiz) {
		fprintf(stderr,"File too big to run\n");
		return(-1);
	}
	/* read text segment */
	if((n = read(fd, tloadpt, tloadsize)) < tloadsize) {
		fprintf(stderr,"Text read failure\n");
		return(-1);
	}
	/* read data segment */
	if((n = read(fd, dloadpt, dloadsize)) < dloadsize) {
		fprintf(stderr,"Data read failure\n");
		return(-1);
	}
	/* clear out the rest of memory */
	p = (short *)(dloadpt + dloadsize);
	while(p < (short *)memsiz) *p++ = 0;
	/* close file before starting it */
	close(fd);
	/* set up illegal instruction trapping */
	signal(SIGILL, illtrap);
	/* lets give it a try */
	start(argv, envp);
}
illtrap(){
	unsigned short *pcptr;
	int instr;
	register int i;
	extern getregs();
	/* record the fact that we are not in compatability mode now */
	incompat = 0;
	/* get the register values before they get clobbered */
	getregs();
	/* figure out what the pc was */
	pcptr = (unsigned short *)((char *)&pcptr + 20);
	pc = (unsigned short *) *pcptr;
	/* get the instruction */
	instr = *pc;
	/* incriment the pc over this instruction */
	pc++;
	/* set register 7 as pc synonym */
	regs[7] = (unsigned short)(int)pc;
	/* set up psl with condition codes */
	/* a UNIX-32V monitor patch is required to not clear condition codes */
	psl = 0x83c00000 | (*(pcptr - 6) & 017);
	/* pick out the appropriate action for this illegal instruction */
	switch(instr>>8){
	case	TRAPS:
		dotrap(instr & 0377);
		break;
	case	EMTS:
		if(sigvals[SIGEMT] && ((sigvals[SIGEMT]%2) != 1)) {
			dosig(SIGEMT, pc);
			break;
		}
		doemt(instr & 0377);
		break;
	default:
		if(instr >= 075000 && instr < 075040) {
			/* fis instructions */
			if(dofloat(instr) == 0)
				break;
		}
		if(instr >=  0170000) {
			/* floating point unit instructions */
			if(dofloat(instr) == 0)
				break;
		}
		/* genuine illegal instruction */
		/* if signal trap set go to user's trap location */
		if(sigvals[SIGILL] && ((sigvals[SIGILL]%2) != 1)) {
			dosig(SIGILL, pc);
			break;
		}
		/* ignore uncaught setd instructions */
		if(instr == SETD)
			break;
		/* otherwise put out a message and quit */
		printf("illegal instruction, psl 0x%08x, pc 0%04o\n",psl,pc-1);
		for(i=0; i<7; i++) printf("0x%04x  ",regs[i]);
		printf("0x%04x -> 0%o\n",pc-1,instr);
		/* set up to dump on illegal instruction */
		signal(SIGILL,SIG_DFL);
		/* set pc back to bad instruction */
		pc--;
		/* go do it again for dump */
		compat();
	}
	/* go back to compatability mode */
	incompat++;
	compat();
}
