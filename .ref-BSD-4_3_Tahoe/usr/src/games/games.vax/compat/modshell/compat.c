#include "defs.h"
#define BS 8
#define	MAXARGS	300
short buf[BS];
/*
 *	Check for possible program to run under alternate runtime system
 *	If it checks out, start the rts and pass file and args
 *	Art Wetzel 3/13/80
 */
compat(file, argv, envp) char *file, *argv[], *envp[]; {
	register int fd;
	register char *rts;
	register char **nargv;
	char 	*nargs[MAXARGS];
	/* alternate rts images must be readable */
	/* if not just go back and let rest of shell worry about it */
	if((fd = open(file,0))<0)
		return(0);
	/* read first BS pdp-11 words */
	if(read(fd,buf,sizeof buf) != sizeof buf) {
		close(fd);
		/* if can't, go back as it may be a short shell file */
		return(0);
	}
	close(fd);
	/* check type of image and set up run time system name */
	if(buf[0]==0407 || buf[0]==0410 || buf[0]==0411 || buf[0]==0405) {
		/* looks like UNIX a.out file */
		/* RTS or default rts */
		if((rts = rtsnod.namval) == 0)
			rts = defrts;
		/* if header unused is set to 1 force version 6 */
		/* this is not a real difference between v6 and v7 a.outs */
		/* rather, version 6 a.outs were patched to be identifiable */
		if(buf[6] == 1)
			rts = "/usr/bin/v6run";
	} else if(buf[6] == 0) {
		/* it looks like almost all RT-11 save images have 0 here */
		rts = "/usr/bin/rtrun";
	} else {
		/* was not a recognizable file type */
		return(0);
	}
	/* must make a new argv list with runtime system prefix */
	nargv = &nargs[0];
	*nargv++ = rts;
	/* have to pass full file name to rts */
	*nargv++ = file;
	argv++;
	while(*argv && nargv < &nargs[MAXARGS-1])
		*nargv++ = *argv++;
	/* force in final null */
	*nargv++ = (char *)0;
	/* try to start rts */
	execve(nargs[0], nargs, envp);
	/* if that failed, report no runtime system */
	failed(nargs[0], norts);
}
