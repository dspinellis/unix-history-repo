/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/*
 * UNIX ksh
 *
 * D. G. Korn
 * Bell Telephone Laboratories
 * adapted from APOLLO changes to Bourne Shell
 *
 */

#include	<invoke.h>
#include	<errno.h>
#include	<sys/types.h>
#include	<sys/stat.h>
#include        "defs.h"
#include        "brkincr.h"
#include	"stak.h"


#define PATHLEN	256


extern int errno;
extern void	failed();
extern void	fault();
extern char	*fullname();
extern void	p_prp();
extern void	p_setout();
extern char	**setenv();
extern void	sync_io();
extern MSG	*sysmsg[];


static int conlist[] = {0,1,2,3,-1};

int exec_here(com)
register char **com;
{
	register char *prog = com[1];
	char **arge;
	register char *path;
	char iname[PATHLEN];
	int (*oldsig)();
	int sig;
	int xitval;
	path = prog;
	/* see if program name contains a / */
	if(strchr(prog,'/')==0)
	{
		if((path = fullname(prog))==NULL)
			failed(prog,notfound);
		endstak(path+strlen(path));
	}
	arge = setenv();
	oldsig = signal(SIGQUIT,SIG_DFL);
	sync_io();
	errno = 0;
	xitval = invokeve(path,INV_WAIT,conlist,com+1,arge);
	if(errno==ENOEXEC)
	{
		char *savcom = com[0];
		if(get_shell(path,iname)<0)
			failed(badexec);
		com[0] = iname;
		xitval = invokeve(iname,INV_WAIT,conlist,com,arge);
		com[0] = savcom;
	}
	signal(SIGQUIT,oldsig);
	if(xitval>=0)
	{
		if(sig=(xitval&0177))
		{
			if(sig==2)
				fault(sig);
			if(*sysmsg[sig])
			{
				if(output!=stderr)
					p_setout(stderr);
				if((states&PROMPT)==0)
					p_prp(itos(getpid()),SP);
				fputs(sysmsg[sig],output);
				newline();
				p_setout(output);
				xitval = sig|SIGFLG;
			}
		}
		else
			xitval >>= 8;
	}
	else if(xitval == -1)
		failed(prog,badexec);
	return(xitval);
}

