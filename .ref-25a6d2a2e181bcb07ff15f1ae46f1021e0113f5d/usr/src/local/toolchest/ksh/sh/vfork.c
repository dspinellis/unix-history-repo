/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)vfork.c	1.1 */
/*
 * UNIX shell
 *
 * S. R. Bourne
 * Rewritten by David Korn
 * Bell Telephone Laboratories
 *
 */

#include	"flags.h"
#include	"defs.h"
#include	"sym.h"
#include	"name.h"
#include	"io.h"
#include	"stak.h"
#include	"jobs.h"
#include	"builtins.h"
#include	"brkincr.h"
#include	"mode.h"

/*
 * This module is provided to allow the Shell to work with vfork instead
 * of fork.  With vfork the data area is shared by parent and child.
 * Save state variables at fork and make Shell variables copy on write.
 * Restore everything to previous state when fork_exit is called and
 * terminate process.
 */

/* The following structure contains the variables that must be saved */
struct f_save
{
	struct	f_save *f_save_fork;
	DOLPTR	f_savearg;
	STKPTR	f_staksave;
	struct	State	f_st;
	char	f_trapflg[MAXTRAP+1];
	char	*f_trapcom[MAXTRAP+1];
	FILE	f_save_iob[FCIO+1];
	struct	jobs	f_jobstat;
};

/* The following routines are defined by this module */
int	vfork_check();
void	vfork_restore();
int	vfork_save();

/* The following external routines are referenced by this module */
extern DOLPTR		arg_use();
extern void		arg_free();
extern NAMPTR		checkfor();
extern unsigned		chkid();
extern void		free();
extern char		*malloc();
extern struct Amemory	*namep;
extern char		*simple();
extern char		trapflg[];
extern char		*valup();

static struct f_save *save_fork;	/* most recently saved data */

/*
 * Save state on fork
 */

int	vfork_save()
{
	register struct f_save *fp = (struct f_save*)malloc(sizeof(struct f_save));
	if(fp==NULL)
		return(-1);
	locstak();
	fp->f_save_fork = save_fork;
	save_fork = fp;
	fp->f_staksave = savstak();
	fp->f_st = st;
	fp->f_jobstat = jobstat;
	jobstat.p_pwlist = 0;
	fp->f_savearg = arg_use();
	bcopy(trapflg,fp->f_trapflg,MAXTRAP+1);
	bcopy((char*)trapcom,(char*)fp->f_trapcom,(MAXTRAP+1)*sizeof(char*));
#ifdef _N_STATIC_IOBS
	bcopy((char*)_iob,(char*)fp->f_save_iob,(_N_STATIC_IOBS)*sizeof(FILE));
	bcopy((char*)_myiob,(char*)(fp->f_save_iob+_N_STATIC_IOBS),
		(FCIO+1-_N_STATIC_IOBS)*sizeof(FILE));
#else
	bcopy((char*)_iob,(char*)fp->f_save_iob,(FCIO+1)*sizeof(FILE));
#endif /* _N_STATIC_IOBS */
	states |= VFORKED;
	return(0);
}

/*
 * Restore state and exit
 */

void	vfork_restore()
{
	register struct f_save *fp = save_fork;
	if((states&VFORKED)==0)
		return;
#ifdef _N_STATIC_IOBS
	bcopy((char*)fp->f_save_iob,(char*)_iob,(_N_STATIC_IOBS)*sizeof(FILE));
	bcopy((char*)(fp->f_save_iob+_N_STATIC_IOBS),(char*)_myiob,
		(FCIO+1-_N_STATIC_IOBS)*sizeof(FILE));
#else
	bcopy((char*)fp->f_save_iob,(char*)_iob,(FCIO+1)*sizeof(FILE));
#endif /* _N_STATIC_IOBS */
	bcopy(fp->f_trapflg,trapflg,MAXTRAP+1);
	bcopy((char*)fp->f_trapcom,(char*)trapcom,(MAXTRAP+1)*sizeof(char*));
	st = fp->f_st;
	jobstat = fp->f_jobstat;
	arg_free(fp->f_savearg,0);
	save_fork = fp->f_save_fork;
	free(fp);
	tdystak(fp->f_staksave);
}


/*
 * Get the interpreter name given a script file 
 * The first line must be of the form #!  <iname>.
 * Returns 1 if <iname> is found, 0 otherwise
 */
int     get_shell(name,iname)
char *name;
char *iname;
{
	register int c;
	register int state = 0;
	register int fd;
	int n;
	char *cp;
	int	rval = 0;
	char buffer[256];
	cp = valup(SHELLNOD);
	/* don't use csh */
	if(strcmp(simple(cp),"csh")==0)
		cp = 0;
	strcpy(iname,cp?cp:"/bin/sh");
	if((fd=open(name,0))<0)
		return(-1);
	n = read(fd,buffer,sizeof(buffer));
	cp = buffer;
	while(n-- > 0)
	{
		c = *cp++;
		switch(state)
		{
			case 0:
				if(c!='#')
					goto out;
				break;

			case 1:
				if(c!='!')
					goto out;
				break;

			case 2:
				if(c==' ' || c =='\t')
					continue;
			default:
				if(c=='\n')
				{
					*iname = 0;
					rval = 1;
					goto out;
				}
				*iname++ = c;
		}
		state++;
	}
out:
	close(fd);
	return(rval);
}

/*
 * returns non-zero if process should vfork, 0 otherwise
 * we do not vfork for functions and built-ins in the background
 */
int	vfork_check(t)
TREPTR t;
{
	register COMPTR	tf;
	register ARGPTR arg;
	register char *arg0 = NIL;
	NAMPTR np;
	int bltno;
	/* simple command */
	if((t->tretyp&COMMSK)==TCOM)
		return(1);
	tf = (COMPTR)(((FORKPTR)t)->forktre);
	if((tf->comtyp&COMMSK)!=TCOM)
		return(0);
	/* background command */
	arg = tf->comarg;
	bltno = tf->comtyp>>(COMBITS+1);
	/* can't vfork assignments or most built-ins */
	if(arg==0 || bltno > SYSLOGIN)
		return(0);
	if(tf->comtyp&COMSCAN)
	{
		if(arg->argflag&A_RAW)
			arg0 = arg->argval;
	}
	else
		arg0 = *(((DOLPTR)arg)->dolarg+1);
	/* no vfork if not sure */
	if(arg0==NIL)
		return(0);
	/* eliminate functions */
	if(chkid(arg0) && (np=checkfor(arg0,prnames))&&  np->value.namval.ip)
		return(0);
	/* command substitution with i/o redirection use fork */
	if((t->tretyp&FCOMSUB) && t->treio==(IOPTR)0)
		return(0);
	return(2);
}

#ifndef BSD_4_2
/*
 * copy <n> bytes from <sp> to <dp>
 */ 

int bcopy(sp,dp,n)
register char *sp, *dp;
register int n;
{
	while(n-- >0)
		*dp++ = *sp++;
}
#endif BSD_4_2
