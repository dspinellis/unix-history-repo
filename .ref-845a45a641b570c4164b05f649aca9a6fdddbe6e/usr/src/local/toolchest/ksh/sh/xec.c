/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)xec.c	1.1 */
/*
 * UNIX shell
 *
 * S. R. Bourne
 * Rewritten by David Korn
 * AT&T Bell Laboratories
 *
 */

#include	<sys/types.h>
#include	<sys/times.h>
#include	<errno.h>
#ifdef SXT
#include	<sys/tty.h>
#include	<sys/sxt.h>
#endif	/* SXT */
#include	"defs.h"
#include	"sym.h"
#include	"mode.h"
#include	"flags.h"
#include	"name.h"
#include	"brkincr.h"
#include	"stak.h"
#include	"builtins.h"
#include	"shtype.h"
#include	"jobs.h"
#include	"io.h"
#ifdef BSD
#define TIC_SEC		60
#include	<sys/timeb.h>
#endif	/* BSD */

/* These routines are defined by this module */
int execute();
void trace_command();

/* These routines are referenced by this module */
extern char	**arg_build();
extern DOLPTR	arg_use();
extern DOLPTR	arg_free();
extern void	assign();
extern void	await();
extern FILE	*chkopen();
extern void	builtin();
extern void	chktrap();
extern void	chkpipe();
extern void	done();
extern void	execa();
extern void	exfunct();
extern void	exitsh();
extern void	failed();
extern void	free();
extern NAMPTR	findnod();
extern char	*getpath();
extern char	*heap();
extern char	*itos();
extern NAMPTR	lookup();
extern char	*macro();
extern char	*mactrim();
extern void	mem_scope();
extern void	mem_unscope();
extern char	*movstr();
extern char	*malloc();
extern void	oldsigs();
extern void	p_sub();
extern void	p_num();
extern void	p_str();
extern void	p_time();
extern void	p_flush();
extern void	p_setout();
extern void	p_list();
extern void	pipe_close();
extern void	postclr();
extern char	*qvalup();
extern void	restore();
extern void	rmtemp();
extern char	*strcpy();
extern void	setlist();
extern void	swap_iodoc_nm();
extern void	sync_io();
extern void	tdystak();
extern long	times();
#ifdef DEVFD
extern void	close_pipes();
#endif	/* DEVFD */
#ifdef VFORK
extern int	vfork_check();
extern int	vfork_save();
extern void	vfork_restore();
#endif	/* VFORK */

static int trim_eq();
static char locbuf[TMPSIZ]; 	/* store last argument here if it fits */


/* ========	command execution	========*/


/* VARARGS */
execute(argt, execflg, apf1, apf2)
TREPTR 	argt;
unsigned execflg;
FILE	*apf1[],*apf2[];
{
	/* `stakbot' is preserved by this routine */
	register TREPTR t;
	STKPTR 	sav=savstak();
	FILE *inpipe[2],*otpipe[2];
	FILE **pf1 = apf1;
	FILE **pf2 = apf2;
	unsigned errorflg = (execflg&ERRFLG);
#ifdef VFORK
	int v_fork;
#endif	/* VFORK */
	if(trapnote&SIGSET)
		exitsh(SIGFAIL);
	if(errorflg==0)
		states &= ~ERRFLG;
	if((t=argt) && execbrk==0)
	{
		register int 	type;
		char	**com;
		int	argn;
		char *com0 = NIL;
		type = t->tretyp;
		oldexit=exitval;
		exitval=0;
		switch(type&COMMSK)
		{
			case TCOM:
			{
				register IOPTR		io;
				register ARGPTR		argp;
				NAMPTR np;
				type >>= (COMBITS+1);
				cmdline = ((COMPTR)t)->comline;
				com = arg_build(&argn,((COMPTR)t));
				if(t->tretyp&COMSCAN)
				{
					argp = ((COMPTR)t)->comarg;
					if(argp && (argp->argflag&A_RAW)==0)
						type = syslook(com[0],commands);
					if(trapnote&SIGSET)
						exitsh(SIGFAIL);
				}
				com0 = com[0];
				argp = ((COMPTR)t)->comset;
				io = t->treio;
				if(argn==0 || (type>0 && type<=SYSSPECIAL))
				{
					setlist(argp, 0);
					argp = NULL;
				}
				if((io||argn) && is_option(NOEXEC)==0)
					/* print command if EXECPR */
				{
					if(argn==0)
					{
						/* fake a built-in */
						argn=1;
						type = SYSMAX;
					}
					/* set +x doesn't echo */
					else if(type!=SYSSET)
					{
						trace_command(com);
					}
					if(io)
						p_flush();
#ifdef apollo
					if(is_option(INPROC) &&  type==0 
						&& (states&FORKED)==0 &&
						!findnod(com0,prnames,CHK_FOR))
					{
						type = SYSINPROCESS;
						com--;
						argn++;
					}
#endif	/* apollo */
					/* check for builtins */
					if(type)
					{
						/* failures on built-ins not fatal */
						jmp_buf retbuf;
						jmp_buf *savreturn = freturn;
						int indx = topfd;
						int scope=0;
						if(type>=SYSNULL)
							freturn = (jmp_buf*)retbuf;
						if(setjmp(retbuf) == 0)
						{
							int flag = 1;
							if(type>=SYSNULL)
								states |= BUILTIN;
							else
							{
								flag = (type!=SYSLOGIN);
								if(type==SYSEXEC)
									flag = -(com[1]==0);
							}
							indx = initio(io,flag);
							if(argp)
							{
								mem_scope(argp);
								scope++;
							}
							builtin(type,argn,com,t);
						}
						states &= ~BUILTIN;
						freturn = savreturn;
						if(scope)
							mem_unscope();
						restore(indx);
						goto setexit;
					}
					/* check for functions */
					if((np=findnod(com0,prnames,CHK_FOR))
							 && np->value.namval.ip)
					{
						int indx;
						indx = initio(io,1);
						exfunct((TREPTR)(*funtree(np))
							,com
							,execflg|(attest(np,T_FLAG)?EXECPR:0)
							,((COMPTR)t)->comset);
						restore(indx);
						goto setexit;
					}
					/* track alias if possible */
					getpath(com0);
				}
				else if(io==0)
				{
				setexit:
					exitset();
					chktrap();
					break;
				}
				type = TCOM;
			}
			case TFORK:
			{
				int no_fork;
				sync_io();
#ifdef SXT
				/* find job number and create synchronization pipe */
				if((jobstat.cur_job = next_job()) < jobstat.maxjob)
					if(pipe(jobstat.pipe)<0)
						jobstat.maxjob = 0;
#endif	/* SXT */
				no_fork = (execflg&1) && (type&(FAMP|FPOU))==0;
				if(no_fork)
					parent=0;
				else
				/* FORKLIM is the max period between forks -
					power of 2 usually.  Currently shell tries after
					2,4,8,16, and 32 seconds and then quits */
				{
					register int forkcnt=1;
					if(type&FTMP)
					{
						link_iodocs(iotemp);
						linked = 1;
					}
					if((type&(FPCL|FPIN|FPOU)) == (FPIN|FPOU))
					/* set up pipe for cooperating process */
					{
						if(cpid)
							failed(cmdadr,pexists);
						chkpipe(pf2=otpipe);
						cpipe[INPIPE] = frenumber(pf2[INPIPE],CINPIPE);
						chkpipe(pf1=inpipe);
						cpipe[OTPIPE] = frenumber(pf1[OTPIPE],COTPIPE);
					}
#ifdef VFORK
					if(v_fork=vfork_check(t))
						vfork_save();
					while((parent=(v_fork?vfork():fork())) == -1)
#else
					while((parent=fork()) == -1)
#endif	/* VFORK */
					{
						if((forkcnt *= 2) > FORKLIM)
						{
							switch(errno)
							{
								case ENOMEM:
									error(noswap);
									 break;
								default:
								case EAGAIN:
									error(nofork);
									break;
							}
						}
						if(trapnote&SIGSET)
							exitsh(SIGFAIL);
						alarm(forkcnt);
						pause(); 
						alarm(0);
					}
				}
	
				if(parent)
					/* This is the parent branch of fork;    */
				/* it may or may not wait for the child. */
				{
					register int pno;
#ifdef VFORK
					if(v_fork)
						vfork_restore();
#endif	/* VFORK */
#ifdef JOBS
# ifdef SXT
					if(jobstat.pipe[1] > 0)
						close(jobstat.pipe[1]);
# endif /* SXT */
					/* assign each pipeline its own process group */
					if(jobstat.j_flag==0||(states&MONITOR)==0)
						jobstat.cur_pgrp = parent;
#endif	/* JOBS */
					if(type&FPCL)
						fclose(pf1[INPIPE]);
					else if((type&FPIN)&&(type&FPOU))
					{
						cpid = parent;
						fclose(pf1[INPIPE]);
						fclose(pf2[OTPIPE]);
					}
					if((type&(FAMP|FPOU))==0)
					{
#ifdef DEVFD
						close_pipes();
#endif	/* DEVFD */
						await(parent,0);
					}
					else if(type&(FPOU|FPIN))
					{
						int flag;
						if(cpid==parent)
							flag = 0;
#ifdef JOBS
						else
							flag = P_PIPEJOB;
						pno = post(parent,flag);
#else
						pno = post(parent);
#endif	/* JOBS */
					}
					else
					{
#ifdef JOBS
						pno = post(parent,0);
#else
						pno = post(parent);
#endif
						movstr(itos(parent),pcsadr);
					}
					if((type&FPRS) && (states&TTYFLG))
					{
						p_setout(stderr);
#ifdef JOBS
						/* print job number */
						p_sub(pno,'\t');
#endif	/* JOBS */
						p_num(parent,NL);
					}
					chktrap();
					break;
				}
	
				else
				/*
				 * this is the FORKED branch (child) of execute
				 */
				{
					if(standout != stdout)
						standout = frenumber(standout,1);
					states |= FORKED;
					off_option(HASHALL);
					if(no_fork==0)
					{
						states &= ~(RM_TMP|IS_TMP);
						states |= NO_TMP;
						if(linked == 1)
						{
							swap_iodoc_nm(iotemp);
							linked = 2;
						}
						else
							iotemp=0;
					}
#ifdef ACCT
					suspacct();
#endif	/* ACCT */
					/* child should not unlink the tmpfile */
					cpipe[INPIPE] = cpipe[OTPIPE] = NULL;
					cpid = 0;
					/* Turn off INTR and QUIT if `FINT'  */
					/* Reset remaining signals to parent */
					/* except for those `lost' by trap   */
					oldsigs();
#ifdef JOBS
					if(states&MONITOR)
					{
# ifdef BSD
						register int pid = getpid();
						int pgrp;
						if(jobstat.j_flag==0)
							pgrp = pid;
						else
							pgrp = jobstat.cur_pgrp;
						setpgrp(pid,pgrp);
						if(states&NONSTOP)
							signal(SIGTSTP,SIG_IGN);
						else
						{
							signal(SIGTTIN,SIG_DFL);
							signal(SIGTTOU,SIG_DFL);
							signal(SIGTSTP,SIG_DFL);
						}
#else
# ifdef SXT
						if(jobstat.cur_job < jobstat.maxjob
							|| (type&FAMP))
# else
#  ifdef DEVFD
						if((type&(FINT|FAMP))==(FINT|FAMP))
#  else
						if(type&FAMP)
#  endif /* DEVFD */
# endif /* SXT */
							setpgrp();
#endif	/* BSD */
					}
					else
#endif	/* JOBS */
					if(type&FINT)
					{
						signal(SIGINT,SIG_IGN);
						signal(SIGQUIT,SIG_IGN);
					}
					/* pipe in or out */
					if((type&FAMP) && is_option(BGNICE))
						nice(4);
#if VSH || ESH
					if(type&(FAMP|FPOU))
						off_option(EMACS|EDITVI|GMACS);
#endif
					if(type&FPIN)
					{
						frenumber(pf1[INPIPE],0);
						if((type&FPOU)==0)
							fclose(pf1[OTPIPE]);
						setbuf(stdin,NIL);
					}
					if(type&FPOU)
					{
						frenumber(pf2[OTPIPE],1);
						pipe_close(pf2);
					}
					/* default std input for & */
#ifdef JOBS
# ifdef BSD
					if((states&MONITOR) == 0)
# endif /* BSD */
# ifdef SXT
					if((states&MONITOR)==0 ||
						jobstat.cur_job >= jobstat.maxjob)
				 	{
# endif /* SXT */
#endif	/* JOBS */
						if((type&FINT) && ioset==0)
						{
							fclose(stdin);
							chkopen(devnull);
						}
						/* io redirection */
#ifdef JOBS
# ifdef SXT
					}
					else
						j_new_chan();
# endif /* SXT */
					states &= ~MONITOR;
#endif	/* JOBS */
					initio(t->treio,0);
					if(type!=TCOM)
					{
						/* don't clear job table for out
						   pipes so that jobs can be
						   piped
						 */
						if(no_fork==0 && (type&FPOU)==0)
							postclr();
						execute(((FORKPTR) t)->forktre,execflg|1);
					}
					else if(com0!=ENDARGS)
					{
						off_option(ERRFLG);
						rmtemp((IOPTR)0);
						execa(com,((COMPTR)t)->comset);
					}
					done(0);
				}
			}

			case TSETIO:
			{
			/*
			 * don't create a new process, just
			 * save and restore io-streams
			 */
				int indx = initio(((FORKPTR)t)->forkio,1);
				execute(((FORKPTR)t)->forktre,execflg);
				restore(indx);
				break;
			}

			case TPAR:
				execute(((PARPTR) t)->partre,execflg);
				done(0);
	
			case TFIL:
			{
			/*
			 * This code sets up a pipe.
			 * All elements of the pipe are started by the parent.
			 * Only the last one is waited for.
			 */
				register FORKPTR tf;
				FILE *pvo[2];	/* old pipe for multi-pipeline */
				FILE *pvn[2];	/* set up pipe */
				register int rc = 1;
				do
				{
					/* create the pipe */
					chkpipe(pvn);
					tf = (FORKPTR)(((LSTPTR)t)->lstlef);
					/* rc==0 on multi-stage pipe */
					if(rc==0)
						tf->forktyp |= FPCL|FPIN;
					/* execute out part of pipe no wait */
					rc = execute((TREPTR)tf, errorflg, pvo, pvn);
					tf = (FORKPTR)(((LSTPTR)t)->lstrit);
					t = tf->forktre;
					/* save the pipe stream-ids */
					pvo[0] = pvn[0];
					/*close out-part of pipe as soon as possible */
					fclose(pvn[OTPIPE]);
#ifdef JOBS
					/* pipeline all in one process group */
					jobstat.j_flag++;
#endif	/* JOBS */
				}
				/* repeat until end of pipeline */
				while(rc==0 && tf->forkio==NULL && t->tretyp==TFIL);
				if(rc == 0)
					execute((TREPTR)tf, execflg,pvn,pf2);
				else
					/* execution failure, close pipe */
					pipe_close(pvn);
				break;
			}
	
			case TLST:
			{
				/*  a list of commands are executed here */
				do
				{
					execute(((LSTPTR) t)->lstlef,errorflg);
					t = (TREPTR)(((LSTPTR)t)->lstrit);
				}
				while(t->tretyp == TLST);
				execute(t,execflg);
				break;
			}
	
			case TAND:
#ifdef JOBS
				states |= NONSTOP;
#endif	/* JOBS */
				if(execute(((LSTPTR) t)->lstlef,0)==0)
					execute(((LSTPTR) t)->lstrit,execflg);
				break;
	
			case TORF:
#ifdef JOBS
				states |= NONSTOP;
#endif	/* JOBS */
				if(execute(((LSTPTR) t)->lstlef,0)!=0)
					execute(((LSTPTR) t)->lstrit,execflg);
				break;
	
			case TFOR:
			case TSELECT:
			{
				register char **args;
				register int nargs;
				NAMPTR n = lookup(((FORPTR) t)->fornam);
				char **arglist;
				DOLPTR	argsav=NULL;
				COMPTR	tp;
				char *nullptr = NULL;
#ifdef JOBS
				states |= NONSTOP;
#endif	/* JOBS */
				if((tp=((FORPTR)t)->forlst)==NULL)
				{
					args=dolv+1;
					nargs = dolc;
					argsav=arg_use();
				}
				else
				{
					args=arg_build(&argn,tp);
					nargs = argn;
				}
				if(type==TSELECT)
				{
					p_setout(stderr);
					p_list(nargs,arglist=args);
				}
				loopcnt++;
				while(*args !=ENDARGS && execbrk == 0)
				{
					if(t->tretyp==TSELECT)
					{
						char *val,*cp;
						/* reuse register */
#define c	type
						chkpr(1);
						if(isatty(0))
							states |= PROMPT;
						readvar(&nullptr,stdin,R_FLAG);
						if(feof(stdin))
						{
							exitval = 1;
							clearerr(stdin);
							break;
						}
						if((val=qvalup(REPLYNOD))==NULL)
							continue;
						else
						{
							if(*(cp=val) == 0)
							{
								p_list(nargs,args);
								continue;
							}
							while(c = *cp++)
							if(c < '0' && c > '9')
								break;
							if(c!=0)
								c = nargs;
							else
								c = atoi(val)-1;
							if(c<0 || c >= nargs)
								c = nargs;
							args += c;
						}
					}
#undef c
					assign(n, *args);
					if(t->tretyp != TSELECT)
						args++;
					else
						args = arglist;
					execute(((FORPTR) t)->fortre,errorflg);
					if(breakcnt<0)
						execbrk = (++breakcnt !=0);
					}
				if(breakcnt>0)
					execbrk = (--breakcnt !=0);
				loopcnt--;
				arg_free(argsav,0);
				break;
			}
	
			case TWH:
			case TUN:
			{
				register int 	i=0;
#ifdef JOBS
				states |= NONSTOP;
#endif	/* JOBS */
				loopcnt++;
				while(execbrk==0 && (execute(((WHPTR)t)->whtre,0)==0)==(type==TWH))
				{
					i = execute(((WHPTR) t)->dotre,errorflg);
					if(breakcnt<0)
						execbrk = (++breakcnt !=0);
				}
				if(breakcnt>0)
					execbrk = (--breakcnt !=0);
				loopcnt--;
				exitval= i;
				break;
			}
	
			case TIF:
#ifdef JOBS
				states |= NONSTOP;
#endif	/* JOBS */
				if(execute(((IFPTR) t)->iftre,0)==0)
					execute(((IFPTR) t)->thtre,execflg);
				else if(((IFPTR) t)->eltre)
					execute(((IFPTR) t)->eltre, execflg);
				else
					exitval=0; /* force zero exit for if-then-fi */
				break;
	
			case TSW:
			{
				char *r = mactrim(((SWPTR) t)->swarg,0);
				t=(TREPTR)((SWPTR) t)->swlst;
				while(t)
				{
					register ARGPTR	rex=(ARGPTR)((REGPTR)t)->regptr;
					while(rex)
					{
						register char *s;
						if(rex->argflag&A_MAC)
							s = macro(rex->argval);
						else
							s = rex->argval;
						if(((rex->argflag&A_RAW)==0&&gmatch(r,s))
							|| trim_eq(r,s))
						{
							execute(((REGPTR)t)->regcom,errorflg);
							t=0;
							break;
						}
						else
							rex=rex->argnxt;
					}
					if(t)
						t=(TREPTR)((REGPTR)t)->regnxt;
				}
				break;
			}

			case TTIME:
			{
				/* time the command */
				struct tms before,after;
				long at,bt;
#ifdef BSD
				struct timeb tb,ta;
				ftime(&tb);
#endif	/* BSD */
				bt = times(&before);
				execute(((PARPTR) t)->partre,0);
				at = times(&after);
#ifdef BSD
				ftime(&ta);
				at = TIC_SEC*(ta.time-tb.time);
				at +=  ((TIC_SEC*((long)(ta.millitm-tb.millitm)))/1000);
#else
				at -= bt;
#endif	/* BSD */
				p_setout(stderr);
				p_str(t_real,'\t');
				p_time(at,NL);
				p_str(t_user,'\t');
				at = after.tms_utime - before.tms_utime;
				at += after.tms_cutime - before.tms_cutime;
				p_time(at,NL);
				p_str(t_sys,'\t');
				at = after.tms_stime - before.tms_stime;
				at += after.tms_cstime - before.tms_cstime;
				p_time(at,NL);
				break;
			}
			case TPROC:
			{
				register NAMPTR np;
				register char *fname = ((PROCPTR)t)->procnam;
				if(!isalpha(*fname))
					failed(fname,notid);
				np = findnod(fname,prnames,1);
				if(np->value.namval.rp)
					free(np->value.namval.rp->ptree);
				else
					np->value.namval.rp = (struct Ufunction*)
						malloc(sizeof(struct Ufunction));
				funtree(np) = (int**)((PROCPTR)t)->proctre;
				np->value.namval.rp->hoffset = ((PROCPTR)t)->procloc;
				sattrib(np,L_FLAG|INT_GER);
				break;
			}
		}
#ifdef JOBS
		jobstat.j_flag = 0;
#endif	/* JOBS */
		/* set $. */
		if( com0)
		{
			if(lastarg!= locbuf)
				free(lastarg);
			if(strlen(com[argn-1]) < TMPSIZ)
				lastarg = strcpy(locbuf,com[argn-1]);
			else
				lastarg = heap(com[argn-1]);
		}
		exitset();
	}
	if(trapnote&SIGSET)
		exitsh(SIGFAIL|exitval);
	tdystak(sav);
	states |= errorflg;
	linked = 0;
	return(exitval);
}

/*
 * test for equality with second argument trimmed
 * returns 1 if r == trim(s) otherwise 0
 */

static trim_eq(r,s)
register char *r,*s;
{
	register char c;
	while(c = *s++)
	{
		if(c==ESCAPE)
			c = *s++;
		if(c != *r++)
			return(0);
	}
	return(*r==0);
}

/*
 * print out the command line if set -x is on
 */

void trace_command(com)
char **com;
{
	if(is_option(EXECPR))
	{
		p_setout(stderr);
		fputs(execpmsg,output);
		echo_list(1,com,output);
		newline();
	}
}

