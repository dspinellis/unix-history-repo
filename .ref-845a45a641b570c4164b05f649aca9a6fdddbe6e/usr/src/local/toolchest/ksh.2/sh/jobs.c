#define USE_OLD_TTY
/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)jobs.c	1.1 */

/*
 *  Job control for UNIX Shell
 *
 *   David Korn
 *   AT&T Bell Laboratories
 *   Room 5D-112
 *   Murray Hill, N. J. 07974
 *   Tel. x7975
 *
 *  Written October, 1982
 */


#include	<errno.h>

#ifdef BSD
# ifdef BSD_4_2
#include	<sys/wait.h>
# else
#include	<wait.h>
# endif /* BSD_4_2 */
#endif	/* BSD */

#ifdef SXT
#define		SIGSTOP	20
#define		SIGSTIN	21
#include	<sys/types.h>
#include	<sys/tty.h>
#include	<sys/sxt.h>
#include	<sys/stat.h>
#endif	/* SXT */

#include	"defs.h"
#include	"flags.h"
#include	"shtype.h"
#include	"jobs.h"
#include	"brkincr.h"
#include	"io.h"
#include	"history.h"

#ifdef SXT
#undef input
static status_update();
static int open_chan();
static int fd_chan = -1;
static char tinybuf;
#endif	/* SXT */

#define LOBYTE	0377
#define MAXMSG	25

void	await();
void	postclr();

extern void	fault();
extern long	hist_list();
extern long	hist_position();
extern char	*itos();
extern char	*malloc();
extern void	failed();
extern void	free();
extern void	p_str();
extern void	p_prp();
extern void	p_num();
extern void	p_sub();
extern void	p_nchr();
extern void	p_flush();
extern void	p_setout();

static int	set_ttygrp();
static int	set_job();
static int	reset_ttygrp();
static struct process *next_proc();
static struct process *jobbyname();
static struct process *job_bynum();
static struct process *job_bypid();
static struct process *job_post();
static struct process *unpost();
static struct process *pgrp_jobs();
static int get_job();
static int match_jobname();
static int unstop();
static void free_job();
static void pw_unlink();
static struct process *freelist;
#define	numrun	jobstat.p_numrun	/* number of running jobs */
#define	numpost	jobstat.p_numpost	/* number of posted jobs */
#define	pwlist	jobstat.p_pwlist
#define freejobs jobstat.p_freejobs	/* free job numbers */
static long pr_jobname();

#ifdef FIOLOOKLD
#define version8	1
extern int tty_ld, ntty_ld;
#define OTTYDISC	tty_ld
#define NTTYDISC	ntty_ld
#endif	/* FIOLOOKLD */


static int savepgrp;
static int savetgrp;
static BOOL old_driver;
static BOOL beenhere;
static int saveldisc;

#ifdef POSIX
static struct termios my_stty;		/* terminal state for shell */
#else
# ifdef BSD
static struct sgttyb my_stty;		/* terminal state for shell */
# else
static struct termio my_stty;		/* terminal state for shell */
# endif	/* BSD */
#endif /* !POSIX */

#ifdef BSD
/*
 * initialize job control
 * if lflag is set the switching driver message will not print
 */

int init_jobs(lflag)
{
	int ldisc;
	register int ntry = 0;
	register int pid;
	jobstat.mypid = pid = getpid();
	savepgrp = getpgrp(pid);
# ifdef version8
	if((ldisc = ioctl (2, FIOLOOKLD, 0)) <0)
# else
	if(ioctl(2,TIOCGETD,&ldisc) !=0)
# endif /* version8 */
		return(-1);
	if(ldisc!=NTTYDISC 
#ifdef notdef /*POSIX*/
	   && ldisc!=POSXDISC
#endif
	   && ldisc!=OTTYDISC)
	{
		/* no job control when running with MPX */
# ifdef VSH
		on_option(VIRAW);
# endif /* VSH */
		return(-1);
	}
retry:
	signal(SIGTTOU,SIG_IGN);
	if(ioctl(2,TIOCGPGRP,&savetgrp) !=0)
	{
		fputs(j_not_tty,output);
		return(-1);
	}
	/* This shell must be in foreground to change state */
	if(savetgrp != savepgrp)
	{
		signal(SIGTTOU,SIG_DFL);
		kill(pid,SIGTTOU);
		/* wait for continue and try again */
		if(ntry++ > 100)
		{
			fputs(j_no_start,output);
			return(-1);
		}
		goto retry;
	}
#ifdef notdef /*POSIX*/
	if(ldisc != POSXDISC)
	{
		saveldisc = ldisc;
		ldisc = POSXDISC;
#else
	if(ldisc == OTTYDISC)
	{
		ldisc = NTTYDISC;
#endif
# ifdef version8
		ioctl(2,TIOCGETP,&my_stty);
		if (ioctl(2, FIOPOPLD, 0) < 0)
			return(-1);
		if (ioctl(2, FIOPUSHLD, &ldisc, 0) < 0)
		{
			ldisc = OTTYDISC;
			 ioctl(2, FIOPUSHLD, &ldisc, 0);
			return(-1);
		}
		ioctl(2,TIOCSETN,&my_stty);
# else
		if(ioctl(2,TIOCSETD,&ldisc) !=0)
			return(-1);
# endif /* version8 */
		if(lflag==0)
		{
			fputs(j_newtty,output);
			old_driver++;
		}
	}
	on_option(MONITOR);
	signal(SIGCHLD,fault);
	signal(SIGTSTP,SIG_IGN);
	signal(SIGTTIN,SIG_IGN);
	setpgrp(pid,pid);
	set_ttygrp(pid);
	return(0);
}


/*
 * see if there are any stopped jobs
 * restore tty driver and pgrp
 */

int close_jobs()
{
	register struct process *pw = pwlist;
	register int count = 0;
	int ldisc;
	if((states&MONITOR)==0)
		return(0);
	for(;pw;pw=pw->p_next)
	{
		if((pw->p_flag&P_STOPPED)==0)
			continue;
		if(beenhere)
			killpg(pw->p_pgrp,SIGTERM);
		count++;
	}
	if(beenhere++ == 0 && pwlist)
	{
		if(count)
		{
			fputs(j_terminate,output);
			return(-1);
		}
		else if(login_sh)
		{
			fputs(j_running,output);
			return(-1);
		}
	}
	set_ttygrp(savepgrp);
	if(old_driver)
	{
		/* restore old line discipline */
#ifdef POSIX
		ldisc = saveldisc;
#else
		ldisc = OTTYDISC;
#endif
# ifdef version8
		ioctl(2,TIOCGETP,&my_stty);
		if (ioctl(2, FIOPOPLD, 0) < 0)
			return(0);
		if (ioctl(2, FIOPUSHLD, &ldisc, 0) < 0)
		{
			ldisc = NTTYDISC;
			ioctl(2, FIOPUSHLD, &ldisc, 0);
			return(0);
		}
		ioctl(2,TIOCSETN,&my_stty);
# else
		if(ioctl(2,TIOCSETD,&ldisc) !=0)
			return(0);
# endif /* version8 */
		fputs(j_oldtty,output);
	}
	return(0);
}

/*
 * Set the ttygroup id to pid
 */

static int set_ttygrp(pid)
{
	if(ioctl(2,TIOCSPGRP,&pid) !=0)
		return(-1);
	return(0);
}

/*
 * Set the ttygroup id to a previously running job
 */
#else	/* !BSD */
# ifdef SXT
static char sxt[] = "/dev/sxt/000";
static struct sxtblock status1,status2;

int init_jobs(lflag)
{
	register int pid;
	register int dev;
	struct stat statbuf;
	jobstat.mypid = pid = getpid();
	savepgrp = getpgrp(pid);
	if(ioctl(2,SXTIOCSTAT,&status2) < 0)
		return(-1);
	if(ioctl(2,TCGETA,&my_stty) < 0)
		return(-1);
	if(fstat(2,&statbuf) < 0)
		return(-1);
	dev = statbuf.st_rdev&0xff;
	/* must be channel 0 */
	if(dev&07)
		return(-1);
	dev >>= 3;
	sxt[10] = '0' + (dev%10);
	sxt[9] = '0' + dev/10;
	my_stty.c_cc[VSWTCH] = CSWTCH;
	if(ioctl(2,TCSETAF,&my_stty) < 0)
		return(-1);
	setpgrp(pid,pid);
	jobstat.maxjob = MAXPCHAN;
	on_option(MONITOR);
	return(0);
}
#endif	/* SXT */

int close_jobs()
{
	register struct process *pw = pwlist;
	register int count = 0;
	if((states&MONITOR)==0)
		return(0);
# ifdef SXT
	for(;pw;pw=pw->p_next)
	{
		if((pw->p_flag&P_STOPPED)==0)
			continue;
		if(beenhere)
			kill(-pw->p_pgrp,SIGTERM);
		count++;
	}
# endif /* SXT */
	if(beenhere++ == 0 && pwlist && login_sh)
# ifdef SXT
	{
		if(count)
		{
			fputs(j_terminate,output);
			return(-1);
		}
		if(login_sh || jobstat.maxjob)
# endif /* SXT */
		{
			fputs(j_running,output);
			return(-1);
		}
# ifdef SXT
	}
	if(jobstat.maxjob)
		kill_all();
# endif /* SXT */
	return(0);
}

#endif	/* !BSD */

static int set_job(pw)
register struct process *pw;
{
	/* save current terminal state */
#if defined(BSD) && !defined(POSIX)
	ioctl(2,TIOCGETP,&my_stty);
#else
	ioctl(2,TCGETA,&my_stty);
#endif	/* BSD */
#ifdef SXT
	/* make sure child has opened channel */
	if(jobstat.pipe[0] > 0)
	{
		read(jobstat.pipe[0],&tinybuf,1);
		close(jobstat.pipe[0]);
		jobstat.pipe[0] = 0;
	}
	if(pw->p_job < jobstat.maxjob)
		if(ioctl(2,SXTIOCSWTCH,pw->p_job)!=0)
		{
			return(-1);
		}
#endif	/* SXT */
	if(pw->p_flag&P_STTY)
	{
		/* restore terminal state for job */
#if defined(BSD) && !defined(POSIX)
		ioctl(2,TIOCSETN,&pw->p_stty);
#else
		ioctl(2,TCSETAF,&pw->p_stty);
#endif	/* BSD */
	}
#ifdef BSD
	if(ioctl(2,TIOCSPGRP,&pw->p_pgrp) !=0)
		return(-1);
#endif	/* BSD */
	return(0);
}

static int	reset_ttygrp(pw,flag)
register struct process *pw;
{
	/* save the terminal state for current job */
#ifdef BSD
	if(ioctl(2,TIOCSPGRP,&jobstat.mypid) !=0)
	{
		return(-1);
	}
#endif	/* BSD */
#ifdef SXT
	if(pw && pw->p_job < jobstat.maxjob)
	{
		/* save the state of terminated process */
		if(flag==0 && fd_chan>=0)
		{
			if(ioctl(fd_chan,TCGETA,&pw->p_stty)==0)
			{
				my_stty = pw->p_stty;
				/* for timing problem */
				ioctl(fd_chan,TCSETAW,&my_stty);
			}
		}
		close(fd_chan);
		fd_chan = -1;
		/* grab control in channel 0 */
		ioctl(2,TCSETAW,&my_stty);
		if(ioctl(2,SXTIOCSWTCH,0)!=0)
		{
			return(-1);
		}
		if(flag==0)
			ioctl(2,TCSETAW,&my_stty);
		beenhere = 0;
		return(0);
	}
#endif	/* SXT */
	if(pw && flag)
	{
#if defined(BSD) && !defined(POSIX)
		if(ioctl(2,TIOCGETP,&pw->p_stty) == 0)
#else
		if(ioctl(2,TCGETA,&pw->p_stty) == 0)
#endif	/* BSD */
			pw->p_flag |= P_STTY;
		/* restore terminal state for job */
#if defined(BSD) && !defined(POSIX)
		ioctl(2,TIOCSETN,&my_stty);
#else
		ioctl(2,TCSETAF,&my_stty);
#endif	/* BSD */
	}
	beenhere = 0;
	return(0);
}

#if BSD || SXT

/*
 * move job to foreground if bgflag == 0
 * move job to background if bgflag != 0
 */

switch_jobs(ajob,bgflag)
register char *ajob;
{
	register struct process *pw=NULL;
	register int pgrp;
# ifdef SXT
	if(jobstat.maxjob==0)
		return(NULL);
# endif /* SXT */
	if(*ajob)
		pw = pgrp_jobs(ajob);
	else if(pwlist)
		pw = job_bynum((int)pwlist->p_job);
	if(pw)
	{
		if(bgflag)
		{
			pgrp = pw->p_pgrp;
			if(pw->p_flag&P_STOPPED)
			{
				p_sub((int)pw->p_job,'\t');
				pr_jobname(pw,'&',0L);
				fputs(j_amp,output);
				unstop(pgrp);
			}
		}
		else
		{
			pw_unlink(pw);
			pw->p_next = pwlist;
			pwlist = pw;
			pr_jobname(pw,'&',0L);
			newline();
			await(pw->p_pid,3);
		}
	}
	return(pw!=NULL);
}

/*
 * list the current jobs
 * flag L_FLAG for long listing
 * flag N_FLAG for list only jobs marked for notification
 */
#endif	/* BSD */

list_jobs(flag)
int flag;
{
	register struct process *pw;
	register struct process *px;
	register int m;
	register struct process *py;
	int  n;
	long offset;
	int jobm = 0;
	register char *msg;
	int msize;
	if(pwlist==NULL)
		return;
	await(0,2);
	p_setout(standout);
	/* get job number corresponding to '-' */
	if(pw=pgrp_jobs("%-"))
		jobm = pw->p_job;
	for(pw=pwlist;pw;pw=py)
	{
		py = pw->p_next;
		if(pw->p_flag&P_PIPEJOB)
			continue;
		if((flag&N_FLAG) && (pw->p_flag&P_NOTIFY)==0)
			continue;
		n = pw->p_job;
		p_sub(n,' ');
		m = ' ';
		if(pw->p_job==pwlist->p_job)
			m = '+';
		else if(pw->p_job==jobm)
		{
			m = '-';
			jobm = 0;
		}
		putc(m,output);
		putc(' ',output);
		offset = 0;
		if(flag&L_FLAG)
			px = next_proc(pwlist,(int)pw->p_job);
		else
			px = pw;
		do
		{
			unsigned xitval = 0;
			if(flag&L_FLAG)
				p_num(px->p_pid,'\t');
			if(px->p_sig&0177)
				msg = sysmsg[px->p_sig&0177];
			else if(px->p_flag&P_NOTIFY)
			{
				msg = j_Done;
				xitval = px->p_sig>>8;
			}
			else
				msg = j_Running;
			fputs(msg,output);
			msize = strlen(msg);
			if(xitval)
			{
				putc('(',output);
				p_num((int)xitval,')');
				msize += (3+(xitval>10)+(xitval>100));
			}
			if(px->p_flag&P_COREDUMP)
			{
				fputs(j_coredump,output);
				msize += strlen(j_coredump);
			}
			p_nchr(SP,MAXMSG-msize);
			if(flag&L_FLAG)
				px = next_proc(px->p_next,(int)pw->p_job);
			else
				px = NULL;
			offset = pr_jobname(pw,(px?'|':0),offset);
			if(flag&L_FLAG)
			{
				if(px)
					fputs(j_space,output);
				else if(pw->p_pid == cpid)
					fputs(j_cpid,output);
			}
		}
		while(px);
		if(pw->p_flag&P_STOPPED)
			pw->p_flag &= ~P_NOTIFY;
		else if(pw->p_flag&P_NOTIFY)
		{
			unpost(pw);
			if(jobm && (px=pgrp_jobs("%-")))
				jobm = px->p_job;
		}
	}
}

/*
 * return the pointer of next process with given job number after slot px
 */

static struct process * next_proc(pw,job)
register struct process *pw;
register int job;
{
	for(;pw;pw=pw->p_next)
	{
		if(pw->p_job == job)
			return(pw);
	}
	return(NULL);
}

/*
 * print the command
 * Stop the output if character <ch> encountered
 */

static long pr_jobname(pw,ch,offset)
register struct process *pw;
register long offset;
int ch;
{
	if(fc_fix)
	{
		offset = hist_list(pw->p_name+offset,ch,";");
		offset -= pw->p_name;
	}
	return(++offset);
}

/*
 * get the process group given the job number
 * This routine returns the process group number or -1
 */

static struct process *pgrp_jobs(ajob)
register char *ajob;
{
	register struct process *pw;
	register int c;
	if(*ajob++ != '%' || pwlist==NULL)
		return(NULL);
	c = *ajob;
	if(isdigit(c))
		pw = job_bynum(atoi(ajob));
	else if(c=='+' || c=='%')
		pw = job_bynum((int)pwlist->p_job);
	else if(c=='-')
	{
		/* find the second jobnumber on the list */
		for(pw=pwlist->p_next;pw && pw->p_job==pwlist->p_job;pw=pw->p_next);
		if(pw==NULL)
			return(pw);
		pw = job_bynum((int)pw->p_job);
	}
	else
		pw = jobbyname(ajob);
	if(pw && pw->p_flag)
		return(pw);
	return(NULL);
}

/*
 * send a hang-up signal to each of the jobs
 */
int kill_all()
{
	register struct process *pw = pwlist;
	for(;pw;pw=pw->p_next)
	{
		if(pw->p_pgrp == pw->p_pid)
		{
#ifdef BSD
			unstop(pw->p_pgrp);
			killpg(pw->p_pgrp,SIGHUP);
#else
			kill(-pw->p_pgrp,SIGHUP);
#endif	/* BSD */
		}
	}
}

/*
 * Kill a job or process
 */

int job_kill(job,sig)
register char *job;
int sig;
{
	register char *rjob = job;
	register struct process *pw;
	register int pid;
	register int r;
	char *msg;
	if(*rjob == '%')
	{
		if(pw=pgrp_jobs(rjob))
		{
#ifdef BSD
			r = unstop(pw->p_pgrp);
			if(sig!=SIGCONT || r < 0)
				r = killpg(pw->p_pgrp,sig);
#else
			pid = pw->p_pgrp;
			if(states&MONITOR)
				pid = -pid;
			r = kill(pid,sig);
#endif	/* BSD */
		}
		else
		{
			r = -1;
			errno = ESRCH;
		}
	}
	else
	{
		pid = atoi(rjob);
		if(pid==0 && *rjob != '0')
			failed(bkill,kill_usage);
#ifdef BSD
		if(sig==SIGSTOP && pid==jobstat.mypid && (login_sh || ppid==1))
		{
		/* can't stop login shell */
			errno = EPERM;
			r = -1;
		}
		else
		{
			if((pw=job_bypid(pid)) && (pw->p_flag&P_STOPPED))
			{
				pw->p_flag &= ~P_STOPPED;
				numrun++;
				kill(pid,SIGCONT);
			}
			r = kill(pid,sig);
		}
#else
		r = kill(pid,sig);
#endif	/* BSD */
	}
	if(r<0)
	{
		p_setout(stderr);
		fputs(j_kill,output);
		if(*rjob == '%')
			msg = j_no_job;
		else
			msg = j_no_proc;
		p_str(rjob,':');
		if(errno == EPERM)
			msg = j_perm;
		p_str(msg,NL);
		r = 1;
	}
	return(r);
}

/*
 * Get process structure from job number
 */

static struct process *job_bynum(num)
register int num;
{
	register struct process *pw = pwlist;
	for(;pw;pw=pw->p_next)
	{
		if((pw->p_flag&(P_PIPEJOB|P_WAITED)) == P_PIPEJOB)
			continue;
		if(pw->p_job == num)
			return(pw);
	}
	return(NULL);
}

/*
 * Get process structure from first letters of jobname
 *
 */

static struct process *jobbyname(name)
char *name;
{
	register struct process *pw = pwlist;
	register struct fixcmd *fp = fc_fix;
	if(fp==NULL)
		return(NULL);
	for(;pw;pw=pw->p_next)
	{
		if((pw->p_flag&(P_PIPEJOB|P_WAITED)) == P_PIPEJOB)
			continue;
		if(match_jobname(fp->fixfd,pw,name))
			goto retrn;
	}
	pw = NULL;
retrn:
	return(pw);
}


/*
 * match the name to the command starting with given name
 */

static int	match_jobname(fdi,pw,name)
FILE *fdi;
register struct process *pw;
char *name;
{
	register int c;
	register char *sp;
	if(fdi && pw->p_name>0)
	{
		fseek(fdi,pw->p_name,0);
		for(sp=name;*sp;sp++)
		{
			c = getc(fdi);
			if(c != *sp)
				return(0);
		}
		return(1);
	}
	return(0);
}



/*
 * Initialize the process posting array
 */

void	postclr()
{
	register struct process *pw = pwlist;
	register struct process *px;
	register int j = JBYTES;
	for(;pw;pw=px)
	{
		px = pw->p_next;
		free((char*)pw);
	}
	pwlist = NULL;
	numpost=0;
	numrun = 0;
	while(--j >=0)
		freejobs[j]  = 0;
}

/*
 * put the process <pcsid> on the process list and return the job number
 * turn on the specified flag bits.
 */

int post(pid,flag)
int pid;
{
	register struct process *pw;
	if(pw=job_post(pid))
	{
		pw->p_flag |= flag;
		return(pw->p_job);
	}
	return(-1);
}

/*
 * internal version of post, this routine returns a process structure
 */

static struct process *job_post(pcsid)
int 	pcsid;
{
	register struct process  *pw = pwlist;
	register struct process  *px=NULL;
	register struct fixcmd *fp = fc_fix;
	if(pcsid)
	{
		for(;pw;pw=pw->p_next)
		{
			if(pw->p_pid==pcsid)
				return(pw);
			if(pw->p_pgrp == jobstat.cur_pgrp)
				px = pw;
		}
		if(pw=freelist)
			freelist = pw->p_next;
		else if((pw=(struct process*)malloc(sizeof(struct process)))==NULL)
			error(nospace);
		numrun++;
		numpost++;
		if(px)
		{
			pw->p_next = px->p_next;
			px->p_next = pw;
			pw->p_job = px->p_job;
		}
		else
		{
			/* put at front of list */
			pw->p_next = pwlist;
			pw->p_job = get_job();
			pwlist = pw;
		}
		pw->p_pid = pcsid;
		pw->p_flag = P_RUNNING;
		pw->p_pgrp = jobstat.cur_pgrp;
		if(fp!=NULL)
			pw->p_name=hist_position(fp->fixind-1);
		else
			pw->p_name = -1;
		pw->p_sig = 0;
		if(numpost >= MAXJ-1)
			await(0,0);
		return(pw);
	}
	return(NULL);
}

/*
 * get job number given the process id
 */

static struct process *job_bypid(pid)
register int pid;
{
	register struct process *pw;
	if(pid==0)
		return(NULL);
	for(pw=pwlist;pw;pw=pw->p_next)
	if(pw->p_pid == pid)
			break;
	return(pw);
}

/*
 * Wait for process i to complete
 * flag=1 when wait builtin invoked so that it can be interrupted
 * flag=2 to check for waiting processes and return
 * flag=3 to resume job in foreground
 * i=0 to wait for any process
 * i=-1 to wait for all runing processes
 */

void	await(i, flag)
register int 	i;
int 	flag;
{
	int 	rc=0;
	int 	w;
	int my_job = -1;
#ifdef BSD
	union wait wstat;
#endif	/* BSD */
	register struct process *pw = NULL;
	struct process *px;
	if(i!= -1 && i)
	{
		if(flag)
		{
			pw = job_bypid(i);
			if(flag==3 && pw)
				flag = 0;
		}
		else
			pw = job_post(i);
		if(flag)
		{
			if(pw==0 || (pw->p_flag&P_STOPPED))
			return;
		}
		my_job = pw->p_job;
	}
	if(flag==0 && (states&MONITOR) && pw)
	{
		set_job(pw);
		/* if job is stopped, resume it in the background */
#if BSD || SXT
		if(pw->p_flag&P_STOPPED)
			unstop(pw->p_pgrp);
# ifdef SXT
		if(pw->p_job < jobstat.maxjob)
			open_chan(pw->p_job);
# endif /* SXT */
#endif	/* BSD */
	}
	p_flush();
	while(numrun>0)
	{
		register int 	p;
		register int 	sig;
		register int 	w_hi;
#ifdef BSD
		if(is_option(MONITOR) || flag==2)
		{
		retry:
			errno = 0;
			sig = sighold(SIGCHLD);
			trapnote &= ~SIGJOBS;
			p=wait3(&wstat,WNOHANG|WUNTRACED,0);
			errno = 0;
			if(p==0 && flag!=2)
			{
# ifdef BSD_4_2
				sigpause(sig&~(1<<(SIGCHLD-1)));
# else
				sigpause(SIGCHLD);
# endif /* BSD_4_2 */
				if(errno==EINTR && flag==1 && (trapnote&SIGJOBS)==0)
					break;
				goto retry;
			}
			trapnote &= ~SIGJOBS;
			sigrelse(SIGCHLD);
			if(p==0 || p== -1)
				return;
			w = wstat.w_status;
			pw = job_bypid(p);
			if(pw->p_job == my_job && (states&MONITOR))
			{
				register int ssig = wstat.w_S.w_Stopsig;
				if(WIFSTOPPED(wstat) && (ssig==SIGTTIN||ssig==SIGTTOU))
				{
					/* job stopped before it got terminal */
					killpg(pw->p_pgrp,SIGCONT);
					continue;
				}
				if(p==i)
					reset_ttygrp(pw,w&LOBYTE);
			}
		}
		else
#endif	/* BSD */
		{
#ifndef BSD
# ifdef SXT
			if(jobstat.maxjob >0)
			{
				/* check for job status change */
				status1 = status2;
				status2.input = status2.output = 0;
				if(ioctl(2,SXTIOCSTAT,&status2)!=0)
				{
					jobstat.maxjob = 0;
				}
				else if(status1.input != status2.input)
				{
					status_update(status2.input,status1.input,SIGSTIN);
				}
				else if(status1.output != status2.output)
				{
					status_update(status2.output,status1.output,SIGSTOP);
				}
			}
# endif /* SXT */
			/* enable fault handler to see if there are any pending jobs */
			if((trapnote&SIGJOBS)==0)
				signal(SIGCLD,fault);
			if(flag==2 && (trapnote&SIGJOBS)==0)
				return;
# ifdef SXT
			if(my_job>=0 && (trapnote&SIGJOBS)==0 && jobstat.maxjob)
			{
				errno = 0;
		        	p = ioctl(2, SXTIOCWF, 0);
				if(p==0)
					/* job stopped */
				{
       		         		ioctl(2, SXTIOCBLK, my_job);
					if(fd_chan>=0)
					{
						if(ioctl(fd_chan,TCFLSH,2)!=0)
						{
							;
						}
					}
					pw->p_flag |= (P_NOTIFY|P_STOPPED);
					pw->p_sig = SIGSTOP;
					reset_ttygrp(pw,1);
					break;
				}
			}
# endif /* SXT */
#endif	/* BSD */
			trapnote &= ~ SIGJOBS;
			p=wait(&w);
			if(p== -1)
			{
				if(errno==ECHILD)
				{
					postclr();
					return;
				}
				else if(errno==EINTR)
				{
					if(trapnote&SIGJOBS)
						continue;
					if(flag)
						break;
				}
				continue;
			}
			trapnote &= ~ SIGJOBS;
			pw = job_bypid(p);
#ifdef SXT
			pw->p_flag &= ~P_STOPPED;
#endif	/* SXT */
			/* restore original tty state if jobs aborted */
			if(p == i && (states&MONITOR))
				reset_ttygrp(pw,w&LOBYTE);
		}
		if(pw)
		{
			numrun--;
			pw->p_flag |= P_NOTIFY;
#ifdef BSD
			if((states&MONITOR) && WIFSTOPPED(wstat))
				pw->p_flag |= P_STOPPED;
			else
#endif	/* BSD */
			{
				if(p==cpid)
				{
					cpid = 0;
					cpipe[1] = NULL;
				}
				if(p==i || (states&MONITOR)==0)
				{
					if(px=unpost(pw))
					{
						if(i)
						{
							if(p==i && (states&MONITOR))
								set_job(px);
							i = px->p_pid;
						}
						continue;
					}
				}
			}
		}
		sig = w&0177;
		if(states&MONITOR)
		{
#ifdef BSD
			if(WIFSTOPPED(wstat))
			{
				if(pw)
					pw->p_sig = wstat.w_S.w_Stopsig;
				if(p == i)
					break;
				continue;
			}
			else if(p!=i)
#else
			if(p!=i)
#endif	/* BSD */
			{
				if(pw)
					pw->p_sig = w;
				if(w&HIGHBIT)
					pw->p_flag |= P_COREDUMP;
				continue;
			}
			/* this is needed since child not in same process group */
			else if(sig==SIGINT || sig==SIGQUIT || sig==SIGHUP)
				fault(sig); 
		}
		w_hi = (w>>8)&LOBYTE;
		if(sig)
		{
			if(sig == 0177	/* ptrace! return */)
			{
				fputs(ptrace,output);
				sig = w_hi;
			}
			if(sig!=SIGINT && sig!=SIGPIPE)
			{
				if(i!=p || (states&PROMPT)==0)
					p_prp(itos(p),SP);
				fputs(sysmsg[sig],output);
				if(w&HIGHBIT)
					fputs(coredump,output);
			}
			newline();
		}
		if(p == i)
		{
			rc = (sig ? sig|SIGFAIL : w_hi);
			break;
		}
		if(i == 0)
			break;
	}
	if(flag>=2)
		return;
	exitval=rc;
	exitset();
}

#if BSD || SXT
/*
 * turn off STOP state of a process group
 */

static int unstop(grp)
register int grp;
{
	register struct process *pw;
	register int num = numrun;
	for(pw=pwlist;pw;pw=pw->p_next)
	{
		if(pw->p_pgrp != grp)
			continue;
		if(pw->p_flag&P_STOPPED)
		{
			num++;
			pw->p_flag &= ~P_STOPPED;
			pw->p_sig = 0;
# ifdef SXT
			break;
# endif /* SXT */
		}
	}
	if(num!=numrun)
	{
		p_flush();
#ifdef BSD
		numrun = num;
		return(killpg(grp,SIGCONT));
#endif
	}
	return(-1);
}
#endif	/* BSD */

/*
 * remove a process group from table
 * If all the processes have not completed then unpost returns
 * the structure pointer  of an unfinished process.
 * Otherwise NULL is returned.
 */

static struct process *unpost(pwtop)
struct process *pwtop;
{
	register struct process *pw;
	register struct process *px=NULL;
	register int job = pwtop->p_job;
	register struct process *py;
	/* look for unfinished processes */
	for(pw=pwlist;pw;pw=pw->p_next)
	{
		if(pw->p_job != job)
			continue;
		if((pw->p_flag&P_NOTIFY)==0)
			px = pw;
	}
	if(px)
	{
		px->p_flag &= ~P_PIPEJOB;
		px->p_flag |= P_WAITED;
		return(px);
	}
	/* all processes complete, unpost job */
	for(pw=pwlist;pw;pw=py)
	{
		py = pw->p_next;
		if(pw->p_job != job)
		{
			px = pw;
			continue;
		}
		numpost--;
		if(px==NULL)
			pwlist = py;
		else
			px->p_next = py;
		pw->p_next = freelist;
		freelist = pw;
	}
	free_job((int)pwtop->p_job);
	return(NULL);
}

/*
 * unlink a process form the process list
 */

static void pw_unlink(pw)
register struct process *pw;
{
	register struct process *px;
	if(pw==pwlist)
	{
		pwlist = pw->p_next;
		return;
	}
	for(px=pwlist;px;px=px->p_next)
		if(px->p_next == pw)
		{
			px->p_next = pw->p_next;
			return;
		}
}

/*
 * get an unused job number
 * freejobs is a bit vector, 0 is unused
 */

static int get_job()
{
	register int j=0;
	register unsigned mask = 1;
	register unsigned char *freeword;
	/* skip to first word with a free slot */
	while(freejobs[j] == 0xff)
		j++;
	freeword = &freejobs[j];
	j *= 8;
	for(j++;mask&(*freeword);j++,mask <<=1);
	*freeword  |=mask;
	return(j);
}

/*
 * return a job number
 */

static void free_job(n)
register int n;
{
	register int j = (--n)/8;
	register unsigned mask;
	n -= j*8;
	mask = 1 << n;
	freejobs[j]  &= ~mask;
}

#ifdef SXT
/*
 * open up the input streams for a new channel
 */

static char *open_mode[3] = {"r","w","w+"};
j_new_chan()
{
	register FILE* fd;
	register int i;
	sxt[11] = '0' + jobstat.cur_job;
	close(jobstat.pipe[0]);
	fd = fopen(sxt,"r+");
	if(ioctl(fileno(fd),TCSETA,&my_stty)<0 || fd==NULL)
	{
		close(jobstat.pipe[1]);
		return(0);
	}
	for(i=0;i<3;i++)
	{
		if(isatty(i))
		{
			fclose(file_fd(i));
			fdopen(dup(fileno(fd)),open_mode[i]);
		}
	}
	fclose(fd);
	write(jobstat.pipe[1],nullstr,1);
	close(jobstat.pipe[1]);
	return(1);
}

int next_job()
{
	register int i = get_job();
	free_job(i);
	return(i);
}

static status_update(mask2,mask1,sigval)
{
	register int i,j;
	register struct process *pw;
	for(i = 1; i < MAXPCHAN; i++)
	{
		j = 1<<i;
		if((mask1&j)==0 && (mask2&j))
		{
			if(pw = job_bynum(i))
				pw->p_sig = sigval;
		}
		else if((mask2&j)==0 && (mask1&j))
		{
			if(pw = job_bynum(i))
				pw->p_sig = 0;
		}
	}
}

static int open_chan(j)
{
	sxt[11] = '0' + j;
	if(fd_chan>=0)
	{
		return;
	}
	fd_chan = open(sxt,2);
}
#endif	/* SXT */

/* XXX */
#ifdef POSIX
cfgetospeed(t)
	struct termios *t;
{
	return(t->c_ospeed);
}
#endif
