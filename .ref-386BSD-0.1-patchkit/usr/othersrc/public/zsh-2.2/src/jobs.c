/*
 *
 * jobs.c - job control
 *
 * This file is part of zsh, the Z shell.
 *
 * This software is Copyright 1992 by Paul Falstad
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 *
 */

#include "zsh.h"
#include <sys/errno.h>

/* != 0 means the handler is active */

static int handling = 0;

#ifdef INTHANDTYPE
#define RETURN return 0
#else
#define RETURN return
#endif

/* the signal handler */

HANDTYPE handler(sig,code) /**/
int sig;int code;
{
long pid;
int statusp;
Job jn;
struct process *pn;
#ifdef HAS_RUSAGE
struct rusage ru;
#else
long chlds,chldu;
#endif

#ifdef RESETHANDNEEDED
	signal(sig,handler);
#endif
	if (sig == SIGINT)
		{
		if (sigtrapped[SIGINT])
			dotrap(SIGINT);
		else
			errflag = 1;
		RETURN;
		}
#ifdef SIGWINCH
	if (sig == SIGWINCH)
		adjustwinsize();
#endif
	if (sig != SIGCHLD)
		{
		dotrap(sig);
		if (sig == SIGALRM && !sigtrapped[SIGALRM])
			{
			zerr("timeout",NULL,0);
			exit(1);
			}
		RETURN;
		}
	for (;;)
		{
#ifdef HAS_RUSAGE
		pid = wait3((vptr) &statusp,WNOHANG|WUNTRACED,&ru);
#else
#ifndef WNOHANG
		pid = wait(&statusp);
#else
		pid = wait3((vptr) &statusp,WNOHANG|WUNTRACED,NULL);
#endif
		chlds = shtms.tms_cstime;
		chldu = shtms.tms_cutime;
		times(&shtms);
#endif
		if (pid == -1)
			{
			if (errno != ECHILD)
				zerr("wait failed: %e",NULL,errno);
			RETURN;
			}
		if (!pid)
			RETURN;
		findproc(pid,&jn,&pn);	/* find the process of this pid */
		if (jn)
			{
			pn->statusp = statusp;
			handling = 1;
#ifdef HAS_RUSAGE
			pn->ti.ru = ru;
#else
			pn->ti.st = shtms.tms_cstime-chlds;
			pn->ti.ut = shtms.tms_cutime-chldu;
#endif
			pn->endtime = time(NULL);
			updatestatus(jn);
			handling = 0;
			}
#if 0
		else if (WIFSTOPPED(statusp))
			kill(pid,SIGKILL);	/* kill stopped untraced children */
#endif
		}
}

/* change job table entry from stopped to running */

void makerunning(jn) /**/
Job jn;
{
struct process *pn;

	jn->stat &= ~STAT_STOPPED;
	for (pn = jn->procs; pn; pn = pn->next)
		if (WIFSTOPPED(pn->statusp))
			pn->statusp = SP_RUNNING;
}

/* update status of job, possibly printing it */

void updatestatus(jn) /**/
Job jn;
{
struct process *pn;
int notrunning = 1,alldone = 1,val,job = jn-jobtab,somestopped = 0;

	for (pn = jn->procs; pn; pn = pn->next)
		{
		if (pn->statusp == SP_RUNNING)
			notrunning = 0;
		if (pn->statusp == SP_RUNNING || WIFSTOPPED(pn->statusp))
			alldone = 0;
		if (WIFSTOPPED(pn->statusp))
			somestopped = 1;
		if (!pn->next && jn)
			val = (WIFSIGNALED(pn->statusp)) ?
				0200 | WTERMSIG(pn->statusp) : WEXITSTATUS(pn->statusp);
		}
	if (!notrunning)
		return;
	if (somestopped && (jn->stat & STAT_STOPPED))
		return;
	jn->stat |= (alldone) ? STAT_CHANGED|STAT_DONE :
		STAT_CHANGED|STAT_STOPPED;
	if (alldone && job == thisjob)
		{
		if (!ttyfrozen && !val) {
			gettyinfo(&shttyinfo);
			if (interact && isset(SHINSTDIN) && SHTTY != -1 && isset(USEZLE))
				sanetty(&shttyinfo);
#ifdef TIOCSWINSZ
			if (!(columns = shttyinfo.winsize.ws_col))
				columns = 80;
			lines = shttyinfo.winsize.ws_row;
#endif
		} else
			settyinfo(&shttyinfo);
		lastval = val;
		}
	if ((jn->stat & (STAT_DONE|STAT_STOPPED)) == STAT_STOPPED) {
		prevjob = curjob;
		curjob = job;
	}
	if ((isset(NOTIFY) || job == thisjob) && jn->stat & STAT_LOCKED) {
		printjob(jn,!!isset(LONGLISTJOBS));
		if (zleactive) refresh();
	}
	if (sigtrapped[SIGCHLD] && job != thisjob)
		dotrap(SIGCHLD);
}

/* find process and job associated with pid */

void findproc(pid,jptr,pptr) /**/
int pid;Job *jptr;struct process **pptr;
{
struct process *pn;
int jn;

	for (jn = 1; jn != MAXJOB; jn++)
		for (pn = jobtab[jn].procs; pn; pn = pn->next)
			if (pn->pid == pid)
				{
				*pptr = pn;
				*jptr = jobtab+jn;
				return;
				}
	*pptr = NULL;
	*jptr = NULL;
}

/*
	lng = 0 means jobs 
	lng = 1 means jobs -l
	lng = 2 means jobs -p
*/

void printjob(jn,lng) /**/
Job jn;int lng;
{
int job = jn-jobtab,len = 9,sig = -1,sflag = 0,llen,printed = 0;
int conted = 0,lineleng = getlineleng(),skip = 0,doputnl = 0;
struct process *pn;

	if (lng < 0)
		{
		conted = 1;
		lng = 0;
		}

	/* find length of longest signame, check to see if we
		really need to print this job */

	for (pn = jn->procs; pn; pn = pn->next)
		{
		if (pn->statusp != SP_RUNNING)
			if (WIFSIGNALED(pn->statusp))
				{
				sig = WTERMSIG(pn->statusp);
				llen = strlen(sigmsg[sig]);
				if (WCOREDUMPED(pn->statusp))
					llen += 14;
				if (llen > len)
					len = llen;
				if (sig != SIGINT && sig != SIGPIPE)
					sflag = 1;
				else if (sig == SIGINT)
					errflag = 1;
				if (job == thisjob && sig == SIGINT)
					doputnl = 1;
				}
			else if (WIFSTOPPED(pn->statusp))
				{
				sig = WSTOPSIG(pn->statusp);
				if (strlen(sigmsg[sig]) > len)
					len = strlen(sigmsg[sig]);
				if (job == thisjob && sig == SIGTSTP)
					doputnl = 1;
				}
			else if (isset(PRINTEXITVALUE) && isset(SHINSTDIN) &&
					WEXITSTATUS(pn->statusp))
				sflag = 1;
		}

	/* print if necessary */

	if (interact && jobbing && ((jn->stat & STAT_STOPPED) || sflag ||
			job != thisjob))
		{
		int len2,fline = 1;
		struct process *qn;

		trashzle();
		if (doputnl)
			putc('\n',stderr);
		for (pn = jn->procs; pn;)
			{
			len2 = ((job == thisjob) ? 5 : 10)+len; /* 2 spaces */
			if (lng)
				qn = pn->next;
			else for (qn = pn->next; qn; qn = qn->next)
				{
				if (qn->statusp != pn->statusp)
					break;
				if (strlen(qn->text)+len2+((qn->next) ? 3 : 0) > lineleng)
					break;
				len2 += strlen(qn->text)+2;
				}
			if (job != thisjob)
				if (fline)
					fprintf(stderr,"[%d]  %c ",jn-jobtab,(job == curjob) ? '+' :
						(job == prevjob) ? '-' : ' ');
				else
					fprintf(stderr,(job > 9) ? "        " : "       ");
			else
				fprintf(stderr,"zsh: ");
			if (lng)
				if (lng == 1)
					fprintf(stderr,"%d ",pn->pid);
				else
					{
					int x = jn->gleader;

					fprintf(stderr,"%d ",x);
					do skip++; while (x /= 10);
					skip++;
					lng = 0;
					}
			else
				fprintf(stderr,"%*s",skip,"");
			if (pn->statusp == SP_RUNNING)
				if (!conted)
					fprintf(stderr,"running%*s",len-7+2,"");
				else
					fprintf(stderr,"continued%*s",len-9+2,"");
			else if (WIFEXITED(pn->statusp))
				if (WEXITSTATUS(pn->statusp))
					fprintf(stderr,"exit %-4d%*s",WEXITSTATUS(pn->statusp),
						len-9+2,"");
				else
					fprintf(stderr,"done%*s",len-4+2,"");
			else if (WIFSTOPPED(pn->statusp))
				fprintf(stderr,"%-*s",len+2,sigmsg[WSTOPSIG(pn->statusp)]);
			else if (WCOREDUMPED(pn->statusp))
				fprintf(stderr,"%s (core dumped)%*s",
					sigmsg[WTERMSIG(pn->statusp)],
					len-14+2-strlen(sigmsg[WTERMSIG(pn->statusp)]),"");
			else
				fprintf(stderr,"%-*s",len+2,sigmsg[WTERMSIG(pn->statusp)]);
			for (; pn != qn; pn = pn->next)
				fprintf(stderr,(pn->next) ? "%s | " : "%s",pn->text);
			putc('\n',stderr);
			fline = 0;
			}
		printed = 1;
		}
	else if (doputnl && interact)
		putc('\n',stderr);
	fflush(stderr);

	/* print "(pwd now: foo)" messages */

	if (interact && job==thisjob && strcmp(jn->pwd,pwd))
		{
		printf("(pwd now: ");
		printdir(pwd);
		printf(")\n");
		fflush(stdout);
		}

	/* delete job if done */

	if (jn->stat & STAT_DONE)
		{
		static struct job zero;
		struct process *nx;
		char *s;

		if ((jn->stat & STAT_TIMED) || (reporttime != -1 && report(jn))) {
			dumptime(jn);
			printed = 1;
		}
		for (pn = jn->procs; pn; pn = nx)
			{
			nx = pn->next;
			free(pn);
			}
		free(jn->pwd);
		if (jn->filelist)
			{
			while (s = getnode(jn->filelist))
				{
				unlink(s);
				free(s);
				}
			free(jn->filelist);
			}
		*jn = zero;
		if (job == curjob)
			{
			curjob = prevjob;
			prevjob = job;
			}
		if (job == prevjob)
			setprevjob();
		}
	else
		jn->stat &= ~STAT_CHANGED;
}

/* set the previous job to something reasonable */

void setprevjob() /**/
{
int t0;

	for (t0 = MAXJOB-1; t0; t0--)
		if ((jobtab[t0].stat & STAT_INUSE) && (jobtab[t0].stat & STAT_STOPPED) &&
				t0 != curjob && t0 != thisjob)
			break;
	if (!t0)
		for (t0 = MAXJOB-1; t0; t0--)
			if ((jobtab[t0].stat & STAT_INUSE) && t0 != curjob && t0 != thisjob)
				break;
	prevjob = (t0) ? t0 : -1;
}

/* initialize a job table entry */

void initjob() /**/
{
	jobtab[thisjob].pwd = ztrdup(pwd);
	jobtab[thisjob].stat = STAT_INUSE;
	jobtab[thisjob].gleader = 0;
}

/* add a process to the current job */

struct process *addproc(pid,text) /**/
long pid;char *text;
{
struct process *process;

	if (!jobtab[thisjob].gleader) jobtab[thisjob].gleader = pid;
	process = zcalloc(sizeof *process);
	process->pid = pid;
	if (text) strcpy(process->text,text);
	else *process->text = '\0';
	process->next = NULL;
	process->statusp = SP_RUNNING;
	process->bgtime = time(NULL);
	if (jobtab[thisjob].procs) {
		struct process *n;
		for (n = jobtab[thisjob].procs; n->next; n = n->next);
		process->next = NULL;
		n->next = process;
	} else jobtab[thisjob].procs = process;
	return process;
}

/* determine if it's all right to exec a command without
	forking in last component of subshells; it's not ok if we have files
	to delete */

int execok() /**/
{
Job jn;

	if (!exiting)
		return 0;
	for (jn = jobtab+1; jn != jobtab+MAXJOB; jn++)
		if (jn->stat && jn->filelist)
			return 0;
	return 1;

}

void waitforpid(pid) /**/
long pid;
{
	while (!errflag && (kill(pid,0) >= 0 || errno != ESRCH)) chldsuspend();
}

/* wait for a job to finish */

void waitjob(job) /**/
int job;
{
static struct job zero;
Job jn;

	if (jobtab[job].procs)	/* if any forks were done */
		{
		jobtab[job].stat |= STAT_LOCKED;
		if (jobtab[job].stat & STAT_CHANGED)
			printjob(jobtab+job,!!isset(LONGLISTJOBS));
		while (jobtab[job].stat &&
				!(jobtab[job].stat & (STAT_DONE|STAT_STOPPED)))
			chldsuspend();
		}
	else	/* else do what printjob() usually does */
		{
		char *s;

		jn = jobtab+job;
		free(jn->pwd);
		if (jn->filelist)
			{
			while (s = getnode(jn->filelist))
				{
				unlink(s);
				free(s);
				}
			free(jn->filelist);
			}
		*jn = zero;
		}
}

/* wait for running job to finish */

void waitjobs() /**/
{
	waitjob(thisjob);
	thisjob = -1;
}

/* clear job table when entering subshells */

void clearjobtab() /**/
{
static struct job zero;
int t0;

	for (t0 = 1; t0 != MAXJOB; t0++)
		jobtab[thisjob] = zero;
}

/* get a free entry in the job table to use */

int getfreejob() /**/
{
int t0;

	for (t0 = 1; t0 != MAXJOB; t0++)
		if (!jobtab[t0].stat) {
			jobtab[t0].stat |= STAT_INUSE;
			return t0;
		}
	zerr("job table full or recursion limit exceeded",NULL,0);
	return -1;
}

/* print pids for & */

void spawnjob() /**/
{
struct process *pn;

	if (!subsh)
		{
		if (curjob == -1 || !(jobtab[curjob].stat & STAT_STOPPED))
			{
			curjob = thisjob;
			setprevjob();
			}
		else if (prevjob == -1 || !(jobtab[prevjob].stat & STAT_STOPPED))
			prevjob = thisjob;
		if (interact && jobbing && jobtab[thisjob].procs)
			{
			fprintf(stderr,"[%d]",thisjob);
			for (pn = jobtab[thisjob].procs; pn; pn = pn->next)
				fprintf(stderr," %d",pn->pid);
			fprintf(stderr,"\n");
			fflush(stderr);
			}
		}
	if (!jobtab[thisjob].procs)
		{
		char *s;
		static struct job zero;
		struct job *jn;

		jn = jobtab+thisjob;
		free(jn->pwd);
		if (jn->filelist)
			{
			while (s = getnode(jn->filelist))
				{
				unlink(s);
				free(s);
				}
			free(jn->filelist);
			}
		*jn = zero;
		}
	else
		jobtab[thisjob].stat |= STAT_LOCKED;
	thisjob = -1;
}

void fixsigs() /**/
{
	unblockchld();
}

int report(j) /**/
Job j;
{
	if (!j->procs) return 0;
#ifdef HAS_RUSAGE
	return (j->procs->ti.ru.ru_utime.tv_sec+j->procs->ti.ru.ru_stime.tv_sec)
				 >= reporttime;
#else
	return (j->procs->ti.ut+j->procs->ti.st)/HZ >= reporttime;
#endif
}

void printtime(real,ti,desc) /**/
time_t real;struct timeinfo *ti;char *desc;
{
char *s;
#ifdef sun
long ticks = 1;
int pk = getpagesize()/1024;
#else
long sec;
#endif
#ifdef HAS_RUSAGE
struct rusage *ru = &ti->ru;
#endif

	if (!desc) desc = "";
#ifdef HAS_RUSAGE
#ifdef sun
	ticks = (ru->ru_utime.tv_sec+ru->ru_stime.tv_sec)*HZ+
			  (ru->ru_utime.tv_usec+ru->ru_stime.tv_usec)*HZ/1000000;
	if (!ticks) ticks = 1;
#else
	sec = ru->ru_utime.tv_sec + ru->ru_stime.tv_sec;
	if (!sec) sec = 1;
#endif
#endif
	for (s = timefmt; *s; s++)
		if (*s == '%')
			switch(s++,*s)
				{
				case 'E': fprintf(stderr,"%lds",real); break;
#ifndef HAS_RUSAGE
				case 'U': fprintf(stderr,"%ld.%03lds",
					ti->ut/HZ,ti->ut*1000/60%1000); break;
				case 'S': fprintf(stderr,"%ld.%03lds",
					ti->st/HZ,ti->st*1000/60%1000); break;
				case 'P':
					if (real)
						fprintf(stderr,"%d%%",
							(int) (100*((ti->ut+ti->st)/HZ))/real);
					break;
#else
				case 'U': fprintf(stderr,"%ld.%03lds",
					ru->ru_utime.tv_sec,ru->ru_utime.tv_usec/1000); break;
				case 'S': fprintf(stderr,"%ld.%03lds",
					ru->ru_stime.tv_sec,ru->ru_stime.tv_usec/1000); break;
				case 'P':
					if (real)
						fprintf(stderr,"%d%%",
							(int) (100*(ru->ru_utime.tv_sec+ru->ru_stime.tv_sec))
								/ real);
					break;
				case 'W': fprintf(stderr,"%ld",ru->ru_nswap); break;
#ifdef sun
				case 'K': case 'D':
					fprintf(stderr,"%ld",ru->ru_idrss/ticks*pk); break;
				case 'M': fprintf(stderr,"%ld",ru->ru_maxrss*pk); break;
#else
				case 'X': fprintf(stderr,"%ld",ru->ru_ixrss/sec); break;
				case 'D': fprintf(stderr,"%ld",
					(ru->ru_idrss+ru->ru_isrss)/sec); break;
				case 'K': fprintf(stderr,"%ld",
					(ru->ru_ixrss+ru->ru_idrss+ru->ru_isrss)/sec); break;
				case 'M': fprintf(stderr,"%ld",ru->ru_maxrss/1024); break;
#endif
				case 'F': fprintf(stderr,"%ld",ru->ru_majflt); break;
				case 'R': fprintf(stderr,"%ld",ru->ru_minflt); break;
				case 'I': fprintf(stderr,"%ld",ru->ru_inblock); break;
				case 'O': fprintf(stderr,"%ld",ru->ru_oublock); break;
				case 'r': fprintf(stderr,"%ld",ru->ru_msgrcv); break;
				case 's': fprintf(stderr,"%ld",ru->ru_msgsnd); break;
				case 'k': fprintf(stderr,"%ld",ru->ru_nsignals); break;
				case 'w': fprintf(stderr,"%ld",ru->ru_nvcsw); break;
				case 'c': fprintf(stderr,"%ld",ru->ru_nivcsw); break;
#endif
				case 'J': fprintf(stderr,"%s",desc); break;
				default: fprintf(stderr,"%%%c",*s); break;
				}
		else
			putc(*s,stderr);
	putc('\n',stderr);
	fflush(stderr);
}

void dumptime(jn) /**/
Job jn;
{
struct process *pn = jn->procs;

	if (!jn->procs)
		return;
	for (pn = jn->procs; pn; pn = pn->next)
		printtime(pn->endtime-pn->bgtime,&pn->ti,pn->text);
}

void shelltime() /**/
{
struct timeinfo ti;
#ifdef HAS_RUSAGE
struct rusage ru;

	getrusage(RUSAGE_SELF,&ru);
	memcpy(&ti.ru,&ru,sizeof(ru));
	printtime(time(NULL)-shtimer,&ti,"shell");

	getrusage(RUSAGE_CHILDREN,&ru);
	memcpy(&ti.ru,&ru,sizeof(ru));
	printtime(time(NULL)-shtimer,&ti,"children");
#else
struct tms buf;

	times(&buf);
	ti.ut = buf.tms_utime;
	ti.st = buf.tms_stime;
	printtime(time(NULL)-shtimer,&ti,"shell");
	ti.ut = buf.tms_cutime;
	ti.st = buf.tms_cstime;
	printtime(time(NULL)-shtimer,&ti,"children");
#endif
}

/* SIGHUP any jobs left running */

void killrunjobs() /**/
{
int t0,killed = 0;

	if (isset(NOHUP)) return;
	for (t0 = 1; t0 != MAXJOB; t0++)
		if (t0 != thisjob && (jobtab[t0].stat & STAT_LOCKED) &&
				!(jobtab[t0].stat & STAT_STOPPED)) {
			if (killpg(jobtab[t0].gleader,SIGHUP) != -1) killed++;
		}
	if (killed) zerr("warning: %d jobs SIGHUPed",NULL,killed);
}

/* check to see if user has jobs running/stopped */

void checkjobs() /**/
{
int t0;

	scanjobs();
	for (t0 = 1; t0 != MAXJOB; t0++)
		if (t0 != thisjob && jobtab[t0].stat & STAT_LOCKED)
			break;
	if (t0 != MAXJOB) {
		if (jobtab[t0].stat & STAT_STOPPED) {
#ifdef USE_SUSPENDED
			zerr("you have suspended jobs.",NULL,0);
#else
			zerr("you have stopped jobs.",NULL,0);
#endif
		} else
			zerr("you have running jobs.",NULL,0);
		stopmsg = 1;
	}
}

/* send a signal to a job (simply involves kill if monitoring is on) */

int killjb(jn,sig) /**/
Job jn;int sig;
{
struct process *pn;
int err;

	if (jobbing)
		return(killpg(jn->gleader,sig));
	for (pn = jn->procs; pn; pn = pn->next)
		if ((err = kill(pn->pid,sig)) == -1 && errno != ESRCH)
			return -1;
	return err;
}

