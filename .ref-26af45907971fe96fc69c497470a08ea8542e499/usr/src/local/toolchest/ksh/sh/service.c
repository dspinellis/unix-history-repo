/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)service.c	1.1 */
/*
 * UNIX shell
 *
 * S. R. Bourne
 * Rewritten by David Korn
 * AT&T Bell Laboratories
 *
 */

#include	<errno.h>
#include	<sys/types.h>
#include	<sys/stat.h>
#include	"flags.h"
#include	"defs.h"
#include	"sym.h"
#include	"io.h"
#include	"name.h"
#include	"brkincr.h"
#include	"mode.h"
#include	"stak.h"
#include	"shtype.h"
#include	"builtins.h"
#include	"jobs.h"

#ifndef BSD_4_2
# ifdef BSD
# define fcntl(a,b,c)	dup2(a,b)
# endif /* BSD */
#endif /* BSD_4_2 */

FILE	*pathopen();
char	*getpath();
char	*fullname();
int	initio();
void	exfunct();
#ifndef JOBS
void	postclr();
int	post();
void	await();
#endif	/* JOBS */

extern void	arg_clear();
extern DOLPTR	arg_new();
extern void	arg_set();
extern void	arg_reset();
extern char	*catpath();
extern FILE	*chkopen();
extern STKPTR	cpystak();
extern FILE	*create();
extern void	exitsh();
extern void	failed();
extern NAMPTR	findnod();
extern void	free();
extern void	gscan_some();
extern char	*itos();
extern STKPTR	locstak();
extern void	mac_subst();
extern char	*mactrim();
extern void	mem_scope();
extern void	mem_unscope();
extern char	*movstr();
extern void	p_setout();
extern void	p_flush();
extern void	p_prp();
#ifdef JOBS
extern void	postclr();
#endif	/* JOBS */
extern char	*qvalup();
extern char	*realias();
extern void	rmlocal();
extern char	**setenv();
extern void	setlist();
extern char	*simple();
extern char	*strchr();
extern FILE	*tmp_open();
extern void	trim();
extern char	*valup();

static char	*prune();
#ifndef VFORK
static void	 exscript();
#endif /* VFORK */
static char	 *execs();

#define LOBYTE 0377
#ifdef u370
#define MAXP 75
#else
#define MAXP 32
#endif	/* u370 */
#define MAXDEPTH (32*sizeof(int))	/* maximum levels of recursion */


#ifndef JOBS
static int	maxpost;	/* highest number running process */
static int	numpost;	/* number of running processes */

/* for processes to be waited for */
static struct process pwlist[MAXP];
#endif	/* JOBS */
static char *xecmsg;
static char **xecenv;
#ifdef VFORK
NAMPTR lookup();
#endif	/* VFORK */


/*
 * service routines for `execute'
 * flag > 0 if files are to be restored
 * flag < 0 if files are to be closed on exec
 */

int	initio(iop,flag)
IOPTR		iop;
{
	register char *ion;
	register int 	iof;
	register FILE	*fd;
	char fname[TMPSIZ];
	int fn;
	int mark = MARK;
	int indx = topfd;
	if(flag<0)
		mark = 0;
	for(;iop;iop=iop->ionxt)
	{
		iof=iop->iofile;
		if(flag>0)
		{
			/* save current file descriptor */
			savefd(iof&IOUFD,indx);
		}
		ion=mactrim(iop->ioname,1);
		if(*ion && is_option(NOEXEC)==0)
		{
			if(iof&IODOC)
			{
				fd = tmp_open(fname);
				unlink(fname);
				mac_subst(chkopen(ion),fd);
			}
			else if(iof&IOMOV)
			{
				if(eq(minus,ion))
				{
					fclose(file_fd(iof&IOUFD));
					fd = NULL;
				}
				else if(((fn=atoi(ion))>=USERIO) ||
					(fd=fdopen(dup(fn),(iof&IOPUT)?"w+":"r")) == NULL)
					failed(ion,badfile);
			}
			else if((iof&IOPUT)==0)
			{
				fd=chkopen(ion);
			}
			else if(is_option(RSHFLG))
				failed(ion,restricted);
			else if((iof&IOAPP) == 0 || (fd=fdopen(open(ion,1),"a"))==NULL)
			{
				if((fd=create(ion)) == NULL)
					failed(ion,badcreate);
			}
			if(fd!=NULL)
				frenumber(fd,(iof&IOUFD)|mark);
		}
	}
	return(indx);
}

/*
 * given <s> return a colon separated list of directories to search on the stack
 * This routine adds names to the tracked alias list, if possible, and returns
 * a reduced path string for tracked aliases
 */

char *getpath(s)
char *s;
{
	register char *path;
	register NAMPTR np;
	if(strchr(s,'/'))
		return(nullstr);
	path = qvalup(PATHNOD);
	if(path==NULL)
		path = defpath;
	path = cpystak(path);
	/* track alias if possible */
	np=findnod(s,alias,CHK_FOR);
	if(np==NULL && is_option(HASHALL) && (np=findnod(s,alias,CHK_FOR|1)))
		attrib(np, N_EXPORT|NO_ALIAS|T_FLAG);
	if(np && attest(np,T_FLAG))
	{
		if(attest(np,NO_ALIAS))
		{
			/* don't bother to look up alias if forked  */
			if(states&FORKED)
				return(path);
			/* if realias fails then a search won't find anything */
			if(realias(np)==NIL)
				return(nullstr);
		}
		/* tracked alias use reduced path */
		path = prune(path,valup(np));
	}
	return(path);
}

FILE	*pathopen(name)
register char *name;
{
	register char *path;
	register int n;
	struct stat statb;
	if(strchr(name,'/'))
	{
		if(is_option(RSHFLG))
			failed(name, restricted);
		else
			path = nullstr;
	}
	else
	{
		path = qvalup(PATHNOD);
		if(path==NULL)
			path = defpath;
		path = cpystak(path);
	}
	do
	{
		path=catpath(path,name);
		if((n = open(curstak(),0)) >= 0)
		{
			if(fstat(n,&statb)<0 || (statb.st_mode&S_IFMT)!=S_IFREG)
			{
				close(n);
				n = -1;
			}
		}
	}
	while( n<0 && path);
	return(fdopen(n,"r"));
}


/*
 * do a path search and find the full pathname of file name
 * if name is not a simple name, then name is a tracked alias
 */

char	*fullname(fname)
char *fname;
{
	register char *name; 
	register int	f;
	register char *path;
	if(*fname!='/')
		name=fname;
	else
		name = simple(fname);
	path = getpath(name);
	do
	{
		path=catpath(path,name);
		if((f=access(curstak(),1))>=0)
		{
			if(!ftype(curstak(),S_IFMT,S_IFREG))
				f = -1;
		}
	}
	while(f<0 && path);
	return(f<0?0:curstak());
}

char *catpath(path,name)
register char *path;
char *	name;
{
	/* leaves result on top of stack */
	register char *scanp = path;
	register char *argp = locstak();
	while(*scanp && *scanp!=':')
		*argp++ = *scanp++;
	if(scanp!=path)
	{
		*argp++= '/';
		/* position past ":" unless a trailing colon after pathname */
		if(*scanp && *++scanp==0)
			scanp--;
	}
	else
		while(*scanp == ':')
			scanp++;
	path=(*scanp ? scanp : 0);
	scanp=name;
	while((*argp++ = *scanp++));
	staktop = argp;
	return(path);
}


void	execa(at,local)
char *	at[];
ARGPTR local;		/* local environment modification */
{
	register char *path = nullstr;
	register char **t = at;
	if(is_option(NOEXEC)==0)
	{
		xecmsg=notfound;
#ifdef VFORK
		if(local)
			mem_scope(local);
		xecenv=setenv();
#else
		setlist(local,N_EXPORT);
		xecenv=setenv();
#endif	/* VFORK */
		if(strchr(t[0],'/'))
		{
			/* name containing / not allowed for restricted shell */
			if(is_option(RSHFLG))
				failed(t[0],restricted);
		}
		else
			path=getpath(*t);
#ifdef VFORK
		if(local)
			mem_unscope();
#endif	/* VFORK */
		/* insert _= onto stack in front of pathname */
		*--xecenv =  stakbot;
		*stakbot++ = '_';
		*stakbot++ = '=';
		while(path=execs(path,t));
		failed(*t,xecmsg);
	}
}

/*
 * This routine constructs a short path consisting of all
 * Relative directories up to the directory of fullname <name>
 */
static char *prune(path,fullname)
register char *path;
char *fullname;
{
	register char *p = path;
	register char *s;
	int n = 1; 
	char *base;
	char *inpath = path;
	if(fullname==NULL  || *fullname != '/' || *path==0)
		return(path);
	base = simple(fullname);
	do
	{
		/* a null path means current directory */
		if(*path == ':')
		{
			*p++ = ':';
			path++;
			continue;
		}
		s = path;
		path=catpath(path,base);
		if(*s != '/' || (n=strcmp(curstak(),fullname))==0)
		{
			/* position p past end of path */
			while(*s && *s!=':')
				*p++ = *s++;
			if(n==0)
			{
				*p = 0;
				return(inpath);
			}
			*p++ = ':';
		}
	}
	while(path);
	/* if there is no match just return path */
	path = qvalup(PATHNOD);
	if(path==NULL)
		path = defpath;
	strcpy(inpath,path);
	return(inpath);
}

#ifdef XENIX
/*
 *  This code takes care of a bug in the XENIX exec routine
 *  Contributed by Pat Wood
 */
static ex_xenix(file)
char *file;
{
	struct stat stats;
	register int fd;
	unsigned short magic;
	if((fd = open(file,0)) == -1) /* can't read, so can't be shell prog */
		return(1);
	read(fd, &magic, sizeof(magic));
	if(magic == 01006) /* magic for xenix executable */
	{
		close(fd);
		return(1);
	}
	fstat(fd, &stats);
	close(fd);
	errno = ENOEXEC;
	if(!geteuid())
	{
		if(!(stats.st_mode & 0111))
			errno = EACCES;
		return(0);
	}
	if((geteuid() == stats.st_uid))
	{
		if(!(stats.st_mode & 0100))
			errno = EACCES;
		return(0);
	}
	if((getegid() == stats.st_gid))
	{
		if(!(stats.st_mode & 0010))
			errno = EACCES;
		return(0);
	}
	if(!(stats.st_mode & 0001))
		errno = EACCES;
	return(0);
}
#endif	/* XENIX */

static char *execs(ap,t)
char *	ap;
register char **t;
{
	register char *p, *prefix;
	prefix=catpath(ap,t[0]);
	trim(p=curstak());
	p_flush();
	if(trapnote&SIGSET)
		exitsh(SIGFAIL);
#ifdef XENIX
	if(ex_xenix(p))
#endif	/* XENIX */
	execve(p, &t[0] ,xecenv);
	switch(errno)
	{
		case ENOEXEC:
#ifdef VFORK
		{
			/* this code handles the !# interpreter name convention */
			char iname[256];
#ifdef SUID_EXEC
			/* check if file cannot open for read or script is setuid/setgid  */
			static char name[] = "/tmp/euidXXXXXX";
			register int n;
			register int euserid;
			struct stat statb;
			if((n=open(p,0)) >= 0)
			{
				if(fstat(n,&statb)==0)
				{
					if((statb.st_mode&(S_ISUID|S_ISGID))==0)
						goto openok;
				}
				close(n);
			}
			if((euserid=geteuid()) != userid)
			{
				strcpy(name+9,itos(getpid()));
				/* create a suid open file with owner equal effective uid */
				if((n=creat(name,04100)) < 0)
					goto fail;
				unlink(name);
				/* make sure that file has right owner */
				if(fstat(n,&statb)<0 || statb.st_uid != euserid)
					goto fail;
				if(n!=10)
				{
					close(10);
					fcntl(n,0,10);
					close(n);
				}
			}
			*--t = p;
			execve(suid_exec,t,xecenv);
	fail:
			failed(p, badopen);
	openok:
			close(n);
#endif SUID_EXEC
			/* get name returns the interpreter name */
			if(get_shell(p, iname)<0)
				failed(p, badexec);
			t--;
			t[0] = iname;
			execve(iname, t, xecenv);
			if(access(iname,0)==0)
				xecmsg=badexec;
			failed(iname, xecmsg);
		}
#else
			exscript(p,t);
#endif	/* VFORK */

		case ENOMEM:
			failed(p,toobig);

		case E2BIG:
			failed(p,arglist);

		case ETXTBSY:
			failed(p,txtbsy);

		default:
			if(access(p,0)==0)
				xecmsg=badexec;
		case ENOENT:
			return(prefix);
	}
}

/*
 * File is executable but not machine code.
 * Assume file is a Shell script and execute it.
 */


static void exscript(p,t)
register char *p;
register char *t[];
{
	char *savet;
	flags=0;
	states = 0;
	comdiv=0;
	ioset=0;
	arg_clear(); /* remove open files and for loop junk */
	postclr();
	if(fileno(input))
		fclose(input);
	p_flush();
	standout= stdout;
	setbuf(stdin,(char*)_sibuf);
#ifdef SUID_EXEC
	/* check if file cannot open for read or script is setuid/setgid  */
	{
		static char name[] = "/tmp/euidXXXXXX";
		register int n;
		register int euserid;
		struct stat statb;
		if((n=open(p,0)) >= 0)
		{
			if(fstat(n,&statb)==0)
			{
				if((statb.st_mode&(S_ISUID|S_ISGID))==0)
					goto openok;
			}
			close(n);
		}
		if((euserid=geteuid()) != userid)
		{
			strcpy(name+9,itos(getpid()));
			/* create a suid open file with owner equal effective uid */
			if((n=creat(name,04100)) < 0)
				goto fail;
			unlink(name);
			/* make sure that file has right owner */
			if(fstat(n,&statb)<0 || statb.st_uid != euserid)
				goto fail;
			if(n!=10)
			{
				close(10);
				fcntl(n,0,10);
				close(n);
			}
		}
		savet = *--t;
		*t = p;
		execve(suid_exec,t,xecenv);
	fail:
		/*
		 *  The following code is just for compatibility
		 *  It should be replaced with the line failed(p,badexec);
		 */
		n = open(p,0);
		if(n < 0)
			failed(p, badopen);
		*t++ = savet;
		close(10);

	openok:
		input = fdopen(n,"r");
	}
#else
	input = chkopen(p);
#endif /* SUID_EXEC */
#ifdef ACCT
	preacct(p) ;  /* reset accounting */
#endif	/* ACCT */
	gscan_some(rmlocal,namep,N_EXPORT,0);	/* remove local variables*/
	gscan_some(rmlocal,alias,N_EXPORT,0);	/* remove local aliases*/
	gscan_some(rmlocal,prnames,N_EXPORT,0);	/* remove local functions*/
	if(attest(IFSNOD,N_EXPORT)==0)
		assign(IFSNOD,sptbnl);
/* set up new args */
	arg_set(t);
	longjmp(subshell,1);
}

/*
 * The following routine is used to execute shell functions and command subs
 * when com!=NULL $* is saved and restored
 */

void exfunct(t,com,execflg,envlist)
TREPTR t;
register char *com[];
register unsigned execflg;
ARGPTR envlist;
{
	/* execute user defined function */
	register char *trap;
	jmp_buf retbuf;
	jmp_buf *savreturn = freturn;
	DOLPTR	argsav=0;
	int mode;
	DOLPTR savargfor;
	char *savtrap0 = trapcom[0];
	char *savtrap1 = trapcom[MAXTRAP];
	SHFILE savstandin;
	struct State savst;
	savst = st;
	loopcnt = 0;
	if(com)
	{
		mem_scope(envlist);
		if(execflg&EXECPR)
			on_option(EXECPR);
		else
			off_option(EXECPR);
		execflg &= ~EXECPR;
		cmdadr = com[0];
		trapcom[MAXTRAP] = 0;
		argsav = arg_new(com,&savargfor);
	}
	freturn = (jmp_buf*)retbuf;
	if((mode=setjmp(retbuf)) == 0)
	{
		states |= FUNCTION;
		if(fn_depth++ > MAXDEPTH)
			longjmp(*freturn,2);
		else
			execute(t,execflg);
	}
	fn_depth--;
	freturn = savreturn;
	if(com)
	{
		mem_unscope();
		arg_reset(argsav,savargfor);
		trapcom[MAXTRAP] = savtrap1;
		trapnote = 0;
	}
	savstandin = standin;
	st = savst;
	while((savstandin != standin) &&  pop(0));
	if(mode == 2)
	{
		if(fn_depth==0)
			failed(com[0],recursive);
		else
			longjmp(*freturn,2);
	}
	if(com && (trap=trapcom[0]) && (savtrap0!=trap))
	{
		trapcom[0] = savtrap0;
		execexp(trap,(FILE*)0);
		free(trap);
	}
}

#ifndef JOBS
/*
 * These routines have been moved to jobs.c when compiling with JOBS option
 */

/*
 * Initialize the process posting array
 */
void postclr()
{
	register struct process  *pw = pwlist;
	while(pw < &pwlist[maxpost])
		(pw++)->p_pid = 0;
	numpost=0;
	maxpost=0;
}

int	post(pcsid)
int 	pcsid;
{
	register struct process  *pw = pwlist;
	if(pcsid)
	{
		while(pw->p_pid)
			pw++;
		if(numpost >= MAXP-1)
			pw--;
		else
			numpost++;
		if(numpost > maxpost)
			maxpost = numpost;
		pw->p_pid = pcsid;
		if(numpost >= MAXP-1)
			await(0,0);
		return(pw-pwlist);
	}
	return(-1);
}

void	await(i, bckg)
int 	i, bckg;
{
	int 	rc=0, wx=0;
	int 	w;
	post(i);
	while(numpost)
	{
		register int 	p;
		register int 	sig;
		int 	w_hi;
		int found = 0;
		{
			register struct process *pw=pwlist;
			errno = 0;
			p=wait(&w);
			if((p== -1) && bckg && errno==EINTR)
				break;
			while(pw <= &pwlist[maxpost])
			{
				if(pw->p_pid==p)
				{
					if(p==cpid)
					{
						cpid = 0;
						cpipe[1] = NULL;
					}
					pw->p_pid=0;
					numpost--;
					found++;
				}
				else
					 pw++;
			}
		}
		if(p == -1)
		{
			if(bckg)
			{
				register struct process *pw =pwlist;
				while(pw <= &pwlist[maxpost] && i != pw->p_pid)
					pw++;
				if(i == pw->p_pid)
				{
					pw->p_pid = 0;
					numpost--;
				}
			}
			continue;
		}
		w_hi = (w>>8)&LOBYTE;
		if(sig = w&0177)
		{
			p_setout(stderr);
			if(sig == 0177	/* ptrace! return */)
			{
				fputs(ptrace,output);
				sig = w_hi;
			}
#ifdef apollo
			if(*sysmsg[sig])
#else
			if(sysmsg[sig])
#endif	/* apollo */
			{
				if(i!=p || (states&PROMPT)==0)
				{
					p_prp(itos(p),SP);
				}
				fputs(sysmsg[sig],output);
				if(w&HIGHBIT)
					fputs(coredump,output);
			}
			newline();
		}
		wx |= w;
		if(p == i)
		{
			rc = (sig ? sig|SIGFLG : w_hi);
			break;
		}
	}
	exitval=rc;
	exitset();
}
#endif	/* JOBS */

#ifdef ACCT
#include <acctdef.h>
#include <sys/acct.h>
#include <sys/times.h>

static int compress();

struct acct sabuf;
struct tms buffer;
extern long times();
static long	before;
static char *SHACCT; /* 0 environment variable SHACCT not set so never acct
		ptr to SHACCT value if set, so acct if shell procedure*/
static shaccton; /* 0 implies do not write record on exit
			  1 implies write acct record on exit
		*/
/*
 *	initialize accounting, i.e., see if SHACCT variable set
 */
void initacct()
{

	SHACCT = valup(ACCTNOD);
}
/*
* suspend accounting unitl turned on by preacct()
*/
void suspacct()
{
	shaccton=0;
}

int preacct(cmdname)
char	*cmdname;
{
	char * strncpy();
	if(SHACCT)
	{
		sabuf.ac_btime = time((long *)0);
		before = times(&buffer);
		sabuf.ac_uid = getuid();
		sabuf.ac_gid = getgid();
		strncpy(sabuf.ac_comm, (char*)simple(cmdname),
			sizeof(sabuf.ac_comm));
		shaccton = 1;
	}
}
#include <fcntl.h>
void	doacct()
{
	int	fd;
	long	after;

	if(shaccton)
	{
		after = times(&buffer);
		sabuf.ac_utime = compress(buffer.tms_utime + buffer.tms_cutime);
		sabuf.ac_stime = compress(buffer.tms_stime + buffer.tms_cstime);
		sabuf.ac_etime = compress( after - before);
		fd = open( SHACCT , O_WRONLY | O_APPEND | O_CREAT,0666);
		write(fd, &sabuf, sizeof( sabuf ));
		close( fd);
	}
}
 
/*
 * Produce a pseudo-floating point representation
 * with 3 bits base-8 exponent, 13 bits fraction.
 */
static int compress(t)
register time_t t;
{
	register int exp = 0, rund = 0;

	while (t >= 8192)
	{
		exp++;
		rund = t&04;
		t >>= 3;
	}
	if (rund)
	{
		t++;
		if (t >= 8192)
		{
			t >>= 3;
			exp++;
		}
	}
	return((exp<<13) + t);
}
#endif	/* ACCT */

