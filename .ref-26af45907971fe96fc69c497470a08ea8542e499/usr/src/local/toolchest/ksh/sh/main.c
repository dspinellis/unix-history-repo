/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)main.c	1.1 */
/*
 * UNIX shell
 *
 * S. R. Bourne
 * Rewritten by David Korn
 * AT&T Bell Laboratories
 *
 */

#ifdef BSD
# ifdef BSD_4_2
# include	<sys/param.h>
# else
# include	<sys/types.h>
# endif /* BSD_4_2 */
#include   	 <sgtty.h>
#else
#include	<sys/types.h>
#endif /* BSD */
#include	<sys/stat.h>
#include	"defs.h"
#include	"sym.h"
#include	"flags.h"
#include	"io.h"
#include	"history.h"
#include	"mode.h"
#include	"name.h"
#include	"stak.h"
#include	"timeout.h"
#include	"brkincr.h"
#include	"builtins.h"
#ifdef pdp11
#include	<execargs.h>
#endif	/* pdp11 */

#define FC_CHAR		'!'


/* These routines are defined by this module */
int	main();
void	chkpr();
char	*getpwd();

/* These routines are referenced by this module */
extern void	addblok();
extern void	assign();
extern FILE	*chkopen();
extern TREPTR	cmd();
extern void	done();
extern void	fassign();
extern char	*getpwd();
extern void	gscan_some();
extern int	hist_open();
extern void	hist_close();
extern void	hist_eof();
extern void	hist_flush();
extern void	initf();
extern char	*itos();
extern char	*mactry();
extern char	*movstr();
extern void	mem_reinit();
extern void	meminit();
extern FILE	*pathopen();
extern void	p_flush();
extern void	p_num();
extern void	p_str();
extern void	p_setout();
extern void	rmlocal();
extern char	*strrchr();
extern void	settemp();
extern char	*simple();
extern void	stdsigs();
extern void	tdystak();
extern char	*valup();

static void exfile();
static void pr_prompt();
static void chkmail();

extern char **environ;


/* The following is defined for fixcmd */

static struct stat lastmail;
static long mailtime;
static BOOL	beenhere = 0;
static int euserid;
static int egroupid;

main(c, v)
int 	c;
register char *v[];
{
	FILE fd;
	register char *sim;
	register int 	rsflag=1;	/* local restricted flag */
#ifdef apollo
	extern char pm_$unix_env ;
	extern FILE *_iobmax;
	pm_$unix_env = -1;
	_iobmax = _iob + 20;
#endif	/* apollo */
	standout = stdout;
	p_setout(stderr);
	standin = &stdfile;
	userid=getuid();
	euserid=geteuid();
	egroupid=getegid();
	time(&mailtime);
	stdsigs();
	/* initialize storage allocation */
	stakbot = 0;
	addblok((unsigned)0);
	/* set up memory for namenods */
	meminit();
	/* set names from userenv
	 *  'rsflag' is zero if SHELL variable is
	 * set in environment and contains an'r' in
	 * the simple file part of the value.
	 */

	rsflag=genenv();
	/* a shell is also restricted if argv(0) has
	 *  an 'rsh' for its simple name
	 */
	sim = simple(*v);
	if(*sim=='-')
	{
		sim++;
		login_sh = 2;
	}
	/* check for restricted shell */
	if(c>0 && gmatch(sim,"*rsh"))
		rsflag=0;
	/* look for options */
	/* dolc is $#	*/
	dolc=arg_opts(c,v);
	dolv=v+c-dolc--;
	dolv[0] = v[0];
	if(dolc < 1)
		on_option(STDFLG);
	if(is_option(STDFLG|CFLAG)==0)
	{
		dolc--;
		dolv++;
	}
	/* set[ug]id scripts run with the -p flag */
	if(userid!=euserid || getgid()!=egroupid)
	{
#ifdef BSD_4_2
		/* careful of #! setuid scripts with name beginning with - */
		if(login_sh && strcmp(v[0],v[1])==0)
			error(prohibited);
#endif /* BSD_4_2 */
		assign(PATHNOD,defpath);
		on_option(PRIVM);
	}
	if(is_option(RSHFLG))
	{
		rsflag = 0;
		off_option(RSHFLG);
	}
	/*
	 * return here for shell file execution
	 * but not for parenthesis subshells
	 */
	setjmp(subshell);
	cmdadr=dolv[0]; /* cmdadr is $0 */
	/* set pidname '$$' */
	movstr(itos(ppid=getpid()),pidadr);
	srand(ppid);
	ppid = getppid();
	settemp(pidadr);
	if((beenhere++)==0)
	{
		int prof = !is_option(PRIVM);
		/* decide whether shell is interactive */
		if(is_option(ONEFLG|CFLAG)==0 && is_option(STDFLG) && isatty(0) &&
				isatty(2))
			on_option(INTFLG);
		if(ppid==1)
			login_sh++;
		if(login_sh >= 2)
		{
			/* ? profile */
			login_sh += 2;
#ifdef JOBS
# ifdef BSD
			init_jobs(1);
# endif	/* BSD */
# ifdef SXT
			init_jobs(1);
# endif /* SXT */
#endif	/* JOBS */
			/*	system profile	*/
			if((input=pathopen(sysprofile)) != NULL)
				{
					exfile(TTYFLG);	/* file exists */
					fclose(input);
				}
			if(prof &&  (input=pathopen(mactry(profile))) != NULL)
			{
				exfile(TTYFLG);
				states &= ~TTYFLG;
				fclose(input);
			}
		}
		/* make sure PWD is set up correctly */
		if(getpwd(1))
			attrib(PWDNOD,N_EXPORT);
		if(prof)
		{
			if(sim = valup(ENVNOD))
				if(*(sim = mactry(sim)) == 0)
					sim = 0;
		}
		else
			sim = suid_profile;
		if(sim && (input = pathopen(sim)) != NULL)
		{
			exfile(TTYFLG);
			states &= ~TTYFLG;
			fclose(input);
		}
		if(rsflag==0)
			on_option(RSHFLG);
		/* open input file if specified */
		if(comdiv)
		{
			estabf(comdiv,&fd);
		}
		else
		{
			sim = cmdadr;
			cmdadr = v[0];
			if(is_option(STDFLG))
				input = stdin;
			else
			{
#ifdef VFORK
				char *sp = sim;
				/* avoid path search if we already have the
				 * path name in the environment
				 */
				if(strcmp(sim,simple(*environ))==0)
					sp = (*environ)+2;
#endif /* VFORK */
#ifdef SUID_EXEC
				/* open stream should have been passed into shell */
				if(gmatch(sim,devfdNN))
				{
					struct stat statb;
					int fd = atoi(sim+8);
					if(fstat(fd,&statb)<0)
						failed(cmdadr,badopen);
					input = fdopen(fd,"r");
					sim = cmdadr;
					off_option(EXECPR|READPR);
				}
				else
#endif /* SUID_EXEC */
#ifdef VFORK
				if((input=pathopen(sp))==NULL)
#else
				if((input=pathopen(sim))==NULL)
					/* for compatibility with bsh */
					if((input=chkopen(sim))==NULL)
#endif /* VFORK */
						failed(sim,badopen);
				/* eliminate local aliases and functions */
				gscan_some(rmlocal,alias,N_EXPORT,0);
				gscan_some(rmlocal,prnames,N_EXPORT,0);
			}
			cmdadr = sim;
			comdiv--;
#ifdef ACCT
			initacct();
			if(fileno(input) != 0)
				preacct(cmdadr);
#endif	/* ACCT */
		}
	}
#ifdef pdp11
	else
		*execargs=(char *) dolv;	/* for `ps' cmd */
#endif	/* pdp11 */
	states |= is_option(INTFLG);
	exfile(0);
	if(states&PROMPT)
	        newline();
	done(0);
}

static void	exfile(prof)
register int prof;
{
	long curtime;
	TREPTR t;
	register FILE *fp = input;
	register struct fixcmd *fixp;
	register int fno;
	unsigned execflags;
	/* move input */
	if(fisopen(fp) && (fno=fileno(fp))!=F_STRING)
	{
		if(fno > 0)
		{
			fp = input = frenumber(fp,INIO);
			setbuf(fp,(char*)_sibuf);
			if(prof==0)
				setbuf(stdin,NIL);
		}
		/* if stdin is a pipe it must be unbuffered */
		else
			setbuf(fp,ispipe(fp)?NIL:(char*)_sibuf);
	}
	if(states&INTFLG)
	{
		if(isnull(PS1NOD))
			assign(PS1NOD, (euserid?stdprompt:supprompt));
		states |= TTYFLG|PROMPT;
		ignsig(SIGTERM);
		hist_open();
		if(fc_fix)
			on_option(FIXFLG);
#ifdef JOBS
# ifdef BSD
		if(login_sh<=1)
			init_jobs(0);
# endif	/* BSD */
# ifdef SXT
		if(login_sh<=1)
			init_jobs(0);
# endif /* SXT */
#endif	/* JOBS */
	}
	else
	{
		if(prof)
			states |= prof;
		else
		{
			off_option(EDITVI|EMACS|GMACS);
			on_option(HASHALL|INPROC);
		}
		states &= ~(PROMPT|MONITOR);
		off_option(FIXFLG);
	}
	fixp = fc_fix;
	if(setjmp(errshell) && prof)
	{
		fclose(fp);
		return;
	}
	/* error return here */
	freturn = (jmp_buf*)errshell;
	loopcnt=peekc=peekn=0;
	iopend=0;
	linked = 0;
	if(fp!=NULL)
		initf(fp);
	if(feof(fp))
		goto eof_or_error;
	/* command loop */
	while(1)
	{
		tdystak((STKPTR)0);
		stakchk(); /* may reduce sbrk */
		exitset();
		states &= ~(ERRFLG|READPR|RWAIT|MONITOR);
		states |= is_option(READPR)|WAITING|ERRFLG;
		/* -eim  flags don't apply to profiles */
		if(prof)
			states &= ~(INTFLG|ERRFLG|MONITOR);
		p_setout(stderr);
		if((states&PROMPT) && standin->fstak==0 && !feof(fp))
		{
			register char *mail;
#ifdef JOBS
			/* allow children to be stopped*/
			states &= ~(MONITOR|NONSTOP);
			states |= is_option(MONITOR);
			list_jobs(N_FLAG);
#endif	/* JOBS */
			if((mail=valup(MAILPNOD)) || (mail=valup(MAILNOD)))
			{
				time(&curtime);
				if ((curtime - mailtime) >= mailchk)
				{
					chkmail(mail);
					mailtime = curtime;
				}
			}
			if(fixp)
				hist_eof();
			pr_prompt(mactry(valup(PS1NOD)));
			/* sets timeout for command entry */
#if TIMEOUT!=0
			if(timeout <= 0 || timeout > TIMEOUT)
				timeout = TIMEOUT;
#endif
			if(timeout>0)
				alarm((unsigned)timeout);
			standin->flin = 1;
		}
		trapnote=0;
		peekc=readc();
		if(feof(fp) || ferror(fp))
		{
		eof_or_error:
			if((states&PROMPT) && standin->fstak==0 && ferror(fp)==0) 
			{
				clearerr(fp);
				if(is_option(NOEOF) && !ferror(output))
				{
					p_str(logout,NL);
					continue;
				}
#ifdef JOBS
				else if(close_jobs()<0)
					continue;
#endif	/* JOBS */
				hist_close();
			}
			return;
		}
		if(timeout>0)
			alarm(0);
		if((states&PROMPT) && fixp)
		{
			hist_eof();
			putc(peekc,fixp->fixfd);
		}
		states |= is_option(FIXFLG);
		states &= ~ WAITING;
		t = cmd(NL,MTFLG);
		if(fixp)
			hist_flush();
		if(t)
		{
			execflags = ERRFLG;
			/* sh -c simple-command may not have to fork */
			if(prof==0 && is_option(CFLAG) && (t->tretyp&COMMSK)==TCOM
				&& nextchar(fp)==0)
			{
				execflags |= 1;
			}
			execute(t,execflags);
		}
	}
}

/*
 * if there is pending input, the prompt is not printed.
 * prints PS2 if flag is zero otherwise PS3
 */

void	chkpr(flag)
register int flag;
{
	if(flag || (states&PROMPT) && standin->fstak==0)
	{
		/* if characters are in input buffer don't issue prompt */
		if((flag?stdin:input)->_cnt > 0)
			return;
		p_setout(stderr);
		fputs(valup(flag?PS3NOD:PS2NOD),output);
	}
}



/* prints out messages if files in list have been modified since last call */
static void chkmail(files)
char *files;
{
	register char *cp,*sp,*qp;
	register char save;
	struct stat	statb;
	if(*(cp=files) == 0)
		return;
	sp = cp;
	do
	{
		/* skip to : or end of string saving first '?' */
		for(qp=0;*sp && *sp != ':';sp++)
			if((*sp == '?' || *sp=='%') && qp == 0)
				qp = sp;
		save = *sp;
		*sp = 0;
		/* change '?' to end-of-string */
		if(qp)
			*qp = 0;
		gchain = NULL;
		do
		{
			/* see if time has been modified since last checked
			 * and the access time <= the modification time
			 */
			if(stat(cp,&statb) >= 0 && statb.st_mtime >= mailtime
				&& statb.st_atime <= statb.st_mtime)
			{
				/* check for directory */
				if(gchain==NULL && (statb.st_mode&S_IFMT)==S_IFDIR) 
				{
					/* generate list of directory entries */
					f_complete(cp,"/*");
				}
				else
				{
					/*
					 * If the file has shrunk,
					 * or if the size is zero
					 * then don't print anything
					 */
					if(statb.st_size &&
						(  statb.st_ino != lastmail.st_ino
						|| statb.st_dev != lastmail.st_dev
						|| statb.st_size > lastmail.st_size))
					{
						/* save and restore $_ */
						char *save = lastarg;
						lastarg = cp;
						p_str(mactry(qp==0?mailmsg:qp+1),NL);
						lastarg = save;
					}
					lastmail = statb;
					break;
				}
			}
			if(gchain)
			{
				cp = gchain->argval;
				gchain = gchain->argchn;
			}
			else
				cp = NULL;
		}
		while(cp);
		if(qp)
			*qp = '?';
		*sp++ = save;
		cp = sp;
	}
	while(save);
	tdystak((STKPTR)0);
}

/*
 * print the primary prompt
 */

static void pr_prompt(string)
register char *string;
{
	register char *cp;
	register int c;
	static short cmdno;
#ifdef BSD
	int mode;
#include	<sys/ioctl.h>
	mode = LFLUSHO;
	ioctl(fileno(output),TIOCLBIC,&mode);
#endif	/* BSD */
	p_flush();
	p_setout(stderr);
	for(cp=string;c= *cp;cp++)
	{
		if(c==FC_CHAR)
		{
			/* look at next character */
			c = *++cp;
			/* print out line number if not !! */
			if(c!= FC_CHAR)
				p_num(fc_fix?fc_fix->fixind:++cmdno,0);
			if(c==0)
				break;
		}
		putc(c,output);
	}
#if VSH || ESH
	*output->_ptr = 0;
	/* prompt flushed later */
#else
	p_flush();
#endif
}

/* make sure PWD is set up correctly */

/*
 * Return the value of the PWD variable
 * Invokes /bin/pwd if necessary
 * If mode non-zero, then PWD must have same device/inode as dot
 */

char *getpwd(mode)
{
	register char *sim;
	register int count = 0;
	while((sim=valup(PWDNOD))==NULL || *sim==0 || (mode&&!eq_inode(sim,dot)))
	{
		if(count++ > 0)
			return(NULL);
		if((sim=valup(HOME)) && *sim=='/' && eq_inode(sim,dot))
		{
			fassign(PWDNOD,sim);
			break;
		}
		else
		{
#ifdef apollo
			char buff[BUFSIZ+1];
			extern char *getcwd();
			sim=getcwd(buff+1,BUFSIZ);
			if(buff[1]=='/' && buff[2]=='/')
			{
				buff[0]='/';
				buff[1] = 'n';
				sim = buff;
			}
			fassign(PWDNOD,sim);
#else
			/* even restricted shells can pwd */
			optflag savflags = flags;
			off_option(RSHFLG);
			execexp(setpwd,(FILE*)0);
			flags = savflags;
#endif	/* apollo */
		}
	}
	return(sim);
}


/*
 * This version of access checks against effective uid/gid
 */

access(name, mode)
register char	*name;
register int mode;
{	
	struct stat statb;
	if (stat(name, &statb) == 0)
	{
		if(mode == 0)
			return(mode);
		else if(euserid == 0)
		{
			if((statb.st_mode&S_IFMT) != S_IFREG || mode != 1)
				return(0);
		    	/* root needs execute permission for someone */
			mode = (S_IEXEC|(S_IEXEC>>3)|(S_IEXEC>>6));
		}
		else if(euserid == statb.st_uid)
			mode <<= 6;
		else if(egroupid == statb.st_gid)
			mode <<= 3;
#ifdef BSD
# ifdef BSD_4_2
		/* in BSD_4_2 you can be in several groups */
		else
		{
			int groups[NGROUPS];
			register int n;
			n = getgroups(NGROUPS,groups);
			while(--n >= 0)
			{
				if(groups[n] == statb.st_gid)
				{
					mode <<= 3;
					break;
				}
			}
		}
# endif /* BSD_4_2 */
#endif /* BSD */
		if(statb.st_mode & mode)
			return(0);
	}
	return(-1);
}

