/* @(#)builtin.c	1.1 */

/*
 *  builtin routines for the shell
 *
 *   David Korn
 *   AT&T Bell Laboratories
 *   Room 5D-112
 *   Murray Hill, N. J. 07974
 *   Tel. x7975
 *
 */

#include	"defs.h"
#include	"sym.h"
#include	"io.h"
#include	"history.h"
#include	"builtins.h"
#include	"flags.h"
#include	"name.h"
#include	"mode.h"
#include	"brkincr.h"
#include	"shtype.h"
#include	"stak.h"
#ifdef JOBS
#include	"jobs.h"
#endif	/* JOBS */

#ifdef BSD_4_2
#include	<sys/time.h> 	/* needed for ulimit */
#include	<sys/resource.h>/* needed for ulimit */
#define	LIM_FSIZE	RLIMIT_FSIZE
#define	LIM_DATA	RLIMIT_DATA
#define	LIM_STACK	RLIMIT_STACK
#define	LIM_CORE	RLIMIT_CORE
#define	LIM_CPU		RLIMIT_CPU
#define	LIM_MAXRSS	RLIMIT_RSS
#define	INFINITY	RLIM_INFINITY
#endif	/* BSD_4_2 */

/* This module defines these routines */
void	builtin();
int	execexp();

/* This module references these external routines */
extern long	aeval();
extern void	arg_set();
extern void	await();
extern void	cannon_path();
extern char	*catpath();
extern NAMPTR	checkfor();
extern FILE	*chkopen();
extern void	clrsig();
extern TREPTR	cmd();
extern void	do_whence();
extern void	done();
#ifdef ECHO_N
extern char	*echo_mode();
#endif	/* ECHO_N */
extern int	estabf();
extern void	execa();
extern void	exitsh();
extern void	failed();
extern void	fassign();
extern void	free();
extern char	*getpwd();
extern void	getsig();
extern void	gscan_all();
extern char	*heap();
extern void	hist_cancel();
extern void	hist_close();
extern histloc	hist_find();
extern void	hist_flush();
extern long	hist_list();
extern void	hist_open();
extern void	hist_subst();
extern long	hist_position();
extern void	initf();
extern char	*movstr();
extern void	oldsigs();
extern void	p_flush();
extern void	p_list();
extern void	p_num();
extern void	p_setout();
extern void	p_str();
extern void	p_time();
extern FILE	*pathopen();
extern void	pipe_close();
extern void	printflg();
extern void	prinscan();
extern int	printnam();
extern char	*realias();
extern char	*strcat();
extern char	*strchr();
extern char	*strcpy();
extern char	*substitute();
extern FILE	*tmp_open();
extern void	trace_command();
extern void	unassign();
extern char	*utos();
extern char	*valup();

static int flagset();
static char *cmd_name;
static int sig_number();
static char *mycom[2];
#ifdef JOBS
static void sig_list();
#endif	/* JOBS */

void builtin(xbuiltin, argn, com,t)
int	argn;
register char	*com[];
TREPTR	t;
{
	register char *a1 = com[1];
	register int flag = 0;
	struct Amemory *troot;
	int scoped = 0;
	int aflag;
	cmd_name = com[0];
	switch(xbuiltin)
	{
		case SYSEXEC:
			com++;
			ioset = 0;
			if(a1==0)
				break;

		case SYSLOGIN:
	
			if(is_option(RSHFLG))
				failed(cmd_name,restricted);
			else
			{
#ifdef JOBS
				if(close_jobs() < 0)
				{
					exitval=1;
					break;
				}
#endif /* JOBS */
				/* force bad exec to terminate shell */
				states &= ~(TTYFLG|BUILTIN);
				oldsigs();
				hist_close();
				rmtemp(0);
				execa(com,(ARGPTR)NULL);
				done(0);
			}

		case SYSTEST:	/* test	expression */
			exitval = testfn(argn, com);
			break;


		case SYSPWD:	/* pwd routine */
			argn = 1;
			if((*mycom = getpwd(1))==NULL)
				failed(cmd_name,pwderr);
			com = mycom-1;
			
		case SYSECHO:	/* system V echo routine */
			/* equivalent to print - */
			com--;
			argn++;
#ifdef ECHO_N
			/* This mess is because /bin/echo on BSD is archaic */
			a1 = echo_mode();
#else
			a1 = minus;
#endif	/* ECHO_N */

		case SYSPRINT:	/* print routine */
		{
			register FILE *fd;
			int raw = 0;
			wdnum =  1;
			while(a1 && *a1 == '-')
			{
				int c = *(a1+1);
				/* handle the -R flag for BSD style echo */
				if(flag&R_JUST)
				{
					if(strcmp(a1,"-n")==0)
						c = 0;
					else
						break;
				}
				flag |= flagset(a1,~(N_FLAG|R_FLAG|P_FLAG|U_FLAG|S_FLAG|R_JUST));
				com++;
				argn--;
				if(c==0)
					break;
				a1 = com[1];
			}
			wdnum %= 10;
			if(flag&(R_FLAG|R_JUST))
				raw = 1;
			if(flag&S_FLAG)
			{
				/* print to history file */
				hist_open();
				if(fc_fix==NULL)
					failed(cmd_name,nohistory);
				fd = fc_fix->fixfd;
				states |= FIXFLG;
				goto skip;
			}
			else if(flag&P_FLAG)
			{
				if((fd=cpipe[OTPIPE])==NULL)
					failed(cmd_name,noquery);
			}
			else if(flag&U_FLAG)
			{
				fd = file_fd(wdnum);
				if(!fiswrite(fd))
					failed(cmd_name,badfile);
			}
			else	
				fd = standout;

			clearerr(fd);
			p_setout(fd);
		skip:
			if(echo_list(raw,com+1,fd) && (flag&N_FLAG)==0)
				putc(NL,fd);
			if(flag&S_FLAG)
				hist_flush();
			break;
		}

		case SYSLET:
		{
			if(argn < 2)
				failed(cmd_name,argcount);
			while(--argn)
				exitval = !aeval(*++com);
			break;
		}

		/*
		 * The following few builtins are provided to set,print,
		 * and test attributes and variables for shell variables,
		 * aliases, and functions.
		 * In addition, typeset -f can be used to test whether a
		 * function has been defined or to list all defined functions
		 * Note readonly is same as typeset -r.
		 * Note export is same as typeset -x.
		 */
		case SYSRDONLY:
			flag = R_FLAG;
			aflag = '-';
			goto typset;

		case SYSXPORT:
			flag = X_FLAG;
			aflag = '-';
			goto typset;

		case SYSALIAS:
		case SYSTYPESET:
		{
			FILE *fd;
			int type;	/* 0 for typeset, non-zero for alias */
			flag = 0;
			aflag = (a1?*a1:0);
			wdnum = 0;
			if(aflag == '-' || aflag == '+')
			{
				flag = flagset(a1,~(L_JUST|R_JUST|Z_FILL|INT_GER|L_TO_U
					|U_TO_L|X_FLAG|R_FLAG|F_FLAG|P_FLAG|T_FLAG
					|A_FLAG));
				com++;
			}
			if((flag&INT_GER) && (flag&(L_JUST|R_JUST|Z_FILL)))
				failed(cmd_name,badopt);
			/* S_FLAG forces name to be in newest scope */
			if(fn_depth)
				scoped = S_FLAG;

		typset:
			type = 0;
			if(xbuiltin == SYSALIAS)
			{
				if(flag&~(N_EXPORT|T_FLAG))
					failed(cmd_name,badopt);
				troot = alias;
				/* setname treats this value specially */
				type = V_FLAG;
			}
			else if(flag&F_FLAG)
			{
				if(flag&~(N_EXPORT|F_FLAG|T_FLAG))
					failed(cmd_name,badopt);
				troot = prnames;
				flag &= ~F_FLAG;
			}
			else
				troot = namep;
			if(flag&P_FLAG)
			{
				flag &= ~P_FLAG;
				if((fd=cpipe[OTPIPE])==NULL)
					failed(cmd_name,noquery);
			}
			else
				fd = standout;
			p_setout(fd);
			if(aflag == 0)
			{
				if(type)
					prinscan(fd,0,troot,0);
				else
					gscan_all(printflg,troot);
				break;
			}
			if(com[1])
			{
				while(a1 = *++com)
				{
					register unsigned newflag;
					register struct Namnod *np;
					struct Namnod  *setname();
					unsigned curflag;
					if(troot == prnames)
					{
						/*
						 *functions can be exported or
						 * traced but not set
						 */
						if(np=checkfor(a1,prnames))
						{
							if((flag&(N_EXPORT|T_FLAG))==0)
							{
								printnam(np,0);
								continue;
							}
							if(aflag=='-')
								attrib(np,flag);
							else if(aflag=='+')
								pattrib(np,~flag);
						}
						else
							exitval++;
						continue;
					}
					np = setname (a1,(type|scoped));
					/* tracked alias */
					if(type && (flag&T_FLAG) && aflag=='-')
					{
						attrib(np,flag|N_EXPORT);
						realias(np);
						continue;
					}
					if(flag==0 && aflag!='-' && strchr(a1,'=') == NULL)
					{
						/* type==0 for TYPESET */
						if(type==0)
						{
							if(valup(np))
								printflg(np);
							else
								exitval++;
							continue;
						}
						if(printnam(np,0) == 0)
						{
							fputs(a1,fd);
							p_str(noalias,NL);
							exitval++;
						}
						continue;
					}
					curflag = namflag(np);
					if (aflag == '-')
					{
						newflag = curflag | flag;
						if (flag & INT_GER)
							newflag &= ~(R_JUST|L_JUST);
						else if (flag & (L_JUST|R_JUST))
						{
							newflag &= ~INT_GER;
							if (flag & L_JUST)
								newflag &= ~R_JUST;
							else
								newflag &= ~L_JUST;
						}
						if (flag & U_TO_L)
							newflag &= ~L_TO_U;
						else if (flag & L_TO_U)
							newflag &= ~U_TO_L;
					}
					else
						newflag = curflag & ~flag;
					if (aflag && (wdnum>0 || (curflag!=newflag)))
					{
#ifdef apollo
						/* keep aliases from going
						   into environment */
						if(type)
							namflag(np) = newflag;
						else
							chattrib (np, newflag,wdnum);
#endif /* apollo */
						chattrib (np, newflag,wdnum);
					}
				}
			}
			else
				prinscan(fd,flag,troot,aflag=='+');
			break;
		}


		/*
		 * The removing of Shell variable names, aliases, and functions
		 * is performed here.
		 * Unset functions with unset -f
		 * Non-existent items being deleted give non-zero exit status
		 */
		case SYSUNALIAS:
		case SYSUNSET:
		{
			register NAMPTR np;
#ifdef apollo
			short namlen;
#endif /* apollo */
			if(xbuiltin == SYSUNALIAS)
			{
				troot = alias;
				goto unall;
			}
			if(a1 && *a1 == '-')
			{
				flag = flagset(a1,~F_FLAG);
				com++;
				argn--;
				troot = prnames;
			}
			else
				troot = namep;
		unall:
			if(argn < 2)
				failed(cmd_name,argcount);
			while(--argn)
			{
				a1 = *++com;
				np=checkfor(a1,troot);
				if(np)
				{
					if(troot==namep)
					{
						if(attest(np,ARRAY) && (
							(a1=strchr(a1,']'))==NIL
							|| astchar(a1[-1])))
							arayp(np)->adot = NO_SUBSCRIPT;
						if (attest (np, N_RDONLY))
							failed(np->namid,wtfailed);
#ifdef apollo
						namlen =strlen(np->namid);
						ev_$delete_var(np->namid,&namlen);
#endif /* apollo */
					}
					unassign(np);
				}
				else
					exitval = 1;
			}
			break;
		}

		case SYSDOT:
			if(a1)
			{
				register FILE *f;
				if((f=pathopen(a1)) == NULL)
					failed(a1,notfound);
				else
				{
					if(argn > 2)
						arg_set(com+1);
					execexp((char*)0,f);
				}
			}
			break;
	
		case SYSTIMES:
		{
			long int t[4];
			times(t);
			p_setout(standout);
			p_time(t[0],' ');
			p_time(t[1],NL);
			p_time(t[2],' ');
			p_time(t[3],NL);
			break;
		}
		
		case SYSRETURN:	/* return from a subroutine */
			if(freturn)
			{
				exitval = (a1?atoi(a1):oldexit);
				longjmp(*freturn,1);
			}

		case SYSEXIT:
#ifdef JOBS
			if(close_jobs()<0)
				break;
#endif
			states &= ~(TTYFLG|BUILTIN);	/* force exit */
			exitsh(a1?atoi(a1):oldexit);
		
		case SYSNULL:
			break;
		
		case SYSCONT:
			if(loopcnt)
			{
				execbrk = breakcnt = 1;
				if(a1)
					breakcnt = atoi(a1);
				if(breakcnt > loopcnt)
					breakcnt = loopcnt;
				else
					breakcnt = -breakcnt;
			}
			break;
		
		case SYSBREAK:
			if(loopcnt)
			{
				execbrk = breakcnt = 1;
				if(a1)
					breakcnt = atoi(a1);
				if(breakcnt > loopcnt)
					breakcnt = loopcnt;
			}
			break;
		
		case SYSTRAP:
			if(a1)
			{
				register BOOL	clear;
				char *action = a1;
				if((clear=isdigit(*a1))==0)
				{
					++com;
					if(*a1=='-')
						clear++;
				}
				while(a1 = *++com)
				{
					flag = sig_number(a1);
					if(flag>MAXTRAP || flag<MINTRAP)
						failed(a1,badtrap);
					else if(clear)
						clrsig(flag);
					else
					{
						free(trapcom[flag]);
						trapcom[flag] = heap(action);
						if(*action)
							getsig(flag);
						else
							ignsig(flag);
					}
				}
			}
			else /* print out current traps */
			{
				p_setout(standout);
				for(flag=0; flag<=MAXTRAP; flag++)
					if(trapcom[flag])
					{
						p_num(flag,':');
						p_str(trapcom[flag],NL);
					}
			}
			break;
		
		case SYSCD:
		{
			register char *dp;
			register char *cdpath = nullstr;
			char newdir[256];	/* enough for any pathname */
			char *oldpwd;
			if(is_option(RSHFLG))
				failed(cmd_name,restricted);
			else if(argn >3)
				failed(cmd_name, argcount);
			if(argn==3)
				a1 = substitute(getpwd(0),a1,com[2],newdir);
			else if(a1==0 || *a1==0)
				a1 = valup(HOME);
			else if(*a1 == '-' && *(a1+1) == 0)
				a1 = valup(OLDPWDNOD);
			if(a1==0 || *a1==0)
				failed(cmd_name,argn==3?badsub:baddir);
			if(*a1 != '/')
				cdpath = valup(CDPNOD);
			if(cdpath==0)
				cdpath = nullstr;
			if(*a1=='.')
			{
				/* test for pathname . ./ .. or ../ */
				if(*(dp=a1+1) == '.')
					dp++;
				if(*dp==0 || *dp=='/')
					cdpath = nullstr;
			}
			do
			{
				dp = cdpath;
				cdpath=catpath(dp,a1);
			}
			while((flag=chdir(curstak()))<0 && cdpath);
			if(flag<0)
				failed(a1,baddir);
			if(a1 == valup(OLDPWDNOD) || argn==3)
				dp = a1;	/* print out directory for cd - */
			a1 = (char*)fixstak();
			if(*dp && *dp!= ':' && (states&PROMPT) && strchr(a1,'/'))
			{
				p_setout(standout);
				p_str(a1,NL);
			}
			oldpwd = getpwd(0);
			fassign(OLDPWDNOD,oldpwd);
			if(*a1 == '/')
				strcpy(newdir,a1);
			else
			{
				dp = movstr(oldpwd,newdir);
				*dp++ = '/';
				movstr(a1,dp);
			}
			/* eliminate redundant / */
			a1 = newdir;
			cannon_path(a1);
			fassign(PWDNOD,a1);
#ifndef INT16
			/* Because of possible symbolic links, make sure we are where
			 * we think we are.
			 */
			if(!eq_inode(dot,a1))
				chdir(a1);
#endif	/* INT16 */
			break;
		}
		
		case SYSSHFT:
		{
			flag = (a1?aeval(a1):1);
			if(flag<0 || dolc<flag)
				failed(cmd_name,badnum);
			else
			{
				dolv += flag;
				dolc -= flag;
			}
			break;
		}
		
		case SYSWAIT:
			await(a1?atoi(a1):-1,1);
			break;
		
		case SYSREAD:
		{
			register FILE *fd;
			wdnum = 0;
			while(a1 && *a1 == '-')
			{
				flag |= flagset(a1,~(R_FLAG|P_FLAG|U_FLAG|S_FLAG));
				com++;
				argn--;
				if(*(a1+1)==0)
					break;
				a1 = com[1];
			}
			if(flag&P_FLAG)
			{
				fd = cpipe[INPIPE];
				states |= PROMPT;
			}
			else if(flag&U_FLAG)
				fd = file_fd(wdnum);
			else
				fd = stdin;
			if(!fisread(fd))
				failed(cmd_name,badfile);
			if(isatty(fileno(fd)))
			{
				FILE *fdo;
				p_setout(output);
				states |= PROMPT;
				/* look for prompt */
				if(a1=strchr(a1,'?'))
				{
					if(fiswrite(fd)==0 ||
					(fdo=fdopen(dup(fileno(fd)),"w"))==NULL)
						fdo = stderr;
					p_setout(fdo);
					fputs(a1+1,fdo);
					if(fdo!=stderr)
						fclose(fdo);
				}
			}
			readvar(&com[1],fd,flag&(R_FLAG|S_FLAG));
			if(feof(fd))
			{
				exitval=1;
				if(flag&P_FLAG)
				{
					pipe_close(cpipe);
					cpipe[INPIPE]=0;
					cpid = 0;
				}
			}
			clearerr(fd);
			break;
		}
	
		case SYSSET:
			flag = is_option(EXECPR);
			if(a1)
			{
				register int argc;
				argc = arg_opts(argn,com);
				/* RWAIT is set if -- flag is given */
				if(argc>1 || (states&RWAIT))
					arg_set(com+argn-argc);
				states &= ~(RWAIT|READPR|MONITOR);
				states |= is_option(READPR|MONITOR);
			}
			if(flag)
				trace_command(com);
			if(a1==0 && ((COMPTR) t)->comset==0)
				/*scan name chain and print*/
				prinscan(standout,0,namep,0);
			break;
		
		case SYSEVAL:
			if(a1)
				execexp(a1,(FILE*)&com[2]);
			break;

		case SYSFC:
		{
			register struct fixcmd *fp;
			FILE *fdo;
			char *argv[2];
			char fname[TMPSIZ];
			int index2;
			int indx = -1; /* used as subscript for range */
			char *edit = NULL;		/* name of editor */
			char *replace = NULL;		/* replace old=new */
			int incr;
			int range[2];	/* upper and lower range of commands */
			int lflag = 0;
			int nflag = 0;
			int rflag = 0;
			histloc location;
			wdnum = 0;
			hist_open();
			if((fp=fc_fix)==NULL)
				failed(cmd_name,nohistory);
			while((a1=com[1]) && *a1 == '-')
			{
				flag = flagset(a1,~(E_FLAG|L_FLAG|N_FLAG|R_FLAG));
				if(flag==0)
				{
					range[++indx] = fp->fixind - wdnum-1;
					wdnum = 0;
					if(indx==1)
						break;
				}
				else
				{
					if(flag&E_FLAG)
					{
						/* name of editor specified */
						com++;
						if((edit=com[1]) == NULL)
							failed(cmd_name,argexp);
					}
					if(flag&N_FLAG)
						nflag++;
					if(flag&L_FLAG)
						lflag++;
					if(flag&R_FLAG)
						rflag++;
				}
				com++;
			}
			flag = indx;
			while(flag<1 && (a1=com[1]))
			{
				if(isdigit(*a1) || *a1 == '-')
				{
					/* see if completely numeric */
					do	a1++;
					while(isdigit(*a1));
					if(*a1==0)
					{
						a1 = com[1];
						range[++flag] = atoi(a1);
						if(*a1 == '-')
							range[flag] += (fp->fixind-1);
						com++;
						continue;
					}
				}
				/* look for old=new argument */
				else if(replace==NULL && strchr(a1+1,'='))
				{
					replace = a1;
					com++;
					continue;
				}
				/* search for last line starting with string */
				location = hist_find(com[1],fp->fixind-1,0,-1);
				if((range[++flag] = location.his_command) < 0)
					failed(com[1],notfound);
				com++;
			}
			if(flag <0)
			{
				/* set default starting range */
				if(lflag)
				{
					flag = fp->fixind-16;
					if(flag<1)
						flag = 1;
				}
				else
					flag = fp->fixind-2;
				range[0] = flag;
				flag = 0;
			}
			if(flag==0)
				/* set default termination range */
				range[1] = (lflag?fp->fixind-1:range[0]);
			if((index2 = fp->fixind - fp->fixmax) <=0)
			index2 = 1;
			/* check for valid ranges */
			for(flag=0;flag<2;flag++)
				if(range[flag]<index2 ||
					range[flag]>=(fp->fixind-(lflag==0)))
					failed(cmd_name,badnum);
			if(edit && *edit=='-' && range[0]!=range[1])
				failed(cmd_name,badnum);
			/* now list commands from range[rflag] to range[1-rflag] */
			incr = 1;
			flag = rflag>0;
			if(range[1-flag] < range[flag])
				incr = -1;
			if(lflag)
			{
				fdo = standout;
				a1 = "\n\t";
			}
			else
			{
				fdo = tmp_open(fname);
				a1 = "\n";
				nflag++;
			}
			p_setout(fdo);
			while(1)
			{
				if(nflag==0)
					p_num(range[flag],'\t');
				else if(lflag)
					putc('\t',fdo);
				hist_list(hist_position(range[flag]),EOF,a1);
				if(lflag && (trapnote&SIGSET))
					exitsh(SIGFAIL);
				if(range[flag] == range[1-flag])
					break;
				range[flag] += incr;
			}
			fseek(fp->fixfd,0L,2);
			if(lflag)
				return;
			p_setout(stderr);
			a1 = edit;
			if(a1==NULL && (a1=valup(FCEDNOD)) == NULL)
				a1 = defedit;
			if(*a1 != '-')
			{
				argv[0] =  fname;
				argv[1] = NULL;
				execexp(a1,(FILE*)argv);
			}
			closefd(fdo);
			fdo = chkopen(fname);
			unlink(fname);
			/* don't history fc itself */
			hist_cancel();
			states |= (READPR|FIXFLG);	/* echo lines as read */
			if(replace!=NULL)
				hist_subst(cmd_name,fdo,replace);
			else if(exitval == 0)
				 execexp((char*)0,fdo);
			else
			{
				fclose(fdo);
				if(is_option(READPR)==0)
					states &= ~(READPR|FIXFLG);
			}
			break;
		}

		case SYSWHENCE:
		{
			if(a1 && *a1 == '-')
			{
				flag = flagset(a1,~V_FLAG);
				com++;
				argn--;
			}
			if(argn < 2)
				failed(cmd_name,argcount);
			p_setout(standout);
			do_whence(com,flag);
			break;
		}


		case SYSUMASK:
		{
			if(a1)
			{
				register int c;	
				flag = 0;
				while(c = *a1++)
				{
					if (c>='0' && c<='7')	
						flag = (flag<<3) + (c-'0');	
					else
						failed(cmd_name,badnum);
				}
				umask(flag);	
			}	
			else
			{
				p_setout(standout);
#ifdef pdp11
				a1 = utos((long)(argn=umask(0)),8);
#else
				a1 = utos((unsigned long)(argn=umask(0)),8);
#endif /* pdp11 */
				umask(argn);
				*++a1 = '0';
				p_str(a1,NL);
			}
			break;
		}

#ifndef apollo

#ifdef BSD
#define BLK_SIZ		512
#define KBYTE		1024
#ifdef BSD_4_2
#else
#include	<sys/vlimit.h>
#endif
		case SYSULIMIT:
		{
#ifdef BSD_4_2
			struct rlimit rlp;
#endif
			long i;
			char *opts = 0;
			char label=0;
			int n;
			int unit = BLK_SIZ;
			flag = LIM_FSIZE;
			if(a1 && *a1 == '-')
				opts = ++a1;
			do
			{
				if(opts)
				{
					switch(*opts)
					{
						case 'a':
							label++;
							opts = "tmdsfc";
						case 't':
							flag = LIM_CPU;
							unit = 1;
							n = 0;
							break;
						case 'c':
							flag = LIM_CORE;
							n = 5;
							break;
						case 'f':
							flag = LIM_FSIZE;
							n = 4;
							break;
						case 'd':
							flag = LIM_DATA;
							unit = KBYTE;
							n = 2;
							break;
						case 's':
							flag = LIM_STACK;
							unit = KBYTE;
							n = 3;
							break;
						case 'm':
							flag = LIM_MAXRSS;
							unit = KBYTE;
							n = 1;
							break;
						default:
							failed(cmd_name,badopt);
					}
					if((a1 && *++a1)||(label&&com[2]))
						failed(cmd_name,badopt);
					a1 = com[2];
				}
				if(a1)
				{
					if((i=aeval(a1)) < 0)
						failed(cmd_name,badnum);
					i *= unit;
#ifdef 	BSD_4_2
					if(getrlimit(flag,&rlp) <0)
						failed(cmd_name,badnum);
					rlp.rlim_cur = i;
					if(setrlimit(flag,&rlp) <0)
						failed(cmd_name,badnum);
#endif
				}
				else
				{
#ifdef 	BSD_4_2
				if(getrlimit(flag,&rlp) <0)
						failed(cmd_name,badnum);
					i = rlp.rlim_cur;
#else
					i = -1;
				}
				if((i=vlimit(flag,i)) < 0)
					failed(cmd_name,badnum);
				if(a1==0)
				{
#endif
					p_setout(standout);
					if(label)
						p_str(limit_names[n],SP);
					if(i!=INFINITY)
					{
						i = (i+unit-1)/unit;
						p_str(utos((unsigned long)i,10),NL);
					}
					else
						p_str(unlimited,NL);
				}
			}
			while(opts && *++opts);
			break;
		}
#else
		case SYSULIMIT:
		{
#ifndef VENIX
			long i;
			long ulimit();
			register int mode = 2;
			if(a1 && *a1 == '-')
			{
#ifdef RT
				flag = flagset(a1,~(F_FLAG|P_FLAG));
#else
				flag = flagset(a1,~F_FLAG);
#endif /* RT */
				a1 = com[2];
			}
			if(flag&P_FLAG)
				mode = 5;
			if(a1)
			{
				if((i=aeval(a1)) < 0)
					failed(cmd_name,badnum);
			}
			else
			{
				mode--;
				i = -1;
			}
			if((i=ulimit(mode,i)) < 0)
				failed(cmd_name,badnum);
			if(a1==0)
			{
				p_setout(standout);
# ifdef pdp11
				p_str(utos((long)i,10),NL);
# else
				p_str(utos((unsigned long)i,10),NL);
# endif /* pdp11 */
			}
#endif	/* VENIX */
			break;
		}
#endif
#endif	/* apollo */

#ifdef JOBS
# if BSD || SXT
		case SYSBG:
			flag = 1;
		case SYSFG:
			if((states&MONITOR)==0)
			{
				exitval = 1;
				if(states&PROMPT)
					failed(cmd_name,j_no_jctl);
				break;
			}
			if(argn==1)
				a1 = nullstr;
			if(switch_jobs(a1,flag)==0)
				failed(cmd_name,j_no_job);
			break;
# endif

		case SYSJOBS:
			if(a1 && *a1 == '-')
				flag = flagset(a1,~L_FLAG);
			list_jobs(flag);
			break;

		case SYSKILL:
		{
			if(argn < 2)
				failed(cmd_name,argcount);
			/* just in case we send a kill -9 $$ */
			p_flush();
			flag = 15;
			if(*a1 == '-')
			{
				a1++;
				if(*a1 == 'l')
				{
					sig_list();
					break;
				}
				flag = sig_number(a1);
				if(flag <0 || flag >= NSIG)
					failed(cmd_name,badopt);
				com++;
				argn--;
			}
			while(--argn)
			{
				a1 = *++com;
				exitval += job_kill(a1,flag);
			}
			break;
		}
#endif
		
#ifdef apollo
		/*
		 *  Apollo system support library loads into the virtual address space
		 */
		case SYSINLIB:
		{
			int status,xfer;
			short len;
			std_$call void pm_$load();
			std_$call void pm_$call();
			if(a1)
			{
				len = strlen(a1);
				pm_$load(*a1, len, 2 , 0, xfer,status);
				if(status!=0)
				failed(a1,"cannot inlib");
				else if(xfer)
					pm_$call(xfer);
			}
			break;
		}

		case SYSINPROCESS:
			if(argn < 2)
				on_option(INPROC);
			else
				exitval = exec_here(com);
			break;
#endif	/* apollo */
	}
}

static const char flgchar[] = "efilmnprstuvwxzHLRZ";
static const int flgval[] = {E_FLAG,F_FLAG,I_FLAG,L_FLAG,M_FLAG,
			N_FLAG,P_FLAG,R_FLAG,S_FLAG,T_FLAG,U_FLAG,V_FLAG,
			W_FLAG,X_FLAG,Z_FLAG,A_FLAG,L_JUST,R_JUST,R_JUST|Z_FILL};
/*
 * process option flags for built-ins
 * flagmask are the invalid options
 */

static int flagset(flaglist,flagmask)
char *flaglist;
{
	register int flag = 0;
	register int c;
	register char *cp,*sp;

	for(cp=flaglist+1;c = *cp;cp++)
	{
		if(isdigit(c))
			wdnum = 10*wdnum + (c - '0');
		else if(sp=strchr(flgchar,c))
			flag |= flgval[sp-flgchar];
		else
			goto badoption;
	}
	if((flag&flagmask)==0)
		return(flag);
badoption:
	failed(cmd_name,badopt);
	/* NOTREACHED */
}


int execexp(s,f)
register char *s;
register FILE	*f;
{
	FILEBLK	fb;
	FILE	fd;
	TREPTR t;
	char inbuf[BUFSIZ];
	push(&fb);
	if(s)
	{
		estabf(s,&fd);
		fb.feval=(char **)(f);
	}
	else if(f!=NULL)
	{
		initf(f);
		setbuf(f,inbuf);
	}
	exec_flag++;
	t = cmd(NL,NLFLG|MTFLG);
	exec_flag--;
	if(is_option(READPR)==0)
		states &= ~READPR;
	if(s==NULL && fc_fix)
		hist_flush();
	pop(0);
	execute(t,states&ERRFLG);
}


/*
 * Given the name or number of a signal return the signal number
 */

static int sig_number(string)
register char *string;
{
	register int n;
	if(isdigit(*string))
		n = atoi(string);
	else
	{
		n = syslook(string,signal_names);
		n &= (1<<SIGBITS)-1;
		n--;
	}
	return(n);
}

#ifdef JOBS
/*
 * list all the possible signals
 */
static void sig_list()
{
	register SYSPTR	syscan;
	register int n = MAXTRAP+1;
	char *names[MAXTRAP+1];
	syscan=signal_names;
	p_setout(standout);
	/* not all signals may be defined */
	while(--n >= 0)
		names[n] = badtrap;
	while(*syscan->sysnam)
	{
		n = syscan->sysval;
		n &= ((1<<SIGBITS)-1);
		names[n] = syscan->sysnam;
		syscan++;
	}
	n = MAXTRAP;
	while(names[--n]==badtrap);
	names[n+1] = NULL;
	p_list(n-1,names+2);
}
#endif	/* JOBS */
