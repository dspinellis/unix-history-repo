/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)args.c	1.1 */
/*
 * UNIX shell
 *
 * S. R. Bourne
 * Rewritten by David Korn
 * AT&T Bell Laboratories
 *
 */

#include	"flags.h"
#include	"defs.h"
#include	"sym.h"
#include	"mode.h"
#include	"name.h"
#include	"io.h"
#include	"builtins.h"
#include	"brkincr.h"
#include	"stak.h"
#ifdef DEVFD
# ifdef JOBS
#include	"jobs.h"
# endif /* JOBS */
#endif	/* DEVFD */


void	arg_set();
void	arg_reset();
void	arg_clear();
DOLPTR	arg_free();
DOLPTR	arg_use();
DOLPTR	arg_new();
int	arg_opts();
char	**arg_build();
char	*arg_dolminus();
#ifdef DEVFD
void	close_pipes();
#endif	/* DEVFD */

extern char *malloc();
extern char *macro();
extern char *movstr();
extern char *strchr();
extern char *itos();
extern void assign();
extern void failed();
extern void chkpipe();
extern void exitsh();
extern void free();
extern void gsort();
extern void trim();
extern void p_str();
extern void p_nchr();
extern void p_setout();
extern char *qvalup();

static int	arg_expand();
static DOLPTR	copyargs();
static void	print_opts();
static int	split();

static char	*null;
static DOLPTR argfor;	/* linked list of blocks to be cleaned up */
static DOLPTR dolh;
static char flagadr[12];
static const char flagchar[] =
{
	'i',	'n',	'v',	't',	's',	'x',	'e',	'r',	'k',
	'u', 'f',	'a',	'm',	'h',	'p',	'c', 0
};
static const optflag flagval[]  =
{
	INTFLG,	NOEXEC,	READPR,	ONEFLG, STDFLG,	EXECPR,	ERRFLG,	RSHFLG,	KEYFLG,
	NOSET,	NOGLOB,	ALLEXP,	MONITOR, HASHALL, PRIVM, CFLAG, 0
};

/* ======== option handling	======== */

/*
 *  This routine turns options on and off
 *  The options "sicr" are illegal from set command.
 *  The -o option is used to set option by name
 *  This routine returns the number of non-option arguments
 */

int arg_opts(argc,argv)
char **argv;
int  argc;
{
	register char *cp;
	register int c;
	register char **argp=argv;
	register char *flagc;
	register optflag newflags=flags;
	register optflag opt;
	char minus;
	int sort = 0;
	int setflag = eq(*argp,bset);
	while((cp= *++argp) && ((c= *cp) == '-' || c=='+'))
	{
		minus = (c == '-');
		argc--;
		if((c= *++cp)==0)
		{
			newflags &= ~(EXECPR|READPR);
			argp++;
			break;
		}
		else if(c == '-')
		{
			if(setflag)
				states |= RWAIT;
			argp++;
			break;
		}
		while(c= *cp++)
		{
			if(setflag)
			{
				if(c=='s')
				{
					sort++;
					continue;
				}
				else if(strchr("icr",c))
					failed(argv[1], badopt);
			}
			if(c=='c' && minus && argc>=2 && comdiv==0)
			{
				comdiv= *++argp;
				argc--;
				newflags |= CFLAG;
				continue;
			}
			if(flagc=strchr(flagchar,c))
				opt = flagval[flagc-flagchar];
			else if(c != 'o')
				failed(argv[1],badopt);
			else
			{
				argp++;
				if(*argp==NULL)
				{
					print_opts(newflags);
					argp--;
					continue;
				}
				else
				{
					argc--;
					c=syslook(*argp,option_flags);
					opt = 1L<<c;
					if(opt&(1|INTFLG|RSHFLG))
						failed(*argp,badopt);
				}
			}
			if(minus)
			{
#if ESH || VSH
				if(opt&(EDITVI|EMACS|GMACS))
					newflags &= ~ (EDITVI|EMACS|GMACS);
#endif
				newflags |= opt;
			}
			else
				newflags &= ~opt;
		}
	}
	/* cannot set -n for interactive shells since there is no way out */
	if(is_option(INTFLG))
		newflags &= ~NOEXEC;
#ifdef RAWONLY
	if(is_option(EDITVI))
		newflags |= VIRAW;
#endif	/* RAWONLY */
	if(sort)
	{
		if(argc>1)
			gsort(argp,argc-1);
		else
			gsort(dolv+1,dolc);
	}
	if((newflags&PRIVM) && !is_option(PRIVM))
		assign(PATHNOD,defpath);
	flags = newflags;
	return(argc);
}

/*
 * returns the value of $-
 */

char *arg_dolminus()
{
	register char *flagc=flagchar;
	register char *flagp=flagadr;
	while(*flagc)
	{
		if(flags&flagval[flagc-flagchar])
			*flagp++ = *flagc;
		flagc++;
	}
	*flagp++=0;
	return(flagadr);
}

/*
 * set up positional parameters 
 */

void arg_set(argi)
char *argi[];
{
	register char **argp=argi;
	register int size = 0; /* count number of bytes needed for strings */
	register int 	argn=0;
	register char *cp;
	/* count args and number of bytes of arglist */
	while((cp=(char*)*argp++) != ENDARGS)
	{
		size += strlen(cp);
	}
	/* free old ones unless on for loop chain */
	argn = argp - argi;
	arg_free(dolh,0);
	dolh=copyargs(argi, --argn, size);
	dolc=argn-1;
}

/*
 * free the argument list if the use count is 1
 * If count is greater than 1 decrement count and return same blk
 * Free the argument list if the use count is 1 and return next blk
 * Delete the blk from the argfor chain
 * If flag is set, then the block dolh is not freed
 */

DOLPTR arg_free(blk,flag)
DOLPTR 	blk;
{
	register DOLPTR	argr=blk;
	register DOLPTR	argblk;
	if(argblk=argr)
	{
		if((--argblk->doluse)==0)
		{
			if(flag && argblk==dolh)
				dolh->doluse = 1;
			else
			{
				/* delete from chain */
				if(argfor == argblk)
					argfor = argblk->dolnxt;
				else
				{
					for(argr=argfor;argr;argr=argr->dolnxt)
						if(argr->dolnxt==argblk)
							break;
					if(argr==0)
					{
						return(NULL);
					}
					argr->dolnxt = argblk->dolnxt;
				}
				free((char*)argblk);
			}
			argr = argblk->dolnxt;
		}
	}
	return(argr);
}

/*
 * grab space for arglist and link argblock for cleanup
 * The strings are copied after the argment vector
 */

static DOLPTR copyargs(from, n, size)
char *from[];
{
	register DOLPTR dp=(DOLPTR)malloc((unsigned)(DOLTYPE + n*sizeof(char*) + size + n));
	register char **pp;
	register char *sp;
	dp->doluse=1;	/* use count */
	/* link into chain */
	dp->dolnxt = argfor;
	argfor = dp;
	pp= dp->dolarg;
	dolv=pp;
	sp = (char*)dp + DOLTYPE + n*sizeof(char*);
	while(n--)
	{
		*pp++ = sp;
		sp = movstr(*from++,sp) + 1;
	}
	*pp = ENDARGS;
	return(dp);
}

/*
 *  used to set new argument chain for functions
 */

DOLPTR arg_new(argi,savargfor)
char *argi[];
DOLPTR *savargfor;
{
	register DOLPTR olddolh = dolh;
	*savargfor = argfor;
	dolh = NULL;
	argfor = NULL;
	arg_set(argi);
	return(olddolh);
}

/*
 * reset arguments as they were before function
 */

void arg_reset(blk,afor)
DOLPTR blk;
DOLPTR afor;
{
	while(argfor=arg_free(argfor,0));
	dolh = blk;
	argfor = afor;
}

void arg_clear()
{
	/* force `for' $* lists to go away */
	while(argfor=arg_free(argfor,1));
	/* clean up io files */
	argfor = dolh;
	while(pop(0));
#ifdef DEVFD
	close_pipes();
#endif	/* DEVFD */
}

/*
 * increase the use count so that an arg_set will not make it go away
 */

DOLPTR arg_use()
{
	register DOLPTR dh;
	if(dh=dolh)
		dh->doluse++;
	return(dh);
}

/*
 *  Print option settings on standard output
 */

static void print_opts(oflags)
#ifndef pdp11
register
#endif	/* pdp11 */
optflag oflags;
{
	register SYSPTR	syscan = option_flags;
#ifndef pdp11
	register
#endif	/* pdp11 */
	optflag value;
	p_setout(standout);
	p_str(opt_heading,NL);
	while(value=syscan->sysval)
	{
		value = 1<<value;
		p_str(syscan->sysnam,SP);
		p_nchr(SP,16-strlen(syscan->sysnam));
		if(oflags&value)
			p_str(on_,NL);
		else
			p_str(off_,NL);
		syscan++;
	}
}


/*
 * build an argument list
 */

char **arg_build(nargs,comptr)
int 	*nargs;
COMPTR	comptr;
{
	register ARGPTR	argp;
	{
		register COMPTR	ac = comptr;
		register ARGPTR	schain;
		/* see if the arguments have already been expanded */
		if(ac->comarg==NULL)
		{
			*nargs = 0;
			return(&null);
		}
		else if((ac->comtyp&COMSCAN)==0)
		{
			*nargs = ((DOLPTR)ac->comarg)->doluse;
			return(((DOLPTR)ac->comarg)->dolarg+1);
		}
		schain = gchain;
		gchain = NULL;
		*nargs = arg_expand(ac);
		argp = gchain;
		gchain = schain;
	}
	{
		register char	**comargn;
		register int	argn;
		register char	**comargm;
		argn = *nargs;
		argn++;	/* allow room to prepend interpreter name */
		comargn=(char **) getstak(BYTESPERWORD*argn+BYTESPERWORD);
		comargm = comargn += argn;
		*comargn = ENDARGS;
		while(argp)
		{
			*--comargn = argp->argval;
			if((argp->argflag&A_RAW)==0)
				trim(*comargn);
			if((argp=argp->argchn)==0 || (argp->argflag&A_MAKE))
			{
				if((argn=comargm-comargn)>1)
					gsort(comargn,argn);
				comargm = comargn;
			}
		}
		return(comargn);
	}
}

#ifdef DEVFD
static FILE *to_close[15];

void close_pipes()
{
	register FILE **fd = to_close;
	while(*fd)
	{
		fclose(*fd);
		*fd++ = NULL;
	}
}
#endif	/* DEVFD */

/* Argument list generation */

static int arg_expand(ac)
COMPTR		ac;
{
	register ARGPTR	argp;
	register int 	count=0;
#ifdef DEVFD
	int indx = 0;
	close_pipes();
#endif	/* DEVFD */
	if(ac)
	{
		argp = ac->comarg;
		while(argp)
		{
			argp->argflag &= ~A_MAKE;
#ifdef DEVFD
			if(*argp->argval==0 && (argp->argflag&A_EXP))
			{
				/* argument of the form (cmd) */
				register ARGPTR ap;
				char *cp;
				FILE *pv[2];
				int fd;
				ap = (ARGPTR)locstak();
				ap->argflag |= A_MAKE;
				ap->argflag &= ~A_RAW;
				ap->argchn= gchain;
				gchain = ap;
				count++;
				cp = movstr(devfd,ap->argval);
				chkpipe(pv);
				fd = argp->argflag&A_RAW;
				endstak(movstr(itos(fileno(pv[fd])),cp));
				if(fd)
					execute((TREPTR)argp->argchn,states&ERRFLG,pv,(FILE**)0);
				else
					execute((TREPTR)argp->argchn,states&ERRFLG,(FILE**)0,pv);
#ifdef JOBS
				jobstat.j_flag++;
#endif	/* JOBS */
				fclose(pv[1-fd]);
				to_close[indx++] = pv[fd];
			}
			else
#endif	/* DEVFD */
			if((argp->argflag&A_RAW)==0)
			{
				register char *ap; ap = argp->argval;
				if(argp->argflag&A_MAC)
					ap = macro(ap);
				count += split(ap,argp->argflag&A_MAC);
			}
			else
			{
				argp->argchn= gchain;
				gchain = argp;
				argp->argflag |= A_MAKE;
				count++;
			}
			argp = argp->argnxt;
		}
	}
	return(count);
}

static int split(s,macflg) /* blank interpretation routine */
char *s;
{
	register char *argp;
	register int 	c;
	register ARGPTR ap;
	int 	count=0;
	int expflag = (is_option(NOGLOB)==0);
	char *seps = (macflg?qvalup(IFSNOD):NULL);
	if(seps==NULL || *seps==0)
		seps = sptbnl;
	while(1)
	{
		if(trapnote&SIGSET)
			exitsh(SIGFAIL);
		ap = (ARGPTR)locstak();
		argp = ap->argval;
		while(c= *s++)
		{
			if(c == ESCAPE)
			{
				c = *s++;
				if(c!='/')
					*argp++ = ESCAPE;
			}
			else if(strchr(seps,c))
				break;
			if(argp >= brkend)
				setbrk(BRKINCR);
			*argp++ = c;
		}
	/* This allows contiguous visible delimiters to count as delimiters */
		if(argp==ap->argval)
		{
			if(c==0)
				return(count);
			if(macflg==0 || strchr(sptbnl,c))
				continue;
		}
		else if(c==0)
		{
			s--;
		}
		/* file name generation */
		endstak(argp);
		ap->argflag &= ~(A_RAW|A_MAKE);
		if(expflag && (c=expand(ap->argval,0)))
			count += c;
		else
		{
			count++;
			ap->argchn= gchain;
			gchain = ap;
		}
		gchain->argflag |= A_MAKE;
	}
}
