/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)macro.c	1.1 */
/*
 * UNIX shell
 *
 * S. R. Bourne
 * AT&T Bell Laboratories
 * Rewritten by David Korn
 *
 */

#include	<sys/types.h>
#include	<sys/stat.h>
#include	"flags.h"
#include	"defs.h"
#include	"io.h"
#include	"sym.h"
#include	"stak.h"
#include	"name.h"
#include	"shtype.h"
#include	"mode.h"
#include	"jobs.h"
#include	"builtins.h"
#include	"brkincr.h"
#ifdef MULTIBYTE
#include	"national.h"
#endif /* MULTIBYTE */

#define unreadc(c)	(peekc = (c)|MARK)
#define blt_no(t)	((t)>>(COMBITS+1))
#define RBRACE		'}'

/* These routines are defined by this module */
char	*macro();
char	*mactry();
char	*mactrim();
void	mac_subst();

/* These external routines are referenced by this module */
extern char	*arg_dolminus();
extern void	assign();
extern void	await();
extern FILE	*chkopen();
extern void	chkpipe();
extern TREPTR	cmd();
extern void	exfunct();
extern void	failed();
extern void	initf();
extern char	*itos();
extern NAMPTR	lookup();
extern long	lseek();
extern TREPTR	makefork();
extern char	*match_paren();
extern char	*movstr();
extern void	p_setout();
extern int	readc();
extern char	*strcpy();
extern void	tdystak();
extern FILE	*tmp_open();
extern void	trim();
extern char	*valup();

#ifdef MULTIBYTE
static int	charlen();
#endif /* MULTIBYTE */
static char	*copyto();
static char	*substring();
static void	skipto();
static int	getch();
static void	comsubst();
static void	mac_error();

static char	quote;	/* used locally */
static char	quoted;	/* used locally */
static char	mflag;	/* set for macro expansion, unset for here docs */
static FILE	*w_fd;
static int mac_try;
static jmp_buf mac_buf;
static char	idb[2];


static char *copyto(endch)
register char	endch;
{
	register int	c;

	while((c=getch(endch))!=endch && c)
	{
		if(quote || c==ESCAPE)
		{
			pushstak(ESCAPE);
			if(c==ESCAPE)
			{
				c = readc();
				if(quote && !escchar(c) && c!= '"')
				{
					pushstak(ESCAPE);
					pushstak(ESCAPE);
				}
			}
		}
		pushstak(c);
	}
	zerostak();
	if(c!=endch)
		mac_error();
}

	/* skip chars up to } */
static void skipto(endch)
register char endch;
{
	register char	c;
	while((c=readc()) && c!=endch)
	{
		switch(c)
		{
			case SQUOTE:	case DQUOTE:
				skipto(c);
				break;

			case DOLLAR:
				if(readc()==BRACE)
					skipto(RBRACE);
		}
	}
	if(c!=endch)
		mac_error();
}

static int getch(endch)
int	endch;
{
	register int	c;
	int atflag;  /* set if $@ or ${array[@]} within double quotes */
retry:
	c = readc();
	if(!subchar(c))
		return(c);
	if(c==DOLLAR)
	{
		register int	 bra = 0; /* {...} bra =1, {#...} bra=2 */
		register char *v;
		register char *argp;
		register NAMPTR	 n=(NAMPTR)NULL;
		int 	dolg=0;
		int dolmax = dolc+1;
		BOOL 	nulflg;
		char *id=idb;
		*id = 0;
	retry1:
		c = readc();
		switch(c)
		{
			case DOLLAR:
				v=pidadr;
				break;

			case '!':
				v=pcsadr;
				break;

			case BRACE:
				if(bra++ ==0)
					goto retry1;

			case LPAREN:
				if(bra==0 && mac_try==0)
				{
					comsubst(1);
					goto retry;
				}
				goto nosub;

			case RBRACE:
				if(bra!=2)
					goto nosub;
				bra = 0;
			case '#':
				if(bra ==1)
				{
					bra++;
					goto retry1;
				}
				v=itos(dolc);
				break;

			case '?':
				v=itos(savexit);
				break;

			case '-':
				v=arg_dolminus();
				break;
			
			default:
				if(isalpha(c))
				{
					argp=(char *) relstak();
					while(isalnum(c))
					{
						pushstak(c);
						c= readc();
					}
					if(c=='[' && bra)
					{
						if((c=readc(),astchar(c)))
						{
							*id = c;
							if(c=readc()!=']')
								mac_error();
							dolmax = 0;
						}
						else
						{
							int savq = quote;
							pushstak('[');
							unreadc(c);
							quote = 0;
							copyto(']');
							quote = savq;
							pushstak(']');
						}
					}
					else
						unreadc(c);
					zerostak();
					n=lookup(absstak(argp));
					setstak(argp);
					v = valup(n);
					id = n->namid;
					if(dolmax == 0 && attest(n, ARRAY))
					{
						dolg = -((int)(arayp(n)->maxi) + 1);
						while(v==0)
						{
							arayp(n)->adot++;
							if(++dolg == 0)
								break;
							v = valup(n);
						}
					}
					goto cont1;
				}
				if(digchar(c))
				{
					*id = c;
					if(astchar(c))
					{
						dolg=1;
						c=1;
					}
					else
					{
						c -= '0';
						if(bra)
						{
							int d;
							while((d=readc(),isdigit(d)))
								c = 10*c + (d-'0');
							unreadc(d);
						}
					}
					v=((c==0)?cmdadr:(c<=dolc)?dolv[c] : (char *)(dolg=0));
					goto cont1;
	 			}
			nosub:
				if(bra)
					mac_error();
				else
				{
					unreadc(c);
					return(DOLLAR);
				}
			}
	cont1:
		c = readc();
		if(bra==2)
		{
			if(c!=RBRACE)
				mac_error();
			if(dolg==0 && dolmax)
#ifdef MULTIBYTE
				c = (v?charlen(v):0);
#else
				c = (v?strlen(v):0);
#endif /* MULTIBYTE */
			else if(dolg>0)
				c = dolc;
			else if(dolg<0)
				c = arayp(n)->maxi+1;
			else
				c = (v!=0);
			v = itos(c);
			dolg = 0;
			c = RBRACE;
		}
		/* check for quotes @ */
		if(idb[0]=='@' && quote && !atflag)
		{
			quoted--;
			atflag = 1;
		}
		if(c==':' && bra)	/* null and unset fix */
		{
			nulflg=1;
			c=readc();
		}
		else
			nulflg=0;
		if(!defchar(c) && bra)
			mac_error();
		argp = 0;
		if(bra)
		{
			if(c!=RBRACE)
			{
				argp=(char *)relstak();
				if((v==0 || (nulflg && *v==0)) ^ (setchar(c)!=0))
					copyto(RBRACE);
				else
					skipto(RBRACE);
				argp=absstak(argp);
			}
		}
		else
		{
			unreadc(c);
			c=0;
		}
		/* check for substring operations */
		if(c == '#' || c == '%')
		{
			if(dolg != 0)
				mac_error();
			if(v && *v)
			{
				/* allow room for escapes */
				staktop += strlen(v);
				strcpy(staktop,v);
				trim(argp);
				if(*argp==c)
				{
					c |= MARK;
					argp++;
				}
				v = substring(staktop,argp,c);
				if(c&MARK)
					argp--;
			}
			staktop = argp;
		}
		if(v && (!nulflg || *v ) && c!='+')
		{
			while(1)
			{
				BOOL no_ifs = 0;
				int sep = SP;
				argp = valup(IFSNOD);
				if(argp==0 || *argp==0)
					no_ifs++;
				else
					sep = *argp;
				/* quoted null strings have to be marked */
				if(*v==0 && quote)
				{
					pushstak(ESCAPE);
					pushstak(0);
				}
				while(c = *v++)
				{
					if(staktop >= brkend)
						setbrk(BRKINCR);
					if(quote || (c==ESCAPE&&mflag)
						 || (no_ifs&&isspace(c)))
				 		pushstak(ESCAPE); 
			 		pushstak(c); 
				}
				if(dolg==0 || (++dolg>=dolmax))
					 break;
				if(dolg>0)
					v = dolv[dolg];
				else
				{
					arayp(n)->adot++;
					while((v=valup(n))==0)
					{
						arayp(n)->adot++;
						if(dolg++==0)
						break;
					}
						if(v==0)
					break;
				}
				if(quote && *id=='*')
				{
					if(no_ifs)
						continue;
					pushstak(ESCAPE);
				}
				pushstak(sep);
			}
		}
		else if(argp)
		{
			if(c=='?')
			{
				if(mac_try)
					mac_error();
				else
				{
					trim(argp);
					failed(id,*argp?argp:badparam);
				}
			}
			else if(c=='=')
			{
				if(n)
				{
					trim(argp);
					assign(n,argp);
					staktop = movstr(valup(n),argp);
				}
				else
					mac_error();
			}
		}
		else if(is_option(NOSET))
		{
			if(mac_try)
				mac_error();
			else
				failed(id,unset);
		}
		goto retry;
	}
	else if(c==endch)
		return(c);
	else if(c==SQUOTE && mac_try==0)
	{
		comsubst(0);
		goto retry;
	}
	else if(c==DQUOTE)
	{
		if(quote ==0)
		{
			atflag = 0;
			quoted++;
		}
		quote ^= 1;
		goto retry;
	}
	return(c);
}

	/* Strip "" and do $ substitution
	 * Leaves result on top of stack
	 */
char *macro(as)
char *as;
{
	register BOOL	savqu =quoted;
	register char	savq = quote;
	FILE	fblk;
	FILEBLK	cb;
	mflag = 1;
	push(&cb);
	estabf(as,&fblk);
	usestak();
	quote=0;
	quoted=0;
	copyto(0);
	pop(1);
	if(quoted && (stakbot == staktop))
	{
		pushstak(ESCAPE);
		pushstak(0);
	}
	/* above is the fix for *'.c' bug	*/
	quote=savq;
	quoted=savqu;
	return(fixstak());
}

/*
 * command substitution
 * type==0 for ``
 * type==1 for $()
*/

static void comsubst(type)
int type;
{
	FILEBLK	cb;
	register FILE	*fd;
	FILE 	*pv[2];
	FILE	fblk;
	char tmp_fname[TMPSIZ];
	register unsigned int	d;
	register TREPTR t;
	register char *argc;
	IOPTR saviotemp = iotemp;
	int forkflag = FPOU|FCOMSUB;
	STKPTR savtop = staktop;
	STKPTR savptr = fixstak();
	char inbuff[BUFSIZ];
	int saveflag = states&FIXFLG;
	register int waitflag = 0;
	if(w_fd)
		fflush(w_fd);	/* flush before executing command */
	usestak();
	if(type)
	{
		staktop = (STKPTR)(match_paren((char*)stakbot,LPAREN,RPAREN,0)-1);
	}
	else
	{
		while((d=readc())!=SQUOTE && d)
		{
			if(d==ESCAPE)
			{
				d = readc();
				/*
				 * This is wrong but it preserves compatibility with
				 * the SVR2 shell
				 */
				if(!(escchar(d) || (d=='"' && quote)))
					pushstak(ESCAPE);
			}
			pushstak(d);
		}
	}
	argc=fixstak();
	states &= ~FIXFLG;		/* do not save command subs in fc file */
	push(&cb);
	estabf(argc,&fblk);
	subflag = 0;
	exec_flag++;
	t = cmd(EOFSYM,MTFLG|NLFLG);
	exec_flag--;
	d = t->tretyp;
	if(!subflag && !t->treio && (d&COMMSK)==TCOM && blt_no(d)>SYSSPECIAL)
	{
		/* nested command subs not handled specially */
		/* handle command substitution of most builtins separately */
		/* exec, login, cd, ., eval and shift not handled this way */
		/* put output into tmpfile */
		FILE *save1_out = standout;
		if((states&IS_TMP)==0)
		{
			/* create and keep open a /tmp file for command subs */
			fd = tmp_open(tmp_fname);
			fd = frenumber(fd,TMPIO);
			states |= IS_TMP;
			/* root cannot unlink because fsck could give bad ref count */
			if(userid)
				unlink(tmp_fname);
			else
				states |= RM_TMP;
		}
		else
			fd = file_fd(TMPIO);
		standout = fd;
		/* this will only flush the buffer if output is fd already */
		p_setout(fd);
#ifdef JOBS
		states |= NONSTOP;
#endif	/* JOBS */
		putc(0,fd);
		exfunct(t,(char**)0,states&ERRFLG);
		putc(0,fd);
#ifdef JOBS
		states &= ~NONSTOP;
#endif	/* JOBS */
		if(*_sobuf != 0)
		{
			/* file is larger than buffer, read from it */
			fflush(fd);
			fseek(fd,1L,0);
			initf(fd);
			waitflag = -1;
		}
		else
		{
			/* The file is all in the buffer */
			setbuf(fd,NIL);
			strcpy(inbuff,(char*)_sobuf+1);
			setbuf(fd,(char*)_sobuf);
			estabf(inbuff,(fd= &fblk));
		}
		standout = save1_out;
		goto readit;
	}
	else if(d==0 && ((COMPTR)t)->comarg==0)
	{
		if(((t->treio)->iofile) == 0)
			argc = mactrim((t->treio)->ioname,1);
		else
			argc = devnull;
		fd = chkopen(argc);
	}
	else
	{
		waitflag++;
		if(iotemp!=saviotemp)
			forkflag |= FTMP;
		t = makefork(forkflag,t);
		  /* this is done like this so that the pipe
		   * is open only when needed
		   */
		chkpipe(pv);
#ifdef JOBS
		jobstat.cur_pgrp = jobstat.mypid;
		jobstat.j_flag++;
#endif	/* JOBS */
		execute(t, states&ERRFLG, (FILE**)0, pv);
#ifdef JOBS
		jobstat.j_flag = 0;
#endif	/* JOBS */
		fd = pv[INPIPE];
		fclose(pv[OTPIPE]);
	}
	setbuf(fd,inbuff);
	initf(fd);

readit:
	tdystak(savptr);
	d = savtop - savptr;
	while(d--)
		*staktop++ = *savptr++;
	while(d=readc())
	{
		if(quote || (d==ESCAPE&&mflag))
			pushstak(ESCAPE);
		pushstak(d);
	}
	if(waitflag>0)
		await(parent,0);
	while(stakbot!=staktop)
	{
		if(*--staktop != NL)
		{
			*++staktop;
			break;
		}
		else if(quote)
			staktop--;
	}
	pop(waitflag>=0?0:1);
	states |= saveflag;
}


void mac_subst(in,ot)
FILE 	*in;
register FILE *ot;
{
	register char	c;
	register flag = is_option(EXECPR);
	FILEBLK 	fb;
	char inbuff[BUFSIZ];
	char otbuff[BUFSIZ];
	mflag = 0;
	w_fd = ot;
	push(&fb);
	initf(in);
	/* DQUOTE used to stop it from quoting */
	setbuf(in,inbuff);
	setbuf(ot,otbuff);
	if(flag)
		p_setout(stderr);
	usestak();
	while(1)
	{
		c=getch(DQUOTE);
		if(c==ESCAPE)
		{
			c = readc();
			if(!escchar(c))
				pushstak(ESCAPE);
		}
		if(staktop!=stakbot)
		{
			*staktop = 0;
			fputs(stakbot,ot);
			if(flag)
				fputs(stakbot,output);
			staktop = stakbot;
		}
		if(c==0)
			break;
		putc(c,ot);
		if(flag)
			putc(c,output);
	}
	pop(0);
	w_fd = NULL;
	fflush(ot);
	fseek(ot,0L,0);
	setbuf(ot,NIL);
}


 
/*
 * Computes the substring of STRING using the expression PAT
 * depending on which FLAG is set.
 */

static char *substring(string,pat,flag)
char *string;
char *pat;
int flag;
{
	register char *sp = string;
	register char *cp;
	switch(flag)
	{
		case '#':
		case MARK|'#':
		{
			register int c;
			cp = sp;
			do
			{
#ifdef MULTIBYTE
				c = *sp;
				c = echarset(c);
				sp += (in_csize(c)+(c>=2));
				c = *sp;
#else
				c= *++sp;
#endif /* MULTIBYTE */
				*sp=0;
				if(gmatch(string,pat))
				{
					cp = sp;
					if(flag=='#')
						break;
				}
				*sp = c;
			}
			while(c);
			*sp = c;
			return(cp);
		}

		case '%':
		case MARK|'%':
		{
			sp += strlen(sp);
			cp = sp;
			while(sp>=string)
			{
				if(gmatch(sp,pat))
				{
					cp = sp;
					if(flag=='%')
						break;
				}
				sp--;
#ifdef MULTIBYTE
				if(*sp&HIGHBIT)
				{
					if(*(sp-in_csize(3))==ESS3)
						sp -= in_csize(3);
					else if(*(sp-in_csize(2))==ESS2)
						sp -= in_csize(2);
					else
						sp -= (in_csize(1)-1);
				}
#endif /* MULTIBYTE */
			}
			*cp = 0;
			return(string);
		}
	}
	return(sp);
}


/*
 * do parameter and command substitution and strip of quotes
 * attempt file name expansion if <type> not zero
 */

char *mactrim(s,type)
char *	s;
{
	register char *t=macro(s);
	ARGPTR schain = gchain;
	if(type && f_complete(t,nullstr)==1)
		t = gchain->argval;
	gchain = schain;
	trim(t);
	return(t);
}

/*
 * perform only parameter substitution and catch failures
 */

char *mactry(s)
register char *s;
{
	mac_try++;
	if(setjmp(mac_buf)==0)
		s = mactrim(s,0);
	mac_try = 0;
	return(s);
}

static void mac_error()
{
	if(mac_try)
		longjmp(mac_buf,1);
	error(badsub);
}



#ifdef MULTIBYTE
static int	charlen(str)
register char *str;
{
	register int n = 0;
	register int c;
	while(*str)
	{
		c = echarset(*str);		/* find character set */
		str += (in_csize(c)+(c>=2));	/* move to next char */
		n += out_csize(c);		/* add character size */
	}
	return(n);
}
#endif /* MULTIBYTE */
