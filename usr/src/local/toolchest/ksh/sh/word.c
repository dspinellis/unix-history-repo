/* @(#)word.c	1.1 */
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
#include	"io.h"
#include	"history.h"
#include	"stak.h"
#include	"sym.h"
#include	"shtype.h"
#include	"brkincr.h"
#include	"name.h"
#include	"builtins.h"
#ifdef JOBS
#ifdef BSD
#include	<signal.h>
static jmp_buf readerr;
static int nreadc;
#endif	/* BSD */
#endif	/* JOBS */



static int letflg = 0;
static FILEBLK a_fb;
static FILE a_fd;

/* This module defines the following routines */
char	*match_paren();
int	nextc();
int	readc();
int	word();

/* This module references these external routines */
extern void	arg_clear();
extern void	chktrap();
extern void	chkpr();
extern void	exitsh();
extern NAMPTR	findnod();
extern long	lseek();
extern char	*movstr();
extern void	synbad();
extern char	*tilde();
extern char	*valup();

/* ========	character handling for command lines	========*/

/*
 * Get the next word and put it on the top of the stak
 * Determine the type of word and set wdnum and wdset accordingly
 * Returns the token type
 */

word()
{
	register int c;
	register int d;
	register char *argp;
	register char *tildp;
	char not_alias;
	char chk_keywd;
	int 	alpha = 0;
	wdnum=0;
	/* condition needed to check for keywords, name=value */
	chk_keywd = reserv!=0 || (wdset&KEYFLG);
	wdset &= ~KEYFLG;
	wdarg = (ARGPTR)locstak();
	argp = wdarg->argval;
	if(letflg)
	{
		letflg = 0;
		*argp++ =(DQUOTE);
		argp = match_paren(argp, LPAREN, RPAREN, 1);
		*(argp-1)=(DQUOTE);
		c = nextc();
		wdval = 0;
		if(c != ')')
		{
			stakbot = wdarg->argval;
			cpystak(let_syntax);
			synbad();
		}
		endstak(--argp);
		return(0);
	}
	tildp = NULL;
	while(1)
	{
		while((c=nextc(), isspace(c)));
		if(c==COMCHAR)
		{
			while((c=readc()) != NL && c != ENDOF);
			peekc=c;
		}
		else	 /* out of comment - white isspace loop */
			break;
	}
	if(c=='~')
		tildp = argp;
	not_alias = (aliflg==0);
	if(!ismeta(c))
	{
		do
		{
			if(c==LITERAL)
				argp = match_paren(argp,c,c,0);
			else
			{
				if(argp==wdarg->argval&&chk_keywd&&isalpha(c))
				{
					alpha++;
				}
				*argp++=(c);
				if(c == ESCAPE)
					*argp++ = readc();
				if(alpha)
				{
					if(c == '[')
					{
						argp = match_paren(argp,'[',']',-1);
					}
					else if(c=='=')
					{
						wdset |= KEYFLG;
						tildp = argp;
						alpha = 0;
					}
					else if(!isalnum(c))
						alpha = 0;
				}
				if(qotchar(c))
				{
					argp = match_paren(argp,c,c,0);
				}
			}
			d = c;
			c = nextc();
			if(d==DOLLAR && c ==LPAREN)
			{
				subflag++;
				*argp++ = c;
				argp = match_paren(argp, LPAREN, RPAREN, 0);
				c = nextc();
			}
			else if(tildp!=NULL &&  (c == '/'  || c==':' || ismeta(c)))
			{
				/* check for tilde expansion */
				register char *dir;
				*argp = 0;
				staktop = argp;
				dir=tilde(tildp);
				/* This check needed if tilde() uses malloc() */
#ifndef INT16
				if(stakbot != (STKPTR)wdarg)
				{
					tildp += ((char*)staktop-argp);
					argp = staktop;
					wdarg = (ARGPTR)stakbot;
				}
#endif /*INT16 */
				if(dir)
				{
					argp=tildp;
					argp = movstr(dir,argp);
				}
				else
					tildp = NULL;
			}
			if(c==':' && (wdset&KEYFLG))
				tildp = argp+1;
		}
		while(!ismeta(c));
		argp=endstak(argp);
		peekn=c|MARK;
		if(((ARGPTR) argp)->argval[1]==0 &&
			(d=((ARGPTR) argp)->argval[0], isdigit(d)) && (c=='>' || c=='<'))
		{
			word();
			wdnum=d-'0';
		}
		else
		{
			/*check for reserved words and aliases */
			wdval = (reserv!=0?syslook(((ARGPTR)argp)->argval,reserved):0);
			/* for unity database software, allow select to be aliased */
			if((reserv!=0 && (wdval==0||wdval==SELSYM)) || (wdset&S_FLAG))
			{
				/* check for aliases */
				NAMPTR np;
				char *alp = ((ARGPTR)argp)->argval;
				if(not_alias && (wdset&(E_FLAG|KEYFLG))==0 &&
					(np=findnod(alp,alias,CHK_FOR)))
				{
					if(attest(np,T_FLAG)==0 && (alp=valup(np)))
					{
						wdval = 0;
						c = standin->flin;
						push(&a_fb);
						estabf(alp,&a_fd);
						a_fb.flin = c;
						aliflg = peekn;
						peekn = 0;
						wdset |= KEYFLG;
						c = word();
						return(c);
					}
				}
			}
		}
	}
	else if(dipchar(c))
	{
		if((d=nextc())==c)
		{
			wdval = c|SYMREP;
			if(c=='<')
			{
				if((d=nextc())=='-')
					wdnum |= IOSTRIP;
				else
					 peekn = d|MARK;
			}
			/* arithmetic evaluation ((expr)) */
			else if(c == LPAREN && reserv != 0)
			{
				wdval = 0;
				letflg = 1;
				argp = endstak(movstr(blet,argp));
			}
		}
		else if(c=='|' && d=='&')
			wdval = COOPSYM;
#ifdef DEVFD
		else if(d==LPAREN && (c=='<'||c == '>'))
			wdval = (c=='>'?OPROC:IPROC);
#endif	/* DEVFD */
		else
		{
			peekn = d|MARK;
			wdval = c;
		}
	}
	else
	{
		if((wdval=c)==ENDOF)
		{
			wdval=EOFSYM;
		}
		if(iopend && eolchar(c))
		{
			copy(iopend);
			iopend=0;
		}
	}
	reserv=0;
	return(wdval);
}

/*
 * skip until matching <closed>
 * if flag > 0, then newlines and spaces are removed
 * if flag < 0, then each newline cause syntax errors
 */

char *match_paren(argp,open,close,flag)
register char *argp;
register int open;
{
	register int c;
	register int count = 1;
	register int quoted = 0;
	int was_dollar=0;
	char *oldargp = argp;
	int line = standin->flin;
	while(count)
	{
		/* check for unmatched <open> */
		if((c=(open==LITERAL?readc():nextc()))==0)
		{
			/* eof before matching quote */
			/* This keeps old shell scripts running */
			if(fileno(input) == F_STRING)
				break;
			standin->flin = line;
			wdval = open|EOFSYM;
			synbad();
		}
		if(c == NL)
		{
			if(flag<0)
				break;
			chkpr(0);
			if(flag)
				continue;
		}
		else if(c == close)
		{
			if(!quoted)
				count--;
		}
		else if(c == open && !quoted)
			count++;
		if(flag<=0 || c != SP )
		{
			if(open==LITERAL)
				*argp++ = ESCAPE;
			if(argp >= (char*)brkend)
				setbrk(BRKINCR);
			*argp++ = c;
			if(open==LITERAL)
				continue;
		}
		if(!quoted && flag==0)
		{
			/* check for nested '', "", and `` within $() */
			if(open!=close) 
			{
				if(c==LITERAL)
					argp--;
				else if(!qotchar(c))
					goto skip;
				argp = match_paren(argp,c,c,0);
			}
			/* check for $() within '', "", and `` */
			else if(was_dollar && c==LPAREN)
			{
				argp = match_paren(argp,LPAREN,RPAREN,0);
			}
		skip:
			was_dollar = (c==DOLLAR);
		}
		if(c == ESCAPE)
			quoted = 1 - quoted;
		else
			quoted = 0;
	}
	if(open==LITERAL)
	{
		argp -= 2;
		if(argp==oldargp)
		{
			/* handle null argument specially */
			*argp++ = '"';
			*argp++ = '"';
		}
	}
	return(argp);
}

/*
 * If quote is equal to zero then
 * this routine returns the next input character but strips shell
 * line continuations and issues prompts at end of line
 * Otherwise this routine is the same as readc()
 */

nextc()
{
	register int c, d;
	static int oldd;
retry:
	d = readc();
	if(d==ESCAPE && oldd!=ESCAPE)
	{
		if((c=readc())==NL)
		{
			chkpr(0);
			goto retry;
		}
		peekc = c|MARK;
	}
	oldd = d;
	return(d);
}

readc()
{
	register int c;
	register SHFILE	f = standin;
	register FILE *fd = input;
	int maxtry = 20;
	if(staktop >= brkend)
		setbrk(BRKINCR);
	if(peekn)
	{
		c = peekn&~MARK;
		peekn = 0;
		return(c);
	}
	if(peekc)
	{
		c = peekc&~MARK;
		peekc = 0;
		return(c);
	}
retry:
#ifdef JOBS
#ifdef BSD
	if(states&READC)
		nreadc++;
	else
	{
		nreadc = 1;
		states |= READC;
	}
	/* this is needed to implement Bourne shell semantics of traps */
	/* reads automatically restart with jobs library */
	if(fd->_cnt==0 && setjmp(readerr))
		goto trapfound;
#endif	/* BSD */
#endif	/* JOBS */
	if((c=getc(fd)) != EOF)
	{
		if(c==0)
		{
			if(f->feval && estabf(*f->feval++,fd)==0)
				c = SP;
			/* treat the NULL byte as eof for TMPIO */
			else if(fileno(fd) == TMPIO)
			{
				setbuf(fd,NIL);
				lseek(TMPIO,0L,0);
			}
			/* skip over null bytes in files */
			else if(fileno(fd) !=  F_STRING)
				goto retry;
			else if(aliflg)
			{
				c = (aliflg&~MARK);
				aliflg = 0;
				wdset |= S_FLAG;
				pop(1);
			}
			else
			/* end-of-string is end-of-file */
			{
				f->feval = 0;
				estabf(nullstr,fd);
				fd->_flag |= _IOEOF;
			}
		}
		if((f->fstak==0  || (states&FIXFLG)) && c != 0)
		{
			if((states&READPR) && aliflg==0)
				 putc(c,output);
			if((states&(FIXFLG)) && fileno(fd)!=F_STRING)
				putc(c,fc_fix->fixfd);
		}
		if(c==NL)
			f->flin++;
	}
	else if(feof(fd))
	{
		fd->_flag |= _IOEOF;
		c = ENDOF;
	}
	else
	{
		clearerr(fd);
		if(trapnote&SIGSET)
		{
			newline();
			exitsh(SIGFAIL);
		}
		else if((trapnote&TRAPSET) && (states&RWAIT))
		{
		trapfound:
			newline();
			chktrap();
			arg_clear();
		}
		else if(--maxtry > 0)
			goto retry;
		else
			fd->_flag |= _IOERR;
		c = ENDOF;
	}
#ifdef JOBS
#ifdef BSD
	if(--nreadc <=0)
		states &= ~READC;
#endif	/* BSD */
#endif	/* JOBS */
	return(c);
}

#ifdef JOBS
#ifdef BSD
/*
 * This routine is here because signals behave differently with sigset
 */

interrupt()
{
	register FILE *fd = input;
	clearerr(fd);
	if(trapnote&SIGSET)
	{
		newline();
		trapnote = 0;
		exitsh(SIGFAIL);
	}
	else if((trapnote&TRAPSET) && (states&RWAIT))
		longjmp(readerr,1);
}

#endif	/* BSD */
#endif	/* JOBS */

