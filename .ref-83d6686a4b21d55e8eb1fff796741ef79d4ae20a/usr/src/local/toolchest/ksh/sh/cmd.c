/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)cmd.c	1.1 */
/*
 * UNIX shell
 *
 * S. R. Bourne
 * Rewritten by David Korn
 * AT&T Bell Laboratories
 *
 */

#include	"defs.h"
#include	"sym.h"
#include	"flags.h"
#include	"name.h"
#include	"io.h"
#include	"history.h"
#include	"mode.h"
#include	"stak.h"
#include	"shtype.h"
#include	"brkincr.h"
#include	"builtins.h"


/* These routines are defined by this module */
void	synbad();
TREPTR	cmd();
TREPTR	makefork();

/* These routines are referenced by this module */
extern void	addblok();
extern void	chkpr();
extern void	exitsh();
extern void	free();
extern STKPTR	getstak();
extern void	hist_cancel();
extern void	hist_flush();
extern long	hist_position();
extern char	*malloc();
extern char	*movstr();
extern void	p_setout();
extern void	p_str();
extern void	p_prp();
extern void	p_num();

static TREPTR	makelist();
static ARGPTR	qscan();
static IOPTR	inout();
static void	chkword();
static void	chkflags();
static void	chksym();
static TREPTR	term();
static TREPTR	list();
static REGPTR	syncase();
static TREPTR	item();
static int	skipnl();
static void	prsym();


static int	heredoc;

/*
 * ========	command line decoding	========
 *
 *  This is the parser for a shell command line
 */




/*
 * Make a node which will cause the shell to fork
 */

TREPTR	makefork(flgs, i)
int 	flgs;
TREPTR		i;
{
	register FORKPTR	t;
	t=(FORKPTR) getstak(FORKTYPE);
	t->forktyp = flgs|TFORK;
	t->forktre = i;
	t->forkio = 0;
	return((TREPTR)t);
}

/*
 *  Make a node corresponding to a command list
 */

static TREPTR	makelist(type,i,r)
int 	type;
TREPTR		i, r;
{
	register LSTPTR	t;
	if(i==0 || r==0)
		synbad();
	else
	{
		t = (LSTPTR) getstak(LSTTYPE);
		t->lsttyp = type;
		t->lstlef = i;
		t->lstrit = r;
	}
	return((TREPTR)t);
}

/*
 * cmd
 *	empty
 *	list
 *	list & [ cmd ]
 *	list [ ; cmd ]
 */

TREPTR	cmd(sym,flg)
register int 	sym;
int 	flg;
{
	register int flag = FINT|FPRS|FAMP;
	register TREPTR	i, e;
	IOPTR saviotemp = iotemp;
	/* parser output goes on standard error */
	p_setout(stderr);
	i = list(flg);
	if(wdval==NL)
	{
		if(flg&NLFLG)
		{
			wdval=';';
			chkpr(0);
		}
	}
	else if(i==0 && (flg&MTFLG)==0)
		synbad();
	switch(wdval)
	{
		case COOPSYM:		/* set up a cooperating process */
			flag |= FPIN|FPOU;
		case '&':
			if(i)
			{
				if(saviotemp!=iotemp || heredoc)
					flag |= FTMP;
				i = (TREPTR)makefork(flag, i);
			}
			else
				 synbad();

		case ';':
			if(e=cmd(sym,flg|MTFLG))
				i=(TREPTR)makelist(TLST, i, e);
			break;

		case EOFSYM:
			if(sym==NL)
				break;

		default:
			if(sym)
				chksym(sym);

	}
	/* restore output stream */
	return(i);
}

/*
 * list
 *	term
 *	list && term
 *	list || term
 */

static TREPTR	list(flg)
{
	register TREPTR	r;
	register int 	b;
	r = term(flg);
	while(r && ((b=(wdval==ANDFSYM)) || wdval==ORFSYM))
	{
		r = makelist((b ? TAND : TORF), r, term(NLFLG));
	}
	return(r);
}

/*
 * term
 *	item
 *	item |^ term
 */

static TREPTR	term(flg)
register int flg;
{
	register TREPTR	t;
	register PARPTR	p = NULL;
	heredoc = 0;
	reserv++;
	if(flg&NLFLG)
		skipnl();
	else
		 word();
	/* check to see if pipeline is to be timed */
	if(wdval==TIMSYM)
	{
		p=(PARPTR) getstak(PARTYPE);
		p->partyp=TTIME;
		reserv++;
		word();
	}
	if((t=item(NLFLG|MTFLG)) && wdval=='|')
	{
		flg = heredoc|FPOU;
		t=makelist(TFIL,makefork(flg,t),makefork(FPIN|FPCL,term(NLFLG)));
	}
	if(p)
	{
		p->partre= t;
		return((TREPTR)p);
	}
	else
		return(t);
}

/*
 * case statement
 */

static REGPTR	syncase(esym)
register int esym;
{
	wdset |= E_FLAG; 	/* set to avoid aliasing expressions */
	skipnl();
	if(wdval==esym)
	{
		wdset &= ~E_FLAG;
		return(0);
	}
	else
	{
		register REGPTR	r=(REGPTR) getstak(REGTYPE);
		r->regptr=0;
		while(1)
		{
			chkflags(wdarg,1);
			wdarg->argnxt=r->regptr;
			r->regptr=wdarg;
			if(wdval==')' || wdval=='|' || ( word()!=')' && wdval!='|' ))
				synbad();
			if(wdval=='|')
				word();
			else
				break;
		}
		wdset &= ~E_FLAG;
		r->regcom=cmd(0,NLFLG|MTFLG);
		if(wdval==ECSYM)
			r->regnxt=syncase(esym);
		else
		{
			chksym(esym);
			r->regnxt=0;
		}
		return(r);
	}
}

/*
 * item
 *
 *	( cmd ) [ < in ] [ > out ]
 *	word word* [ < in ] [ > out ]
 *	if ... then ... else ... fi
 *	for ... while ... do ... done
 *	case ... in ... esac
 *	begin ... end
 */

static TREPTR	item(flag)
BOOL		flag;
{
	register TREPTR	t;
	register IOPTR	io;
	if(flag)
		io=inout((IOPTR)0,1);
	else
		io=0;
	switch(wdval)
	{
		/* case statement */
		case CASYM:
		{
			t=(TREPTR) getstak(SWTYPE);
			chkword();
			((SWPTR) t)->swarg=wdarg->argval;
			skipnl();
			chksym(INSYM|BRSYM);
			((SWPTR) t)->swlst=syncase(wdval==INSYM?ESSYM:KTSYM);
			((SWPTR) t)->swtyp=TSW;
			break;
		}

		/* if statement */
		case IFSYM:
		{
			register int w;
			t=(TREPTR) getstak(IFTYPE);
			((IFPTR) t)->iftyp=TIF;
			((IFPTR) t)->iftre=cmd(THSYM,NLFLG);
			((IFPTR) t)->thtre=cmd(ELSYM|FISYM|EFSYM,NLFLG);
			((IFPTR) t)->eltre=((w=wdval)==ELSYM?cmd(FISYM,NLFLG):
				(w==EFSYM?(wdval=IFSYM, item(0)):0));
			if(w==EFSYM)
				return(t);
			break;
		}

		/* for and select statement */
		case FORSYM:
		case SELSYM:
		{
			t=(TREPTR) getstak(FORTYPE);
			((FORPTR) t)->fortyp=(wdval==FORSYM?TFOR:TSELECT);
			((FORPTR) t)->forlst=0;
			chkword();
			((FORPTR) t)->fornam=(char*) wdarg->argval;
			if(skipnl()==INSYM)
			{
				chkword();
				 ((FORPTR) t)->forlst=(COMPTR) item(0);
				if(wdval!=NL && wdval!=';')
					synbad();
				if(wdval==NL)
					chkpr(0);
				skipnl();
			}
			/* 'for i;do cmd' is valid syntax */
			else if(wdval==';')
			{
				reserv = 1;
				word();
			}
			chksym(DOSYM|BRSYM);
			((FORPTR) t)->fortre=cmd(wdval==DOSYM?ODSYM:KTSYM,NLFLG);
			break;
		}

		/* This is the code for parsing function definitions */
		case PROCSYM:
		funct_5_2:
		{
			TREPTR cmdptr;
			BLKPTR blokptr;
			int savstates = states;
			int saveline = firstline;
			register FILE *fd = NULL;
			IOPTR saviotemp = iotemp;
			t=(TREPTR) getstak(PROCTYPE);
			((PROCPTR) t)->proctyp=TPROC;
			((PROCPTR) t)->procloc = -1;
			firstline = standin->flin;
			if(wdval == PROCSYM)
				chkword();
			((PROCPTR) t)->procnam=(char *) wdarg->argval;
			skipnl();
			chksym(BRSYM);
			/* force a new stak frame to compile the command */
			addblok(-1);
			if(is_option(INTFLG))
			{
				/* just in case history file not open yet */
				hist_open();
				if(fc_fix)
				{
					fd = fc_fix->fixfd;
					states |= FIXFLG;
					((PROCPTR)t)->procloc = 
						hist_position(fc_fix->fixind) +
						fd->_ptr - fd->_base;
				}
			}
			cmdptr = cmd(KTSYM,NLFLG);
			/* force another stak frame to save the command */
			addblok(-1);
			blokptr = stakbsy;
			stakbsy = stakbsy->word;
			/* save the entry point in block */
			blokptr->word = BLK(cmdptr);
			((PROCPTR) t)->proctre = blokptr;
			if(iotemp != saviotemp)
			{
				iotemp = saviotemp;
				states |= RM_TMP;
			}
			if(fd && (savstates&FIXFLG)==0)
			{
				hist_flush();
				hist_cancel();
				states &= ~FIXFLG;
			}
			firstline = saveline;
			break;
		}

		/* while and until */
		case WHSYM:
		case UNSYM:
		{
			t=(TREPTR) getstak(WHTYPE);
			((WHPTR) t)->whtyp=(wdval==WHSYM ? TWH : TUN);
			((WHPTR) t)->whtre = cmd(DOSYM,NLFLG);
			((WHPTR) t)->dotre = cmd(ODSYM,NLFLG);
			break;
		}

		/* command group with { */
		case BRSYM:
			t=cmd(KTSYM,NLFLG);
			break;

		case '(':
		{
			register PARPTR	 p;
			p=(PARPTR) getstak(PARTYPE);
			p->partre=cmd(')',NLFLG);
			p->partyp=TPAR;
			t=makefork(0,(TREPTR)p);
			break;
		}

		default:
			if(io==0)
				return(0);

		/* simple command */
		case 0:
		{
			register ARGPTR	argp;
			register ARGPTR	*argtail;
			register ARGPTR	*argset=0;
			int 	keywd=KEYFLG;
			int	argno = 0;
			int bltin = 0;
			t=(TREPTR) getstak(COMTYPE);
			((COMPTR)t)->comio=io; /*initial io chain*/
			/* set command line number for error messages */
			((COMPTR)t)->comline = (exec_flag?cmdline:
				standin->flin-firstline-1);
			argtail = &(((COMPTR)t)->comarg);
			while(wdval==0)
			{
				argp = wdarg;
				argp->argchn = 0;
				/* test for keyword argument */
				if(wdset&keywd)
				{
					chkflags(argp,0);
					argp->argnxt=(ARGPTR) argset;
					argset=(ARGPTR *) argp;
					/* alias substitutions allowed */
					wdset |= (KEYFLG|S_FLAG);
				}
				else
				{
					wdset = 0;	/* don't hunt for aliases*/
					chkflags(argp,1);
					if((argp->argflag&A_RAW) == 0)
						argno = -1;
					if(argno>=0 && argno++==0)
					{
						/* check for builtin command */
						bltin=syslook(argp->argval,commands);
					}
					*argtail = argp;
					argtail = &(argp->argnxt);
					wdset = keywd=is_option(KEYFLG);
				}
#ifdef DEVFD
			retry:
				word();
				if((wdval&STRIP)=='(')
				{
					TREPTR t;
					int flag = (wdval==OPROC);
					t = cmd(')',NLFLG|(argno==1&&wdval=='('?MTFLG:0));
					if(t == NULL)
					{
						wdarg = argp;
						goto funct_5_2;
					}
					argp = (ARGPTR)locstak();
					argno = -1;
					*argtail = argp;
					argtail = &(argp->argnxt);
					endstak(movstr(nullstr,argp->argval));
					argp->argchn = (ARGPTR)makefork(flag?FPIN|FAMP|FPCL:FPOU,t);
					argp->argflag =  (A_EXP|flag);
					goto retry;
				}
#else
				word();
				if(argno==1 && argset==NULL && wdval== '(')
				{
					/* SVR2 style function */
					word();
					if(wdval == ')')
					{
						wdarg = argp;
						goto funct_5_2;
					}
					wdval = '(';
				}
#endif	/* DEVFD */
				if(flag)
				{
					if(io)
					{
						while(io->ionxt)
							io = io->ionxt;
						io->ionxt = inout((IOPTR)0,0);
					}
					else
						((COMPTR)t)->comio = io = inout((IOPTR)0,0);
				}
			}
			*argtail = 0;
			((COMPTR)t)->comtyp = (TCOM|(bltin<<(COMBITS+1)));
			/* expand argument list if possible */
			if(argno>0)
				((COMPTR)t)->comarg = qscan(t,argno);
			else if(((COMPTR)t)->comarg)
				((COMPTR)t)->comtyp |= COMSCAN;
			((COMPTR)t)->comset=(ARGPTR) argset;
			wdset &= ~S_FLAG;
			return(t);
		}
	}
	reserv++;
	word();
	if(io=inout(io,0))
	{
		int type = t->tretyp&COMMSK;
		t=makefork(0,t);
		t->treio=io;
		if(type != TFORK)
			t->tretyp = TSETIO;
	}
	return(t);
}


/*
 * skip past newlines but issue prompt if interactive
 */

static int	skipnl()
{
	while((reserv++, word()==NL))
		chkpr(0);
	return(wdval);
}

/*
 * check for and process and i/o redirections
 * if flag is set then an alias can be in the next word
 */

static IOPTR	inout(lastio,flag)
IOPTR		lastio;
{
	register int 	iof;
	register IOPTR	iop;
	register int c;
	iof=wdnum;
	switch(wdval)
	{
		case DOCSYM:	/*	<<	*/
			iof |= IODOC;
			heredoc = FTMP;
			break;

		case APPSYM:	/*	>>	*/
		case '>':
			if(wdnum==0)
				iof |= 1;
			iof |= IOPUT;
			if(wdval==APPSYM)
			{
				iof |= IOAPP;
				break;
			}

		case '<':
			if((c=nextc())=='&')
				iof |= IOMOV;
			else if(c=='>')
			/*	<> is open for read and write	*/
			/*	unadvertised feature		*/
				iof |= IORDW;
			else
				 peekn=c|MARK;
			break;

		default:
			return(lastio);
	}
	chkword();
	iop=(IOPTR) getstak(IOTYPE);
	iop->ioname=wdarg->argval;
	iop->iofile=iof;
	if(iof&IODOC)
	{
		iop->iolst=iopend;
		iopend=iop;
	}
	word();
	iop->ionxt=inout(lastio,0);
	/* allow alias substitutions */
	if(flag)
		wdset |= S_FLAG;
	return(iop);
}

/*
 * get next token and make sure that it is not a keyword or meta-character
 */

static void	chkword()
{
	if(word())
		synbad();
}

/*
 * see if this token is syntactically correct
 */

static void	chksym(sym)
register int sym;
{
	register int 	x = sym&wdval;
	if(((x&SYMFLG) ? x : sym) != wdval)
		synbad();
}

/*
 * print the name of a syntactic token
 */

static void	prsym(sym)
register int sym;
{
	if(sym&SYMFLG)
	{
		register SYSPTR	sp=reserved;
		while(sp->sysval && sp->sysval!=sym)
			sp++;
		fputs(sp->sysnam,output);
	}
	else if(sym==EOFSYM)
		fputs(endoffile,output);
	else
	{
		if(sym&SYMREP)
			putc(sym,output);
		if(sym==NL)
			fputs("newline or ;",output);
		else
			putc(sym,output);
	}
	putc('\'',output);
}

/*
 * print a bad syntax message
 */

void	synbad()
{
	register char *cp = unexpected;
	register int w = wdval;
	p_setout(stderr);
	p_prp(synmsg,0);
	if((states&TTYFLG)==0)
	{
		fputs(atline,output);
		p_num((int)standin->flin,SP);
	}
	p_str(colon,'`');
	if(w)
		prsym(w);
	else
		p_str(wdarg->argval,'\'');
	if((w&EOFSYM) && w!=EOFSYM)
		cp = unmatched;
	p_str(cp,NL);
	hist_flush();
	exitsh(SYNBAD);
}

/*
 * check argument for possible optimizations
 * in many cases we can skip macro and file name expansion
 * The fexp flag is set when file expansion is possible
 */

#define EXP_MACRO	2	/* macro expansion needed */
#define EXP_TRIM	4	/* quoted characters in string */
#define EXP_FILE	8	/* file expansion characters*/
#define EXP_QUOTE	16	/* string contains " character */

static void chkflags(argp,fexp)
register ARGPTR argp;
{
	register int c;
	argp->argflag = 0;
	{
		register int flag = 0;
		char nquote = 0;
		char *sp=argp->argval;
		while(c= *sp++)
		{
			if(c==ESCAPE)
			{
				flag |= EXP_TRIM;
				sp++;
			}
			else if(isexp(c))
			{
				if(c == '$' || c == '`')
				{
					flag |= EXP_MACRO;
					if(c=='`')
						subflag++;
				}
				else if(nquote==0)
				{
					/* special case of '[' */
					if(*sp || c!='[')
						flag |= EXP_FILE;
				}
			}
			else if(c == '"')
			{
				/* toggle the quote count */
				nquote = 1 - nquote;
				flag |= EXP_QUOTE;
			}
		}
		if(fexp==0)
			flag &= ~EXP_FILE;
		/* return if no macro expansion, file expansion or trimming required */
		if(flag==0)
		{
			argp->argflag |= A_RAW;
			return;
		}
		/* return if macro or command substitution needed */
		if(flag&EXP_MACRO)
		{
			argp->argflag |= (A_MAC|A_EXP);
			return;
		}
		/* check to see if file expansion is required */
		if(flag&EXP_FILE)
		{
			argp->argflag|= A_EXP;
			/* return if no quotes otherwise don't optimize */
			if(flag&(EXP_QUOTE|EXP_TRIM))
			{
				argp->argflag= A_MAC;
				return;
			}
			return;
		}
		argp->argflag |= A_RAW;
	}
	/* just get rid of quoting stuff and consider argument as expanded */
	{
		register char *dp,*sp;
		char nquote = 0;	/* set within quoted string */
		dp = sp =  argp->argval;
		while(c= *sp++)
		{
			if(c != '"')
			{
				if(c==ESCAPE)
				{
					/* strip escchar's in double quotes */
					c = *sp++;
					if(nquote && !escchar(c) && c!='"')
						*dp++ = ESCAPE;
				}
				*dp++ = c;
			}
			else	/* toggle quote marker */
				nquote = 1-nquote;
		}
		*dp = 0;
	}
}

/*
 * convert argument chain to argument list when no special arguments
 */

static ARGPTR qscan(ac,argn)
COMPTR	ac;
int argn;
{
	register char **cp;
	register ARGPTR ap;
	register DOLPTR dp;
	/* leave space for an extra argument at the front */
	dp = (DOLPTR)getstak((unsigned)DOLTYPE + sizeof(char*) + argn*sizeof(char*));
	cp = dp->dolarg+1;
	dp->doluse = argn;
	ap = ac->comarg;
	while(ap)
	{
		*cp++ = ap->argval;
		ap = ap->argnxt;
	}
	*cp = NULL;
	return((ARGPTR)dp);
}

