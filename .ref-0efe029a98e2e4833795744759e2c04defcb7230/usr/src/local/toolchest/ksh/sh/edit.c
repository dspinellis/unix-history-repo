/* @(#)edit.c	1.1 */

/*
 *  edit.c - common routines for vi and emacs one line editors in shell
 *
 *   David Korn				P.D. Sullivan
 *   AT&T Bell Laboratories		AT&T Bell Laboratories
 *   Room 5D-112			Room 1E253
 *   Murray Hill, N. J. 07974		Columbus, OH 43213
 *   Tel. x7975				Tel. x 2655
 *
 *   Coded April 1983.
 */

#include	<errno.h>

#ifdef KSHELL
#include	"shtype.h"
#include	"flags.h"
#include	"defs.h"
#include	"io.h"
#include	"name.h"
#include	"sym.h"
#include	"stak.h"
#include	"mode.h"
#include	"builtins.h"
#include	"brkincr.h"

#else
#include	<stdio.h>
#include	<signal.h>
#include	<setjmp.h>
#include	<ctype.h>
#endif	/* KSHELL */

#include	"history.h"
#include	"edit.h"

#define BAD	-1
#define GOOD	0
#define	TRUE	(-1)
#define	FALSE	0
#define	SYSERR	-1

#ifdef VENIX
#define RT 1
#endif	/* VENIX */

void	e_crlf();
void	e_flush();
int	e_getchar();
void	e_putchar();
void	e_ringbell();
void	e_setup();
int	e_virt_to_phys();
int	e_window();
void	setcooked();
void	ungetchar();

#if BSD || RT
#include	<sgtty.h>
# ifdef BSD_4_2
# include	<sys/time.h>
# endif /* BSD_4_2 */
static struct sgttyb ttyparm;		/* initial tty parameters */
static struct sgttyb nttyparm;		/* raw tty parameters */
# ifdef BSD
extern int tty_speeds[];
static int delay;
static int l_mask;
static struct tchars l_ttychars;
static struct ltchars l_chars;
static  char  l_changed;	/* set if mode bits changed */
#define L_CHARS	4
#define T_CHARS	2
#define L_MASK	1
# endif /* BSD */
#else
# ifdef XENIX	/* avoid symbol table overflows */
# define NCC 8
struct termio {
	unsigned short	c_iflag;	/* input modes */
	unsigned short	c_oflag;	/* output modes */
	unsigned short	c_cflag;	/* control modes */
	unsigned short	c_lflag;	/* line discipline modes */
	char		c_line;		/* line discipline */
	unsigned char	c_cc[NCC];	/* control chars */
	char		c_res;		/* padding, AFTER the array */
};
#define TIOC	('T'<<8)
#define	BRKINT	0000002
#define	CBAUD	0000017
#define	ECHO	0000010
#define	ECHOE	0000020
#define	ECHOK	0000040
#define	ECHONL	0000100
#define	ICANON	0000002
#define	ICRNL	0000400
#define	IGNCR	0000200
#define	IGNPAR	0000004
#define	INLCR	0000100
#define	PARMRK	0000010
#define	TCGETA	(TIOC|1)
#define	TCSETAW	(TIOC|3)
#define	TCXONC	(TIOC|6)
#define	VEOF	4
#define	VERASE	2
#define	VKILL	3
#define	VMIN	4
#define	VTIME	5
#define TM_CECHO	0010
# else
# include	<termio.h>
# endif /* XENIX */

static struct termio ttyparm;		/* initial tty parameters */
static struct termio nttyparm;		/* raw tty parameters */
#endif	/* RT */

#define lookahead	editb.e_index
#define env		editb.e_env
#define previous	editb.e_lbuf
#define fildes		editb.e_fd

#ifdef KSHELL
extern struct Amemory	*alias;
extern char		**arg_build();
extern void		failed();
extern void		fault();
extern struct Namnod	*findnod();
extern int		f_complete();
extern void		gsort();
extern char		*movstr();
extern void		p_flush();
extern void		p_setout();
extern char		*simple();
extern char		*tilde();
extern char		*valup();
static char macro[]	= "_?";

#else
static char badcooked[]	= "cannot reset tty to cooked mode";
struct edit editb;
char trapnote;
extern char trapnote;
extern int errno;
#define p_flush()	fflush(stderr)
#define p_setout(s)	fflush(stdout)
#define error(s)	failed(s,NULL)
#define SIGSLOW 1
#define output	stderr
#endif	/* KSHELL */

extern char *strrchr();
extern char *strcpy();

static char bellchr[] = "\7";		/* bell char */

static int control();
/*{	SETCOOKED( fd )
 *
 *	This routine will set the tty in cooked mode.
 * It is also called by error.done().
 *
}*/

void setcooked(fd)
register int fd;
{
	/*** don't do ioctl unless ttyparm has valid data ***/
	/* or in raw mode */

	if(editb.e_ttyspeed==0 || editb.e_raw==0)
		return;

#ifdef BSD
	if(editb.e_raw==RAWMODE && ioctl(fd, TIOCSETN, &ttyparm) == SYSERR )
	{
		if(errno!=EBADF && errno!=ENOTTY)
			error(badcooked);
		return;
	}
	/* restore flags */
	if(l_changed&L_MASK)
		ioctl(fd,TIOCLSET,&l_mask);
	if(l_changed&T_CHARS)
		/* restore alternate break character */
		ioctl(fd,TIOCSETC,&l_ttychars);
	if(l_changed&L_CHARS)
		/* restore alternate break character */
		ioctl(fd,TIOCSLTC,&l_chars);
	l_changed = 0;
#else
# ifdef RT
	if (stty(fd,&ttyparm) == SYSERR)
# else
	if( editb.e_raw && control(fd, TCSETAW, &ttyparm) == SYSERR )
# endif /* RT */
	{
		if(errno!=EBADF && errno!=ENOTTY)
			error(badcooked);
		return;
	}
#endif	/* BSD */
	editb.e_raw = 0;
	return;
}

/*{	SETRAW( fd )
 *
 *	This routine will set the tty in raw mode.
 *
}*/

setraw(fd)
register int fd;
{
#ifdef BSD
	struct ltchars lchars;
#endif	/* BSD */
	if(editb.e_raw==RAWMODE)
		return(GOOD);
	/* characters are echoed on standard error */
	p_setout(stderr);
#if BSD || RT
# ifdef BSD
	if((ioctl(fd,TIOCGETP,&ttyparm) == SYSERR)
# else
	if ((gtty(fd,&ttyparm) == SYSERR)
# endif /* BSD */
		|| !(ttyparm.sg_flags & ECHO ) 
		|| (ttyparm.sg_flags & LCASE ))
	{
		return(BAD);
	}
	nttyparm = ttyparm;
	nttyparm.sg_flags &= ~(ECHO | TBDELAY);
# ifdef BSD
	nttyparm.sg_flags |= CBREAK;
# else
	nttyparm.sg_flags |= RAW;
# endif /* BSD */
	editb.e_erase = ttyparm.sg_erase;
	editb.e_kill = ttyparm.sg_kill;
	editb.e_eof = cntl(D);
# ifdef BSD
	if( ioctl(fd, TIOCSETN, &nttyparm) == SYSERR )
# else
	if( stty(fd, &nttyparm) == SYSERR )
# endif /* BSD */
	{
		return(BAD);
	}
	editb.e_ttyspeed = (ttyparm.sg_ospeed>=B1200?FAST:SLOW);
#ifdef BSD
	delay = tty_speeds[ttyparm.sg_ospeed];
	/* try to remove effect of ^V  and ^Y */
	if(ioctl(fd,TIOCGLTC,&l_chars) != SYSERR)
	{
		lchars = l_chars;
		lchars.t_lnextc = -1;
		lchars.t_dsuspc = -1;	/* no delayed stop process signal */
		if(ioctl(fd,TIOCSLTC,&lchars) != SYSERR)
			l_changed |= L_CHARS;
	}
#endif	/* BSD */
#else

# ifndef RAWONLY
	if(editb.e_raw != ALTMODE)
# endif /* RAWONLY */
		if( (ioctl(fd, TCGETA, &ttyparm) == SYSERR)
			|| (!(ttyparm.c_lflag & ECHO )))
		{
			return(BAD);
		}

	nttyparm = ttyparm;
#ifndef u370
	nttyparm.c_iflag &= ~(IGNPAR|PARMRK|INLCR|IGNCR|ICRNL);
	nttyparm.c_iflag |= BRKINT;
	nttyparm.c_lflag &= ~(ICANON|ECHO);
#else
	nttyparm.c_iflag &= 
			~(IGNBRK|PARMRK|INLCR|IGNCR|ICRNL|INPCK);
	nttyparm.c_iflag |= (BRKINT|IGNPAR);
	nttyparm.c_lflag &= ~(ICANON|ECHO);
#endif	/* u370 */
	nttyparm.c_cc[VTIME] = 0;
	nttyparm.c_cc[VMIN] = 1;
	editb.e_eof = ttyparm.c_cc[VEOF];
	editb.e_erase = ttyparm.c_cc[VERASE];
	editb.e_kill = ttyparm.c_cc[VKILL];
#ifndef u370
	if( control(fd, TCSETAW, &nttyparm) == SYSERR )
#else
	/* delays are too long, don't wait for output to drain */
	if( control(fd, TCSETA, &nttyparm) == SYSERR )
#endif	/* u370 */
	{
		return(BAD);
	}
	control(fd,TCXONC,1);
	editb.e_ttyspeed = ttyparm.c_cflag & CBAUD;
#endif
	editb.e_raw = RAWMODE;
	return(GOOD);
}

#ifndef BSD
/*
 * give two tries for ioctl
 * interrupts are ignored
 */

static int control(fd,request,arg)
{
	register int i;
	register int k = 2;
	errno = 0;
	while(k--)
	{
		if((i = ioctl(fd,request,arg)) != SYSERR)
			return(i);
		if(errno == EINTR)
		{
			errno = 0;
			k++;
		}
	}
	return(SYSERR);
}
#endif	/* BSD */
#ifndef RAWONLY

/*	SET_TTY( fd )
 *
 *	Get tty parameters and make ESC and '\r' wakeup characters.
 *
 */

#ifdef BSD
setalt(fd)
register int fd;
{
	int mask;
	struct tchars ttychars;
	if(editb.e_raw==ALTMODE)
		return(GOOD);
	if(editb.e_raw==RAWMODE)
		setcooked(fd);
	l_changed = 0;
	if( editb.e_ttyspeed == 0)
	{
		if((ioctl(fd,TIOCGETP,&ttyparm) != SYSERR))
			editb.e_ttyspeed = (ttyparm.sg_ospeed>=B1200?FAST:SLOW);
	}
	if(ioctl(fd,TIOCGETC,&l_ttychars) == SYSERR)
		return(BAD);
	if(ioctl(fd,TIOCLGET,&l_mask)==SYSERR)
		return(BAD);
	ttychars = l_ttychars;
	mask =  LCRTBS|LCRTERA|LCTLECH|LPENDIN|LCRTKIL;
	if((l_mask|mask) != l_mask)
		l_changed = L_MASK;
	if(ioctl(fd,TIOCLBIS,&mask)==SYSERR)
		return(BAD);
	ttychars.t_brkc = ESC;
	l_changed |= T_CHARS;
	if(ioctl(fd,TIOCSETC,&ttychars) == SYSERR)
		return(BAD);
	editb.e_raw = ALTMODE;
	return(GOOD);
}
#else
setalt(fd)
register int fd;
{
	if(editb.e_raw==ALTMODE)
		return(GOOD);
	if(editb.e_raw==RAWMODE)
		setcooked(fd);
	if( (control(fd, TCGETA, &ttyparm) == SYSERR)
		|| (!(ttyparm.c_lflag & ECHO )))
	{
		return(BAD);
	}
	nttyparm = ttyparm;
	nttyparm.c_iflag &= ~(IGNCR|ICRNL);
	nttyparm.c_iflag |= INLCR;
	nttyparm.c_lflag |= (ECHOE|ECHOK);
	nttyparm.c_cc[VEOF] = ESC;	/* make ESC the eof char */
	nttyparm.c_cc[VEOL] = '\r';	/* make CR an eol char */
	editb.e_eof = ttyparm.c_cc[VEOF];
	nttyparm.c_cc[VEOL2] = editb.e_eof;	/* make EOF an eol char */
	editb.e_erase = ttyparm.c_cc[VERASE];
	editb.e_kill = ttyparm.c_cc[VKILL];
	if( control(fd, TCSETAW, &nttyparm) == SYSERR )
	{
		return(BAD);
	}
	editb.e_ttyspeed = ((ttyparm.c_cflag&CBAUD)>=B1200?FAST:SLOW);
	editb.e_raw = ALTMODE;
	return(GOOD);
}

#endif	/* BSD */
#endif	/* RAWONLY */

/*
 *	E_WINDOW()
 *
 *	return the window size
 */

int e_window()
{
	register int n = DFLTWINDOW-1;
	register char *cp = valup(COLUMNS);
	if(cp)
	{
		n = atoi(cp)-1;
		if(n < MINWINDOW)
			n = MINWINDOW;
		if(n > MAXWINDOW)
			n = MAXWINDOW;
	}
	return(n);
}

/*	E_FLUSH()
 *
 *	Flush the output buffer.
 *
 */

void e_flush()
{
	register unsigned char *buf = (unsigned char*)output->_base;
	register int n = editb.e_outptr-buf;
	register int fd = fileno(output);
	if(n<=0)
		return;
	write(fd,(char*)buf,n);
	editb.e_outptr = buf;
#ifdef BSD
# ifdef BSD_4_2
	if(delay && n > delay/100)
	{
		/* delay until output drains */
		struct timeval timeloc;
		n *= 10;
		timeloc.tv_sec = n/delay;
		timeloc.tv_usec = (1000000*(n%delay))/delay;
		select(0,0,0,0,&timeloc);
	}
# endif /* BSD_4_2 */
#else
# ifndef RT
	if(editb.e_raw==RAWMODE && n > 16)
		ioctl(fd, TCSETAW, &nttyparm);
# endif /* RT */
#endif /* BSD */
}

/*
 * send the bell character ^G to the terminal
 */

void e_ringbell()
{
	write(fileno(output),bellchr,1);
}

/*
 * send a carriage return line feed to the terminal
 */

void e_crlf()
{
#ifdef u370
	e_putchar('\r');
#endif	/* u370 */
#ifdef VENIX
	e_putchar('\r');
#endif /* VENIX */
	e_putchar('\n');
	e_flush();
}
 
/*	E_SETUP( max_prompt_size )
 *
 *	This routine sets up the prompt string
 */

void	e_setup(fd,PRSIZE)
{
	register char *last;
	register char *pp = (char*)(output->_base);
	char *ppmax;
	editb.e_fd = fd;
	if(fc_fix)
	{
		register struct fixcmd *fp = fc_fix;
		editb.e_hismax = fp->fixind;
		editb.e_hloff = fp->fixline;
		editb.e_hismin = fp->fixind-fp->fixmax;
		if(editb.e_hismin<0)
			editb.e_hismin = 0;
	}
	else
	{
		editb.e_hismax = editb.e_hismin = editb.e_hloff = 0;
	}
	editb.e_hline = editb.e_hismax;
	editb.e_wsize = e_window()-2;
	editb.e_outptr = (unsigned char*)pp;
	editb.e_crlf = YES;
	*(output->_ptr) = 0;
	if((last=strrchr(pp,'\n'))==NULL)
	{
		if(*(last=pp)==0)
			editb.e_crlf = NO;
	}
	else
		last++;
	pp = editb.e_prompt;
	ppmax = pp+PRSIZE-1;
	*pp++ = '\r';
	{
		register int c;
		while(c = *last++)
		{
			/* cut out bells */
			if(c!=BELL)
			{
				if(pp < ppmax)
					*pp++ = c;
				if(!isprint(c))
					editb.e_crlf = NO;
			}
		}
	}
	editb.e_plen = pp - editb.e_prompt - 1;
	*pp = 0;
#ifdef u370
	if (editb.e_raw == RAWMODE)
		u370fflush(output);
	else
#endif	/* u370 */
	p_flush();
}

#ifdef u370
/* The u370 does not \r before \n in raw mode (known bug).
	  To get around this we will insert a \r before each \n
	  in the output buffer.
*/

u370fflush(file)
FILE *file;
{
	unsigned char *base,*ptr,buffer[BUFSIZ*2];
	int icnt,ocnt;
	ptr = buffer;
	icnt = file->_ptr - file->_base;
	ocnt = icnt;
	base = file->_base ;
	if (icnt <= 0) return;
	while (base < file->_ptr)
	{
		if (*base == '\n' )
		{
			*ptr++ = '\r';
			ocnt++;
		}
		*ptr++ = *base++;
	}
	base = file->_base;
	ptr = buffer;
	while (ocnt>0)
	{
		for(icnt=0;icnt<BUFSIZ;icnt++)
		{
			*base++ = *ptr++;
			if (--ocnt <= 0) break;
		}
		file->_ptr = base;
		p_flush();
		base = file->_base;
	}
}
#endif	/* u370 */

#ifdef KSHELL
/*
 * look for edit macro named _i
 * if found, puts the macro definition into lookahead buffer and returns 1
 */

e_macro(i)
register int i;
{
	register char *out;
	struct Namnod *np;
	genchar buff[LOOKAHEAD+1];
	if(i != '@')
		macro[1] = i;
	if (isalnum(i)&&(np=findnod(macro,alias,0))&&(out=valup(np)))
	{
#ifdef MULTIBYTE
		/* copy to buff in internal representation */
		int c = out[LOOKAHEAD];
		out[LOOKAHEAD] = 0;
		i = e_internal(out,buff);
		out[LOOKAHEAD] = c;
#else
		strncpy((char*)buff,out,LOOKAHEAD);
		i = strlen((char*)buff);
#endif /* MULTIBYTE */
		while(i-- > 0)
			ungetchar(buff[i]);
		return(1);
	} 
	return(0);
}
/*
 * file name generation for edit modes
 * non-zero exit for error, <0 ring bell
 * don't search back past <start> character of the buffer
 * mode is '*' for inline expansion, otherwise files are listed in select format
 */

q_expand(outbuff,cur,eol,start,mode)
char outbuff[];
int *cur;
int *eol;
int start;
int mode;
{
	STKPTR staksav = stakbot;
	COMPTR  comptr = (COMPTR)getstak(COMTYPE);
	ARGPTR ap = (ARGPTR)locstak();
	register char *out;
	char *outmin = outbuff + start;
	char *begin;
	char *last;
	int rval = 0;
	int strip;
	optflag savflags = flags;
#ifdef MULTIBYTE
	{
		register int c = *cur;
		register genchar *cp;
		/* adjust cur */
		cp = (genchar *)outbuff + *cur;
		c = *cp;
		*cp = 0;
		*cur = e_external((genchar*)outbuff,(char*)stakbot);
		*cp = c;
		*eol = e_external((genchar*)outbuff,outbuff);
	}
#endif /* MULTIBYTE */
	out = outbuff + *cur;
	comptr->comtyp = COMSCAN;
	comptr->comarg = ap;
	ap->argflag = (A_MAC|A_EXP);
	ap->argnxt = 0;
	{
		register int c;
		register char *ptr = ap->argval;
		int chktilde = 0;
		int flag;
		char *cp;
		if(out>outmin)
		{
			/* go to beginning of word */
			do
			{
				out--;
				c = *(unsigned char*)out;
			}
			while(out>outmin && !isqmeta(c));
			/* copy word into arg */
			if(isqmeta(c))
				out++;
		}
		else
			out = outmin;
		begin = out;
		flag = '*';
		strip = TRUE;
		/* copy word to arg and do ~ expansion */
		do
		{
			c = *(unsigned char*)out++;
			if(isexp(c))
				flag = 0;
			if ((c == '/') && (flag == 0))
				strip = FALSE;
			*ptr++ = c;
			if(chktilde==0 && (c==0 || c == '/'))
			{
				chktilde++;
				if(cp=tilde(begin))
				{
					ptr = movstr(cp,ap->argval);
					*ptr++ = c;
				}
			}

		} while (c && !isqmeta(c));

		out--;
		*(ptr-1) = flag;
		endstak(ptr);
		last = ptr-1;
	}
	if(mode!='*')
		on_option(MARKDIR);
	{
		register char **com;
		int	 narg;
		register int size;
		while(1)
		{
			com = arg_build(&narg,comptr);
			/*  match? */
			if (narg > 1 || !eq(ap->argval,*com))
				break;
			if (*last == 0)
				*last = '*';
			else
			{
				rval = -1;
				goto done;
			}
		}
		if(mode!='*')
		{
			if (strip)
			{
				register char **ptrcom;
				for(ptrcom=com;*ptrcom;ptrcom++)
					/* trim directory prefix */
					*ptrcom = simple (*ptrcom);
			}
			p_setout(stderr);
			newline();
			p_list(narg,com);
			p_flush();
			goto done;
		}
		/* see if there is enough room */
		size = *eol - (out-begin);
		size += narg;
		{
			char **savcom = com;
			while (*com)
				size += strlen(*com++);
			com = savcom;
		}
		/* see if room for expansion */
		if(outbuff+size >= &outbuff[MAXLINE])
		{
			com[0] = ap->argval;
			com[1] = NULL;
		}
		/* save remainder of the buffer */
		strcpy(stakbot,out);
		out = begin;
		while (*com)
		{
			out = movstr(*com,out);
			if (*++com)
				*out++  = ' ';
		}
		*cur = (out-outbuff);
		/* restore rest of buffer */
		out = movstr(stakbot,out);
		*eol = (out-outbuff);
	}
 done:
	tdystak(staksav);
	flags = savflags;
#ifdef MULTIBYTE
	{
		register int c;
		/* first re-adjust cur */
		out = outbuff + *cur;
		c = *out;
		*out = 0;
		*cur = e_internal(outbuff,(genchar*)stakbot);
		*out = c;
		outbuff[*eol+1] = 0;
		*eol = e_internal(outbuff,(genchar*)outbuff);
	}
#endif /* MULTIBYTE */
	return(rval);
}
#endif	/* KSHELL */
 

/*
 * routine to perform read from terminal for vi and emacs mode
 */


int 
e_getchar()
{
	register int i;
	register int c;
	register int maxtry = 100;
	int nchar;	/* number of characters to read at a time */
#ifdef MULTIBYTE
	static int curchar;
	static int cursize;
#endif /* MULTIBYTE */
	char readin[LOOKAHEAD] ;
	if (lookahead)
	{
		c = previous[--lookahead];
		/*** map '\r' to '\n' ***/
		if(c == '\r')
			c = '\n';
		return(c);
	}
	
	e_flush() ;
	/* you can't chance read ahead at the end of line */
	nchar = (editb.e_cur>=editb.e_eol?1:READAHEAD);
	/* Set 'i' to indicate read failed, in case intr set */
retry:
	i = -1;
	errno = 0;
	editb.e_inmacro = 0;
	while((trapnote&SIGSLOW)==0 && maxtry--)
	{
		errno=0;
		if ((i = read(fildes,readin, nchar)) != -1)
			break;
	}
#ifdef MULTIBYTE
	lookahead = maxtry = i;
	i = 0;
	while (i < maxtry)
	{
		c = readin[i++] & STRIP;
	next:
		if(cursize-- > 0)
		{
			curchar = (curchar<<7) | (c&~HIGHBIT);
			if(cursize==0)
			{
				c = curchar;
				goto gotit;
			}
			else if(i>=maxtry)
				goto retry;
			continue;
		}
		else if(curchar = echarset(c))
		{
			cursize = in_csize(curchar);
			if(curchar != 1)
				c = 0;
			curchar <<= 7*(ESS_MAXCHAR-cursize);
			if(c)
				goto next;
			else if(i>=maxtry)
				goto retry;
			continue;
		}
	gotit:
		previous[--lookahead] = c;
#else
	while (i > 0)
	{
		c = readin[--i] & STRIP;
		previous[lookahead++] = c;
#endif /* MULTIBYTE */
#ifndef BSD
		if( c == '\0' )
		{
			/*** user break key ***/
			lookahead = 0;
# ifdef KSHELL
			fault(SIGINT);
			longjmp(env, UINTR);
# endif	/* KSHELL */
		}
#endif	/* !BSD */
	}
#ifdef MULTIBYTE
	/* shift lookahead buffer if necessary */
	if(lookahead)
	{
		for(i=lookahead;i < maxtry;i++)
			previous[i-lookahead] = previous[i];
	}
	lookahead = maxtry-lookahead;
#endif /* MULTIBYTE */
	if (lookahead > 0)
		return(e_getchar(1));
	longjmp(env,(i==0?UEOF:UINTR)); /* What a mess! Give up */
	/* NOTREACHED */
}

void ungetchar(c)
register int c;
{
	if (lookahead < LOOKAHEAD)
		previous[lookahead++] = c;
	return;
}

/*
 * put a character into the output buffer
 */

void	e_putchar(c)
register int c;
{
	register unsigned char *dp = editb.e_outptr;
#ifdef MULTIBYTE
	register int d;
	/* check for place holder */
	if(c == MARKER)
		return;
	if(d = icharset(c))
	{
		if(d == 2)
			*dp++ = ESS2;
		else if(d == 3)
			*dp++ = ESS3;
		d = in_csize(d);
		while(--d>0)
			*dp++ = HIGHBIT|(c>>(7*d));
		c |= HIGHBIT;
	}
#endif	/* MULTIBYTE */
	if (c == '_')
	{
		*dp++ = ' ';
		*dp++ = '\b';
	}
	*dp++ = c;
	*dp = '\0';
	if ((dp - (unsigned char*)(output->_base))>=(BUFSIZ-3))
		e_flush();
	else
		editb.e_outptr = dp;
}

/*
 * copy virtual to physical and return the index for cursor in physical buffer
 */
e_virt_to_phys(virt,phys,cur,voff,poff)
genchar *virt;
genchar *phys;
int cur;
{
	register genchar *sp = virt;
	register genchar *dp = phys;
	register int c;
	genchar *curp = sp + cur;
	genchar *dpmax = phys+MAXLINE;
	int r = 0;
#ifdef MULTIBYTE
	int d;
#endif /* MULTIBYTE */
	sp += voff;
	dp += poff;
	for(r=poff;c= *sp;sp++)
	{
		if(curp == sp)
			r = dp - phys;
#ifdef MULTIBYTE
		d = out_csize(icharset(c));
		if(d>1)
		{
			/* multiple width character put in place holders */
			*dp++ = c;
			while(--d >0)
				*dp++ = MARKER;
			/* in vi mode the cursor is at the last character */
			if(dp>=dpmax)
				break;
			continue;
		}
		else
#endif	/* MULTIBYTE */
		if(!isprint(c))
		{
			if(c=='\t')
			{
				c = dp-phys;
				c = ((c+8)&~07) - c;
				while(--c>0)
					*dp++ = ' ';
				c = ' ';
			}
			else
			{
				*dp++ = '^';
				c ^= TO_PRINT;
			}
			/* in vi mode the cursor is at the last character */
			if(curp == sp && is_option(EDITVI))
				r = dp - phys;
		}
		*dp++ = c;
		if(dp>=dpmax)
			break;
	}
	*dp = 0;
	return(r);
}

#ifdef MULTIBYTE
/*
 * convert external representation <src> to an array of genchars <dest>
 * <src> and <dest> can be the same
 * returns number of chars in dest
 */

int	e_internal(src,dest)
register unsigned char *src;
genchar *dest;
{
	register int c;
	register genchar *dp = dest;
	register int d;
	register int size;
	if((unsigned char*)dest == src)
	{
		genchar buffer[MAXLINE];
		c = e_internal(src,buffer);
		e_gencpy(dp,buffer);
		return(c);
	}
	while(c = *src++)
	{
		if(size = echarset(c))
		{
			d = (size==1?c:0);
			c = size;
			size = in_csize(c);
			c <<= 7*(ESS_MAXCHAR-size);
			if(d)
			{
				size--;
				c = (c<<7) | (d&~HIGHBIT);
			}
			while(size-- >0)
				c = (c<<7) | ((*src++)&~HIGHBIT);
		}
		*dp++ = c;
	}
	*dp = 0;
	return(dp-dest);
}

/*
 * convert internal representation <src> into character array <dest>.
 * The <src> and <dest> may be the same.
 * returns number of chars in dest.
 */

int	e_external(src,dest)
genchar *src;
char *dest;
{
	register int c;
	register char *dp = dest;
	register int d;
	char *dpmax = dp+sizeof(genchar)*MAXLINE-2;
	if((char*)src == dp)
	{
		char buffer[MAXLINE*sizeof(genchar)];
		c = e_external(src,buffer);
		strcpy(dest,buffer);
		return(c);
	}
	while((c = *src++) && dp<dpmax)
	{
		if(d = icharset(c))
		{
			if(d == 2)
				*dp++ = ESS2;
			else if(d == 3)
				*dp++ = ESS3;
			d = in_csize(d);
			while(--d>0)
				*dp++ = HIGHBIT|(c>>(7*d));
			c |= HIGHBIT;
		}
		*dp++ = c;
	}
	*dp = 0;
	return(dp-dest);
}

/*
 * copy <sp> to <dp>
 */

int	e_gencpy(dp,sp)
register genchar *dp;
register genchar *sp;
{
	while(*dp++ = *sp++);
}

/*
 * copy at most <n> items from <sp> to <dp>
 */

int	e_genncpy(dp,sp, n)
register genchar *dp;
register genchar *sp;
register int n;
{
	while(n-->0 && (*dp++ = *sp++));
}

/*
 * find the string length of <str>
 */

int	e_genlen(str)
register genchar *str;
{
	register genchar *sp = str;
	while(*sp++);
	return(sp-str-1);
}
#endif /* MULTIBYTE */
