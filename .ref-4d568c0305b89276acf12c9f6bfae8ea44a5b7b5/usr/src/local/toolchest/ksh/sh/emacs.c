/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)emacs.c	1.1 */
/* Adapted for ksh by David Korn */
/* EMACS_MODES: c tabstop=4 

One line screen editor for any program


Questions and comments should be
directed to 

	Michael T. Veach
	IX 1C-341 X1614
	ihuxl!veach

*/
#ifdef	DMERT	/* 3bcc #undefs RT */
#define	RT
#endif

#ifdef KSHELL
#include	"defs.h"
#include	"io.h"
#include	"shtype.h"

#else
#include	<setjmp.h>
#include	<stdio.h>
#include	<signal.h>
#include	<ctype.h>
#endif	/* KSHELL */

#include	"history.h"
#include	"edit.h"

#undef blank
#undef putchar
#define putchar(c)	e_putchar(c)
#define beep()		e_ringbell()


#ifdef KSHELL
extern void p_flush();
extern char *valup();

#else
static char version[] = "@(#)Editlib version 06/03/86";
extern unsigned char *_sobuf;
#define p_flush()	fflush(stderr)
#define output		stderr
#endif	/* KSHELL */

#ifdef MULTIBYTE
#define gencpy(a,b)	e_gencpy(a,b)
#define genncpy(a,b,n)	e_genncpy(a,b,n)
#define genlen(str)	e_genlen(str)
static int	print();
static int	isword();

#else
#define gencpy(a,b)	strcpy((char*)(a),(char*)(b))
#define genncpy(a,b,n)	strncpy((char*)(a),(char*)(b),n)
#define genlen(str)	strlen(str)
#define print(c)	isprint(c)
#define isword(c)	isalnum(out[c])
#endif /*MULTIBYTE */

extern histloc hist_find();
extern histloc hist_locate();
extern char	*hist_word();
extern char	*itos();
extern char	*strcpy();
extern char	*strncpy();
extern void	e_flush();
extern int	e_getchar();
extern void	e_putchar();
extern void	ungetchar();

#define eol		editb.e_eol
#define cur		editb.e_cur
#define mark		editb.e_fchar
#define hline		editb.e_hline
#define hloff		editb.e_hloff
#define hismin		editb.e_hismin
#define usrkill		editb.e_kill
#define usreof		editb.e_eof
#define usrerase	editb.e_erase
#define crallowed	editb.e_crlf
#define llimit		editb.e_llimit
#define Prompt		editb.e_prompt
#define plen		editb.e_plen
#define kstack		editb.e_tmp
#define lstring		editb.e_search
#define lookahead	editb.e_index
#define env		editb.e_env
#define raw		editb.e_raw
#define histlines	editb.e_hismax
#define w_size		editb.e_wsize
#define drawbuff	editb.e_inbuf
#define NO	0
#define YES	1
#define LBUF	100
#define KILLCHAR	UKILL
#define ERASECHAR	UERASE
#define EOFCHAR		UEOF

/**********************
A large lookahead helps when the user is inserting
characters in the middle of the line.
************************/


static genchar *screen;		/* pointer to window buffer */
static genchar *cursor;		/* Cursor in real screen */
static enum
{
	CRT=0,	/* Crt terminal */
	PAPER	/* Paper terminal */
} terminal ;

typedef enum
{
	FIRST,		/* First time thru for logical line, prompt on screen */
	REFRESH,	/* Redraw entire screen */
	APPEND,		/* Append char before cursor to screen */
	UPDATE,		/* Update the screen as need be */
	FINAL		/* Update screen even if pending look ahead */
} DRAWTYPE;

static void draw();
static int escape();
static void putstring();
static int search();
static void setcursor();

static int cr_ok;

hread(fd,buff,scend)
char *buff;
int fd,scend;
{
	register int c;
	register int i;
	register genchar *out;
	register int count;
	int adjust,oadjust;
	char backslash;
	genchar *kptr;
static	histloc location;
static int CntrlO;
	char prompt[LBUF];
	genchar stack[MAXLINE];
	char string[LBUF*CHARSIZE];
	genchar Screen[MAXWINDOW];
	Prompt = prompt;
	kstack = stack;
	lstring = string;
	screen = Screen;
	drawbuff = out = (genchar*)buff;
	kstack[0] = '\0';
	if(setraw(fd) < 0)
	{
		 p_flush();
		 return(read(fd,buff,scend));
	}
	raw = 1;
	/* This mess in case the read system call fails */
	
	e_setup(fd,LBUF);
	if ((i=setjmp(env))!=0)
	{
		setcooked(fd);
		if (i == UEOF)
		{
			return(0); /* EOF */
		}
		return(-1); /* some other error */
	}
#ifdef MULTIBYTE
	plen = e_internal(&Prompt[1],out);  /* Skip the leading \r */
#else
	gencpy(buff,&Prompt[1]);  /* Skip the leading \r */
#endif	/* MULTIBYTE */
	scend -= plen;
	llimit = scend;
	mark = eol = cur = plen;
	draw(FIRST);
	adjust = -1;
	backslash = 0;
	if (CntrlO)
	{
		location = hist_locate(location.his_command,location.his_line,1);
		if (location.his_command < histlines)
		{
			hline = location.his_command;
			hloff = location.his_line;
			hist_copy((char*)kstack,hline,hloff);
#ifdef MULTIBYTE
			e_internal((char*)kstack,kstack);
#endif	/* MULTIBYTE */
			ungetchar(cntl(Y));
		}
	}
	CntrlO = 0;
	while ((c = e_getchar()) != (-1))
	{
		if (backslash)
		{
			backslash = 0;
			if (c==usrerase||c==usrkill||(!print(c) &&
				(c!='\r'&&c!='\n')))
			{
				/* accept a backslashed character */
				cur--;
				out[cur++] = c;
				out[eol] = '\0';
				draw(APPEND);
				continue;
			}
		}
		if (c == usrkill)
		{
			c = KILLCHAR ;
		}
		else if (c == usrerase)
		{
			c = ERASECHAR ;
		} 
		else if ((c == usreof)&&(eol == plen))
		{
			c = EOFCHAR;
		}
		oadjust = count = adjust;
		if(count<0)
			count = 1;
		adjust = -1;
		i = cur;
		switch(c)
		{
		case cntl(V):
			{
				genchar string[100];
				/* save current line */
				genncpy(string,out,sizeof(string)/CHARSIZE-1);
				out[plen] = '\0';
				cur = plen;
#ifdef MULTIBYTE
				e_internal(&version[4],out+plen);
#else
				gencpy(buff+plen,&version[4]);
#endif	/* MULTIBYTE */
				draw(UPDATE);
				c = e_getchar();
				ungetchar(c);
				/* restore line */
				cur = i;
				genncpy(out,string,sizeof(string)/CHARSIZE-1);
				draw(UPDATE);
			}
			continue;
		case '\0':
			mark = i;
			continue;
		case cntl(X):
			i = e_getchar();
			if (i != cntl(X))
			{
				beep();
				continue;
			}
			if (mark > eol)
				mark = eol;
			i = mark;
			mark = cur;
			goto update;
		case EOFCHAR:
			e_flush();
			setcooked(fd);
			return(0);
#ifdef u370
		case cntl(S) :
		case cntl(Q) :
			continue;
#endif	/* u370 */
		default:
			i = ++eol;
			if (i >= (scend)) /*  will not fit on line */
			{
				eol--;
				ungetchar(c); /* save character for next line */
				draw(FINAL);
				goto process;
			}
			for(i=eol;i>=cur;i--)
			{
				out[i] = out[i-1];
			}
			backslash =  (c == '\\');
			out[cur++] = c;
			draw(APPEND);
			continue;
		case cntl(Y) :
			{
				c = genlen(kstack);
				if ((c + eol) > scend)
				{
					beep();
					continue;
				}
				mark = i;
				for(i=eol;i>=cur;i--)
					out[c+i] = out[i];
				kptr=kstack;
				while (i = *kptr++)
					out[cur++] = i;
				draw(UPDATE);
				eol = genlen(out);
				continue;
			}
		case '\n':
		case '\r':
			draw(FINAL);
			out[eol++] = '\n';
			out[eol] = '\0';
			e_crlf();
			goto process;
		case ERASECHAR :
			if (count > (i-plen))
				count = i-plen;
			while ((count--)&&(i>plen))
			{
				i--;
				eol--;
			}
			genncpy(kstack,out+i,cur-i);
			kstack[cur-i] = 0;
			gencpy(out+i,out+cur);
			mark = i;
			goto update;
		case cntl(W) :
			if (mark > eol )
				mark = eol;
			if (mark == i)
				continue;
			if (mark > i)
			{
				adjust = mark - i;
				ungetchar(cntl(D));
				continue;
			}
			adjust = i - mark;
			ungetchar('\b');
			continue;
		case cntl(D) :
			mark = i;
			kptr = kstack;
			while ((count--)&&(eol>plen)&&(i<eol))
			{
				*kptr++ = out[i];
				eol--;
				while(1)
				{
					if ((out[i] = out[(i+1)])==0)
						break;
					i++;
				}
				i = cur;
			}
			*kptr = '\0';
			goto update;
		case cntl(C) :
		case cntl(F) :
		{
			int cntlC = (c==cntl(C));
			while (count-- && eol>i)
			{
				if (cntlC)
				{
					c = out[i];
#ifdef MULTIBYTE
					if((c&~STRIP)==0 && islower(c))
#else
					if(islower(c))
#endif /* MULTIBYTE */
					{
						c += 'A' - 'a';
						out[i] = c;
					}
				}
				i++;
			}
			goto update;
		}
		case cntl(]) :
			c = e_getchar();
			if (out[i])
				i++;
			while (i < eol)
			{
				if (out[i] == c)
					goto update;
				i++;
			}
			i = plen;
			while (i < cur)
			{
				if (out[i] == c)
					break;
				i++;
			};

update:
			cur = i;
			draw(UPDATE);
			continue;

		case cntl(B) :
			if (count > (i-plen))
				count = i - plen;
			i -= count;
			goto update;
		case cntl(T) :
			if ((is_option(GMACS))||(eol==i))
			{
				if (i >= plen + 2)
				{
					c = out[i - 1];
					out[i-1] = out[i-2];
					out[i-2] = c;
				}
				else
				{
					beep();
					continue;
				}
			}
			else
			{
				if (eol>(i+1))
				{
					c = out[i];
					out[i] = out[i+1];
					out[i+1] = c;
					i++;
				}
				else
				{
					beep();
					continue;
				}
			}
			goto update;
		case cntl(A) :
			i = plen;
			goto update;
		case cntl(E) :
			i = eol;
			goto update;
		case cntl(U) :
			adjust = 4*count;
			continue;
		case KILLCHAR :
			cur = plen;
			oadjust = -1;
		case cntl(K) :
			if(oadjust >= 0)
			{
				mark = plen+count;
				ungetchar(cntl(W));
				continue;
			}
			i = cur;
			eol = i;
			mark = i;
			gencpy(kstack,&out[i]);
			out[i] = 0;
			draw(UPDATE);
			if (c == KILLCHAR)
			{
				if (terminal == PAPER)
					putstring("\r\n");
				c = e_getchar();
				if (c != usrkill)
				{
					ungetchar(c);
					continue;
				}
				if (terminal == PAPER)
					terminal = CRT;
				else
				{
					terminal = PAPER;
					putstring("\r\n");
				}
			}
			continue;
		case cntl(L):
			e_crlf();
			draw(REFRESH);
			continue;
		case cntl([) :
			adjust = escape(out,oadjust);
			continue;
		case cntl(R) :
			search(out,count);
			goto drawline;
		case cntl(P) :
			location = hist_locate(hline,hloff,-count);
			hline = location.his_command;
			if (hline < hismin)
			{
				hline = hismin;
				beep();
			}
			goto common;

		case cntl(O) :
			location.his_command = hline;
			location.his_line = hloff;
			CntrlO = 1;
			draw(FINAL);
			out[eol++] = '\n';
			out[eol] = '\0';
			e_crlf();
			goto process;
		case cntl(N) :
			location = hist_locate(hline,hloff,count);
			if (location.his_command > histlines)
			{
				beep();
				continue;
			}
			hline = location.his_command;
		common:
			hloff = location.his_line;
			hist_copy(&out[plen],hline,hloff);
#ifdef MULTIBYTE
			e_internal((char*)(&out[plen]),&out[plen]);
#endif /* MULTIBYTE */
		drawline:
			eol = genlen(out);
			cur = eol;
			draw(UPDATE);
			continue;
		}
		
	}
	
process:

	if (c == (-1))
	{
		lookahead = 0;
		beep();
		out[plen] = '\0';
	}
	gencpy(out,&out[plen]);
#ifdef MULTIBYTE
	e_external(out,buff);
#endif /* MULTIBYTE */
#ifdef u370
	putchar('\0');
#endif	/* u370 */
	e_flush();
	setcooked(fd);
	i = strlen(buff);
	if (i)
		return(i);
	return(-1);
}


static void 
putstring(s)
register char *s;
{
	register int c;
	while (c= *s++)
		 putchar(c);
}


static int 
escape(out,count)
register genchar *out;
{
	register int i,value;
	int digit,ch;
	digit = 0;
	value = 0;
	while ((i=e_getchar()),isdigit(i))
	{
		value *= 10;
		value += (i - '0');
		digit = 1;
	}
	if (digit)
	{
		ungetchar(i) ;
		return(value);
	}
	value = count;
	if(value<0)
		value = 1;
	switch(ch=i)
	{
		case ' ':
			mark = cur;
			return(-1);

		case 'p':	/* M-p == ^W^Y (copy stack == kill & yank) */
			ungetchar(cntl(Y));
			ungetchar(cntl(W));
			return(-1);

		case 'l':	/* M-l == lower-case */
		case 'd':
		case 'c':
		case 'f':
		{
			i = cur;
			while(value-- && i<eol)
			{
				while (!isword(i))
					i++;
				while ((out[i])&&(isword(i)))
					i++;
			}
			if(ch=='l')
			{
				value = i-cur;
				while (value-- > 0)
				{
					i = out[cur];
#ifdef MULTIBYTE
					if((i&~STRIP)==0 && isupper(i))
#else
					if(isupper(i))
#endif /* MULTIBYTE */
					{
						i += 'a' - 'A';
						out[cur] = i;
					}
					cur++;
				}
				draw(UPDATE);
				return(-1);
			}

			else if(ch=='f')
			{
				cur = i;
				draw(UPDATE);
				return(-1);
			}
			else if(ch=='c')
			{
				ungetchar(cntl(C));
				return(i-cur);
			}
			else
			{
				if (i-cur)
				{
					ungetchar(cntl(D));
					return(i-cur);
				}
				beep();
				return(-1);
			}
		}
		
		
		case 'b':
		case DELETE :
		case '\b':
		case 'h':
		{
			i = cur;
			while(value-- && i>plen)
			{
				i--;
				while ((i>plen)&&(!isword(i)))
					i--;
				while ((i>plen)&&(isword(i-1)))
					i--;
			}
			if(ch=='b')
			{
				cur = i;
				draw(UPDATE);
				return(-1);
			}
			else
			{
				ungetchar(ERASECHAR);
				return(cur-i);
			}
		}
		
		case '>':
			ungetchar(cntl(N));
			return(histlines-(hline+1));
		
		case '<':
			ungetchar(cntl(P));
			return(hline);


#ifdef KSHELL
		case '_' :
		case '.' :
		{
			genchar name[MAXLINE];
			char buf[MAXLINE];
			char *ptr;
			ptr = hist_word(buf,(count?count:-1));
#ifndef KSHELL
			if(ptr==NULL)
			{
				beep();
				break;
			}
#endif	/* KSHELL */
			if ((eol - cur) >= sizeof(name))
			{
				beep();
				return(-1);
			}
			mark = cur;
			gencpy(name,&out[cur]);
			while(*ptr)
			{
				out[cur++] = *ptr++;
				eol++;
			}
			gencpy(&out[cur],name);
			draw(UPDATE);
			return(-1);
		}

		/* file name expansion */
		case cntl([) :	/* easier to type */
			i = '*';
		case '*':
		case '=':	/* escape = - list all matching file names */
			mark = cur;
			if(q_expand(out,&cur,&eol,plen,i) < 0)
				beep();
			else if(i=='*')
				draw(UPDATE);
			else
				draw(REFRESH);
			return(-1);

		default:
			/* look for user defined macro definitions */
			if(e_macro(i))
				return(-1);
#else
		default:
#endif	/* KSHELL */
		ungetchar(i);
		ungetchar(cntl([));
		ungetchar('\\');
		return(-1);
	}
}


static int 
search(out,direction)
genchar out[];
{
	static int prevdirection =  1 ;
	histloc location;
	register int i,sl;
	genchar str_buff[100];
	register genchar *string = drawbuff;
	/* save current line */
	char sav_cur = cur;
	genncpy(str_buff,string,sizeof(str_buff)/CHARSIZE-1);
	string[plen] = '^';
	string[plen+1] = 'R';
	string[plen+2] = '\0';
	sl = 2+plen;
	cur = sl;
	draw(UPDATE);
	while ((i = e_getchar())&&(i != '\r')&&(i != '\n'))
	{
		if (i==usrerase)
		{
			if (sl > 2+plen)
			{
				string[--sl] = '\0';
				cur = sl;
				draw(UPDATE);
			}
			else
				beep();
			continue;
		}
		if (i==usrkill)
		{
			beep();
			goto restore;
		}
		if (i == '\\')
		{
			string[sl++] = '\\';
			string[sl] = '\0';
			cur = sl;
			draw(APPEND);
			i = e_getchar();
			string[--sl] = '\0';
		}
		string[sl++] = i;
		string[sl] = '\0';
		cur = sl;
		draw(APPEND);
	}
	i = genlen(string);
	
	if (direction < 1)
	{
		prevdirection = -prevdirection;
		direction = 1;
	}
	else
		direction = -1;
	if (i != 2+plen)
	{
		gencpy(lstring,&string[2+plen]);
#ifdef MULTIBYTE
		e_external(lstring,(char*)lstring);
#endif /* MULTIBYTE */
		prevdirection = direction;
	}
	else
		direction = prevdirection ;
	location = hist_find((char*)lstring,hline,1,direction);
	i = location.his_command;
	if(i>0)
	{
		hline = i;
		hloff = location.his_line;
		hist_copy((char*)&out[plen],hline,hloff);
#ifdef MULTIBYTE
		e_internal((char*)&out[plen],&out[plen]);
#endif /* MULTIBYTE */
		return;
	}
	if (i < 0)
	{
		beep();
		hloff = (fc_fix?fc_fix->fixline:0);
		hline = histlines;
	}
restore:
	genncpy(string,str_buff,sizeof(str_buff)/CHARSIZE-1);
	cur = sav_cur;
	return;
}


/* Adjust screen to agree with inputs: logical line and cursor */
/* If 'first' assume screen is blank */

static void
draw(option)
DRAWTYPE option;
{
#define	NORMAL ' '
#define	LOWER  '<'
#define	BOTH   '*'
#define	UPPER  '>'
#define UNDEF	0

	static char overflow;		/* Screen overflow flag set */
	register genchar *sptr;		/* Pointer within screen */
	
	static int offset;		/* Screen offset */
	static char scvalid;	/* Screen is up to date */
	
	genchar nscreen[2*MAXLINE];  /* New entire screen */
	genchar *ncursor;	    /* New cursor */
	register genchar *nptr;	    /* Pointer to New screen */
	char  longline;	    /* Line overflow */
	genchar *logcursor;
	genchar *nscend;		/* end of logical screen */
	register int i;
	
	nptr = nscreen;
	sptr = drawbuff;
	logcursor = sptr + cur;
	ncursor = nscreen;
	longline = NORMAL;
	
	if (option == FIRST || option == REFRESH)
	{
		overflow = NORMAL;
		cursor = screen;
		offset = 0;
		cr_ok = crallowed;
		if (option == FIRST)
		{
			scvalid = 1;
			gencpy(cursor,sptr);
			cursor += plen;
			return;
		}
		*cursor = '\0';
	}
	
	/*********************
	 Do not update screen if pending characters
	**********************/
	
	if ((lookahead)&&(option != FINAL))
	{
		
		scvalid = 0; /* Screen is out of date, APPEND will not work */
		
		return;
	}
	
	/***************************************
	If in append mode, cursor at end of line, screen up to date,
	the previous character was a 'normal' character,
	and the window has room for another character.
	Then output the character and adjust the screen only.
	*****************************************/
	

	i = *(logcursor-1);
	
	if ((option == APPEND)&&(scvalid)&&(*logcursor == '\0')&&
	    print(i)&&((cursor-screen)<(w_size-1)))
	{
		putchar(i);
		*cursor++ = i;
		*cursor = '\0';
		return;
	}

	/* copy the prompt */
	i = plen;
	while(--i >=0)
		nptr[i] = sptr[i];
	/* now the rest of the line */
	ncursor = nptr + e_virt_to_phys(sptr,nptr,cur,plen,plen);
	nptr += genlen(nptr);
	sptr += genlen(sptr);
	nscend = nptr - 1;
	if(sptr == logcursor)
		ncursor = nptr;
	
	/*********************
	 Does ncursor appear on the screen?
	 If not, adjust the screen offset so it does.
	**********************/
	
	i = ncursor - nscreen;
	
	if ((i <= offset)||(i >= (offset+w_size)))
	{
		offset = i - (w_size>>1);
		if (offset < plen)
		{
			offset = (crallowed == YES) ? 0 : plen;
		}
		if ((offset >= plen)&&(cr_ok == NO))
		{
			
			/*********************************
			 Don't really know whats on the screen
			 because of strange characters in the prompt.
			
			 Mark entire screen as unknow.
			***********************************/
			
			cursor = screen;
			*cursor = '\0';
			putchar('\r');
			overflow =  UNDEF;
			cr_ok = YES;
		}
	}
	/*********************
	 Is the range of screen[0] thru screen[w_size] up-to-date
	 with nscreen[offset] thru nscreen[offset+w_size] ?
	 If not, update as need be.
	***********************/
	
	nptr = &nscreen[offset];
	sptr = screen;
	
	i = w_size;
	
	while (i-- > 0)
	{
		
		if (*nptr == '\0')
		{
			*(nptr + 1) = '\0';
			*nptr = ' ';
		}
		if (*sptr == '\0')
		{
			*(sptr + 1) = '\0';
			*sptr = ' ';
		}
		if (*nptr == *sptr)
		{
			nptr++;
			sptr++;
			continue;
		}
		setcursor(sptr-screen,*nptr);
		*sptr++ = *nptr++;
#ifdef MULTIBYTE
		while(*nptr==MARKER)
		{
			*sptr++ = *nptr++;
			i--;
			cursor++;
		}
#endif /* MULTIBYTE */
	}
	
	/******************
	
	Screen overflow checks 
	
	********************/
	
	if (nscend >= &nscreen[offset+w_size])
	{
		if (offset > plen)
			longline = BOTH;
		else
			longline = UPPER;
	}
	else
	{
		if (offset > plen)
			longline = LOWER;
	}
	
	/* Update screen overflow indicator if need be */
	
	if (longline != overflow)
	{
		setcursor(w_size,longline);
		overflow = longline;
	}
	i = (ncursor-nscreen) - offset;
	setcursor(i,0);
	scvalid = 1;
	return;
}

/*
 * put the cursor to the <new> position within screen buffer
 * if <c> is non-zero then output this character
 * cursor is set to reflect the change
 */

static void
setcursor(new,c)
register int new,c;
{
	register int old = cursor - screen;
	if (old > new)
	{
		if ((cr_ok == NO) || ((new*2)>old))
		{
			while (old > new)
			{
				putchar('\b');
				old--;
			}
			goto skip;
		}
		putchar('\r');
		old = 0;
	}
	while (new > old)
		putchar(screen[old++]);
skip:
	if(c)
	{
		putchar(c);
		new++;
	}
	cursor = screen+new;
	return;
}

#ifdef MULTIBYTE
static int print(c)
register int c;
{
	return((c&~STRIP)==0 && isprint(c));
}

static int isword(i)
register int i;
{
	register int c = drawbuff[i];
	return((c&~STRIP) || isalnum(c));
}
#endif /* MULTIBYTE */
