/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "io.h"
#include "ctype.h"
#include "termcap.h"

#ifdef IBMPC

/* here come the actual emulation routines	*/

#include <dos.h>
#include <conio.h>

#define BYTE	unsigned char
#define WORD	unsigned int

#ifdef MAC
#	undef private
#	define private
#endif

#ifdef	LINT_ARGS
private BYTE near get_mode(void);

private WORD
	near cur_page(void),
	near get_cur(void);

private void
	near ch_out(BYTE, BYTE),
	near clr_eop(void),
	near cur_advance(void),
	near cur_down(void),
	near cur_left(void),
	near cur_right(void),
	near cur_up(void),
	near line_feed(void),
	near normfun(char),
	near scr_win(int, BYTE, BYTE, BYTE, BYTE),
	near set_cur(WORD),
	near set_mode(BYTE),
	near wherexy(BYTE *, BYTE *);
#else
private BYTE near get_mode();

private WORD
	near cur_page(),
	near get_cur();

private void
	near ch_out(),
	near clr_eop(),
	near cur_advance(),
	near cur_down(),
	near cur_left(),
	near cur_right(),
	near cur_up(),
	near line_feed(),
	near normfun(),
	near scr_win(),
	near set_cur(),
	near set_mode(),
	near wherexy();
#endif	/* LINT_ARGS */

#ifdef MAC
#	undef private
#	define private static
#endif

#define VIDEO   0x10

#define intr(n, r)	int86(n, r, r);

BYTE CHPL=80, 
     LPP=25, 
     CUR_PAGE=0, 
     C_ATTR = 0x07,
     C_X=0, 
     C_Y=0;

int Fgcolor = 7,
    Bgcolor = 0,
	Mdcolor = 0;
    
void setcolor(fg, bg)
BYTE fg, bg;
{
   C_ATTR = ((bg&0xf)<<4)|(fg&0xf);
}   

private
WORD near cur_page()
{
   union REGS vr;

   vr.h.ah = 0x0f;
   intr(VIDEO, &vr);
   return(vr.h.bh);
}
   
private
void near set_cur(xy)
WORD xy;
{
   union REGS vr;
   
   vr.h.bh = CUR_PAGE;
   vr.h.ah = 0x02;
   vr.x.dx = xy;
   intr(VIDEO, &vr);
}

private
WORD near get_cur()
{
   union REGS vr;

   vr.h.bh = CUR_PAGE;
   vr.h.ah = 0x03;
   intr(VIDEO, &vr);
   return (vr.x.dx);
}		    
 
private
BYTE near get_mode()
{
  union REGS vr;

  vr.h.ah = 0x0f;
  intr(VIDEO, &vr);
  return(vr.h.al);
}

BYTE lpp()
{
   int far *regen = (int far *) 0x44C;
   int what;
   BYTE chpl();
   
   what = (*regen&0xff00)/2/chpl();
   return (what > 43 ? 25 : what);
}    
	  
private
void near set_mode(n)
BYTE n;
{
  union REGS vr;

  vr.h.ah = 0x00;
  vr.h.al = n;
  intr(VIDEO, &vr);
} 

#define gotoxy(x,y)	set_cur((x)<<8|((y)&0xff))
#define cur_mov(x,y)	set_cur((C_X=x)<<8|((C_Y=y)&0xff))

private
void near wherexy( x, y)
BYTE *x, *y;
{
  register WORD xy;

  xy = get_cur();
  *x = xy>>8;
  *y = xy&0xff;
}
    
#define wherex()	C_X
#define wherey()	C_Y

private
void near scr_win(no, ulr, ulc, lrr, lrc)
int no;
BYTE ulr, ulc, lrr, lrc;
{
  union REGS vr;

  if (no >= 0)
     vr.h.ah = 0x06;
  else {
     vr.h.ah = 0x07;
     no = - no;
  }
  vr.h.al = no;
  vr.x.cx = ulr<<8 | ulc;
  vr.x.dx = lrr<<8 | lrc;
  vr.h.bh = C_ATTR;
  intr(VIDEO, &vr);
}    

BYTE chpl()
{
  union REGS vr;

  vr.h.ah = 0x0f;
  intr(VIDEO, &vr);
  return(vr.h.ah);
}
   
#define clr_page()	scr_win(0, 0, 0, LPP-1, CHPL-1), \
			gotoxy(C_X = 0, C_Y = 0)
		
private
void near cur_right()
{
   if (C_Y < CHPL-1) 
      C_Y++;
   gotoxy(C_X, C_Y);
}	     

private
void near cur_up()
{
   if (C_X)
      C_X--;
   gotoxy(C_X, C_Y);
}

private
void near cur_left()
{
   if (C_Y)
      C_Y--;
   gotoxy(C_X, C_Y);
}

private
void near cur_down()
{
   if (C_X < LPP-1)
      C_X++;
   gotoxy(C_X, C_Y);
}
 			  
private
void near ch_out(c, n)
BYTE c, n;
{
  union REGS vr;

  vr.h.ah = 0x09;
  vr.h.al = c;
  vr.h.bl = C_ATTR;
  vr.h.bh = CUR_PAGE;
  vr.x.cx = n;
  intr(VIDEO, &vr);
}

#define wrch(c)		ch_out((c), 1), cur_advance()

#define home_cur()	gotoxy(C_X = 0, C_Y = 0)

#define clr_eoln()	ch_out(' ', CHPL-wherey())

private
void near clr_eop()
{
  clr_eoln();
  scr_win(LPP-1-wherex(), wherex()+1, 0, LPP-1, CHPL-1);
}

void init_43()
{
   BYTE far *info = (BYTE far *) 0x487;
   WORD far *CRTC = (WORD far *) 0x463;
   union REGS vr;
   WORD cur;
         
   CUR_PAGE = cur_page();
   CHPL = chpl();
   LPP = lpp();

   if (get_mode()!=3)
      set_mode(3);
   cur = get_cur();
   
   vr.x.ax = 0x1112;
   vr.h.bl = 0;
   intr(VIDEO, &vr);

   *info |= 1;
   vr.x.ax = 0x0100;
   vr.h.bh = 0;
   vr.x.cx = 0x0600;
   intr(VIDEO, &vr);

   outp(*CRTC, 0x14);
   outp(*CRTC+1, 0x07);

   vr.x.ax = 0x1200;
   vr.h.bl = 0x20;
   intr(VIDEO, &vr);
   
   LPP = lpp();

   set_cur(cur);
   wherexy(&C_X, &C_Y);
}

void reset_43()
{
   BYTE far *info = (BYTE far *) 0x487;
   WORD far *CRTC = (WORD far *) 0x463;
   union REGS vr;
   
   set_mode(3);

   *info &= 128;
   vr.x.ax = 0x0100;
   vr.h.bh = 0x0607;
   vr.x.cx = 0x0607;
   intr(VIDEO, &vr);             

   outp(*CRTC, 0x14);
   outp(*CRTC+1, 13);

}

#define scr_up()		scr_win(1, 0, 0, LPP-1, CHPL-1)
#define back_space()	cur_left()

private
void near line_feed()
{
   if (++C_X > LPP-1) {
      C_X = LPP-1;
      scr_up();
   }   
   gotoxy(C_X, C_Y);
}

#define BELL_P 0x61			/* speaker */
#define BELL_D 0x2dc			/* 550 hz  */
#define TIME_P 0x40			/* timer   */
#define TINI   182			/* 10110110b timer initialization */

void dobell(x)
{
   unsigned int n = 0x8888;
   int orgval;

   outp(TIME_P+3, TINI);
   outp(TIME_P+2, BELL_D&0xff);
   outp(TIME_P+2, BELL_D>>8);
   orgval = inp(BELL_P);
   outp(BELL_P, orgval|3);		/* turn speaker on  */
   while (--n > 0)
	 ;
   outp(BELL_P, orgval);
}

#define carriage_return()	gotoxy(wherex(), C_Y = 0)
   
private
void near cur_advance()
{
   if (++C_Y > CHPL-1) {
      C_Y = 0;
      if (++C_X > LPP-1) {
         scr_up();   	
         C_X = LPP-1;
      }   
   }
   gotoxy(C_X, C_Y);
}	     

void init_term()
{
   if (lpp() == 43)
      reset_43();   
   CUR_PAGE = cur_page();
   CHPL = chpl();
   LPP = lpp();
   wherexy(&C_X, &C_Y); 
}

private
void near normfun();

void write_em(s)
char *s;
{
  while (*s)
        normfun(*s++);
}          

void write_emif(s)
char *s;
{
  if (s)
	 write_em(s);
}

void write_emc(s, n)
char *s;
int n;
{
   while (n--)
         normfun(*s++);
}           

private
void near normfun(c)
char c;
{
      switch (c) {
        case 10: line_feed(); break;
        case 13: carriage_return(); break;
        case  8: back_space(); break;
        case  7: dobell(0); break;
		case  0: break;
        default: wrch(c);
      }  
}

#endif	/* IBMPC */


extern int	BufSize;

int	AbortCnt,
	tabstop = 8;

#if !(defined(IBMPC) || defined(MAC))	
int	(*TTins_line)(),
	(*TTdel_line)();
#endif /* (defined(IBMPC) || defined(MAC)) */

struct scrimage
	*DesiredScreen = 0,
	*PhysScreen = 0;

struct screenline	*Screen = 0,	/* the screen (a bunch of screenline) */
			*Savelines = 0,	/* another bunch (LI of them) */
			*Curline = 0;	/* current line */
char	*cursor,			/* offset into current Line */
	*cursend;

int	CapCol,
	CapLine,

	i_line,
	i_col;

void
make_scr()
{
	register int	i;
	register struct screenline	*ns;
	register char	*nsp;

#ifdef RESHAPING
	/* In case we are RESHAPING the window! */
	if (DesiredScreen)
		free((char *) DesiredScreen);
	if (PhysScreen)
		free((char *) PhysScreen);
	if (Savelines)
		free((char *) Savelines);
	if (Screen) {
		free(Screen->s_line);	/* free all the screen data */
		free((char *) Screen);
	}
#endif /* RESHAPING */

	DesiredScreen = (struct scrimage *) malloc((unsigned) LI * sizeof (struct scrimage));
	PhysScreen = (struct scrimage *) malloc((unsigned) LI * sizeof (struct scrimage));

	Savelines = (struct screenline *)
			malloc((unsigned) LI * sizeof(struct screenline));
	ns = Screen = (struct screenline *)
			malloc((unsigned) LI * sizeof(struct screenline));

	nsp = (char *) malloc((unsigned)CO * LI);
	if (nsp == 0) {
		printf("\n\rCannot malloc screen!\n");
		finish(1);
	}

	for (i = 0; i < LI; i++) {
		ns->s_line = nsp;
		nsp += CO;
		ns->s_length = nsp - 1;		/* End of Line */
		ns += 1;
	}
	cl_scr(0);
}

void
clrline(cp1, cp2)
register char	*cp1,
		*cp2;
{
	while (cp1 <= cp2)
		*cp1++ = ' ';
}

#if !(defined(IBMPC) || defined(MAC))
# define sputc(c)	((*cursor != (char) (c)) ? dosputc(c) : (cursor++, i_col++))
#endif /* (defined(IBMPC) || defined(MAC)) */

#ifdef IBMPC
int force = 0;
# define sputc(c)	dosputc(c)
#endif /* IBMPC */

#ifdef MAC
# define sputc(c)	bufputc(c)	/* line buffered for mac display */
#endif /* MAC */

#define soutputc(c)	if (--n <= 0) break; else sputc(c)

void
cl_eol()
{
	if (cursor > cursend)
		return;

	if (cursor < Curline->s_length) {
#if !(defined(IBMPC) || defined(MAC))
		if (CE) {
#endif /* (defined(IBMPC) || defined(MAC)) */
			Placur(i_line, i_col);
#ifdef TERMCAP
			putpad(CE, 1);
#else 
		clr_eoln();
#endif /* TERMCAP */
			clrline(cursor, Curline->s_length);
#if !(defined(IBMPC) || defined(MAC))
		} else {
		/* Ugh.  The slow way for dumb terminals. */
			register char *savecp = cursor;

			while (cursor <= Curline->s_length)
				sputc(' ');
			cursor = savecp;
		}
#endif /* (defined(IBMPC) || defined(MAC)) */
		Curline->s_length = cursor;
	}
}

void
cl_scr(doit)
{
	register int	i;
	register struct screenline	*sp = Screen;

	for (i = 0; i < LI; i++, sp++) {
		clrline(sp->s_line, sp->s_length);
		sp->s_length = sp->s_line;
		PhysScreen[i].s_id = 0;
	}
	if (doit) {
#ifdef TERMCAP
		putpad(CL, LI);
#else 
		clr_page();
#endif /* TERMCAP */
		CapCol = CapLine = 0;
		UpdMesg = YES;
	}
}

#ifdef ID_CHAR
extern int	IN_INSmode;
#endif

/* Output one character (if necessary) at the current position */

#ifndef MAC
int		/* only for lints sake */
dosputc(c)
register char	c;
{
#ifndef IBMPC
	if (*cursor != c) {
# ifdef ID_CHAR
		if (IN_INSmode)
			INSmode(0);
# endif
#else /* IBMPC */
	if ((force) || (*cursor != c)) {
#endif /* IBMPC */
		if (i_line != CapLine || i_col != CapCol)
			Placur(i_line, i_col);
#ifndef IBMPC
		if (UL && (c & CHARMASK) == '_' && (*cursor & CHARMASK) != ' ')
			putstr(" \b");		/* Erase so '_' looks right. */
#endif /* IBMPC */
		*cursor++ = c;
#ifndef IBMPC
		putchar(c & CHARMASK);
#else /* IBMPC */
		normfun(c);
#endif /* IBMPC */
		AbortCnt -= 1;
		CapCol += 1;
		i_col += 1;
	} else {
		cursor += 1;
		i_col += 1;
	}
}
#else /* MAC */

/* Character output to bit-mapped screen is very expensive. It makes
   much more sense to write the entire line at once. So, we print all
   the characters, whether already there or not, once the line is
   complete.  */
   
#define BUFFLUSH (char) 0
#define BUFSTART (char) 1
    
bufputc(c)
register char c;
{
	static char buf[256];
	static int len = 0;
	
	if(c == BUFSTART) {
/*		if (i_line != CapLine || i_col != CapCol)*/
			NPlacur(i_line, i_col);
		len = 0;
		return;
	}
	if(c == BUFFLUSH) {
		buf[0] = (unsigned char) len;
		writechr(buf);
		len = 0;
	}
	else {
		if(len > 255) return;
		*cursor++ = c;
		if(c == '0') buf[++len] = 0xAF;	/* slashed zero */
 		else buf[++len] = c;
		CapCol++;
		i_col++;
	}
	return;
}
#endif /* MAC */

/* Write `line' at the current position of `cursor'.  Stop when we
   reach the end of the screen.  Aborts if there is a character
   waiting.  */

#ifdef MAC		/* This was getting too complicated with ifdefs ... */
int
swrite(line, inversep, abortable)
register char	*line;
register int	abortable;
{
	register int	c;
	int	col = i_col,
		aborted = 0;
	register int	n = cursend - cursor;

	if (n <= 0)
		return 1;
	sputc(BUFSTART);	/* Okay, because no interruption possible */

	while (c = *line++) {
		if (c == '\t') {
			int	nchars;

			nchars = (tabstop - (col % tabstop));
			col += nchars;

			while (nchars--)
				soutputc(' ');
			if (n <= 0)
				break;
		} else if (isctrl(c)) {
			soutputc('^');
			c = ((c == '\177') ? '?' : c + '@');
			soutputc(c);
			col += 2;
		} else {
			soutputc(c);
			col += 1;
		}
	}
	if (n <= 0) {
		if ((*line == '\0') && (c != '\t') && !isctrl(c))
			sputc(c);
			sputc('!');
	}
	if (cursor > Curline->s_length)
		Curline->s_length = cursor;
	sputc(BUFFLUSH);
	return !aborted;
}
#else /* MAC */

int
swrite(line, inversep, abortable)
register char	*line;
register int	abortable;
{
	register int	c;
	int	col = i_col,
		aborted = 0;
	register int	n = cursend - cursor;
#ifndef IBMPC
	int	or_byte = inversep ? 0200 : 0,
		thebyte;
#else 
	int	thebyte;
#endif /* IBMPC */

#ifdef IBMPC
        force = inversep? 1: 0;  /* to force a redraw of the modeline */
#endif /* IBMPC */

	if (n <= 0)
		return 1;
	while (c = *line++) {
#if !(defined(IBMPC) || defined(MAC))	/* don't check after every character */
		if (abortable && AbortCnt < 0) {
			AbortCnt = BufSize;
			if (InputPending = charp()) {
				aborted = 1;
				break;
			}
		}
#endif /* (defined(IBMPC) || defined(MAC)) */
		if (c == '\t') {
			int	nchars;

			nchars = (tabstop - (col % tabstop));
			col += nchars;

#ifndef IBMPC
			thebyte = (' ' | or_byte);
#endif /* IBMPC */
			while (nchars--)
#ifndef IBMPC
				soutputc(thebyte);
#else /* IBMPC */
				soutputc(' ');
#endif /* IBMPC */
			if (n <= 0)
				break;
		} else if (isctrl(c)) {
#ifndef IBMPC
			thebyte = ('^' | or_byte);
			soutputc(thebyte);
			thebyte = (((c == '\177') ? '?' : c + '@') | or_byte);
			soutputc(thebyte);
#else /* IBMPC */
			soutputc('^');
			c = ((c == '\177') ? '?' : c + '@');
			soutputc(c);
#endif /* IBMPC */
			col += 2;
		} else {
#ifndef IBMPC
			thebyte = (c | or_byte);
			soutputc(thebyte);
#else /* IBMPC */
 		    if (c == 255) c = 1;
			if (c == ' ' && inversep) c = 255;
			soutputc(c);
#endif /* IBMPC */
			col += 1;
		}
	}
	if (n <= 0) {
		if ((*line == '\0') && (c != '\t') && !isctrl(c))
#ifndef IBMPC
			sputc(c|or_byte);
#else /* IBMPC */
			sputc(c);
#endif /* IBMPC */
		else
#ifndef IBMPC
			sputc('!'|or_byte);
#else /* IBMPC */
			sputc('!');
#endif /* IBMPC */
	}
	if (cursor > Curline->s_length)
		Curline->s_length = cursor;
#ifdef IBMPC
	force = 0;
#endif
	return !aborted;
}
#endif /* MAC */
/* This is for writing a buffer line to the screen.  This is to
   minimize the amount of copying from one buffer to another buffer.
   This gets the info directly from the disk buffers. */

int
BufSwrite(linenum)
{
	register int	n = cursend - cursor,
			col = 0,
			c = -1;
	register char	*bp;
	int	StartCol = DesiredScreen[linenum].s_offset,
		visspace = DesiredScreen[linenum].s_window->w_flags & W_VISSPACE,
		aborted = 0;

	bp = lcontents(DesiredScreen[linenum].s_lp);
	if (*bp) for (;;) {
		if (col >= StartCol) {
			DesiredScreen[linenum].s_offset = col;
			break;
		}

		c = *bp++ & CHARMASK;
		if (c == '\0')
			break;
		if (c == '\t')
			col += (tabstop - (col % tabstop));
		else if (isctrl(c))
			col += 2;
		else
			col += 1;
	}
#ifdef MAC
	sputc(BUFSTART);	/* Okay because we can't be interrupted */
#endif

	if (c != '\0') while (c = *bp++) {
#if !(defined(IBMPC) || defined(MAC))		/* will never get true so why bother */
		if (AbortCnt < 0) {
			AbortCnt = BufSize;
			if (InputPending = charp()) {
				aborted = 1;
				break;
			}
		}
#endif /* (defined(IBMPC) || defined(MAC)) */
		if (c == '\t') {
			int	nchars = (tabstop - (col % tabstop));

			col += nchars;
			if (visspace) {
				soutputc('>');
				nchars -= 1;
			}
			while (--nchars >= 0)
				soutputc(' ');
			if (n <= 0)
				break;
		} else if (isctrl(c)) {
			soutputc('^');
			soutputc((c == '\177') ? '?' : c + '@');
			col += 2;
		} else {
			if (c == ' ' && visspace)
				c = '_';
#ifdef IBMPC
			if (c == 255)
			   c = 1;
#endif /* IBMPC */
			soutputc(c);
			col += 1;
		}
	}
	if (n <= 0) {
		if ((*bp == '\0') && (c != '\t') && !isctrl(c))
			sputc(c);
		else
			sputc('!');
	}
	if (cursor > Curline->s_length)
		Curline->s_length = cursor;
#ifdef MAC
	sputc(BUFFLUSH);
#endif
	return !aborted;		/* Didn't abort */
}

void
i_set(nline, ncol)
register int	nline,
		ncol;
{
	Curline = &Screen[nline];
	cursor = Curline->s_line + ncol;
	cursend = &Curline->s_line[CO - 1];
	i_line = nline;
	i_col = ncol;
}

/* Insert `num' lines a top, but leave all the lines BELOW `bottom'
   alone (at least they won't look any different when we are done).
   This changes the screen array AND does the physical changes. */

void
v_ins_line(num, top, bottom)
{
	register int	i;

	/* Save the screen pointers. */

	for(i = 0; i < num && top + i <= bottom; i++)
		Savelines[i] = Screen[bottom - i];

	/* Num number of bottom lines will be lost.
	   Copy everything down num number of times. */

	for (i = bottom; i > top && i-num >= 0; i--)
		Screen[i] = Screen[i - num];

	/* Restore the saved ones, making them blank. */

	for (i = 0; i < num; i++) {
		Screen[top + i] = Savelines[i];
		clrline(Screen[top + i].s_line, Screen[top + i].s_length);
		Screen[top + i].s_length = Screen[top + i].s_line;
	}

#if !(defined(IBMPC) || defined(MAC))
	(*TTins_line)(top, bottom, num);
#endif

#ifdef MAC
	i_lines(top, bottom, num);
#endif 

#ifdef IBMPC
	scr_win(-num, top, 0, bottom, CHPL-1);
#endif 
}

/* Delete `num' lines starting at `top' leaving the lines below `bottom'
   alone.  This updates the internal image as well as the physical image.  */

void
v_del_line(num, top, bottom)
{
	register int	i,
			bot;

	bot = bottom;

	/* Save the lost lines. */

	for (i = 0; i < num && top + i <= bottom; i++)
		Savelines[i] = Screen[top + i];

	/* Copy everything up num number of lines. */

	for (i = top; num + i <= bottom; i++)
		Screen[i] = Screen[i + num];

	/* Restore the lost ones, clearing them. */

	for (i = 0; i < num; i++) {
		Screen[bottom - i] = Savelines[i];
		clrline(Screen[bot].s_line, Screen[bot].s_length);
		Screen[bot].s_length = Screen[bot].s_line;
		bot -= 1;
	}

#if !(defined(IBMPC) || defined(MAC))
	(*TTdel_line)(top, bottom, num);
#endif

#ifdef MAC
	d_lines(top, bottom, num);
#endif

#ifdef IBMPC
	scr_win(num, top, 0, bottom, CHPL-1);
#endif 

}

#ifndef MAC	/* remainder of this file */
#ifdef IBMPC

/* No cursor optimization on an IBMPC, this simplifies things a lot.
   Think about it: it would be silly!
 */

int	phystab = 8;

void
Placur(line, col)
{
	cur_mov(line, col);
	CapCol = col;
	CapLine = line;
}

void
SO_on()
{
	if (Mdcolor)
		setcolor(Mdcolor&0xf, Mdcolor>>4);
	else
		setcolor(Bgcolor, Fgcolor);
}

void
SO_off()
{
   setcolor(Fgcolor, Bgcolor);
}

extern int EGA;

void

UnsetTerm(foo)
char *foo;
{
  Placur(ILI, 0);
  clr_eoln();
  if (EGA)
	 reset_43();
}


void
ResetTerm()
{
	if (EGA)
	   init_43();
	else
   	   init_term();
	   
	do_sgtty();		/* this is so if you change baudrate or stuff
				   like that, JOVE will notice. */
	ttyset(ON);
}

#else /* IBMPC */ 

/* The cursor optimization happens here.  You may decide that this
   is going too far with cursor optimization, or perhaps it should
   limit the amount of checking to when the output speed is slow.
   What ever turns you on ...   */

private struct cursaddr {
	int	cm_numchars,
		(*cm_proc)();
};

private char	*Cmstr;
private struct cursaddr	*HorMin,
			*VertMin,
			*DirectMin;

private void
	GENi_lines(),
	GENd_lines(),
	ForMotion(),
	ForTab(),
	BackMotion(),
	RetTab(),
	DownMotion(),
	UpMotion(),
	GoDirect(),
	HomeGo(),
	BottomUp();
	

private struct cursaddr	WarpHor[] = {
	0,	ForMotion,
	0,	ForTab,
	0,	BackMotion,
	0,	RetTab
};

private struct cursaddr	WarpVert[] = {
	0,	DownMotion,
	0,	UpMotion
};

private struct cursaddr	WarpDirect[] = {
	0,	GoDirect,
	0,	HomeGo,
	0,	BottomUp
};

#undef	FORWARD
#define	FORWARD		0	/* Move forward */
#define FORTAB		1	/* Forward using tabs */
#undef	BACKWARD
#define	BACKWARD	2	/* Move backward */
#define RETFORWARD	3	/* Beginning of line and then tabs */
#define NUMHOR		4

#define DOWN		0	/* Move down */
#define UPMOVE		1	/* Move up */
#define NUMVERT		2

#define DIRECT		0	/* Using CM */
#define HOME		1	/* HOME	*/
#define LOWER		2	/* Lower Line */
#define NUMDIRECT	3

#define	home()		Placur(0, 0)
#define LowLine()	putpad(LL, 1), CapLine = ILI, CapCol = 0
#define PrintHo()	putpad(HO, 1), CapLine = CapCol = 0

int	phystab = 8;

private void
GoDirect(line, col)
register int	line,
		col;
{
	putpad(Cmstr, 1);
	CapLine = line;
	CapCol = col;
}

private void
RetTab(col)
register int	col;
{
	putchar('\r');
	CapCol = 0;
	ForTab(col);
}

private void
HomeGo(line, col)
{
	PrintHo();
	DownMotion(line);
	ForTab(col);
}

private void
BottomUp(line, col)
register int	line,
		col;
{
	LowLine();
	UpMotion(line);
	ForTab(col);
}

/* Tries to move forward using tabs (if possible).  It tabs to the
   closest tabstop which means it may go past 'destcol' and backspace
   to it. */

private void
ForTab(destcol)
int	destcol;
{
	register int	tabgoal,
			ntabs,
			tabstp = phystab;

	if (TABS && (tabstp > 0)) {
		tabgoal = destcol + (tabstp / 2);
		tabgoal -= (tabgoal % tabstp);

		/* Don't tab to last place or else it is likely to screw up. */
		if (tabgoal >= CO)
			tabgoal -= tabstp;

		ntabs = (tabgoal / tabstp) - (CapCol / tabstp);
		while (--ntabs >= 0)
			putchar('\t');
		CapCol = tabgoal;
	}
	if (CapCol > destcol)
		BackMotion(destcol);
	else if (CapCol < destcol)
		ForMotion(destcol);
}

private void
ForMotion(destcol)
register int	destcol;
{
	register int	nchars = destcol - CapCol;
	register char	*cp = &Screen[CapLine].s_line[CapCol];

	while (--nchars >= 0)
		putchar(*cp++ & CHARMASK);
	CapCol = destcol;
}

private void
BackMotion(destcol)
register int	destcol;
{
	register int	nchars = CapCol - destcol;

	if (BC)
		while (--nchars >= 0)
			putpad(BC, 1);
	else
		while (--nchars >= 0)
			putchar('\b');
	CapCol = destcol;
}

private void
DownMotion(destline)
register int	destline;
{
	register int	nlines = destline - CapLine;

	while (--nlines >= 0)
		putpad(NL, 1);
	CapLine = destline;
}

private void
UpMotion(destline)
register int	destline;
{
	register int	nchars = CapLine - destline;

	while (--nchars >= 0)
		putpad(UP, 1);
	CapLine = destline;
}

#ifdef ID_CHAR
static int	EIlen;
#endif
extern int	IMlen;

void
InitCM()
{
	HOlen = HO ? strlen(HO) : 1000;
	LLlen = LL ? strlen(LL) : 1000;
	UPlen = UP ? strlen(UP) : 1000;
#ifdef ID_CHAR
	if (EI)
		EIlen = strlen(EI);
#endif
}

void
Placur(line, col)
{
	int	dline,		/* Number of lines to move */
		dcol;		/* Number of columns to move */
	register int	best,
			i;
	register struct cursaddr	*cp;
	int	xtracost = 0;	/* Misc addition to cost. */

#define CursMin(which,addrs,max) \
	for (best = 0, cp = &addrs[1], i = 1; i < max; i++, cp++) \
		if (cp->cm_numchars < addrs[best].cm_numchars) \
			best = i; \
	which = &addrs[best];

	if (line == CapLine && col == CapCol)
		return;		/* We are already there. */

	dline = line - CapLine;
	dcol = col - CapCol;
#ifdef ID_CHAR
	if (IN_INSmode && MI)
		xtracost = EIlen + IMlen;
	/* If we're already in insert mode, it is likely that we will
	   want to be in insert mode again, after the insert. */
#endif

	/* Number of characters to move horizontally for each case.
	   1: Just move forward by typing the right character on the screen.
	   2: Print the correct number of back spaces.
	   3: Try tabbing to the correct place.
	   4: Try going to the beginning of the line, and then tab. */

	if (dcol == 1 || dcol == 0) {		/* Most common case. */
		HorMin = &WarpHor[FORWARD];
		HorMin->cm_numchars = dcol + xtracost;
	} else {
		WarpHor[FORWARD].cm_numchars = dcol >= 0 ? dcol + xtracost : 1000;
		WarpHor[BACKWARD].cm_numchars = dcol < 0 ? -(dcol + xtracost) : 1000;
		WarpHor[FORTAB].cm_numchars = dcol >= 0 && TABS ?
				ForNum(CapCol, col) + xtracost : 1000;
		WarpHor[RETFORWARD].cm_numchars = (xtracost + 1 + (TABS ? ForNum(0, col) : col));

		/* Which is the shortest of the bunch */

		CursMin(HorMin, WarpHor, NUMHOR);
	}

	/* Moving vertically is more simple. */

	WarpVert[DOWN].cm_numchars = dline >= 0 ? dline : 1000;
	WarpVert[UPMOVE].cm_numchars = dline < 0 ? ((-dline) * UPlen) : 1000;

	/* Which of these is simpler */
	CursMin(VertMin, WarpVert, NUMVERT);

	/* Homing first and lowering first are considered 
	   direct motions.
	   Homing first's total is the sum of the cost of homing
	   and the sum of tabbing (if possible) to the right. */
	
	if (VertMin->cm_numchars + HorMin->cm_numchars <= 3) {
		DirectMin = &WarpDirect[DIRECT];	/* A dummy ... */
		DirectMin->cm_numchars = 100;
	} else {
		WarpDirect[DIRECT].cm_numchars = CM ?
				strlen(Cmstr = tgoto(CM, col, line)) : 1000;
		WarpDirect[HOME].cm_numchars = HOlen + line +
				WarpHor[RETFORWARD].cm_numchars;
		WarpDirect[LOWER].cm_numchars = LLlen + ((ILI - line) * UPlen) +
				WarpHor[RETFORWARD].cm_numchars;
		CursMin(DirectMin, WarpDirect, NUMDIRECT);
	}

	if (HorMin->cm_numchars + VertMin->cm_numchars < DirectMin->cm_numchars) {
		if (line != CapLine)
			(*VertMin->cm_proc)(line);
		if (col != CapCol) {
#ifdef ID_CHAR
			if (IN_INSmode)	/* We may use real characters ... */
				INSmode(0);
#endif
			(*HorMin->cm_proc)(col);
		}
	} else {
#ifdef ID_CHAR
		if (IN_INSmode && !MI)
			INSmode(0);
#endif
		(*DirectMin->cm_proc)(line, col);
	}
}

#define abs(x)	((x) >= 0 ? (x) : -(x))

int
ForNum(from, to)
register int	from;
{
	register int	tabgoal,
			tabstp = phystab;
	int		numchars = 0;

	if (from >= to)
		return from - to;
	if (TABS && (tabstp > 0)) {
		tabgoal = to + (tabstp / 2);
		tabgoal -= (tabgoal % tabstp);
		if (tabgoal >= CO)
			tabgoal -= tabstp;
		numchars = (tabgoal / tabstop) - (from / tabstp);
		from = tabgoal;
	}
	return numchars + abs(from - to);
}

#ifdef WIRED_TERMS

void
BGi_lines(top, bottom, num)
{
	printf("\033[%d;%dr\033[%dL\033[r", top + 1, bottom + 1, num);
	CapCol = CapLine = 0;
}

void
SUNi_lines(top, bottom, num)
{
	Placur(bottom - num + 1, 0);
	printf("\033[%dM", num);
	Placur(top, 0);
	printf("\033[%dL", num);
}

void
C100i_lines(top, bottom, num)
{
	if (num <= 1) {
		GENi_lines(top, bottom, num);
		return;
	}
	printf("\033v%c%c%c%c", ' ', ' ', ' ' + bottom + 1, ' ' + CO);
	CapLine = CapCol = 0;
	Placur(top, 0);
	while (num--)
		putpad(AL, ILI - CapLine);
	printf("\033v%c%c%c%c", ' ', ' ', ' ' + LI, ' ' + CO);
	CapLine = CapCol = 0;
}

#endif /* WIRED_TERMS */

private void
GENi_lines(top, bottom, num)
{
	register int	i;

	if (CS) {
		putpad(tgoto(CS, bottom, top), 1);
		CapCol = CapLine = 0;
		Placur(top, 0);
		for (i = 0; i < num; i++)
			putpad(SR, bottom - top);
		putpad(tgoto(CS, ILI, 0), 1);
		CapCol = CapLine = 0;
	} else {
		Placur(bottom - num + 1, 0);
		if (M_DL && (num > 1)) {
			char	minibuf[16];

			sprintf(minibuf, M_DL, num);
			putpad(minibuf, ILI - CapLine);
		} else {
			for (i = 0; i < num; i++)
				putpad(DL, ILI - CapLine);
		}
		Placur(top, 0);
		if (M_AL && (num > 1)) {
			char	minibuf[16];

			sprintf(minibuf, M_AL, num);
			putpad(minibuf, ILI - CapLine);
		} else {
			for (i = 0; i < num; i++)
				putpad(AL, ILI - CapLine);
		}
	}
}

#ifdef WIRED_TERMS

void
BGd_lines(top, bottom, num)
{
	printf("\033[%d;%dr\033[%dM\033[r", top + 1, bottom + 1, num);
	CapCol = CapLine = 0;
}

void
SUNd_lines(top, bottom, num)
{
	Placur(top, 0);
	printf("\033[%dM", num);
	Placur(bottom + 1 - num, 0);
	printf("\033[%dL", num);
}

void
C100d_lines(top, bottom, num)
{
	if (num <= 1) {
		GENd_lines(top, bottom, num);
		return;
	}
	printf("\033v%c%c%c%c", ' ', ' ', ' ' + bottom + 1, ' ' + CO);
	CapLine = CapCol = 0;
	Placur(top, 0);
	while (num--)
		putpad(DL, ILI - CapLine);
	printf("\033v%c%c%c%c", ' ', ' ', ' ' + LI, ' ' + CO);
	CapLine = CapCol = 0;
}

#endif /* WIRED_TERMS */

private void
GENd_lines(top, bottom, num)
{
	register int	i;

	if (CS) {
		putpad(tgoto(CS, bottom, top), 1);
		CapCol = CapLine = 0;
		Placur(bottom, 0);
		for (i = 0; i < num; i++)
			putpad(SF, bottom - top);
		putpad(tgoto(CS, ILI, 0), 1);
		CapCol = CapLine = 0;
	} else {
		Placur(top, 0);
		if (M_DL && (num > 1)) {
			char	minibuf[16];

			sprintf(minibuf, M_DL, num);
			putpad(minibuf, ILI - top);
		} else {
			for (i = 0; i < num; i++)
				putpad(DL, ILI - top);
		}
		Placur(bottom + 1 - num, 0);
		if (M_AL && (num > 1)) {
			char	minibuf[16];

			sprintf(minibuf, M_AL, num);
			putpad(minibuf, ILI - CapLine);
		} else {
			for (i = 0; i < num; i++)
				putpad(AL, ILI - CapLine);
		}
	}
}

struct ID_lookup {
	char	*ID_name;
	int	(*I_proc)();	/* proc to insert lines */
	int	(*D_proc)();	/* proc to delete lines */
} ID_trms[] = {
	"generic",	GENi_lines,	GENd_lines,	/* This should stay here */
#ifdef WIRED_TERMS
	"sun",		SUNi_lines,	SUNd_lines,
	"bg",		BGi_lines,	BGd_lines,
	"c1",		C100i_lines,	C100d_lines,
#endif /* WIRED_TERMS */
	0,		0,		0
};

void
IDline_setup(tname)
char	*tname;
{
	register struct ID_lookup	*idp;

	for (idp = &ID_trms[1]; idp->ID_name; idp++)
		if (strncmp(idp->ID_name, tname, strlen(idp->ID_name)) == 0)
			break;
	if (idp->ID_name == 0)
		idp = &ID_trms[0];
	TTins_line = idp->I_proc;
	TTdel_line = idp->D_proc;
}

#endif /* IBMPC */
#endif /* MAC */
