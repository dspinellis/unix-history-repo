/*
 *      TERM.C
 *      UTREE terminal, screen and keyboard routines.
 *      3.01-um klin, Wed May  1 14:21:09 1991
 *              klin, Mon Oct  7 15:16:22 1991, Bug in putchar() deleted
 *              klin, Sat Oct 26 15:26:07 1991, Marking directories changed
 *      3.02-um klin, Fri Nov  1 10:44:45 1991, Screen layout changed
 *                    Sun Nov 10 19:46:21 1991, Function key handling changed
 *                    Sun Nov 24 12:22:56 1991, Extensions for XENIX reported
 *                                              by Rolf Gebhardt (RG 11/22/91)
 *                                              Bug fixes in output reported by
 *                                              Reinhard Wobst and Rolf Gebhardt
 *                                              Video attributes changed
 *      3.03-um klin, Tue Feb 11 19:39:09 1992, Video handling changed
 *                                              Handle glitch capabilities
 *                                              properly
 *              klin, Sun Feb 23 20:33:30 1992, Key handling and key bindings
 *                                              changed. getkey() changed for
 *                                              handling key bindings from
 *                                              key bindings list
 *            a klin, Sun Mar 15 19:08:25 1992, Bug fix in getkey(), clearline()
 *                                              and cleartoend().
 *                                              Minor changes for AIX 3.2
 *            c klin, Mon Mar 30 15:37:45 1992, More bug fixes in clearxxx()
 *                                              from Rolf Gebhardt (RG 03/19/92)
 *            e klin, Sat Apr 11 11:05:54 1992, Use colors for video attributes
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#ifndef lint
static char sccsid[] = "@(#) utree 3.03e-um (klin) Apr 11 1992 term.c";
#endif  /* !lint */

#include "defs.h"

/*      TEST:   Handling of sg/ug glitches (i.e. for bando terminal)
 *      Handling of sg/ug glitches may be wrong because i could test
 *      their handling on xterm simulating the sg/ug glitches only.
 */

/* ---- Local/global variables and definitions ------------------------ */

#define TCAPLEN 1024            /* Length of termcap buffers            */
#define KBUFLEN 254             /* Length of keyboard buffer            */
#define HUGE    9999            /* A huge number                        */

GLOBL char PC;                  /* Needed by termcap (?)                */
GLOBL char *UP;                 /* Needed by termcap (?)                */
GLOBL char *BC;                 /* Needed by termcap (?)                */

LOCAL int _XR;                  /* Carriage return glitch               */
LOCAL int _MS;                  /* Can move cursor in standout mode     */
LOCAL int _SG;                  /* Standout mode glitch: # of blanks    */
LOCAL int _UG;                  /* Underline glitch: # of blanks        */
LOCAL char *_CM;                /* Cursor motion                        */
LOCAL char *_CL;                /* Clear screen                         */
LOCAL char *_CD;                /* Clear to end of screen               */
LOCAL char *_CE;                /* Clear to end of line                 */
LOCAL char *_TI;                /* Init string for cursor motion        */
LOCAL char *_TE;                /* Exit string for cursor motion        */
LOCAL char *_KS;                /* Turn on keypad transmit mode         */
LOCAL char *_KE;                /* Turn off keypad transmit mode        */
LOCAL char *_MR;                /* Turn on reverse video mode           */
LOCAL char *_MB;                /* Turn on blink video mode             */
LOCAL char *_MD;                /* Turn on bold video mode              */
LOCAL char *_MH;                /* Turn on half bright video mode       */
LOCAL char *_ME;                /* Turn off all video attributes        */
LOCAL char *_US;                /* Start underlining                    */
LOCAL char *_UE;                /* End underlining                      */
LOCAL char *_IC;                /* Insert character at cursor position  */
LOCAL char *_DC;                /* Delete character at cursor position  */
LOCAL char *_AL;                /* Insert line above line cursor is on  */
LOCAL char *_DL;                /* Delete line cursor is on             */
LOCAL char *_CS;                /* Set scroll region                    */
LOCAL char *_NL;                /* New line (default: ^J = NL)          */
LOCAL char *_CR;                /* Carriage return (default: ^M = CR)   */
LOCAL char *_LE;                /* Cursor left (default: ^H = BS)       */
LOCAL char *_DO;                /* Cursor down                          */
LOCAL char *_SF;                /* Scroll screen up                     */
LOCAL char *_SR;                /* Scroll screen down                   */
LOCAL char *_SC;                /* Save cursor position                 */
LOCAL char *_RC;                /* Restore cursor position              */
LOCAL char *_VE;                /* Cursor normal                        */
LOCAL char *_VS;                /* Cursor very visible                  */
LOCAL char *_VI;                /* Cursor invisible                     */
LOCAL char *_BL;                /* Bell (default ^G = BEL)              */
LOCAL char *_AC = NULL;         /* Alternate character set              */
LOCAL char *_EA = NULL;         /* Enable alternate character set       */
LOCAL char *_AS = NULL;         /* Alternate character set on           */
LOCAL char *_AE = NULL;         /* Alternate character set off          */
#ifdef  XENIX   /* RG 11/22/91 */
LOCAL char *_G2 = NULL;         /* Upper left corner                    */
LOCAL char *_GH = NULL;         /* Horizontal bar                       */
LOCAL char *_G1 = NULL;         /* Upper right corner                   */
LOCAL char *_GV = NULL;         /* Vertical bar                         */
LOCAL char *_G4 = NULL;         /* Lower right corner                   */
LOCAL char *_G3 = NULL;         /* Lower left corner                    */
LOCAL char *_GD = NULL;         /* Top tee                              */
LOCAL char *_GL = NULL;         /* Right tee                            */
LOCAL char *_GU = NULL;         /* Bottom tee                           */
LOCAL char *_GR = NULL;         /* Left tee                             */
LOCAL char *_GC = NULL;         /* Plus sign = mark sign                */
#endif  /* XENIX */

#ifdef  USEANSICOLORS
LOCAL int  _CO = 0;             /* Number of colors                     */
LOCAL int  _AM = 0;             /* Automargin                           */
LOCAL char *_OP = NULL;         /* Reset original color pair            */
LOCAL char *_AB = NULL;         /* Set background color                 */
LOCAL char *_AF = NULL;         /* Set foreground color                 */
#endif  /* USEANSICOLORS */

LOCAL char termbuf[TCAPLEN];    /* Termcap buffer                       */
LOCAL int nfkeys;               /* Number of defined function keys      */
#ifdef BSD
LOCAL struct sgttyb raw, tty;   /* Terminal driver data record          */
# define TCGETA TIOCGETP
# define TCSETA TIOCSETP
#else   /* SYSV */
LOCAL struct termio raw, tty;
# ifndef HASFIONREAD
LOCAL int kbflag;               /* Input mode flag                      */
# endif /* !HASFIONREAD */
#endif  /* BSD */

LOCAL kchar kbuf[KBUFLEN+2];    /* Input buffer                         */
LOCAL int kcnt = 0;             /* Input buffer counter                 */
LOCAL int kind = 0;             /* Input buffer index                   */
LOCAL int curx = HUGE;          /* Current screen column                */
LOCAL int cury = HUGE;          /* Current screen line                  */
LOCAL int vatt = VA_NORMAL;     /* Current video attributes setting     */
LOCAL int gatt = GC_OFF;        /* Current graphic character set flag   */

/* ---- External variables and functions ------------------------------ */

#ifdef  BSD
EXTRN short ospeed;             /* Is ospeed really needed?             */
#endif  /* BSD */
EXTRN char *getenv();
EXTRN char *tgetstr();
EXTRN char *tgoto();

/* ---- Local/global functions and procedures ------------------------- */

/*
 *      INTERNAL USED ROUTINES
 */

/* Output one character for tputs() */
LOCAL int _putch(c)
  register int c;
{
  return(putc((unsigned char) c, stdout));

} /* _putch() */

/*TEST: Handling of sg/ug glitches */
/* Move cursor back for terminals with underline or standout glitch */
LOCAL VOID _backglitch(j)
  register int j;
{
  register int i;

  for(i = 0; i < j; i++) {
    if(_LE)
      tputs(_LE, 1, _putch);
    else if(BC)
      tputs(BC, 1, _putch);
    else
      (void) _putch('\b');
  }

} /* _backglitch() */

/*
 *      VIDEO ATTRIBUTE AND GRAPHIC CHARSET ROUTINES
 */

/* Turn on/off video attributes as defined in mask v */
GLOBL VOID videoset(v)
  register int v;
{
#ifdef  USEANSICOLORS
  if(colorcap && usecolors) {           /* Use colors for video attributes */
    switch(v) {
      case DA_NORMAL:                   /* Normal colors */
	tputs(tgoto(_AB, CS_BLUE, CS_BLUE), 1, _putch);
	tputs(tgoto(_AF, CS_CYAN, CS_CYAN), 1, _putch);
	break;
      case DA_REVERSE:                  /* Reverse colors */
	tputs(tgoto(_AB, CS_CYAN, CS_CYAN), 1, _putch);
	tputs(tgoto(_AF, CS_BLUE, CS_BLUE), 1, _putch);
	break;
      case DA_BOLD:                     /* Bold color */
	tputs(tgoto(_AB, CS_BLUE,  CS_BLUE),  1, _putch);
	tputs(tgoto(_AF, CS_WHITE, CS_WHITE), 1, _putch);
	break;
      case DA_HALF:                     /* Half color */
	tputs(tgoto(_AB, CS_BLUE, CS_BLUE), 1, _putch);
	tputs(tgoto(_AF, CS_CYAN, CS_CYAN), 1, _putch);
	break;
      case DA_ERROR:                    /* Error color */
	tputs(tgoto(_AB, CS_WHITE, CS_WHITE), 1, _putch);
	tputs(tgoto(_AF, CS_RED,   CS_RED),   1, _putch);
	break;
      case DA_MARK:                     /* Menu mark color */
	tputs(tgoto(_AB, CS_CYAN, CS_CYAN), 1, _putch);
	tputs(tgoto(_AF, CS_RED,  CS_RED),  1, _putch);
	break;
      case DA_BOLDREV:                  /* Reverse bold color */
	tputs(tgoto(_AB, CS_WHITE, CS_WHITE), 1, _putch);
	tputs(tgoto(_AF, CS_BLUE,  CS_BLUE),  1, _putch);
	break;
      case DA_HALFREV:                  /* Reverse half color */
	tputs(tgoto(_AB, CS_WHITE, CS_WHITE), 1, _putch);
	tputs(tgoto(_AF, CS_CYAN,  CS_CYAN),  1, _putch);
	break;
      case DA_BLINKREV:                 /* Reverse blink color */
	tputs(tgoto(_AB, CS_WHITE, CS_WHITE), 1, _putch);
	tputs(tgoto(_AF, CS_RED,   CS_RED),   1, _putch);
	break;
    }
    vatt = v;
    return;
  }
  else
#endif  /* USEANSICOLORS */
  if(vatt) {                            /* Video attributes off */
    if((vatt & VA_UNDERLINE) && _UE) {
      tputs(_UE, 1, _putch);
/*TEST: Handling of sg/ug glitches */
      if(_UG > 0)
	_backglitch(_UG);
      vatt &= ~VA_UNDERLINE;
    }
    if(vatt && _ME) {
      tputs(_ME, 1, _putch);
/*TEST: Handling of sg/ug glitches */
      if(_SG > 0)
	_backglitch(_SG);
    }
  }
  vatt = VA_NORMAL;
  if(v & VA_REVERSE && _MR) {           /* Video reverse */
    tputs(_MR, 1, _putch);
/*TEST: Handling of sg/ug glitches */
    if(_SG > 0)
      _backglitch(_SG);
    vatt |= VA_REVERSE;
  }
  if(v & VA_UNDERLINE && _US) {         /* Start underlining */
    tputs(_US, 1, _putch);
/*TEST: Handling of sg/ug glitches */
    if(_UG > 0)
      _backglitch(_UG);
    vatt |= VA_UNDERLINE;
  }
  if(v & VA_BLINK && _MB) {             /* Video blink */
    tputs(_MB, 1, _putch);
    vatt |= VA_BLINK;
  }
  if(v & VA_BOLD && _MD) {              /* Video bold */
    tputs(_MD, 1, _putch);
    vatt |= VA_BOLD;
  }
  if(v & VA_HALF && _MH) {              /* Video half bright */
    tputs(_MH, 1, _putch);
    vatt |= VA_HALF;
  }

} /* videoset() */

/* Turn on/off graphic character set as defined in flag f */
GLOBL VOID graphicset(f)
  register int f;
{
  if(gatt != f) {
    if(f && _AS)                /* Enable graphic charset */
      tputs(_AS, 1, _putch);
    else if(_AE)                /* Disable graphic charset */
      tputs(_AE, 1, _putch);
    gatt = f;
  }
#ifdef  USEANSICOLORS
  if(colorcap && usecolors)     /* Setting may destroy color settings ! */
    videoset(vatt);
#endif  /* USEANSICOLORS */

} /* graphicset() */

/*
 *      OUTPUT ROUTINES
 */

/* Ring the bell */
GLOBL VOID bell(f)
  register int f;
{
  if(f) {
    if(_BL)
      tputs(_BL, 1, _putch);
    else
      (void) _putch(7);
  }

} /* bell() */

/* Move cursor back one character */
GLOBL VOID backspace()
{
  if(curx > 0) {
    if(_LE)
      tputs(_LE, 1, _putch);
    else if(BC)
      tputs(BC, 1, _putch);
    else
      (void) _putch('\b');
    --curx;
  }

} /* backspace() */

/* Move to beginning of line */
GLOBL VOID begline()
{
  if(curx > 0) {
    curx = 0;
    if(_XR)
      (void) cursorxy(0, cury);
    else if(_CR)
      tputs(_CR, 1, _putch);
    else
      (void) _putch('\r');
  }

} /* begline() */

/* Move to beginning of next line */
GLOBL VOID newline()
{
  if(cury < lines) {
    begline();
    ++cury;
    if(_NL)
      tputs(_NL, 1, _putch);
    else if(_DO)
      tputs(_DO, 1, _putch);
    else
      (void) _putch('\n');
  }

} /* newline() */

/* Write character c if cursor is on screen */
GLOBL int putchar(c)
  register int c;
{
  if(videoattr != vatt)         /* Check and set video attributes */
    videoset(videoattr);
  if(graphattr != gatt)         /* Check and set graphic charset */
    graphicset(graphattr);
  switch(c) {
    case 7:                     /* Bell */
      bell(1);
      break;
    case '\b':                  /* Backspace */
      backspace();
      break;
    case '\t':                  /* Tab */
      if(curx < columns && cury < lines) {
	do
	  (void) _putch(' ');
	while(++curx % 8 && curx < columns);
      }
      break;
    case '\n':                  /* Newline */
      newline();
      break;
    case '\r':                  /* Carriage return */
      begline();
      break;
    default:                    /* Others */
      if(curx < columns && cury < lines && isprint(c & 0x7f)) {
	(void) _putch(c);
	++curx;
      }
      break;
  }
  return(curx < columns && cury < lines ? 1 : 0);

} /* putchar() */

/* Flush output buffer */
GLOBL VOID flushout()
{
  (void) fflush(stdout);

} /* flushout() */

/*
 *      INPUT ROUTINES
 */

/* Read one character from keyboard. Ignore or handle signals */
GLOBL int getchar()
{
  register int c;

#ifdef  BSD
  atread = 1;
# if    defined(SIGWINCH) && defined(TIOCGWINSZ)
  /* BSD: Signal SIGWINCH doesn't interrupt systemcall read() ! */
  /*      Set up here if SIGWINCH is catched and return K_SIZE  */
  if(setjmp(winchjump) && sizechange) {
    sizechange = atread = 0;
    return(K_SIZE);
  }
# endif /* SIGWINCH && TIOCGWINSZ */
  c = getc(stdin);
  atread = 0;
  return(c);
#else   /* SYSV */
  do {
    c = getc(stdin);
# if    defined(SIGWINCH) && defined(TIOCGWINSZ)
    /* SYSV: Signal SIGWINCH interrupts systemcall read() ! */
    /*       So return K_SIZE if signal SIGWINCH is catched */
    if(c < 0 && sizechange) {
      sizechange = 0;
      return(K_SIZE);
    }
# endif /* SIGWINCH && TIOCGWINSZ */
  }
  while(c < 0 && errno == EINTR);
  return(c);
#endif  /* BSD */

} /* getchar() */

/* Check if input from keyboard is pending */
GLOBL int keypressed()
{
  int c;

#if     defined(BSD) || defined(HASFIONREAD)
  /* Quick check how many chars are to read */
  return(ioctl(fileno(stdin), FIONREAD, &c) < 0 ? 0 : c);
#else   /* SYSV && !HASFIONREAD */
  /* Set stdin to no delay and try to read one char */
  (void) fcntl(fileno(stdin), F_SETFL, kbflag|O_NDELAY);
  c = getc(stdin);
  (void) fcntl(fileno(stdin), F_SETFL, kbflag);
  if(c >= 0) {
    (void) ungetc(c, stdin);
    return(1);
  }
  return(0);
#endif  /* BSD || HASFIONREAD */

} /* keypressed() */

/* Read a character from keyboard with respect to function keys */
GLOBL int getkey()
{
  register klist *fp, *lp, *p;
  register kchar c;
  register int k;

  /* Flush output buffer */
  flushout();

  /* Input buffer contains character(s) */
  if(kcnt > 0) {
    --kcnt;
    return((int) kbuf[kind++]);
  }

  /* Get next character */
  if((c = getchar()) == 0)      /* Map ASCII-NUL */
    c = 0200;
  /* Search for first matching entry in key binding list */
  for(fp = kroot; fp; fp = (klist *) KBNXT(fp))
    if(KBCHR(fp, 0) == c)
      break;
  /* No match: return character */
  if(fp == KNULL)
    return((int) c);

  /* Search for last matching entry in key binding list */
  lp = fp;
  while((p = (klist *) KBNXT(lp)) && KBCHR(p, 0) == c)
    lp = p;

  /* Continue comparison of input and key strings */
  for(k = 1; ; k++) {
    /* Match: return bound key symbol */
    if(fp == lp && KBCHR(fp, k) == 0) {
      kcnt = 0;
      if(KBSYM(fp) == K_STR && KBINS(fp)) {
	kind = 0;
	(void) ungetstring(KBINS(fp));
	break;
      }
      return(KBSYM(fp));
    }
    /* Else: get next character */
    if((c = getchar()) == 0)    /* Map ASCII-NUL */
      c = 0200;
    kbuf[kcnt++] = c;
    /* Search for next first and last matching entries in binding list */
    while(KBCHR(fp, k) != c && fp != lp)
      fp = (klist *) KBNXT(fp);
    while(KBCHR(lp, k) != c && lp != fp)
      lp = (klist *) KBPRV(lp);
    /* No match: exit loop */
    if(KBCHR(fp, k) != c)
      break;
  }

  /* No match: return character from input buffer */
  --kcnt;
  return((int) kbuf[kind++]);

} /* getkey() */

/* Put back character c into input buffer */
GLOBL VOID ungetkey(c)
  register int c;
{
  if(kcnt < KBUFLEN)
    kbuf[kcnt++] = (kchar) c;

} /* ungetkey() */

/* Put back string s into input buffer */
GLOBL int ungetstring(s)
  register char *s;
{
  register int c;

  while(*s) {
    if(*s == '\\')
      switch(*++s) {
	default:                        /* Error */
	  return(1);
	case 'b':                       /* Backspace ?? */
	  c = '\b';
	  break;
	case 'f':                       /* Formfeed  ?? */
	  c = '\f';
	  break;
	case 'r':                       /* Carriage return */
	case 'n':                       /* Newline */
	  c = '\n';
	  break;
	case 't':                       /* Tab */
	  c = '\t';
	  break;
	case 's':                       /* Space */
	  c = ' ';
	  break;
	case 'e':                       /* Escape */
	case 'E':
	  c = 0x1b;
	  break;
      }
    else if(*s == '^') {                /* Control chars ?? */
      ++s;
      if(*s == '?')                     /* DEL */
	c = 0x7f;
      else if(*s >= '@' && *s <= '_')   /* NUL .. US */
	c = *s - '@';
      else if(*s >= 'a' && *s <= 'z')   /* SOH .. SUB */
	c = *s - '`';
      else
	return(1);
    }
    else
      c = *s;
    ungetkey(c);
    ++s;
  }
  return(0);

} /* ungetstring() */

/*
 *      GLOBAL SCREEN ROUTINES
 */

/* Move the cursor to new x,y position */
GLOBL int cursorxy(x, y)
  register int x, y;
{
  if(x < 0) x = columns + x;
  if(y < 0) y = lines   + y;
  if(x < 0 || x >= columns || y < 0 || y >= lines) {
    curx = cury = HUGE;
    return(0);
  }
  if( !_MS && vatt)             /* Reset video attributes */
    videoset(VA_NORMAL);
  if(gatt)                      /* Reset graphic charset */
    graphicset(GC_OFF);
  tputs(tgoto(_CM, x, y), 1, _putch);
  curx = x;
  cury = y;
  return(1);

} /* cursorxy() */

/* Return current cursor position */
GLOBL VOID cursorpos(x, y)
  register int *x, *y;
{
  *x = curx;
  *y = cury;

} /* cursorpos() */

/* Insert character at cursor position */
GLOBL int insertchar()
{
  if(_IC) {
    tputs(_IC, 1, _putch);
    return(1);
  }
  return(0);

} /* insertchar() */

/* Delete character under cursor */
GLOBL int deletechar()
{
#ifdef  USEANSICOLORS
  if(colorcap && usecolors)
    return(0);
#endif  /* USEANSICOLORS */
  if(_DC) {
    tputs(_DC, 1, _putch);
    return(1);
  }
  return(0);

} /* deletechar() */

/* Insert n lines above line cursor is on */
GLOBL int insertline(y, n)
  register int y, n;
{
  if(_AL && cursorxy(0, y)) {
    while(n--)
      tputs(_AL, 1, _putch);
    return(1);
  }
  return(0);

} /* insertline() */

/* Delete n lines cursor is on */
GLOBL int deleteline(y, n)
  register int y, n;
{
  if(_DL && cursorxy(0, y)) {
    while(n--)
      tputs(_DL, 1, _putch);
    return(1);
  }
  return(0);

} /* deleteline() */

/* Set scroll window from line f to line t */
GLOBL int windowset(f, t)
  register int f, t;
{
  /* Set scroll region from line f to line t */
  if(_CS && f <= t) {
    tputs(tgoto(_CS, t, f), t - f + 1, _putch);
    return(1);
  }
  return(0);

} /* windowset() */

/* Scroll n lines up window from line f to line t */
GLOBL int windowup(f, t, n)
  register int f, t, n;
{
#ifdef  USEANSICOLORS
  if(colorcap && usecolors)     /* No scrolling ! */
    return(0);
#endif  /* USEANSICOLORS */
  /* Set scrollregion from f to t and scroll up n lines */
  if(_SF && windowset(f, t)) {
    (void) cursorxy(0, t);
    while(n-- > 0)
      tputs(_SF, lines, _putch);
    /* Reset scroll region to screen */
    (void) windowset(0, lines - 1);
    return(1);
  }
  /* Scroll up with a combination of insert and delete line */
  else if(_AL && deleteline(f, n)) {
    (void) insertline(t - n + 1, n);
    return(1);
  }
  return(0);

} /* windowup() */

/* Scroll n lines down in window from line f to line t */
GLOBL int windowdown(f, t, n)
  register int f, t, n;
{
#ifdef  USEANSICOLORS
  if(colorcap && usecolors)     /* No scrolling ! */
    return(0);
#endif  /* USEANSICOLORS */
  /* Set scrollregion from f to t and scroll down n lines */
  if(_SR && windowset(f, t)) {
    (void) cursorxy(0, f);
    while(n-- > 0)
      tputs(_SR, lines, _putch);
    /* Reset scroll region to screen */
    (void) windowset(0, lines - 1);
    return(1);
  }
  /* Scroll down with a combination of insert and delete line */
  else if(_AL && deleteline(t - n + 1 ,n)) {
    (void) insertline(f, n);
    return(1);
  }
  return(0);

} /* windowdown() */

/* Clear from cursor position to end of line */
GLOBL VOID clearline()
{
#ifdef  USEANSICOLORS
  register int i, x;

  if(colorcap && usecolors) {   /* Fill with blanks ! */
    x = curx;
    for(i = curx; i < columns - 1; i++)
      (void) _putch(' ');
    if(_AM && _IC) {
      tputs(tgoto(_CM, columns - 2, cury), 1, _putch);
      tputs(_IC, 1, _putch);
    }
    _putch(' ');
    tputs(tgoto(_CM, x, cury), 1, _putch);
    curx = x;
    return;
  }
#endif  /* USEANSICOLORS */
  if( !_MS && vatt)             /* Reset video attributes */
    videoset(VA_NORMAL);
  if(gatt)                      /* Reset graphic charset */
    graphicset(GC_OFF);
  tputs(_CE, 1, _putch);

} /* clearline() */

/* Clear the screen */
GLOBL VOID clearscreen()
{
#ifdef  USEANSICOLORS
  register int i;

  if(colorcap && usecolors) {   /* Fill with blanks ! */
    videoset(VA_NORMAL);
    for(i = 0; i < lines; i++) {
      (void) cursorxy(0, i);
      clearline();
    }
    curx = cury = 0;
    return;
  }
#endif  /* USEANSICOLORS */
  if( !_MS && vatt)             /* RG 03/19/92: Reset video attributes */
    videoset(VA_NORMAL);
  if(gatt)                      /* RG 03/19/92: Reset graphic charset */
    graphicset(GC_OFF);
  tputs(_CL, lines, _putch);
  curx = cury = 0;

} /* clearscreen() */

/* Clear to end of screen */
GLOBL int cleartoend()
{
#ifdef  USEANSICOLORS
  register int i;

  if(colorcap && usecolors) {   /* Fill with blanks ! */
    for(i = cury; i < lines; i++)  {
      (void) cursorxy(0, i);
      clearline();
    }
    return(1);
  }
#endif  /* USEANSICOLORS */
  if(_CD) {
    if( !_MS && vatt)           /* RG 03/19/92: Reset video attributes */
      videoset(VA_NORMAL);
    if(gatt)                    /* RG 03/19/92: Reset graphic charset */
      graphicset(GC_OFF);
    tputs(_CD, lines, _putch);
    return(1);
  }
  return(0);

} /* cleartoend() */

/* Clear screen from line f to line t */
GLOBL VOID clearwindow(f, t)
  register int f, t;
{
#ifdef  USEANSICOLORS
  register int i;

  if(colorcap && usecolors) {   /* Fill with blanks ! */
    for(i = f; i <= t; i++) {
      (void) cursorxy(0, i);
      clearline();
    }
    return;
  }
#endif  /* USEANSICOLORS */
  /* Clear one line only */
  if(f == t) {
    if( !_MS && vatt)           /* RG 03/19/92: Reset video attributes */
      videoset(VA_NORMAL);
    if(gatt)                    /* RG 03/19/92: Reset graphic charset */
      graphicset(GC_OFF);
    (void) cursorxy(0, f);
    tputs(_CE, 1, _putch);
  }
  /* Try clear to end of screen */
  else if(t == lines - 1 && _CD) {
    if( !_MS && vatt)           /* RG 03/19/92: Reset video attributes */
      videoset(VA_NORMAL);
    if(gatt)                    /* RG 03/19/92: Reset graphic charset */
      graphicset(GC_OFF);
    (void) cursorxy(0, f);
    tputs(_CD, lines, _putch);
  }
  /* Try clear by window scrolling */
  else if( !windowup(f, t, t - f + 1)) {
    /* Clear line by line */
    if( !_MS && vatt)           /* RG 03/19/92: Reset video attributes */
      videoset(VA_NORMAL);
    if(gatt)                    /* RG 03/19/92: Reset graphic charset */
      graphicset(GC_OFF);
    while(f <= t) {
      (void) cursorxy(0, f++);
      tputs(_CE, 1, _putch);
    }
    (void) cursorxy(0, f);
  }

} /* clearwindow() */

/* Turn on/off keypad transmit mode if f is set/unset */
GLOBL VOID keypadxmit(f)
  register int f;
{
  if(f && _KS)                  /* Keypad transmit mode */
    tputs(_KS, 1, _putch);
  else if(_KE)                  /* Keypad normal mode */
    tputs(_KE, 1, _putch);

} /* keypadxmit() */

/* Do some cursor functions defined in mask c */
GLOBL VOID cursorset(c)
  register int c;
{
  if(c & CF_SAVE && _SC) {              /* Save cursor */
    if(vatt)                            /* Reset video attributes */
      videoset(VA_NORMAL);
    tputs(_SC, 1, _putch);
  }
  else if(c & CF_RESTORE && _RC)        /* Restore cursor */
    tputs(_RC, 1, _putch);
  if(c & CF_VISIBLE && _VE)             /* Cursor visible */
    tputs(_VE, 1, _putch);
  else if(c & CF_INVISIBLE && _VI)      /* Cursor invisible */
    tputs(_VI, 1, _putch);

} /* cursorset() */

/* Init graphical character set if f is set */
GLOBL VOID initgraphics(f)
  register int f;
{
  register char *ac;

  GC_HB = '-';                  /* Use ascii meta chars as default */
  GC_VB = GC_LT = GC_RT = '|';
  GC_TT = GC_BT = GC_UL = GC_LL = GC_UR = GC_LR = GC_TG = '+';
  graphcap = 0;
  if(f && (ac = _AC)) {         /* If f is set and acsc is defined */
#ifdef  AIX
    GC_UL = *ac ? *ac++ : '+';  /* Upper left corner */
    GC_HB = *ac ? *ac++ : '-';  /* Horizontal bar */
    GC_UR = *ac ? *ac++ : '+';  /* Upper right corner */
    GC_VB = *ac ? *ac++ : '|';  /* Vertical bar */
    GC_LR = *ac ? *ac++ : '+';  /* Lower right corner */
    GC_LL = *ac ? *ac++ : '+';  /* Lower left corner */
    GC_TT = *ac ? *ac++ : '+';  /* Top tee */
    GC_RT = *ac ? *ac++ : '|';  /* Right tee */
    GC_BT = *ac ? *ac++ : '+';  /* Bottom tee */
    GC_LT = *ac ? *ac++ : '|';  /* Left tee */
    GC_TG = *ac ? *ac++ : '+';  /* Tag: plus sign */
#else   /* !AIX */
# ifdef XENIX   /* RG 11/22/91 */
    if(*ac == '\0') {
      GC_UL = _G2 ? *_G2 : '+'; /* Upper left corner */
      GC_HB = _GH ? *_GH : '-'; /* Horizontal bar */
      GC_UR = _G1 ? *_G1 : '+'; /* Upper right corner */
      GC_VB = _GV ? *_GV : '|'; /* Vertical bar */
      GC_LR = _G4 ? *_G4 : '+'; /* Lower right corner */
      GC_LL = _G3 ? *_G3 : '+'; /* Lower left corner */
      GC_TT = _GD ? *_GD : '+'; /* Top tee */
      GC_RT = _GL ? *_GL : '|'; /* Right tee */
      GC_BT = _GU ? *_GU : '+'; /* Bottom tee */
      GC_LT = _GR ? *_GR : '|'; /* Left tee */
      GC_TG = _GC ? *_GC : '+'; /* Tag: plus sign */
    }
# endif /* XENIX */
    do {
      switch(*ac) {
	default:                /* Skip */
	  ++ac;
	case '\0':              /* End of acsc */
	  break;
	case 'j':               /* Lower right corner */
	  GC_LR = *++ac ? *ac : '+'; break;
	case 'k':               /* Upper right corner */
	  GC_UR = *++ac ? *ac : '+'; break;
	case 'l':               /* Upper left corner */
	  GC_UL = *++ac ? *ac : '+'; break;
	case 'm':               /* Lower left corner */
	  GC_LL = *++ac ? *ac : '+'; break;
	case 'q':               /* Horizontal bar */
	  GC_HB = *++ac ? *ac : '-'; break;
	case 't':               /* Left tee */
	  GC_LT = *++ac ? *ac : '|'; break;
	case 'u':               /* Right tee */
	  GC_RT = *++ac ? *ac : '|'; break;
	case 'v':               /* Bottom tee */
	  GC_BT = *++ac ? *ac : '+'; break;
	case 'w':               /* Top tee */
	  GC_TT = *++ac ? *ac : '+'; break;
	case 'x':               /* Vertical bar */
	  GC_VB = *++ac ? *ac : '|'; break;
	case '`':               /* Tag sign: diamond */
	  GC_TG = *++ac ? *ac : '+'; break;
	case 'n':               /* Alternate tag sign: plus */
	  if(*++ac && GC_TG == '+')
	    GC_TG = *ac;
	  break;
      }
    } while(*ac && *++ac);
#endif  /* AIX */
    graphcap = 1;
    if(_EA)
      tputs(_EA, 1, _putch);
  }

} /* initgraphics() */

/*
 *      COLOR HANDLING ON ANSI COMPATIBLE TERMINALS
 */
#ifdef  USEANSICOLORS
/* Reset any color settings to original colors and clear screen */
GLOBL VOID colorset(f)
  register int f;
{
  if(colorcap) {
    if(f == CS_RESET) {
      if(_OP)
	tputs(_OP, 1, _putch);
      tputs(_CL, lines, _putch);
    }
    else if(f == CS_INIT && usecolors) {
      clearscreen();
      flushout();
    }
  }

} /* colorset() */
#endif  /* USEANSICOLORS */

/*
 *      TERMINAL ROUTINES
 */

/* Switch terminal to raw mode */
GLOBL VOID terminalraw(f)
  register int f;
{
  if(_TI && f)
    tputs(_TI, 1, _putch);
  curx = cury = HUGE;
  keypadxmit(KP_XMIT);
  flushout();
  (void) ioctl(fileno(stdin), TCSETA, &raw);

} /* terminalraw() */

/* Reset terminal to initial mode */
GLOBL VOID terminalreset(f)
  register int f;
{
  if(f && _TE)
    tputs(_TE, 1, _putch);
  curx = cury = HUGE;
  keypadxmit(KP_NORMAL);
#ifdef  USEANSICOLORS
  if(colorcap && usecolors)
    colorset(CS_RESET);
  else
#endif  /* USEANSICOLORS */
  videoset(VA_NORMAL);
  flushout();
  (void) ioctl(fileno(stdin), TCSETA, &tty);

} /* terminalreset() */

/* Enable signal handling */
GLOBL VOID enablesignals()
{
#ifdef BSD
  raw.sg_flags &= ~RAW;
  raw.sg_flags |= CBREAK;
#else   /* SYSV */
  raw.c_lflag |= ISIG;
#endif  /* BSD */
  flushout();
  (void) ioctl(fileno(stdin), TCSETA, &raw);

} /* enablesignals() */

/* Disable signal handling */
GLOBL VOID disablesignals()
{
#ifdef BSD
  raw.sg_flags &= ~CBREAK;
  raw.sg_flags |= RAW;
#else   /* SYSV */
  raw.c_lflag &= ~ISIG;
#endif  /* BSD */
  flushout();
  (void) ioctl(fileno(stdin), TCSETA, &raw);

} /* disablesignals() */

/*
 *      INITIALIZATION AND RESET
 */

/* Init screen, return error message on error */
GLOBL char *initscreen(term)
  register char *term;
{
  char termcap[TCAPLEN];
  char *cp = termbuf, *pc;

  /* Get terminal type and init terminal data base */
  if(term == NULL)
    return("Terminal variable TERM not defined");
  switch(tgetent(termcap, term)) {
    case -1 :
      return("Terminfo library not found");
    case 0 :
      return("Unknown terminal type");
  }

  /* Get all needed terminal capabilities from data base */
  if((columns = tgetnum("co")) <= 0)
    columns = MINCOLS;
  if((lines   = tgetnum("li")) <= 0)
    lines = MINLINS;
  UP = tgetstr("up", &cp);
  BC = tgetstr("bc", &cp);
  if(pc = tgetstr("pc", &cp))
    PC = *pc;
  _CM = tgetstr("cm", &cp);
  _CL = tgetstr("cl", &cp);
  _CE = tgetstr("ce", &cp);

  /* Cursor motion, clear screen and clear line must be defined ! */
  if(_CM == NULL || _CE == NULL || _CL == NULL)
    return("Terminal too stupid");

  _XR = tgetflag("xr");
  _MS = tgetflag("ms");
  _SG = tgetnum("sg");
#ifdef  BSD
  _UG = tgetnum("ug");
#else   /* SYSV */
  _UG = _SG;
#endif  /* BSD */
  _CD = tgetstr("cd", &cp);
  _TI = tgetstr("ti", &cp);
  _TE = tgetstr("te", &cp);
  _KS = tgetstr("ks", &cp);
  _KE = tgetstr("ke", &cp);
  if((_ME = tgetstr("me", &cp)) == NULL)
    _ME = tgetstr("se", &cp);
  if((_MR = tgetstr("mr", &cp)) == NULL)
    _MR = tgetstr("so", &cp);
  if(_US = tgetstr("us", &cp)) {
    if((_UE = tgetstr("ue", &cp)) && _ME && EQU(_ME, _UE))
      _UE = NULL;
  }
  /* No more video attributes and no cursor visibility functions for   */
  /* terminals with standout mode and/or underline glitch (i.e. bando) */
  if(_UG <= 0 && _SG <= 0) {
    _MB = tgetstr("mb", &cp);
    _MD = tgetstr("md", &cp);
    _MH = tgetstr("mh", &cp);
    _VS = tgetstr("vs", &cp);
    _VI = tgetstr("vi", &cp);
    _VE = tgetstr("ve", &cp);
  }
  _IC = tgetstr("ic", &cp);
  _DC = tgetstr("dc", &cp);
  _AL = tgetstr("al", &cp);
  _DL = tgetstr("dl", &cp);
  _CS = tgetstr("cs", &cp);
  _NL = tgetstr("nl", &cp);
  _CR = tgetstr("cr", &cp);
  _LE = tgetstr("le", &cp);
  _DO = tgetstr("do", &cp);
  _SF = tgetstr("sf", &cp);
  _SR = tgetstr("sr", &cp);
  _SC = tgetstr("sc", &cp);
  _RC = tgetstr("rc", &cp);
  _BL = tgetstr("bl", &cp);

#ifdef  AIX
  _AC = tgetstr("bx", &cp);     /* Use box1 (bx) instead of acsc (ac) */
  if( !(_AS = tgetstr("as", &cp)))
    _AS = tgetstr("f1", &cp);   /* 3.2: Use font1 (f1) instead of smacs (as) */
  if( !(_AE = tgetstr("ae", &cp)))
    _AE = tgetstr("f0", &cp);   /* 3.2: Use font0 (f0) instead of rmacs (ae) */
#else   /* ACSC */
  _EA = tgetstr("eA", &cp);
  _AC = tgetstr("ac", &cp);
  _AS = tgetstr("as", &cp);
  _AE = tgetstr("ae", &cp);
# ifdef  XENIX  /* RG 11/22/91 */
  if(_AC == NULL) {
    _AC = "";                   /* Use empty string _AC as flag */
    _AS = tgetstr("GS", &cp);
    _AE = tgetstr("GE", &cp);
    _G2 = tgetstr("G2", &cp);
    _GH = tgetstr("GH", &cp);
    _G1 = tgetstr("G1", &cp);
    _GV = tgetstr("GV", &cp);
    _G4 = tgetstr("G4", &cp);
    _G3 = tgetstr("G3", &cp);
    _GD = tgetstr("GD", &cp);
    _GL = tgetstr("GL", &cp);
    _GU = tgetstr("GU", &cp);
    _GR = tgetstr("GR", &cp);
    _GC = tgetstr("GC", &cp);
  }
# endif /* XENIX */
#endif  /* AIX */

#ifdef  USEANSICOLORS
  if((_CO = tgetnum("Co")) > 7) {       /* Seems to know ansi colors */
    _AM = tgetflag("am");
    _OP = tgetstr("op", &cp);
    if( !(_AB = tgetstr("AB", &cp)))
      _AB = tgetstr("Sb", &cp);
    if( !(_AF = tgetstr("AF", &cp)))
      _AF = tgetstr("Sf", &cp);
  }
  if(_CO < 8 || !_AB || !_AF)
    colorcap = 0;
  else
    colorcap = 1;
#endif  /* USEANSICOLORS */

  if(_CS && !_SF) {
    if(_DO)
      _SF = _DO;
    else if(_NL)
      _SF = _NL;
    else
      _SF = "\n";
  }
  if(_VS && !_VE)
    _VE = _VS;

  /* Set capability flags */
  glitchcap = _UG > 0 || _SG > 0;
  scrollcap = (_AL && _DL) || (_CS && _SF && _SR);
  cursorcap = 0;
  if(_VE && _VI)
    cursorcap |= CF_VISIBLE|CF_INVISIBLE;
  if(_SC && _RC)
    cursorcap |= CF_SAVE|CF_RESTORE;
  videocap = 0;
  if(_MR)
    videocap |= VA_REVERSE;
  if(_MB)
    videocap |= VA_BLINK;
  if(_MD)
    videocap |= VA_BOLD;
  if(_MH)
    videocap |= VA_HALF;
  if(_US)
    videocap |= VA_UNDERLINE;

  /* Initialize key bindings */
  initbindings(term, &cp);

  /* Get terminal driver data and initialize raw mode */
  if(ioctl(fileno(stdin), TCGETA, &tty) < 0)
    return("Error in ioctl");
#ifdef  STRUCTCOPY
  (void) STRUCTCOPY(tty, raw, sizeof(tty));
#else   /* STRUCTASSIGN */
  raw = tty;
#endif  /* STRUCTCOPY */

#ifdef BSD
  ospeed = tty.sg_ospeed;
  raw.sg_flags |= ANYP|RAW;
  raw.sg_flags &= ~(ECHO|XTABS);
#else   /* SYSV */
  raw.c_cc[VMIN] = 1;
  raw.c_cc[VTIME] = 0;
  raw.c_iflag |= IGNBRK;
  raw.c_iflag &= ~(ICRNL|ISTRIP);
  raw.c_oflag |= ONLCR|TAB0;
  raw.c_cflag |= CS8;
  raw.c_cflag &= ~PARENB;
  raw.c_lflag &= ~(ECHO|ICANON|ISIG);
#endif  /* BSD */

#if     !defined(BSD) && !defined(HASFIONREAD)
  /* Get keyboard flag and set to no delay mode */
  kbflag = fcntl(fileno(stdin), F_GETFL, 0) & O_NDELAY;
#endif  /* !BSD && !HASFIONREAD */

  return(NULL);

} /* initscreen() */

/* Restore screen and exit */
GLOBL int exitscreen(rval)
  int rval;
{
  cursorset(CF_VISIBLE);
  (void) cursorxy(0, lines-1);
  terminalreset(1);
  flushout();
  exit(rval);

} /* exitscreen() */
