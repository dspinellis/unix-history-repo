/*
 *      ECHO.C
 *      UTREE input and output routines.
 *      3.01-um klin, Wed May  1 15:37:35 1991
 *              klin, Sat Oct 26 15:30:11 1991, putfile() changed
 *      3.02-um klin, Fri Nov  1 13:40:54 1991, putfile() changed
 *              klin, Sun Nov 24 15:30:35 1991, Video attributes changed
 *      3.03-um klin, Tue Feb 11 19:39:09 1992, Generic list glist added
 *                                              getline() and putfxy() changed
 *                                              printf() no more needed
 *              klin, Sun Feb 23 17:14:20 1992, Key handling and key bindings
 *                                              changed
 *            c klin, Mon Mar 30 11:13:46 1992, Bug fix in hitakey()
 *            e klin, Sat Apr 11 11:05:54 1992, Use colors for video attributes
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#ifndef lint
static char sccsid[] = "@(#) utree 3.03e-um (klin) Apr 11 1992 echo.c";
#endif  /* !lint */

#include "defs.h"

/* ---- Local variables and definitions ------------------------------- */

#ifdef  BUFSIZ
# define FBUFLEN BUFSIZ         /* Format buffer length                 */
#else   /* !BUFSIZ */
# define FBUFLEN 512
#endif  /* BUFSIZ */

LOCAL char helpbuf[FBUFLEN];    /* Help line buffer                     */
LOCAL char echobuf[FBUFLEN];    /* Echo line buffer                     */

/* ---- Functions and procedures -------------------------------------- */

/* Switch graphical character on/off */
GLOBL int setgraphic(f)
  register int f;
{
  if(graphcap && VARSET(V_GC)) {
    graphattr = f;
    return(1);
  }
  return(0);

} /* setgraphic() */

/* Set video attributes from display attribute f */
GLOBL VOID setvideo(f)
  register int f;
{
#ifdef  USEANSICOLORS
  if(colorcap && usecolors)
    videoattr = f;
  else
#endif  /* USEANSICOLORS */
  if(videomode > 0) {
    switch(f) {
      case DA_NORMAL:           /* Reset attributes */
	videoattr = VA_NORMAL;
	break;
      case DA_REVERSE:          /* Invert */
	videoattr = VA_REVERSE;
	break;
      case DA_BOLD:             /* Highlight */
	if(videomode > 1)
	  videoattr = VA_BOLD;
	break;
      case DA_HALF:             /* Hide */
	if(videomode > 1)
	  videoattr = VA_HALF;
	break;
      case DA_ERROR:            /* Error */
	if(videomode > 1)
	  videoattr = VA_BOLD|VA_BLINK;
	break;
      case DA_MARK:             /* Menu mark */
	videoattr = videomode > 1 ? VA_BOLD|VA_REVERSE : VA_UNDERLINE|VA_REVERSE;
	break;
      case DA_BOLDREV:          /* Reverse hightlight */
	videoattr = videomode > 1 ? VA_BOLD|VA_REVERSE : VA_REVERSE;
	break;
      case DA_HALFREV:          /* Reverse hide */
	videoattr = videomode > 1 ? VA_HALF|VA_REVERSE : VA_REVERSE;
	break;
      case DA_BLINKREV:         /* Reverse blink */
	videoattr = videomode > 1 ? VA_BLINK|VA_REVERSE : VA_REVERSE;
	break;
      default:
	break;
    }
  }
  else
    videoattr = VA_NORMAL;

} /* setvideo() */

/* Write character c to column x at line y */
GLOBL VOID putcxy(x, y, c)
  register int x, y, c;
{
  if(cursorxy(x, y))
    (void) putchar(c);

} /* putcxy() */

/* Write string s to column x at line y. Return new column position */
GLOBL int putsxy(x, y, s)
  int x, y;
  register char *s;
{
  (void) cursorxy(x, y);
  while(*s && *s != '\n' && putchar(*s))
    ++s;
  cursorpos(&x, &y);
  return(x);

} /* putsxy() */

/* Write formatted to column x at line y */
#if     !defined(BSD) || defined(HASVSPRINTF)
/*VARARGS3*/
GLOBL int putfxy(x, y, c, va_alist)
  register int x, y, c;
  va_dcl
{
  va_list ap;
  char buf[FBUFLEN];
  register char *fmt;
  register int n;

  va_start(ap);
  fmt = va_arg(ap, char *);
  (void) vsprintf(buf, fmt, ap);
  va_end(ap);
#else   /* BSD && !HASVSPRINTF */
/*VARARGS4*/
GLOBL int putfxy(x, y, c, fmt, p1, p2, p3, p4, p5, p6)
  int x, y, c;
  char *fmt, *p1, *p2, *p3, *p4, *p5, *p6;
{
  char buf[FBUFLEN];
  register int n;

  (void) sprintf(buf, fmt, p1, p2, p3, p4, p5, p6);
#endif  /* !BSD || HASVSPRINTF */
  n = putsxy(x, y, buf);
  if(n < columns && c)
    clearline();
  return(n);

} /* putfxy() */

/* Write formatted to echoline, return new column position */
#if     !defined(BSD) || defined(HASVSPRINTF)
/*VARARGS0*/
GLOBL int putecho(va_alist)
  va_dcl
{
  va_list ap;
  register char *fmt;
  register int n;

  va_start(ap);
  fmt = va_arg(ap, char *);
  (void) vsprintf(echobuf, fmt, ap);
  va_end(ap);
#else   /* BSD && !HASVSPRINTF */
/*VARARGS1*/
GLOBL int putecho(fmt, p1, p2, p3, p4, p5, p6)
  char *fmt, *p1, *p2, *p3, *p4, *p5, *p6;
{
  register int n;

  (void) sprintf(echobuf, fmt, p1, p2, p3, p4, p5, p6);
#endif  /* !BSD || HASVSPRINTF */
  if((n = putsxy(0, echoline, echobuf)) < columns) {
    (void) putchar(' ');
    ++n;
    clearline();
  }
  treeflag |= SF_ECHO;
  fileflag |= SF_ECHO;
  return(n);

} /* putecho() */

/* Write formatted to helpline */
#if     !defined(BSD) || defined(HASVSPRINTF)
/*VARARGS0*/
GLOBL VOID puthelp(va_alist)
  va_dcl
{
  va_list ap;
  register char *fmt;

  va_start(ap);
  fmt = va_arg(ap, char *);
  (void) vsprintf(helpbuf, fmt, ap);
  va_end(ap);
#else   /* BSD && !HASVSPRINTF */
/*VARARGS1*/
GLOBL VOID puthelp(fmt, p1, p2, p3, p4, p5, p6)
  char *fmt, *p1, *p2, *p3, *p4, *p5, *p6;
{
  (void) sprintf(helpbuf, fmt, p1, p2, p3, p4, p5, p6);
#endif  /* !BSD || HASVSPRINTF */
  setvideo(DA_REVERSE);
  (void) putsxy(0, helpline, helpbuf);
  while(putchar(' '))
    ;
  setvideo(DA_NORMAL);
  treeflag |= SF_HELP;
  fileflag |= SF_HELP;

} /* puthelp() */

/* Write menu title t and commands line s to help line */
GLOBL VOID putmenu(t, s)
  register char *t, *s;
{
  (void) cursorxy(0, helpline);
  setvideo(DA_REVERSE);
  while(*t && putchar(*t))      /* Title */
    ++t;
  while(*s && putchar(*s)) {    /* Menu line */
    if(*s == ' ' && *(s+1) && *(s+1) != ' ') {
      setvideo(DA_MARK);
      (void) putchar(*++s);
      setvideo(DA_REVERSE);
    }
    ++s;
  }
  while(putchar(' '))
    ;
  setvideo(DA_NORMAL);
  treeflag |= SF_HELP;
  fileflag |= SF_HELP;

} /* putmenu() */

/* Write filename and mode or tag flag */
GLOBL VOID putfile(dp, f, fs)
  register dlist *dp;
  register int f, fs;
{
  register char *s;
  register int i;

  /* File on file screen? */
  if(fs && FFROW(dp, f) >= firstline && FFROW(dp, f) <= lastline)
    (void) cursorxy(FFCOL(dp, f), FFROW(dp, f));
  /* Or file window on tree screen? */
  else if( !fs && FTROW(f) >= firstfline && FTROW(f) <= lastfline)
    (void) cursorxy(FTCOL(f), FTROW(f));
  /* Out of bounds */
  else
    return;

  if(DNFIL(dp) > 0 && DFVEC(dp)) {
    if(ISTAG(dp, f) && setgraphic(GC_ON)) {
      (void) putchar(GC_TG);
      (void) setgraphic(GC_OFF);
    }
    else
      (void) putchar(FITAG(dp, f));
    /* Mark tagged or current file */
    if(fs) {
      if(fs == FF_MARK)
	setvideo(DA_BLINKREV);
      else if(f == DFCUR(dp))
	setvideo(DA_BOLDREV);
      else
	setvideo(DA_BOLD);
    }
    /* Write file mode and filename */
    (void) putchar(FMODE(dp, f));
    for(s = FFNAM(dp, f), i = 0; i < FNAMSZ && s[i]; i++)
      (void) putchar(s[i]);
    /* Cut off too long filenames */
    if(s[i])
      (void) putchar(s[i+1] ? '>' : s[i]);
    else
      while(i++ <= FNAMSZ && putchar(' '))
	;
    if(fs)
      setvideo(DA_NORMAL);
  }

} /* putfile() */

/* Write message s to line l with videoattr v and get a key */
/*VARARGS1*/
GLOBL int hitakey(s, l, v)
  register char *s;
  register int l, v;
{
  register int c;

  if(s) {
    if(v)
      setvideo(v);
    if(l == echoline) {
      (void) putfxy(0, l, 1, "%s ", s);
      treeflag |= SF_ECHO;
      fileflag |= SF_ECHO;
    }
    else
      (void) putfxy(0, l, 1, " %s ", s);
    if(v)
      setvideo(DA_NORMAL);
  }
  cursorset(CF_VISIBLE);
  c = getkey();
  cursorset(CF_INVISIBLE);
  flushout();
  switch(c) {
    case K_BRK:                 /* Interrupt */
    case K_CANC:
      c = RV_INT;
      break;
    case K_SIZE:                /* Screen size changed */
      c = RV_SIZ;
      break;
    case K_EOF:                 /* EOF */
      c = RV_END;
      break;
    case K_SEL:                 /* CR or NL */
      c = K_SEL;
      break;
    default:                    /* Return lower character */
      if(c > 0xff)
	c = RV_NUL;
      else if(isupper(c))
	c = tolower(c);
      break;
  }
  return(c);

} /* hitakey() */

/* Write error message to echo line and get a key */
GLOBL int errequest(fn, s)
  register char *fn, *s;
{
  bell(1);
  setvideo(DA_ERROR);
  (void) putecho("!! %s: %s", fn, s);
  setvideo(DA_NORMAL);
  return(hitakey(NULL));

} /* errequest() */

/* Read and edit a line */
GLOBL int getline(buf, max, cur, hk, pre, rp, f)
  char *buf, *pre;
  register int max, cur, hk, f;
  glist *rp;
{
  char help[FBUFLEN], echo[FBUFLEN];
  int xmin, xpos;
  register int c;

  (void) strcpy(help, helpbuf); /* Save current help and echo lines */
  (void) strcpy(echo, echobuf);
  xmin = xpos = 0;              /* Setup input buffer variables */
  if(pre)
    (void) strcpy(buf, pre);
  else
    buf[0] = '\0';
  while(1) {                    /* Edit line loop */
    cursorset(CF_VISIBLE);
    c = editline(buf, max, cur, &xmin, &xpos);
    cursorset(CF_INVISIBLE);
    flushout();
    switch(c) {
      case RV_HLP:              /* Help requested */
	if((c = showhelp(hk)) == RV_END)
	  return(c);
	puthelp("%s", help);
	(void) putecho("%s", echo);
	break;
      case RV_PRV:              /* Previous in list */
	if(rp) {
	  if(f)
	    f = 0;
	  else if(GPREV(rp))
	    rp = GPREV(rp);
	  else {
	    while(GNEXT(rp))
	      rp = GNEXT(rp);
	    bell(1);
	  }
	  (void) strcpy(buf, GNAME(rp));
	}
	else
	  bell(1);
	break;
      case RV_NXT:              /* Next in list */
	if(rp) {
	  if(f)
	    f = 0;
	  else if(GNEXT(rp))
	    rp = GNEXT(rp);
	  else {
	    while(GPREV(rp))
	      rp = GPREV(rp);
	    bell(1);
	  }
	  (void) strcpy(buf, GNAME(rp));
	}
	else
	  bell(1);
	break;
      default:                  /* Others: return */
	return(c);
    }
  }
  /*NEVER REACHED*/

} /* getline() */
