/*
 *      EDIT.C
 *      UTREE simple line editor.
 *      3.01-um klin, Wed May  1 16:13:20 1991
 *      3.02-um klin, Sun Nov 24 15:28:27 1991, Video attributes changed
 *      3.03-um klin, Tue Feb 11 19:39:09 1992, Line editor changed
 *              klin, Sun Feb 23 17:15:13 1992, Key handling and key bindings
 *                                              changed
 *            b klin, Sun Mar 22 10:41:24 1992, Bug in editline() fixed
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#ifndef lint
static char sccsid[] = "@(#) utree 3.03b-um (klin) Mar 22 1992 edit.c";
#endif  /* !lint */

#include "defs.h"

/* ---- Local variables and definitions ------------------------------- */

LOCAL char *line;                       /* Line buffer                  */
LOCAL int pos, len, xpos;               /* Lines position and length    */
LOCAL int size, xmin, xmax, xdiff;      /* Window variables             */
LOCAL int omode = 1;                    /* Insert/overwrite mode flag   */

/* ---- Local/global functions and procedures ------------------------- */

/*
 *      INTERNAL USED ROUTINES
 */

/* Update/show line */
LOCAL VOID putline(from, to, clear)
  register int from, to, clear;
{
  register int i;

  for(i = from; line[i] && i < to && i <= xmax; i++)
    (void) putchar(line[i]);
  if(clear && i < xmax)
    clearline();
  (void) cursorxy(xpos + pos - xmin, echoline);

} /* putline() */

/* Scroll forward line */
LOCAL VOID scrollforw(to)
  register int to;
{
  xmin += xdiff;
  xmax += xdiff;
  if(to) {
    (void) cursorxy(xpos, echoline);
    putline(xmin, to, 1);
  }

} /* scrollforw() */

/* Scroll backward line */
LOCAL VOID scrollback(to)
  register int to;
{
  xmin -= xdiff;
  xmax -= xdiff;
  if(to) {
    (void) cursorxy(xpos, echoline);
    putline(xmin, to, 0);
  }

} /* scrollback() */

/* Move cursor one character forward */
LOCAL VOID charforw()
{
  if(xmax < len && pos == xmax)
    scrollforw(len);
  (void) putchar(line[pos++]);

} /* charforw() */

/* Move cursor one character backward */
LOCAL VOID charback()
{
  if(xmin > 0 && pos == xmin)
    scrollback(len);
  --pos;
  backspace();

} /* charback() */

/* Show insert/overwrite mode setting at end of helpline */
LOCAL VOID showmode()
{
  setvideo(DA_REVERSE);
  (void) putsxy(-11, helpline, omode ? "[OVERWRITE]" : "   [INSERT]");
  setvideo(DA_NORMAL);
  (void) cursorxy(xpos + pos - xmin, echoline);

} /* showmode() */

/*
 *      LINE EDITOR
*/

/* Read and edit a line */
GLOBL int editline(buf, max, cur, xm, xp)
  char *buf;
  register int max, cur;
  register int *xm, *xp;
{
  register int rv, maxc, mark, c, oc, i, j;

  /* Adjust to at least 32 columns window size */
  if((columns - cur) < 32) {
    xpos = columns - 32;
    (void) cursorxy(xpos-2, echoline);
    clearline();
    (void) putchar(':');
    (void) putchar(' ');
    flushout();
  }
  else
    xpos = cur;

  /* Calculate window sizes for scrolling */
  maxc = columns - xpos;
  size = max < maxc ? max : maxc;
  --max;
  --size;

  /* Calculate scrolling sizes */
  xdiff = size / 4;
  xmin = *xm;
  xmax = xmin + size;

  /* Setup input buffer variables */
  line = buf;
  pos  = *xp;
  len  = strlen(buf);
  mark = 0;
  rv   = IN_OK + 99;

  /* Set up mode string */
  showmode();

  /* Display input buffer if not empty */
  if(len > 0) {
    (void) cursorxy(xpos, echoline);
    putline(xmin, xmax, 1);
  }

  /* Line editor loop */
  do {
    switch(c = getkey()){
      case K_EOF:               /* EOF */
	line[0] = '\0';
	rv = IN_EOF;
	break;
      case K_BRK:               /* Interrupt */
	line[0] = '\0';
	rv = IN_INT;
	break;
      case K_SIZE:              /* Screen size changed */
	line[0] = '\0';
	rv = IN_SIZ;
	break;
      case K_HELP:              /* Help */
	line[len] = '\0';
	rv = IN_HLP;
	break;
      case K_PREV:              /* Previous */
	line[len] = '\0';
	rv = IN_PRV;
	break;
      case K_NEXT:              /* Next */
	line[len] = '\0';
	rv = IN_NXT;
	break;
      case K_SEL:               /* End edit and accept line */
	while(len > 0 && line[len-1] <= ' ')
	  line[--len] = '\0';
	if(line[0] == ' ') {
	  for(i = 0; line[i] && line[i] == ' '; i++)
	    --len;
	  for(j = 0; j <= len; j++, i++)
	    line[j] = line[i];
	}
	rv = len > 0 ? IN_OK : IN_NUL;
	break;
      case K_INS:               /* Switch insert/overwrite mode */
	omode = !omode;
	showmode();
	break;
      case K_CANC:              /* Cancel line */
	len = pos = 0;
	line[len] = '\0';
	xmin = 0;
	xmax = size;
	mark = 0;
      case K_REFR:              /* Refresh line */
	(void) cursorxy(xpos, echoline);
	putline(xmin, xmax, 1);
	break;
      case K_KILL:              /* Kill rest of line */
	line[len = pos] = '\0';
	clearline();
	mark = 0;
	break;
      case K_MARK:              /* Set mark */
	mark = pos;
	break;
      case K_GOTO:              /* Goto mark */
	i = mark;
	mark = pos;
	if(pos < i)
	  while(pos < len && pos < i)
	    charforw();
	else if(pos > i)
	  while(pos > 0 && pos > i)
	    charback();
	break;
      case K_TAG:               /* Transpose characters */
	if(pos > 0 && pos < len) {
	  oc = buf[pos];
	  buf[pos] = buf[pos-1];
	  buf[pos-1] = oc;
	  charback();
	  charforw();
	  charforw();
	}
	break;
      case K_FORW:              /* Cursor forward */
	if(pos < len)
	  charforw();
	break;
      case K_BACK:              /* Cursor backward */
	if(pos > 0)
	  charback();
	break;
      case K_HOME:              /* Cursor to beginning of line */
	mark = pos;
	if(pos > 0) {
	  pos = 0;
	  (void) cursorxy(xpos, echoline);
	  if(xmin > 0) {
	    xmin = 0;
	    xmax = size;
	    putline(pos, len, 1);
	  }
	}
	break;
      case K_END:               /* Cursor to end of line */
	mark = pos;
	if(pos < len) {
	  pos = len;
	  if(xmax > len)
	    (void) cursorxy(xpos + pos - xmin, echoline);
	  else {
	    do {
	      xmin += xdiff;
	      xmax += xdiff;
	    } while(xmax < len);
	    (void) cursorxy(xpos, echoline);
	    putline(xmin, len, 1);
	  }
	}
	break;
      case K_NPAG:              /* Scroll line forward */
	if(xmax < len) {
	  if((pos - xdiff) < (xmin + xdiff))
	    pos += xdiff;
	  scrollforw(len);
	}
	else
	  for(i = 0; pos < len && i < xdiff; i++)
	    charforw();
	break;
      case K_PPAG:              /* Scroll line backward */
	if(xmin > 0) {
	  if((pos + xdiff) > (xmax - xdiff))
	    pos -= xdiff;
	  scrollback(len);
	}
	else
	  for(i = 0; pos > 0 && i < xdiff; i++)
	    charback();
	break;
      case K_DEL:               /* Delete char left from cursor */
	if(pos <= 0)
	  break;
	--pos;
	if(xmin > 0 && pos == xmin)
	  scrollback(pos);
	else
	  backspace();
      case K_DOWN:              /* Delete character under cursor */
	if(pos == len || len == 0)
	  break;
	--len;
	for(i = pos; i < len; i++)
	  line[i] = line[i+1];
	line[len] = '\0';
	if(xmax < len || !deletechar())
	  putline(pos, len, 1);
	break;
      case K_UP:                /* Not handled symbols */
      case K_STR:
      case K_INV:
	bell(1);
	break;
      default:                  /* Insert character */
	if( !(len < max && isprint(c))) {
	  bell(1);
	  break;
	}
	if(pos == len || omode) {
	  if(pos == xmax)
	    scrollforw(len);
	  line[pos] = c;
	  (void) putchar(c);
	  if(pos == len)
	    buf[++len] = '\0';
	  ++pos;
	}
	else {
	  for(i = pos; i <= len; i++) {
	    oc = line[i];
	    line[i] = c;
	    c = oc;
	  }
	  line[++len] = '\0';
	  if(pos == xmax) {
	    scrollforw(len);
	    (void) putchar(line[pos++]);
	  }
	  else if(insertchar())
	    (void) putchar(line[pos++]);
	  else {
	    (void) putchar(line[pos++]);
	    putline(pos, len, 1);
	  }
	}
	break;
    }
  } while(rv > IN_OK);

  *xm = xmin;
  *xp = pos;
  return(rv);

} /* editline() */
