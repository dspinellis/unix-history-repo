/* widget.c - Provides the screen and widget code */

#ifndef lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/sd/RCS/widget.c,v 7.2 91/02/22 09:32:29 mrose Interim $";
#endif

/*
 * $Header: /f/osi/others/quipu/uips/sd/RCS/widget.c,v 7.2 91/02/22 09:32:29 mrose Interim $
 */


/*    This file has been modified by Damanjit Mahl @ Brunel University
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/*****************************************************************************/
/* File:	widget.c
/* Author:	Paul Sharpe @ GEC Research, Hirst Research Centre.
/* Date:	August 12, 1988.
/* Function:	Provides the screen and widget code.
/*
/* DISCLAIMER:
/* This source file is deemed to be public domain: any person may use the
/* code in any way, on two simple provisos -
/*   1 - That this header remains in this file,
/*   2 - It is accepted that neither the author, nor the authors employees
/*       accept any responsibility for the use, abuse or misuse of the
/*       contained code.
/* No guarantee is made by the author to provide maintainance or up-grading
/* of this source. However, any adaptations made to this file will be viewed
/* at least with interest.
/*****************************************************************************/

#include <strings.h>
#include <ctype.h>
#include <stdio.h>
#include <curses.h>
#include <setjmp.h>
#include <signal.h>

#include "widget.h"

extern char *filtvalue[];
extern unsigned int typeindx;
extern int *av_typeindx;

void get_listed_object(), scrollbar();

struct active {
  int		count;
  WIDGET	*widgets[MAXACTIVE];
  char	lastindex[MAXACTIVE];
  WINDOW	*text[MAXACTIVE];
} activelist;

char command;
WINDOW *Text;
int text_height;
jmp_buf	env;

void initwidgets()
{
  (void) initscr();
  (void) noecho();
  (void) crmode();
  Text = stdscr;
  typetoggled = 0;
  activelist.count = 0;
}

void textfresh()
{
  (void) wrefresh (Text);
}

void setwidgets(thesewdgts, y)
     int	y;
     WIDGET	*thesewdgts;
{
  currwidgets = thesewdgts;
  lowy = posnwidgets(thesewdgts, y);
  Text = newwin(LINES-1-lowy, COLS-3, lowy, 3);
  text_height = LINES-1-lowy;
  refresh();
  (void) scrollok(Text, FALSE);
  (void) wrefresh(Text);
  makewidgets(thesewdgts);
  activewidget(thesewdgts, Text);
  rfrshwidgets(thesewdgts);
}

int linec()
{
  return (int) LINES;
}

int gety()
{
  int y,x;
  getyx(Text, y, x);
  /* Get rid of lint warning */
  x = x + 1;
  return y + lowy;
}

/* Determine the positions of the widgets: return the lowest point */
int posnwidgets(thesewdgts, starty)
     int	starty;
     WIDGET	thesewdgts[];
{
  register int	cnt = 0, x = 0, hght = WDGTHGHT;

/* If no explicit position provided, put on the next level */
  if (starty < 0)
    starty = lowesty();

/* Set the position of the widgets, as dynamically as possible */

  while (thesewdgts[cnt].type != FINISH) {

    if (thesewdgts[cnt].type != DUMMY) {
/* If the widget is a scrollbar put on a new line */
      if (thesewdgts[cnt].type == SCROLLBAR) {
	starty += WDGTHGHT;
	hght = LINES - starty;
	x = 0;
      } else 

/* If the initial structure y-value is CRNL, put on a new line */
	if (thesewdgts[cnt].y == CRNL) {
	  starty += WDGTHGHT;
	  hght = WDGTHGHT;
	  x = 0;
	}

/* If we ain't got a width, make one based on the type/label-length */
      if (thesewdgts[cnt].wdth <= 0)
	setwdgtwdth(&thesewdgts[cnt], x);

/* If the widget won't fit, start a new line of widgets */
      if (x + thesewdgts[cnt].wdth > COLS) {
	starty += WDGTHGHT;
	x =  0;
      }
      thesewdgts[cnt].x = x;
      thesewdgts[cnt].y = starty;
      thesewdgts[cnt].hght = hght;
      x += thesewdgts[cnt].wdth;
      ++cnt;
    } else 
      ++cnt;
  }
  return(starty);
}

/* Create the widgets: return the number of widgets */
void makewidgets(wdgts)
     WIDGET	wdgts[];
{
  register int		cnt = 0;
  register WIDGET		*wdgt;

/* Now try to make, box and label the widget windows */
  while (wdgts[cnt].type != FINISH) {
    if (wdgts[cnt].type != DUMMY) {
      wdgt = &wdgts[cnt++];
      wdgt->wndw = newwin(wdgt->hght,wdgt->wdth,wdgt->y,wdgt->x);
      boxwdgt(wdgt, '-', '|');
      printwdgt(wdgt);
    } else ++cnt;
  }
}

/* Set a widgets width, based on the TYPE of widget and label length */
void setwdgtwdth(wdgt, currx)
     int	currx;
     WIDGET	*wdgt;
{
  char	expand;
  register int	len = -1, cnt = -1;

  if (expand = (wdgt->wdth == EXPAND))
    wdgt->wdth = COLS - currx;

    switch (wdgt->type) {
    case LABEL:
      len = strlen(wdgt->label) + 2;
      if (wdgt->wdth < len)
	if (expand)
	  wdgt->wdth = COLS;
	else
	  wdgt->wdth = len;
      break;

    case DIALOG:
      len = strlen(wdgt->label) + 2;
      if (wdgt->dstrlen > DIALOGLEN)
	len += DIALOGLEN;
      else
	len += wdgt->dstrlen;
      if (wdgt->wdth < len) {
	if (expand)
	  wdgt->wdth = COLS;
	else
	  wdgt->wdth = len;
      }
      break;
    case TOGGLE:
      if (wdgt->tvalues == (char **)NULL)
	wdgt->wdth = strlen(wdgt->label) + 2;
      else {
	while(wdgt->tvalues[++cnt] != (char *)NULL)
	  if (strlen(wdgt->tvalues[cnt]) > len)
	    len = strlen(wdgt->tvalues[cnt]);
	wdgt->wdth = strlen(wdgt->label) + len + 2;
      }
      break;
    default:	wdgt->wdth = strlen(wdgt->label) + 2;
      break;
    }
}

/* Erase and remove the widgets, decrementing the activelist counter */
void killwidgets(thesewdgts)
     WIDGET	*thesewdgts;
{
  register int	cnt = 0;
  
  while (thesewdgts[cnt].type != FINISH)
    if (thesewdgts[cnt].type != DUMMY) {
      (void) wclear(thesewdgts[cnt].wndw);
      (void) wrefresh(thesewdgts[cnt++].wndw);
    } else ++cnt;
  cnt = 0;
  (void) wclear(Text);
  (void) wrefresh(Text);
  while (thesewdgts[cnt].type != FINISH)
    if (thesewdgts[cnt].type != DUMMY)
      delwin(thesewdgts[cnt++].wndw);
    else ++cnt;
  delwin(Text);
  
  deleteactive();
  
  Text = activelist.text[activelist.count-1];
  if (Text != (WINDOW *)NULL)
    (void) wrefresh(Text);
}

/* THESE FUNCTIONS MANIPULATE THE ACTIVELIST ARRAY OF WIDGETS */

/* This should check that the number of active widgets is not excessive, or
 * have the activelist really as a linked list.
 */
/* ARGSUSED */
void activewidget(wdgts, text)
     WIDGET	wdgts[];
     WINDOW	*text;
{
  activelist.widgets[activelist.count] = wdgts;
  activelist.text[activelist.count]    = Text;
  ++(activelist.count);
}

void deleteactive()
{
  if (activelist.count > 0)
    --(activelist.count);
}

void activeindex(indx)
     char	indx;
{
  activelist.lastindex[activelist.count - 1] = indx;
}

/* Refresh each of the active widgets and the current text window */
void redraw()
{
  register int	i;

  clearok(curscr,TRUE);
  for (i=0; i<activelist.count; i++)
    rfrshwidgets(activelist.widgets[i]);

  (void) wrefresh(Text);
}

void rfrshwidgets(thesewdgts)
     WIDGET	*thesewdgts;
{
  int	i = 0;

  while(thesewdgts[i].wndw != (WINDOW *)NULL && thesewdgts[i].type != DUMMY){
    touchwin(thesewdgts[i].wndw);
    (void) wrefresh(thesewdgts[i++].wndw);
  }
}

/* Draw a perimeter box around WDGT, with horizontal char XCH etc */
void boxwdgt(wdgt, xch, ych)
     char	xch, ych;
     register WIDGET	*wdgt;
{
  register int x, y;

  mvwaddch(wdgt->wndw, 0, 0, '.');
  for (x = 1; x < wdgt->wdth-1; x++)
    (void) waddch(wdgt->wndw, xch);
  (void) waddch(wdgt->wndw, '.');
  
  mvwaddch(wdgt->wndw, 1, 0, ych);
  for (y = 1; y < wdgt->hght-1; y++) {
    (void) mvwaddch(wdgt->wndw, y, 0, ych);
    (void) mvwaddch(wdgt->wndw, y, wdgt->wdth-1, ych);
  }

  mvwaddch(wdgt->wndw, wdgt->hght-1, 0, '`');
  for (x = 1; x < wdgt->wdth-1; x++)
    (void) waddch(wdgt->wndw, xch);
  (void) waddch(wdgt->wndw, '\'');
}

/* THESE ROUTINES PRINT THE INDIVIDUAL WIDGET BOXES */

/* Print a widgets label, dependant on the widget type */
void printwdgt(wdgt)
     WIDGET	*wdgt;
{
  switch(wdgt->type) {

  case LABEL:
    printlabel(wdgt);
    break;

  case DIALOG:
    printdialog(wdgt);
    break;

  case TOGGLE:
    printtoggle(wdgt);
    break;
    
  default :
    printcommand(wdgt);
    break;
  }
}

void printbar(list_size, first, display_num)
     int list_size, first, display_num;
{
  WIDGET * wdgt;
  int cnt, bar_size, bar_pos=0, space_size;

  for(cnt = 0; currwidgets[cnt].type != SCROLLBAR; cnt++) ;
  wdgt = &currwidgets[cnt];
  
  (void) wclear(wdgt->wndw);
  boxwdgt(wdgt, '-', '|');
  
  space_size = wdgt->hght - 4;

  if(display_num == list_size) {
    bar_size = space_size;
    bar_pos = 1;
  } else {
    bar_size = (display_num*space_size)/(list_size+1);
    bar_size = bar_size? bar_size: 1;
    
    while(!((list_size*bar_pos)/(first*(space_size+1)))) bar_pos++;
  }
  
  while((bar_size + bar_pos - 1) > space_size) {
    bar_size--;
  }
  
  for(cnt = 0; cnt < bar_size; cnt++)
    (void) mvwaddch(wdgt->wndw, cnt+1+bar_pos, 1, '*');
  
  (void) mvwaddch(wdgt->wndw, 1, 1, ']');
  (void) mvwaddch(wdgt->wndw, wdgt->hght-2, 1, '[');
  
  (void) wrefresh(wdgt->wndw);
}

/* Print a LABEL widgets label string, dependant on the justification char */
void printlabel(wdgt)
     WIDGET	*wdgt;
{
  register int	x, labellen, wdgtlen;

  labellen = strlen(wdgt->label);
  wdgtlen = wdgt->wdth - 2;
  
  if (labellen > wdgtlen)
    wdgt->label[wdgtlen] = '\0';
  
  if (wdgt->callch & CENTRE)
    x = (wdgtlen - labellen)/2;
  else
    if (wdgt->callch & LEFT)
      x = 0;
    else
      if (wdgt->callch & RIGHT)	x = wdgtlen - labellen;

  mvwaddstr(wdgt->wndw,1,1+x,wdgt->label);
  (void) wrefresh(wdgt->wndw);
}

/* Print a DIALOG widget label: if it don't all fit, show the last part */
void printdialog(wdgt)
     WIDGET	*wdgt;
{
  register int	length, maxlen;
  register char	*showptr;

  (void) wclear(wdgt->wndw);
  boxwdgt(wdgt, '-', '|');
  if (wdgt->dstr != (char *)NULL) {
    length = strlen(wdgt->dstr);
    maxlen = wdgt->wdth - 4 - strlen(wdgt->label);
    if (length > maxlen)
      showptr = &(wdgt->dstr[length - maxlen]);
    else
      showptr = wdgt->dstr;
    (void) mvwprintw(wdgt->wndw, 1, 1, "%s%c%s",wdgt->label, 
		     (length > maxlen)?'<':' ',showptr);
  }
  (void) wrefresh(wdgt->wndw);
}

/* Print a TOGGLE widget label, and the current toggle value */
void printtoggle(wdgt)
     WIDGET	*wdgt;
{
  (void) wclear(wdgt->wndw);
  boxwdgt(wdgt, '-', '|');
  if (wdgt->tvalues == (char **)NULL)
    return;
  mvwaddstr(wdgt->wndw,1,1,wdgt->label);
  (void) waddstr(wdgt->wndw,wdgt->tvalues[wdgt->tindx]);
  (void) wclrtoeol(wdgt->wndw);
  (void) mvwaddch(wdgt->wndw,1,wdgt->wdth-1,'|');
  (void) wrefresh(wdgt->wndw);
}

/* Print a COMMAND widget label */
void printcommand(wdgt)
     WIDGET	*wdgt;
{
  mvwaddstr(wdgt->wndw,1,1,wdgt->label);
  (void) wrefresh(wdgt->wndw);
}

/* THESE ROUTINES GET AND REACT TO A USERS INPUT FROM THE KEYBOARD */

/* Loop forever, calling widget callback functions when activated */
void interact()
{
  register int	ch, indx;
  void		int_quit(), jumpback();

  for (;;) {
/* Get a character input, and set the interrupt jump vector */
    (void) setjmp(env);
    (void) signal(SIGINT, int_quit);

    move(0,0);
    (void) wrefresh(Text);
    refresh();
    ch = getchar();
    if (isupper(ch))
      ch = tolower(ch);
    (void) signal(SIGINT, jumpback);
    
/* Allow the user to refresh the entire screen, with a CTRL-L */
    if (ch == '\014') {
      redraw();
      scrollbar('\0');
      continue;
    }

/* Search through the current widgets for one matching that required */
    command = ch;
    indx = findactiveinput(ch);
    if (indx >= 0)
      docallback(indx);
    ch = 0;
    }
}

/* Find a callback 'ch' from the currently active set of widgets */
int findactiveinput(ch)
     char	ch;
{
  register int	indx;
  register WIDGET	*wdgts;

  if (ch > 'z' || ch < 'a') {
    switch (ch) {
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      ch = '*';
      break;
    case '[':
    case ']':
      ch = '%';
      break;
    case '?':
      ch = 'h';
      break;
    }
  }

/* See whether the 'ch' exists in the currently active widgets */
  wdgts = activelist.widgets[activelist.count - 1];
  indx = getwidgetindex(wdgts, ch);
  if (indx >= 0)
    return(indx);
  
/* If not, check the previously active widgets, if possible */
  if (activelist.count <= 1)
    return(-1);
  indx = getwidgetindex(activelist.widgets[activelist.count - 2], ch);
  if (indx >= 0) {
    killwidgets(activelist.widgets[activelist.count - 1]);
    return(indx);
  }
  return(-1);
}

void docallback(indx)
     char	indx;
{
  WIDGET	*wdgts;

  activeindex(indx);
  wdgts = activelist.widgets[activelist.count - 1];
  switch (wdgts[indx].type) {
  
  case DIALOG:
    if (command >= '1' && command <= '9')
      get_listed_object (command, &wdgts[indx]);
    else {
      dialog(&wdgts[indx]);
      (*wdgts[indx].callfn)();
    }
    break;

  case TOGGLE:
    toggle(&wdgts[indx]);
    (*wdgts[indx].callfn)();
    break;

  case SCROLLBAR :
    scrollbar(command);
    break;

  default:
    (*wdgts[indx].callfn)();
    break;
  }
}

/* THESE ROUTINES SEARCH THE ACTIVE WIDGET SET FOR ONE SPECIFIED WIDGET */

/* Find a widget based on the call-back character */
WIDGET *getwidget(wdgts, callch)
     int	callch;
     WIDGET	wdgts[];
{
  register int	indx;

  indx = getwidgetindex(wdgts, callch);
  if (indx >= 0) return(&(wdgts[indx]));

  return((WIDGET *)NULL);
}

int getwidgetindex(wdgts, callch)
     int	callch;
     WIDGET	wdgts[];
{
  register int	cnt = 0;

  while (wdgts[cnt].type != FINISH) {
    if (callch == wdgts[cnt].callch)  break;
    ++cnt;
  }
  if (wdgts[cnt].type != FINISH) return(cnt);

  return(-1);
}

/* THESE ROUTINES MANIPULATE THE DIALOG WIDGETS */

void dialog(wdgt)
     WIDGET	*wdgt;
{
  register int		i, length, labellen, maxlen;
  register char		ch, *endptr, *showptr;
  register char		*blanks;
  extern char		*malloc();
  
  labellen = strlen(wdgt->label);	/* The length of the prompt string  */
  length = strlen(wdgt->dstr);	/* The length of the current string */
  
  maxlen = wdgt->wdth - 4 - labellen;	/* The maximum length of shown str  */
  blanks = malloc((unsigned)(maxlen + 2));
  for (i=0; i<maxlen; i++)
    blanks[i] = ' ';
  blanks[i] = '\0';
  
  endptr = &(wdgt->dstr[length]);	/* The next character pos'n to fill */
  *endptr = '\0';
  
  if (length > maxlen)
    showptr = &(wdgt->dstr[length - maxlen]);
  else
    showptr = wdgt->dstr;
    (void) mvwprintw(wdgt->wndw, 1, 1, "%s%c%s", wdgt->label,
		     (length > maxlen)? '<' : ' ', 
		     showptr);
  (void) wrefresh(wdgt->wndw);
  
  while ((ch = getchar() & 127) != '\n' && ch != '\r' && ch != '\f') {
    if (ch == '\014') {			/* Allow for redrawing */
      redraw();
      continue;
    }

    if (ch == '\b' || ch == 127) {	/* Delete a character, with wrapping */
      if (length == 0)
	continue;
      *(--endptr) = '\0';		/* Make the last character NULL */
      if (showptr > wdgt->dstr)	/* We have parts of the string hidden */
	--showptr;
      --length;
      if (length < maxlen) {	/* Only need to erase one character */
	(void) waddstr(wdgt->wndw,"\b \b");
	(void) wrefresh(wdgt->wndw);
	continue;
      }
      /* We'll have to erase everything */
      (void) wprintw(wdgt->wndw, "\r|%s%c%s \b", wdgt->label,
		     (length <= maxlen)? ' ' : '<',
		     showptr);
      (void) wrefresh(wdgt->wndw);
      continue;
    }
    
    if (ch == 21) {		/* ^U to delete the entire line of text */
      length = 0;
      endptr = wdgt->dstr;
      *endptr = '\0';
      showptr = wdgt->dstr;
      (void) wprintw(wdgt->wndw, "\r|%s %s\r|%s ", wdgt->label, blanks,
		     wdgt->label);
      (void) wrefresh(wdgt->wndw);
      continue;
    }
    
/* Otherwise, add the character if there is room and it ain't a control code */
    if (length == 1024 || ch < 32)
      continue;
    if (length == wdgt->dstrlen){
      *endptr++ = ch;
      *endptr = '\0';
      setdialogstr(wdgt, showptr, wdgt->dstrlen);
      continue;
    }
    *endptr++ = ch;
    *endptr = '\0';
    if (++length <= maxlen) {	/* Just add this character to the end */
      (void) waddch(wdgt->wndw, ch);
      (void) wrefresh(wdgt->wndw);
      continue;
    }
    ++showptr;
    (void) wprintw(wdgt->wndw, "\r|%s<%s", wdgt->label, showptr);
    (void) wrefresh(wdgt->wndw);
  }
  free(blanks);
}


void setdialogstr(wdgt, dstr, maxlen)
     char	*dstr;
     WIDGET	*wdgt;
int maxlen ;
{
  if (wdgt->type != DIALOG) return;

  wdgt->dstr = dstr;
  wdgt->dstrlen = maxlen;
}

int getdialogstr(wdgt, str)		/* 'str' must be long enough... */
     char	str[];
     WIDGET	*wdgt;
{
  if (wdgt->type != DIALOG || wdgt->dstr == (char *)NULL) return(FALSE);

  (void) strcpy(str, wdgt->dstr);
  return(TRUE);
}

/* THESE ROUTINES MANIPULATE THE TOGGLE WIDGETS */

void toggle(wdgt)
     WIDGET	*wdgt;
{
  WIDGET *vwdgt;
  int av_indx;

  if (wdgt->tvalues == (char **)NULL)
    return;

  if (wdgt == getwidget(currwidgets, 't')) {
    typetoggled = 1;
    vwdgt = getwidget(currwidgets, 's');
    (void) strcpy(filtvalue[wdgt->tindx], vwdgt->dstr);

    av_indx = 0;
    while (av_typeindx[av_indx] != wdgt->tindx && av_typeindx[av_indx] >= 0) 
      av_indx++;

    if (av_typeindx[av_indx] == wdgt->tindx)
      av_indx++;
    
    if (av_typeindx[av_indx] < 0)
      av_indx = 0;

    wdgt->tindx = av_typeindx[av_indx];

    (void) strcpy(vwdgt->dstr, filtvalue[wdgt->tindx]);
    typeindx = wdgt->tindx;

    printdialog(vwdgt);
  }

  printtoggle(wdgt);
}

void settogglstrs(wdgt, togglstrs, togglindx)
     int	togglindx;
     char	**togglstrs;
     WIDGET	*wdgt;
{
  if (wdgt->type != TOGGLE)
    return;
  wdgt->tvalues = togglstrs;
  wdgt->tindx = togglindx;
}

int settogglindx(wdgt, indx)
     int	indx;
     WIDGET	*wdgt;
{
  int	i;
  
  if (wdgt->type != TOGGLE || wdgt->tvalues == (char **)NULL)
    return(FALSE);
  for (i=0; i<indx; i++)
    if (wdgt->tvalues[i] == (char *)NULL)
      break;
  if (i != indx)		/* There ain't that many toggle strings */
    return(FALSE);
  wdgt->tindx = indx;
  return(TRUE);
}

int gettogglindx(wdgt)
     WIDGET	*wdgt;
{
  if (wdgt->type != TOGGLE || wdgt->tvalues == (char **)NULL)
    return(-1);
  return(wdgt->tindx);
}

int gettogglstr(wdgt, str)		/* 'str' must be long enough... */
     WIDGET	*wdgt;
     char	str[];
{
  if (wdgt->type != TOGGLE || wdgt->tvalues == (char **)NULL)
    return(FALSE);
  (void) strcpy(str, wdgt->tvalues[wdgt->tindx]);
  return(TRUE);
}

/* THESE ROUTINES MANIPULATE THE LABEL WIDGETS */

void setlabel(wdgt, label)
     WIDGET	*wdgt;
     char	*label;
{
  wdgt->label = label;
}

void getlabel(wdgt, label)		/* 'label' must be long enough... */
     WIDGET	*wdgt;
     char	label[];
{
  (void) strcpy(label, wdgt->label);
}

/* MISCELLANEOUS FUNCTIONS */

/* Try to locate the bottom of the last set of widgets displayed */
int lowesty()
{
  register int		cnt = 0;
  register WIDGET		*wdgts;
  
  if (activelist.count <= 0)  return(0);

  wdgts = activelist.widgets[activelist.count - 1];
  while (wdgts[cnt].type != FINISH) ++cnt;

  if (cnt == 0) return(0);

  return((wdgts[cnt-1].y) + WDGTHGHT);
}

/* This satisfies the generalised printing structure */
/* ARGSUSED */
void wprint(here, fmt, a,b,c,d,e,f,g,h,i,j)
     WINDOW	*here;
     char	*fmt, *a,*b,*c,*d,*e,*f,*g,*h,*i,*j;
{
  (void) wprintw(Text,fmt,a,b,c,d,e,f,g,h,i,j);
  (void) wrefresh(Text);
}

/* This can be called as a way for an application to print text */
/* VARARGS1 */
void tprint(fmt, a,b,c,d,e,f,g,h,i,j)
     char	*fmt, *a,*b,*c,*d,*e,*f,*g,*h,*i,*j;
{
  (void) wprintw(Text,fmt,a,b,c,d,e,f,g,h,i,j);
  (void) wrefresh(Text);
}

void xprint(fmt)
     char *fmt;
{
  (void) wprintw(Text, "%s", fmt);
}

void xprintint(fmt, a)
     char *fmt;
     int a;
{
  (void) wprintw(Text,fmt, a);
}

void cleartext()
{
  clearok (Text,TRUE);
  (void) wclear (Text);
}

/* Jump back to the interact function only on an interrupt */
void jumpback()
{
  (void) waddstr(Text,"\n*** Interrupted ***\n");
  (void) wrefresh(Text);
  longjmp(env, TRUE);
}

/* This is used as a declaration, when no function callback is required */
void nullfn()
{}

/* This is used by widgets that just want to kill the current level */
void quitfn()
{
  (void) wclear(Text);
  (void) wrefresh(Text);
  killwidgets(activelist.widgets[activelist.count - 1]);
}

void endwidgets()
{
  move(LINES-1, 0);
  refresh();
  endwin();
}



