/*	SC	A Spreadsheet Calculator
 *		Main driver
 *
 *		original by James Gosling, September 1982
 *		modifications by Mark Weiser and Bruce Israel,
 *			University of Maryland
 *
 *              More mods Robert Bond, 12/86
 *		More mods by Alan Silverstein, 3-4/88, see list of changes.
 *		Currently supported by pur-phy!sawmill!buhrt (Jeff Buhrt)
 *		$Revision: 6.8 $
 *
 */


#include <signal.h>
#include <curses.h>
#include <ctype.h>

#ifdef BSD42
#include <strings.h>
#else
#ifndef SYSIII
#include <string.h>
#endif
#endif

#include <stdio.h>
#include "sc.h"

char *getenv();

#ifdef SYSV3
void exit();
#endif

#ifndef DFLT_PAGER
#define	DFLT_PAGER "more"	/* more is probably more widespread than less */
#endif /* DFLT_PAGER */

#define MAXCMD 160	/* for ! command below */

extern	char	*rev;

/* Globals defined in sc.h */

struct ent ***tbl;
int strow, stcol;
int currow, curcol;
int savedrow, savedcol;
int FullUpdate;
int maxrow, maxcol;
int maxrows, maxcols;
int *fwidth;
int *precision;
char *col_hidden;
char *row_hidden;
char line[FBUFLEN];
int changed;
struct ent *to_fix;
int modflg;
int numeric;
char *mdir;
int showsc, showsr;	/* Starting cell for highlighted range */
char mode_ind = '.';

char curfile[PATHLEN];
char    revmsg[80];

int  linelim = -1;

int  showtop   = 1;	/* Causes current cell value display in top line  */
int  showcell  = 1;	/* Causes current cell to be highlighted	  */
int  showrange = 0;	/* Causes ranges to be highlighted		  */
int  showneed  = 0;	/* Causes cells needing values to be highlighted  */
int  showexpr  = 0;	/* Causes cell exprs to be displayed, highlighted */

int  autocalc = 1 ;	/* 1 to calculate after each update */
int  calc_order = BYROWS;
int  tbl_style = 0;	/* headers for T command output */

int  lastmx, lastmy;	/* Screen address of the cursor */
int  lastcol;		/* Spreadsheet Column the cursor was in last */
char under_cursor[] = " "; /* Data under the < cursor */

#ifdef VMS
int VMS_read_raw = 0;
#endif

int seenerr;

void
yyerror(err)
char *err; {
    if (seenerr) return;
    seenerr++;
    (void) move(1,0);
    (void) clrtoeol();
    (void) printw("%s: %.*s<=%s",err,linelim,line,line+linelim);
}

struct ent *
lookat(row,col)
int	row, col;
{
    register struct ent **pp;

    checkbounds(&row, &col);
    pp = ATBL(tbl, row, col);
    if (*pp == (struct ent *)0) {
	*pp = (struct ent *) xmalloc((unsigned)sizeof(struct ent));
	if (row>maxrow) maxrow = row;
	if (col>maxcol) maxcol = col;
	(*pp)->label = (char *)0;
	(*pp)->row = row;
	(*pp)->col = col;
	(*pp)->flags = 0;
	(*pp)->expr = (struct enode *)0;
	(*pp)->v = (double) 0.0;
	(*pp)->evnext = (struct ent *)0;
    }
    return *pp;
}

/*
 * This structure is used to keep ent structs around before they
 * are deleted to allow the sync_refs routine a chance to fix the
 * variable references.
 * We also use it as a last-deleted buffer for the 'p' command.
 */

void
free_ent(p)
register struct ent *p;
{
    p->next = to_fix;
    to_fix = p;
    p->flags |= is_deleted;
}

void
flush_saved()
{
    register struct ent *p;
    register struct ent *q;

    if (!(p = to_fix))
	return;
    while (p) {
	(void) clearent(p);
	q = p->next;
	xfree((char *)p);
	p = q;
    }
    to_fix = 0;
}

/*
 * standout last time in update()?
 *	At this point we will let curses do work
 */
int	standlast	= FALSE;

void
update (anychanged)
int	anychanged;	/* did any cell really change in value? */
{
    register    row,
                col;
    register struct ent **pp;
    int     mxcol;
    int     mxrow;
    int     rows;
    int     cols;
    int     minsr, minsc, maxsr, maxsc;
    register r;
    register i;

    while (row_hidden[currow])   /* You can't hide the last row or col */
	currow++;
    while (col_hidden[curcol])
	curcol++;
    /* First see if the last display still covers curcol */
    if (stcol <= curcol) { 
	for (i = stcol, cols = 0, col = RESCOL;
			(col + fwidth[i]) < COLS-1 && i < maxcols; i++) {
	    cols++;
	    if (col_hidden[i])
		continue;
	    col += fwidth[i];
	}
    }
    while (stcol + cols - 1 < curcol || curcol < stcol) {
	FullUpdate++;
	if (stcol - 1 == curcol) {    /* How about back one? */
	    stcol--;
	} else if (stcol + cols == curcol) {   /* Forward one? */
	    stcol++;
	} else {
	    /* Try to put the cursor in the center of the screen */
	    col = (COLS - RESCOL - fwidth[curcol]) / 2 + RESCOL; 
	    stcol = curcol;
	    for (i=curcol-1; i >= 0 && col-fwidth[i] > RESCOL; i--) {
		stcol--;
		if (col_hidden[i])
		    continue;
		col -= fwidth[i];
	    }
	}
	/* Now pick up the counts again */
	for (i = stcol, cols = 0, col = RESCOL;
			(col + fwidth[i]) < COLS-1 && i < maxcols; i++) {
	    cols++;
	    if (col_hidden[i])
		continue;
	    col += fwidth[i];
	}
    }
    /* Now - same process on the rows */
    if (strow <= currow) { 
	for (i = strow, rows = 0, row=RESROW; row<LINES && i<maxrows; i++) {
	    rows++;
	    if (row_hidden[i])
		continue;
	    row++;
	}
    }
    while (strow + rows - 1 < currow || currow < strow) {
	FullUpdate++;
	if (strow - 1 == currow) {    /* How about up one? */
	    strow--;
	} else if (strow + rows == currow) {   /* Down one? */
	    strow++;
	} else {
	    /* Try to put the cursor in the center of the screen */
	    row = (LINES - RESROW) / 2 + RESROW; 
	    strow = currow;
	    for (i=currow-1; i >= 0 && row-1 > RESROW; i--) {
		strow--;
		if (row_hidden[i])
		    continue;
		row--;
	    }
	}
	/* Now pick up the counts again */
	for (i = strow, rows = 0, row=RESROW; row<LINES && i<maxrows; i++) {
	    rows++;
	    if (row_hidden[i])
		continue;
	    row++;
	}
    }
    mxcol = stcol + cols - 1;
    mxrow = strow + rows - 1;
    if (FullUpdate || standlast) {
	(void) move(2, 0);
	(void) clrtobot();
	(void) standout();
	for (row=RESROW, i=strow; i <= mxrow; i++) {
	    if (row_hidden[i]) 
		continue;
	    (void) move(row,0);
	    if (maxrows < 1000)
		(void) printw("%-*d", RESCOL-1, i);
	    else
		(void) printw("%-*d", RESCOL, i);
	    row++;
	}
	(void) move(2,0);
	(void) printw("%*s", RESCOL, " ");

	for (col=RESCOL, i = stcol; i <= mxcol; i++) {
	    register int k;
	    if (col_hidden[i])
		continue;
	    (void) move(2, col);
	    k = fwidth[i]/2;
	    if (k == 0)
		(void) printw("%1s", coltoa(i));
	    else
	        (void) printw("%*s%-*s", k, " ", fwidth[i]-k, coltoa(i));
	    col += fwidth[i];
	}
	(void) standend();
    }

    /* Get rid of cursor standout on the cell at previous cursor position */
    if (showcell)
    {	(void) move(lastmx, lastmy);
        repaint(lastmx, lastmy, fwidth[lastcol]);
    }

    if (showrange) {
	minsr = showsr < currow ? showsr : currow;
	minsc = showsc < curcol ? showsc : curcol;
	maxsr = showsr > currow ? showsr : currow;
	maxsc = showsc > curcol ? showsc : curcol;

	if (showtop) {
	    (void) move(1,0);
	    (void) clrtoeol();
	    (void) printw("Default range:  %s",
			    r_name(minsr, minsc, maxsr, maxsc));
	}
    }

    /* Repaint the visible screen */
    if (showrange || anychanged || FullUpdate || standlast)
    {
	/* may be reset in loop, if not next time we will do a FullUpdate */
      if (standlast)
      {	FullUpdate = TRUE;
	standlast = FALSE;
      }
      for (row = strow, r = RESROW; row <= mxrow; row++) {
	register c = RESCOL;
	int do_stand = 0;
	int fieldlen;
	int nextcol;

	if (row_hidden[row])
	    continue;
	for (pp = ATBL(tbl, row, col = stcol); col <= mxcol;
	         pp += nextcol - col,  col = nextcol, c += fieldlen) {

	    nextcol = col+1;
	    if (col_hidden[col]) {
		fieldlen = 0;
		continue;
	    }

	    fieldlen = fwidth[col];

	    /*
	     * Set standout if:
	     *
	     * - showing ranges, and not showing cells which need to be filled
	     *	 in, and not showing cell expressions, and in a range, OR
	     *
	     * - if showing cells which need to be filled in and this one is
	     *	 of that type (has a value and doesn't have an expression,
	     *	 or it is a string expression), OR
	     *
	     * - if showing cells which have expressions and this one does.
	     */

	    if ((showrange && (! showneed) && (! showexpr)
			   && (row >= minsr) && (row <= maxsr)
			   && (col >= minsc) && (col <= maxsc))
		    || (showneed && (*pp) && ((*pp) -> flags & is_valid)
			&& (((*pp) -> flags & is_strexpr) || !((*pp) -> expr)))
		    || (showexpr && (*pp) && ((*pp) -> expr)))
	    {
		(void) move(r, c);
		(void) standout();
		standlast++;
		if (!*pp)	/* no cell, but standing out */
		{	(void) printw("%*s", fwidth[col], " ");
			(void) standend();
			continue;
		}
		else
			do_stand = 1;
	    }
	    else
		do_stand = 0;

	    if ((*pp) && ((*pp) -> flags & is_changed || FullUpdate) || do_stand) {
		if (do_stand) {
		    (*pp) -> flags |= is_changed; 
		} else {
		    (void) move(r, c);
		    (*pp) -> flags &= ~is_changed;
		}

		/*
		 * Show expression; takes priority over other displays:
		 */

		if (showexpr && ((*pp) -> expr)) {
		    linelim = 0;
		    editexp(row, col);		/* set line to expr */
		    linelim = -1;
		    showstring(line, /* leftflush = */ 1, /* hasvalue = */ 0,
				row, col, & nextcol, mxcol, & fieldlen, r, c);
		}
		else {

		    /*
		     * Show cell's numeric value:
		     */

		    if ((*pp) -> flags & is_valid) {
			char field[FBUFLEN];
			(void)sprintf(field,"%*.*f", fwidth[col], precision[col], (*pp)->v);
			if(strlen(field) > fwidth[col]) {
			    for(i = 0; i<fwidth[col]; i++)
				(void)addch('*');
			} else {
			    (void)addstr(field);
			}
		    }

		    /*
		     * Show cell's label string:
		     */

		    if ((*pp) -> label) {
			showstring((*pp) -> label,
				    (*pp) -> flags & is_leftflush,
				    (*pp) -> flags & is_valid,
				    row, col, & nextcol, mxcol,
				    & fieldlen, r, c);
		    }
		    else	/* repaint a blank cell: */
		    if ((do_stand || !FullUpdate) &&
				((*pp)->flags & is_changed) &&
				!((*pp)->flags & is_valid) && !(*pp)->label) {
			(void) printw("%*s", fwidth[col], " ");
		    }
		} /* else */

		if (do_stand) {
		    (void) standend();
		    do_stand = 0;
		}
	    }
	}
	r++;
      }
    }
	    
    (void) move(lastmy, lastmx+fwidth[lastcol]);
    if((inch() & A_CHARTEXT ) == '<')
        (void) addstr(under_cursor);

    lastmy =  RESROW;
    for (row = strow; row < currow; row++)
	if (!row_hidden[row])
		lastmy += 1;
    lastmx = RESCOL;
    for (col = stcol; col < curcol; col++)
	if (!col_hidden[col])
		lastmx += fwidth[col];
    lastcol = curcol;
    if (showcell && (! showneed) && (! showexpr)) {
	(void) move(lastmy, lastmx);
        (void) standout();
        repaint(lastmx, lastmy, fwidth[lastcol]);
        (void) standend();
    }
    (void) move(lastmy, lastmx+fwidth[lastcol]);
    *under_cursor = (inch() & A_CHARTEXT );
    (void) addstr("<");

    (void) move(0, 0);
    (void) clrtoeol();
    if (linelim >= 0) {
	(void) addch(mode_ind);
	(void) addstr("> ");
	(void) addstr(line);
	(void) move(0, linelim+3);
    } else {
	if (showtop) {			/* show top line */
	    register struct ent *p1;
	    int printed = 0;		/* printed something? */

            (void) printw("%s%d ", coltoa(curcol), currow);

	    if (p1 = *ATBL(tbl, currow, curcol)) {
		if (p1 -> expr) {
		    /* has expr of some type */
		    linelim = 0;
		    editexp(currow, curcol);	/* set line to expr */
		    linelim = -1;
		}

		/*
		 * Display string part of cell:
		 */

		if ((p1 -> expr) && (p1 -> flags & is_strexpr)) {
		    (void) addstr((p1 -> flags & is_leftflush) ? "<{" : ">{");
		    (void) addstr(line);
		    (void) addstr("} ");	/* and this '}' is for vi % */
		    printed = 1;

		} else if (p1 -> label) {
		    /* has constant label only */
		    (void) addstr ((p1 -> flags & is_leftflush) ? "<\"" : ">\"");
		    (void) addstr (p1 -> label);
		    (void) addstr ("\" ");
		    printed = 1;
		}

		/*
		 * Display value part of cell:
		 */

		if (p1 -> flags & is_valid) {
		    /* has value or num expr */
		    if ((! (p1 -> expr)) || (p1 -> flags & is_strexpr))
			(void) sprintf (line, "%.15g", p1 -> v);

		    (void) addstr ("[");
		    (void) addstr (line);
		    (void) addstr ("]");
		    printed = 1;
		}
	    }
	    if (! printed)
		(void) addstr ("[]");
	}
	(void) move (lastmy, lastmx + fwidth[lastcol]);
    }
    if (revmsg[0]) {
	(void) move(0, 0);
	(void) clrtoeol ();	/* get rid of topline display */
	(void) printw(revmsg);
	revmsg[0] = '\0';	/* don't show it again */
	(void) move (lastmy, lastmx + fwidth[lastcol]);
    }
    FullUpdate = FALSE;
}

void
repaint(x, y, len)
int x, y, len;
{
    int c;

    while(len-- > 0) {
	(void) move(y,x);
	c = inch() & A_CHARTEXT;
	(void) addch(c);
	x++;
    }
}

char    *progname;

int
main (argc, argv)
int argc;
char  **argv;
{
    int     inloop = 1;
    register int   c;
    int     edistate = -1;
    int     arg = 1;
    int     narg;
    int     nedistate;
    int	    running;
    char    *revi;
    int	    anychanged = FALSE;

    /*
     * Keep command line options around until the file is read so the
     * command line overrides file options
     */

    int Mopt = 0;
    int Nopt = 0;
    int Copt = 0; 
    int Ropt = 0;

    int tempx, tempy; 	/* Temp versions of curx, cury */

    if ((revi = strrchr(argv[0], '/')) != NULL)
	progname = revi+1;
    else
	progname = argv[0];

    while (argc > 1 && argv[1][0] == '-') {
	argv++;
	argc--;
    	switch (argv[0][1]) {
	    case 'x':
#ifdef VMS
		    (void) fprintf(stderr, "Crypt not available for VMS\n");
		    exit(1);
#else 
		    Crypt = 1;
#endif
		    break;
	    case 'm':
		    Mopt = 1;
		    break;
	    case 'n':
		    Nopt = 1;
		    break;
	    case 'c':
		    Copt = 1;
		    break;
	    case 'r':
		    Ropt = 1;
		    break;
	    default:
		    (void) fprintf(stderr,"%s: unrecognized option: \"%c\"\n",
			progname,argv[0][1]);
		    exit(1);
	}
    }

    *curfile ='\0';

    signals();
    (void) initscr();

	/* setup the spreadsheet arrays, initscr() will get the screen size */
    if (!growtbl(GROWNEW, 0, 0))
    {	endwin();
	exit(1);
    }

    (void) clear();
#ifdef VMS
    VMS_read_raw = 1;
#else
    nonl();
    noecho ();
    cbreak();
#endif
    initkbd();
    scrollok(stdscr, 1);

    /*
     * Build revision message for later use:
     */

    (void) strcpy (revmsg, progname);
    for (revi = rev; (*revi++) != ':'; );	/* copy after colon */
    (void) strcat (revmsg, revi);
    revmsg [strlen (revmsg) - 2] = 0;		/* erase last character */
    (void) strcat (revmsg, ":  Type '?' for help.");

    if (argc > 1) {
	(void) strcpy(curfile,argv[1]);
	readfile (argv[1], 0);
    }

    if (Mopt)
	autocalc = 0;
    if (Nopt)
	numeric = 1;
    if (Copt)
	calc_order = BYCOLS;
    if (Ropt)
	calc_order = BYROWS;

    modflg = 0;
#ifdef VENIX
    setbuf (stdin, NULL);
#endif
    FullUpdate++;
    while (inloop) { running = 1;
    while (running) {
	nedistate = -1;
	narg = 1;
	if (edistate < 0 && linelim < 0 && autocalc && (changed || FullUpdate))
	{    EvalAll ();
	     if (changed)		/* if EvalAll changed or was before */
		anychanged = TRUE;
	     changed = 0;
	}
	else		/* any cells change? */
	if (changed)
	     anychanged = TRUE;

	update(anychanged);
	anychanged = FALSE;
#ifndef SYSV3
	(void) refresh(); /* 5.3 does a refresh in getch */ 
#endif
	c = nmgetch();
	getyx(stdscr, tempy, tempx);
	(void) move (1, 0);
	(void) clrtoeol ();
	(void) move(tempy, tempx);
	(void) fflush (stdout);
	seenerr = 0;
	showneed = 0;	/* reset after each update */
	showexpr = 0;

	/* if ((c < ' ') || ( c == DEL ))   how about international here ? PB */
	   if ( iscntrl(c) )
	    switch (c) {
#ifdef SIGTSTP
		case ctl('z'):
		    (void) deraw();
		    (void) kill(0, SIGTSTP); /* Nail process group */

		    /* the pc stops here */

		    (void) goraw();
		    break;
#endif
		case ctl('r'):
		    showneed = 1;
		case ctl('l'):
		    FullUpdate++;
		    (void) clearok(stdscr,1);
		    break;
		case ctl('x'):
		    FullUpdate++;
		    showexpr = 1;
		    (void) clearok(stdscr,1);
		    break;
		default:
		    error ("No such command (^%c)", c + 0100);
		    break;
		case ctl('b'):
		    backcol(arg);
		    break;
		case ctl('c'):
		    running = 0;
		    break;

		case ctl('e'):

		    switch (nmgetch()) {
		    case ctl('p'): case 'k':	doend (-1, 0);	break;
		    case ctl('n'): case 'j':	doend ( 1, 0);	break;
		    case ctl('b'): case 'h':
		    case ctl('h'):		doend ( 0,-1);	break;
		    case ctl('f'): case 'l':
		    case ctl('i'): case ' ':	doend ( 0, 1);	break;

		    case ESC:
		    case ctl('g'):
			break;

		    default:
			error("Invalid ^E command");
			break;
		    }

		    break;

		case ctl('f'):
		    forwcol(arg);
		    break;

		case ctl('g'):
		    showrange = 0;
		    linelim = -1;
		    (void) move (1, 0);
		    (void) clrtoeol ();
		    break;

		case ESC:	/* ctl('[') */
		    write_line(ESC);
		    break;

		case ctl('d'):
		    write_line(ctl('d'));
		    break;

		case DEL:
		case ctl('h'):
		    if (linelim < 0) {	/* not editing line */
			backcol(arg);	/* treat like ^B    */
			break;
		    }
		    write_line(ctl('h'));
		    break;

		case ctl('i'): 		/* tab */
		    if (linelim < 0) {	/* not editing line */
			forwcol(arg);
			break;
		    }
		    if (!showrange) {
			startshow();
		    } else {
			showdr();
			linelim = strlen(line);
			line[linelim++] = ' ';
			line[linelim] = 0;
			showrange = 0;
		    }
		    linelim = strlen (line);
		    break;

		case ctl('m'):
		case ctl('j'):
		    write_line(ctl('m'));
		    break;

		case ctl('n'):
		    forwrow(arg);
		    break;

		case ctl('p'):
		    backrow(arg);
		    break;

		case ctl('q'):
		    break;	/* ignore flow control */

		case ctl('s'):
		    break;	/* ignore flow control */

		case ctl('t'):
		    error(
"Toggle:  a:auto  c:cell  e:ext funcs  n:numeric  t:top  x:encrypt  $:pre-scale");
		    (void) refresh();

		    switch (nmgetch()) {
			case 'a': case 'A':
			case 'm': case 'M':
			    autocalc ^= 1;
			    error("Automatic recalculation %sabled.",
				autocalc ? "en":"dis");
			    break;
			case 'n': case 'N':
			    numeric = (! numeric);
			    error ("Numeric input %sabled.",
				    numeric ? "en" : "dis");
			    break;
			case 't': case 'T':
			    showtop = (! showtop);
			    repaint(lastmx, lastmy, fwidth[lastcol]);
			    error ("Top line %sabled.", showtop ? "en" : "dis");
			    break;
			case 'c': case 'C':
			    showcell = (! showcell);
			    repaint(lastmx, lastmy, fwidth[lastcol]);
			    error ("Cell highlighting %sabled.",
				    showcell ? "en" : "dis");
			    break;
			case 'x': case 'X':
			    Crypt = (! Crypt);
			    error ("Encryption %sabled.", Crypt? "en" : "dis");
			    break;
			case '$':
			    if (prescale == 1.0) {
				error ("Prescale enabled.");
				prescale = 0.01;
			    } else {
				prescale = 1.0;
				error ("Prescale disabled.");
			    }
			    break;
			case 'e': case 'E':
			    extfunc = (! extfunc);
			    error ("External functions %sabled.",
				    extfunc? "en" : "dis");
			    break;
			case ESC:
			case ctl('g'):
			    --modflg;	/* negate the modflg++ */
			    break;
			default:
			    error ("Invalid toggle command");
			    --modflg;	/* negate the modflg++ */
		    }
		    FullUpdate++;
		    modflg++;
		    break;

		case ctl('u'):
		    narg = arg * 4;
		    nedistate = 1;
		    break;

		case ctl('v'):	/* insert variable name */
		    if (linelim > 0)
		        ins_string(v_name(currow, curcol));
		    break;

		case ctl('w'):	/* insert variable expression */
		    if (linelim > 0)  {
			char *temp, *temp1;
			int templim;

			temp = strcpy(xmalloc((unsigned)(strlen(line)+1)),line);
			templim = linelim;
			editexp(currow,curcol);
			temp1= strcpy(xmalloc((unsigned)(strlen(line)+1)),line);
			strcpy(line, temp);
			linelim = templim;
			ins_string(temp1);
			xfree(temp);
			xfree(temp1);
		    }
		    break;

		case ctl('a'):	/* insert variable value */
		    if (linelim > 0) {
			struct ent *p = *ATBL(tbl, currow, curcol);
			char temp[100];

			if (p && p -> flags & is_valid) {
			    (void) sprintf (temp, "%.*f",
					precision[curcol],p -> v);
			    ins_string(temp);
			}
		    }
		    break;

	    } /* End of the control char switch stmt */
	else if (isdigit(c) && ((numeric && edistate >= 0) ||
			(!numeric && (linelim < 0 || edistate >= 0)))) {
	    /* we got a leading number */
	    if (edistate != 0) {
		/* First char of the count */
		if (c == '0')      /* just a '0' goes to left col */
		    curcol = 0;
		else {
		    nedistate = 0;
		    narg = c - '0';
		}
	    } else {
		/* Succeeding count chars */
		nedistate = 0;
		narg = arg * 10 + (c - '0');
	    }
	} else if (linelim >= 0) {
	    /* Editing line */
	    switch(c) {
	    case ')':
		if (showrange) {
		    showdr();
		    showrange = 0;
		    linelim = strlen (line);
		}
		break;
	    default:
		break;
	    }
	    write_line(c);

	} else if (!numeric && ( c == '+' || c == '-' ) ) {
	    /* increment/decrement ops */
	    register struct ent *p = *ATBL(tbl, currow, curcol);
	    if (!p)
		continue;
	    if (p->expr && !(p->flags & is_strexpr)) {
		error("Can't increment/decrement a formula\n");
		continue;
	    }
	    FullUpdate++;
	    modflg++;
	    if( c == '+' )
	    	p -> v += (double) arg;
	    else
		p -> v -= (double) arg;
	} else
	    /* switch on a normal command character */
	    switch (c) {
		case ':':
		    break;	/* Be nice to vi users */

		case '@':
		    EvalAll ();
		    changed = 0;
		    anychanged = TRUE;
		    break;

		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
		case '-': case '.': case '+':
		    (void) sprintf(line,"let %s = %c",
				v_name(currow, curcol), c);
		    linelim = strlen (line);
		    insert_mode();
		    break;

		case '=':
		    (void) sprintf(line,"let %s = ",
					v_name(currow, curcol));
		    linelim = strlen (line);
		    insert_mode();
		    break;

		case '!':
		    {
		    /*
		     *  "! command"  executes command
		     *  "!"	forks a shell
		     *  "!!" repeats last command
		     */
#ifdef VMS
		    error("Not implemented on VMS");
#else /* VMS */
		    char *shl;
		    int pid, temp;
		    char cmd[MAXCMD];
		    static char lastcmd[MAXCMD];

		    if (!(shl = getenv("SHELL")))
			shl = "/bin/sh";

		    deraw();
		    (void) fputs("! ", stdout);
		    (void) fflush(stdout);
		    (void) fgets(cmd, MAXCMD, stdin);
		    cmd[strlen(cmd) - 1] = '\0';	/* clobber \n */
		    if(strcmp(cmd,"!") == 0)		/* repeat? */
			    (void) strcpy(cmd, lastcmd);
		    else
			    (void) strcpy(lastcmd, cmd);

		    if (modflg)
		    {
			(void) puts ("[No write since last change]");
			(void) fflush (stdout);
		    }

		    if (!(pid = fork()))
		    {
			(void) signal (SIGINT, SIG_DFL);  /* reset */
			if(strlen(cmd))
				(void)execl(shl,shl,"-c",cmd,(char *)0);
			else
				(void) execl(shl, shl, (char *)0);
			exit(-127);
		    }

		    while (pid != wait(&temp));

		    (void) printf("Press RETURN to continue ");
		    (void)nmgetch();
		    goraw();
#endif /* VMS */
		    break;
		    }

		/*
		 * Range commands:
		 */

		case '/':
		    error (
"Range:  x:erase  v:value  c:copy  f:fill  d:define  s:show  u:undefine");
		    (void) refresh();

		    switch (nmgetch()) {
		    case 'c':
			(void) sprintf(line,"copy [dest_range src_range] ");
			linelim = strlen(line);
			startshow();
			insert_mode();
			break;
		    case 'x':
			(void) sprintf(line,"erase [range] ");
			linelim = strlen(line);
			startshow();
			insert_mode();
			break;
		    case 'v':
			(void) sprintf(line, "value [range] ");
			linelim = strlen(line);
			startshow();
			insert_mode();
			break;
		    case 'f':
			(void) sprintf(line,"fill [range start inc] ");
			linelim = strlen(line);
			startshow();
			insert_mode();
			break;
		    case 'd':
			(void) sprintf(line,"define [string range] \"");
			linelim = strlen(line);
			startshow();
			insert_mode();
			modflg++;
			break;
		    case 'u':
			(void) sprintf(line,"undefine [range] ");
			linelim = strlen(line);
			insert_mode();
			modflg++;
			break;
		    case 's':
			if(are_ranges())
			{
			FILE *f;
			int pid;
			char px[MAXCMD] ;
			char *pager;

			(void) strcpy(px, "| sort | ");
			if(!(pager = getenv("PAGER")))
				pager = DFLT_PAGER;
			(void) strcat(px,pager);
			f = openout(px, &pid);
			if (!f) {
			    error("Can't open pipe to sort");
			    break;
			}
			list_range(f);
			closeout(f, pid);
			}
			else error("No ranges defined");
			break;
			
		    case ESC:
		    case ctl('g'):
			break;
		   default:
			error("Invalid region command");
			break;
		   }
		   break;

		/*
		 * Row/column commands:
		 */

		case 'i':
		case 'a':
		case 'd':
		case 'p':
		case 'v':
		case 'z':
		case 's':
		    {
			register rcqual;

			if (! (rcqual = get_rcqual (c))) {
			    error ("Invalid row/column command");
			    break;
			}

			error ("");	/* clear line */

			if ( rcqual == ESC || rcqual == ctl('g'))
			    break;

			switch (c) {

			case 'i':
			    if (rcqual == 'r')	insertrow(arg);
			    else		opencol(curcol, arg);
			    break;

			case 'a':
			    if (rcqual == 'r')	while (arg--) duprow();
			    else		while (arg--) dupcol();
			    break;

			case 'd':
			    if (rcqual == 'r')	deleterow(arg);
			    else		closecol(curcol, arg);
			    break;

			case 'p':
			    while (arg--)	pullcells(rcqual);
			    break;

			case 'v':
			    if (rcqual == 'r')	rowvalueize(arg);
			    else		colvalueize(arg);
			    modflg = 1;
			    break;

			case 'z':
			    if (rcqual == 'r')	hiderow(arg);
			    else		hidecol(arg);
			    break;

			case 's':
			    /* special case; no repeat count */

			    if (rcqual == 'r')	rowshow_op();
			    else		colshow_op();
			    break;
			}
			break;
		    }

		case '$':
		    {
		    register struct ent *p;

		    curcol = maxcols - 1;
		    while (!VALID_CELL(p, currow, curcol) && curcol > 0)
			curcol--;
		    break;
		    }
		case '#':
		    {
		    register struct ent *p;

		    currow = maxrows - 1;
		    while (!VALID_CELL(p, currow, curcol) && currow > 0)
			currow--;
		    break;
		    }
		case 'w':
		    {
		    register struct ent *p;

		    while (--arg>=0) {
			do {
			    if (curcol < maxcols - 1)
				curcol++;
			    else {
				if (currow < maxrows - 1) {
				    while(++currow < maxrows - 1 &&
					    row_hidden[currow]) /* */;
				    curcol = 0;
				} else {
				    error("At end of table");
				    break;
				}
			    }
			} while(col_hidden[curcol] ||
				!VALID_CELL(p, currow, curcol));
		    }
		    break;
		    }
		case 'b':
		    {
		    register struct ent *p;

		    while (--arg>=0) {
			do {
			    if (curcol) 
				curcol--;
			    else {
				if (currow) {
				    while(--currow &&
					row_hidden[currow]) /* */;
				    curcol = maxcols - 1;
				} else {
				    error ("At start of table");
				    break;
				}
			    }
			} while(col_hidden[curcol] ||
				!VALID_CELL(p, currow, curcol));
		    }
		    break;
		    }
		case '^':
		    currow = 0;
		    break;
		case '?':
		    help();
		    break;
		case '"':
		    (void) sprintf (line, "label %s = \"",
					v_name(currow, curcol));
		    linelim = strlen (line);
		    insert_mode();
		    break;
		case '<':
		    (void) sprintf (line, "leftstring %s = \"",
			    v_name(currow, curcol));
		    linelim = strlen (line);
		    insert_mode();
		    break;
		case '>':
		    (void) sprintf (line, "rightstring %s = \"",
			   v_name(currow, curcol));
		    linelim = strlen (line);
		    insert_mode();
		    break;
		case 'e':
		    editv (currow, curcol);
		    edit_mode();
		    break;
		case 'E':
		    edits (currow, curcol);
		    edit_mode();
		    break;
		case 'f':
		    if (arg == 1)
			(void) sprintf (line, "format [for column] %s ",
				coltoa(curcol));
		    else {
			(void) sprintf(line, "format [for columns] %s:",
				coltoa(curcol));
			(void) sprintf(line+strlen(line), "%s ",
				coltoa(curcol+arg-1));
		    }
		    error("Current format is %d %d",
				fwidth[curcol],precision[curcol]);
		    linelim = strlen (line);
		    insert_mode();
		    break;
		case 'g':
		    (void) sprintf (line, "goto [v] ");
		    linelim = strlen (line);
		    insert_mode();
		    break;
		case 'P':
		    (void) sprintf (line, "put [\"dest\" range] \"");
		    if (*curfile)
			error ("Default path is \"%s\"",curfile);
		    linelim = strlen (line);
		    insert_mode();
		    break;
		case 'M':
		    (void) sprintf (line, "merge [\"source\"] \"");
		    linelim = strlen (line);
		    insert_mode();
		    break;
		case 'R':
		    if (mdir)
			(void) sprintf (line,"merge [\"macro_file\"] \"%s/", mdir);
		    else
			(void) sprintf (line,"merge [\"macro_file\"] \"");
		    linelim = strlen (line);
		    insert_mode();
		    break;
		case 'D':
		    (void) sprintf (line, "mdir [\"macro_directory\"] \"");
		    linelim = strlen (line);
		    insert_mode();
		    break;
		case 'G':
		    (void) sprintf (line, "get [\"source\"] \"");
		    if (*curfile)
			error ("Default file is \"%s\"",curfile);
		    linelim = strlen (line);
		    insert_mode();
		    break;
		case 'W':
		    (void) sprintf (line, "write [\"dest\" range] \"");
		    linelim = strlen (line);
		    insert_mode();
		    break;
		case 'S':	/* set options */
		    (void) sprintf (line, "set ");
		    error("Options: byrows, bycols, iterations=n, tblstyle=(0|tbl|latex|tex)");
		    linelim = strlen (line);
		    insert_mode();
		    break;
		case 'T':	/* tbl output */
		    (void) sprintf (line, "tbl [\"dest\" range] \"");
		    linelim = strlen (line);
		    insert_mode();
		    break;
		case 'x':
		    {
		    register struct ent **pp;
		    register int c1;

		    flush_saved();
		    if(calc_order == BYROWS) {
		    for (c1 = curcol; arg-- && c1 < maxcols; c1++) {
			pp = ATBL(tbl, currow, c1);
			if (*pp) {
			    free_ent(*pp);
			    *pp = (struct ent *)0;
			}
		    }
		    }
		    else {
		    for (c1 = currow; arg-- && c1 < maxrows; c1++) {
			pp = ATBL(tbl, c1, curcol);
			if (*pp) {
			    free_ent(*pp);
			    *pp = (struct ent *)0;
			}
		    }
		    }
		    sync_refs();
		    modflg++;
		    FullUpdate++;
		    }
		    break;
		case 'Q':
		case 'q':
		    running = 0;
		    break;
		case 'h':
		    backcol(arg);
		    break;
		case 'j':
		    forwrow(arg);
		    break;
		case 'k':
		    backrow(arg);
		    break;
		case ' ':
		case 'l':
		    forwcol(arg);
		    break;
		case 'm':
		    savedrow = currow;
		    savedcol = curcol;
		    break;
		case 'c': {
		    register struct ent *p = *ATBL(tbl, savedrow, savedcol);
		    register c1;
		    register struct ent *n;
		    if (!p)
			break;
		    FullUpdate++;
		    modflg++;
		    for (c1 = curcol; arg-- && c1 < maxcols; c1++) {
			n = lookat (currow, c1);
			(void) clearent(n);
			copyent( n, p, currow - savedrow, c1 - savedcol);
		    }
		    break;
		}
		default:
		    if ((toascii(c)) != c)
			error ("Weird character, decimal %d\n",
				(int) c);
		    else
			    error ("No such command (%c)", c);
		    break;
	    }
	edistate = nedistate;
	arg = narg;
    }				/* while (running) */
    inloop = modcheck(" before exiting");
    }				/*  while (inloop) */
    deraw();
    endwin();
#ifdef VMS	/* Unit VMS "fixes" exit we should say 1 here */
    exit(1);
#else
    exit(0);
#endif
    /*NOTREACHED*/
}

void
startshow()
{
    showrange = 1;
    showsr = currow;
    showsc = curcol;
}

void
showdr()
{
    int     minsr, minsc, maxsr, maxsc;

    minsr = showsr < currow ? showsr : currow;
    minsc = showsc < curcol ? showsc : curcol;
    maxsr = showsr > currow ? showsr : currow;
    maxsc = showsc > curcol ? showsc : curcol;
    (void) sprintf (line+linelim,"%s", r_name(minsr, minsc, maxsr, maxsc));
}

void
setorder(i)
int i;
{
	if((i == BYROWS)||(i == BYCOLS))
	    calc_order = i;
	else
	    error("Not yet implemented");
}

void
setauto(i)
int i;
{
	autocalc = i;
}


#ifdef VMS

goraw()
{
    VMS_read_raw = 1;
    FullUpdate++;
}

deraw()
{
    (void) move (LINES - 1, 0);
    (void) clrtoeol();
    (void) refresh();
    VMS_read_raw = 0;
}

#else /* VMS */
void
goraw()
{
#if SYSV2 || SYSV3
    fixterm();
#else /* SYSV2 || SYSV3 */
    cbreak();
    nonl();
    noecho ();
#endif /* SYSV2 || SYSV3 */
    kbd_again();
    (void) clear();
    FullUpdate++;
}

void
deraw()
{
    (void) move (LINES - 1, 0);
    (void) clrtoeol();
    (void) refresh();
#if SYSV2 || SYSV3
    resetterm();
#else
    nocbreak();
    nl();
    echo();
#endif
    resetkbd();
}

#endif /* VMS */

void
signals()
{
#ifdef SIGVOID
    void quit();
    void time_out();
    void dump_me();
#else
    int quit();
    int time_out();
    int dump_me();
#endif

    (void) signal(SIGINT, SIG_IGN);
    (void) signal(SIGQUIT, dump_me);
    (void) signal(SIGPIPE, quit);
    (void) signal(SIGTERM, quit);
    (void) signal(SIGALRM, time_out);
    (void) signal(SIGFPE, quit);
    (void) signal(SIGBUS, quit);
}

#ifdef SIGVOID
void
#endif
quit()
{
    diesave();
    deraw();
    resetkbd();
    endwin();
    exit(1);
}

#ifdef SIGVOID
void
#endif
dump_me()
{
    diesave();
    deraw();
    abort();
}

/* try to save the current spreadsheet if we can */
diesave()
{   char	path[PATHLEN];
    if (modcheck(" before Spreadsheet dies") == 1)
    {	sprintf(path, "~/SC.SAVE");
	if (writefile(path, 0, 0, maxrow, maxcol) < 0)
	    if (writefile("/tmp/SC.SAVE", 0, 0, maxrow, maxcol) < 0)
		error("Couldn't save current spreadsheet, Sorry");
    }
}

int
modcheck(endstr)
char *endstr;
{
    if (modflg && curfile[0]) {
	int	yn_ans;
	char	lin[100];

	(void) sprintf (lin,"File \"%s\" is modified, save%s? ",curfile,endstr);
	if ((yn_ans = yn_ask(lin)) < 0)
		return(1);
	else
	if (yn_ans == 1)
	{    if (writefile(curfile, 0, 0, maxrow, maxcol) < 0)
 		return (1);
	}
    } else if (modflg) {
	int	yn_ans;

	if ((yn_ans = yn_ask("Do you want a chance to save the data? ")) < 0)
		return(1);
	else
		return(yn_ans);
    }
    return(0);
}
