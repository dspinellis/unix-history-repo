#ifndef lint
static char sccsid[] = "@(#)subr.c	4.2 (Berkeley) 8/11/83";
#endif

/*
 * subr.c: general subroutines for fed.
 */

#include "fed.h"

/*
 * initialize: various one time initializations.
 */
initialize()
{
	register int i, j;
	register char *cp;

	/* Initialize random variables */
	curwind = -1;
	pencolor = 1;
	penweight = 0;

	/*
	 * Initialize value of sqrtmat.  This is a constant table
	 * so we don't have to redo all these square roots when the pen
	 * changes every time.
	 */
	for (i=0; i<10; i++) {
		for (j=0; j<10; j++) {
			sqrtmat[i][j] = sqrt((float) i*i + j*j);
		}
	}

	/* Initialize base locations on screen. These remain fixed. */
	for (i=0; i<NROW; i++)
		for (j=0; j<NCOL; j++) {
			base[NCOL*i+j].c = (GLCOL+GLPAD) * j + 1;
			base[NCOL*i+j].r = SCRHI - (GLROW+GLPAD+10) * i - GLROW - 3;
		}

	setbuf(stdout, stoutbuf);

	curzoom = 1;	/* default is zoomed completely out */
	ttyinit();
}

/*
 * showfont: Wipe clean the screen, display the font
 * in a properly spaced fashion, wait for a char to be typed, if it's
 * p print the font, then clear the screen and ungetc the char.
 */
showfont()
{
	register int i, cr, cc, nc;
	int roff, coff;
	char maxc, minc;
	char nextcmd;
	char tmpbuf[WINDSIZE];

	zoomout();
	message("Show font from <char>");
	minc = inchar();
	sprintf(msgbuf, "Show font from %s to <char>", rdchar(minc));
	message(msgbuf);
	maxc = inchar();

	clearg();
	zermat(tmpbuf, GLROW, GLCOL);
	cr = SCRHI-GLROW; cc = 3;
	for (i=minc; i<=maxc; i++) {
		if (disptable[i].nbytes) {
			/*
			 * We really should try to find out how far to the
			 * left the glyph goes so we don't run off the left
			 * end of the screen, but this is hard, so we fake it.
			 * Usually glyphs don't run past the left so it's OK.
			 */
			if (cc - disptable[i].left < 0)
				cc = disptable[i].left;
			nc = cc + disptable[i].width;
			if (nc >= SCRWID) {
				cc = 0;
				nc = disptable[i].width;
				cr -= 85; /* Should be GLROW but 4*100>360 */
				if (cr < 0)
					break;	/* Screen full.  Just stop. */
			}
			dispmsg(rdchar(i), cc, cr, 2);
			placechar(i, cr+BASELINE, cc, tmpbuf);
			cc = nc;
		}
	}
	for (;;) {
		nextcmd = inchar();
		if (nextcmd != 'p')
			break;
		printg();
	}
	if (nextcmd != 'Q' && nextcmd != 'E' && nextcmd != 'N')
		redraw();
	else
		clearg();
	ungetc(nextcmd, stdin);
}

/*
 * typein: Like showfont but takes a line of text from the user
 * and "typesets" it on the screen.
 */
typein()
{
	register int i, cr, cc, nc;
	char *p;
	int roff, coff;
	char maxc, minc;
	char nextcmd;
	char tmpbuf[WINDSIZE];
	char msgtype[100];

	zoomout();
	readline("Input line to be typeset: ", msgtype, sizeof msgtype);

	clearg();
	zermat(tmpbuf, GLROW, GLCOL);
	cr = SCRHI-GLROW; cc = 3;
	for (p=msgtype; *p; p++) {
		i = *p;
		if (disptable[i].nbytes) {
			if (cc - disptable[i].left < 0)
				cc = disptable[i].left;
			nc = cc + disptable[i].width;
			if (nc >= SCRWID) {
				cc = 0;
				nc = disptable[i].width;
				cr -= 85; /* Should be GLROW but 4*100>360 */
				if (cr < 0)
					break;	/* Screen full.  Just stop. */
			}
			dispmsg(rdchar(i), cc, cr, 2);
			placechar(i, cr+BASELINE, cc, tmpbuf);
			cc = nc;
		}
	}
	for (;;) {
		nextcmd = inchar();
		if (nextcmd != 'p')
			break;
		printg();
	}
	if (nextcmd != 'Q' && nextcmd != 'E' && nextcmd != 'N')
		redraw();
	else
		clearg();
	ungetc(nextcmd, stdin);
}

/*
 * placechar: draw the character ch at position (llr, llc) on the screen.
 * Position means the logical center of the character.  zero is a GLROW x GLCOL
 * matrix of zeros which is needed for comparison, that is, we assume that
 * the spot on the screen where this is going is blank, so the chars better
 * not overlap.
 */
placechar(ch, llr, llc, zero)
int ch;
int llr, llc;
bitmat zero;
{
	bitmat glbuf;
	int roff, coff;

	glbuf = findbits(ch, GLROW, GLCOL, 0, 0, &roff, &coff);
	if (glbuf == NULL)
		return;
	if (trace)
		fprintf(trace, "placechar('%s'), roff=%d, coff=%d, llr=%d, llc=%d, down=%d, left=%d, r=%d, c=%d\n", rdchar(ch), roff, coff, llr, llc, disptable[ch].down, disptable[ch].left, llr-disptable[ch].down, llc-disptable[ch].left);

	update(zero, glbuf, GLROW, GLCOL, llr-GLROW+roff, llc-coff);
	if (trace)
		fprintf(trace, "placechar, free %x\n", glbuf);
	free(glbuf);
}

/*
 * redraw: The screen has gotten screwed up somehow.
 * Assume nothing but make it look right.
 */
redraw()
{
	register int i;

	zoomout();
	clearg();
	turnofrb();
	for (i=0; i<NWIND; i++)
		if (wind[i].onscreen != NULL) {
			zermat(wind[i].onscreen, GLROW, GLCOL);
			syncwind(i);

			/* Print the char at the lower left of the window */
			sprintf(msgbuf, "%s", rdchar(wind[i].used));
			dispmsg(msgbuf, base[i].c, base[i].r-11, 2);
		}
	if (curwind >= 0)
		drawbox(base[curwind].r-1, base[curwind].c-1, 1, GLROW+2, GLCOL+2);
}

/*
 * findbits: find the data bits of glyph c, wherever they are, and make
 * nr x nc bitmat and put them in it, shifted by horoff and vertoff.
 */
bitmat
findbits(c, nr, nc, horoff, vertoff, rcenter, ccenter)
int c;
int nr, nc;	/* the size of the dest */
int horoff, vertoff;
int *rcenter, *ccenter;
{
	register int i, j;
	register int r1, r2, c1, c2;
	bitmat retval, source;
	int tr, tc;	/* the size of source */
	char tmp[WINDSIZE];

	if (trace)
		fprintf(trace, "findbits(c=%s, nr=%d, nc=%d, horoff=%d, vertoff=%d\n", rdchar(c), nr, nc, horoff, vertoff);
	if (disptable[c].nbytes == 0)
		return (NULL);
	switch (cht[c].wherewind) {
	case -2:
		if (trace)
			fprintf(trace, "case -2, saved from prev place\n");
		/* Saved from previous place */
		source = cht[c].whereat;

		/* Ignore horoff/vertoff assuming they are already right */
		*rcenter = cht[c].rcent;
		*ccenter = cht[c].ccent;
		/*
		 * Small but important optimization: if the desired result is
		 * a whole window and the source happens to be in a whole
		 * window, just return the source pointer.  This saves
		 * lots of memory copies and happens quite often.
		 */
		if (nr == GLROW && nc == GLCOL)
			return (source);
		tr = GLROW; tc = GLCOL;
		break;
	case -1:
		if (trace)
			fprintf(trace, "case -1: first time\n");
		/* First time for this glyph: get it from font file */
		fseek(fontdes, (long) fbase+disptable[c].addr, 0);
		tr = cht[c].nrow; tc = cht[c].ncol;
		if (tr > GLROW || tc > GLCOL || disptable[c].nbytes > WINDSIZE)
			error("glyph too large for window");
		*rcenter = vertoff + disptable[c].up;
		*ccenter = horoff  + disptable[c].left;
		source = tmp;
		fread(source, disptable[c].nbytes, 1, fontdes);
		break;
	default:
		if (trace)
			fprintf(trace, "case default, in window %d", cht[c].wherewind);
		source = wind[cht[c].wherewind].val;
		tr = GLROW; tc = GLCOL;
		*rcenter = vertoff + cht[c].rcent;
		*ccenter = horoff  + cht[c].ccent;
		break;
	}
	if (trace)
		fprintf(trace, "curchar=%c=%d, tr=%d, tc=%d\n", curchar, curchar, tr, tc);

	dumpmat("before copy, source", source, tr, tc);
	/* Copy in the bits into a bitmat of the right size */
	retval = newmat(nr, nc);
	r1 = max(0, -vertoff);
	r2 = min(GLROW-vertoff-1, GLROW-1);
	r2 = min(r2, tr-1);
	c1 = max(0, -horoff);
	c2 = min(GLCOL-horoff-1, GLCOL-1);
	c2 = min(c2, tc-1);
	if (trace)
		fprintf(trace, "findbits copy: r1=%d, r2=%d, c1=%d, c2=%d, horoff=%d, vertoff=%d\n", r1, r2, c1, c2, horoff, vertoff);
	for (i=r1; i<=r2; i++) {
		for (j=c1; j<=c2; j++)
			setmat(retval, nr, nc, i+vertoff, j+horoff, mat(source, tr, tc, i, j, 6));
	}
	dumpmat("result of copy", retval, nr, nc);
	return (retval);
}

/*
 * bufmod: called just before a buffer modifying command.
 * Makes a backup copy of the glyph so we can undo later.
 */
bufmod()
{
	changes++;
	if (curwind < 0)
		return;
	if (wind[curwind].undval == NULL)
		wind[curwind].undval = newmat(GLROW, GLCOL);
	bitcopy(wind[curwind].undval, wind[curwind].val, GLROW, GLCOL);
	und_p_r = pen_r; und_p_c = pen_c;
	und_c_r = curs_r; und_c_c = curs_c;
}

/*
 * undo: restore the backup copy.  We just swap pointers, which is
 * the same as interchanging the two matrices.  This way, undo is
 * its own inverse.
 */
undo()
{
	register bitmat tmp;

	if (wind[curwind].undval == NULL) {
		error("Nothing to undo");
	}
	tmp = wind[curwind].val;
	wind[curwind].val = wind[curwind].undval;
	wind[curwind].undval = tmp;
	pen_r = und_p_r; pen_c = und_p_c;
	move(base[curwind].c+pen_c, base[curwind].r+GLROW-pen_r);
	curs_r = und_c_r; curs_c = und_c_c;
	syncwind(curwind);
	changes++;
}

/*
 * drawline: draw a line of current flavor between the named two points.
 * All points are relative to current window.
 *
 * The algorithm is that of a simple DDA.  This is similar to what the
 * hardware of the HP 2648 does but the placing of the points will be
 * different (because of thick pens and erasers).
 */
drawline(from_r, from_c, to_r, to_c)
{
	int length, i;
	float x, y, xinc, yinc;

	if (trace)
		fprintf(trace, "drawline from (%d, %d) to (%d, %d)\n", from_r, from_c, to_r, to_c);
	length = max(abs(to_r-from_r), abs(to_c-from_c));
	if (length <= 0) {
		/*
		 * The actual value doesn't matter, we're just avoiding
		 * division by zero here.
		 */
		xinc = yinc = 1.0;
	} else {
		xinc = ((float) (to_r-from_r))/length;
		yinc = ((float) (to_c-from_c))/length;
	}
	drawpoint(from_r, from_c);
	x = from_r + 0.5; y = from_c + 0.5;

	for (i=0; i<length; i++) {
		x += xinc; y += yinc;
		drawpoint((int) x, (int) y);
	}
}

/*
 * drawpoint: make a point of the current flavor at (r, c).
 */
drawpoint(r, c)
register int r, c;
{
	register int i, j;

	if (penweight == 0)
		setmat(wind[curwind].val, GLROW, GLCOL, r, c, pencolor);
	else {
		for (i=0; i<10; i++)
			for (j=0; j<10; j++)
				if (penmat[i][j])
					setmat(wind[curwind].val, GLROW, GLCOL, r+i-4, c+j-4, pencolor);
	}
}

/*
 * setcmd: handle the s command.  Format: s <what> <where>.
 */
setcmd()
{
	char what, where;

	message("set <what>");
	what = inchar();
	switch (what) {

	case 'p':	/* set pen */
		message("set pen <weight>");
		where = inchar();
		switch (where) {
		case 'f':	/* set pen fine */
		case 'l':	/* set pen light */
			message("set pen fine");
			penweight = 0;
			break;
		case 'h':	/* set pen heavy */
		case 'b':	/* set pen bold */
			message("set pen heavy");
			penweight = 1;
			break;
		default:
			error("Illegal kind of pen weight");
		}
		break;
	
	case 's':	/* set size of heavy pen */
		message("set pen size to <size>");
		where = inchar() - '0';
		sprintf(msgbuf, "set pen size to %d", where);
		message(msgbuf);
		if (where > 0 && where < 10) {
			setpen(where);
		} else
			error("Illegal size");
		break;
	
	case 'd':
		message("set draw");
		pencolor = 1;
		break;

	case 'e':
		message("set erase");
		pencolor = 0;
		break;

	default:
		error("Illegal set");
	}
}

/*
 * setpen: set the heavy pen size to s.
 * Main work here is defining template of pen.
 */
setpen(s)
int s;
{
	register int i, j;
	register float radius;

	if (s < 1)
		s = 1;
	hpensize = s;
	radius = hpensize;
	radius /= 2;
	for (i=0; i<10; i++) {
		for (j=0; j<10; j++) {
			penmat[i][j] = (radius >= sqrtmat[abs(i-4)][abs(j-4)]);
		}
	}

	/*
	 * Kludge to make a 2-wide pen possible by specifying 1.
	 */
	if (hpensize == 1)
		penmat[4][5] = 1;

	if (trace)
		for (i=0; i<10; i++) {
			for (j=0; j<10; j++) {
				fprintf(trace, "%c", penmat[i][j] ? 'P' : '.');
			}
			fprintf(trace, "\n");
		}
}

/*
 * error: print the given error message and return for another command.
 */
error(msg)
char *msg;
{
	message(msg);
	longjmp(env);
}

/*
 * copymove: do a move or copy command.
 * cmd is C or M, the command.
 */
copymove(cmd)
char cmd;
{
	char *action;
	char src, dest;
	bitmat cpy;
	char lochr[5];

	if (cmd == 'C')
		action = "copy";
	else
		action = "move";
	sprintf(msgbuf, "%s <from>", action);
	message(msgbuf);
	src = inchar();
	sprintf(msgbuf, "%s %s to <to>", action, rdchar(src));
	message(msgbuf);
	dest = inchar();
	strcpy(lochr, rdchar(src));
	sprintf(msgbuf, "%s %s to %s", action, lochr, rdchar(dest));
	message(msgbuf);

	/* Do the copy */
	disptable[dest] = disptable[src];
	cht[dest] = cht[src];
	if (cht[dest].wherewind >= 0)
		wind[cht[dest].wherewind].used = dest;

	if (cmd == 'C') {
		if (cht[dest].wherewind != -1) {
			/*
			 * Make copies of the window so changing
			 * one won't change the other.
			 * The old copy gets the window on the screen, if any,
			 * relegating the new copy to the background.
			 */
			cpy = newmat(GLROW, GLCOL);
			if (cht[dest].wherewind >= 0)
				bitcopy(cpy, wind[cht[src].wherewind].val, GLROW, GLCOL);
			else
				bitcopy(cpy, cht[src].whereat, GLROW, GLCOL);
			if (cht[dest].wherewind == curwind)
				curwind = -1;
			cht[dest].wherewind = -2;
			cht[dest].whereat = cpy;
		}
	} else {
		/*
		 * Move. Delete the old entries.
		 */
		disptable[src].addr = disptable[src].nbytes = 0;
		cht[src].wherewind = -1;
	}
	changes++;
}

/*
 * cch: make sure there is a current character.
 */
cch()
{
	if (curwind < 0)
		error("No current glyph");
}

/*
 * confirm: if there have been changes, ask user if he is sure.
 */
confirm()
{
	char ch;

	if (changes == 0)
		return;
	message("Changes since last write -- Are you sure?");
	ch = inchar();
	if (isupper(ch))
		ch = tolower(ch);
	switch (ch) {
	case 'y':
	case 'q':
	case 'e':
		return;
	case 'n':
	default:
		error("Not sure - aborted");
	}
}

/*
 * delchar: the D command.  Delete a character from the buffer.
 */
delchar()
{
	register char c, c1, c2;
	register int w;
	char buf[5];

	message("delete <char>");
	c1 = inchar();
	sprintf(msgbuf, "delete %s through <char>", rdchar(c1));
	message(msgbuf);
	c2 = inchar();
	strcpy(buf, rdchar(c1));
	sprintf(msgbuf, "delete %s through %s", buf, rdchar(c2));
	message(msgbuf);
	changes++;

	for (c=c1; c<=c2; c++) {
		if ((w = cht[c].wherewind) >= 0) {
			zermat(wind[w].val, GLROW, GLCOL);
			syncwind(w);
		}
		cht[c].wherewind = -1;
		disptable[c].addr = 0;
		disptable[c].nbytes = 0;
		disptable[c].up = 0;
		disptable[c].down = 0;
		disptable[c].left = 0;
		disptable[c].right = 0;
		disptable[c].width = 0;
	}
}

/*
 * zoom out to full screen so the screen doean't go nuts when we
 * print off the current zoom window.  Save old value of zoom in
 * oldzoom so space can put us back.
 */
zoomout()
{
	if (curzoom != 1)
		zoomn(curzoom = 1);
}

/*
 * newglyph: the n command.
 */
newglyph()
{
	register int i, j;
	int windno;
	int vertoff, horoff;
	char *tmp;

	message("new glyph <char>");
	curchar = inchar();
	sprintf(msgbuf, "new glyph %s", rdchar(curchar));
	message(msgbuf);

	if (trace)
		fprintf(trace, "\n\nnewglyph(%s)\n", rdchar(curchar));
	if (disptable[curchar].nbytes != 0) {
		if (trace)
			fprintf(trace, "char exists: %s\n", rdchar(curchar));
		sprintf(msgbuf, "char exists: %s", rdchar(curchar));
		error(msgbuf);
	}

	turnofcurs();
	/*
	 * Not on screen.  First find a suitable window,
	 * using round robin.
	 */
	windno = nextwind;
	if (trace)
		fprintf(trace, "chose window %d\n", windno);
	if (++nextwind >= NWIND)
		nextwind = 0;
#ifdef notdef
	if (nextwind >= 3)
		nextwind = 0;
#endif
	wind[windno].used = curchar;

	/* Put a box around the current window */
	if (windno != curwind) {
		drawbox(base[curwind].r-1, base[curwind].c-1, 0, GLROW+2, GLCOL+2);
		drawbox(base[windno].r-1, base[windno].c-1, 1, GLROW+2, GLCOL+2);
	}

	/* Print the char at the lower left of the window */
	sprintf(msgbuf, "%s", rdchar(curchar));
	dispmsg(msgbuf, base[windno].c, base[windno].r-11, 2);
	
	/* Now make room in the window */
	if (wind[windno].onscreen == NULL) {
		/* Brand new window, have to allocate space */
		wind[windno].onscreen = newmat(GLROW, GLCOL);
	} else {
		/* Save prev glyph for later */
		cht[wind[curchar].used].whereat = wind[windno].val;
		cht[wind[curchar].used].wherewind = -2;
	}
	if (wind[windno].undval != NULL) {
		if (trace)
			fprintf(trace, "newglyph frees undo: %x\n", wind[windno].undval);
		free(wind[windno].undval);
	}
	wind[windno].undval = NULL;

	/*
	 * Vertical & horizontal offsets.  Line up the baseline
	 * of the char at BASELINE from bottom, but center
	 * horizontally.
	 */
	wind[windno].val = newmat(GLROW, GLCOL);

	curwind = windno;
	cht[curchar].wherewind = windno;
	cht[curchar].rcent = curs_r = GLROW - BASELINE;
	cht[curchar].ccent = curs_c = GLCOL / 2;

#ifdef notdef
	dumpmat("wind[windno].onscreen", wind[windno].onscreen, GLROW, GLCOL);
#endif
	syncwind(windno);

	/*
	 * Mung the zoom out to 1 and back.  This is needed to
	 * re-center the glyph on the screen if zoomed in, otherwise
	 * if you move by one window it puts the cursor way over at
	 * the right with only half the window visible.
	 */
	if ((i = curzoom) > 1) {
		zoomn(1);
		zoomn(i);
	}
}

/*
 * numedit: change one of the numerical parameters.
 */
numedit()
{
	short * sp = 0;
	char * cp = 0;
	char c, f;
	char *fld;
	short ovalue, nvalue;
	char numb[20];

	message("number of <char>");
	c = inchar();
	sprintf(msgbuf, "number of %s <field>", rdchar(c));
	message(msgbuf);
	f = inchar();

	switch (f) {
	case 'a': sp = (short *)
			&disptable[c].addr;	fld = "addr";	break;
	case 'n': sp = &disptable[c].nbytes;	fld = "nbytes";	break;
	case 'u': cp = &disptable[c].up;	fld = "up";	break;
	case 'd': cp = &disptable[c].down;	fld = "down";	break;
	case 'l': cp = &disptable[c].left;	fld = "left";	break;
	case 'r': cp = &disptable[c].right;	fld = "right";	break;
	case 'w': sp = &disptable[c].width;	fld = "width";	break;
	case 's': sp = (short *) &disptable[c].nbytes;
						fld = "size";	break;
	default: error("No such field");
	}

	ovalue = sp ? *sp : *cp;
	sprintf(msgbuf, "number of %s %s (old value %d) is ", rdchar(c), fld, ovalue);
	readline(msgbuf, numb, sizeof numb);
	nvalue = atoi(numb);
	if (cp)
		*cp = nvalue;
	else
		*sp = nvalue;
	changes++;
}

/*
 * These routines turn the cursor and rubber band line on and off,
 * remembering its state for the o and r commands.
 */
turnoncurs()
{
	curon();
	curcurs = 1;
}

turnofcurs()
{
	curoff();
	curcurs = 0;
}

turnonrb()
{
	rbon();
	currb = 1;
}

turnofrb()
{
	rboff();
	currb = 0;
}

synccurs()
{
	register int x, y;

	x = base[curwind].c + curs_c;
	y = base[curwind].r + GLROW - curs_r - 1;
	movecurs(x, y);
}

inchar()
{
	sync();
	synccurs();
	return (rawchar());
}

/*
 * fillin - fill in with 1's all the spots that are in the enclosed
 * area that (x, y) is in.
 */
fillin(x, y)
int x, y;
{
	if (x<0 || x>=GLROW || y<0 || y>=GLCOL ||
		mat(wind[curwind].val, GLROW, GLCOL, x, y))
		return;

	setmat(wind[curwind].val, GLROW, GLCOL, x, y, 1);
	fillin(x-1, y);
	fillin(x+1, y);
	fillin(x, y-1);
	fillin(x, y+1);
}

/*
 * syncwind: make sure that window #n shows on the screen what it's
 * supposed to after an arbitrary change.
 */
syncwind(n)
int n;
{
	if (trace)
		fprintf(trace, "syncwind(%d)\n", n);
	update(wind[n].onscreen, wind[n].val, GLROW, GLCOL, base[n].r, base[n].c);
	bitcopy(wind[n].onscreen, wind[n].val, GLROW, GLCOL);
}

/*
 * Embolden artificially emboldens the glyphs in the font by smearing
 * them to the right by the current heavy pen size.  Or else italicize it.
 */
artificial()
{
	int low, high, cur;
	int oldps, newps;
	char lowch[10];
#define ITAL	0
#define BOLD	1
#define RESIZE	2
#define SMOOTH	3
	int kind;
	char *strbold;

	sprintf(msgbuf, "Artificially <embolden/italicize/resize/smooth>");
	message(msgbuf);

	cur = inchar();
	switch(cur) {
	case 'i': case 'I': kind = ITAL; strbold = "italicize"; break;
	case 'e': case 'E': kind = BOLD; strbold = "embolden"; break;
	case 'r': case 'R': kind = RESIZE; strbold = "resize"; break;
	case 's': case 'S': kind = SMOOTH; strbold = "smooth"; break;
	default: error("No such artificial operation");
	}

	sprintf(msgbuf, "Artificially %s glyphs from <char>", strbold);
	message(msgbuf);
	low = inchar();
	strcpy(lowch, rdchar(low));
	sprintf(msgbuf, "Artificially %s glyphs from %s to <char>", strbold, lowch);
	message(msgbuf);
	high = inchar();
	if (kind == RESIZE) {
		sprintf(msgbuf, "Artificially %s glyphs from %s to %s from <point size>", strbold, lowch, rdchar(high));
		oldps = readnum(msgbuf);
		sprintf(msgbuf, "Artificially %s glyphs from %s to %s from %dP to <point size>P", strbold, lowch, rdchar(high), oldps);
		newps = readnum(msgbuf);
		sprintf(msgbuf, "Artificially %s glyphs from %s to %s from %dP to %dP", strbold, lowch, rdchar(high), oldps, newps);
		message(msgbuf);
		if (oldps <= 0 || oldps > 36 || newps <= 0 || newps > 36 || oldps == newps)
			error("Bad point sizes");
	} else {
		sprintf(msgbuf, "Artificially %s glyphs from %s to %s", strbold, lowch, rdchar(high));
		message(msgbuf);
	}

	for (cur=low; cur<=high; cur++) {
		getglyph(cur);
		if (curchar == cur) {	/* e.g. if the getglyph succeeded */
			fflush(stdout);
			switch (kind) {
			case BOLD:
				boldglyph();
				break;
			case ITAL:
				italglyph();
				break;
			case RESIZE:
				if (oldps > newps)
					shrinkglyph(oldps, newps);
				else
					blowupglyph(oldps, newps);
				break;
			case SMOOTH:
				smoothglyph();
				break;
			}
			syncwind(curwind);
		}
	}
	message("Done");
}

/*
 * Artificially embolden the current glyph.
 */
boldglyph()
{
	register int r, c, i;
	int smear = hpensize < 2 ? 2 : hpensize;

	for (r=0; r<GLROW; r++)
		for (c=GLCOL-1; c>=smear; c--)
			for (i=1; i<=smear; i++)
				if (mat(wind[curwind].val, GLROW, GLCOL, r, c-i))
					setmat(wind[curwind].val, GLROW, GLCOL, r, c, 1);
}

/*
 * Artificially italicize the current glyph.
 */
italglyph()
{
	register int r, c, i, off;
	int baser = cht[curchar].rcent; /* GLROW - BASELINE; */

	for (r=0; r<baser; r++) {
		off = (baser-r) / SLOPE + 0.5;
		for (c=GLCOL-1; c>=off; c--) {
			setmat(wind[curwind].val, GLROW, GLCOL, r, c,
				mat(wind[curwind].val, GLROW, GLCOL, r, c-off));
		}
		for (c=off-1; c>=0; c--)
			setmat(wind[curwind].val, GLROW, GLCOL, r, c, 0);
	}
	for (r=baser; r<GLROW; r++) {
		off = (r-baser) * (2.0/7.0) + 0.5;
		for (c=off; c<GLCOL; c++)
			setmat(wind[curwind].val, GLROW, GLCOL, r, c-off,
				mat(wind[curwind].val, GLROW, GLCOL, r, c));
		for (c=off-1; c>=0; c--)
			setmat(wind[curwind].val, GLROW, GLCOL, r, c, 0);
	}
}

/*
 * Blow up or shrink a glyph from oldps points to newps points.
 * The basic idea is that for each on point in the old glyph we
 * find the corresponding point in the new glyph and copy the value.
 */
shrinkglyph(oldps, newps)
int oldps, newps;
{
	float ratio;
	register int or, oc, nr, nc;
	int n;
	bitmat tmp, curw;
	int baser = cht[curchar].rcent;
	int basec = cht[curchar].ccent;

	ratio = (float) newps / (float) oldps;
	tmp = newmat(GLROW, GLCOL);
	curw = wind[curwind].val;
	bitcopy(tmp, curw, GLROW, GLCOL);
	zermat(curw, GLROW, GLCOL);
	for (or=0; or<GLROW; or++) {
		nr = baser + (or-baser)*ratio + 0.5;
		for (oc=0; oc<GLCOL; oc++) {
			nc = basec + (oc-basec)*ratio + 0.5;
			if (nr < 0 || nr >= GLROW || nc < 0 || nc >= GLCOL)
				n = 0;
			else
				n = mat(tmp, GLROW, GLCOL, or, oc);
			if (n)
				setmat(curw, GLROW, GLCOL, nr, nc, n);
		}
	}
	disptable[curchar].width = disptable[curchar].width * ratio + 0.5;
	free(tmp);
}

/*
 * blow up a glyph.  Otherwise like shrinkglyph.
 */
blowupglyph(oldps, newps)
int oldps, newps;
{
	float ratio;
	register int or, oc, nr, nc;
	int n;
	bitmat tmp, curw;
	int baser = cht[curchar].rcent;
	int basec = cht[curchar].ccent;

	ratio = (float) oldps / (float) newps;
	tmp = newmat(GLROW, GLCOL);
	curw = wind[curwind].val;
	bitcopy(tmp, curw, GLROW, GLCOL);
	zermat(curw, GLROW, GLCOL);
	for (nr=0; nr<GLROW; nr++) {
		or = baser + (nr-baser)*ratio + 0.5;
		for (nc=0; nc<GLCOL; nc++) {
			oc = basec + (nc-basec)*ratio + 0.5;
			if (or < 0 || or >= GLROW || oc < 0 || oc >= GLCOL)
				n = 0;
			else
				n = mat(tmp, GLROW, GLCOL, or, oc);
			if (n)
				setmat(curw, GLROW, GLCOL, nr, nc, n);
		}
	}
	disptable[curchar].width = disptable[curchar].width / ratio + 0.5;
	free(tmp);
}

/*
 * Smooth a glyph.  We look for corners and trim the point.  Corners of
 * both blanks and dots in all 4 orientations are looked for.
 */
smoothglyph()
{
	bitmat tmp, curw;
	register int r, c;
	register int c3;
	int a3, b2, b3, b4, c1, c2, c4, c5, d2, d3, d4, e3;

	tmp = newmat(GLROW, GLCOL);
	curw = wind[curwind].val;
	bitcopy(tmp, curw, GLROW, GLCOL);
	for (r=2; r<GLROW-2; r++)
		for (c=2; c<GLCOL-2; c++) {
			/*
			 *		a3
			 *	     b2 b3 b4
			 *	  c1 c2 c3 c4 c5
			 *	     d2 d3 d4
			 *	        d4
			 * where c3 is the square we are interested in
			 */
			b3 = mat(tmp, GLROW, GLCOL, r-1, c  );
			c2 = mat(tmp, GLROW, GLCOL, r  , c-1);
			c4 = mat(tmp, GLROW, GLCOL, r  , c+1);
			d3 = mat(tmp, GLROW, GLCOL, r+1, c  );
			/* exactly 2 of the 4 neighbors must be dots */
			if (b3+c2+c4+d3 != 2) continue;

			c3 = mat(tmp, GLROW, GLCOL, r  , c  );
			b2 = mat(tmp, GLROW, GLCOL, r-1, c-1);
			b4 = mat(tmp, GLROW, GLCOL, r-1, c+1);
			d2 = mat(tmp, GLROW, GLCOL, r+1, c-1);
			d4 = mat(tmp, GLROW, GLCOL, r+1, c+1);
			/* exactly one of the 4 diags must match the center */
			if (b2+b4+d2+d4 != 3 - 2*c3) continue;

			a3 = mat(tmp, GLROW, GLCOL, r-2, c  );
			c1 = mat(tmp, GLROW, GLCOL, r  , c-2);
			c5 = mat(tmp, GLROW, GLCOL, r  , c+2);
			e3 = mat(tmp, GLROW, GLCOL, r+2, c  );

			/* Figure out which of the 4 directions */
			if (b2==c3) {
				if (b3+c2+c1+a3 != 4*c3) continue;
			} else
			if (b4==c3) {
				if (b3+c4+c5+a3 != 4*c3) continue;
			} else
			if (d2==c3) {
				if (d3+c2+c1+e3 != 4*c3) continue;
			} else
			if (d4==c3) {
				if (d3+c4+c5+e3 != 4*c3) continue;
			}

			/* It must be a corner.  Toggle it. */
			setmat(curw, GLROW, GLCOL, r, c, !c3);
		}
	free(tmp);
}

/*
 * Read a number from bottom line ala readline.
 * This should probably go in lib2648.
 */
int
readnum(prompt)
char *prompt;
{
	char buf[10];
	int retval;

	readline(prompt, buf, sizeof buf);
	retval = atoi(buf);
	if (trace)
		fprintf(trace, "readline returns '%s', retval=%d\n", buf, retval);
	return (retval);
}

invert()
{
	register int r, c;
	int tmp1, tmp2, kind;
	bitmat curw = wind[curwind].val;

	message("Invert <horizontally/vertically>");
	kind = inchar();
	switch (kind) {
	case 'h': case 'H':
		message("Invert horizontally");
		for (r=0; r<GLROW; r++) {
			if (trace)
				fprintf(trace, "row %d\n", r);
			for (c=0; c<=(GLCOL-1)/2; c++) {
				tmp1 = mat(curw, GLROW, GLCOL, r, c);
				tmp2 = mat(curw, GLROW, GLCOL, r, GLCOL-1-c);
				if (trace)
					fprintf(trace, "cols %d (%d) <=> %d (%d)\n", c, tmp1, GLCOL-1-c, tmp2);
				setmat(curw, GLROW, GLCOL, r, c, tmp2);
				setmat(curw, GLROW, GLCOL, r, GLCOL-1-c, tmp1);
			}
		}
		break;
	case 'v': case 'V':
		message("Invert vertically");
		for (c=0; c<GLCOL; c++) {
			for (r=0; r<=(GLROW-1)/2; r++) {
				tmp1 = mat(curw, GLROW, GLCOL, r, c);
				tmp2 = mat(curw, GLROW, GLCOL, GLROW-1-r, c);
				setmat(curw, GLROW, GLCOL, r, c, tmp2);
				setmat(curw, GLROW, GLCOL, GLROW-1-r, c, tmp1);
			}
		}
		break;
	default:
		error("Bad choice");
	}
	syncwind(curwind);
}
