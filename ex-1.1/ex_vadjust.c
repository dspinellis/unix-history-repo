#include "ex.h"
#ifdef VISUAL
#include "ex_tty.h"
#include "ex_vis.h"

/*
 * Ex - a text editor
 * Bill Joy UCB September 1977
 */

vback(i)
	register int i;
{
	register int j, *tp;

	/*
	 * Candidate for first line is tp.
	 * To take tp-1 instead he must fit in
	 * the remaining space and obviously must
	 * exit, hence need "tp > one".
	 */
	for (tp = dot; tp > one; tp--) {
		getline(tp[-1]);
		j = vdepth();
		if (j > i)
			break;
		i =- j;
	}
	return (tp);
}

int	listchar(), (*Putchar)();
/*
 * Return the depth of the line in linebuf.
 * Note that VCOLUMNS are always completely usable.
 * The last line on a terminal with automargins is not
 * normally used; if the terminal does not have an addressible
 * cursor, then we use the last line but not the last column.
 */
vdepth()
{
	register int i;

	i = (column(0) + VCOLUMNS - 1 + (Putchar == &listchar)) / VCOLUMNS;
	return (i == 0 ? 1 : i);
}

/*
 * Open the line tp at line l on the physical screen.
 * The rest of the lines are shifted down as necessary.
 * The caller is responsible for adjusting vcline as he/she sees fit.
 * We return the number of lines occupied by this line on the screen.
 */
vopen(tp, l)
	int *tp, l;
{
	register int i, j;

	/*
	 * If we are doing an open command then we start anew with
	 * each opened line.
	 */
	if (!visual)
		vcnt = 0, vcline = 0;
#ifdef DEBUG
	if (vcline > vcnt || vcline < 0 || vcnt < 0 || vcnt > TUBELINES)
		error("Internal error: vopen@- please tell someone");
	if (tp < one || tp > dol)
		error("Internal error: vopen tp@- please tell someone");
#endif
	/*
	 * Have to be very careful before calling vreopen.
	 * Note that as a consequence of this reopen, the screen
	 * may roll up, invalidating our value for l.
	 * Thus we must put this l into the vliny array so it
	 * will be adjusted properly.
	 */
	for (j = vcnt; j >= vcline; j--)
		vliny[j + 1] = vliny[j];
	vliny[vcline] = l;
	/*
	 * Get the line to be opened and "reopen" it on the screen.
	 * Reopen returns the number of screen lines occupied.
	 */
	vcnt++;
	getline(*tp);
	i = vreopen(l, tp - zero);
	/*
	 * If this is the last line on the screen, then there is
	 * not much to do here.
	 */
	if (vcline + 1 == vcnt) {
		vliny[vcnt] = vliny[vcline] + i;
		return (i);
	}
	/*
	 * There are lines on the screen which appeared after the place
	 * where this one was opened and they have a chance to remain.
	 * If there is not enough space between the new line and the
	 * one following it then the screen tail will have to be redrawn
	 * to reflect the new line's presence.
	 */
	vfixopen(i);
	return (i);
}

vfixopen(i)
	register int i;
{
	register int need;
	extern int (*Pline)(), numbline();

	if (LINES != VLINES) {
		need = vliny[vcline] + i - vliny[vcline + 1];
		if (need > 0)
			vopenup(need);
	}
	vsync(vliny[vcline] + i);
}

/*
 * Guts of the open of the line in linebuf at
 * line l on the physical screen.  We first check
 * that the line will fit on the screen.  We then
 * goto the beginning of the line, put out the
 * characters of the line and clear to the end of
 * the line, returning the number of lines we used.
 */
vreopen(l, lineno)
	int l, lineno;
{
	register int i;

	vigoto(l, 0);
	i = vdepth();
	if ((!CA && i > 1) || i > LINES - 1)
		error("Line too long@for open");
	pline(lineno);
	if (Putchar == &listchar)
		putchar('$');
	vclreol();
	return (i);
}

/*
 * Return the number of new screen lines required
 * to make i lines starting at t fit on the screen.
 */
vfit(i, t)
	int i, *t;
{
	register int j;

	j = 0;
	while (i > 0) {
		i--;
		getline(t[i]);
		j =+ vdepth();
	}
	/*
	 * Account for the free space
	 * at the bottom.
	 */
	j =- VLINES - vlast;
	return (j);
}

/*
 * Roll a specified number of lines onto the screen.
 * If more than one line is being rolled, switch to
 * cooked mode so that the user can stop or interrupt the output.
 */
vroll(ocnt)
	int ocnt;
{
	register int cnt;

	cnt = ocnt;
	if (cnt > 1)
		vcook();
	for (; cnt > 0; cnt--) {
		dot++;
		vcline++;
		if (!CA) {
			vup1();
			vscroll(1);
			vlast = LINES - 1;
		}
		vopen(dot, vlast);
		vcsync();
	}
	if (ocnt > 1) {
		flusho();
		vraw();
	}
}

/*
 * Roll the screen up.  The argument is by reference because
 * it may be used in subsequent computation and therefore must
 * be changed to reflect this call.  We make the argument line
 * be the last line of the screen.
 */
vrollup(ip)
	int *ip;
{
	register int i;

	i = *ip - LINES + 2;
	vscroll(i);
	*ip =- i;
	vmoveitup(i);
	vscrap();
}

/*
 * Move the screen up i lines physically
 */
vmoveitup(i)
	int i;
{

#ifdef DEBUG
	if (i < 0)
		error("Internal error: vmoveitup@- please tell someone");
#endif
	if (i == 0)
		return;
	destline = LINES + i - 1;
	destcol = outcol % COLUMNS;
	fgoto();
}

/*
 * Move the screen up i lines logically
 */
vscroll(i)
	register int i;
{
	register int j;
	char *tlines[TUBELINES];

#ifdef DEBUG
	if (i < 0 || i > TUBELINES)
		error("Internal error: vscroll@- please tell someone");
#endif
	if (i == 0)
		return;
	copy(tlines, vtube, sizeof vtube);
	copy(vtube, &tlines[i], sizeof vtube - i * sizeof vtube[0]);
	copy(&vtube[TUBELINES - i], tlines, i * sizeof vtube[0]);
	for (j = TUBELINES -  i; j < TUBELINES; j++)
		vclrbyte(vtube[j], VCOLUMNS);
	for (j = 0; j <= vcnt; j++)
		vliny[j] =- i;
}

/*
 * Discard logical lines due to physical
 * wandering off the screen.
 */
vscrap()
{
	register int i, j;

	for (j = 0; j < vcnt; j++)
		if (vliny[j] >= ZERO) {
			if (j == 0)
				break;
			/*
			 * Discard the first j physical lines off the top
			 */
			vcnt =- j;
			vcline =- j;
			for (i = 0; i <= vcnt; i++)
				vliny[i] = vliny[i + j];
			break;
		}
	/*
	 * Discard lines off the bottom
	 */
	for (j = 0; j < vcnt; j++)
		if (vliny[j] >= VLINES)
			vcnt = j;
	if (vcnt == 0)
		error("No lines fit@on screen!");
}

/*
 * Open blank lines on the screen
 */
vopenup(i)
	int i;
{
	register int j, l;

	if (!visual && !CA) {
		vsave();
		error("Line too long@for open");
	}
	j = LINES - vliny[vcline + 1];
#ifdef DEBUG
	if (j < 0)
		error("Internal error: vopenup@- please tell someone");
#endif
	if (i > j)
		i = j;
	for (l = vcline + 1; l <= vcnt; l++)
		vliny[l] =+ i;
	vscrap();
}

/*
 * Synchronize the screen.  Vredraw is more ambitious than vsync
 * but correspondingly may take more resources to do its deed.
 */
vredraw(p)
	register int p;
{
	register int l, *tp;
	char temp[LBSIZE];
	int ovcline;

#ifdef DEBUG
	if (p < 0 || p > VLINES)
		error("Internal error: vredraw@- please tell someone");
#endif
	strcpy(temp, linebuf);
	l = 0;
	tp = dot - vcline;
	while (l < vcnt && vliny[l] < p)
		l++, tp++;
	for (; l < vcnt; l++) {
		if (l == vcline)
			strcLIN(temp);
		else
			getline(*tp);
		vliny[l] = p;
		p =+ vreopen(p, tp - zero);
		tp++;
	}
	ovcline = vcline;
	vcline = l;
	for (; tp <= dol; tp++) {
		getline(*tp);
		if (p + vdepth() > VLINES)
			break;
		p =+ vopen(tp, p);
		vcline++;
	}
	vcline = ovcline;
	for (; p < LINES - 1; p++)
		vclrlin(p, tp);
	strcLIN(temp);
}

vsync(p)
	register int p;
{
	register int l, lim;
	char temp[LBSIZE];

#ifdef UNIMP
	if (allredraw) {
		vredraw(p);
		return;
	}
#endif
	strcpy(temp, linebuf);
	l = 0;
	while (l < vcnt && vliny[l] < p)
		l++;
	lim = LINES - 1;
	if (!visual && !CA)
		lim++;
	for (; p < lim; p++)
		if (l < vcnt && vliny[l] == p) {
			if (l == vcline)
				strcLIN(temp);
			else
				getline(dot[l - vcline]);
			p =+ vreopen(p, (dot - zero) + (l - vcline)) - 1;
			l++;
		} else
			vclrlin(p, dot + (l - vcline));
	strcLIN(temp);
}

/*
 * Remove (up to) cnt lines from the screen
 * starting with the line at vfirst as the
 * result of a delete or join.
 */
velide(cnt, vfirst)
	int cnt, vfirst;
{
	register int i;

	/*
	 * Can't delete more lines than there are.
	 */
	if (vcnt - vfirst < cnt)
		cnt = vcnt - vfirst;
	/*
	 * Shift the rest of the lines up in the array.
	 */
	for (i = vfirst + cnt; i <= vcnt; i++)
		vliny[i - cnt] = vliny[i];
	/*
	 * Lost cnt lines.
	 */
	vcnt =- cnt;
}

vup1()
{

	vmoveitup(1);
}
#endif
