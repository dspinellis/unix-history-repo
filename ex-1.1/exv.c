#include "ex.h"
#include "ex_vis.h"
#include "ex_tty.h"
/*
 * Ex - a text editor
 * Bill Joy UCB June 1977
 */

vop()
{
#ifndef VISUAL
	error("No visual in this version");
}
#endif
#ifdef VISUAL
	register int c;
	char atube[TUBESIZE + LBSIZE], *ic;
	int lines;

	if (!CA)
		error("Visual requires addressible cursor");
	if (!CLEAR)
		error("Visual requires clear screen capability");
	if (!value(OPEN))
		error("Can't use visual mode unless open option is set");
	VLINES = LINES - 1;
	vok(atube);
	visual = 1;
	VCOLUMNS = COLUMNS;
	setdot();
	if (addr2 == zero && dol > zero)
		addr1 = one, addr2 = one;
	nonzero();
	c = getchar();
	switch (c) {
		case '^':
			if (addr2 == one)
				error("At TOP|At top-of-file");
		case '-':
		case '.':
			vch = c;
			break;
		case '+':
			if (addr2 == dol)
				error("At EOF|At end-of-file");
			addr2++;
			vch = 0;
			break;
		default:
			ungetchar(c);
			vch = 0;
			break;
	}
	skipwh();
	ZERO = VLINES - (digit(peekchar()) ? getnum() : value(WINDOW));
	if (ZERO >= VLINES)
		ZERO = VLINES - 1;
	if (ZERO < 0)
		ZERO = 0;
	newline();
	Outchar = &vputchar;
	if (!inglobal)
		saveall();
	lines = dol - zero;
	dot = addr2;
	vmoving = 0;
	if (!value(VISUALMESSAGE) && TMODE & 077) {
		chmod(TTYNAM, TMODE &~ 077);
		diddle++;
	}
	ostart();
	do {
		ic = vinit();
		if (vcnt == 0)
			error("Can't fit any complete lines on screen");
	} while (vmain(ic) == 1);
	splitw++;
	vigoto(LINES - 1, value(PROMPT));
	vclreol();
	splitw = 0;
	ostop();
	setoutt();
	inopen = 0;
	netchHAD(lines);
	undiddle();
}

vinit()
{
	register int *tp;
	register int voy, vny;
	char *ic;
	int nvc;

	vcook();
	holdat = 1;
	vclear();
	getDOT();
	switch (vch) {
		case '^':
			dot = vback((VLINES - ZERO) - vdepth());
			getDOT();
		case '-':
			tp = vback((VLINES - ZERO) - vdepth());
			break;
		case '.':
			tp = vback((VLINES - ZERO) / 2 - vdepth());
			break;
		default:
			tp = dot;
	}
	vcline = 0;
	vcnt = 0;
	vny = ZERO;
	for (; tp <= dol; tp++) {
		voy = vny;
		getline(*tp);
		if (tp == dot) {
			nvc = vcline;
			ic = vskipwh(linebuf);
		}
		vny = voy + vdepth();
		if (vny > VLINES)
			break;
		vopen(tp, voy);
		vcline++;
	}
	vcline = nvc;
	flusho();
	holdat = 0;
	vsync(vlast);
	vraw();
	return (ic);
}

vcook()
{

	gTTY(1);
	tty[2] =& ~RAW;
	sTTY(1);
}

vraw()
{

	gTTY(1);
	tty[2] =| RAW;
	sTTY(1);
}

vok(atube)
	register char *atube;
{
	register int i;

	if (COLUMNS == 1000)
		error("Don't know enough about your terminal to use %s", Command);
	if (COLUMNS > TUBECOLS)
		error("Terminal too wide");
	if (LINES > TUBELINES || COLUMNS * LINES > TUBESIZE)
		error("Screen too large for my buffer");
	vtube0 = atube;
	for (i = 0; i < TUBELINES; i++) {
		vtube[i] = atube;
		atube =+ COLUMNS;
	}
	vutmp = atube;
	vundkind = VNONE;
	ZERO = 0;
	inopen = 1;
	vmoving = 0;
	splitw = 0;
	nextic = 0;
	if (vSCROLL == 0)
		vSCROLL = value(SCROLL);
}

#endif
undiddle()
{

	if (diddle)
		chmod(TTYNAM, TMODE);
	diddle = 0;
}
