#include "ex.h"
#ifdef VISUAL
#include "ex_vis.h"
/*
 * Ex - a text editor
 * Bill Joy UCB
 * Version 1.1 November, 1977
 */


setLAST()
{

	if (vglobp)
		return;
	lasthad = Xhadcnt;
	lastcnt = Xcnt;
	*lastcp = 0;
	strcpy(lastcmd, workcmd);
}

char	xtree, hadx;

setwork(c)
	char c;
{

	if (vglobp) {
		if (hadx)
			hadx--;
		return;
	}
	lastcp = workcmd;
	if (hadx && --hadx == 0)
		c = xtree;
	*lastcp++ = c;
}

xtrey(c)
	char c;
{

	hadx = 2;
	xtree = c;
}

killglob()
{

	if (hadx)
		return;
	vglobp = 0;
}
addtext(cp)
	char *cp;
{

	if (vglobp)
		return;
	addto(INS, cp);
	if (INS[0] == OVERBUF)
		lastcmd[0] = 0;
}

setDEL()
{

	setBUF(DEL);
}

setBUF(BUF)
	char *BUF;
{
	char c;

	c = *wcursor;
	*wcursor = 0;
	BUF[0] = 0;
	addto(BUF, cursor);
	*wcursor = c;
	if (BUF == DEL)
		vyancnt = 0;
}

addto(buf, str)
	register char *buf, *str;
{

	if (buf[0] == OVERBUF)
		return;
	if (strlen(buf) + strlen(str) + 1 >= BUFSIZE) {
		buf[0] = OVERBUF;
		return;
	}
	strcat(buf, str);
}
#endif
