#include "ex.h"
#ifdef VISUAL
#include "ex_tty.h"
#include "ex_vis.h"
/*
 * Ex - a text editor
 * Bill Joy UCB September 1977
 */

#define	blank()	white(wcursor[0])

int beep(), vmove(), vdelete(), vchange(), vyankit(), vgrabit();

operate(c, cnt)
	register int c, cnt;
{
	register int i;
	int (*moveop)(), (*deleteop)(), listchar();
	register int (*op)();
	extern int (*Putchar)();
	char subop;
	static char lastFKND, lastFCHR;

	moveop = vmove;
	deleteop = vdelete;
	wcursor = cursor;
	dir = 1;
	subop = 0;
	switch (c) {
		case '@':
		case CTRL(x):
			wcursor = linebuf;
			vdelete('@');
			return;
		case 'd':
			moveop = vdelete;
			deleteop = beep;
			break;
		case 's':
			ungetkey(' ');
			subop++;
		case 'c':
			if (c == 'c' && workcmd[0] == 'C')
				subop++;
			moveop = vchange;
			deleteop = beep;
			break;
		case 'y':
			moveop = vyankit;
			deleteop = beep;
			break;
		case 'g':
			moveop = vgrabit;
			deleteop = beep;
			break;
		case 'r':
			if (cnt > strlen(cursor))
				goto errlab;
			if (*cursor == '\t' || Putchar == &listchar)
				vgotoCL(column(cursor - 1));
			c = getesc();
			if (c == 0) {
				vfixcurs();
				return;
			}
			ungetkey(c);
			strcpy(vutmp, linebuf);
			vundkind = VCHNG;
			wcursor = cursor + cnt;
			setLAST();
			strcpy(cursor, wcursor);
			vappend('r', cnt, 0);
			*lastcp++ = c;
			setLAST();
			return;
		default:
			goto nocount;
	}
	if (digit(peekkey()) && peekkey() != '0') {
		cnt =* vgetcnt();
		Xcnt = cnt;
		if (cnt < 0) {
			beep();
			return;
		}
	}
	c = getesc();
	if (c == 0)
		return;
	if (!subop)
		*lastcp++ = c;
	if (NDSPACE && NDSPACE[0] == c && NDSPACE[1] == 0)
		goto space;
nocount:
	switch (c) {
		case CTRL(w):
			c = 'B';
		case 'b':
		case 'B':
			dir = -1;
		case 'W':
		case 'w':
			wdkind = c & ' ';
			op = moveop;
			if (edge())
				goto errlab;
			while (cnt > 0 && !edge()) {
				word(op, cnt);
				cnt--;
			}
			break;
#ifdef UNIMP
		case 'E':
			dir = -1;
		case 'e':
			wdkind = 1;
			op = moveop;
			if (edge())
				goto errlab;
			while (cnt > 1 && !edge()) {
				word(op, cnt);
				cnt--;
			}
			eend(op, cnt);
			break;
#endif
		case '0':
			wcursor = linebuf;
			op = moveop;
			break;
		case ';':
			if (lastFKND == 0) {
				beep();
				return;
			}
			c = lastFKND;
			ungetkey(lastFCHR);
			subop++;
			goto nocount;
		case 'F':	/* inverted find */
		case 'T':
			dir = -1;
		case 'f':	/* find */
		case 't':
			i = getesc();
			if (i == 0)
				return;
			if (!subop)
				*lastcp++ = i;
			lastFKND = c;
			lastFCHR = i;
			while (cnt > 0) {
				if (find(i) == 0)
					goto errlab;
				cnt--;
			}
			switch (c) {
				case 'T':
					wcursor++;
					break;
				case 't':
					wcursor--;
				case 'f':
fixup:
					if (moveop != vmove)
						wcursor++;
					break;
			}
			op = moveop;
			break;
		case '|':
			if (Xhadcnt) {
				if (Pline == &numbline)
					cnt =+ 8;
				vmovcol = cnt;
			}
			vmoving = 1;
			wcursor = vfindcol(cnt);
			op = moveop;
			break;
		case '^':
			wcursor = vskipwh(linebuf);
			op = moveop;
			break;
		case '$':
			wcursor = strend(linebuf) - 1;
			goto fixup;
		case 'h':
		case CTRL(h):
			dir = -1;
		case ' ':
space:
		case 'l':
			op = moveop;
			if (margin() || op == &vmove && edge())
				goto errlab;
moveit:
			while (cnt > 0 && !margin()) {
				wcursor =+ dir;
				cnt--;
			}
			if (margin() && op == &vmove || wcursor < linebuf)
				wcursor =- dir;
			break;
		case 'D':
			cnt = INF;
			goto deleteit;
		case 'X':	/* inverted delete */
		case '#':	/* "oex" delete */
			dir = -1;
deleteit:
		case 'x':	/* delete */
			if (margin())
				goto errlab;
			while (cnt > 0 && !margin()) {
				wcursor =+ dir;
				cnt--;
			}
			op = deleteop;
			break;
		default:
errlab:
			beep();
			return;
	}
	(*op)(c);
}

find(c)
	char c;
{

	for(;;) {
		if (edge())
			return (0);
		wcursor =+ dir;
		if (*wcursor == c)
			return (1);
	}
}

word(op, cnt)
	register int (*op)();
	int cnt;
{
	register int which;
	register char *iwc;

	if (dir == 1) {
		iwc = wcursor;
		/*
		 * Word going forward.
		 * Determine whether the character under
		 * the cursor is a "word" character.
		 * If it is, skip through such
		 * else through nonesuch.
		 */
		which = wordch(wcursor);
		while (!margin() && wordof(which, wcursor))
			wcursor++;
		/*
		 * Unless this the last segment of a change
		 * we want to skip blanks.
		 */
		if (op != vchange || cnt > 1)
			while (!margin() && blank())
				wcursor++;
		else
			if (wcursor == iwc && *iwc)
				wcursor++;
		/*
		 * Can't move off end
		 */
		if (op == vmove && margin())
			wcursor--;
	} else {
		/*
		 * Word going backwards.
		 * First skip through blanks, then through word
		 * or non-word characters.
		 */
		wcursor--;
		while (!margin() && blank())
			wcursor--;
		if (!margin()) {
			which = wordch(wcursor);
			while (!margin() && wordof(which, wcursor))
				wcursor--;
		}
		if (margin() || !wordof(which, wcursor))
			wcursor++;
	}
}

#ifdef UNIMP
eend(op, cnt)
	register int (*op)();
	int cnt;
{
	register int which;
	register char *iwc;

	if (dir == 1) {
		if (!margin())
			wcursor++;
		while (!margin() && blank())
			wcursor++;
		which = wordch(wcursor);
		while (!margin() && wordof(which, wcursor))
			wcursor++;
		if (cnt == 1 && op != &vchange && op != &vdelete)
			wcursor--;
	} else {
		if (!blank()) {
			which = wordch(wcursor);
			while (!margin() && wordof(which, wcursor))
				wcursor--;
		}
		while (!margin() && blank())
			wcursor--;
		if (margin())
			wcursor++;
	}
}
#endif
		
wordof(which, wcursor)
	char which, *wcursor;
{

	if (white(*wcursor))
		return (0);
	return (!wdkind || wordch(wcursor) == which);
}

wordch(wcursor)
	char *wcursor;
{
	register int c;

	c = wcursor[0];
	return (letter(c) || digit(c) || c == '_');
}

edge()
{

	if (linebuf[0] == 0)
		return (1);
	if (dir == 1)
		return (wcursor[1] == 0);
	else
		return (wcursor == linebuf);
}

margin()
{

	return (wcursor < linebuf || wcursor[0] == 0);
}

beep()
{
	vputc('\07');
}
#endif
