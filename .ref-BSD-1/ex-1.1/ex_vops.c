#include "ex.h"
#ifdef VISUAL
#include "ex_tty.h"
#include "ex_vis.h"
/*
 * Ex - a text editor
 * Bill Joy UCB September 1977
 */

vdelete(c)
	char c;
{
	register char *cp;
	register int i;

	if (c == '#')
		cursor++, wcursor++;
	if (wcursor < linebuf)
		wcursor = linebuf;
	if (cursor == wcursor) {
		beep();
		return;
	}
	vdcMID();
	setDEL();
	cp = cursor;
	strcpy(cp, wcursor);
	if (cp > linebuf && (cp[0] == 0 || c == '#'))
		cp--;
	i = vliny[vcline];
	i =+ vreopen(i, dot - zero);
	while (i < vliny[vcline + 1]) {
		vclrlin(i, dot + 1);
		i++;
	}
	vsetcurs(cp);
}

vdcMID()
{
	register char *cp;

	setLAST();
	vundkind = VCHNG;
	strcpy(vutmp, linebuf);
	if (wcursor < cursor) {
		cp = wcursor;
		wcursor = cursor;
		cursor = cp;
	}
}

vgrabit()
{

	takeout(INS);
}

vyankit()
{

	takeout(DEL);
}

takeout(BUF)
	char *BUF;
{
	register char *cp;

	if (wcursor < linebuf)
		wcursor = linebuf;
	if (cursor == wcursor) {
		beep();
		return;
	}
	if (wcursor < cursor) {
		cp = wcursor;
		wcursor = cursor;
		cursor = cp;
	}
	setBUF(BUF);
	if (BUF[0] == OVERBUF)
		beep();
}

vchange()
{
	register char *cp;
	register int i;

	if (wcursor < linebuf)
		wcursor = linebuf;
	if (cursor == wcursor) {
		beep();
		return;
	}
	vdcMID();
	vfixcurs();
	cp = cursor;
	vsetcurs(wcursor - 1);
	putchar('$');
	cursor = cp;
	setDEL();
	strcpy(cursor, wcursor);
	if (*cursor == 0)
		vgotoCL(column(cursor));
	else if (*cursor == '\t')
		vgotoCL(column(cursor - 1));
	else
		vfixcurs();
	vappend('c', 1, 0);
}

char	vaifirst;

vappend(ch, cnt, indent)
	char ch;
	int cnt, indent;
{
	register int i;
	register char *gcursor;
	char escape, *ic;
	int repcnt;

	if (!visual && !CA)
		vsave();
	vaifirst = indent == 0;
	if (ch == 'r')
		repcnt = 2;
	else
		repcnt = 0;
	ic = cursor;
	switch (ch) {
		case 'c':
		case 'r':
		case 'o':
			/* roll your own */
			break;
		default:
			vundkind = VCHNG;
			strcpy(vutmp, linebuf);
			break;
	}
	if (value(AUTOINDENT) && indent != 0) {
		gcursor = genindent(indent);
		*gcursor = 0;
		vgotoCL(qcolumn(cursor - 1, genbuf));
	} else {
		gcursor = genbuf;
		*gcursor = 0;
		if (ch == 'o')
			vfixcurs();
	}
	if ((vglobp && *vglobp == 0) || peekbr()) {
		if (!INS[0] || INS[0] == OVERBUF) {
			beep();
			return;
		}
		vglobp = INS;
	} else if (vglobp == 0)
		INS[0] = 0;
	/*
	 * At the beginning of the following loop which gathers appended
	 * lines, gcursor points to the beginning of the place for the
	 * appended text.  Linebuf contains the line in which the operation
	 * is taking place, the appended text is placed starting at the
	 * position indicated by cursor.
	 */
	for (;;) {
		/*
		 * Get a line into genbuf.  Gcursor indicates that we
		 * have possibly generated some input (e.g. autoindent).
		 * If the line is terminated with a newline character
		 * then escape will be set to the newline.  The repcnt
		 * limits the number of characters appended, e.g. it is
		 * 2 to get exactly one character (i.e. for a replace)
		 * or 0 to get an arbitrary number of characters.
		 */
		if (ch == 'r' && repcnt == 0)
			escape = 0;
		else {
			gcursor = vgetline(repcnt, gcursor, &escape);
#ifdef UNIMP
			if (HADUP)
				addtext(HADZERO ? "0" : "^");
			while (CDCNT > 0) {
				addtext("\04");
				CDCNT--;
			}
#endif
			addtext(ogcursor);
		}
		repcnt = 0;
		if (!vaifirst && value(AUTOINDENT)) {
			/*
			 * Fix up the autoindent.  Fixindent gathers white
			 * space from the beginning of the line and turns
			 * it into tabs and spaces as appropriate.
			 * We reset the cursor to be the end of the line
			 * as it was when it was returned from vgetline.
			 */
			i = fixindent(indent);
			if (!hadup)
				indent = i;
			gcursor = strend(genbuf);
		}
		/*
		 * Have to cut down the count if the specified count
		 * would make a line which is too long.
		 */
		cnt = vmaxrep(ch, cnt);
		/*
		 * We save the rest of the line after the end of the
		 * appended material (at gcursor + 1).
		 * This frees up linebuf for use in saving lines, etc.
		 */
		strcpy(gcursor + 1, cursor);
		/*
		 * Catenate cnt copies of the inserted text after
		 * the material which precedes this insert.
		 */
		do {
			strcpy(cursor, genbuf);
			cursor =+ gcursor - genbuf;
		} while (--cnt > 0);
		/*
		 * If the insert didnt terminate with a new line character
		 * then we want to put the whole line back together.
		 */
		if (escape != '\n')
			strcpy(cursor, gcursor + 1);
		/*
		 * Clean up the line image on the screen.
		 * I is the first line after this lines image for use
		 * in making a new next line.
		 */
		i = vreopen(vliny[vcline], dot - zero);
		if (escape != '\n')
			break;
		addtext("\n");
		vsave();
		if (vundkind != VMANY) {
			vundkind = VMANY;
			vrescurs = ic;
			strcLIN(vutmp);
			vulines[0] = putline();
			vrescnt = 1;
			vdelcnt = 1;
			vresaddr = dot;
			getDOT();
		}
		vdelcnt++;
		cnt = 1;
		/*
		 * We wish to place the text we are "pushing ahead"
		 * in front of the append in linebuf, cursor pointing
		 * to the beginning of linebuf.  We also wish to leave
		 * in genbuf the white space implied by an autoindent
		 * or if not ai then just start the new line at the
		 * beginning of genbuf.
		 */
		if (value(AUTOINDENT)) {
			if (!hadup && vaifirst)
				indent = whitecnt(linebuf);
			vaifirst = 0;
			/*
			 * If we are generating an indent, then we
			 * don't want the white space of the pushed ahead
			 * line, preferring to generate our own.  Discard
			 * the white space of the pushed ahead material
			 * and copy the remainder back to linebuf.
			 */
			strcLIN(vpastwh(gcursor + 1));
			/*
			 * Generate indent white space and leave the
			 * cursor pointing just after it.
			 * Note that genindent doesn't make the generated
			 * white space into a string so we must do that
			 * ourselves.
			 */
			gcursor = genindent(indent);
			*gcursor = 0;
			/*
			 * If the line is too long when the generated white
			 * space is added, then just pretend ai isn't set.
			 */
			if (gcursor + strlen(linebuf) > &genbuf[510])
				gcursor = genbuf;
			/*
			 * Paste the first image of the line together.
			 */
			strcpy(gcursor, linebuf);
		} else {
			/*
			 * Without ai the first image of the new line
			 * is just the pushed ahead material, and there
			 * are no pre-inserted characters.
			 */
			strcpy(genbuf, gcursor + 1);
			gcursor = genbuf;
		}
		/*
		 * Make a new line after dot with the material in genbuf.
		 * Note that this call increments dot as a side effect,
		 * so that dot now references the new line.
		 */
		vdoappend(genbuf);
		/*
		 * Now open up the new line on the screen after
		 * the current line
		 */
		vcline++;
		if (!visual) {
			vup1();
			voinit();
		} else {
			i =+ vliny[vcline - 1];
			vopen(dot, i);
		}
		/*
		 * Put the stuff we are pushing into linebuf
		 * and set the cursor to indicate that no stuff
		 * will preceded the inserted material on the new line.
		 */
		strcLIN(gcursor);
		*gcursor = 0;
		cursor = linebuf;
		vgotoCL(qcolumn(cursor - 1, genbuf));
	}
	/*
	 * We ended an append with a non-newline.
	 * At this point the new image of the line is in linebuf
	 * and the cursor points at the desired cursor position.
	 * We have already shown this line, but must clean up the
	 * remainder of the screen.  Note that the cursor should
	 * also be set to the last character appended, so
	 * it needs to be backed up if there was such a character.
	 */
	if (cursor > linebuf)
		cursor--;
	vfixopen(i);
	vfixcurs();
}

#endif
genindent(indent)
	register int indent;
{
	register char *cp;

	for (cp = genbuf; indent >= 8; indent =- 8)
		*cp++ = '\t';
	for (; indent > 0; indent--)
		*cp++ = ' ';
	return (cp);
}

#ifdef VISUAL
fixindent(indent)
	int indent;
{
	register int i;
	register char *cp;

	i = whitecnt(genbuf);
	cp = vpastwh(genbuf);
	if (*cp == 0 && i == indent && linebuf[0] == 0) {
		genbuf[0] = 0;
		return (i);
	}
	strcpy(genindent(i), cp);
	return (i);
}
#endif

vpastwh(cp)
	register char *cp;
{

	while (white(*cp))
		cp++;
	return (cp);
}

whitecnt(cp)
	register char *cp;
{
	register int i;

	i = 0;
	for (;;)
		switch (*cp++) {
			case '\t':
				i =+ 8;
				i =& ~7;
				break;
			case ' ':
				i++;
				break;
			default:
				return (i);
		}
}

#ifdef VISUAL
vgetline(cnt, gcursor, aescaped)
	int cnt;
	register char *gcursor;
	char *aescaped;
{
	register int c;
	register char *cp;
	int x, y, iwhite;
	char *iglobp, ch;

	*aescaped = 0;
	ogcursor = gcursor;
	flusho();
#ifdef UNIMP
	CDCNT = 0;
	HADUP = 0;
	HADZERO = 0;
#endif
	hadup = 0;
	iwhite = whitecnt(genbuf);
	iglobp = vglobp;
	for (;;) {
		if (cnt != 0) {
			cnt--;
			if (cnt == 0)
				goto vadone;
		}
		ch = c = getkey();
		switch (c) {
			case DELETE:	/* interrupt */
			case FS:	/* quit */
				ungetkey(c);
				goto vadone;
			case CTRL(d):
			case CTRL(t):
				*gcursor = 0;
				cp = vpastwh(genbuf);
				c = whitecnt(genbuf);
				if (ch == CTRL(t)) {
					if (cp != gcursor)
						continue;
					cp = genindent(iwhite = backtab(c + value(SHIFTWIDTH) + 1));
					ogcursor = cp;
					goto vbackup;
				}
#ifdef UNIMP
				if (c == iwhite && c != 0)
					if (cp == gcursor) {
						iwhite = backtab(c);
						CDCNT++;
						ogcursor = cp = genindent(iwhite);
						goto vbackup;
					} else if (cp + 1 == gcursor &&
					    (*cp == '^' || *cp == '0')) {
						hadup = *cp == '^';
						ogcursor = cp = genbuf;
						HADUP = 1;
						CDCNT = 1;
						goto vbackup;
					}
				if (vglobp && vglobp - iglobp >= 2 &&
				    (vglobp[-2] == '^' || vglobp[-2] == '0')
				    && gcursor == ogcursor + 1)
					goto bakchar;
#endif
				continue;
			case CTRL(h):	/* back character */
bakchar:
				cp = gcursor - 1;
				if (cp < ogcursor) {
					beep();
					continue;
				}
				goto vbackup;
			case CTRL(w):	/* back word */
				wdkind = 0;
				for (cp = gcursor; cp > ogcursor &&
				    white(cp[-1]); cp--)
					continue;
				for (c = wordch(cp - 1); cp > ogcursor &&
				    wordof(c, cp - 1); cp--)
					continue;
				goto vbackup;
			case '@':
			case CTRL(x):
				cp = ogcursor;
vbackup:
				if (cp == gcursor) {
					beep();
					continue;
				}
				while (gcursor < cp) putchar(*gcursor++);
				*cp = 0;
				vgotoCL(qcolumn(cursor - 1, genbuf));
				gcursor = cp;
				continue;
			case CR:
				c = '\n';
			case NL:
				*aescaped = c;
				goto vadone;
			case ESCAPE:
				goto vadone;
			case '\\':
				x = destcol;
				y = destline;
				putchar('\\');
				vcsync();
				c = getkey();
				switch (c) {
					case DELETE:
					case FS:
					case CTRL(d):
					case CTRL(h):
					case CTRL(t):
					case CTRL(w):
					case '@':
					case CTRL(x):
						vgoto(y, x);
						break;
					default:
						ungetkey(c);
						c = '\\';
						goto noput;
				}
			default:
				putchar(c);
noput:
				if (gcursor > &genbuf[510])
					error("Line too long");
				*gcursor++ = c;
				vcsync();
				continue;
		}
	}
vadone:
	*gcursor = 0;
	return (gcursor);
}

int	vgetsplit();
char	*vsplitpt;

vdoappend(lp)
	char *lp;
{

	vsplitpt = lp;
	inglobal++;
	append(vgetsplit, dot);
	inglobal--;
}

vgetsplit()
{

	if (vsplitpt == 0)
		return (EOF);
	strcLIN(vsplitpt);
	vsplitpt = 0;
	return(0);
}

vmaxrep(ch, cnt)
	char ch;
	register int cnt;
{
	register int len, replen;

	if (cnt > 510)
		cnt = 510;
	replen = strlen(genbuf);
	if (ch == 'R') {
		len = strlen(cursor);
		strcpy(cursor, cursor + (replen > len ? len : replen));
	}
	len = strlen(linebuf);
	if (len + cnt * replen <= 510)
		return (cnt);
	cnt = (510 - len) / replen;
	if (cnt == 0) {
		vsave();
		error("Line too long");
	}
	return (cnt);
}

vmove()
{

	vsetcurs(wcursor);
}
#endif
