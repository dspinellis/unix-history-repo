/* Copyright (c) 1979 Regents of the University of California */
#include "ex.h"
#include "ex_argv.h"
#include "ex_temp.h"
#include "ex_tty.h"

/*
 * Command mode subroutines implementing
 *	append, args, copy, delete, join, move, put,
 *	shift, tag, yank, z and undo
 */

bool	endline = 1;
line	*tad1;

/*
 * Append after line a lines returned by function f.
 * Be careful about intermediate states to avoid scramble
 * if an interrupt comes in.
 */
append(f, a)
	int (*f)();
	line *a;
{
	register line *a1, *a2, *rdot;
	int nline;

	nline = 0;
	dot = a;
	/*
	 * This is probably a bug, since it's different than the other tests
	 * in appendnone, delete, and deletenone. It is known to fail for
	 * the command :g/foo/r xxx (where there is one foo and the file
	 * xxx exists) and you try to undo it. I'm leaving it in for now
	 * because I'm afraid if I change it I'll break something.
	 */
	if (!inglobal && !inopen && f != getsub) {
		undap1 = undap2 = dot + 1;
		undkind = UNDCHANGE;
	}
	while ((*f)() == 0) {
		if (truedol >= endcore) {
			if (morelines() < 0) {
				if (!inglobal && f == getsub) {
					undap1 = addr1;
					undap2 = addr2 + 1;
				}
				error("Out of memory@- too many lines in file");
			}
		}
		nline++;
		a1 = truedol + 1;
		a2 = a1 + 1;
		dot++;
		undap2++;
		dol++;
		unddol++;
		truedol++;
		for (rdot = dot; a1 > rdot;)
			*--a2 = *--a1;
		*rdot = 0;
		putmark(rdot);
		if (f == gettty) {
			dirtcnt++;
			TSYNC();
		}
	}
	return (nline);
}

appendnone()
{

	if (inopen >= 0 && (inopen || !inglobal)) {
		undkind = UNDCHANGE;
		undap1 = undap2 = addr1;
	}
}

/*
 * Print out the argument list, with []'s around the current name.
 */
pargs()
{
	register char **av = argv0, *as = args0;
	register int ac;

	for (ac = 0; ac < argc0; ac++) {
		if (ac != 0)
			putchar(' ');
		if (ac + argc == argc0 - 1)
			printf("[");
		lprintf("%s", as);
		if (ac + argc == argc0 - 1)
			printf("]");
		as = av ? *++av : strend(as) + 1;
	}
	noonl();
}

/*
 * Delete lines; two cases are if we are really deleting,
 * more commonly we are just moving lines to the undo save area.
 */
delete(hush)
	bool hush;
{
	register line *a1, *a2;

	nonzero();
	if (inopen >= 0 && (inopen || !inglobal)) {
		register int (*dsavint)();

		change();
		dsavint = signal(SIGINT, SIG_IGN);
		undkind = UNDCHANGE;
		a1 = addr1;
		squish();
		a2 = addr2;
		if (a2++ != dol) {
			reverse(a1, a2);
			reverse(a2, dol + 1);
			reverse(a1, dol + 1);
		}
		dol -= a2 - a1;
		unddel = a1 - 1;
		if (a1 > dol)
			a1 = dol;
		dot = a1;
		pkill[0] = pkill[1] = 0;
		signal(SIGINT, dsavint);
	} else {
		register line *a3;
		register int i;

		change();
		a1 = addr1;
		a2 = addr2 + 1;
		a3 = truedol;
		i = a2 - a1;
		unddol -= i;
		undap2 -= i;
		dol -= i;
		truedol -= i;
		do
			*a1++ = *a2++;
		while (a2 <= a3);
		a1 = addr1;
		if (a1 > dol)
			a1 = dol;
		dot = a1;
	}
	if (!hush)
		killed();
}

deletenone()
{

	if (inopen >= 0 && (inopen || !inglobal)) {
		undkind = UNDCHANGE;
		squish();
		unddel = addr1;
	}
}

/*
 * Crush out the undo save area, moving the open/visual
 * save area down in its place.
 */
squish()
{
	register line *a1 = dol + 1, *a2 = unddol + 1, *a3 = truedol + 1;

	if (inopen == -1)
		return;
	if (a1 < a2 && a2 < a3)
		do
			*a1++ = *a2++;
		while (a2 < a3);
	truedol -= unddol - dol;
	unddol = dol;
}

/*
 * Join lines.  Special hacks put in spaces, two spaces if
 * preceding line ends with '.', or no spaces if next line starts with ).
 */
static	int jcount, jnoop();

join(c)
	int c;
{
	register line *a1;
	register char *cp, *cp1;

	cp = genbuf;
	*cp = 0;
	for (a1 = addr1; a1 <= addr2; a1++) {
		getline(*a1);
		cp1 = linebuf;
		if (a1 != addr1 && c == 0) {
			while (*cp1 == ' ' || *cp1 == '\t')
				cp1++;
			if (*cp1 && cp > genbuf && cp[-1] != ' ' && cp[-1] != '\t') {
				if (*cp1 != ')') {
					*cp++ = ' ';
					if (cp[-2] == '.')
						*cp++ = ' ';
				}
			}
		}
		while (*cp++ = *cp1++)
			if (cp > &genbuf[LBSIZE-2])
				error("Line overflow|Result line of join would be too long");
		cp--;
	}
	strcLIN(genbuf);
	delete(0);
	jcount = 1;
	ignore(append(jnoop, --addr1));
}

static
jnoop()
{

	return(--jcount);
}

/*
 * Move and copy lines.  Hard work is done by move1 which
 * is also called by undo.
 */
int	getcopy();

move()
{
	register line *adt;
	bool iscopy = 0;

	if (Command[0] == 'm') {
		setdot1();
		markpr(addr2 == dot ? addr1 - 1 : addr2 + 1);
	} else {
		iscopy++;
		setdot();
	}
	nonzero();
	adt = address(0);
	if (adt == 0)
		serror("%s where?|%s requires a trailing address", Command);
	newline();
	move1(iscopy, adt);
	killed();
}

move1(cflag, addrt)
	int cflag;
	line *addrt;
{
	register line *adt, *ad1, *ad2;
	int lines;

	adt = addrt;
	lines = (addr2 - addr1) + 1;
	if (cflag) {
		tad1 = addr1;
		ad1 = dol;
		ignore(append(getcopy, ad1++));
		ad2 = dol;
	} else {
		ad2 = addr2;
		for (ad1 = addr1; ad1 <= ad2;)
			*ad1++ &= ~01;
		ad1 = addr1;
	}
	ad2++;
	if (adt < ad1) {
		if (adt + 1 == ad1 && !cflag && !inglobal)
			error("That move would do nothing!");
		dot = adt + (ad2 - ad1);
		if (++adt != ad1) {
			reverse(adt, ad1);
			reverse(ad1, ad2);
			reverse(adt, ad2);
		}
	} else if (adt >= ad2) {
		dot = adt++;
		reverse(ad1, ad2);
		reverse(ad2, adt);
		reverse(ad1, adt);
	} else
		error("Move to a moved line");
	change();
	if (!inglobal)
		if (cflag) {
			undap1 = addrt + 1;
			undap2 = undap1 + lines;
			deletenone();
		} else {
			undkind = UNDMOVE;
			undap1 = addr1;
			undap2 = addr2;
			unddel = addrt;
			squish();
		}
}

getcopy()
{

	if (tad1 > addr2)
		return (EOF);
	getline(*tad1++);
	return (0);
}

/*
 * Put lines in the buffer from the undo save area.
 */
getput()
{

	if (tad1 > unddol)
		return (EOF);
	getline(*tad1++);
	tad1++;
	return (0);
}

put()
{
	register int cnt;

	cnt = unddol - dol;
	if (cnt && inopen && pkill[0] && pkill[1]) {
		pragged(1);
		return;
	}
	tad1 = dol + 1;
	ignore(append(getput, addr2));
	undkind = UNDPUT;
	notecnt = cnt;
	netchange(cnt);
}

/*
 * A tricky put, of a group of lines in the middle
 * of an existing line.  Only from open/visual.
 * Argument says pkills have meaning, e.g. called from
 * put; it is 0 on calls from putreg.
 */
pragged(kill)
	bool kill;
{
	extern char *cursor;
	register char *gp = &genbuf[cursor - linebuf];

	/*
	 * This kind of stuff is TECO's forte.
	 * We just grunge along, since it cuts
	 * across our line-oriented model of the world
	 * almost scrambling our addled brain.
	 */
	if (!kill)
		getDOT();
	strcpy(genbuf, linebuf);
	getline(*unddol);
	if (kill)
		*pkill[1] = 0;
	strcat(linebuf, gp);
	putmark(unddol);
	getline(dol[1]);
	if (kill)
		strcLIN(pkill[0]);
	strcpy(gp, linebuf);
	strcLIN(genbuf);
	putmark(dol+1);
	undkind = UNDCHANGE;
	undap1 = dot;
	undap2 = dot + 1;
	unddel = dot - 1;
	undo(1);
}

/*
 * Shift lines, based on c.
 * If c is neither < nor >, then this is a lisp aligning =.
 */
shift(c, cnt)
	int c;
	int cnt;
{
	register line *addr;
	register char *cp;
	char *dp;
	register int i;

	if (!inglobal)
		save12(), undkind = UNDCHANGE;
	cnt *= value(SHIFTWIDTH);
	for (addr = addr1; addr <= addr2; addr++) {
		dot = addr;
#ifdef LISPCODE
		if (c == '=' && addr == addr1 && addr != addr2)
			continue;
#endif
		getDOT();
		i = whitecnt(linebuf);
		switch (c) {

		case '>':
			if (linebuf[0] == 0)
				continue;
			cp = genindent(i + cnt);
			break;

		case '<':
			if (i == 0)
				continue;
			i -= cnt;
			cp = i > 0 ? genindent(i) : genbuf;
			break;

#ifdef LISPCODE
		default:
			i = lindent(addr);
			getDOT();
			cp = genindent(i);
			break;
#endif
		}
		if (cp + strlen(dp = vpastwh(linebuf)) >= &genbuf[LBSIZE - 2])
			error("Line too long|Result line after shift would be too long");
		CP(cp, dp);
		strcLIN(genbuf);
		putmark(addr);
	}
	killed();
}

/*
 * Find a tag in the tags file.
 * Most work here is in parsing the tags file itself.
 */
tagfind(quick)
	bool quick;
{
	char cmdbuf[BUFSIZ];
	char filebuf[FNSIZE];
	register int c, d;
	bool samef = 1;
	bool notagsfile = 0;
	short master = -1;
	short omagic;

	omagic = value(MAGIC);
	if (!skipend()) {
		register char *lp = lasttag;

		while (!iswhite(peekchar()) && !endcmd(peekchar()))
			if (lp < &lasttag[sizeof lasttag - 2])
				*lp++ = getchar();
			else
				ignchar();
		*lp++ = 0;
		if (!endcmd(peekchar()))
badtag:
			error("Bad tag|Give one tag per line");
	} else if (lasttag[0] == 0)
		error("No previous tag");
	c = getchar();
	if (!endcmd(c))
		goto badtag;
	if (c == EOF)
		ungetchar(c);
	clrstats();
	do {
		io = open(master ? "tags" : MASTERTAGS, 0);
		if (master && io < 0)
			notagsfile = 1;
		while (getfile() == 0) {
			register char *cp = linebuf;
			register char *lp = lasttag;
			char *oglobp;

			while (*cp && *lp == *cp)
				cp++, lp++;
			if (*lp || !iswhite(*cp))
				continue;
			close(io);
			while (*cp && iswhite(*cp))
				cp++;
			if (!*cp)
badtags:
				serror("%s: Bad tags file entry", lasttag);
			lp = filebuf;
			while (*cp && *cp != ' ' && *cp != '\t') {
				if (lp < &filebuf[sizeof filebuf - 2])
					*lp++ = *cp;
				cp++;
			}
			*lp++ = 0;
			if (*cp == 0)
				goto badtags;
			if (dol != zero) {
				/*
				 * Save current position in 't for ^^ in visual.
				 */
				names['t'-'a'] = *dot &~ 01;
				if (inopen) {
					extern char *ncols['z'-'a'+1];
					extern char *cursor;

					ncols['t'-'a'] = cursor;
				}
			}
			strcpy(cmdbuf, cp);
			if (strcmp(filebuf, savedfile) || !edited) {
				char cmdbuf2[sizeof filebuf + 10];

				if (!quick) {
					ckaw();
					if (chng && dol > zero)
						error("No write@since last change (:tag! overrides)");
				}
				oglobp = globp;
				strcpy(cmdbuf2, "e! ");
				strcat(cmdbuf2, filebuf);
				globp = cmdbuf2;
				d = peekc; ungetchar(0);
				/*
				 * BUG: if it isn't found (user edited header
				 * line) we get left in nomagic mode.
				 */
				value(MAGIC) = 0;
				commands(1, 1);
				peekc = d;
				globp = oglobp;
				value(MAGIC) = omagic;
				samef = 0;
			}
			oglobp = globp;
			globp = cmdbuf;
			d = peekc; ungetchar(0);
			if (samef)
				markpr(dot);
			value(MAGIC) = 0;
			commands(1, 1);
			peekc = d;
			globp = oglobp;
			value(MAGIC) = omagic;
			return;
		}
	} while (++master == 0);
	if (notagsfile)
		error("No tags file");
	serror("%s: No such tag@in tags file", lasttag);
}

/*
 * Save lines from addr1 thru addr2 as though
 * they had been deleted.
 */
yank()
{

	save12();
	undkind = UNDNONE;
	killcnt(addr2 - addr1 + 1);
}

/*
 * z command; print windows of text in the file.
 *
 * If this seems unreasonably arcane, the reasons
 * are historical.  This is one of the first commands
 * added to the first ex (then called en) and the
 * number of facilities here were the major advantage
 * of en over ed since they allowed more use to be
 * made of fast terminals w/o typing .,.22p all the time.
 */
bool	zhadpr;
bool	znoclear;
short	zweight;

zop(hadpr)
	int hadpr;
{
	register int c, lines, op;
	bool excl;

	zhadpr = hadpr;
	notempty();
	znoclear = 0;
	zweight = 0;
	excl = exclam();
	switch (c = op = getchar()) {

	case '^':
		zweight = 1;
	case '-':
	case '+':
		while (peekchar() == op) {
			ignchar();
			zweight++;
		}
	case '=':
	case '.':
		c = getchar();
		break;

	case EOF:
		znoclear++;
		break;

	default:
		op = 0;
		break;
	}
	if (isdigit(c)) {
		lines = c - '0';
		for(;;) {
			c = getchar();
			if (!isdigit(c))
				break;
			lines *= 10;
			lines += c - '0';
		}
		if (lines < LINES)
			znoclear++;
		value(WINDOW) = lines;
		if (op == '=')
			lines += 2;
	} else
		lines = op == EOF ? value(SCROLL) : excl ? LINES - 1 : 2*value(SCROLL);
	if (inopen || c != EOF) {
		ungetchar(c);
		newline();
	}
	addr1 = addr2;
	if (addr2 == 0 && dot < dol && op == 0)
		addr1 = addr2 = dot+1;
	setdot();
	zop2(lines, op);
}

zop2(lines, op)
	register int lines;
	register int op;
{
	register line *split;

	split = NULL;
	switch (op) {

	case EOF:
		if (addr2 == dol)
			error("\nAt EOF");
	case '+':
		if (addr2 == dol)
			error("At EOF");
		addr2 += lines * zweight;
		if (addr2 > dol)
			error("Hit BOTTOM");
		addr2++;
	default:
		addr1 = addr2;
		addr2 += lines-1;
		dot = addr2;
		break;

	case '=':
	case '.':
		znoclear = 0;
		lines--;
		lines >>= 1;
		if (op == '=')
			lines--;
		addr1 = addr2 - lines;
		if (op == '=')
			dot = split = addr2;
		addr2 += lines;
		if (op == '.') {
			markDOT();
			dot = addr2;
		}
		break;

	case '^':
	case '-':
		addr2 -= lines * zweight;
		if (addr2 < one)
			error("Hit TOP");
		lines--;
		addr1 = addr2 - lines;
		dot = addr2;
		break;
	}
	if (addr1 <= zero)
		addr1 = one;
	if (addr2 > dol)
		addr2 = dol;
	if (dot > dol)
		dot = dol;
	if (addr1 > addr2)
		return;
	if (op == EOF && zhadpr) {
		getline(*addr1);
		putchar('\r' | QUOTE);
		shudclob = 1;
	} else if (znoclear == 0 && CL != NOSTR && !inopen) {
		flush1();
		vclear();
	}
	if (addr2 - addr1 > 1)
		pstart();
	if (split) {
		plines(addr1, split - 1, 0);
		splitit();
		plines(split, split, 0);
		splitit();
		addr1 = split + 1;
	}
	plines(addr1, addr2, 0);
}

static
splitit()
{
	register int l;

	for (l = COLUMNS > 80 ? 40 : COLUMNS / 2; l > 0; l--)
		putchar('-');
	putnl();
}

plines(adr1, adr2, movedot)
	line *adr1;
	register line *adr2;
	bool movedot;
{
	register line *addr;

	pofix();
	for (addr = adr1; addr <= adr2; addr++) {
		getline(*addr);
		pline(lineno(addr));
		if (inopen)
			putchar('\n' | QUOTE);
		if (movedot)
			dot = addr;
	}
}

pofix()
{

	if (inopen && Outchar != termchar) {
		vnfl();
		setoutt();
	}
}

/*
 * Dudley doright to the rescue.
 * Undo saves the day again.
 * A tip of the hatlo hat to Warren Teitleman
 * who made undo as useful as do.
 *
 * Command level undo works easily because
 * the editor has a unique temporary file
 * index for every line which ever existed.
 * We don't have to save large blocks of text,
 * only the indices which are small.  We do this
 * by moving them to after the last line in the
 * line buffer array, and marking down info
 * about whence they came.
 *
 * Undo is its own inverse.
 */
undo(c)
	bool c;
{
	register int i;
	register line *jp, *kp;
	line *dolp1, *newdol, *newadot;

	if (inglobal && inopen <= 0)
		error("Can't undo in global@commands");
	if (!c)
		somechange();
	pkill[0] = pkill[1] = 0;
	change();
	if (undkind == UNDMOVE) {
 		/*
		 * Command to be undone is a move command.
		 * This is handled as a special case by noting that
		 * a move "a,b m c" can be inverted by another move.
		 */
		if ((i = (jp = unddel) - undap2) > 0) {
			/*
			 * when c > b inverse is a+(c-b),c m a-1
			 */
			addr2 = jp;
			addr1 = (jp = undap1) + i;
			unddel = jp-1;
		} else {
			/*
			 * when b > c inverse is  c+1,c+1+(b-a) m b
			 */
			addr1 = ++jp;
			addr2 = jp + ((unddel = undap2) - undap1);
		}
		kp = undap1;
		move1(0, unddel);
		dot = kp;
		Command = "move";
		killed();
	} else {
		int cnt;

		newadot = dot;
		cnt = lineDOL();
		newdol = dol;
		dolp1 = dol + 1;
		/*
		 * Command to be undone is a non-move.
		 * All such commands are treated as a combination of
		 * a delete command and a append command.
		 * We first move the lines appended by the last command
		 * from undap1 to undap2-1 so that they are just before the
		 * saved deleted lines.
		 */
		if ((i = (kp = undap2) - (jp = undap1)) > 0) {
			if (kp != dolp1) {
				reverse(jp, kp);
				reverse(kp, dolp1);
				reverse(jp, dolp1);
			}
			/*
			 * Account for possible backward motion of target
			 * for restoration of saved deleted lines.
			 */
			if (unddel >= jp)
				unddel -= i;
			newdol -= i;
			/*
			 * For the case where no lines are restored, dot
			 * is the line before the first line deleted.
			 */
			dot = jp-1;
		}
		/*
		 * Now put the deleted lines, if any, back where they were.
		 * Basic operation is: dol+1,unddol m unddel
		 */
		if (undkind == UNDPUT) {
			unddel = undap1 - 1;
			squish();
		}
		jp = unddel + 1;
		if ((i = (kp = unddol) - dol) > 0) {
			if (jp != dolp1) {
				reverse(jp, dolp1);
				reverse(dolp1, ++kp);
				reverse(jp, kp);
			}
			/*
			 * Account for possible forward motion of the target
			 * for restoration of the deleted lines.
			 */
			if (undap1 >= jp)
				undap1 += i;
			/*
			 * Dot is the first resurrected line.
			 */
			dot = jp;
			newdol += i;
		}
		/*
		 * Clean up so we are invertible
		 */
		unddel = undap1 - 1;
		undap1 = jp;
		undap2 = jp + i;
		dol = newdol;
		netchHAD(cnt);
		if (undkind == UNDALL) {
			dot = undadot;
			undadot = newadot;
		}
		undkind = UNDCHANGE;
	}
	if (dot == zero && dot != dol)
		dot = one;
}

/*
 * Be (almost completely) sure there really
 * was a change, before claiming to undo.
 */
somechange()
{
	register line *ip, *jp;

	switch (undkind) {

	case UNDMOVE:
		return;

	case UNDCHANGE:
		if (undap1 == undap2 && dol == unddol)
			break;
		return;

	case UNDPUT:
		if (undap1 != undap2)
			return;
		break;

	case UNDALL:
		if (unddol - dol != lineDOL())
			return;
		for (ip = one, jp = dol + 1; ip <= dol; ip++, jp++)
			if ((*ip &~ 01) != (*jp &~ 01))
				return;
		break;

	case UNDNONE:
		error("Nothing to undo");
	}
	error("Nothing changed|Last undoable command didn't change anything");
}

/*
 * Map command:
 * map src dest
 */
mapcmd(un)
	int un;	/* true if this is unmap command */
{
	char lhs[10], rhs[100];	/* max sizes resp. */
	register char *p;
	register char c;
	char *dname;

	if (skipend()) {
		int i;

		/* print current mapping values */
		if (peekchar() != EOF)
			ignchar();
		if (inopen)
			pofix();
		for (i=0; arrows[i].mapto; i++)
			if (arrows[i].cap) {
				lprintf("%s", arrows[i].descr);
				putchar('\t');
				lprintf("%s", arrows[i].cap);
				putchar('\t');
				lprintf("%s", arrows[i].mapto);
				putNFL();
			}
		return;
	}

	ignore(skipwh());
	for (p=lhs; ; ) {
		c = getchar();
		if (c == CTRL(v)) {
			c = getchar();
		} else if (any(c, " \t")) {
			if (un)
				eol();	/* will usually cause an error */
			else
				break;
		} else if (endcmd(c)) {
			ungetchar(c);
			if (un) {
				newline();
				addmac(lhs, NOSTR, NOSTR);
				return;
			} else
				error("Missing rhs");
		}
		*p++ = c;
	}
	*p = 0;

	if (skipend())
		error("Missing rhs");
	for (p=rhs; ; ) {
		c = getchar();
		if (c == CTRL(v)) {
			c = getchar();
		} else if (endcmd(c)) {
			ungetchar(c);
			break;
		}
		*p++ = c;
	}
	*p = 0;
	newline();
	/*
	 * Special hack for function keys: #1 means key f1, etc.
	 * If the terminal doesn't have function keys, we just use #1.
	 */
	if (lhs[0] == '#') {
		char *fnkey;
		char *fkey();
		char funkey[3];

		fnkey = fkey(lhs[1] - '0');
		funkey[0] = 'f'; funkey[1] = lhs[1]; funkey[2] = 0;
		if (fnkey)
			strcpy(lhs, fnkey);
		dname = funkey;
	} else {
		dname = lhs;
	}
	addmac(lhs,rhs,dname);
}

/*
 * Add a macro definition to those that already exist. The sequence of
 * chars "src" is mapped into "dest". If src is already mapped into something
 * this overrides the mapping. There is no recursion. Unmap is done by
 * using NOSTR for dest.
 */
addmac(src,dest,dname)
	register char *src, *dest, *dname;
{
	register int slot, zer;

	if (dest) {
		/* Make sure user doesn't screw himself */
		/*
		 * Prevent tail recursion. We really should be
		 * checking to see if src is a suffix of dest
		 * but we are too lazy here, so we don't bother unless
		 * src is only 1 char long.
		 */
		if (src[1] == 0 && src[0] == dest[strlen(dest)-1])
			error("No tail recursion");
		/*
		 * We don't let the user rob himself of ":", and making
		 * multi char words is a bad idea so we don't allow it.
		 * Note that if user sets mapinput and maps all of return,
		 * linefeed, and escape, he can screw himself. This is
		 * so weird I don't bother to check for it.
		 */
		if (isalpha(src[0]) && src[1] || any(src[0],":"))
			error("Too dangerous to map that");
		/*
		 * If the src were null it would cause the dest to
		 * be mapped always forever. This is not good.
		 */
		if (src[0] == 0)
			error("Null lhs");
	}

	/* see if we already have a def for src */
	zer = -1;
	for (slot=0; arrows[slot].mapto; slot++) {
		if (arrows[slot].cap) {
			if (eq(src, arrows[slot].cap))
				break;	/* if so, reuse slot */
		} else {
			zer = slot;	/* remember an empty slot */
		}
	}

	if (dest == NOSTR) {
		/* unmap */
		if (arrows[slot].cap) {
			arrows[slot].cap = NOSTR;
			arrows[slot].descr = NOSTR;
		} else {
			error("Not mapped|That macro wasn't mapped");
		}
		return;
	}

	/* reuse empty slot, if we found one and src isn't already defined */
	if (zer >= 0 && arrows[slot].mapto == 0)
		slot = zer;

	/* if not, append to end */
	if (slot >= MAXNOMACS)
		error("Too many macros");
	if (msnext == 0)	/* first time */
		msnext = mapspace;
	/* Check is a bit conservative, we charge for dname even if reusing src */
	if (msnext - mapspace + strlen(dest) + strlen(src) + strlen(dname) + 3 > MAXCHARMACS)
		error("Too much macro text");
	CP(msnext, src);
	arrows[slot].cap = msnext;
	msnext += strlen(src) + 1;	/* plus 1 for null on the end */
	CP(msnext, dest);
	arrows[slot].mapto = msnext;
	msnext += strlen(dest) + 1;
	if (dname) {
		CP(msnext, dname);
		arrows[slot].descr = msnext;
		msnext += strlen(dname) + 1;
	} else {
		/* default descr to string user enters */
		arrows[slot].descr = src;
	}
}

/*
 * Implements macros from command mode. c is the buffer to
 * get the macro from.
 */
cmdmac(c)
char c;
{
	char macbuf[BUFSIZ];
	line *ad, *a1, *a2;
	char *oglobp;
	char pk;
	bool oinglobal;

	lastmac = c;
	oglobp = globp;
	oinglobal = inglobal;
	pk = peekc; peekc = 0;
	if (inglobal < 2)
		inglobal = 1;
	regbuf(c, macbuf, sizeof(macbuf));
	a1 = addr1; a2 = addr2;
	for (ad=a1; ad<=a2; ad++) {
		globp = macbuf;
		dot = ad;
		commands(1,1);
	}
	globp = oglobp;
	inglobal = oinglobal;
	peekc = pk;
}
