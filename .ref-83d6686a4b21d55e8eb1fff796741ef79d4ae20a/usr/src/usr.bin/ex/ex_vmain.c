/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char *sccsid = "@(#)ex_vmain.c	7.10 (Berkeley) %G%";
#endif not lint

#include "ex.h"
#include "ex_tty.h"
#include "ex_vis.h"

/*
 * This is the main routine for visual.
 * We here decode the count and possible named buffer specification
 * preceding a command and interpret a few of the commands.
 * Commands which involve a target (i.e. an operator) are decoded
 * in the routine operate in ex_voperate.c.
 */

#define	forbid(a)	{ if (a) goto fonfon; }

vmain()
{
	register int c, cnt, i;
	char esave[TUBECOLS];
	char *oglobp;
	short d;
	line *addr;
	int ind, nlput;
	int shouldpo = 0;
	int onumber, olist, (*OPline)(), (*OPutchar)();

	vch_mac = VC_NOTINMAC;

	/*
	 * If we started as a vi command (on the command line)
	 * then go process initial commands (recover, next or tag).
	 */
	if (initev) {
		oglobp = globp;
		globp = initev;
		hadcnt = cnt = 0;
		i = tchng;
		addr = dot;
		goto doinit;
	}

	/*
	 * NB:
	 *
	 * The current line is always in the line buffer linebuf,
	 * and the cursor at the position cursor.  You should do
	 * a vsave() before moving off the line to make sure the disk
	 * copy is updated if it has changed, and a getDOT() to get
	 * the line back if you mung linebuf.  The motion
	 * routines in ex_vwind.c handle most of this.
	 */
	for (;;) {
		/*
		 * Decode a visual command.
		 * First sync the temp file if there has been a reasonable
		 * amount of change.  Clear state for decoding of next
		 * command.
		 */
		TSYNC();
		vglobp = 0;
		vreg = 0;
		hold = 0;
		seenprompt = 1;
		wcursor = 0;
		Xhadcnt = hadcnt = 0;
		Xcnt = cnt = 1;
		splitw = 0;
		if (i = holdupd) {
			if (state == VISUAL)
				ignore(peekkey());
			holdupd = 0;
/*
			if (LINE(0) < ex_ZERO) {
				vclear();
				vcnt = 0;
				i = 3;
			}
*/
			if (state != VISUAL) {
				vcnt = 0;
				vsave();
				vrepaint(cursor);
			} else if (i == 3)
				vredraw(WTOP);
			else
				vsync(WTOP);
			vfixcurs();
		}

		/*
		 * Gobble up counts and named buffer specifications.
		 */
		for (;;) {
looptop:
#ifdef MDEBUG
			if (trace)
				fprintf(trace, "pc=%c",peekkey());
#endif
			if (isdigit(peekkey()) && peekkey() != '0') {
				hadcnt = 1;
				cnt = vgetcnt();
				forbid (cnt <= 0);
			}
			if (peekkey() != '"')
				break;
			ignore(getkey()), c = getkey();
			/*
			 * Buffer names be letters or digits.
			 * But not '0' as that is the source of
			 * an 'empty' named buffer spec in the routine
			 * kshift (see ex_temp.c).
			 */
			forbid (c == '0' || !isalpha(c) && !isdigit(c));
			vreg = c;
		}
reread:
		/*
		 * Come to reread from below after some macro expansions.
		 * The call to map allows use of function key pads
		 * by performing a terminal dependent mapping of inputs.
		 */
#ifdef MDEBUG
		if (trace)
			fprintf(trace,"pcb=%c,",peekkey());
#endif
		op = getkey();
		maphopcnt = 0;
		do {
			/*
			 * Keep mapping the char as long as it changes.
			 * This allows for double mappings, e.g., q to #,
			 * #1 to something else.
			 */
			c = op;
			op = map(c,arrows);
#ifdef MDEBUG
			if (trace)
				fprintf(trace,"pca=%c,",c);
#endif
			/*
			 * Maybe the mapped to char is a count. If so, we have
			 * to go back to the "for" to interpret it. Likewise
			 * for a buffer name.
			 */
			if ((isdigit(c) && c!='0') || c == '"') {
				ungetkey(c);
				goto looptop;
			}
			if (!value(REMAP)) {
				c = op;
				break;
			}
			if (++maphopcnt > 256)
				error("Infinite macro loop");
		} while (c != op);

		/*
		 * Begin to build an image of this command for possible
		 * later repeat in the buffer workcmd.  It will be copied
		 * to lastcmd by the routine setLAST
		 * if/when completely specified.
		 */
		lastcp = workcmd;
		if (!vglobp)
			*lastcp++ = c;

		/*
		 * First level command decode.
		 */
		switch (c) {

		/*
		 * ^L		Clear screen e.g. after transmission error.
		 */

		/*
		 * ^R		Retype screen, getting rid of @ lines.
		 *		If in open, equivalent to ^L.
		 *		On terminals where the right arrow key sends
		 *		^L we make ^R act like ^L, since there is no
		 *		way to get ^L.  These terminals (adm31, tvi)
		 *		are intelligent so ^R is useless.  Soroc
		 *		will probably foul this up, but nobody has
		 *		one of them.
		 */
		case CTRL('l'):
		case CTRL('r'):
			if (c == CTRL('l') || (KR && *KR==CTRL('l'))) {
				vclear();
				vdirty(0, vcnt);
			}
			if (state != VISUAL) {
				/*
				 * Get a clean line, throw away the
				 * memory of what is displayed now,
				 * and move back onto the current line.
				 */
				vclean();
				vcnt = 0;
				vmoveto(dot, cursor, 0);
				continue;
			}
			vredraw(WTOP);
			/*
			 * Weird glitch -- when we enter visual
			 * in a very small window we may end up with
			 * no lines on the screen because the line
			 * at the top is too long.  This forces the screen
			 * to be expanded to make room for it (after
			 * we have printed @'s ick showing we goofed).
			 */
			if (vcnt == 0)
				vrepaint(cursor);
			vfixcurs();
			continue;

		/*
		 * $		Escape just cancels the current command
		 *		with a little feedback.
		 */
		case ESCAPE:
			beep();
			continue;

		/*
		 * @   		Macros. Bring in the macro and put it
		 *		in vmacbuf, point vglobp there and punt.
		 */
		 case '@':
			c = getesc();
			if (c == 0)
				continue;
			if (c == '@')
				c = lastmac;
			if (isupper(c))
				c = tolower(c);
			forbid(!islower(c));
			lastmac = c;
			vsave();
			CATCH
				char tmpbuf[BUFSIZ];

				regbuf(c,tmpbuf,sizeof(vmacbuf));
				macpush(tmpbuf, 1);
			ONERR
				lastmac = 0;
				splitw = 0;
				getDOT();
				vrepaint(cursor);
				continue;
			ENDCATCH
			vmacp = vmacbuf;
			goto reread;

		/*
		 * .		Repeat the last (modifying) open/visual command.
		 */
		case '.':
			/*
			 * Check that there was a last command, and
			 * take its count and named buffer unless they
			 * were given anew.  Special case if last command
			 * referenced a numeric named buffer -- increment
			 * the number and go to a named buffer again.
			 * This allows a sequence like "1pu.u.u...
			 * to successively look for stuff in the kill chain
			 * much as one does in EMACS with C-Y and M-Y.
			 */
			forbid (lastcmd[0] == 0);
			if (hadcnt)
				lastcnt = cnt;
			if (vreg)
				lastreg = vreg;
			else if (isdigit(lastreg) && lastreg < '9')
				lastreg++;
			vreg = lastreg;
			cnt = lastcnt;
			hadcnt = lasthad;
			vglobp = lastcmd;
			goto reread;

		/*
		 * ^U		Scroll up.  A count sticks around for
		 *		future scrolls as the scroll amount.
		 *		Attempt to hold the indentation from the
		 *		top of the screen (in logical lines).
		 *
		 * BUG:		A ^U near the bottom of the screen
		 *		on a dumb terminal (which can't roll back)
		 *		causes the screen to be cleared and then
		 *		redrawn almost as it was.  In this case
		 *		one should simply move the cursor.
		 */
		case CTRL('u'):
			if (hadcnt)
				ex_vSCROLL = cnt;
			cnt = ex_vSCROLL;
			if (state == VISUAL)
				ind = vcline, cnt += ind;
			else
				ind = 0;
			vmoving = 0;
			vup(cnt, ind, 1);
			vnline(NOSTR);
			continue;

		/*
		 * ^D		Scroll down.  Like scroll up.
		 */
		case CTRL('d'):
#ifdef TRACE
		if (trace)
			fprintf(trace, "before vdown in ^D, dot=%d, wdot=%d, dol=%d\n", lineno(dot), lineno(wdot), lineno(dol));
#endif
			if (hadcnt)
				ex_vSCROLL = cnt;
			cnt = ex_vSCROLL;
			if (state == VISUAL)
				ind = vcnt - vcline - 1, cnt += ind;
			else
				ind = 0;
			vmoving = 0;
			vdown(cnt, ind, 1);
#ifdef TRACE
		if (trace)
			fprintf(trace, "before vnline in ^D, dot=%d, wdot=%d, dol=%d\n", lineno(dot), lineno(wdot), lineno(dol));
#endif
			vnline(NOSTR);
#ifdef TRACE
		if (trace)
			fprintf(trace, "after vnline in ^D, dot=%d, wdot=%d, dol=%d\n", lineno(dot), lineno(wdot), lineno(dol));
#endif
			continue;

		/*
		 * ^E		Glitch the screen down (one) line.
		 *		Cursor left on same line in file.
		 */
		case CTRL('e'):
			if (state != VISUAL)
				continue;
			if (!hadcnt)
				cnt = 1;
			/* Bottom line of file already on screen */
			forbid(lineDOL()-lineDOT() <= vcnt-1-vcline);
			ind = vcnt - vcline - 1 + cnt;
			vdown(ind, ind, 1);
			vnline(cursor);
			continue;

		/*
		 * ^Y		Like ^E but up
		 */
		case CTRL('y'):
			if (state != VISUAL)
				continue;
			if (!hadcnt)
				cnt = 1;
			forbid(lineDOT()-1<=vcline); /* line 1 already there */
			ind = vcline + cnt;
			vup(ind, ind, 1);
			vnline(cursor);
			continue;


		/*
		 * m		Mark position in mark register given
		 *		by following letter.  Return is
		 *		accomplished via ' or `; former
		 *		to beginning of line where mark
		 *		was set, latter to column where marked.
		 */
		case 'm':
			/*
			 * Getesc is generally used when a character
			 * is read as a latter part of a command
			 * to allow one to hit rubout/escape to cancel
			 * what you have typed so far.  These characters
			 * are mapped to 0 by the subroutine.
			 */
			c = getesc();
			if (c == 0)
				continue;

			/*
			 * Markreg checks that argument is a letter
			 * and also maps ' and ` to the end of the range
			 * to allow '' or `` to reference the previous
			 * context mark.
			 */
			c = markreg(c);
			forbid (c == 0);
			vsave();
			names[c - 'a'] = (*dot &~ 01);
			ncols[c - 'a'] = cursor;
			anymarks = 1;
			continue;

		/*
		 * ^F		Window forwards, with 2 lines of continuity.
		 *		Count repeats.
		 */
		case CTRL('f'):
			vsave();
			if (vcnt > 2) {
				addr = dot + (vcnt - vcline) - 2 + (cnt-1)*basWLINES;
				forbid(addr > dol);
				dot = addr;
				vcnt = vcline = 0;
			}
			vzop(0, 0, '+');
			continue;

		/*
		 * ^B		Window backwards, with 2 lines of continuity.
		 *		Inverse of ^F.
		 */
		case CTRL('b'):
			vsave();
			if (one + vcline != dot && vcnt > 2) {
				addr = dot - vcline + 2 - (cnt-1)*basWLINES;
				forbid (addr <= zero);
				dot = addr;
				vcnt = vcline = 0;
			}
			vzop(0, 0, '^');
			continue;

		/*
		 * z		Screen adjustment, taking a following character:
		 *			z<CR>		current line to top
		 *			z<NL>		like z<CR>
		 *			z-		current line to bottom
		 *		also z+, z^ like ^F and ^B.
		 *		A preceding count is line to use rather
		 *		than current line.  A count between z and
		 *		specifier character changes the screen size
		 *		for the redraw.
		 *
		 */
		case 'z':
			if (state == VISUAL) {
				i = vgetcnt();
				if (i > 0)
					vsetsiz(i);
				c = getesc();
				if (c == 0)
					continue;
			}
			vsave();
			vzop(hadcnt, cnt, c);
			continue;

		/*
		 * Y		Yank lines, abbreviation for y_ or yy.
		 *		Yanked lines can be put later if no
		 *		changes intervene, or can be put in named
		 *		buffers and put anytime in this session.
		 */
		case 'Y':
			ungetkey('_');
			c = 'y';
			break;

		/*
		 * J		Join lines, 2 by default.  Count is number
		 *		of lines to join (no join operator sorry.)
		 */
		case 'J':
			forbid (dot == dol);
			if (cnt == 1)
				cnt = 2;
			if (cnt > (i = dol - dot + 1))
				cnt = i;
			vsave();
			vmacchng(1);
			setLAST();
			cursor = strend(linebuf);
			vremote(cnt, join, 0);
			notenam = "join";
			vmoving = 0;
			killU();
			vreplace(vcline, cnt, 1);
			if (!*cursor && cursor > linebuf)
				cursor--;
			if (notecnt == 2)
				notecnt = 0;
			vrepaint(cursor);
			continue;

		/*
		 * S		Substitute text for whole lines, abbrev for c_.
		 *		Count is number of lines to change.
		 */
		case 'S':
			ungetkey('_');
			c = 'c';
			break;

		/*
		 * O		Create a new line above current and accept new
		 *		input text, to an escape, there.
		 *		A count specifies, for dumb terminals when
		 *		slowopen is not set, the number of physical
		 *		line space to open on the screen.
		 *
		 * o		Like O, but opens lines below.
		 */
		case 'O':
		case 'o':
			vmacchng(1);
			voOpen(c, cnt);
			continue;

		/*
		 * C		Change text to end of line, short for c$.
		 */
		case 'C':
			if (*cursor) {
				ungetkey('$'), c = 'c';
				break;
			}
			goto appnd;

		/*
		 * ~	Switch case of letter under cursor
		 */
		case '~':
			{
				char mbuf[4];
				setLAST();
				mbuf[0] = 'r';
				mbuf[1] = *cursor;
				mbuf[2] = cursor[1]==0 ? 0 : 'l';
				mbuf[3] = 0;
				if (isalpha(mbuf[1]))
					mbuf[1] ^= ' ';	/* toggle the case */
				macpush(mbuf, 1);
			}
			continue;


		/*
		 * A		Append at end of line, short for $a.
		 */
		case 'A':
			operate('$', 1);
appnd:
			c = 'a';
			/* fall into ... */

		/*
		 * a		Appends text after cursor.  Text can continue
		 *		through arbitrary number of lines.
		 */
		case 'a':
			if (*cursor) {
				if (state == HARDOPEN)
					ex_putchar(*cursor);
				cursor++;
			}
			goto insrt;

		/*
		 * I		Insert at beginning of whitespace of line,
		 *		short for ^i.
		 */
		case 'I':
			operate('^', 1);
			c = 'i';
			/* fall into ... */

		/*
		 * R		Replace characters, one for one, by input
		 *		(logically), like repeated r commands.
		 *
		 * BUG:		This is like the typeover mode of many other
		 *		editors, and is only rarely useful.  Its
		 *		implementation is a hack in a low level
		 *		routine and it doesn't work very well, e.g.
		 *		you can't move around within a R, etc.
		 */
		case 'R':
			/* fall into... */

		/*
		 * i		Insert text to an escape in the buffer.
		 *		Text is arbitrary.  This command reminds of
		 *		the i command in bare teco.
		 */
		case 'i':
insrt:
			/*
			 * Common code for all the insertion commands.
			 * Save for redo, position cursor, prepare for append
			 * at command and in visual undo.  Note that nothing
			 * is doomed, unless R when all is, and save the
			 * current line in a the undo temporary buffer.
			 */
			vmacchng(1);
			setLAST();
			vcursat(cursor);
			prepapp();
			vnoapp();
			doomed = c == 'R' ? 10000 : 0;
			if(FIXUNDO)
				vundkind = VCHNG;
			vmoving = 0;
			CP(vutmp, linebuf);

			/*
			 * If this is a repeated command, then suppress
			 * fake insert mode on dumb terminals which looks
			 * ridiculous and wastes lots of time even at 9600B.
			 */
			if (vglobp)
				hold = HOLDQIK;
			vappend(c, cnt, 0);
			continue;

		/*
		 * ^?		An attention, normally a ^?, just beeps.
		 *		If you are a vi command within ex, then
		 *		two ATTN's will drop you back to command mode.
		 */
		case ATTN:
			beep();
			if (initev || peekkey() != ATTN)
				continue;
			/* fall into... */

		/*
		 * ^\		A quit always gets command mode.
		 */
		case QUIT:
			/*
			 * Have to be careful if we were called
			 *	g/xxx/vi
			 * since a return will just start up again.
			 * So we simulate an interrupt.
			 */
			if (inglobal)
				onintr();
			/* fall into... */

#ifdef notdef
		/*
		 * q		Quit back to command mode, unless called as
		 *		vi on command line in which case dont do it
		 */
		case 'q':	/* quit */
			if (initev) {
				vsave();
				CATCH
					error("Q gets ex command mode, :q leaves vi");
				ENDCATCH
				splitw = 0;
				getDOT();
				vrepaint(cursor);
				continue;
			}
#endif
			/* fall into... */

		/*
		 * Q		Is like q, but always gets to command mode
		 *		even if command line invocation was as vi.
		 */
		case 'Q':
			vsave();
			/*
			 * If we are in the middle of a macro, throw away
			 * the rest and fix up undo.
			 * This code copied from getbr().
			 */
			if (vmacp) {
				vmacp = 0;
				if (inopen == -1)	/* don't screw up undo for esc esc */
					vundkind = VMANY;
				inopen = 1;	/* restore old setting now that macro done */
			}
			return;


		/*
		 * ZZ		Like :x
		 */
		 case 'Z':
			forbid(getkey() != 'Z');
			oglobp = globp;
			globp = "x";
			vclrech(0);
			goto gogo;
			
		/*
		 * P		Put back text before cursor or before current
		 *		line.  If text was whole lines goes back
		 *		as whole lines.  If part of a single line
		 *		or parts of whole lines splits up current
		 *		line to form many new lines.
		 *		May specify a named buffer, or the delete
		 *		saving buffers 1-9.
		 *
		 * p		Like P but after rather than before.
		 */
		case 'P':
		case 'p':
			vmoving = 0;
#ifdef notdef
			forbid (!vreg && value(UNDOMACRO) && inopen < 0);
#endif
			/*
			 * If previous delete was partial line, use an
			 * append or insert to put it back so as to
			 * use insert mode on intelligent terminals.
			 */
			if (!vreg && DEL[0]) {
				forbid ((DEL[0] & (QUOTE|TRIM)) == OVERBUF);
				vglobp = DEL;
				ungetkey(c == 'p' ? 'a' : 'i');
				goto reread;
			}

			/*
			 * If a register wasn't specified, then make
			 * sure there is something to put back.
			 */
			forbid (!vreg && unddol == dol);
			/*
			 * If we just did a macro the whole buffer is in
			 * the undo save area.  We don't want to put THAT.
			 */
			forbid (vundkind == VMANY && undkind==UNDALL);
			vsave();
			vmacchng(1);
			setLAST();
			i = 0;
			if (vreg && partreg(vreg) || !vreg && pkill[0]) {
				/*
				 * Restoring multiple lines which were partial
				 * lines; will leave cursor in middle
				 * of line after shoving restored text in to
				 * split the current line.
				 */
				i++;
				if (c == 'p' && *cursor)
					cursor++;
			} else {
				/*
				 * In whole line case, have to back up dot
				 * for P; also want to clear cursor so
				 * cursor will eventually be positioned
				 * at the beginning of the first put line.
				 */
				cursor = 0;
				if (c == 'P') {
					dot--, vcline--;
					c = 'p';
				}
			}
			killU();

			/*
			 * The call to putreg can potentially
			 * bomb since there may be nothing in a named buffer.
			 * We thus put a catch in here.  If we didn't and
			 * there was an error we would end up in command mode.
			 */
			addr = dol;	/* old dol */
			CATCH
				vremote(1, vreg ? putreg : put, vreg);
			ONERR
				if (vreg == -1) {
					splitw = 0;
					if (op == 'P')
						dot++, vcline++;
					goto pfixup;
				}
			ENDCATCH
			splitw = 0;
			nlput = dol - addr + 1;
			if (!i) {
				/*
				 * Increment undap1, undap2 to make up
				 * for their incorrect initialization in the
				 * routine vremote before calling put/putreg.
				 */
				if (FIXUNDO)
					undap1++, undap2++;
				vcline++;
				nlput--;

				/*
				 * After a put want current line first line,
				 * and dot was made the last line put in code
				 * run so far.  This is why we increment vcline
				 * above and decrease dot here.
				 */
				dot -= nlput - 1;
			}
#ifdef TRACE
			if (trace)
				fprintf(trace, "vreplace(%d, %d, %d), undap1=%d, undap2=%d, dot=%d\n", vcline, i, nlput, lineno(undap1), lineno(undap2), lineno(dot));
#endif
			vreplace(vcline, i, nlput);
			if (state != VISUAL) {
				/*
				 * Special case in open mode.
				 * Force action on the screen when a single
				 * line is put even if it is identical to
				 * the current line, e.g. on YP; otherwise
				 * you can't tell anything happened.
				 */
				vjumpto(dot, cursor, '.');
				continue;
			}
pfixup:
			vrepaint(cursor);
			vfixcurs();
			continue;

		/*
		 * ^^		Return to previous file.
		 *		Like a :e #, and thus can be used after a
		 *		"No Write" diagnostic.
		 */
		case CTRL('^'):
			forbid (hadcnt);
			vsave();
			ckaw();
			oglobp = globp;
			if (value(AUTOWRITE))
				globp = "e! #";
			else
				globp = "e #";
			goto gogo;

		/*
		 * ^]		Takes word after cursor as tag, and then does
		 *		tag command.  Read ``go right to''.
		 */
		case CTRL(']'):
			grabtag();
			oglobp = globp;
			globp = "tag";
			goto gogo;

		/*
		 * &		Like :&
		 */
		 case '&':
			oglobp = globp;
			globp = "&";
			goto gogo;
			
		/*
		 * ^G		Bring up a status line at the bottom of
		 *		the screen, like a :file command.
		 *
		 * BUG:		Was ^S but doesn't work in cbreak mode
		 */
		case CTRL('g'):
			oglobp = globp;
			globp = "file";
gogo:
			addr = dot;
			vsave();
			goto doinit;

#ifdef SIGTSTP
		/*
		 * ^Z:	suspend editor session and temporarily return
		 * 	to shell.  Only works with Berkeley/IIASA process
		 *	control in kernel.
		 */
		case CTRL('z'):
			forbid(dosusp == 0 || !ldisc);
			vsave();
			oglobp = globp;
			globp = "stop";
			goto gogo;
#endif

		/*
		 * :		Read a command from the echo area and
		 *		execute it in command mode.
		 */
		case ':':
			forbid (hadcnt);
			vsave();
			i = tchng;
			addr = dot;
			if (readecho(c)) {
				esave[0] = 0;
				goto fixup;
			}
			getDOT();
			/*
			 * Use the visual undo buffer to store the global
			 * string for command mode, since it is idle right now.
			 */
			oglobp = globp; strcpy(vutmp, genbuf+1); globp = vutmp;
doinit:
			esave[0] = 0;
			fixech();

			/*
			 * Have to finagle around not to lose last
			 * character after this command (when run from ex
			 * command mode).  This is clumsy.
			 */
			d = peekc; ungetchar(0);
			if (shouldpo) {
				/*
				 * So after a "Hit return..." ":", we do
				 * another "Hit return..." the next time
				 */
				pofix();
				shouldpo = 0;
			}
			CATCH
				/*
				 * Save old values of options so we can
				 * notice when they change; switch into
				 * cooked mode so we are interruptible.
				 */
				onumber = value(NUMBER);
				olist = value(LIST);
				OPline = Pline;
				OPutchar = Put_char;
#ifndef CBREAK
				vcook();
#endif
				commands(1, 1);
				if (dot == zero && dol > zero)
					dot = one;
#ifndef CBREAK
				vraw();
#endif
			ONERR
#ifndef CBREAK
				vraw();
#endif
				copy(esave, vtube[WECHO], TUBECOLS);
			ENDCATCH
			fixol();
			Pline = OPline;
			Put_char = OPutchar;
			ungetchar(d);
			globp = oglobp;

			/*
			 * If we ended up with no lines in the buffer, make
			 * a line, and don't consider the buffer changed.
			 */
			if (dot == zero) {
				fixzero();
				ex_sync();
			}
			splitw = 0;

			/*
			 * Special case: did list/number options change?
			 */
			if (onumber != value(NUMBER))
				ignorf(setnumb(value(NUMBER)));
			if (olist != value(LIST))
				ignorf(setlist(value(LIST)));

fixup:
			/*
			 * If a change occurred, other than
			 * a write which clears changes, then
			 * we should allow an undo even if .
			 * didn't move.
			 *
			 * BUG: You can make this wrong by
			 * tricking around with multiple commands
			 * on one line of : escape, and including
			 * a write command there, but its not
			 * worth worrying about.
			 */
			if (FIXUNDO && tchng && tchng != i)
				vundkind = VMANY, cursor = 0;

			/*
			 * If we are about to do another :, hold off
			 * updating of screen.
			 */
			if (vcnt < 0 && Peek_key == ':') {
				getDOT();
				shouldpo = 1;
				continue;
			}
			shouldpo = 0;

			/*
			 * In the case where the file being edited is
			 * new; e.g. if the initial state hasn't been
			 * saved yet, then do so now.
			 */
			if (unddol == truedol) {
				vundkind = VNONE;
				Vlines = lineDOL();
				if (!inglobal)
					savevis();
				addr = zero;
				vcnt = 0;
				if (esave[0] == 0)
					copy(esave, vtube[WECHO], TUBECOLS);
			}

			/*
			 * If the current line moved reset the cursor position.
			 */
			if (dot != addr) {
				vmoving = 0;
				cursor = 0;
			}

			/*
			 * If current line is not on screen or if we are
			 * in open mode and . moved, then redraw.
			 */
			i = vcline + (dot - addr);
			if (i < 0 || i >= vcnt && i >= -vcnt || state != VISUAL && dot != addr) {
				if (state == CRTOPEN)
					vup1();
				if (vcnt > 0)
					vcnt = 0;
				vjumpto(dot, (char *) 0, '.');
			} else {
				/*
				 * Current line IS on screen.
				 * If we did a [Hit return...] then
				 * restore vcnt and clear screen if in visual
				 */
				vcline = i;
				if (vcnt < 0) {
					vcnt = -vcnt;
					if (state == VISUAL)
						vclear();
					else if (state == CRTOPEN) {
						vcnt = 0;
					}
				}

				/*
				 * Limit max value of vcnt based on $
				 */
				i = vcline + lineDOL() - lineDOT() + 1;
				if (i < vcnt)
					vcnt = i;
				
				/*
				 * Dirty and repaint.
				 */
				vdirty(0, LINES);
				vrepaint(cursor);
			}

			/*
			 * If in visual, put back the echo area
			 * if it was clobberred.
			 */
			if (state == VISUAL) {
				int sdc = destcol, sdl = destline;

				splitw++;
				vigoto(WECHO, 0);
				for (i = 0; i < TUBECOLS - 1; i++) {
					if (esave[i] == 0)
						break;
					vputchar(esave[i]);
				}
				splitw = 0;
				vgoto(sdl, sdc);
			}
			continue;

		/*
		 * u		undo the last changing command.
		 */
		case 'u':
			vundo(1);
			continue;

		/*
		 * U		restore current line to initial state.
		 */
		case 'U':
			ex_vUndo();
			continue;

fonfon:
			beep();
			vmacp = 0;
			inopen = 1;	/* might have been -1 */
			continue;
		}

		/*
		 * Rest of commands are decoded by the operate
		 * routine.
		 */
		operate(c, cnt);
	}
}

/*
 * Grab the word after the cursor so we can look for it as a tag.
 */
grabtag()
{
	register char *cp, *dp;

	cp = vpastwh(cursor);
	if (*cp) {
		dp = lasttag;
		do {
			if (dp < &lasttag[sizeof lasttag - 2])
				*dp++ = *cp;
			cp++;
		} while (isalpha(*cp) || isdigit(*cp) || *cp == '_'
#ifdef LISPCODE
			|| (value(LISP) && *cp == '-')
#endif LISPCODE
			);
		*dp++ = 0;
	}
}

/*
 * Before appending lines, set up addr1 and
 * the command mode undo information.
 */
prepapp()
{

	addr1 = dot;
	deletenone();
	addr1++;
	appendnone();
}

/*
 * Execute function f with the address bounds addr1
 * and addr2 surrounding cnt lines starting at dot.
 */
vremote(cnt, f, arg)
	int cnt, (*f)(), arg;
{
	register int oing = inglobal;

	addr1 = dot;
	addr2 = dot + cnt - 1;
	inglobal = 0;
	if (FIXUNDO)
		undap1 = undap2 = dot;
	(*f)(arg);
	inglobal = oing;
	if (FIXUNDO)
		vundkind = VMANY;
	vmcurs = 0;
}

/*
 * Save the current contents of linebuf, if it has changed.
 */
vsave()
{
	char temp[LBSIZE];

	CP(temp, linebuf);
	if (FIXUNDO && vundkind == VCHNG || vundkind == VCAPU) {
		/*
		 * If the undo state is saved in the temporary buffer
		 * vutmp, then we sync this into the temp file so that
		 * we will be able to undo even after we have moved off
		 * the line.  It would be possible to associate a line
		 * with vutmp but we assume that vutmp is only associated
		 * with line dot (e.g. in case ':') above, so beware.
		 */
		prepapp();
		strcLIN(vutmp);
		putmark(dot);
		vremote(1, yank, 0);
		vundkind = VMCHNG;
		notecnt = 0;
		undkind = UNDCHANGE;
	}
	/*
	 * Get the line out of the temp file and do nothing if it hasn't
	 * changed.  This may seem like a loss, but the line will
	 * almost always be in a read buffer so this may well avoid disk i/o.
	 */
	getDOT();
	if (strcmp(linebuf, temp) == 0)
		return;
	strcLIN(temp);
	putmark(dot);
}

#undef	forbid
#define	forbid(a)	if (a) { beep(); return; }

/*
 * Do a z operation.
 * Code here is rather long, and very uninteresting.
 */
vzop(hadcnt, cnt, c)
	bool hadcnt;
	int cnt;
	register int c;
{
	register line *addr;

	if (state != VISUAL) {
		/*
		 * Z from open; always like a z=.
		 * This code is a mess and should be cleaned up.
		 */
		vmoveitup(1, 1);
		vgoto(outline, 0);
		ostop(normf);
		setoutt();
		addr2 = dot;
		vclear();
		destline = WECHO;
		zop2(Xhadcnt ? Xcnt : value(WINDOW) - 1, '=');
		if (state == CRTOPEN)
			putnl();
		putNFL();
		termreset();
		Outchar = vputchar;
		ignore(ostart());
		vcnt = 0;
		outline = destline = 0;
		vjumpto(dot, cursor, 0);
		return;
	}
	if (hadcnt) {
		addr = zero + cnt;
		if (addr < one)
			addr = one;
		if (addr > dol)
			addr = dol;
		markit(addr);
	} else
		switch (c) {

		case '+':
			addr = dot + vcnt - vcline;
			break;

		case '^':
			addr = dot - vcline - 1;
			forbid (addr < one);
			c = '-';
			break;

		default:
			addr = dot;
			break;
		}
	switch (c) {

	case '.':
	case '-':
		break;

	case '^':
		forbid (addr <= one);
		break;

	case '+':
		forbid (addr >= dol);
		/* fall into ... */

	case CR:
	case NL:
		c = CR;
		break;

	default:
		beep();
		return;
	}
	vmoving = 0;
	vjumpto(addr, NOSTR, c);
}
