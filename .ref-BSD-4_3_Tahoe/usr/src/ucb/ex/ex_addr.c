/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char *sccsid = "@(#)ex_addr.c	7.4 (Berkeley) 3/9/87";
#endif not lint

#include "ex.h"
#include "ex_re.h"

/*
 * Routines for address parsing and assignment and checking of address bounds
 * in command mode.  The routine address is called from ex_cmds.c
 * to parse each component of a command (terminated by , ; or the beginning
 * of the command itself.  It is also called by the scanning routine
 * in ex_voperate.c from within open/visual.
 *
 * Other routines here manipulate the externals addr1 and addr2.
 * These are the first and last lines for the current command.
 *
 * The variable bigmove remembers whether a non-local glitch of . was
 * involved in an address expression, so we can set the previous context
 * mark '' when such a motion occurs.
 */

static	bool bigmove;

/*
 * Set up addr1 and addr2 for commands whose default address is dot.
 */
setdot()
{

	setdot1();
	if (bigmove)
		markDOT();
}

/*
 * Call setdot1 to set up default addresses without ever
 * setting the previous context mark.
 */
setdot1()
{

	if (addr2 == 0)
		addr1 = addr2 = dot;
	if (addr1 > addr2) {
		notempty();
		error("Addr1 > addr2|First address exceeds second");
	}
}

/*
 * Ex allows you to say
 *	delete 5
 * to delete 5 lines, etc.
 * Such nonsense is implemented by setcount.
 */
setcount()
{
	register int cnt;

	pastwh();
	if (!isdigit(peekchar())) {
		setdot();
		return;
	}
	addr1 = addr2;
	setdot();
	cnt = getnum();
	if (cnt <= 0)
		error("Bad count|Nonzero count required");
	addr2 += cnt - 1;
	if (addr2 > dol)
		addr2 = dol;
	nonzero();
}

/*
 * Parse a number out of the command input stream.
 */
getnum()
{
	register int cnt;

	for (cnt = 0; isdigit(peekcd());)
		cnt = cnt * 10 + ex_getchar() - '0';
	return (cnt);
}

/*
 * Set the default addresses for commands which use the whole
 * buffer as default, notably write.
 */
setall()
{

	if (addr2 == 0) {
		addr1 = one;
		addr2 = dol;
		if (dol == zero) {
			dot = zero;
			return;
		}
	}
	/*
	 * Don't want to set previous context mark so use setdot1().
	 */
	setdot1();
}

/*
 * No address allowed on, e.g. the file command.
 */
setnoaddr()
{

	if (addr2 != 0)
		error("No address allowed@on this command");
}

/*
 * Parse an address.
 * Just about any sequence of address characters is legal.
 *
 * If you are tricky you can use this routine and the = command
 * to do simple addition and subtraction of cardinals less
 * than the number of lines in the file.
 */
line *
address(inline)
	char *inline;
{
	register line *addr;
	register int offset, c;
	short lastsign;

	bigmove = 0;
	lastsign = 0;
	offset = 0;
	addr = 0;
	for (;;) {
		if (isdigit(peekcd())) {
			if (addr == 0) {
				addr = zero;
				bigmove = 1;
			}
			loc1 = 0;
			addr += offset;
			offset = getnum();
			if (lastsign >= 0)
				addr += offset;
			else
				addr -= offset;
			lastsign = 0;
			offset = 0;
		}
		switch (c = getcd()) {

		case '?':
		case '/':
		case '$':
		case '\'':
		case '\\':
			bigmove++;
		case '.':
			if (addr || offset)
				error("Badly formed address");
		}
		offset += lastsign;
		lastsign = 0;
		switch (c) {

		case ' ':
		case '\t':
			continue;

		case '+':
			lastsign = 1;
			if (addr == 0)
				addr = dot;
			continue;

		case '^':
		case '-':
			lastsign = -1;
			if (addr == 0)
				addr = dot;
			continue;

		case '\\':
		case '?':
		case '/':
			c = compile(c, 1);
			notempty();
			savere(scanre);
			addr = dot;
			if (inline && execute(0, dot)) {
				if (c == '/') {
					while (loc1 <= inline) {
						if (loc1 == loc2)
							loc2++;
						if (!execute(1))
							goto nope;
					}
					break;
				} else if (loc1 < inline) {
					char *last;
doques:

					do {
						last = loc1;
						if (loc1 == loc2)
							loc2++;
						if (!execute(1))
							break;
					} while (loc1 < inline);
					loc1 = last;
					break;
				}
			}
nope:
			for (;;) {
				if (c == '/') {
					addr++;
					if (addr > dol) {
						if (value(WRAPSCAN) == 0)
error("No match to BOTTOM|Address search hit BOTTOM without matching pattern");
						addr = zero;
					}
				} else {
					addr--;
					if (addr < zero) {
						if (value(WRAPSCAN) == 0)
error("No match to TOP|Address search hit TOP without matching pattern");
						addr = dol;
					}
				}
				if (execute(0, addr)) {
					if (inline && c == '?') {
						inline = &linebuf[LBSIZE];
						goto doques;
					}
					break;
				}
				if (addr == dot)
					error("Fail|Pattern not found");
			}
			continue;

		case '$':
			addr = dol;
			continue;

		case '.':
			addr = dot;
			continue;

		case '\'':
			c = markreg(ex_getchar());
			if (c == 0)
				error("Marks are ' and a-z");
			addr = getmark(c);
			if (addr == 0)
				error("Undefined mark@referenced");
			break;

		default:
			ungetchar(c);
			if (offset) {
				if (addr == 0)
					addr = dot;
				addr += offset;
				loc1 = 0;
			}
			if (addr == 0) {
				bigmove = 0;
				return (0);
			}
			if (addr != zero)
				notempty();
			addr += lastsign;
			if (addr < zero)
				error("Negative address@- first buffer line is 1");
			if (addr > dol)
				error("Not that many lines@in buffer");
			return (addr);
		}
	}
}

/*
 * Abbreviations to make code smaller
 * Left over from squashing ex version 1.1 into
 * 11/34's and 11/40's.
 */
setCNL()
{

	setcount();
	newline();
}

setNAEOL()
{

	setnoaddr();
	eol();
}
