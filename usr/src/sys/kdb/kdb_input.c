/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kdb_input.c	7.3 (Berkeley) 12/15/86
 */

#include "../kdb/defs.h"

char	line[LINSIZ];
char	*lp;
char	peekc,lastc = EOR;

/* input routines */

eol(c)
	char c;
{
	return (c==EOR || c==';');
}

rdc()
{
	do
		(void) readchar();
	while (lastc==SP || lastc==TB);
	return (lastc);
}

readchar()
{
	static char *erase = "\b \b";

	if (lp==0) {
		lp=line;
		do {
			(void) kdbreadc(lp);
			if (mkfault)
				error((char *)0);
			switch (*lp) {
			case CTRL(h): case 0177:
				if (lp > line)
					kdbwrite(erase, 3), lp--;
				break;
			case CTRL(u):
				while (lp > line) 
					kdbwrite(erase, 3), lp--;
				break;
			case CTRL(r):
				kdbwrite("^R\n", 3);
				if (lp > line)
					kdbwrite(line, lp-line);
				break;
			case CTRL(w):
				if (lp <= line)
					break;
				do {
					if (!isspace(*lp))
						goto erasenb;
					kdbwrite(erase, 3);
				} while (--lp > line);
				break;
			erasenb:
				do
					kdbwrite(erase, 3);
				while (--lp > line && !isspace(*lp));
				break;
			default:
				echo(*lp++);
				break;
			}
		} while (lp == line || lp[-1] != EOR);
		*lp=0; lp=line;
	}
	if (lastc = peekc)
		peekc=0;
	else  if (lastc = *lp)
		lp++;
	return (lastc);
}

static
echo(c)
	char c;
{
	char buf[2];

	if (c==0177 || (c<SP && c != TB && c != EOR)) {
		buf[0] = '^';
		buf[1] = c ^ 0100;
		kdbwrite(buf, 2);
	} else
		kdbwrite(&c, 1);
}

nextchar()
{

	if (eol(rdc())) {
		lp--;
		return (0);
	}
	return (lastc);
}

quotchar()
{

	if (readchar()=='\\')
		return (readchar());
	if (lastc=='\'')
		return (0);
	return (lastc);
}

getformat(deformat)
	char *deformat;
{
	register char *fptr;
	register int quote;

	fptr=deformat; quote=0;
	while ((quote ? readchar()!=EOR : !eol(readchar())))
		if ((*fptr++ = lastc)=='"')
			quote = ~quote;
	lp--;
	if (fptr!=deformat)
		*fptr++ = '\0';
}
