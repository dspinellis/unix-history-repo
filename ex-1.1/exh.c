#include "ex.h"
/*
 * Ex - a text editor
 * Bill Joy UCB June 1977
 */

extern	char *erpath, pfast;

helpinit()
{

	erfile = open(erpath, 0);
	if (erfile < 0)
		flush();
	else
		pstop();
}

error(str, i1, i2, i3)
	register char *str;
{
	register c;

	seek(erfile, str, 0);
	str = linebuf;
	if (read(erfile, str, 128) < 2)
		str = "ERROR";
	if (pfast)
		pfast = 1;
	flush();
	resetflav();
	if (laste) {
		laste = 0;
		sync();
	}
	dingdong();
	if (inopen) {
		termreset();
		putnl();
	}
	inopen = 0;
	inconf = 0;
	lprintf(mesg(str), i1, i2, i3);
	putNFL();
	if (die)
		exit(1);
	undiddle();
	seek(0, 0, 2);
	if (inglobal)
		setlastchar('\n');
	inglobal = 0;
	globp = 0;
	while (lastchar() != '\n' && lastchar() != EOF)
		c = getchar();
	ungetchar(0);
	endline = 1;
	if (io > 0) {
		close(io);
		io = -1;
	}
	reset();
}

dingdong()
{

	if (value(ERRBELLS))
		printf("\207\207");
}

/*
 * Mesg decodes the terse/verbose strings. Thus
 *	'xxx@yyy' -> 'xxx' if terse, else 'xxx yyy'
 *	'xxx|yyy' -> 'xxx' if terse, else 'yyy'
 * All others map to themselves.
 */
mesg(str)
	register char *str;
{
	register char *cp;

	str = strcpy(genbuf, str);
	for (cp = str; *cp; cp++)
		switch (*cp) {
			case '@':
				if (value(TERSE))
					*cp = 0;
				else
					*cp = ' ';
				break;
			case '|':
				if (value(TERSE) == 0)
					return (cp + 1);
				*cp = 0;
				break;
		}
	return (str);
}

normal()
{

	if (normtty) {
		gTTY(1);
		tty[2] = normf;
		sTTY(1);
	}
	normtty = 0;
}

helpthem()
{
	register char *cp, *icp;
	char buff[513];
	register int buffcnt;

	strcpy(buff, "/usr/lib/how_ex/");
	icp = strend(buff);
	skipwh();
	if (endcmd(peekchar()))
		strcat(buff, "help");
	else {
		cp = icp;
		while (!white(peekchar()) && !endcmd(peekchar()))
			*cp++ = getchar();
		*cp++ = 0;
		skipwh();
		if (!endcmd(peekchar()))
			error("Help takes one keyword only as argument@- \"help index\" gives a list of keywords");
	}
	eol();
	io = open(buff, 0);
	if (io < 0)
		error("Don't know anything about %s@- \"help index\" gives a list of known subjects", icp);
	setnorm();
	for(;;) {
		buffcnt = read(io, buff, 512);
		if (buffcnt <= 0)
			break;
		buff[buffcnt] = 0;
		printf("%s", buff);
	}
	flush();
}
