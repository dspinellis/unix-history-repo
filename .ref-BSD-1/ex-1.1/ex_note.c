#
/*
 * Ex - a text editor
 * Bill Joy UCB September, 1977
 */

#include "ex.h"
#include "ex_glob.h"

notable(i)
	int i;
{

	return (value(HUSH) == 0 && !inglobal && i > value(NOTIFY));
}

killed()
{
	register int cnt;

	cnt = addr2 - addr1 + 1;
	if (!notable(cnt))
		return;
	printf("%d lines", cnt);
	if (value(TERSE) == 0) {
		printf(" %c%s", Command[0] | ' ', Command + 1);
		if (Command[strlen(Command) - 1] != 'e')
			putchar('e');
		putchar('d');
	}
	putNFL();
}

netchHAD(cnt)
	int cnt;
{

	netchange((dol - zero) - cnt);
}

netchange(i)
	register int i;
{
	register char *cp;

	if (i > 0)
		cp = "more";
	else
		cp = "less", i = -i;
	if (!notable(i))
		return;
	printf(mesg("%d %s lines@in file after %s"), i, cp, Command);
	putNFL();
}

snote(total, lines)
	register int total, lines;
{

	if (!notable(total))
		return;
	printf(mesg("%d subs|%d substitutions"), total);
	if (lines != 1 && lines != total)
		printf(" on %d lines", lines);
	putNFL();
}

noteargs()
{
	if (argc > 1) {
		printf(mesg("%d files@to edit"), argc);
		putNFL();
	}
}
