#
/*
 * Ex - a text editor
 * Bill Joy UCB September, 1977
 */

#include "ex.h"

#define	UXBSZ	100

char	uxb[UXBSZ + 2];

unix()
{
	register char *up, *fp;
	register c;
	char printub, puxb[UXBSZ + 2];

	setnoaddr();
	printub = 0;
	strcpy(puxb, uxb);
	c = getchar();
	if (c == '\n' || c == EOF)
		error("Incomplete shell escape command@- use 'shell' to get a shell");
	up = uxb;
	do {
		switch (c) {
			case '\\':
				if (any(peekchar(), "%`!"))
					c = getchar();
			default:
				if (up >= &uxb[UXBSZ]) {
tunix:
					uxb[0] = 0;
					error("Command too long@- limit 100 characters");
				}
				*up++ = c;
				break;
			case '!':
				fp = puxb;
				if (*fp == 0) {
					uxb[0] = 0;
					error("No previous command@to substitute for !");
				}
				printub++;
				while (*fp) {
					if (up >= &uxb[UXBSZ])
						goto tunix;
					*up++ = *fp++;
				}
				break;
			case '`':
				fp = altfile;
				if (*fp == 0) {
					uxb[0] = 0;
					error("No alternate filename@to substitute for `");
				}
				goto uexp;
			case '%':
				fp = savedfile;
				if (*fp == 0) {
					uxb[0] = 0;
					error("No filename@to substitute for %%");
				}
uexp:
				printub++;
				while (*fp) {
					if (up >= &uxb[UXBSZ])
						goto tunix;
					*up++ = *fp++ | QUOTE;
				}
				break;
		}
		c = getchar();
	} while (c == '|' || !endcmd(c));
	*up = 0;
	up = uxb;
	unix2("-c", printub, uxb);
}

unix2(opt, printub, up)
	char *opt, printub, *up;
{
	register int savint;

	if (chngflag && xchngflag != chngflag) {
		xchngflag = chngflag;
		if (!value(FORK))
			error("No write@since last change");
		dingdong();
		printf(mesg("[No write]\n|[No write since last change]\n"));
		flush();
	}
	if (value(HUSH) == 0 && printub) {
		if (uxb[0] == 0)
			error("No previous command@to repeat");
		lprintf("!%s\n", uxb);
		flush();
	}
	savint = signal(INTR, 1);
	pid = fork();
	if (pid < 0) {
		signal(INTR, savint);
		error("No more processes");
	}
	if (pid == 0) {
/*
		if (io)
			close(io);
		if (tfile)
			close(tfile);
		close(erfile);
*/
		signal(HUP, oldhup);
		signal(QUIT, oldquit);
		execl(value(SHELL), "sh", opt, up, 0);
		die++;
		error("No %s!\n", value(SHELL));
	}
	waitfor();
	signal(INTR, savint);
	if (value(HUSH) == 0)
		printf("!\n");
	flush();
	termreset();
	gettmode();
}
