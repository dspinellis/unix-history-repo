#ifndef lint
static char sccsid[] = "@(#)cmds.c	1.2 (Lucasfilm) %G%";
#endif

/*
 * Command support.
 */

#include "systat.h"

command(cmd)
        char *cmd;
{
        register char *cp;
        register struct cmdtab *p;
        char *arg;

        for (cp = cmd; *cp && !isspace(*cp); cp++)
                ;
        if (*cp)
                *cp++ = '\0';
        if (strcmp(cmd, "quit") == 0 || strcmp(cmd, "q") == 0)
                die();
	if (strcmp(cmd, "load") == 0) {
		load();
		return;
	}
	p = lookup(cmd);
        if (p != (struct cmdtab *)-1) {
                if (curcmd == p)
                        return;
                alarm(0);
		(*curcmd->c_close)(wnd);
		wnd = (*p->c_open)();
                curcmd = p;
		if (p->c_flags == 0) {
			(*p->c_init)();
			p->c_flags = 1;
		}
		labels();
                display();
                status();
                return;
        }
        if (strcmp(cmd, "stop") == 0) {
                alarm(0);
                mvaddstr(22, 0, "Refresh disabled.");
                clrtoeol();
                return;
        }
        /* commands with arguments */
        for (; *cp && isspace(*cp); cp++)
                ;
        if (strcmp(cmd, "start") == 0 || strcmp(cmd, "interval") == 0) {
                int x;

                if (*cp == '\0')
                        x = naptime;
                else
                        x = atoi(cp);
                if (x <= 0) {
                        mvprintw(22, 0, "%d: bad interval.", x);
                        clrtoeol();
                        return;
                }
                alarm(0);
                naptime = x;
                display();
                status();
                return;
        }
	if (*cmd) {
		mvprintw(22, 0, "%s: Unknown command.", cmd);
		clrtoeol();
	}
}

struct cmdtab *
lookup(name)
	register char *name;
{
	register char *p, *q;
	register struct cmdtab *c, *found;
	register int nmatches, longest;

	longest = 0;
	nmatches = 0;
	found = 0;
	for (c = cmdtab; p = c->c_name; c++) {
		for (q = name; *q == *p++; q++)
			if (*q == 0)		/* exact match? */
				return (c);
		if (!*q) {			/* the name was a prefix */
			if (q - name > longest) {
				longest = q - name;
				nmatches = 1;
				found = c;
			} else if (q - name == longest)
				nmatches++;
		}
	}
	if (nmatches > 1)
		return ((struct cmdtab *)-1);
	return (found);
}

status()
{

        mvprintw(22, 0, "Showing %s, refresh every %d seconds.",
          curcmd->c_name, naptime);
        clrtoeol();
}

suspend()
{
        int oldmask;

	alarm(0);
        move(22, 0);
        refresh();
        echo();
        nocrmode();
        signal(SIGTSTP, SIG_DFL);
        oldmask = sigsetmask(0);
        kill(getpid(), SIGTSTP);
        sigsetmask(oldmask);
        signal(SIGTSTP, suspend);
        crmode();
        noecho();
        move(22, col);
        wrefresh(curscr);
	alarm(naptime);
}
