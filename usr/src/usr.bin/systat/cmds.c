#ifndef lint
static char sccsid[] = "@(#)cmds.c	1.4 (Lucasfilm) %G%";
#endif

/*
 * Command support.
 */

#include "systat.h"
#include <signal.h>
#include <ctype.h>

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
	if (*cmd == '\0')
		return;
	for (; *cp && isspace(*cp); cp++)
		;
        if (strcmp(cmd, "quit") == 0 || strcmp(cmd, "q") == 0)
                die();
	if (strcmp(cmd, "load") == 0) {
		load();
		return;
	}
        if (strcmp(cmd, "stop") == 0) {
                alarm(0);
                mvaddstr(CMDLINE, 0, "Refresh disabled.");
                clrtoeol();
                return;
        }
        if (strcmp(cmd, "start") == 0 || strcmp(cmd, "interval") == 0) {
                int x;

		x = *cp ? atoi(cp) : naptime;
                if (x <= 0) {
			error("%d: bad interval.", x);
                        return;
                }
                alarm(0);
                naptime = x;
                display();
                status();
                return;
        }
	p = lookup(cmd);
	if (p == (struct cmdtab *)-1) {
		error("%s: Ambiguous command.", cmd);
		return;
	}
        if (p) {
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
	if (curcmd->c_cmd && (*curcmd->c_cmd)(cmd, cp))
		return;
	error("%s: Unknown command.", cmd);
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

        error("Showing %s, refresh every %d seconds.",
          curcmd->c_name, naptime);
}

suspend()
{
        int oldmask;

	alarm(0);
        move(CMDLINE, 0);
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
        move(CMDLINE, col);
        wrefresh(curscr);
	alarm(naptime);
}
