#ifndef lint
static	char *sccsid = "@(#)cmd2.c	3.25 84/04/05";
#endif

#include "defs.h"

char *help_shortcmd[] = {
	"{1-9}   Select window {1-9} and return to conversation mode.",
	"%{1-9}  Select window {1-9} but stay in command mode.",
	"escape  Return to conversation mode",
	"        and don't change the current window.",
	"^^      Return to conversation mode",
	"        and change to previously selected window.",
	"c{1-9}  Close window {1-9}.",
	"C       Close all windows.",
	"S       Show all windows in sequence.",
	"L       List all windows with their labels.",
	"w       Open a new window.",
	"m{1-9}  Move window {1-9}.",
	"M{1-9}  Move window {1-9} to previous position.",
	"v       List all variables.",
	"{^Y^E}  Scroll {up, down} one line",
	"{^U^D}  Scroll {up, down} half a window.",
	"{^B^F}  Scroll {up, down} a full window.",
	"{hjkl}  Move cursor {left, down, up, right}.",
	"^L      Redraw screen.",
	"^Z      Suspend.",
	"q       Quit.",
	0
};
char *help_longcmd[] = {
	":%{1-9}               Select window {1-9}.",
	":buffer lines         Set the default window buffer size.",
	":close {1-9}          Close window.",
	":cursor modes         Set the cursor modes.",
	":escape C             Set escape character to C.",
	":label {1-9} string   Label window {1-9}.",
	":source filename      Execute commands in ``filename''.",
	":terse [off]          Turn on (or off) terse mode.",
	":unset variable       Deallocate ``variable''.",
	":window row col nrow ncol [nline label]",
	"                      Open a window at ``row'', ``col''",
	"                      of size ``nrow'', ``ncol'',",
	"                      with ``nline'', and ``label''.",
	":write {1-9} string   Write ``string'' to window {1-9}.",
	0
};

struct ww *getwin();
struct ww *openwin();
char *strtime();
dohelp()
{
	register struct ww *w;

	if ((w = openwin(wwnrow - 1, "Help")) == 0) {
		if (terse)
			Ding();
		else
			wwputs("Can't open help window.  ", cmdwin);
		return;
	}
	wwprintf(w, "The escape character is %s, which gets you into command mode.\r\n\n",
		unctrl(escapec));
	wwprintf(w, "Short commands:\r\n\n");
	wwprintf(w, "{1-9}   Select window {1-9} and return to conversation mode.\r\n");
	wwprintf(w, "%%{1-9}  Select window {1-9}.\r\n");
	wwprintf(w, "c{1-9}  Close window {1-9}.\r\n");
	wwprintf(w, "C       Close all windows.\r\n");
	wwprintf(w, "S       Show all windows in sequence.\r\n");
	wwprintf(w, "L       List all windows with their labels.\r\n");
	wwprintf(w, "w       Open a new window.\r\n");
	wwprintf(w, "[^U^D]  Scroll [up, down] half a window.\r\n");
	wwprintf(w, "[^B^F]  Scroll [up, down] a full window.\r\n");
	wwprintf(w, "[hjkl]  Move cursor [left, down, up, right].\r\n");
	wwprintf(w, "escape  Exit command mode.\r\n");
	wwprintf(w, "^L      Redraw screen.\r\n");
	wwprintf(w, "^Z      Suspend.\r\n");
	wwprintf(w, ".       Quit.\r\n");
	if (help_print(w, "Short commands", help_shortcmd) >= 0)
		(void) help_print(w, "Long commands", help_longcmd);
	closewin(w);
}

help_print(w, name, list)
register struct ww *w;
char *name;
register char **list;
{
	wwprintf(w, "%s:\n\n", name);
	while (*list)
		switch (more(w, 0)) {
		case 0:
			wwputs(*list++, w);
			wwputc('\n', w);
			break;
		case 1:
			wwprintf(w, "%s: (continued)\n\n", name);
			break;
		case 2:
			return -1;
		}
	return more(w, 1) == 2 ? -1 : 0;
}

dolist()
{
	register struct ww *w, *w1;
	int id;
	char doneit = 0;

	if ((w = openwin(14, "Active Windows")) == 0) {
		if (terse)
			Ding();
		else
			wwputs("Can't open listing window.  ", cmdwin);
		return;
	}
	for (id = 1; id <= NWINDOW; id++) {
		if ((w1 = wwfind(id)) == 0)
			continue;
		doneit = 1;
		wwprintf(w, "%d   %s\r\n", id, w1->ww_label);
	}
	if (!doneit)
		wwprintf(w, "No windows.\r\n");
	waitnl(w);
	closewin(w);
}

doquit()
{
	while (wwpeekc() < 0)
		wwiomux();
	if (wwgetc() == 'y') {
		wwputs("Yes", cmdwin);
		quit++;
	} else
		wwputs("\r\n", cmdwin);
	if (terse)
		Whide(cmdwin->ww_win);
}
