#ifndef lint
static	char *sccsid = "@(#)cmd2.c	3.26 84/04/07";
#endif

#include "defs.h"

char *help_shortcmd[] = {
	"{1-9}   Select window {1-9} and return to conversation mode.",
	"%{1-9}  Select window {1-9} but stay in command mode.",
	"escape  Return to conversation mode without changing window.",
	"^^      Return to conversation mode and change to previous window.",
	"c{1-9}  Close window {1-9}.",
	"w       Open a new window.",
	"m{1-9}  Move window {1-9}.",
	"M{1-9}  Move window {1-9} to previous position.",
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
	":close {1-9} . . .    Close windows.",
	":close all            Close all windows.",
	":cursor modes         Set the cursor modes.",
	":escape C             Set escape character to C.",
	":foreground {1-9} [off]",
	"                      Make {1-9} a foreground window.",
	":label {1-9} string   Label window {1-9}.",
	":list                 List all windows.",
	":nline lines          Set the default number of lines",
	"                      in window text buffers.",
	":source filename      Execute commands in ``filename''.",
	":terse [off]          Turn on (or off) terse mode.",
	":unset variable       Deallocate ``variable''.",
	":variable             List all variables.",
	":window row col nrow ncol [nline label]",
	"                      Open a window at ``row'', ``col''",
	"                      of size ``nrow'', ``ncol'',",
	"                      with ``nline'', and ``label''.",
	":write {1-9} string . . .",
	"                      Write strings to window {1-9}.",
	0
};

c_help()
{
	register struct ww *w;

	if ((w = openiwin(wwnrow - 3, "Help")) == 0) {
		error("Can't open help window: %s.", wwerror());
		return;
	}
	wwprintf(w, "The escape character is %s, which gets you into command mode.\n\n",
		unctrl(escapec));
	if (help_print(w, "Short commands", help_shortcmd) >= 0)
		(void) help_print(w, "Long commands", help_longcmd);
	closeiwin(w);
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

c_quit()
{
	char oldterse = terse;

	setterse(0);
	wwputs("Really quit [yn]? ", cmdwin);
	wwcurtowin(cmdwin);
	while (wwpeekc() < 0)
		wwiomux();
	if (wwgetc() == 'y') {
		wwputs("Yes", cmdwin);
		quit++;
	} else
		wwputs("\r\n", cmdwin);
	setterse(!quit && oldterse);
}
