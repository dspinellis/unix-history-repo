#ifndef lint
static char sccsid[] = "@(#)cmd2.c	3.31 %G%";
#endif

#include "defs.h"

char *help_shortcmd[] = {
	"#       Select window # and return to conversation mode.",
	"%#      Select window # but stay in command mode.",
	"escape  Return to conversation mode without changing window.",
	"^^      Return to conversation mode and change to previous window.",
	"c#      Close window #.",
	"w       Open a new window.",
	"m#      Move window #.",
	"M#      Move window # to its previous position.",
	"s#      Change the size of window #.",
	"S#      Change window # to its previous size.",
	"^Y,^E   Scroll up, down one line.",
	"^U,^D   Scroll up, down half a window.",
	"^B,^F   Scroll up, down a full window.",
	"h,j,k,l Move cursor left, down, up, right.",
	"^S,^Q   Stop, start output in current window.",
	"^L      Redraw screen.",
	"^Z      Suspend.",
	"q       Quit.",
	":       Enter a long command.",
	0
};
char *help_longcmd[] = {
	":%#                   Select window #.",
	":close # . . .        Close windows.",
	":close all            Close all windows.",
	":cursor modes         Set the cursor modes.",
	":echo # string . . .  Print ``string . . .'' in window #.",
	":escape C             Set escape character to C.",
	":foreground # [off]   Make # a foreground window.",
	":label # string       Set label of window # to ``string''.",
	":list                 Give a list of all windows.",
	":nline lines          Set the default number of lines",
	"                      in window text buffers.",
	":shell string . . .   Set default shell program to ``string . . .''",
	":source filename      Execute commands in ``filename.''",
	":terse [off]          Turn on (or off) terse mode.",
	":unset variable       Deallocate ``variable''.",
	":variable             List all variables.",
	":window row col nrow ncol [nline label pty frame shell]",
	"                      Open a window at ``row'', ``col''",
	"                      of size ``nrow'', ``ncol'',",
	"                      with ``nline'', and ``label''.",
	":write # string . . . Write ``string . . .'' to window #.",
	0
};

c_help()
{
	register struct ww *w;

	if ((w = openiwin(wwnrow - 3, "Help")) == 0) {
		error("Can't open help window: %s.", wwerror());
		return;
	}
	wwprintf(w, "The escape character is %c.\n", escapec);
	wwprintf(w, "(Below, # is one of the digits from 1 to 9.)\n\n");
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
		wwputc('\n', cmdwin);
	setterse(!quit && oldterse);
}
