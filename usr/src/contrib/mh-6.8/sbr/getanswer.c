/* getanswer.c - get a yes/no answer from the user */

#include "../h/mh.h"
#include <stdio.h>


int     getanswer (prompt)
register char   *prompt;
{
    static int  interactive = NOTOK;

    if (interactive < OK)
	interactive = isatty (fileno (stdin)) ? DONE : OK;

    return (interactive ? gans (prompt, anoyes) : DONE);
}
