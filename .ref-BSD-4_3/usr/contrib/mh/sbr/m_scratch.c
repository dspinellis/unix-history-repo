/* m_scratch.c - construct a scratch file */

#include "../h/mh.h"
#include <stdio.h>


char   *m_scratch (file, template)
register char  *file,
               *template;
{
    register char  *cp;
    static char buffer[BUFSIZ],
		tmpfil[BUFSIZ];

    (void) sprintf (tmpfil, "%sXXXXXX", template);
    (void) mktemp (tmpfil);
    if ((cp = r1bindex (file, '/')) == file)
	(void) strcpy (buffer, tmpfil);
    else
	(void) sprintf (buffer, "%.*s%s", cp - file, file, tmpfil);
    (void) unlink (buffer);

    return buffer;
}
