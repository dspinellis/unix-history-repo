/* m_backup.c - construct a backup file */

#include "../h/mh.h"
#include <stdio.h>


char   *m_backup (file)
register char   *file;
{
    register char  *cp;
    static char buffer[BUFSIZ];

    if ((cp = r1bindex (file, '/')) == file)
	(void) sprintf (buffer, "%s%s", SBACKUP, cp);
    else
	(void) sprintf (buffer, "%.*s%s%s", cp - file, file, SBACKUP, cp);
    (void) (unlink (buffer));

    return buffer;
}
