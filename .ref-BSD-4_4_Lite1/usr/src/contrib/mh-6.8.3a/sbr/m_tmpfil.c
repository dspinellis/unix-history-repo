/* m_tmpfil.c - construct a temporary file */

#include "../h/mh.h"
#include <stdio.h>


char   *m_tmpfil (template)
register char  *template;
{
    static char tmpfil[BUFSIZ];

    (void) sprintf (tmpfil, "/tmp/%sXXXXXX", template);
    (void) unlink (mktemp (tmpfil));

    return tmpfil;
}
