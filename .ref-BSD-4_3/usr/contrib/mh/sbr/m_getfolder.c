/* m_getfolder.c - get the current folder */

#include "../h/mh.h"
#include <stdio.h>


char *m_getfolder()
{
    register char  *folder;

    if ((folder = m_find (pfolder)) == NULL || *folder == 0)
	folder = defalt;
    return folder;
}
