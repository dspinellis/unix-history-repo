/* m_fmsg.c - free a folder */

#include "../h/mh.h"
#include <stdio.h>


void	m_fmsg (mp)
register struct msgs *mp;
{
    register int    i;

    if (mp == NULL)
	return;

    if (mp -> foldpath)
	free (mp -> foldpath);
#ifdef	MTR
    free ((char *) mp -> msgbase);
#endif	MTR
    for (i = 0; mp -> msgattrs[i]; i++)
	free (mp -> msgattrs[i]);
    free ((char *) mp);
}
