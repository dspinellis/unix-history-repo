#include "mh.h"
#include <stdio.h>

char *m_getfolder()
{
	register char *folder;

	m_getdefs();
	if((folder = m_find(pfolder)) == NULL || *folder == 0)
		folder = defalt;
	return(folder);
}
