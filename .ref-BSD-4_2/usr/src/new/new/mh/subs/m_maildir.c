#include "mh.h"
#include <stdio.h>

char *mypath;

char *m_maildir(folder)
char *folder;
{
	register char *fold, *path, *cp;
	static char mailfold[128];

	m_getdefs();
	if(!(fold = folder))
		fold = m_getfolder();
	if(*fold == '/' || *fold == '.')
		return(fold);
	cp = mailfold;
	if((path = m_find("path")) != NULL && *path) {
		if(*path != '/') {
			sprintf(cp, "%s/", mypath);
			cp += strlen(cp);
		}
		cp = copy(path, cp);
		if(cp[-1] != '/')
			*cp++ = '/';
	}
	strcpy(cp, fold);
	return(mailfold);
}
