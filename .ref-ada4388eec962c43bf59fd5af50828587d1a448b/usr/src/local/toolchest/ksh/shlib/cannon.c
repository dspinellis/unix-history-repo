/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */

/* @(#)cannon.c	1.1 */

/*
 *  cannon_path - Generate canonical pathname from given pathname
 *
 *   David Korn
 *   AT&T Bell Laboratories
 *   Room 5D-112
 *   Murray Hill, N. J. 07974
 *   Tel. x7975
 *
 *   Written December 1982
 */

#ifdef BSD
#define strrchr rindex
#endif	/* BSD */
extern char *strrchr();
extern char *strcpy();

/*
 *  canonicalize path-name
 *  The canonicalized pathname replaces path
 */

void	cannon_path(path)
char *path;
{
	register char *a1;
	register char *newdir = path;
	register char *dp;
	/* eliminate redundant / */
	a1 = newdir;
	for(dp=a1;*dp = *a1++;dp++)
	{
		if(*dp == '/')
			while(*a1 == '/')
				a1++;
	}
	/* check for ./ and ../ */
	a1 = newdir;
	while(*a1)
	{
		if(*a1++ != '/' || *a1 != '.')
			continue;
		/* pathname begins with /.  */
		dp = a1-1;
		if(*++a1 == '/')	/* skip ./  */
			a1++;
		else if(*a1 == '.')
		{
			/* pathname begins with /.. */
			if(*++a1 != '/' && *a1 != 0)
				/* file name begins with .. */
				continue;
			else
			{
				/* parent directory */
				*dp = 0;
				if((dp=strrchr(newdir,'/')) == 0)
					dp = newdir;
				*dp = *a1++;
			}
		}
		else if(*a1 == 0)
			*dp = 0;
		else
			continue;
		if(*dp == 0)
			break;
		strcpy(dp+1,a1);
		a1 = dp;
	}
	for(a1=newdir;*a1;a1++);	/* skip to last character*/
	if(*--a1 == '/')		/* eliminate trailing / */
		*a1 = 0;
	if(*newdir == 0)
	{
		*newdir = '/';
		newdir[1] = 0;
	}
}
