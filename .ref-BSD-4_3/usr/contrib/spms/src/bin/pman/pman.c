static char *rcsid = "$Header$";
/*
 * pman - print project manual
 *
 * Author: Peter J. Nicklin
 */
#include <stdio.h>
#include "getarg.h"
#include "path.h"
#include "spms.h"
#include "yesno.h"

#define CMDBUFSIZE	1024

char *PGN = "pman";			/* program name */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int PPDEBUG;		/* project pathname debug flag */
	char *argvtos();		/* convert cmd arguments to string */
	char cmdbuf[CMDBUFSIZE];	/* man command buffer */
	char *getcwp();			/* get current working project */
	char *ppathname = CURPROJECT;	/* project pathname */
	char *sprintf();		/* print output to string */
	int status = 0;			/* exit status */
	int xppath();			/* expand project pathname */
	PATH pathbuf;			/* pathname struct buffer */

	{
	register char *s;		/* option pointer */
	while (--argc > 0 && **++argv == '-')
		{
		for (s = argv[0]+1; *s != '\0'; s++)
			switch (*s)
				{
				case 'D':
					PPDEBUG = YES;
					break;
				case 'P':
					ppathname = GETARG(s);
					if (*ppathname == '\0')
						{
						warn("missing project name");
						status = 1;
						}
					goto endfor;
				default:
					warn("bad option -%c", *s);
					status = 1;
					goto endfor;
				}
		endfor: continue;
		}
	}
	if (status == 1 || argc < 1)
		fatal("usage: pman [-P projectname] [section] topic ...");

	/* convert project pathname to regular pathname */
	if (xppath(ppathname, &pathbuf) == -1)
		{
		patherr(ppathname);
		exit(1);
		}
	else switch (pathbuf.p_mode & P_IFMT)
		{
		case P_IFHOME:
		case P_IFPROOT:
			break;
		default:
			fatal("%s: no such project", ppathname);
			break;
		}
	sprintf(cmdbuf, "man -P %s/man %s",pathbuf.p_path,argvtos(argc, argv));
	system(cmdbuf);
	exit(status);
}
