static char *rcsid = "$Header$";
/*
 * pmv - move or rename files
 *
 * Author: Peter J. Nicklin
 */
#include "bin.h"
#include "macro.h"
#include "null.h"
#include "path.h"
#include "yesno.h"

char *PGN = "pmv";			/* program name */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int PPDEBUG;		/* project pathname debug flag */
	char **nargv;			/* new argument list */
	char *strsav();			/* save string somewhere */
	int filecount = 0;		/* # of valid files or directories */
	int nargi = 0;			/* new argument list index */
	int xppath();			/* expand project pathname */
	PATH pathbuf;			/* pathname struct buffer */

	nargv = argv;
	nargv[nargi++] = "mv";

	{
	register char *s;		/* option pointer */
	while (--argc > 0 && **++argv == '-')
		{
		s = argv[0]+1;
		if (*s == '\0')		/* special kludge to handle `-' flag */
			{
			nargv[nargi++] = *argv++;
			break;
			}
		else	{
			for (; *s != '\0'; s++)
				switch (*s)
					{
					case 'D':
						PPDEBUG = YES;
						break;
					default:
						nargv[nargi++] = *argv;
						break;
					}
			}
		}
	}
	if (argc < 2)
		fatal("usage: %s f1 f2; or %s f1 ... f2 d1", PGN, PGN);

	/* expand project pathnames and attach to new argument list */
	for (; argc > 1; argc--, argv++)
		if (xppath(*argv, &pathbuf) == -1)
			patherr(*argv);
		else switch (pathbuf.p_mode & P_IFMT)
			{
			case P_IFNEW:
				patherr(*argv);
				break;
			case P_IFPDIR:
				warn("can't move project directory %s", *argv);
				break;
			case P_IFHOME:
			case P_IFPROOT:
				warn("can't move project %s", *argv);
				break;
			default:
				nargv[nargi++] = strsav(pathbuf.p_path);
				filecount++;
			}

	if (filecount == 0)
		exit(1);
	else if (xppath(*argv, &pathbuf) == -1)
		{
		patherr(*argv);
		exit(1);
		}
	else	{
		nargv[nargi++] = strsav(pathbuf.p_path);
		nargv[nargi] = NULL;
		}
	
	/* execute "mv" command */
	execv(MV, nargv);
	fatal("can't exec %s", MV);
}
