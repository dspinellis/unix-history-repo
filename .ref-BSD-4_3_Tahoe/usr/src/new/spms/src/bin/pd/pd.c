static char *rcsid = "$Header$";
/*
 * pd - change working project directory
 *
 * Author: Peter J. Nicklin
 */
#include <stdio.h>
#include "macro.h"
#include "null.h"
#include "path.h"
#include "pdb.h"
#include "pld.h"
#include "spms.h"
#include "yesno.h"

char *PGN = "pd";			/* program name */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int PPDEBUG;		/* project pathname debug flag */
	char *alias;			/* project directory alias */
	char *cmd;			/* change directory command pointer */
	char *cwp;			/* current project regular pathname */
	char dirdesc[DIRDESCSIZE];	/* project directory description */
	char *getcwp();			/* get current project pathname */
	char *pbgetstring();		/* get specified string field */
	char *ppathname;		/* project pathname */
	int dflag = NO;			/* print description flag */
	int errpdb();			/* print database error message */
	int pfndent();			/* find and load database entry */
	int status = 0;			/* exit status */
	int xppath();			/* expand project pathname */
	PATH pathbuf;			/* pathname struct buffer */
	PDB *openpdb();			/* open database */
	PDB *pldp;			/* project link directory stream */
	void nodir();			/* "can't find dir" fatal error call */

	cmd = "cd";
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
				case 'd':
					dflag = YES;
					break;
				case 'p':
					cmd = "pushd";
					break;
				default:
					warn("bad option -%c", *s);
					status = 1;
					goto endfor;
				}
		endfor: continue;
		}
	}
	if (status == 1 || argc > 1)
		fatal("usage: pd [-d] pdirname");

	/* print the target directory */
	ppathname = (argc < 1) ? CURPROJECT : *argv;
	if (xppath(ppathname, &pathbuf) == -1)
		nodir(ppathname);
	else switch (pathbuf.p_mode & P_IFMT)
		{
		case P_IFNEW:
			nodir(ppathname);
			break;
		case P_IFREG:
			break;
		case P_IFPDIR:
		case P_IFHOME:
		case P_IFPROOT:
			/* change project if different (for cd only) */
			if (*cmd == 'c' && (cwp = getcwp()) != NULL)
				if (!EQUAL(cwp, pathbuf.p_project))
					printf("setenv PROJECT %s;",
					       pathbuf.p_project);
			break;
		}
	printf("%s %s", cmd, pathbuf.p_path);

	/* print project directory description */
	if (dflag == YES && (pathbuf.p_mode&P_IFMT) != P_IFREG)
		{
		if ((pathbuf.p_mode&P_IFMT) == P_IFPDIR)
			alias = pathbuf.p_alias;
		else
			alias = CURPROJECT;
		if ((pldp = openpdb(PLDNAME, pathbuf.p_project, "r")) == NULL)
			exit(errpdb((PDB *) NULL));
		if (pfndent(alias, pldp) == NO)
			fatal("can't find %s alias in %s", alias, pldp->path);
		if (*pbgetstring(PDIRDESC, dirdesc) != '\0')
			fprintf(stderr, "%s\n", dirdesc);
		}
	exit(0);
}



/*
 * nodir() complains that it can't find the directory and calls exit(1).
 */
void
nodir(ppathname)
	char *ppathname;		/* project pathname */
{
	*PGN = '\0';
	patherr(ppathname);
	exit(1);
}
