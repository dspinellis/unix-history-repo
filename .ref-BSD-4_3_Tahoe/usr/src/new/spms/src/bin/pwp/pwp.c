static char *rcsid = "$Header$";
/*
 * pwp - path to working project
 *
 * Author: Peter J. Nicklin
 */
#include <pwd.h>
#include <stdio.h>
#include "macro.h"
#include "null.h"
#include "path.h"
#include "pdb.h"
#include "pdbuf.h"
#include "pld.h"
#include "spms.h"
#include "yesno.h"

#define PASSWDFILE	"/etc/passwd"

char *PGN = "pwp";			/* program name */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int PPDEBUG;		/* project pathname debug flag */
	char alias[ALIASSIZE];		/* project alias */
	char *cwp;			/* current working  project */
	char *getcwp();			/* get current working project */
	char *gethdir();		/* get home directory pathname */
	char lastcwp[PATHSIZE];		/* last current working project */
	char *pbfndstring();		/* find string field */
	char *pbgetkey();		/* get next key */
	char ppathname[PPATHSIZE];	/* project pathname */
	char *ppptr = &ppathname[(sizeof ppathname)-1];
					/* project pathname pointer */
	char *prepend();		/* prepend projectname to pathname */
	char *relpath;			/* relative pathname */
	char *strcpy();			/* string copy */
	char *xorpath();		/* remove subpathname */
	int chproject();		/* change working project */
	int closepdb();			/* close database */
	int errpdb();			/* print database error message */
	int lflag = NO;			/* long format flag */
	int pbcmpfield();		/* compare non-key fields */
	int pbfndflag();		/* find flag field */
	int pgetent();			/* load next entry into buffer */
	int status = 0;			/* exit status */
	PDB *pldp;			/* project link directory stream */
	PDB *openpdb();			/* open database */
	struct passwd *getpwdir();	/* get password file entry for dir */
	struct passwd *pw;		/* password entry pointer */

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
				case 'l':
					lflag = YES;
					break;
				default:
					warn("bad option -%c", *s);
					status = 1;
					goto endfor;
				}
		endfor: continue;
		}
	}
	if (status == 1 || argc > 0)
		fatal("usage: pwp [-l]");

	if ((cwp = getcwp()) == NULL)
		fatal("no project environment");
	if (lflag == YES)
		printf("%s\n", cwp);
	else	{
		for (;;)
			{
			strcpy(lastcwp, cwp);
			chproject(PARENTPROJECT);
			cwp = getcwp();
			chdir(cwp);
			if (EQUAL(cwp, lastcwp))
				break;		/* reached home directory */
			relpath = xorpath(cwp, lastcwp);
			if ((pldp = openpdb(PLDNAME, CURDIR, "r")) == NULL)
				exit(errpdb((PDB *) NULL));
			while (pgetent(pldp) != EOF)
				{
				if (pbfndflag(PROOTDIR) == NO)
					continue;
				if (pbcmpfield(relpath, pbfndstring(PDIRPATH)) == 0)
					break;
				}
			ppptr = prepend(PPATHSEP, prepend(pbgetkey(alias), ppptr));
			closepdb(pldp);
			}
		if (!EQUAL(cwp, gethdir(CNULL)))
			if ((pw = getpwdir(cwp)) == NULL)
				{
				warn("can't find %s home directory in %s", cwp,
				     PASSWDFILE);
				}
			else	{
				ppptr = prepend(USERPROJECT, prepend(pw->pw_name, ppptr));
				}
		printf("%s\n", (*ppptr == '\0') ? ROOTPROJECT : ppptr);
		}
	exit(0);
}



/*
 * prepend() tacks a project alias onto the front of a project pathname.
 */
static char *
prepend(alias, ppathname)
	register char *alias;
	register char *ppathname;
{
	register int i;			/* alias name size counter */

	for (i = 0; *alias != '\0'; i++, alias++)
		continue;
	while (i-- > 0)
		*--ppathname = *--alias;
	return(ppathname);
}
