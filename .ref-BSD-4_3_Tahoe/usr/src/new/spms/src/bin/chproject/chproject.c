static char *rcsid = "$Header$";
/*
 * chproject - change current project
 *
 * Author: Peter J. Nicklin
 */
#include <stdio.h>
#include "bin.h"
#include "macro.h"
#include "path.h"
#include "pdb.h"
#include "pld.h"
#include "spms.h"
#include "system.h"
#include "yesno.h"

char *PGN;				/* program name */
int CSHELL = 0;				/* C shell command interpreter flag */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int PPDEBUG;		/* project pathname debug flag */
	char *getcwp();			/* get current working project */
	char *getshell();		/* get command shell pathname */
	char *pathtail();		/* remove pathname head */
	int chproject();		/* change project */
	int dflag = NO;			/* project description flag */
	int fflag = YES;		/* execute PROJECTRC file */
	int status = 0;			/* exit status */
	void mustchdir();		/* must change directory */
	void printPROJECT();		/* print PROJECT value */
	void printcd();			/* print "cd" command */
	void printdesc();		/* print project description */
	void printrc();			/* print PROJECTRC execution request */

	PGN = pathtail(*argv);
	if (EQUAL(pathtail(getshell()), pathtail(CSH))) CSHELL++;

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
				case 'f':
					fflag = NO;
					break;
				default:
					warn("bad option -%c", *s);
					status = 1;
					goto endfor;
				}
		endfor: continue;
		}
	}
	if (status == 1 || argc != 1)
		fatal("usage: chproject [-df] projectname");
	if (chproject(*argv) == NO)
		exit(1);
	mustchdir(getcwp());
	printcd(getcwp());
	printPROJECT();
	if (fflag == YES && FILEXIST(PROJECTRC))
		printrc();
	if (dflag == YES) printdesc();
	exit(0);
}



/*
 * mustchdir() must change current working directory or die.
 */
void
mustchdir(pathname)
	char *pathname;			/* destination directory */
{
	if (!CHDIR(pathname))
		fatal("permission denied");
}



/*
 * printPROJECT prints PROJECT environment variable value.
 */
void
printPROJECT()
{
	char *getcwp();			/* get current working project */

	if (CSHELL)
		printf("; setenv PROJECT ");
	else
		printf("; export PROJECT; PROJECT=");

       printf("%s", getcwp());
}



/*
 * printcd() prints the "cd" change directory command.
 */
void
printcd(pathname)
	char *pathname;			/* pathname of destination directory */
{
	printf("cd %s", pathname);
}



/*
 * printrc() prints the PROJECTRC project initialization file execution
 * request.
 */
void
printrc()
{
	if (CSHELL)
		printf("; source %s", PROJECTRC);
	else
		printf("; . %s", PROJECTRC);
}



/*
 * printdesc() prints project description.
 */
void
printdesc()
{
	char dirdesc[DIRDESCSIZE];	/* project directory description */
	char *pbgetstring();		/* get specified string field */
	int errpdb();			/* print database error message */
	int pfndent();			/* find and load database entry */
	PDB *openpdb();			/* open database */
	PDB *pldp;			/* project link directory stream */

	if ((pldp = openpdb(PLDNAME, CURDIR, "r")) == NULL)
		exit(errpdb((PDB *) NULL));
	if (pfndent(CURPROJECT, pldp) == NO)
		fatal("can't find %s alias in %s", CURPROJECT, pldp->path);
	if (*pbgetstring(PDIRDESC, dirdesc) != '\0')
		fprintf(stderr, "%s\n", dirdesc);
}
