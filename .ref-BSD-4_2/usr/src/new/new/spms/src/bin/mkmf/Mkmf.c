static char *rcsid = "$Header$";
/*
 * mkmf - makefile editor
 *
 * Author: Peter J. Nicklin
 */
#include "Mkmf.h"
#include "getarg.h"
#include "hash.h"
#include "null.h"
#include "path.h"
#include "target.h"
#include "slist.h"
#include "suffix.h"
#include "system.h"
#include "yesno.h"

char *L_MAKEFILE = "l.Makefile";	/* default library makefile template */
char OBJSFX[SUFFIXSIZE] = ".o";		/* default object suffix */
char *P_MAKEFILE = "p.Makefile";	/* default program makefile template */
char *PGN = "mkmf";			/* program name */
int CFLAG = YES;			/* makefile creation message flag */
int DEPEND = 1;				/* dependency analysis? */
SLIST *HEADLIST;			/* header file name list */
SLIST *LIBLIST;				/* library pathname list */
SLIST *SRCLIST;				/* source file name list */
HASH *MDEFTABLE;			/* macro definition table */
SUFFIX DEFSFX[] =			/* default suffix list */
	{
#include "defaultsfx.h"
	NULL, 0
	};

main(argc, argv)
	int argc;
	char **argv;
{
	char *mfname = NULL;		/* makefile name */
	char mfpath[PATHSIZE];		/* makefile template pathname */
	HASHBLK *htb;			/* hash table block */
	HASHBLK *htinstall();		/* install hash table entry */
	HASHBLK *htlookup();		/* find hash table entry */
	int buildliblist();		/* build list of library pathnames */
	int buildsfxtable();		/* build table of suffixes */
	int buildsrclist();		/* build list of source file names */
	int status = 0;			/* exit status */
	int storemacro();		/* store macro definition */
	short iflag = NO;		/* interactive flag */
	TARGET target;			/* type of makefile target */
	void answer();			/* install answer in macro def table */
	void editmf();			/* edit makefile */
	void findmf();			/* find makefile */

	target.type = target.dest = VUNKNOWN;

	{
	register char *s;		/* option pointer */
	while (--argc > 0 && **++argv == '-')
		{
		for (s = argv[0]+1; *s != '\0'; s++)
			switch (*s)
				{
				case 'F':
					P_MAKEFILE = L_MAKEFILE = GETARG(s);
					goto endfor;
				case 'c':
					CFLAG = NO;
					break;
				case 'd':
					/* turn OFF dependency analysis */
					DEPEND--;
					break;
				case 'f':
					mfname = GETARG(s);
					goto endfor;
				case 'i':
					iflag = YES;
					break;
				case 'l':
					target.type = VLIBRARY;
					break;
				default:
					warn("bad option -%c", *s);
					status = 1;
					goto endfor;
				}
		endfor: continue;
		}
	}
	
	/* initialize macro definition table */
	MDEFTABLE = htinit(MDEFTABLESIZE);

	/* get command line macro definitions */
	for (; argc > 0; argc--, argv++)
		if (storemacro(*argv) == NO)
			{
			warn("%s not a macro definition", *argv);
			status = 1;
			}

	if (status == 1)
		fatal("usage: mkmf [-cdil] [-f makefile] [-F template] [macroname=value...]");


	/* determine the makefile name */
	if (mfname == NULL)
		if ((htb = htlookup(MMAKEFILE, MDEFTABLE)) != NULL)
			mfname = htb->h_def;
		else if (FILEXIST("makefile"))
			mfname = "makefile";
		else if (FILEXIST("Makefile"))
			mfname = "Makefile";
		else
			mfname = "Makefile";
	if (htinstall(MMAKEFILE, mfname, VREADWRITE, MDEFTABLE) == NULL)
		exit(1);


	/* find the makefile (template) and load useful macro definitions */
	if (target.type == VUNKNOWN)
		{
		if (htlookup(MPROGRAM, MDEFTABLE) != NULL)
			target.type = VPROGRAM;
		else if (htlookup(MLIBRARY, MDEFTABLE) != NULL)
			target.type = VLIBRARY;
		}
	findmf(mfname, mfpath, &target);
	

	/* interactive option */
	if (iflag == YES)
		{
		if (htlookup(MPROGRAM, MDEFTABLE) == NULL &&
		    htlookup(MLIBRARY, MDEFTABLE) == NULL)
			if (target.type == VPROGRAM)
				{
				printf("program name? ");
				answer(MPROGRAM, VREADWRITE);
				}
			else if (target.type == VLIBRARY)
				{
				printf("library name? ");
				answer(MLIBRARY, VREADWRITE);
				}
		if (htlookup(MDESTDIR, MDEFTABLE) == NULL && target.dest == VDESTDIR)
			{
			printf("destination directory? ");
			answer(MDESTDIR, VREADWRITE);
			}
		}
	

	/* build the suffix table */
	if (buildsfxtable() == NO)
		exit(1);


	/* build the source code and header file name lists */
	if (buildsrclist() == NO)
		exit(1);
	

	/* build the library pathname list */
	if (buildliblist() == NO)
		exit(1);


	/* edit makefile */
	editmf(mfname, mfpath);

	exit(0);
}
