/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */
#include <signal.h>
#include <stdio.h>
#include "Mkmf.h"
#include "hash.h"
#include "macro.h"
#include "null.h"
#include "slist.h"
#include "system.h"
#include "yesno.h"

static char *Mftemp;			/* temporary makefile */
static char *Dftemp;			/* dependency list */

/*
 * editmf() replaces macro definitions within a makefile.
 */
void
editmf(mfname, mfpath)
	char *mfname;			/* makefile name */
	char *mfpath;			/* makefile template pathname */
{
	extern int DEPEND;		/* dependency analysis? */
	extern SLIST *EXTLIST;		/* external header file name list */
	extern SLIST *HEADLIST;		/* header file name list */
	extern SLIST *LIBLIST;		/* library pathname list */
	extern SLIST *SRCLIST;		/* source file name list */
	extern HASH *MDEFTABLE;		/* macro definition table */
	char *findmacro();		/* is the line a macro definition? */
	char *getlin();			/* get a line from input stream */
	char *mktemp();			/* make file name */
	char mnam[MACRONAMSIZE];	/* macro name buffer */
	FILE *dfp;			/* dependency file stream */
	FILE *ifp;			/* input stream */
	FILE *mustfopen();		/* must open file or die */
	FILE *ofp;			/* output stream */
	HASHBLK *htb;			/* hash table block */
	HASHBLK *htlookup();		/* find hash table entry */
	int dependmark();		/* generated dependency line? */
	int fastcopy();			/* copy a file to output stream */
	int rmtemp();			/* remove temporary makefile */
	void mkdepend();		/* generate object-include file deps */
	void purgcontinue();		/* get rid of continuation lines */
	void putmacro();		/* put macro defs from table */
	void putlin();			/* put a makefile line */
	void putobjmacro();		/* put object file name macro def */
	void putslmacro();		/* put macro defs from linked list */

	ifp = mustfopen(mfpath, "r");
	Mftemp = mktemp("mkmfXXXXXX");
	Dftemp = mktemp("/tmp/mkmfXXXXXX");

	signal(SIGHUP, rmtemp);
	signal(SIGINT, rmtemp);
	signal(SIGQUIT, rmtemp);

	ofp = mustfopen(Mftemp, "w");
	if (DEPEND)
		{
		dfp = mustfopen(Dftemp, "w");
		mkdepend(dfp);
		fclose(dfp);
		}

	while (getlin(ifp) != NULL)
		{
		if (DEPEND && dependmark())
			break;
		if (findmacro(mnam) != NULL)
			if ((htb = htlookup(mnam, MDEFTABLE)) != NULL)
				{
				if (htb->h_val == VREADWRITE)
					{
					putmacro(htb->h_def, ofp);
					purgcontinue(ifp);
					}
				else	{
					putlin(ofp);
					}
				}
			else if (EQUAL(mnam, MHEADERS))
				{
				putslmacro(HEADLIST, ofp);
				purgcontinue(ifp);
				}
			else if (EQUAL(mnam, MOBJECTS))
				{
				putobjmacro(ofp);
				purgcontinue(ifp);
				}
			else if (EQUAL(mnam, MSOURCE))
				{
				putslmacro(SRCLIST, ofp);
				purgcontinue(ifp);
				}
			else if (EQUAL(mnam, MEXTERNALS))
				{
				if (DEPEND)
					{
					putslmacro(EXTLIST, ofp);
					purgcontinue(ifp);
					}
				else
					putlin(ofp);
				}
			else if (EQUAL(mnam, MLIBLIST) && LIBLIST != NULL)
				{
				putslmacro(LIBLIST, ofp);
				purgcontinue(ifp);
				}
			else
				putlin(ofp);
		else
			putlin(ofp);
		}
	fclose(ifp);
	if (DEPEND)
		{
		fflush(ofp);
		fastcopy(Dftemp, ofp);
		unlink(Dftemp);
		}
	fclose(ofp);

	signal(SIGHUP, SIG_IGN);
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);

	RENAME(Mftemp, mfname);
}



/*
 * rmtemp() removes the temporary makefile and dependency file, and
 * calls exit(1).
 */
rmtemp()
{
	signal(SIGHUP, rmtemp);
	signal(SIGINT, rmtemp);
	signal(SIGQUIT, rmtemp);

	unlink(Mftemp);
	unlink(Dftemp);
	exit(1);
}
