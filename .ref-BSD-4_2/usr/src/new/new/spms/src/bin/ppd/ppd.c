static char *rcsid = "$Header$";
/*
 * ppd - list project directories
 *
 * Author: Peter J. Nicklin
 */
#include <stdio.h>
#include "getarg.h"
#include "macro.h"
#include "null.h"
#include "path.h"
#include "pdb.h"
#include "pdtyp.h"
#include "pld.h"
#include "slslist.h"
#include "spms.h"
#include "yesno.h"

#define MAXLINE			80	/* maximum output line size */
#define TABSIZE			8	/* default tab width */
#define MINIMUM_GAP		2	/* minimum space between items */

/*
 * Information request definitions
 */
#define ABSOLUTE_PATH_INFO	1	/* list absolute pathnames */
#define ALIAS_INFO		2	/* list aliases + absolute pathnames */
#define DESCRIPTION_INFO	3	/* list directory descriptions */
#define REGULAR_INFO		4	/* list project directory aliases */
#define TYPE_LABEL_INFO		5	/* list type labels */

char *PGN = "ppd";			/* program name */
int INFORMATION = REGULAR_INFO;		/* type of information to be printed */
int LIST_ALL_ENTRIES = NO;		/* print "..." && "...." ? */
int LIST_PLD_CONTENTS = YES;		/* list project link dir contents */
int MARK_PROJECT_ROOT = 0;		/* append project root dirs with `^' */
int ONE_ENTRY_PER_LINE = 0;		/* print 1 project directory/line */
int PRINT_HEADING = YES;		/* print headings for projects */
int RECURSIVE = 0;			/* recursively print projects */
PDTYP PDIRTYP;				/* project directory type labels list */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int PPDEBUG;		/* project pathname debug flag */
	char *cwp;			/* current working project */
	char *getcwp();			/* get current working project */
	char *slsappend();		/* append key+string */
	int pdtolist();			/* add project directories to pdlist */
	int pdtparse();			/* parse boolean type label expr */
	int qsort();			/* quicker sort */
	int readpath();			/* read project or regular pathname */
	int status = 0;			/* exit status */
	int strpcmp();			/* compare pointed-to strings */
	PATH pathbuf;			/* pathname struct buffer */
	SLSBLK *pblk;			/* project list block */
	SLSLIST *pdlist;		/* project directory list */
	SLSLIST *plist;			/* project list */
	SLSLIST *slsinit();		/* initialize key+string list */
	void printlist();		/* print project directory list */
	void slsrm();			/* remove key+string list */

	{
	register char *s;		/* option pointer */
	while (--argc > 0 && **++argv == '-')
		{
		for (s = argv[0]+1; *s != '\0'; s++)
			switch (*s)
				{
				case '1':
					ONE_ENTRY_PER_LINE++;
					break;
				case 'D':
					PPDEBUG = YES;
					break;
				case 'T':
					if (pdtparse(GETARG(s), &PDIRTYP) == NO)
						status = 1;
					goto endfor;
				case 'a':
					LIST_ALL_ENTRIES = YES;
					break;
				case 'd':
					INFORMATION = DESCRIPTION_INFO;
					ONE_ENTRY_PER_LINE++;
					break;
				case 'l':
					INFORMATION = ABSOLUTE_PATH_INFO;
					ONE_ENTRY_PER_LINE++;
					break;
				case 'm':
					MARK_PROJECT_ROOT++;
					break;
				case 'n':
					INFORMATION = ALIAS_INFO;
					ONE_ENTRY_PER_LINE++;
					break;
				case 'p':
					LIST_PLD_CONTENTS = NO;
					break;
				case 'q':
					PRINT_HEADING = NO;
					break;
				case 'r':
					RECURSIVE++;
					break;
				case 't':
					INFORMATION = TYPE_LABEL_INFO;
					ONE_ENTRY_PER_LINE++;
					break;
				default:
					warn("bad option -%c", *s);
					status = 1;
					goto endfor;
				}
		endfor: continue;
		}
	}
	if (status == 1)
		fatal("usage: ppd [-1adlmnpqrt] [-T typexpr] [pdirname ...]");

	if (argc < 1)
		{
		if ((cwp = getcwp()) == NULL)
			fatal("no project environment");
		status |= listproject("", cwp);
		exit(status);
		}

	pdlist = slsinit();
	plist = slsinit();
	qsort((char *) argv, argc, sizeof(char *), strpcmp);
	for (; argc > 0; argc--, argv++)
		if (readpath(*argv, &pathbuf) == -1)
			{
			patherr(*argv);
			status = 1;
			}
		else switch (pathbuf.p_mode & P_IFMT)
			{
			case P_IFNEW:
			case P_IFREG:
				warn("%s: no such project or project directory", *argv);
				status = 1;
				break;
			case P_IFHOME:
			case P_IFPROOT:
				if (LIST_PLD_CONTENTS == YES)
					{
					if (slsappend(*argv, pathbuf.p_path,
					    plist) == NULL)
						exit(1);
					}
				else	{
					status |= pdtolist(*argv, &pathbuf, pdlist);
					}
				break;
			case P_IFPDIR:
				status |= pdtolist(*argv, &pathbuf, pdlist);
				break;
			}

	/* don't bother to print heading if single project request */
	if (RECURSIVE == 0 && SLSNUM(pdlist) == 0 && SLSNUM(plist) == 1)
		PRINT_HEADING = NO;

	/* print discrete project directories */
	printlist("", pdlist);
	slsrm(CNULL, pdlist);

	/* print projects */
	for (pblk = plist->head; pblk != NULL; pblk = pblk->next)
		status |= listproject(pblk->key, pblk->string);
	exit(status);
}



/*
 * getpdesc() gets a project description. Returns constant 1 if error,
 * otherwise 0.
 */
getpdesc(desc, pathname)
	char *desc;			/* description receiving buffer */
	char *pathname;			/* project link directory pathname */
{
	char *pbgetstring();		/* get specified string field */
	int closepdb();			/* close database */
	int errpdb();			/* print database error message */
	int pfndent();			/* find and load database entry */
	PDB *pldp;			/* project link directory stream */
	PDB *openpdb();			/* open database */

	*desc = '\0';
	if ((pldp = openpdb(PLDNAME, pathname, "r")) == NULL)
		return(errpdb((PDB *) NULL));
	if (pfndent(CURPROJECT, pldp) == YES)
		pbgetstring(PDIRDESC, desc);
	return(closepdb(pldp));
}



/*
 * getptype() gets project root directory types. Returns constant 1 if error,
 * otherwise 0.
 */
getptype(type, pathname)
	char *type;			/* type receiving buffer */
	char *pathname;			/* project link directory pathname */
{
	char *pbgetstring();		/* get specified string field */
	int closepdb();			/* close database */
	int errpdb();			/* print database error message */
	int pfndent();			/* find and load database entry */
	PDB *pldp;			/* project link directory stream */
	PDB *openpdb();			/* open database */

	*type = '\0';
	if ((pldp = openpdb(PLDNAME, pathname, "r")) == NULL)
		return(errpdb((PDB *) NULL));
	if (pfndent(CURPROJECT, pldp) == YES)
		pbgetstring(PDIRTYPE, type);
	return(closepdb(pldp));
}



/*
 * ksprint() prints a list of key+string pairs (one pair per line).
 */
void
ksprint(colwidth, pdlist)
	int colwidth;			/* maximum column width */
	SLSLIST *pdlist;		/* project directory list */
{
	register char *kp;		/* key pointer */
	register char *tp;		/* type label pointer */
	register int cw;		/* column width */
	SLSBLK *curblk;			/* current list block */

	for (curblk = pdlist->head; curblk != NULL; curblk = curblk->next)
		{
		for (cw=colwidth, kp=curblk->key; *kp != '\0' && cw-- > 0; kp++)
			putchar(*kp);
		if (cw > 0 && *curblk->string != '\0')
			for(; cw > 0; cw -= TABSIZE)
				putchar('\t');
		if (INFORMATION == TYPE_LABEL_INFO)
			{
			for (tp = curblk->string; *tp != '\0'; tp++)
				if (*tp == _PDTSC)
					{
					putchar(',');
					putchar(' ');
					}
				else	{
					putchar(*tp);
					}
			putchar('\n');
			}
		else	{
			puts(curblk->string);
			}
		}
}



/*
 * listproject() lists a project link directory.
 */
listproject(ppathname, pathname)
	char *ppathname;		/* project pathname */
	char *pathname;			/* regular pathname */
{
	register char *alias;		/* alias buffer pointer */
	register char *path;		/* path buffer pointer */
	char aliasbuf[ALIASSIZE+1];	/* alias marking buffer */
	char descbuf[DIRDESCSIZE];	/* project root directory description */
	char *kp;			/* key pointer */
	char pathbuf[PATHSIZE+1];	/* regular pathname marking buffer */
	char ppathbuf[PPATHSIZE];	/* project path concatenation buffer */
	char *ppathcat();		/* project pathname concatenation */
	char *slsappend();		/* append key+string */
	char *strcat();			/* string concatenation */
	char *strcpy();			/* string copy */
	char typebuf[TYPBUFSIZE];	/* project root directory types */
	int closepdb();			/* close database */
	int errpdb();			/* print database error message */
	int getpdesc();			/* get project description */
	int getptype();			/* get project root directory types */
	int pdtmatch();			/* match project dir type label expr */
	int slssort();			/* sort key+string list */
	int status = 0;			/* return status */
	int strcmp();			/* string comparison */
	PATH *pd;			/* pathname struct pointer */
	PATH *readpld();		/* read project link directory entry */
	PDB *openpdb();			/* open database */
	PDB *pldp;			/* project link directory stream */
	SLSBLK *pblk;			/* project list block */
	SLSLIST *pdlist;		/* project directory list */
	SLSLIST *plist;			/* project list */
	SLSLIST *slsinit();		/* initialize key+string list */
	void printlist();		/* print project directory list */
	void slsrm();			/* remove key+string list */

	pdlist = slsinit();
	plist = slsinit();

	/* read PLDNAME project link directory */
	if ((pldp = openpdb(PLDNAME, pathname, "r")) == NULL)
		return(errpdb((PDB *) NULL));
	while ((pd = readpld(pldp)) != NULL)
		{
		alias = pd->p_alias;
		path = pd->p_path;
		if (EQUAL(alias, PARENTPROJECT))
			{
			if (LIST_ALL_ENTRIES == NO)
				continue;
			}
		else if (EQUAL(alias, CURPROJECT))
			{
			if (PDIRTYP.pfxsize == 0 && LIST_ALL_ENTRIES == NO)
				continue;
			}
		else if (RECURSIVE && pd->p_mode == P_IFPROOT)
			if (slsappend(alias, path, plist) == NULL)
				exit(1);

		if (PDIRTYP.pfxsize != 0 && pdtmatch(&PDIRTYP,pd->p_type) == NO)
			continue;

		if (MARK_PROJECT_ROOT)
			if (pd->p_mode == P_IFPROOT)
				if (INFORMATION == ABSOLUTE_PATH_INFO)
					{
					path = strcpy(pathbuf, path);
					strcat(path, ROOTPROJECT);
					}
				else	{
					alias = strcpy(aliasbuf, alias);
					strcat(alias, ROOTPROJECT);
					}

		switch (INFORMATION)
			{
			case REGULAR_INFO:
				kp = slsappend(alias, "", pdlist);
				break;
			case ABSOLUTE_PATH_INFO:
				kp = slsappend(path, "", pdlist);
				break;
			case ALIAS_INFO:
				kp = slsappend(alias, path, pdlist);
				break;
			case TYPE_LABEL_INFO:
				if (pd->p_mode == P_IFPDIR)
					{
					kp=slsappend(alias, pd->p_type, pdlist);
					}
				else	{
					status |= getptype(typebuf, path);
					kp=slsappend(alias, typebuf, pdlist);
					}
				break;
			case DESCRIPTION_INFO:
				if (pd->p_mode == P_IFPDIR)
					{
					kp=slsappend(alias, pd->p_desc, pdlist);
					}
				else	{
					status |= getpdesc(descbuf, path);
					kp=slsappend(alias, descbuf, pdlist);
					}
				break;
			}
		if (kp == NULL)
			exit(1);
		}
	status |= closepdb(pldp);

	/* sort and print project directories */
	if (slssort(strcmp, pdlist) == NO)
		exit(1);
	printlist(ppathname, pdlist);
	slsrm(CNULL, pdlist);

	/* if RECURSIVE, list subprojects */
	if (RECURSIVE)
		{
		if (slssort(strcmp, plist) == NO)
			exit(1);
		for (pblk = plist->head; pblk != NULL; pblk = pblk->next)
			{
			ppathcat(ppathbuf, ppathname, pblk->key);
			status |= listproject(ppathbuf, pblk->string);
			}
		}
	slsrm(CNULL, plist);

	return(status);
}



/*
 * pdtolist() adds project (root) directories to pdlist. Returns
 * constant 1 if error, otherwise 0.
 */
pdtolist(ppathname, pb, pdlist)
	char *ppathname;		/* project pathname */
	PATH *pb;			/* pathname struct buffer */
	SLSLIST *pdlist;		/* project directory list */
{
	char *kp;			/* key pointer */
	char *slsappend();		/* append key+string */
	char *strcat();			/* string concatenation */
	int status = 0;			/* return status */
	unsigned long pathtyp;		/* type of pathname */

	pathtyp = pb->p_mode & P_IFMT;

	if (MARK_PROJECT_ROOT)
		if (pathtyp == P_IFHOME || pathtyp == P_IFPROOT)
			if (INFORMATION == ABSOLUTE_PATH_INFO)
				strcat(pb->p_path, ROOTPROJECT);
			else
				strcat(ppathname, ROOTPROJECT);
	switch (INFORMATION)
		{
		case REGULAR_INFO:
			kp = slsappend(ppathname, "", pdlist);
			break;
		case ABSOLUTE_PATH_INFO:
			kp = slsappend(pb->p_path, "", pdlist);
			break;
		case ALIAS_INFO:
			kp = slsappend(ppathname, pb->p_path, pdlist);
			break;
		case TYPE_LABEL_INFO:
			kp = slsappend(ppathname, pb->p_type, pdlist);
			break;
		case DESCRIPTION_INFO:
			kp = slsappend(ppathname, pb->p_desc, pdlist);
			break;
		}
	if (kp == NULL)
		exit(1);
	return(status);
}



/*
 * printlist() prints out a list of project directories.
 */
void
printlist(ppathname, pdlist)
	char *ppathname;		/* project pathname */
	SLSLIST *pdlist;		/* project directory list */
{
	static int have_printed;	/* has printing already been done? */
	int colwidth;			/* maximum column width */
	int ncol;			/* number of columns */
	void ksprint();			/* print list of key+string pairs */
	void slsprint();		/* print key+string list (key only) */

	colwidth = pdlist->maxkey + MINIMUM_GAP;
	if (colwidth % TABSIZE) colwidth = TABSIZE * (colwidth/TABSIZE + 1);

	if (*ppathname != '\0' && PRINT_HEADING == YES)
		printf((have_printed) ? "\n%s:\n" : "%s:\n", ppathname);

	if (INFORMATION == REGULAR_INFO || INFORMATION == ABSOLUTE_PATH_INFO)
		if (ONE_ENTRY_PER_LINE)
			slsprint(1, colwidth, YES, stdout, pdlist);
		else	{
			ncol = MAXLINE / colwidth;
			slsprint(ncol, colwidth, YES, stdout, pdlist);
			}
	else
		ksprint(colwidth, pdlist);

	if (SLSNUM(pdlist) > 0)
		have_printed = 1;
}



/*
 * strpcmp() compares strings stored in a pointer array. Returns whatever
 * strcmp() returns.
 */
strpcmp(p1, p2)
	char **p1, **p2;		/* string pointers */
{
	int strcmp();			/* string comparison */

	return(strcmp(*p1, *p2));
}
