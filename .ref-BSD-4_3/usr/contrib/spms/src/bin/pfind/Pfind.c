static char *rcsid = "$Header$";
/*
 * pfind - find files in projects
 *
 * Author: Peter J. Nicklin
 */
#include <sys/types.h>
#include <sys/dir.h>
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

char *PGN = "pfind";			/* program name */
int LONGFORMAT = 0;			/* absolute regular pathname of file? */
PDTYP PDIRTYP;				/* project directory type labels list */

main(argc, argv)
	int argc;
	char **argv;
{
	extern int PPDEBUG;		/* project pathname debug flag */
	char *getcwp();			/* get current working project */
	char *ppathname = NULL;		/* project pathname */
	int dirsearch();		/* search a directory for files */
	int pdtparse();			/* parse boolean type label expr */
	int projectsearch();		/* search a project for files */
	int status = 0;			/* exit status */
	int xppath();			/* expand project pathname */
	PATH pathbuf;			/* pathname struct buffer */
	void bininit();			/* initialize args for binary search */

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
				case 'T':
					if (pdtparse(GETARG(s), &PDIRTYP) == NO)
						status = 1;
					goto endfor;
				case 'l':
					LONGFORMAT++;
					break;
				default:
					warn("bad option -%c", *s);
					status = 1;
					goto endfor;
				}
		endfor: continue;
		}
	}
	if (status == 1 || argc < 1)
		fatal("usage: pfind [-l] [-P pdirname] [-T typexpr] file ...");

	if (ppathname == NULL)
		{
		if (getcwp() == NULL)
			fatal("no project environment");
		ppathname = CURPROJECT;
		}

	/* initialize arguments for binary searching */
	bininit(argc, argv);

	/*
	 * convert project pathname to regular pathname and search
	 * project or directory.
	 */
	 if (xppath(ppathname, &pathbuf) == -1)
		{
		patherr(ppathname);
		exit(1);
		}
	switch (pathbuf.p_mode & P_IFMT)
		{
		case P_IFNEW:
		case P_IFREG:
			fatal("%s: no such project or project directory", ppathname);
		case P_IFPDIR:
			status |= dirsearch(ppathname, pathbuf.p_path);
			break;
		case P_IFHOME:
		case P_IFPROOT:
			status |= projectsearch(ppathname, pathbuf.p_path);
			break;
		}
	exit(status);
}



/*
 * dirsearch() searches a directory for filenames. Returns 1 if can't
 * open directory, otherwise 0.
 */
dirsearch(ppathname, pathname)
	char *ppathname;		/* project pathname */
	char *pathname;			/* directory pathname */
{
	DIR *dirp;			/* directory stream */
	DIR *opendir();			/* open directory stream */
	int binsearch();		/* binary search */
	struct direct *dp;		/* directory entry pointer */
	struct direct *readdir();	/* read a directory entry */

	if ((dirp = opendir(pathname)) == NULL)
		{
		warn("can't open %s", pathname);
		return(1);
		}
	for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp))
		if (binsearch(dp->d_name) == YES)
			if (LONGFORMAT)
				printf("%s/%s\n", pathname, dp->d_name);
			else
				printf("%s/%s\n", ppathname, dp->d_name);
	closedir(dirp);
	return(0);
}



/*
 * projectsearch() searches a project for specified filenames.
 */
projectsearch(ppathname, pathname)
	char *ppathname;		/* project pathname */
	char *pathname;			/* project root directory pathname */
{
	char ppathbuf[PPATHSIZE];	/* project pathname buffer */
	char *ppathcat();		/* project pathname concatenation */
	char *slsprepend();		/* append key+string */
	int closepdb();			/* close database */
	int dirsearch();		/* search a directory for files */
	int errpdb();			/* print database error message */
	int pdtmatch();			/* match project dir type label expr */
	int status = 0;			/* return status */
	PATH *pd;			/* pathname struct pointer */
	PATH *readpld();		/* read project link directory entry */
	PDB *openpdb();			/* open database */
	PDB *pldp;			/* project link directory stream */
	SLSBLK *pblk;			/* project list block */
	SLSLIST *plist;			/* project list */
	SLSLIST *slsinit();		/* initialize key+string list */
	void slsrm();			/* remove key+string list */

	plist = slsinit();

	/* read PLDNAME project link directory */
	if ((pldp = openpdb(PLDNAME, pathname, "r")) == NULL)
		return(errpdb((PDB *) NULL));
	while ((pd = readpld(pldp)) != NULL)
		{
		if (EQUAL(pd->p_alias, PARENTPROJECT))
			continue;
		else if (EQUAL(pd->p_alias, CURPROJECT))
			{
			if (PDIRTYP.pfxsize==0 || pdtmatch(&PDIRTYP,pd->p_type)==YES)
				status |= dirsearch(ppathname, pd->p_path);
			}
		else if (pd->p_mode == P_IFPROOT)
			{
			if (slsprepend(pd->p_alias, pd->p_path, plist) == NULL)
				exit(1);
			}
		else if (PDIRTYP.pfxsize==0 || pdtmatch(&PDIRTYP,pd->p_type)==YES)
			{
			ppathcat(ppathbuf, ppathname, pd->p_alias);
			status |= dirsearch(ppathbuf, pd->p_path);
			}
		}
	status |= closepdb(pldp);

	for (pblk = plist->head; pblk != NULL; pblk = pblk->next)
		{
		ppathcat(ppathbuf, ppathname,  pblk->key);
		status |= projectsearch(ppathbuf, pblk->string);
		}
	slsrm(CNULL, plist);

	return(status);
}
