/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */
#include <stdio.h>
#include "macro.h"
#include "null.h"
#include "path.h"
#include "pdb.h"
#include "pdlist.h"
#include "pld.h"
#include "slslist.h"
#include "spms.h"
#include "yesno.h"

/*
 * execlist() executes a project directory list. Returns non-zero error
 * status if error.
 */
execlist(pdlist)
	PDLIST *pdlist;			/* project directory list */
{
	extern int ERRSTATUS;		/* pexec error status */
	extern int EXECUTE;		/* execute command? */
	extern int PRINT_HEADING;	/* print headings for project dirs */
	int ch_dir();			/* change current working directory */
	int execcmd();			/* execute command in directory */
	int status = 0;			/* return status */
	PDBLK *pdblk;			/* project directory list block */
	void print_title();		/* print project directory title */

	for (pdblk = pdlist->head; pdblk != NULL; pdblk = pdblk->next)
		{
		if (PRINT_HEADING == YES)
			print_title(pdblk->ppath);
		if (ch_dir(pdblk->rpath) == NO)
			status = ERRSTATUS;
		else if (EXECUTE == YES)
			status |= execcmd(pdblk->project);
		}
	return(status);
}



/*
 * execproject() builds a project directory list and executes commands
 * within directories on that list.
 */
execproject(ppathname, pathname)
	char *ppathname;		/* project root dir project pathname */
	char *pathname;			/* regular project root dir pathname */
{
	extern int ERRSTATUS;		/* pexec error status */
	char ppathbuf[PPATHSIZE];	/* project pathname buffer */
	char *pdprepend();		/* prepend project directory */
	char *ppathcat();		/* project pathname concatenation */
	char *slsprepend();		/* prepend key+string */
	int closepdb();			/* close database */
	int errpdb();			/* print database error message */
	int execlist();			/* execute project directory list */
	int slssort();			/* sort list */
	int status = 0;			/* return status */
	int strcmp();			/* string comparison */
	PATH *pd;			/* pathname struct pointer */
	PATH *readpld();		/* read project link directory entry */
	PDB *openpdb();			/* open database */
	PDB *pldp;			/* project link directory stream */
	PDLIST *pdinit();		/* initialize project directory list */
	PDLIST *pdlist;			/* project directory list */
	SLSBLK *pblk;			/* project list block */
	SLSLIST *plist;			/* project list */
	SLSLIST *slsinit();		/* initialize list */
	void pdrm();			/* remove project directory list */
	void pdsort();			/* sort project directory list */
	void slsrm();			/* remove list item */

	pdlist = pdinit();
	plist = slsinit();

	/* read PLDNAME project link directory */
	if ((pldp = openpdb(PLDNAME, pathname, "r")) == NULL)
		return(errpdb((PDB *) NULL));
	while ((pd = readpld(pldp)) != NULL)
		{
		if (EQUAL(pd->p_alias, PARENTPROJECT))
			continue;

		if (EQUAL(pd->p_alias, CURPROJECT))
			{
			pdprepend(ppathname, pd->p_path, pathname, pdlist);
			}
		else if (pd->p_mode == P_IFPROOT)
			{
			if (slsprepend(pd->p_alias, pd->p_path, plist) == NULL)
				pxexit();
			}
		else	{
			ppathcat(ppathbuf, ppathname, pd->p_alias);
			pdprepend(ppathbuf, pd->p_path, pathname, pdlist);
			}
		}
	if (closepdb(pldp) != 0)
		status = ERRSTATUS;

	/* sort and execute project directories */
	pdsort(strcmp, pdlist);
	status |= execlist(pdlist);
	pdrm(pdlist);

	/* execute subprojects */
	if (slssort(strcmp, plist) == NO)
		pxexit();
	for (pblk = plist->head; pblk != NULL; pblk = pblk->next)
		{
		ppathcat(ppathbuf, ppathname, pblk->key);
		status |= execproject(ppathbuf, pblk->string);
		}
	slsrm(CNULL, plist);

	return(status);
}
