/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * readpath() loads a PATH struct, given a regular or project pathname.
 * Returns -1 if bad pathname, otherwise 0.
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "null.h"
#include "path.h"
#include "pdb.h"
#include "pdbuf.h"
#include "pld.h"
#include "spms.h"
#include "yesno.h"

extern char *PATHERR;			/* current pathname error condition */
extern char PDBERR[];			/* project database error message */

readpath(pathname, pb)
	char *pathname;			/* regular or project pathname */
	PATH *pb;			/* project directory struct buffer */
{
	char *pbgetstring();		/* get specified string field */
	int closepdb();			/* close database */
	int errpdb();			/* print database error message */
	int pfndent();			/* find and load database entry */
	int strlen();			/* string length */
	int xppath();			/* expand project pathname */
	PDB *openpdb();			/* open database */
	PDB *pldp;			/* project link directory stream */

	if (xppath(pathname, pb) == -1)
		return(-1);
	switch (pb->p_mode & P_IFMT)
		{
		case P_IFNEW:
		case P_IFREG:
			return(0);
			break;
		case P_IFPDIR:
			if ((pldp = openpdb(PLDNAME, pb->p_project, "r")) == NULL)
				{
				PATHERR = PDBERR;
				return(-1);
				}
			pfndent(pb->p_alias, pldp);
			break;
		case P_IFHOME:
		case P_IFPROOT:
			if ((pldp = openpdb(PLDNAME, pb->p_project, "r")) == NULL)
				{
				PATHERR = PDBERR;
				return(-1);
				}
			pfndent(CURPROJECT, pldp);
			break;
		}
	pbgetstring(PDIRTYPE, pb->p_type);
	pb->p_desc = pb->p_type + strlen(pb->p_type) + 1;
	pbgetstring(PDIRDESC, pb->p_desc);
	closepdb(pldp);
	return(0);
}
