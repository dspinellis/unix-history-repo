/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * getcpd() loads a PATH struct with the attributes of the current working
 * project directory. Don't bother getting  type or description (too slow
 * at present). Returns -1 on error, 0 if a regular directory, or 1
 * if a project directory.
 */
#include <stdio.h>
#include "macro.h"
#include "null.h"
#include "path.h"
#include "pdb.h"
#include "pdbuf.h"
#include "pld.h"
#include "yesno.h"

getcpd(pb)
	PATH *pb;			/* pathname buffer */
{
	char cwd[PATHSIZE];		/* current working directory */
	char *cwp;			/* current working  project */
	char *getcwp();			/* get current working project */
	char *getwd();			/* get current working directory */
	char *mkalias();		/* construct alias from pathname */
	char *pbfndstring();		/* find string field */
	char *pbgetkey();		/* get next key */
	char *relpath;			/* relative pathname */
	char *strcpy();			/* string copy */
	char *xorpath();		/* remove subpathname */
	int closepdb();			/* close database */
	int pbcmpfield();		/* compare non-key fields */
	int pbfndflag();		/* find flag field */
	int pgetent();			/* load next entry into buffer */
	PDB *pldp;			/* project link directory stream */
	PDB *openpdb();			/* open database */

	pb->p_alias = pb->p_buf;
	getwd(cwd);
	if ((cwp = getcwp()) == NULL)
		{
		pb->p_mode = P_IFREG;
		strcpy(pb->p_alias, mkalias(cwd));
		pb->p_path = pb->p_alias + strlen(pb->p_alias) + 1;
		strcpy(pb->p_path, cwd);
		return(0);
		}
	if (EQUAL(cwd, cwp))		/* we are in project root directory */
		relpath = CURDIR;
	else
		relpath = xorpath(cwp, cwd);
	if ((pldp = openpdb(PLDNAME, cwp, "r")) == NULL)
		return(-1);
	while (pgetent(pldp) != EOF)
		{
		if (pbcmpfield(relpath, pbfndstring(PDIRPATH)) == 0)
			{
			if (pbfndflag(PROOTDIR) == YES)
				pb->p_mode = P_IFPROOT;
			else
				pb->p_mode = P_IFPDIR;
			pbgetkey(pb->p_alias);
			pb->p_path = pb->p_alias + strlen(pb->p_alias) + 1;
			strcpy(pb->p_path, cwd);
			closepdb(pldp);
			return(1);
			}
		}
	closepdb(pldp);
	pb->p_mode = P_IFREG;
	strcpy(pb->p_path, cwd);
	return(0);
}
