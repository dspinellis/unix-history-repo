/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * readpld() reads next project link directory entry. Returns a pointer
 * to a PATH struct, or NULL at EOF.
 */
#include <stdio.h>
#include "null.h"
#include "path.h"
#include "pdb.h"
#include "pdbuf.h"
#include "pld.h"
#include "yesno.h"

PATH *
readpld(pldp)
	PDB *pldp;			/* database stream */
{
	static PATH pathbuf;		/* project directory struct */
	char *optpath();		/* pathname optimization */
	char pathname[PATHSIZE];	/* pathname buffer */
	char *pathcat();		/* pathname concatenation */
	char *pbgetkey();		/* get next key */
	char *pbgetstring();		/* get specified string field */
	char *strcpy();			/* string copy */
	int pbfndflag();		/* find flag field */
	int pgetent();			/* load next entry into buffer */
	int strlen();			/* string length */

	if (pgetent(pldp) == EOF)
		return(NULL);
	if (pbfndflag(PROOTDIR) == YES)
		pathbuf.p_mode = P_IFPROOT;
	else
		pathbuf.p_mode = P_IFPDIR;
	pathbuf.p_alias = pathbuf.p_buf;
	pbgetkey(pathbuf.p_alias);
	pathbuf.p_path = pathbuf.p_alias + strlen(pathbuf.p_alias) + 1;
	pbgetstring(PDIRPATH, pathname);
	if (*pathname == _RDIRC)
		strcpy(pathbuf.p_path, pathname);
	else
		pathcat(pathbuf.p_path, pldp->root, pathname);
	optpath(pathbuf.p_path);
	pathbuf.p_type = pathbuf.p_path + strlen(pathbuf.p_path) + 1;
	pbgetstring(PDIRTYPE, pathbuf.p_type);
	pathbuf.p_desc = pathbuf.p_type + strlen(pathbuf.p_type) + 1;
	pbgetstring(PDIRDESC, pathbuf.p_desc);
	return(&pathbuf);
}
