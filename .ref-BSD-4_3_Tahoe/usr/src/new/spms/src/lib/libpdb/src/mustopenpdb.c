/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * mustopenpdb() opens a database in the manner of openpdb(). However, if the
 * database cannot be accessed, exit(1) is called.
 */
#include <stdio.h>
#include "null.h"
#include "path.h"
#include "pdb.h"
#include "system.h"
#include "yesno.h"

PDB *
mustopenpdb(name, path, mode)
	char *name;			/* database name */
	char *path;			/* directory pathname to database */
	register char *mode;		/* mode of access */
{
	char *malloc();			/* memory allocator */
	char *pathcat();		/* pathname concatenation */
	char *strcat();			/* string concatenation */
	char *strcpy();			/* string copy */
	char *strncpy();		/* string copy n characters */
	char tname[15];			/* temporary database name */
	FILE *fopen();			/* open file */
	PDB *pdbp;			/* database stream */

	if ((pdbp = (PDB *) malloc(sizeof(PDB))) == NULL)
		fatal("out of memory");
	pathcat(pdbp->path, path, name);
	if (mode[0]=='r' && mode[1]=='\0')
		{
		if ((pdbp->fp = fopen(pdbp->path, mode)) == NULL)
			{
			pperror(pdbp->path);
			exit(1);
			}
		}
	else if (mode[0]=='w' || mode[0]=='a' || (mode[0]=='r' && mode[1]=='w'))
		{
		strncpy(tname, name, 8);
		tname[8] = '\0';
		strcat(tname, "_temp");
		pathcat(pdbp->tpath, path, tname);
		if (FILEXIST(pdbp->tpath))
			fatal("%s temporarily unavailable", pdbp->path);
		if ((pdbp->fp = fopen(pdbp->path, mode)) == NULL)
			{
			pperror(pdbp->path);
			exit(1);
			}
		if ((pdbp->tfp = fopen(pdbp->tpath, "w")) == NULL)
			{
			pperror(pdbp->path);
			exit(1);
			}
		if (mode[0]=='w' || mode[0]=='a')
			fclose(pdbp->tfp);
		}
	else
		fatal("bad mode %s opening %s", mode, pdbp->path);
	pdbp->flag &= ~(_PACCESS|_PSTAT);
	switch (*mode)
		{
		case 'r':
			pdbp->flag |= _PREAD;
			if (mode[1] != 'w')
				break;
		case 'w':
			pdbp->flag |= _PWRITE;
			break;
		case 'a':
			pdbp->flag |= _PAPPEND | _PEOF;
			break;
		}
	strcpy(pdbp->root, path);
	return(pdbp);
}
