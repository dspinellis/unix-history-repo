/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * openpdb() opens a database named by name located in a directory with
 * name path and associates a database stream with it. Returns a pointer
 * which identifies the database stream in subsequent operations. A null
 * pointer is returned if the database cannot be accessed or is already
 * open for writing or appending. When writing or appending, a temporary
 * file with name name_temp is created in the same directory as the database.
 */
#include <stdio.h>
#include "null.h"
#include "path.h"
#include "pdb.h"
#include "system.h"
#include "yesno.h"

extern int errno;
extern int sys_nerr;
extern char *sys_errlist[];

char PDBERR[PDBERRSIZE];		/* database error message buffer */

PDB *
openpdb(name, path, mode)
	char *name;			/* database name */
	char *path;			/* directory pathname to database */
	register char *mode;		/* mode of access */
{
	char *malloc();			/* memory allocator */
	char *pathcat();		/* pathname concatenation */
	char *sprintf();		/* print output to string */
	char *strcat();			/* string concatenation */
	char *strcpy();			/* string copy */
	char *strncpy();		/* string copy n characters */
	char tname[15];			/* temporary database name */
	FILE *fopen();			/* open file */
	PDB *pdbp;			/* database stream */

	if ((pdbp = (PDB *) malloc(sizeof(PDB))) == NULL)
		{
		sprintf(PDBERR, "out of memory");
		return(NULL);
		}
	pathcat(pdbp->path, path, name);
	if (mode[0]=='r' && mode[1]=='\0')
		{
		if ((pdbp->fp = fopen(pdbp->path, mode)) == NULL)
			{
			if (errno < sys_nerr)
				{
				sprintf(PDBERR, "%s: %s", pdbp->path,
					sys_errlist[errno]);
				}
			else	{
				sprintf(PDBERR, "can't open %s", pdbp->path);
				}
			free((char *) pdbp);
			return(NULL);
			}
		}
	else if (mode[0]=='w' || mode[0]=='a' || (mode[0]=='r' && mode[1]=='w'))
		{
		strncpy(tname, name, 8);
		tname[8] = '\0';
		strcat(tname, "_temp");
		pathcat(pdbp->tpath, path, tname);
		if (FILEXIST(pdbp->tpath))
			{
			sprintf(PDBERR,"%s temporarily unavailable",pdbp->path);
			free((char *) pdbp);
			return(NULL);
			}
		if ((pdbp->fp = fopen(pdbp->path, mode)) == NULL)
			{
			if (errno < sys_nerr)
				{
				sprintf(PDBERR, "%s: %s", pdbp->path,
					sys_errlist[errno]);
				}
			else	{
				sprintf(PDBERR, "can't open %s", pdbp->path);
				}
			free((char *) pdbp);
			return(NULL);
			}
		if ((pdbp->tfp = fopen(pdbp->tpath, "w")) == NULL)
			{
			if (errno < sys_nerr)
				{
				sprintf(PDBERR, "%s: %s", pdbp->path,
					sys_errlist[errno]);
				}
			else	{
				sprintf(PDBERR, "can't open %s", pdbp->path);
				}
			fclose(pdbp->fp);
			free((char *) pdbp);
			return(NULL);
			}
		if (mode[0]=='w' || mode[0]=='a')
			fclose(pdbp->tfp);
		}
	else	{
		sprintf(PDBERR, "bad mode %s opening %s", mode, pdbp->path);
		free((char *) pdbp);
		return(NULL);
		}
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
	*pdbp->perr = '\0';
	return(pdbp);
}
