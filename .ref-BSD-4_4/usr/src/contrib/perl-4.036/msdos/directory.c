/* $RCSfile: directory.c,v $$Revision: 4.0.1.1 $$Date: 91/06/07 11:22:24 $
 *
 *    (C) Copyright 1987, 1988, 1990 Diomidis Spinellis.
 *
 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 *
 * $Log:	directory.c,v $
 * Revision 4.0.1.1  91/06/07  11:22:24  lwall
 * patch4: new copyright notice
 * 
 * Revision 4.0  91/03/20  01:34:24  lwall
 * 4.0 baseline.
 * 
 * Revision 3.0.1.1  90/03/27  16:07:37  lwall
 * patch16: MSDOS support
 * 
 * Revision 1.3  90/03/16  22:39:40  dds
 * Fixed malloc problem.
 *
 * Revision 1.2  88/07/23  00:08:39  dds
 * Added inode non-zero filling.
 *
 * Revision 1.1  88/07/23  00:03:50  dds
 * Initial revision
 *
 */

/*
 * UNIX compatible directory access functions
 */

#include <sys/types.h>
#include <sys/dir.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <dos.h>
#include <ctype.h>

/*
 * File names are converted to lowercase if the
 * CONVERT_TO_LOWER_CASE variable is defined.
 */
#define CONVERT_TO_LOWER_CASE

#define PATHLEN 65

#ifndef lint
static char rcsid[] = "$RCSfile: directory.c,v $$Revision: 4.0.1.1 $$Date: 91/06/07 11:22:24 $";
#endif

DIR *
opendir(char *filename)
{
	DIR            *p;
	char           *oldresult, *result;
	union REGS      srv;
	struct SREGS    segregs;
	register        reslen = 0;
	char            scannamespc[PATHLEN];
	char		*scanname = scannamespc;	/* To take address we need a pointer */

	/*
	 * Structure used by the MS-DOS directory system calls.
	 */
	struct dir_buff {
		char            reserved[21];	/* Reserved for MS-DOS */
		unsigned char   attribute;	/* Attribute */
		unsigned int    time;		/* Time */
		unsigned int    date;		/* Date */
		long            size;		/* Size of file */
		char            fn[13];		/* Filename */
	} buffspc, *buff = &buffspc;


	if (!(p = (DIR *) malloc(sizeof(DIR))))
		return NULL;

	/* Initialize result to use realloc on it */
	if (!(result = malloc(1))) {
		free(p);
		return NULL;
	}

	/* Create the search pattern */
	strcpy(scanname, filename);
	if (strchr("/\\", *(scanname + strlen(scanname) - 1)) == NULL)
		strcat(scanname, "/*.*");
	else
		strcat(scanname, "*.*");

	segread(&segregs);
#if ( defined(M_I86LM) || defined(M_I86CM) || defined(M_I86HM) )
	segregs.ds = FP_SEG(buff);
	srv.x.dx = FP_OFF(buff);
#else
	srv.x.dx = (unsigned int) buff;
#endif
	srv.h.ah = 0x1a;	/* Set DTA to DS:DX */
	intdosx(&srv, &srv, &segregs);

#if ( defined(M_I86LM) || defined(M_I86CM) || defined(M_I86HM) )
	segregs.ds = FP_SEG(scanname);
	srv.x.dx = FP_OFF(scanname);
#else
	srv.x.dx = (unsigned int) scanname;
#endif
	srv.x.cx = 0xff;	/* Search mode */

	for (srv.h.ah = 0x4e; !intdosx(&srv, &srv, &segregs); srv.h.ah = 0x4f) {
		if ((result = (char *) realloc(result, reslen + strlen(buff->fn) + 1)) ==
 NULL) {
			free(p);
			free(oldresult);
			return NULL;
		}
		oldresult = result;
#ifdef CONVERT_TO_LOWER_CASE
		strcpy(result + reslen, strlwr(buff->fn));
#else
		strcpy(result + reslen, buff->fn);
#endif
		reslen += strlen(buff->fn) + 1;
	}

	if (!(result = realloc(result, reslen + 1))) {
		free(p);
		free(oldresult);
		return NULL;
	} else {
		p->start = result;
		p->curr = result;
		*(result + reslen) = '\0';
		return p;
	}
}


struct direct  *
readdir(DIR *dirp)
{
	char           *p;
	register        len;
	static          dummy;

	p = dirp->curr;
	len = strlen(p);
	if (*p) {
		dirp->curr += len + 1;
		strcpy(dirp->dirstr.d_name, p);
		dirp->dirstr.d_namlen = len;
		/* To fool programs */
		dirp->dirstr.d_ino = ++dummy;
		return &(dirp->dirstr);
	} else
		return NULL;
}

long
telldir(DIR *dirp)
{
	return (long) dirp->curr;	/* ouch! pointer to long cast */
}

void
seekdir(DIR *dirp, long loc)
{
	dirp->curr = (char *) loc;	/* ouch! long to pointer cast */
}

void
rewinddir(DIR *dirp)
{
	dirp->curr = dirp->start;
}

void
closedir(DIR *dirp)
{
	free(dirp->start);
	free(dirp);
}
