/*
 * Copyright (c) 1988, 1989, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 * Copyright (c) 1989 by Berkeley Softworks
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dir.h	8.2 (Berkeley) %G%
 */

/* dir.h --
 */

#ifndef	_DIR
#define	_DIR

typedef struct Path {
    char         *name;	    	/* Name of directory */
    int	    	  refCount; 	/* Number of paths with this directory */
    int		  hits;	    	/* the number of times a file in this
				 * directory has been found */
    Hash_Table    files;    	/* Hash table of files in directory */
} Path;

void Dir_Init __P((void));
void Dir_End __P((void));
Boolean Dir_HasWildcards __P((char *));
void Dir_Expand __P((char *, Lst, Lst));
char *Dir_FindFile __P((char *, Lst));
int Dir_MTime __P((GNode *));
void Dir_AddDir __P((Lst, char *));
char *Dir_MakeFlags __P((char *, Lst));
void Dir_ClearPath __P((Lst));
void Dir_Concat __P((Lst, Lst));
void Dir_PrintDirectories __P((void));
void Dir_PrintPath __P((Lst));
void Dir_Destroy __P((ClientData));
ClientData Dir_CopyDir __P((ClientData));

#endif /* _DIR */
