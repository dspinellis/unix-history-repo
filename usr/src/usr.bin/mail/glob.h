/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)glob.h	5.13 (Berkeley) %G%
 */

/*
 * A bunch of global variable declarations lie herein.
 * def.h must be included first.
 */

int	msgCount;			/* Count of messages read in */
int	rcvmode;			/* True if receiving mail */
int	sawcom;				/* Set after first command */
char	*Tflag;				/* -T temp file for netnews */
int	senderr;			/* An error while checking */
int	edit;				/* Indicates editing a file */
int	readonly;			/* Will be unable to rewrite file */
int	noreset;			/* String resets suspended */
int	sourcing;			/* Currently reading variant file */
int	loading;			/* Loading user definitions */
int	cond;				/* Current state of conditional exc. */
FILE	*itf;				/* Input temp file buffer */
FILE	*otf;				/* Output temp file buffer */
FILE	*pipef;				/* Pipe file we have opened */
int	image;				/* File descriptor for image of msg */
FILE	*input;				/* Current command input file */
char	mailname[PATHSIZE];		/* Name of current file */
char	prevfile[PATHSIZE];		/* Name of previous file */
char	*mailrc;			/* Name of startup file */
char	*deadletter;			/* Name of #/dead.letter */
char	*homedir;			/* Path name of home directory */
char	*myname;			/* My login name */
off_t	mailsize;			/* Size of system mailbox */
int	lexnumber;			/* Number of TNUMBER from scan() */
char	lexstring[STRINGLEN];		/* String from TSTRING, scan() */
int	regretp;			/* Pointer to TOS of regret tokens */
int	regretstack[REGDEP];		/* Stack of regretted tokens */
char	*stringstack[REGDEP];		/* Stack of regretted strings */
int	numberstack[REGDEP];		/* Stack of regretted numbers */
struct	message	*dot;			/* Pointer to current message */
struct	message	*message;		/* The actual message structure */
struct	var	*variables[HSHSIZE];	/* Pointer to active var list */
struct	grouphead	*groups[HSHSIZE];/* Pointer to active groups */
struct	ignoretab	ignore[2];	/* ignored and retained fields
					   0 is ignore, 1 is retain */
struct	ignoretab	saveignore[2];	/* ignored and retained fields
					   on save to folder */
char	**altnames;			/* List of alternate names for user */
char	**localnames;			/* List of aliases for our local host */
int	debug;				/* Debug flag set */
int	screenwidth;			/* Screen width, or best guess */
int	screenheight;			/* Screen height, or best guess,
					   for "header" command */
int	realscreenheight;		/* the real screen height */

#include <setjmp.h>

jmp_buf	srbuf;


/*
 * The pointers for the string allocation routines,
 * there are NSPACE independent areas.
 * The first holds STRINGSIZE bytes, the next
 * twice as much, and so on.
 */

#define	NSPACE	25			/* Total number of string spaces */
struct strings {
	char	*s_topFree;		/* Beginning of this area */
	char	*s_nextFree;		/* Next alloctable place here */
	unsigned s_nleft;		/* Number of bytes left here */
} stringdope[NSPACE];
