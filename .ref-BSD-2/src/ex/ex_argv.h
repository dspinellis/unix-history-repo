/* Copyright (c) 1979 Regents of the University of California */
/*
 * The current implementation of the argument list is poor,
 * using an argv even for internally done "next" commands.
 * It is not hard to see that this is restrictive and a waste of
 * space.  The statically allocated glob structure could be replaced
 * by a dynamically allocated argument area space.
 */
char	**argv;
char	**argv0;
char	*args;
char	*args0;
short	argc;
short	argc0;
short	morargc;		/* Used with "More files to edit..." */

short	firstln;		/* From +lineno */

/* Yech... */
struct	glob {
	short	argc;			/* Index of current file in argv */
	short	argc0;			/* Number of arguments in argv */
	char	*argv[NARGS + 1];	/* WHAT A WASTE! */
	char	argspac[NCARGS + sizeof (int)];
} frob;
