/* Copyright (c) 1981 Regents of the University of California */
/* sccs id:	@(#)ex_argv.h	7.1	%G%  */
/*
 * The current implementation of the argument list is poor,
 * using an argv even for internally done "next" commands.
 * It is not hard to see that this is restrictive and a waste of
 * space.  The statically allocated glob structure could be replaced
 * by a dynamically allocated argument area space.
 */
extern char	**argv;
extern char	**argv0;
extern char	*args;
extern char	*args0;
extern short	argc;
extern short	argc0;
extern short	morargc;		/* Used with "More files to edit..." */

extern int	firstln;		/* From +lineno */
extern char	*firstpat;		/* From +/pat	*/

/* Yech... */
struct	glob {
	short	argc;			/* Index of current file in argv */
	short	argc0;			/* Number of arguments in argv */
	char	*argv[NARGS + 1];	/* WHAT A WASTE! */
	char	argspac[NCARGS + sizeof (int)];
};
extern struct	glob frob;
