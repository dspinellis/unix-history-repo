/* Copyright (c) 1979 Regents of the University of California */
/*
 * Declarations and constants specific to an installation.
 * Version 7.
 */
 
#define	MAIL		"/bin/mail"	/* Name of mail sender */
#define	EDITOR		"/usr/ucb/ex"	/* Name of text editor */
#define	VISUAL		"/usr/ucb/vi"	/* Name of display editor */
#define	HELPFILE	"/usr/lib/Mail.help"
					/* Name of casual help file */
#define	THELPFILE	"/usr/lib/Mail.help.~"
					/* Name of casual tilde help */
#define	UIDMASK		0177777		/* Significant uid bits */
#define	MASTER		"/usr/lib/Mail.rc"
#define	APPENDS				/* New mail goes to end of mailbox */

/*
 * Machine dependent type declarations.
 */

typedef	short	flag_t;			/* flag arguments everywhere */
