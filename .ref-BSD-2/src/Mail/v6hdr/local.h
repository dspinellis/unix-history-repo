/*
 * Declarations and constants specific to an installation.
 * Unix Version 6.
 */
 
#define	MAIL		"/bin/mail"	/* Name of mail sender */
#define	EDITOR		"/usr/ucb/ex"	/* Name of text editor */
#define	VISUAL		"/usr/ucb/vi"	/* Name of display editor */
#define	HELPFILE	"/usr/lib/Mail.help"
					/* Name of casual help file */
#define	THELPFILE	"/usr/lib/Mail.help.~"
					/* Name of casual tilde help */
#define	UIDMASK		0377		/* Significant uid bits */
#define	MASTER		"/usr/lib/Mail.rc"
#define	stat	_stat			/* Simulate version 7 */
#define	fstat	_fstat			/* Simulate version 7 */

/*
 * Machine dependent type declarations.
 */

typedef	int	flag_t;			/* flag arguments everywhere */
