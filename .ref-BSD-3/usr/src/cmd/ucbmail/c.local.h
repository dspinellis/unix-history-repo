/*
 * Declarations and constants specific to an installation.
 * EECS Cory Hall 11/70 Version 6.9
 */
 
#define	stat		_stat		/* To simulate version 7 ... */
#define	fstat		_fstat		/* To simulate version 7 ... */
#define	LOCAL		'y'		/* Local machine id */
#define	MAIL		"/usr/bin/mail"	/* Name of mail sender */
#define	VISUAL		"/usr/new/vi"	/* Name of display editor */
#define	EDITOR		"/usr/new/ex"	/* Name of text editor */
#define	SHELL		"/bin/csh"	/* Default shell interpreter */
#define	HELPFILE	"/usr/lib/Mail.help"
					/* Name of casual help file */
#define	THELPFILE	"/usr/lib/Mail.help.~"
					/* Name of casual tilde help */
#define	UIDMASK		0177777		/* Significant uid bits */
#define	MASTER		"/usr/lib/Mail.rc"
#define	AUX		"/usr/lib/Mail.remote"

/*
 * Machine dependent type declarations.
 */

