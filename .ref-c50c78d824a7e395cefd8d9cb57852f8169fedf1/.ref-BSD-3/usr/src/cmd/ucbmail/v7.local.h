/*
 * Declarations and constants specific to an installation.
 *
 * Vax/Unix version 7.
 */
 
#define	LOCAL		'v'		/* Local machine id */
#define	MAIL		"/bin/mail"	/* Name of mail sender */
#define	EDITOR		"/usr/ucb/ex"	/* Name of text editor */
#define	VISUAL		"/usr/ucb/vi"	/* Name of display editor */
#define	SHELL		"/bin/csh"	/* Standard shell */
#define	HELPFILE	"/usr/lib/Mail.help"
					/* Name of casual help file */
#define	THELPFILE	"/usr/lib/Mail.help.~"
					/* Name of casual tilde help */
#define	UIDMASK		0177777		/* Significant uid bits */
#define	MASTER		"/usr/lib/Mail.rc"
#define	AUX		"/usr/lib/Mail.remote"
#define	APPEND				/* New mail goes to end of mailbox */
#define CANLOCK				/* Locking protocol actually works */

/*
 * Machine dependent type declarations.
 */

