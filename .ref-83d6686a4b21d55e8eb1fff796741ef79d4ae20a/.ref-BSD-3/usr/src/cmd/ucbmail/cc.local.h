/*
 * Declarations and constants specific to an installation.
 * Computer Center Unix A
 *
 * Note to fiddlers:  please, please, PLEASE note all changes
 * that you make here so that I can keep an up to date version.
 */
 
#define	LOCAL		'a'		/* Local machine id */
#define	MAIL		"/bin/mail"	/* Name of mail sender */
#define	UIDGID				/* Names and uids like cory */
#define	EDITOR		"/usr/bin/ex"	/* Name of text editor */
#define	VISUAL		"/usr/new/vi"	/* Name of display editor */
#define	SHELL		"/bin/csh"	/* Default shell */
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

