/*
 * Declarations and constants specific to an installation.
 * EECS/ERL 11/40 Unix
 */
 
#define	LOCAL		'z'		/* Local machine id */
#define	MAIL		"/usr/bin/mail"	/* Name of mail sender */
#define	EDITOR		"/usr/bin/ex"	/* Name of text editor */
#define	VISUAL		"/usr/bin/vi"	/* Name of display editor */
#define	HELPFILE	"/usr/lib/Mail.help"
					/* Name of casual help file */
#define	THELPFILE	"/usr/lib/Mail.help.~"
					/* Name of casual tilde help */
#define	UIDMASK		0377		/* Significant uid bits */
#define	MASTER		"/usr/lib/Mail.rc"
#define	AUX		"/usr/lib/Mail.remote"
#define	stat	_stat			/* Simulate version 7 */
#define	fstat	_fstat			/* Simulate version 7 */
