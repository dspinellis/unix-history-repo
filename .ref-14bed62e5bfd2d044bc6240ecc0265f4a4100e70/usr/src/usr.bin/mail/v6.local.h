/*
 * Declarations and constants specific to an installation.
 * Unix Version 6.
 */
 
/*
 * Sccs Id = "@(#)v6.local.h	2.1 %G%";
 */

#define	LOCAL		'i'		/* Local machine id */
#define	MAIL		"/bin/mail"	/* Name of mail sender */
#define	EDITOR		"/usr/bin/ex"	/* Name of text editor */
#define	VISUAL		"/usr/bin/vi"	/* Name of display editor */
#define	SHELL		"/bin/sh"	/* Standard shell */
#define	HELPFILE	"/usr/lib/Mail.help"
					/* Name of casual help file */
#define	THELPFILE	"/usr/lib/Mail.help.~"
					/* Name of casual tilde help */
#define	UIDMASK		0377		/* Significant uid bits */
#define	MASTER		"/usr/lib/Mail.rc"
#define	stat		_stat		/* Simulate version 7 */
#define	fstat		_fstat		/* Simulate version 7 */
