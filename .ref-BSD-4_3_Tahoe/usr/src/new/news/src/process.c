/*
 * This software is Copyright (c) 1986 by Rick Adams.
 *
 * Permission is hereby granted to copy, reproduce, redistribute or
 * otherwise use this software as long as: there is no monetary
 * profit gained specifically from the use or reproduction or this
 * software, it is not sold, rented, traded or otherwise marketed, and
 * this copyright notice is included prominently in any copy
 * made.
 *
 * The author make no claims as to the fitness or correctness of
 * this software for any use whatsoever, and it is provided as is. 
 * Any use of this software is at the user's own risk.
 *
 * process - process options for readnews/vnews
 */

#ifdef SCCSID
static char	*SccsId = "@(#)process.c	2.17	12/16/86";
#endif /* SCCSID */

#include "rparams.h"

char	coptbuf[LBUFLEN], datebuf[LBUFLEN];

#define OPTION	0	/* pick up an option string */
#define STRING	1	/* pick up a string of arguments */

struct optable *optpt, options[] = { /*
optlet	filchar	flag	newstate oldmode	newmode	buf	*/
'p',	'\0',	FALSE,	OPTION,	UNKNOWN,	UNKNOWN,(char *)NULL,	
't',	'\0',	FALSE,	STRING,	ANY,		UNKNOWN,header.title,	
'a',	' ',	FALSE,	STRING,	ANY,		UNKNOWN,datebuf,
'n',   NGDELIM,	FALSE,	STRING,	ANY,		UNKNOWN,header.nbuf,
'c',	' ',	FALSE,	STRING,	UNKNOWN,	UNKNOWN,coptbuf,	
'l',	' ',	FALSE,	OPTION,	UNKNOWN,	UNKNOWN,(char *)NULL,
'r',	'\0',	FALSE,	OPTION,	ANY,		UNKNOWN,(char *)NULL,
's',   NGDELIM,	FALSE,	STRING,	ANY,		UNKNOWN,header.nbuf,
'x',	'\0',	FALSE,	OPTION,	ANY,		UNKNOWN,(char *)NULL,
'h',	'\0',	FALSE,	OPTION,	ANY,		UNKNOWN,(char *)NULL,
#ifdef TMAIL
'M',	'\0',	FALSE,	OPTION,	UNKNOWN,	MAIL,	(char *)NULL,
#else /* !TMAIL */
'\377',	'\0',	FALSE,	OPTION,	UNKNOWN,	UNKNOWN,(char *)NULL,
#endif /* !TMAIL */
'f',	'\0',	FALSE,	OPTION,	ANY,		UNKNOWN,(char *)NULL,
'u',	'\0',	FALSE,	OPTION,	ANY,		UNKNOWN,(char *)NULL,
'e',	'\0',	FALSE,	OPTION,	ANY,		UNKNOWN,(char *)NULL,
'K',	'\0',	FALSE,	OPTION,	ANY,		UNKNOWN,(char *)NULL,
'\0',	'\0',	0,	0,	0,		0,	(char *)NULL
};

process(argc,argv)
register int argc;
register char **argv;
{
	register int state = STRING;
	register char *ptr = header.nbuf;
	char filchar = NGDELIM;
	int len = LBUFLEN, tlen;

	/* loop once per arg. */

	if (argc > 1 && **argv != '-')
		nflag = TRUE;

	while (--argc) {
	    if (state == OPTION) {
		if (**argv != '-') {
			xerror("Bad option string \"%s\"", *argv);
		}
		while (*++*argv != '\0') {
			for (optpt = options; optpt->optlet != '\0'; ++optpt) {
				if (optpt->optlet == **argv)
					goto found;
			}
			/* unknown option letter */
#ifdef TMAIL
			fprintf(stderr, "Usage: %s [ -a [ date ]] [ -n newsgroups ] [ -t titles ] [ -lprxhfuMK ]\n", Progname);
#else /* !TMAIL */
			fprintf(stderr, "Usage: %s [ -a [ date ]] [ -n newsgroups ] [ -t titles ] [ -lprxhfuK ]\n", Progname);
#endif /* !TMAIL */
			fprintf(stderr, "\t[ -c [ ``mailer'' ]]\n\n");
			fprintf(stderr, "       %s -s\n", Progname);
			exit(1);

		    found:;
			if (mode != UNKNOWN && (mode&optpt->oldmode) == 0) {
				xerror("Bad %c option", **argv);
			}
			if (mode == UNKNOWN)
				mode = optpt->newmode;
			filchar = optpt->filchar;
			optpt->flag = TRUE;
			state = optpt->newstate;
			ptr = optpt->buf;
			len = LBUFLEN;
		}

		argv++;		/* done with this option arg. */

	    } else {

		/*
		 * Pick up a piece of a string and put it into
		 * the appropriate buffer.
		 */
		if (**argv == '-') {
			state = OPTION;
			argc++;	/* uncount this arg. */
			continue;
		}

		if ((tlen = strlen(*argv)) >= len)
			xerror("Argument string too long");
		strcpy(ptr, *argv++);
		ptr += tlen;
		if (*(ptr-1) != filchar)
			*ptr++ = filchar;
		len -= tlen + 1;
		*ptr = '\0';
	    }
	}
	return;
}
