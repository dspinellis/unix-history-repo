/*
 * process - process options for readnews
 */

static char *SccsId = "@(#)process.c	2.7	4/23/83";

#include "rparams.h"

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
'M',	'\0',	FALSE,	OPTION,	UNKNOWN,	MAIL,	(char *)NULL,
'f',	'\0',	FALSE,	OPTION,	ANY,		UNKNOWN,(char *)NULL,
'u',	'\0',	FALSE,	OPTION,	ANY,		UNKNOWN,(char *)NULL,
'e',	'\0',	FALSE,	OPTION,	ANY,		UNKNOWN,(char *)NULL,
'\0',	'\0',	0,	0,	0,		0,	(char *)NULL
};

process(argc,argv)
register int argc;
register char **argv;
{
	register int state = OPTION;
	register char *ptr;
	char filchar;
	int len, tlen;

	/* loop once per arg. */

	while (--argc) {
	    if (state == OPTION) {
		if (**argv != '-') {
			sprintf(bfr, "Bad option string \"%s\"", *argv);
			xerror(bfr);
		}
		while (*++*argv != '\0') {
			for (optpt = options; optpt->optlet != '\0'; ++optpt) {
				if (optpt->optlet == **argv)
					goto found;
			}
			/* unknown option letter */
			fprintf(stderr, "Usage: readnews [ -a [ date ]] [ -n newsgroups ] [ -t titles ] [ -lprxhfuM ]\n");
			fprintf(stderr, "\t[ -c [ ``mailer'' ]]\n\n");
			fprintf(stderr, "       readnews -s\n");
			exit(1);

		    found:;
			if (mode != UNKNOWN && (mode&optpt->oldmode) == 0) {
				sprintf(bfr, "Bad %c option", **argv);
				xerror(bfr);
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
