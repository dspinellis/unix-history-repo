/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)utilities.c	1.5 (Berkeley) %G%";
#endif /* not lint */

#define	TELOPTS
#include <arpa/telnet.h>
#include <sys/types.h>

#include <ctype.h>

#include "general.h"

#include "ring.h"

#include "externs.h"

FILE	*NetTrace = 0;		/* Not in bss, since needs to stay */

/*
 * upcase()
 *
 *	Upcase (in place) the argument.
 */

void
upcase(argument)
register char *argument;
{
    register int c;

    while ((c = *argument) != 0) {
	if (islower(c)) {
	    *argument = toupper(c);
	}
	argument++;
    }
}

/*
 * SetSockOpt()
 *
 * Compensate for differences in 4.2 and 4.3 systems.
 */

int
SetSockOpt(fd, level, option, yesno)
int
	fd,
	level,
	option,
	yesno;
{
#ifndef	NOT43
    return setsockopt(fd, level, option,
				(char *)&yesno, sizeof yesno);
#else	/* NOT43 */
    if (yesno == 0) {		/* Can't do that in 4.2! */
	fprintf(stderr, "Error: attempt to turn off an option 0x%x.\n",
				option);
	return -1;
    }
    return setsockopt(fd, level, option, 0, 0);
#endif	/* NOT43 */
}

/*
 * The following are routines used to print out debugging information.
 */


void
Dump(direction, buffer, length)
char	direction;
char	*buffer;
int	length;
{
#   define BYTES_PER_LINE	32
#   define min(x,y)	((x<y)? x:y)
    char *pThis;
    int offset;

    offset = 0;

    while (length) {
	/* print one line */
	fprintf(NetTrace, "%c 0x%x\t", direction, offset);
	pThis = buffer;
	buffer = buffer+min(length, BYTES_PER_LINE);
	while (pThis < buffer) {
	    fprintf(NetTrace, "%.2x", (*pThis)&0xff);
	    pThis++;
	}
	fprintf(NetTrace, "\n");
	length -= BYTES_PER_LINE;
	offset += BYTES_PER_LINE;
	if (length < 0) {
	    return;
	}
	/* find next unique line */
    }
}


/*VARARGS*/
void
printoption(direction, fmt, option, what)
	char *direction, *fmt;
	int option, what;
{
	if (!showoptions)
		return;
	fprintf(NetTrace, "%s ", direction+1);
	if (fmt == doopt)
		fmt = "do";
	else if (fmt == dont)
		fmt = "dont";
	else if (fmt == will)
		fmt = "will";
	else if (fmt == wont)
		fmt = "wont";
	else
		fmt = "???";
	if (option < (sizeof telopts/sizeof telopts[0]))
		fprintf(NetTrace, "%s %s", fmt, telopts[option]);
	else
		fprintf(NetTrace, "%s %d", fmt, option);
	if (*direction == '<') {
		fprintf(NetTrace, "\r\n");
		return;
	}
	fprintf(NetTrace, " (%s)\r\n", what ? "reply" : "don't reply");
}

void
printsub(direction, pointer, length)
char	*direction,		/* "<" or ">" */
	*pointer;		/* where suboption data sits */
int	length;			/* length of suboption data */
{
    if (showoptions) {
	fprintf(NetTrace, "%s suboption ",
				(direction[0] == '<')? "Received":"Sent");
	switch (pointer[0]) {
	case TELOPT_TTYPE:
	    fprintf(NetTrace, "Terminal type ");
	    switch (pointer[1]) {
	    case TELQUAL_IS:
		{
		    char tmpbuf[SUBBUFSIZE];
		    int minlen = min(length, sizeof tmpbuf);

		    memcpy(tmpbuf, pointer+2, minlen);
		    tmpbuf[minlen-1] = 0;
		    fprintf(NetTrace, "is %s.\n", tmpbuf);
		}
		break;
	    case TELQUAL_SEND:
		fprintf(NetTrace, "- request to send.\n");
		break;
	    default:
		fprintf(NetTrace,
				"- unknown qualifier %d (0x%x).\n", pointer[1]);
	    }
	    break;
	default:
	    fprintf(NetTrace, "Unknown option %d (0x%x)\n",
					pointer[0], pointer[0]);
	}
    }
}
