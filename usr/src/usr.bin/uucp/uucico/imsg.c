#ifndef lint
static char sccsid[] = "@(#)imsg.c	5.2 (Berkeley) %G%";
#endif

#include "uucp.h"

char Msync[2] = "\020";

/* to talk to both eunice and x.25 without also screwing up tcp/ip
 * we must adaptively  choose what character to end the msg with
 * 
 * The idea is that initially we send ....\000\n
 * Then, after they have sent us a message, we use the first character
 * they send.
 */

int seenend = 0;
char Mend = '\0';

/*
 *	this is the initial read message routine -
 *	used before a protocol is agreed upon.
 *
 *	return codes:
 *		EOF - no more messages
 *		0 - message returned
 */

imsg(msg, fn)
register char *msg;
register int fn;
{
	register int ret;
	char *amsg;

	DEBUG(5, "imsg %s<", "sync");
	while ((ret = read(fn, msg, 1)) == 1) {
		*msg &= 0177;
		DEBUG(5, (*msg>037 && *msg<0177) ? "%c" : "\\%03o", *msg & 0377);
		if (*msg == Msync[0])
			break;
		fflush(stderr);
	}
	DEBUG(5, ">got %s\n", ret == 1 ? "it" : "EOF");
	if (ret < 1)
		return EOF;
	amsg = msg;
resync:
	DEBUG(5, "imsg %s<", "input");
	while (read(fn, msg, 1) == 1) {
		*msg &= 0177;
		DEBUG(5, (*msg>037 && *msg<0177) ? "%c" : "\\%03o", *msg & 0377);
		if (*msg == Msync[0]) {
			DEBUG(5, "%s\n", ">found sync");
			msg = amsg;
			goto resync;
		}
		if (*msg == '\n' || *msg == '\0') {
			if (!seenend) {
				Mend = *msg;
				seenend++;
				DEBUG(6,"\nUsing \\%o as End of message char\n", Mend);
			}
			break;
		}
		msg++;
		fflush(stderr);
	}
	*msg = '\0';
	DEBUG(5, ">got %d\n", strlen(amsg));
	return 0;
}


/*
 *	this is the initial write message routine -
 *	used before a protocol is agreed upon.
 *
 *	return code:  always 0
 */

omsg(type, msg, fn)
register char *msg;
char type;
int fn;
{
	char buf[MAXFULLNAME];
	register char *c;

	c = buf;
	*c = '\0';	/* avoid pdp 11/23,40 auto-incr stack trap bug */
	*c++ = Msync[0];
	*c++ = type;
	while (*msg)
		*c++ = *msg++;
	*c++ = '\0';
	DEBUG(5, "omsg <%s>\n", buf);
	if (seenend) 
		c[-1] = Mend;
	else
		*c++ = '\n';
	write(fn, buf, (int)(c - buf));
	return 0;
}
