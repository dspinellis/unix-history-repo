#ifndef lint
static char sccsid[] = "@(#)imsg.c	5.1 (Berkeley) %G%";
#endif

#include "uucp.h"

char Msync[2] = "\020";
/*******
 *	imsg(msg, fn)
 *	char *msg;
 *	int fn;
 *
 *	imsg  -  this is the initial read message routine -
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
	DEBUG(7, "imsg %s>", "");
	while ((ret = read(fn, msg, 1)) == 1) {
		*msg &= 0177;	/* Turn off parity bit (mhb5b!smb) */
		DEBUG(7, (*msg>037) ? "%c" : "\\%03o", *msg & 0377);
		if (*msg == Msync[0])
			break;
	}
	DEBUG(7, "%s\n", "<");
	if (ret < 1)
		return(EOF);
	while (read(fn, msg, 1) == 1) {
		*msg &= 0177;
		DEBUG(7, (*msg>037) ? "%c" : "\\%03o", *msg & 0377);
		if (*msg == '\n')
			break;
		if (*msg == '\0')
			break;
		msg++;
	}
	*msg = '\0';
	return(0);
}


/***
 *	omsg(type, msg, fn)
 *	char type, *msg;
 *	int fn;
 *
 *	omsg  -  this is the initial write message routine -
 *	used before a protocol is agreed upon.
 *
 *	return code:  always 0
 */

omsg(type, msg, fn)
register char *msg;
char type;
int fn;
{
	char buf[BUFSIZ];
	register char *c;

	c = buf;
	*c++ = Msync[0];
	*c++ = type;
	while (*msg)
		*c++ = *msg++;
	*c++ = '\0';
	write(fn, buf, strlen(buf) + 1);
	return(0);
}
