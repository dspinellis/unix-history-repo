/*
 * Fetch netdb entry for a host given
 * its name from /etc/x25hosts
 *
 * Frank Pronk  1985
 */

#include <netdb.h>

struct hostent *
getx25hostbyname (name)
register char *name;
{
	register struct hostent *p;
	register char **cp;
	struct hostent *getx25hostent ();

	setx25hostent (0);
	while (p = getx25hostent ()) {
		if (strcmp (p->h_name, name) == 0)
			break;
		for (cp = p->h_aliases; *cp != 0; cp++)
			if (strcmp (*cp, name) == 0)
				goto found;
	}
found:
	endx25hostent ();
	return (p);
}
