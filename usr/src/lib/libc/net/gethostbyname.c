/*	gethostbyname.c	4.2	82/10/05	*/

#include <netdb.h>

struct hostent *
gethostbyname(name)
	register char *name;
{
	register struct hostent *p;
	register char **cp;

	sethostent(0);
	while (p = gethostent()) {
		if (strcmp(p->h_name, name) == 0)
			break;
		for (cp = p->h_aliases; *cp != 0; cp++)
			if (strcmp(*cp, name) == 0)
				goto found;
	}
found:
	endhostent();
	return (p);
}
