/*	getprotoname.c	4.1	82/08/25	*/

#include <netdb.h>

struct protoent *
getprotoname(name)
	register char *name;
{
	register struct protoent *p;
	register char **cp;

	setprotoent(0);
	while (p = getprotoent()) {
		if (strcmp(p->p_name, name) == 0)
			break;
		for (cp = p->p_aliases; *cp != 0; cp++)
			if (strcmp(*cp, name) == 0)
				goto found;
	}
found:
	endprotoent();
	return (p);
}
