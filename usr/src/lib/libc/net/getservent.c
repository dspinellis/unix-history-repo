/*	getservent.c	4.1	82/08/25	*/

#include <stdio.h>
#include <sys/socket.h>
#include <netdb.h>
#include <ctype.h>

#define	MAXALIASES	35

static char SERVDB[] = "/usr/lib/services";
static FILE *servf = NULL;
static char line[BUFSIZ+1];
static struct servent serv;
static char *serv_aliases[MAXALIASES];
static int stayopen = 0;
static unsigned long value();
static char *any();

setservent(f)
	int f;
{
	if (servf == NULL)
		servf = fopen(SERVDB, "r" );
	else
		rewind(servf);
	stayopen |= f;
}

endservent()
{
	if (servf && !stayopen) {
		fclose(servf);
		servf = NULL;
	}
}

struct servent *
getservent()
{
	char *p;
	register char *cp, **q;

	if (servf == NULL && (servf = fopen(SERVDB, "r" )) == NULL)
		return (NULL);
again:
	if ((p = fgets(line, BUFSIZ, servf)) == NULL)
		return (NULL);
	if (*p == '#')
		goto again;
	cp = any(p, "#\n");
	if (cp == NULL)
		goto again;
	*cp = '\0';
	serv.s_name = p;
	p = any(p, " \t");
	if (p == NULL)
		goto again;
	*p++ = '\0';
	while (*p == ' ' || *p == '\t')
		p++;
	cp = any(p, ",/");
	if (cp == NULL)
		goto again;
	*cp++ = '\0';
	serv.s_port = atoi(p);
	serv.s_proto = cp;
	cp = any(cp, " \t");
	if (cp != NULL)
		*cp++ = '\0';
	serv.s_aliases = serv_aliases;
	q = serv_aliases;
	while (*cp) {
		if (*cp == ' ' || *cp == '\t') {
			cp++;
			continue;
		}
		if (q < &serv_aliases[MAXALIASES - 1])
			*q++ = cp;
		cp = any(cp, " \t");
		if (*cp != NULL)
			*cp++ = '\0';
	}
	*q = NULL;
	return (&serv);
}

static char *
any(cp, match)
	register char *cp;
	char *match;
{
	register char *mp, c;

	while (c = *cp) {
		for (mp = match; *mp; mp++)
			if (*mp == c)
				return (cp);
		cp++;
	}
	return ((char *)0);
}
