/*	getprotoent.c	4.1	82/08/25	*/

#include <stdio.h>
#include <sys/socket.h>
#include <netdb.h>
#include <ctype.h>

#define	MAXALIASES	35

static char *PROTODB = "/usr/lib/protocols";
static FILE *protof = NULL;
static char line[BUFSIZ+1];
static struct protoent proto;
static char *proto_aliases[MAXALIASES];
static int stayopen = 0;
static unsigned long value();
static char *any();

setprotoent(f)
	int f;
{
	if (protof == NULL)
		protof = fopen(PROTODB, "r" );
	else
		rewind(protof);
	stayopen |= f;
}

endprotoent()
{
	if (protof && !stayopen) {
		fclose(protof);
		protof = NULL;
	}
}

struct protoent *
getprotoent()
{
	char *p;
	register char *cp, **q;

	if (protof == NULL && (protof = fopen(PROTODB, "r" )) == NULL)
		return (NULL);
again:
	if ((p = fgets(line, BUFSIZ, protof)) == NULL)
		return (NULL);
	if (*p == '#')
		goto again;
	cp = any(p, "#\n");
	if (cp == NULL)
		goto again;
	*cp = '\0';
	proto.p_name = p;
	cp = any(p, " \t");
	if (cp == NULL)
		goto again;
	*cp++ = '\0';
	while (*cp == ' ' || *cp == '\t')
		cp++;
	p = any(cp, " \t");
	if (p != NULL)
		*p++ = '\0';
	proto.p_proto = atoi(cp);
	proto.p_aliases = proto_aliases;
	cp = p;
	q = proto_aliases;
	while (*cp) {
		if (*cp == ' ' || *cp == '\t') {
			cp++;
			continue;
		}
		if (q < &proto_aliases[MAXALIASES - 1])
			*q++ = cp;
		cp = any(cp, " \t");
		if (*cp != NULL)
			*cp++ = '\0';
	}
	*q = NULL;
	return (&proto);
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
