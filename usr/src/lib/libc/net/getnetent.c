/*	getnetent.c	4.1	82/08/25	*/

#include <stdio.h>
#include <sys/socket.h>
#include <netdb.h>
#include <ctype.h>

#define	MAXALIASES	35

static char *NETDB = "/usr/lib/networks";
static FILE *netf = NULL;
static char line[BUFSIZ+1];
static struct netent net;
static char *net_aliases[MAXALIASES];
static int stayopen = 0;
static unsigned long value();
static char *any();

setnetent(f)
	int f;
{
	if (netf == NULL)
		netf = fopen(NETDB, "r" );
	else
		rewind(netf);
	stayopen |= f;
}

endnetent()
{
	if (netf && !stayopen) {
		fclose(netf);
		netf = NULL;
	}
}

struct netent *
getnetent()
{
	char *p;
	register char *cp, **q;

	if (netf == NULL && (netf = fopen(NETDB, "r" )) == NULL)
		return (NULL);
again:
	p = fgets(line, BUFSIZ, netf);
	if (p == NULL)
		return (NULL);
	if (*p == '#')
		goto again;
	cp = any(p, "#\n");
	if (cp == NULL)
		goto again;
	*cp = '\0';
	net.n_name = p;
	cp = any(p, " \t");
	if (cp == NULL)
		goto again;
	*cp++ = '\0';
	while (*cp == ' ' || *cp == '\t')
		cp++;
	p = any(cp, " \t");
	if (p != NULL)
		*p++ = '\0';
	net.n_net = value(cp);
	net.n_aliases = net_aliases;
	q = net_aliases, cp = p;
	while (*cp) {
		if (*cp == ' ' || *cp == '\t') {
			cp++;
			continue;
		}
		if (q < &net_aliases[MAXALIASES - 1])
			*q++ = cp;
		cp = any(cp, " \t");
		if (*cp != NULL)
			*cp++ = '\0';
	}
	*q = NULL;
	return (&net);
}

static unsigned long
value(cp)
	register char *cp;
{
	register unsigned long val, base, n;
	register char c;
	unsigned long parts[4], *pp = parts;

again:
	val = 0; base = 10;
	if (*cp == '0')
		base = 8, cp++;
	if (*cp == 'x' || *cp == 'X')
		base = 16, cp++;
	while (c = *cp) {
		if (isdigit(c)) {
			val = (val * base) + (c - '0');
			cp++;
			continue;
		}
		if (base == 16 && isxdigit(c)) {
			val = (val << 4) + (c + 10 - (islower(c) ? 'a' : 'A'));
			cp++;
			continue;
		}
		break;
	}
	if (*cp == '.') {
		/*
		 * Internet format:
		 *	a.b.c.d
		 *	a.b.c	(with c treated as 16-bits)
		 *	a.b	(with b treated as 24 bits)
		 */
		if (pp >= parts + 4)
			return (-1);
		*pp++ = val, cp++;
		goto again;
	}
	if (*cp && !isspace(*cp))
		return (-1);
	n = pp - parts;
	if (n > 0) {
		if (n > 4)
			return (-1);
		*pp++ = val; n++;
		val = parts[0];
		if (n > 1)
			val <<= 24;
		if (n > 2)
			val |= (parts[1] & 0xff) << 16;
		if (n > 3)
			val |= (parts[2] & 0xff) << 8;
		if (n > 1)
			val |= parts[n - 1];
#if vax || pdp11
		val = htonl(val);
#endif
	}
	return (val);
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
