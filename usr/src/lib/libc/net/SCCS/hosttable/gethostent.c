/*	gethostent.c	4.1	82/08/25	*/

#include <stdio.h>
#include <sys/socket.h>
#include <netdb.h>
#include <ctype.h>

#define	MAXALIASES	35

static char HOSTDB[] = "/usr/lib/hosts";
static FILE *hostf = NULL;
static char line[BUFSIZ+1];
static struct hostent host;
static char *host_aliases[MAXALIASES];
static int stayopen = 0;
static unsigned long value();
static char *any();

sethostent(f)
	int f;
{
	if (hostf == NULL)
		hostf = fopen(HOSTDB, "r" );
	else
		rewind(hostf);
	stayopen |= f;
}

endhostent()
{
	if (hostf && !stayopen) {
		fclose(hostf);
		hostf = NULL;
	}
}

struct hostent *
gethostent()
{
	char *p;
	register char *cp, **q;

	if (hostf == NULL && (hostf = fopen(HOSTDB, "r" )) == NULL)
		return (NULL);
again:
	if ((p = fgets(line, BUFSIZ, hostf)) == NULL)
		return (NULL);
	if (*p == '#')
		goto again;
	cp = any(p, "#\n");
	if (cp == NULL)
		goto again;
	*cp = '\0';
	cp = any(p, " \t");
	if (cp == NULL)
		goto again;
	*cp++ = '\0';
	host.h_addr = value(p);
	while (*cp == ' ' || *cp == '\t')
		cp++;
	host.h_name = cp;
	host.h_addrtype = AF_INET;
	host.h_aliases = host_aliases;
	cp = any(cp, " \t");
	if (cp != NULL)
		*cp++ = '\0';
	q = host_aliases;
	while (*cp) {
		if (*cp == ' ' || *cp == '\t') {
			cp++;
			continue;
		}
		if (q < &host_aliases[MAXALIASES - 1])
			*q++ = cp;
		cp = any(cp, " \t");
		if (*cp != NULL)
			*cp++ = '\0';
	}
	*q = NULL;
	return (&host);
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
