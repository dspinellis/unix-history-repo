/*	gethostent.c	4.4	82/12/17	*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <ctype.h>

/*
 * Internet version.
 */
#define	MAXALIASES	35
#define	MAXADDRSIZE	14

static char HOSTDB[] = "/etc/hosts";
static FILE *hostf = NULL;
static char line[BUFSIZ+1];
static char hostaddr[MAXADDRSIZE];
static struct hostent host;
static char *host_aliases[MAXALIASES];
static int stayopen = 0;
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
	/* THIS STUFF IS INTERNET SPECIFIC */
	host.h_addr = hostaddr;
	*((u_long *)host.h_addr) = inet_addr(p);
	host.h_length = sizeof (u_long);
	host.h_addrtype = AF_INET;
	while (*cp == ' ' || *cp == '\t')
		cp++;
	host.h_name = cp;
	q = host.h_aliases = host_aliases;
	cp = any(cp, " \t");
	if (cp != NULL) {
		*cp++ = '\0';
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
	}
	*q = NULL;
	return (&host);
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
