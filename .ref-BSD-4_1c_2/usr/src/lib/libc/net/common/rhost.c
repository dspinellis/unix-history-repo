static char sccsid[] = "@(#)rhost.c	4.5 12/19/82";

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <netinet/in.h>

char	*any(), *rany(), *malloc();

/*
 * Convert host name to canonical form
 * and return internet address in network
 * order.
 * THIS ROUTINE IS DEPRECATED, USE GETHOSTBYNAME.
 */
rhost(ahost)
	char **ahost;
{
	FILE *hf;
	char hbuf[BUFSIZ], *cp;
	int first = 1, addr;

	if (isdigit(**ahost) && (addr = (int)inet_addr(*ahost)) >= 0)
		return (addr);
	hf = fopen("/etc/hosts", "r");
	if (hf == NULL) {
		perror("/etc/hosts");
		exit(1);
	}
top:
	while (fgets(hbuf, sizeof (hbuf), hf)) {
		if (*hbuf == '#')
			continue;
		cp = rany(hbuf, "#\n");
		if (cp != NULL)
			*cp = '\0';
		for (;;) {
			cp = rany(hbuf, " \t");
			if (cp == NULL)
				break;
			if (!strcmp(cp + 1, *ahost)) {
				addr = (int)inet_addr(hbuf);
				if (addr == -1)
					goto bad;
				fclose(hf);
				cp = any(hbuf, " \t");
				while (*cp == ' ' || *cp == '\t')
					*cp++;
				*ahost = cp;
				cp = any(*ahost, " \t");
				if (cp)
					*cp = '\0';
				cp = malloc(strlen(*ahost)+1);
				strcpy(cp, *ahost);
				*ahost = cp;
				return (addr);
			}
			*cp = '\0';
		}
	}
	if (first == 1) {
		first = 0;
		fclose(hf);
		if (hf = fopen("/etc/hosts.local", "r"))
			goto top;
		return (-1);
	}
bad:
	fclose(hf);
	return (-1);
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

static char *
rany(cp, match)
	register char *cp;
	char *match;
{
	register char *mp, c;
	char *last = NULL;

	while (c = *cp) {
		for (mp = match; *mp; mp++)
			if (*mp == c)
				last = cp;
		cp++;
	
	}
	return (last);
}
