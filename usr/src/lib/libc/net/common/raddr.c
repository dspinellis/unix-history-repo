#ifndef lint
static char sccsid[] = "@(#)raddr.c	4.5 82/12/19";
#endif

#include <sys/types.h>

#include <stdio.h>
#include <ctype.h>

char	*any(), *rany(), *malloc();
extern	u_long inet_addr();

char *
raddr(desaddr)
	int desaddr;
{
	FILE *hf = fopen("/etc/hosts", "r");
	char hbuf[BUFSIZ], *host;
	register char *cp;
	int first = 1;

	if (hf == NULL) {
		perror("/etc/hosts");
		exit(1);
	}
top:
	while (fgets(hbuf, sizeof (hbuf), hf)) {
		u_long addr;

		if (*hbuf == '#')
			continue;
		cp = rany(hbuf, "#\n");
		if (cp == NULL)
			continue;
		*cp = '\0';
		addr = inet_addr(hbuf);
		if (addr != desaddr || addr == -1)
			continue;
		host = any(hbuf, " \t");
		if (host == NULL)
			continue;
		while (*host == ' ' || *host == '\t')
			host++;
		cp = any(host, " \t");
		if (cp)
			*cp = '\0';
		cp = malloc(strlen(host)+1);
		strcpy(cp, host);
		fclose(hf);
		return (cp);
	}
	if (first == 1) {
		first = 0;
		fclose(hf);
		if (hf = fopen("/etc/hosts.local", "r"))
			goto top;
		return (0);
	}
	fclose(hf);
	return (0);
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
