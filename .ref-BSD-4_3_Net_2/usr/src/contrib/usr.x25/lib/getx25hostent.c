/*	getx25hostent.c	2.0	86-9-29	*/

/*
 * Get next entry from /etc/x25hosts table
 * Adapted from 4.2bsd network support code.
 *
 * Frank Pronk
 * The University of British Columbia
 * Laboratory for Computational Vision
 * Copyright (c)
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <netccitt/x25.h>
#include <netdb.h>
#include <ctype.h>

/*
 * ccitt version.
 */

#define	MAXALIASES	8
#define	MAXADDRSIZE	64
#define LINESIZE	256

static char HOSTDB[] = "/etc/x25hosts";
static char line[LINESIZE+1];
static char hostaddr[MAXADDRSIZE];
static struct hostent host;
#ifdef h_addr		/* for 4.3bsd; see <netdb.h> */
static char *host_addrs[2];
#endif
static char *host_aliases[MAXALIASES];
static char *FileStart, *FilePos, *FileEnd;
static short stayopen;
static char *any();
char *malloc ();

setx25hostent(f)
	int f;
{
	register int fd = -1;
	struct stat st;

	if (FileStart == 0) {
		if ((fd = open (HOSTDB, 0)) < 0)
			return;
		fstat (fd, &st);
		if ((FileStart = malloc (st.st_size)) == 0)
			goto fail;
		if (read (fd, FileStart, st.st_size) != st.st_size)
			goto fail;
		FileEnd = FileStart + st.st_size;
		close (fd);
		stayopen |= f;
	}
	FilePos = FileStart;
	return;
fail:
	if (FileStart) {
		free (FileStart);
		FileStart = 0;
	}
	if (fd >= 0)
		close (fd);
}

endx25hostent()
{
	if (FileStart && !stayopen) {
		free (FileStart);
		FileStart = 0;
	}
}

struct hostent *
getx25hostent()
{
	register char *p, *cp, **q, *end;
	long iaddr;

	if (FileStart == 0) {
		setx25hostent (0);
		if (FileStart == 0)
			return (0);
	}
#ifdef h_addr
	host.h_addr_list = host_addrs;
	host_addrs[0] = hostaddr;
#else
	host.h_addr = hostaddr;
#endif
	p = FilePos;
	end = any (p, FileEnd, "\n");
	for (; end; p = end+1, end = any (end+1, FileEnd, "\n")) {
		if (*p == '#')
			continue;
		bcopy (p, line, (end + 1) - p);
		p = line;
		if (cp = any (p, line+LINESIZE, "\n#"))
			*cp = '\0';
		if ((cp = any (p, line+LINESIZE, " \t")) == 0)
			continue;
		*cp++ = '\0';

		if (ccitt_addr(p, (struct sockaddr_x25 *)hostaddr) == 0)
			continue;
		host.h_length = sizeof (struct sockaddr_x25);
		host.h_addrtype = AF_CCITT;

		while (*cp == ' ' || *cp == '\t')
			cp++;
		host.h_name = cp;
		q = host.h_aliases = host_aliases;
		cp = any(cp, line+LINESIZE, " \t");
		if (cp != 0) 
			*cp++ = '\0';
		while (cp && *cp) {
			if (*cp == ' ' || *cp == '\t') {
				cp++;
				continue;
			}
			if (q < &host_aliases[MAXALIASES - 1])
				*q++ = cp;
			cp = any(cp, line+LINESIZE, " \t");
			if (cp != 0)
				*cp++ = '\0';
		}
		*q = 0;
		FilePos = end + 1;
		return (&host);
	}
	return (0);
}

static char *
any(cp, limit, match)
	register char *cp;
	char *limit, *match;
{
	register char *mp, c;

	while (cp < limit) {
		c = *cp;
		for (mp = match; *mp; mp++)
			if (*mp == c)
				return (cp);
		cp++;
	}
	return ((char *)0);
}
