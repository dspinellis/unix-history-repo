/*	gethostnamadr.c	4.5	84/08/28	*/

#include <stdio.h>
#include <netdb.h>
#include <sys/file.h>
#include <ndbm.h>

#define	MAXALIASES	35

static char HOSTDB[] = "/etc/hosts";
DBM *_host_db = (DBM *)NULL;
static struct hostent host;
static char *host_aliases[MAXALIASES];
static char hostbuf[BUFSIZ+1];
int _host_stayopen;	/* set by sethostent(), cleared by endhostent() */

static struct hostent *
fetchhost(key)
	datum key;
{
        register char *cp, *tp, **ap;
	register int naliases;

        if (key.dptr == 0)
                return ((struct hostent *)NULL);
	key = dbm_fetch(_host_db, key);
	if (key.dptr == 0)
                return ((struct hostent *)NULL);
        cp = key.dptr;
	tp = hostbuf;
	host.h_name = tp;
	while (*tp++ = *cp++)
		;
	naliases = *(int *)cp; cp += sizeof (int);
	for (ap = host_aliases; naliases > 0; naliases--) {
		*ap++ = tp;
		while (*tp++ = *cp++)
			;
	}
	*ap = (char *)NULL;
	host.h_aliases = host_aliases;
	host.h_addrtype = *(int *)cp; cp += sizeof (int);
	host.h_length = *(int *)cp; cp += sizeof (int);
	host.h_addr = tp;
	bcopy(cp, tp, host.h_length);
        return (&host);
}

struct hostent *
gethostbyname(nam)
	char *nam;
{
        datum key;
	register struct hostent *hp;

	if ((_host_db == (DBM *)NULL)
	  && ((_host_db = dbm_open(HOSTDB, O_RDONLY)) == (DBM *)NULL))
                return ((struct hostent *)NULL);
        key.dptr = nam;
        key.dsize = strlen(nam);
	hp = fetchhost(key);
	if (!_host_stayopen) {
		dbm_close(_host_db);
		_host_db = (DBM *)NULL;
	}
        return (hp);
}

struct hostent *
gethostbyaddr(addr, length)
	char *addr;
	int length;
{
        datum key;
	register struct hostent *hp;

	if ((_host_db == (DBM *)NULL)
	  && ((_host_db = dbm_open(HOSTDB, O_RDONLY)) == (DBM *)NULL))
                return ((struct hostent *)NULL);
        key.dptr = addr;
        key.dsize = length;
	hp = fetchhost(key);
	if (!_host_stayopen) {
		dbm_close(_host_db);
		_host_db = (DBM *)NULL;
	}
        return (hp);
}
