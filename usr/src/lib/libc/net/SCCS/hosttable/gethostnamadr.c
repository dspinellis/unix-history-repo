/*	gethostnamadr.c	5.1	85/05/30	*/

#include <stdio.h>
#include <netdb.h>
#include <sys/file.h>
#include <ndbm.h>

#define	MAXALIASES	35

static struct hostent host;
static char *host_aliases[MAXALIASES];
static char hostbuf[BUFSIZ+1];

/*
 * The following is shared with gethostent.c
 */
extern	char *_host_file;
DBM	*_host_db = (DBM *)NULL;
int	_host_stayopen;	/* set by sethostent(), cleared by endhostent() */

static struct hostent *
fetchhost(key)
	datum key;
{
        register char *cp, *tp, **ap;
	int naliases;

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
	bcopy(cp, (char *)&naliases, sizeof(int)); cp += sizeof (int);
	for (ap = host_aliases; naliases > 0; naliases--) {
		*ap++ = tp;
		while (*tp++ = *cp++)
			;
	}
	*ap = (char *)NULL;
	host.h_aliases = host_aliases;
	bcopy(cp, (char *)&host.h_addrtype, sizeof (int));
	cp += sizeof (int);
	bcopy(cp, (char *)&host.h_length, sizeof (int));
	cp += sizeof (int);
	host.h_addr = tp;
	bcopy(cp, tp, host.h_length);
        return (&host);
}

struct hostent *
gethostbyname(nam)
	register char *nam;
{
	register struct hostent *hp;
	register char **cp;
        datum key;

	if ((_host_db == (DBM *)NULL)
	  && ((_host_db = dbm_open(_host_file, O_RDONLY)) == (DBM *)NULL)) {
		sethostent(_host_stayopen);
		while (hp = gethostent()) {
			if (strcmp(hp->h_name, nam) == 0)
				break;
			for (cp = hp->h_aliases; cp != 0 && *cp != 0; cp++)
				if (strcmp(*cp, nam) == 0)
					goto found;
		}
	found:
		if (!_host_stayopen)
			endhostent();
		return (hp);
	}
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
gethostbyaddr(addr, length, type)
	char *addr;
	register int length;
	register int type;
{
	register struct hostent *hp;
        datum key;

	if ((_host_db == (DBM *)NULL)
	  && ((_host_db = dbm_open(_host_file, O_RDONLY)) == (DBM *)NULL)) {
		sethostent(_host_stayopen);
		while (hp = gethostent()) {
			if (hp->h_addrtype == type && hp->h_length == length
			    && bcmp(hp->h_addr, addr, length) == 0)
				break;
		}
		if (!_host_stayopen)
			endhostent();
		return (hp);
	}
        key.dptr = addr;
        key.dsize = length;
	hp = fetchhost(key);
	if (!_host_stayopen) {
		dbm_close(_host_db);
		_host_db = (DBM *)NULL;
	}
        return (hp);
}
