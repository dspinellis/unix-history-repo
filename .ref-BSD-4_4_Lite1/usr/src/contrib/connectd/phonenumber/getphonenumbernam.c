#include <stdio.h>
#include "../phonenumber.h"
#include <sys/file.h>
#include <ndbm.h>
#include <ctype.h>

#define	MAXALIASES	35

static struct phonenumberent phonenumber;
static char *phonenumber_aliases[MAXALIASES];
static char phonenumberbuf[BUFSIZ+1];
static char *phonenumber_addrs[256];

int pn_errno;

/*
 * The following is shared with getphonenumberent.c
 */
extern	char *_phonenumber_file;
DBM	*_phonenumber_db = (DBM *)NULL;
int	_phonenumber_stayopen;	/* set by setphonenumberent(),
					cleared by endphonenumberent() */

static struct phonenumberent *
fetchphonenumber(key)
	datum key;
{
        register char *cp, *tp, **ap;
	int naliases;

        if (key.dptr == 0)
                return ((struct phonenumberent *)NULL);
	key = dbm_fetch(_phonenumber_db, key);
	if (key.dptr == 0)
                return ((struct phonenumberent *)NULL);
        cp = key.dptr;
	tp = phonenumberbuf;
	phonenumber.pn_name = tp;
	while (*tp++ = *cp++)
		;
	bcopy(cp, (char *)&naliases, sizeof(int)); cp += sizeof (int);
	for (ap = phonenumber_aliases; naliases > 0; naliases--) {
		*ap++ = tp;
		while (*tp++ = *cp++)
			;
	}
	*ap = (char *)NULL;
	phonenumber.pn_aliases = phonenumber_aliases;
	bcopy(cp, (char *)&phonenumber.h_length, sizeof (int));
	cp += sizeof (int);
	phonenumber.h_addr_list = phonenumber_addrs;
	phonenumber.h_addr = tp;
	bcopy(cp, tp, phonenumber.h_length);
        return (&phonenumber);
}

struct phonenumberent *
getphonenumberbyname(nam)
	register char *nam;
{
	register struct phonenumberent *hp;
	register char **cp;
        datum key;
	char lowname[128];
	register char *lp = lowname;
	
	while (*nam)
		if (isupper(*nam))
			*lp++ = tolower(*nam++);
		else
			*lp++ = *nam++;
	*lp = '\0';

	if ((_phonenumber_db == (DBM *)NULL)
	  && ((_phonenumber_db = dbm_open(_phonenumber_file, O_RDONLY)) == (DBM *)NULL)) {
		setphonenumberent(_phonenumber_stayopen);
		while (hp = getphonenumberent()) {
			if (strcmp(hp->h_name, lowname) == 0)
				break;
			for (cp = hp->h_aliases; cp != 0 && *cp != 0; cp++)
				if (strcmp(*cp, lowname) == 0)
					goto found;
		}
	found:
		if (!_phonenumber_stayopen)
			endphonenumberent();
		return (hp);
	}
        key.dptr = lowname;
        key.dsize = strlen(lowname);
	hp = fetchphonenumber(key);
	if (!_phonenumber_stayopen) {
		dbm_close(_phonenumber_db);
		_phonenumber_db = (DBM *)NULL;
	}
	if ( hp == NULL)
		h_errno = HOST_NOT_FOUND;
        return (hp);
}

struct phonenumberent *
getphonenumberbyaddr(addr, length, type)
	char *addr;
	register int length;
	register int type;
{
	register struct phonenumberent *hp;
        datum key;

	if ((_phonenumber_db == (DBM *)NULL)
	  && ((_phonenumber_db = dbm_open(_phonenumber_file, O_RDONLY)) == (DBM *)NULL)) {
		setphonenumberent(_phonenumber_stayopen);
		while (pnp = getphonenumberent()) {
			if (pnp->h_addrtype == type && pnp->h_length == length
			    && bcmp(hp->h_addr, addr, length) == 0)
				break;
		}
		if (!_phonenumber_stayopen)
			endphonenumberent();
		if ( hp == NULL)
			pn_errno = ENTRY_NOT_FOUND;
		return (pnp);
	}
        key.dptr = addr;
        key.dsize = length;
	pnp = fetchphonenumber(key);
	if (!_phonenumber_stayopen) {
		dbm_close(_phonenumber_db);
		_phonenumber_db = (DBM *)NULL;
	}
	if ( pnp == NULL)
		pn_errno = ENTRY_NOT_FOUND;
        return (pnp);
}
