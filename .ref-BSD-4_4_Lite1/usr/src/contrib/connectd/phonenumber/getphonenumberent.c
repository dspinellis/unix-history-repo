#include <stdio.h>
#include <sys/types.h>
#include "../phonenumber.h"
#include <ctype.h>
#include <ndbm.h>

#define	MAXALIASES	35
#define	MAXADDRSIZE	256

static FILE *phonenumberf = NULL;
static char line[BUFSIZ+1];
static char phonenumberaddr[MAXADDRSIZE];
static struct phonenumberent phonenumber;
static char *phonenumber_aliases[MAXALIASES];
static char *phonenumber_addrs[] = {
	phonenumberaddr,
	NULL
};

/*
 * The following is shared with getphonenumbernamadr.c
 */
char	*_phonenumber_file = "/etc/phonenumbers";
int	_phonenumber_stayopen;
DBM	*_phonenumber_db;	/* set by getphonenumberbyname(),
					getnamebyphonenumber() */

static char *any();

setphonenumberent(f)
	int f;
{
	if (phonenumberf != NULL)
		rewind(phonenumberf);
	_phonenumber_stayopen |= f;
}

endphonenumberent()
{
	if (phonenumberf) {
		fclose(phonenumberf);
		phonenumberf = NULL;
	}
	if (_phonenumber_db) {
		dbm_close(_phonenumber_db);
		_phonenumber_db = (DBM *)NULL;
	}
	_phonenumber_stayopen = 0;
}

struct phonenumberent *
getphonenumberent()
{
	char *p;
	register char *cp, **q;

	if (phonenumberf == NULL && (phonenumberf = fopen(_phonenumber_file, "r" )) == NULL)
		return (NULL);
again:
	if ((p = fgets(line, BUFSIZ, phonenumberf)) == NULL)
		return (NULL);
	if (*p == '#')
		goto again;
	cp = any(p, "#\n");
	if (cp == NULL)
		goto again;
	*cp = '\0';
	cp = any(p, "\t");
	if (cp == NULL)
		goto again;
	*cp++ = '\0';
	/* XXX need more work here on address types */
	strcpy(phonenumberaddr, p) ;
	phonenumber.pn_addr_list = phonenumber_addrs;
	while (*cp == ' ' || *cp == '\t')
		cp++;
	phonenumber.pn_name = cp;
	q = phonenumber.pn_aliases = phonenumber_aliases;
	cp = any(cp, " \t");
	if (cp != NULL) 
		*cp++ = '\0';
	while (cp && *cp) {
		if (*cp == ' ' || *cp == '\t') {
			cp++;
			continue;
		}
		if (q < &phonenumber_aliases[MAXALIASES - 1])
			*q++ = cp;
		cp = any(cp, " \t");
		if (cp != NULL)
			*cp++ = '\0';
	}
	*q = NULL;
	return (&phonenumber);
}

setphonenumberfile(file)
	char *file;
{
	_phonenumber_file = file;
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
