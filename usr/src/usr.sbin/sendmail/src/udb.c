/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
#ifdef USERDB
static char sccsid [] = "@(#)udb.c	5.4 (Berkeley) %G% (with USERDB)";
#else
static char sccsid [] = "@(#)udb.c	5.4 (Berkeley) %G% (without USERDB)";
#endif
#endif

#include "sendmail.h"

#ifdef USERDB

#include <sys/file.h>
#include <sys/time.h>
#include <fcntl.h>
#include <netdb.h>
#include <db.h>

/*
**  UDBEXPAND -- look up user in database and expand
**
**	Parameters:
**		a -- address to expand.
**		sendq -- pointer to head of sendq to put the expansions in.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Modifies sendq.
*/

int	UdbPort = 1616;
int	UdbTimeout = 10;

struct udbent
{
	char	*udb_spec;		/* string version of spec */
	int	udb_type;		/* type of entry */
	union
	{
		/* type UE_REMOTE -- do remote call for lookup */
		struct
		{
			struct sockaddr_in _udb_addr;	/* address */
			int		_udb_timeout;	/* timeout */
		} udb_remote;
#define udb_addr	udb_u.udb_remote._udb_addr
#define udb_timeout	udb_u.udb_remote._udb_timeout

		/* type UE_FORWARD -- forward message to remote */
		struct
		{
			char	*_udb_fwdhost;	/* name of forward host */
		} udb_forward;
#define udb_fwdhost	udb_u.udb_forward._udb_fwdhost

		/* type UE_LOOKUP -- lookup in local database */
		struct
		{
			char	*_udb_dbname;	/* pathname of database */
			DB	*_udb_dbp;	/* open database ptr */
		} udb_lookup;
#define udb_dbname	udb_u.udb_lookup._udb_dbname
#define udb_dbp		udb_u.udb_lookup._udb_dbp
	} udb_u;
};

#define UDB_EOLIST	0	/* end of list */
#define UDB_SKIP	1	/* skip this entry */
#define UDB_REMOTE	2	/* look up in remote database */
#define UDB_LOOKUP	3	/* look up in local database */
#define UDB_FORWARD	4	/* forward to remote host */

#define MAXUDBENT	10	/* maximum number of UDB entries */

struct udbent	UdbEnts[MAXUDBENT + 1];
int		UdbSock = -1;

void
udbexpand(a, sendq)
	register ADDRESS *a;
	ADDRESS **sendq;
{
	int i;
	register char *p;
	DBT key;
	DBT info;
	static bool firstcall = TRUE;
	bool breakout;
	register struct udbent *up;
	int keylen;
	char keybuf[128];
	char buf[8192];

	if (tTd(28, 1))
		printf("expand(%s)\n", a->q_paddr);

	/* make certain we are supposed to send to this address */
	if (bitset(QDONTSEND, a->q_flags) ||
	    UdbSpec == NULL || UdbSpec[0] == '\0')
		return;
	CurEnv->e_to = a->q_paddr;

	/* on first call, locate the database */
	if (firstcall)
	{
		extern void _udbx_init();

		_udbx_init();
		firstcall = FALSE;
	}

	/* if name is too long, assume it won't match */
	if (strlen(a->q_user) > sizeof keybuf - 12)
		return;

	/* if name begins with a colon, it indicates our metadata */
	if (a->q_user[0] == ':')
		return;

	/* build actual database key */
	(void) strcpy(keybuf, a->q_user);
	(void) strcat(keybuf, ":maildrop");
	keylen = strlen(keybuf);

	breakout = FALSE;
	for (up = UdbEnts; !breakout; up++)
	{
		char *user;
		struct timeval timeout;
		fd_set fdset;

		/*
		**  Select action based on entry type.
		**
		**	On dropping out of this switch, "class" should
		**	explain the type of the data, and "user" should
		**	contain the user information.
		*/

		switch (up->udb_type)
		{
		  case UDB_LOOKUP:
			key.data = keybuf;
			key.size = keylen;
			i = (*up->udb_dbp->seq)(up->udb_dbp, &key, &info, R_CURSOR);
			if (i != 0 || info.size <= 0)
			{
				if (i < 0)
					syserr("udbexpand: db-get stat %s");
				if (tTd(28, 2))
					printf("expand: no match on %s\n", keybuf);
				continue;
			}

			/* there is at least one match -- start processing */
			breakout = TRUE;
			do
			{
				if (info.size < sizeof buf)
					user = buf;
				else
					user = xalloc(info.size + 1);
				bcopy(info.data, user, info.size);
				user[info.size] = '\0';

				message(Arpa_Info, "expanded to %s", user);
				AliasLevel++;
				sendtolist(user, a, sendq);
				AliasLevel--;

				if (user != buf)
					free(user);

				/* get the next record */
				i = (*up->udb_dbp->seq)(up->udb_dbp, &key, &info, R_NEXT);
			} while (i == 0 && key.size == keylen &&
					bcmp(key.data, keybuf, keylen) == 0);
			break;

		  case UDB_REMOTE:
			if (sendto(UdbSock, keybuf, keylen, 0,
				   (struct sockaddr *) &up->udb_addr,
				   sizeof up->udb_addr) < 0)
			{
				continue;
			}
			timeout.tv_sec = up->udb_timeout / 10;
			timeout.tv_usec = (up->udb_timeout % 10) * 100000;
			do
			{
				FD_ZERO(&fdset);
				FD_SET(UdbSock, &fdset);
				i = select(FD_SETSIZE, &fdset, NULL, NULL, &timeout);
			} while (i > 0 && !FD_ISSET(UdbSock, &fdset));
			if (i <= 0)
				continue;
			i = recvfrom(UdbSock, buf, sizeof buf - 1, 0, NULL, NULL);
			if (i < 0)
				continue;
			if (buf[0] != ' ' && buf[0] != '-')
				continue;
			breakout = TRUE;
			while (buf[0] == ' ' || buf[0] == '-')
			{
				user = &buf[1];
				buf[i] = '\0';
				message(Arpa_Info, "expanded to %s", user);
				AliasLevel++;
				sendtolist(user, a, sendq);
				AliasLevel--;

				/* try for next record */
				if (buf[0] == ' ')
					break;
				i = recvfrom(UdbSock, buf, sizeof buf - 1, 0, NULL, NULL);
				if (i < 0)
					break;
			}
			break;

		  case UDB_FORWARD:
			i = strlen(up->udb_fwdhost) + strlen(a->q_user) + 1;
			if (i < sizeof buf)
				user = buf;
			else
				user = xalloc(i + 1);
			(void) sprintf(user, "%s@%s", a->q_user, up->udb_fwdhost);
			message(Arpa_Info, "expanded to %s", user);
			AliasLevel++;
			sendtolist(user, a, sendq);
			AliasLevel--;
			if (user != buf)
				free(user);
			breakout = TRUE;
			break;

		  case UDB_EOLIST:
			breakout = TRUE;
			continue;

		  default:
			/* unknown entry type */
			continue;
		}
	}
}

void
_udbx_init()
{
	register char *p;
	int i;
	register struct udbent *up;
	char buf[8192];

	p = UdbSpec;
	up = UdbEnts;
	for (;;)
	{
		char *spec;
		auto int rcode;
		int nmx;
		register struct hostent *h;
		char *mxhosts[MAXMXHOSTS + 1];

		while (*p == ' ' || *p == '\t' || *p == ',')
			p++;
		if (*p == '\0')
			break;
		spec = p;
		p = index(p, ',');
		if (*p != '\0')
			*p++ = '\0';
		switch (*spec)
		{
		  case '+':	/* search remote database */
			h = gethostbyname(spec + 1);
			if (h == NULL)
				continue;
			up->udb_type = UDB_REMOTE;
			up->udb_addr.sin_family = h->h_addrtype;
			up->udb_addr.sin_len = h->h_length;
			bcopy(h->h_addr_list[0],
			      (char *) &up->udb_addr.sin_addr,
			      h->h_length);
			up->udb_addr.sin_port = UdbPort;
			up->udb_timeout = UdbTimeout;
			up++;

			/* set up a datagram socket */
			if (UdbSock < 0)
			{
				UdbSock = socket(AF_INET, SOCK_DGRAM, 0);
				(void) fcntl(UdbSock, F_SETFD, 1);
			}
			break;

		  case '*':	/* search remote database (expand MX) */
			nmx = getmxrr(spec + 1, mxhosts, "", &rcode);
			if (tTd(28, 16))
			{
				int i;

				printf("getmxrr(%s): %d", spec + 1, nmx);
				for (i = 0; i <= nmx; i++)
					printf(" %s", mxhosts[i]);
				printf("\n");
			}
			for (i = 0; i < nmx; i++)
			{
				h = gethostbyname(mxhosts[i]);
				if (h == NULL)
					continue;
				up->udb_type = UDB_REMOTE;
				up->udb_addr.sin_family = h->h_addrtype;
				up->udb_addr.sin_len = h->h_length;
				bcopy(h->h_addr_list[0],
				      (char *) &up->udb_addr.sin_addr,
				      h->h_length);
				up->udb_addr.sin_port = UdbPort;
				up->udb_timeout = UdbTimeout;
				up++;
			}

			/* set up a datagram socket */
			if (UdbSock < 0)
			{
				UdbSock = socket(AF_INET, SOCK_DGRAM, 0);
				(void) fcntl(UdbSock, F_SETFD, 1);
			}
			break;

		  case '@':	/* forward to remote host */
			up->udb_type = UDB_FORWARD;
			up->udb_fwdhost = spec + 1;
			up++;
			break;

		  case '/':	/* look up remote name */
			up->udb_dbp = dbopen(spec, O_RDONLY, 0644, DB_BTREE, NULL);
			if (up->udb_dbp == NULL)
				break;
			up->udb_type = UDB_LOOKUP;
			up++;
			break;
		}
	}
	up->udb_type = UDB_EOLIST;

	if (tTd(28, 4))
	{
		for (up = UdbEnts; ; up++)
		{
			switch (up->udb_type)
			{
			  case UDB_EOLIST:
				return;

			  case UDB_REMOTE:
				printf("REMOTE: addr %s, timeo %d\n",
					inet_ntoa(up->udb_addr.sin_addr),
					up->udb_timeout);
				break;

			  case UDB_LOOKUP:
				printf("LOOKUP\n");
				break;

			  case UDB_FORWARD:
				printf("FORWARD: host %s\n",
					up->udb_fwdhost);
				break;

			  default:
				printf("UNKNOWN\n");
				break;
			}
		}
	}
}

#else /* not USERDB */

void
udbexpand(a, sendq)
	ADDRESS *a;
	ADDRESS **sendq;
{
	return;
}

#endif /* USERDB */
