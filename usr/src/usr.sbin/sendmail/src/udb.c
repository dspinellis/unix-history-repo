/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
#ifdef USERDB
static char sccsid [] = "@(#)udb.c	5.3 (Berkeley) %G% (with USERDB)";
#else
static char sccsid [] = "@(#)udb.c	5.3 (Berkeley) %G% (without USERDB)";
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

struct udbent
{
	char	*udb_spec;		/* string version of spec */
	int	udb_type;		/* type of entry */
	union
	{
		/* type UE_REMOTE -- do remote call for lookup */
		struct
		{
			int		_udb_addrlen;	/* length of addr */
			struct sockaddr_in _udb_addr;	/* address */
			int		_udb_timeout;	/* timeout */
		} udb_remote;
#define udb_addrlen	udb_u.udb_remote._udb_addrlen
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


void
udbexpand(a, sendq)
	register ADDRESS *a;
	ADDRESS **sendq;
{
	int i;
	register char *p;
	auto char *class;
	auto char *list;
	DBT key;
	DBT info;
	register char *bp;
	static bool firstcall = TRUE;
	static int udbsock = -1;
	bool breakout;
	register struct udbent *up;
	struct udbent udbents[MAXUDBENT + 1];
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
		firstcall = FALSE;
		p = UdbSpec;
		up = udbents;
		for (;;)
		{
			char *spec;
			auto int rcode;
			int nmx;
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
			  case '*':	/* search remote database */
				expand("\001j", buf, &buf[sizeof(buf) - 1], CurEnv);
				nmx = getmxrr(spec + 1, mxhosts, buf, &rcode);
				for (i = 0; i < nmx; i++)
				{
					register struct hostent *h;

					h = gethostbyname(mxhosts[i]);
					if (h == NULL)
						continue;
					up->udb_type = UDB_REMOTE;
					up->udb_addr.sin_family = h->h_addrtype;
					up->udb_addrlen = h->h_length;
					bcopy(h->h_addr_list[0],
					      (char *) &up->udb_addr.sin_addr,
					      h->h_length);
					up++;
				}

				/* set up a datagram socket */
				if (udbsock < 0)
				{
					udbsock = socket(AF_INET, SOCK_DGRAM, 0);
					(void) fcntl(udbsock, F_SETFD, 1);
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
	}

	breakout = FALSE;
	for (up = udbents; !breakout; up++)
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
			key.data = a->q_user;
			key.size = strlen(key.data);
			i = (*up->udb_dbp->get)(up->udb_dbp, &key, &info, 0);
			if (i != 0 || info.size <= 0)
			{
				if (i < 0)
					syserr("udbexpand: db-get stat %s");
				if (tTd(28, 2))
					printf("expand: no match on %s\n", key.data);
				continue;
			}

			/* extract the class (first string) and data (second string) */
			class = info.data;
			i = strlen((char *) info.data) + 1;
			p = (char *) info.data + i;
			i = info.size - i;

			/* use internal buffer if it will fit; otherwise malloc */
			if (i < sizeof buf)
				user = buf;
			else
				user = xalloc(i + 1);
			bcopy(p, user, i);
			user[i] = '\0';
			break;

		  case UDB_REMOTE:
			if (sendto(udbsock, a->q_user, strlen(a->q_user), 0,
				   (struct sockaddr *) &up->udb_addr,
				   up->udb_addrlen) < 0)
			{
				continue;
			}
			timeout.tv_sec = up->udb_timeout / 10;
			timeout.tv_usec = (up->udb_timeout % 10) * 100000;
			do
			{
				FD_ZERO(&fdset);
				FD_SET(udbsock, &fdset);
				i = select(FD_SETSIZE, &fdset, NULL, NULL, &timeout);
			} while (i > 0 && !FD_ISSET(udbsock, &fdset));
			if (i <= 0)
				continue;
			i = recvfrom(udbsock, buf, sizeof buf - 1, 0, NULL, NULL);
			if (i < 0)
				continue;
			class = buf;
			user = &buf[strlen(buf)];
			buf[i] = '\0';
			break;

		  case UDB_FORWARD:
			class = "forward";
			i = strlen(up->udb_fwdhost) + strlen(a->q_user) + 1;
			if (i < sizeof buf)
				user = buf;
			else
				user = xalloc(i + 1);
			(void) sprintf(user, "%s@%s", a->q_user, up->udb_fwdhost);
			break;

		  case UDB_EOLIST:
			breakout = TRUE;
			continue;

		  default:
			/* unknown entry type */
			continue;
		}

		if (tTd(28, 1))
			printf("Class %s: %s\n", class, user);

		/* do special processing based on class */
		if (strcmp(class, "user") == 0 || strcmp(class, "forward") == 0)
		{
			message(Arpa_Info, "expanded to (%s) %s", class, user);
			AliasLevel++;
			sendtolist(user, a, sendq);
			AliasLevel--;
			breakout = TRUE;
		}

		/* free memory if we allocated it */
		if (up->udb_type == UDB_FORWARD || up->udb_type == UDB_LOOKUP)
		{
			if (user != buf)
				free(user);
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
