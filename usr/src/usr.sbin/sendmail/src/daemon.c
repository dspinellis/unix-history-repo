/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#include <errno.h>
#include "sendmail.h"
# include <sys/mx.h>

#ifndef lint
#ifdef DAEMON
static char sccsid[] = "@(#)daemon.c	8.35 (Berkeley) %G% (with daemon mode)";
#else
static char sccsid[] = "@(#)daemon.c	8.35 (Berkeley) %G% (without daemon mode)";
#endif
#endif /* not lint */

#ifdef DAEMON

# include <netdb.h>
# include <arpa/inet.h>

#ifdef NAMED_BIND
# include <arpa/nameser.h>
# include <resolv.h>
#endif

/*
**  DAEMON.C -- routines to use when running as a daemon.
**
**	This entire file is highly dependent on the 4.2 BSD
**	interprocess communication primitives.  No attempt has
**	been made to make this file portable to Version 7,
**	Version 6, MPX files, etc.  If you should try such a
**	thing yourself, I recommend chucking the entire file
**	and starting from scratch.  Basic semantics are:
**
**	getrequests()
**		Opens a port and initiates a connection.
**		Returns in a child.  Must set InChannel and
**		OutChannel appropriately.
**	clrdaemon()
**		Close any open files associated with getting
**		the connection; this is used when running the queue,
**		etc., to avoid having extra file descriptors during
**		the queue run and to avoid confusing the network
**		code (if it cares).
**	makeconnection(host, port, outfile, infile, usesecureport)
**		Make a connection to the named host on the given
**		port.  Set *outfile and *infile to the files
**		appropriate for communication.  Returns zero on
**		success, else an exit status describing the
**		error.
**	host_map_lookup(map, hbuf, avp, pstat)
**		Convert the entry in hbuf into a canonical form.
*/

static FILE	*MailPort;	/* port that mail comes in on */
/*
**  GETREQUESTS -- open mail IPC port and get requests.
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Waits until some interesting activity occurs.  When
**		it does, a child is created to process it, and the
**		parent waits for completion.  Return from this
**		routine is always in the child.  The file pointers
**		"InChannel" and "OutChannel" should be set to point
**		to the communication channel.
*/

int		DaemonSocket	= -1;		/* fd describing socket */
SOCKADDR	DaemonAddr;			/* socket for incoming */
int		ListenQueueSize = 10;		/* size of listen queue */
int		TcpRcvBufferSize = 0;		/* size of TCP receive buffer */
int		TcpSndBufferSize = 0;		/* size of TCP send buffer */

getrequests()
{
	int t;
	int on = 1;
	bool refusingconnections = TRUE;
	FILE *pidf;
	int socksize;
	extern void reapchild();

	/*
	**  Set up the address for the mailer.
	*/

	if (DaemonAddr.sin.sin_family == 0)
		DaemonAddr.sin.sin_family = AF_INET;
	if (DaemonAddr.sin.sin_addr.s_addr == 0)
		DaemonAddr.sin.sin_addr.s_addr = INADDR_ANY;
	if (DaemonAddr.sin.sin_port == 0)
	{
		register struct servent *sp;

		sp = getservbyname("smtp", "tcp");
		if (sp == NULL)
		{
			syserr("554 service \"smtp\" unknown");
			DaemonAddr.sin.sin_port = htons(25);
		}
		else
			DaemonAddr.sin.sin_port = sp->s_port;
	}

	/*
	**  Try to actually open the connection.
	*/

	if (tTd(15, 1))
		printf("getrequests: port 0x%x\n", DaemonAddr.sin.sin_port);

	/* get a socket for the SMTP connection */
	DaemonSocket = socket(DaemonAddr.sa.sa_family, SOCK_STREAM, 0);
	if (DaemonSocket < 0)
	{
		/* probably another daemon already */
		syserr("getrequests: can't create socket");
	  severe:
# ifdef LOG
		if (LogLevel > 0)
# endif /* LOG */
		finis();
	}

	/* turn on network debugging? */
	if (tTd(15, 101))
		(void) setsockopt(DaemonSocket, SOL_SOCKET, SO_DEBUG, (char *)&on, sizeof on);

	(void) setsockopt(DaemonSocket, SOL_SOCKET, SO_REUSEADDR, (char *)&on, sizeof on);
	(void) setsockopt(DaemonSocket, SOL_SOCKET, SO_KEEPALIVE, (char *)&on, sizeof on);

#ifdef SO_RCVBUF
	if (TcpRcvBufferSize > 0)
	{
		if (setsockopt(DaemonSocket, SOL_SOCKET, SO_RCVBUF,
			       (char *) &TcpRcvBufferSize,
			       sizeof(TcpRcvBufferSize)) < 0)
			syserr("getrequests: setsockopt(SO_RCVBUF)");
	}
#endif

	switch (DaemonAddr.sa.sa_family)
	{
# ifdef NETINET
	  case AF_INET:
		socksize = sizeof DaemonAddr.sin;
		break;
# endif

# ifdef NETISO
	  case AF_ISO:
		socksize = sizeof DaemonAddr.siso;
		break;
# endif

	  default:
		socksize = sizeof DaemonAddr;
		break;
	}

	if (bind(DaemonSocket, &DaemonAddr.sa, socksize) < 0)
	{
		syserr("getrequests: cannot bind");
		(void) close(DaemonSocket);
		goto severe;
	}

	(void) setsignal(SIGCHLD, reapchild);

	/* write the pid to the log file for posterity */
	pidf = fopen(PidFile, "w");
	if (pidf != NULL)
	{
		extern char *CommandLineArgs;

		/* write the process id on line 1 */
		fprintf(pidf, "%d\n", getpid());

		/* line 2 contains all command line flags */
		fprintf(pidf, "%s\n", CommandLineArgs);

		/* flush and close */
		fclose(pidf);
	}


	if (tTd(15, 1))
		printf("getrequests: %d\n", DaemonSocket);

	struct wh wbuf;

	wbuf.index = index;
	wbuf.count = 0;
	wbuf.ccount = cnt;
	wbuf.data = buf;
	write(MailPort, &wbuf, sizeof wbuf);
}
/*
**  MAKECONNECTION -- make a connection to an SMTP socket on another machine.
**
**	Parameters:
**		host -- the name of the host.
**		port -- the port number to connect to.
**		mci -- a pointer to the mail connection information
**			structure to be filled in.
**		usesecureport -- if set, use a low numbered (reserved)
**			port to provide some rudimentary authentication.
**
**	Returns:
**		An exit code telling whether the connection could be
**			made and if not why not.
**
**	Side Effects:
**		none.
*/

SOCKADDR	CurHostAddr;		/* address of current host */

int
makeconnection(host, port, mci, usesecureport)
	char *host;
	u_short port;
	register MCI *mci;
	bool usesecureport;
{
	register int i, s;
	register struct hostent *hp = (struct hostent *)NULL;
	SOCKADDR addr;
	int sav_errno;
	int addrlen;
#ifdef NAMED_BIND
	extern int h_errno;
#endif

	/*
	**  Set up the address for the mailer.
	**	Accept "[a.b.c.d]" syntax for host name.
	*/

#ifdef NAMED_BIND
	h_errno = 0;
#endif
	errno = 0;
	bzero(&CurHostAddr, sizeof CurHostAddr);
	SmtpPhase = mci->mci_phase = "initial connection";
	CurHostName = host;

	if (host[0] == '[')
	{
		long hid;
		register char *p = strchr(host, ']');

		if (p != NULL)
		{
			*p = '\0';
#ifdef NETINET
			hid = inet_addr(&host[1]);
			if (hid == -1)
#endif
			{
				/* try it as a host name (avoid MX lookup) */
				hp = gethostbyname(&host[1]);
				*p = ']';
				goto gothostent;
			}
			*p = ']';
		}
		if (p == NULL)
		{
			usrerr("553 Invalid numeric domain spec \"%s\"", host);
			return (EX_NOHOST);
		}
#ifdef NETINET
		addr.sin.sin_family = AF_INET;		/*XXX*/
		addr.sin.sin_addr.s_addr = hid;
#endif
	}
	else
	{
		hp = gethostbyname(host);
gothostent:
		if (hp == NULL)
		{
#ifdef NAMED_BIND
			if (errno == ETIMEDOUT || h_errno == TRY_AGAIN)
				return (EX_TEMPFAIL);

			/* if name server is specified, assume temp fail */
			if (errno == ECONNREFUSED && UseNameServer)
				return (EX_TEMPFAIL);
#endif
			return (EX_NOHOST);
		}
		addr.sa.sa_family = hp->h_addrtype;
		switch (hp->h_addrtype)
		{
#ifdef NETINET
		  case AF_INET:
			bcopy(hp->h_addr,
				&addr.sin.sin_addr,
				sizeof addr.sin.sin_addr);
			break;
#endif

		  default:
			bcopy(hp->h_addr,
				addr.sa.sa_data,
				hp->h_length);
			break;
		}
		i = 1;
	}

	/*
	**  Determine the port number.
	*/

	if (port != 0)
		port = htons(port);
	else
	{
		register struct servent *sp = getservbyname("smtp", "tcp");

		if (sp == NULL)
		{
			syserr("554 makeconnection: service \"smtp\" unknown");
			port = htons(25);
		}
		else
			port = sp->s_port;
	}

	switch (addr.sa.sa_family)
	{
#ifdef NETINET
	  case AF_INET:
		addr.sin.sin_port = port;
		addrlen = sizeof (struct sockaddr_in);
		break;
#endif

#ifdef NETISO
	  case AF_ISO:
		/* assume two byte transport selector */
		bcopy((char *) &port, TSEL((struct sockaddr_iso *) &addr), 2);
		addrlen = sizeof (struct sockaddr_iso);
		break;
#endif

	  default:
		syserr("Can't connect to address family %d", addr.sa.sa_family);
		return (EX_NOHOST);
	}

	/*
	**  Try to actually open the connection.
	*/

#ifdef XLA
	/* if too many connections, don't bother trying */
	if (!xla_noqueue_ok(host))
		return EX_TEMPFAIL;
#endif

	for (;;)
	{
		if (tTd(16, 1))
			printf("makeconnection (%s [%s])\n",
				host, anynet_ntoa(&addr));

		/* save for logging */
		CurHostAddr = addr;

		if (usesecureport)
		{
			int rport = IPPORT_RESERVED - 1;

			s = rresvport(&rport);
		}
		else
		{
			s = socket(AF_INET, SOCK_STREAM, 0);
		}
		if (s < 0)
		{
			sav_errno = errno;
			syserr("makeconnection: no socket");
			goto failure;
		}

#ifdef SO_SNDBUF
		if (TcpSndBufferSize > 0)
		{
			if (setsockopt(s, SOL_SOCKET, SO_SNDBUF,
				       (char *) &TcpSndBufferSize,
				       sizeof(TcpSndBufferSize)) < 0)
				syserr("makeconnection: setsockopt(SO_SNDBUF)");
		}
#endif

		if (tTd(16, 1))
			printf("makeconnection: fd=%d\n", s);

		/* turn on network debugging? */
		if (tTd(16, 101))
		{
			int on = 1;
			(void) setsockopt(DaemonSocket, SOL_SOCKET, SO_DEBUG,
					  (char *)&on, sizeof on);
		}
		if (CurEnv->e_xfp != NULL)
			(void) fflush(CurEnv->e_xfp);		/* for debugging */
		errno = 0;					/* for debugging */
		if (connect(s, (struct sockaddr *) &addr, addrlen) >= 0)
			break;

		/* couldn't connect.... figure out why */
		sav_errno = errno;
		(void) close(s);
		if (hp && hp->h_addr_list[i])
		{
			if (tTd(16, 1))
				printf("Connect failed (%s); trying new address....\n",
					errstring(sav_errno));
			switch (addr.sa.sa_family)
			{
#ifdef NETINET
			  case AF_INET:
				bcopy(hp->h_addr_list[i++],
				      &addr.sin.sin_addr,
				      sizeof addr.sin.sin_addr);
				break;
#endif

			  default:
				bcopy(hp->h_addr_list[i++],
					addr.sa.sa_data,
					hp->h_length);
				break;
			}
			continue;
		}

		/* failure, decide if temporary or not */
	failure:
#ifdef XLA
		xla_host_end(host);
#endif
		if (transienterror(sav_errno))
			return EX_TEMPFAIL;
		else
		{
			message("%s", errstring(sav_errno));
			return (EX_UNAVAILABLE);
		}
	}

	/* connection ok, put it into canonical form */
	if ((mci->mci_out = fdopen(s, "w")) == NULL ||
	    (s = dup(s)) < 0 ||
	    (mci->mci_in = fdopen(s, "r")) == NULL)
	{
		syserr("cannot open SMTP client channel, fd=%d", s);
		return EX_TEMPFAIL;
	}

	return (EX_OK);
}
/*
**  MYHOSTNAME -- return the name of this host.
**
**	Parameters:
**		hostbuf -- a place to return the name of this host.
**		size -- the size of hostbuf.
**
**	Returns:
**		A list of aliases for this host.
**
**	Side Effects:
**		Adds numeric codes to $=w.
*/

char **
myhostname(hostbuf, size)
	char hostbuf[];
	int size;
{
	register struct hostent *hp;
	extern struct hostent *gethostbyname();

	if (gethostname(hostbuf, size) < 0)
	{
		(void) strcpy(hostbuf, "localhost");
	}
	hp = gethostbyname(hostbuf);
	if (hp != NULL)
	{
		(void) strncpy(hostbuf, hp->h_name, size - 1);
		hostbuf[size - 1] = '\0';

		if (hp->h_addrtype == AF_INET && hp->h_length == 4)
		{
			register int i;

			for (i = 0; hp->h_addr_list[i] != NULL; i++)
			{
				char ipbuf[100];

				sprintf(ipbuf, "[%s]",
					inet_ntoa(*((struct in_addr *) hp->h_addr_list[i])));
				setclass('w', ipbuf);
			}
		}

		return (hp->h_aliases);
	}
	else
		return (NULL);
}
/*
**  GETAUTHINFO -- get the real host name asociated with a file descriptor
**
**	Uses RFC1413 protocol to try to get info from the other end.
**
**	Parameters:
**		fd -- the descriptor
**
**	Returns:
**		The user@host information associated with this descriptor.
*/

#if IDENTPROTO

static jmp_buf	CtxAuthTimeout;

static
authtimeout()
{
	longjmp(CtxAuthTimeout, 1);
}

#endif

char *
getauthinfo(fd)
	int fd;
{
	SOCKADDR fa;
	int falen;
	register char *p;
#if IDENTPROTO
	SOCKADDR la;
	int lalen;
	register struct servent *sp;
	int s;
	int i;
	EVENT *ev;
#endif
	static char hbuf[MAXNAME * 2 + 2];
	extern char *hostnamebyanyaddr();
	extern char RealUserName[];			/* main.c */

	falen = sizeof fa;
	if (getpeername(fd, &fa.sa, &falen) < 0 || falen <= 0 ||
	    fa.sa.sa_family == 0)
	{
		(void) sprintf(hbuf, "%s@localhost", RealUserName);
		if (tTd(9, 1))
			printf("getauthinfo: %s\n", hbuf);
		return hbuf;
	}

#if IDENTPROTO
	if (TimeOuts.to_ident == 0)
		goto noident;

	lalen = sizeof la;
	if (fa.sa.sa_family != AF_INET ||
	    getsockname(fd, &la.sa, &lalen) < 0 || lalen <= 0 ||
	    la.sa.sa_family != AF_INET)
	{
		/* no ident info */
		goto noident;
	}

	/* create ident query */
	(void) sprintf(hbuf, "%d,%d\r\n",
		ntohs(fa.sin.sin_port), ntohs(la.sin.sin_port));

	/* create local address */
	la.sin.sin_port = 0;

	/* create foreign address */
	sp = getservbyname("auth", "tcp");
	if (sp != NULL)
		fa.sin.sin_port = sp->s_port;
	else
		fa.sin.sin_port = htons(113);

	s = -1;
	if (setjmp(CtxAuthTimeout) != 0)
	{
		if (s >= 0)
			(void) close(s);
		goto noident;
	}

	/* put a timeout around the whole thing */
	ev = setevent(TimeOuts.to_ident, authtimeout, 0);

	/* connect to foreign IDENT server using same address as SMTP socket */
	s = socket(AF_INET, SOCK_STREAM, 0);
	if (s < 0)
	{
		clrevent(ev);
		goto noident;
	}
	if (bind(s, &la.sa, sizeof la.sin) < 0 ||
	    connect(s, &fa.sa, sizeof fa.sin) < 0)
	{
		goto closeident;
	}

	if (tTd(9, 10))
		printf("getauthinfo: sent %s", hbuf);

	/* send query */
	if (write(s, hbuf, strlen(hbuf)) < 0)
		goto closeident;

	/* get result */
	i = read(s, hbuf, sizeof hbuf);
	(void) close(s);
	clrevent(ev);
	if (i <= 0)
		goto noident;
	if (hbuf[--i] == '\n' && hbuf[--i] == '\r')
		i--;
	hbuf[++i] = '\0';

	if (tTd(9, 3))
		printf("getauthinfo:  got %s\n", hbuf);

	/* parse result */
	p = strchr(hbuf, ':');
	if (p == NULL)
	{
		/* malformed response */
		goto noident;
	}
	while (isascii(*++p) && isspace(*p))
		continue;
	if (strncasecmp(p, "userid", 6) != 0)
	{
		/* presumably an error string */
		goto noident;
	}
	p += 6;
	while (isascii(*p) && isspace(*p))
		p++;
	if (*p++ != ':')
	{
		/* either useridxx or malformed response */
		goto noident;
	}

	/* p now points to the OSTYPE field */
	p = strchr(p, ':');
	if (p == NULL)
	{
		/* malformed response */
		goto noident;
	}

	/* 1413 says don't do this -- but it's broken otherwise */
	while (isascii(*++p) && isspace(*p))
		continue;

	/* p now points to the authenticated name */
	(void) sprintf(hbuf, "%s@%s",
		p, RealHostName == NULL ? "localhost" : RealHostName);
	goto finish;

closeident:
	(void) close(s);
	clrevent(ev);

#endif /* IDENTPROTO */

noident:
	if (RealHostName == NULL)
	{
		if (tTd(9, 1))
			printf("getauthinfo: NULL\n");
		return NULL;
	}
	(void) strcpy(hbuf, RealHostName);

finish:
	if (RealHostName != NULL && RealHostName[0] != '[')
	{
		p = &hbuf[strlen(hbuf)];
		(void) sprintf(p, " [%s]", anynet_ntoa(&RealHostAddr));
	}
	if (tTd(9, 1))
		printf("getauthinfo: %s\n", hbuf);
	return hbuf;
}
/*
**  HOST_MAP_LOOKUP -- turn a hostname into canonical form
**
**	Parameters:
**		map -- a pointer to this map (unused).
**		name -- the (presumably unqualified) hostname.
**		av -- unused -- for compatibility with other mapping
**			functions.
**		statp -- an exit status (out parameter) -- set to
**			EX_TEMPFAIL if the name server is unavailable.
**
**	Returns:
**		The mapping, if found.
**		NULL if no mapping found.
**
**	Side Effects:
**		Looks up the host specified in hbuf.  If it is not
**		the canonical name for that host, return the canonical
**		name.
*/

char *
host_map_lookup(map, name, av, statp)
	MAP *map;
	char *name;
	char **av;
	int *statp;
{
	register struct hostent *hp;
	u_long in_addr;
	char *cp;
	int i;
	register STAB *s;
	char hbuf[MAXNAME];
	extern struct hostent *gethostbyaddr();
	extern int h_errno;

	/*
	**  See if we have already looked up this name.  If so, just
	**  return it.
	*/

	s = stab(name, ST_NAMECANON, ST_ENTER);
	if (bitset(NCF_VALID, s->s_namecanon.nc_flags))
	{
		if (tTd(9, 1))
			printf("host_map_lookup(%s) => CACHE %s\n",
				name, s->s_namecanon.nc_cname);
		errno = s->s_namecanon.nc_errno;
		h_errno = s->s_namecanon.nc_herrno;
		*statp = s->s_namecanon.nc_stat;
		if (CurEnv->e_message == NULL && *statp == EX_TEMPFAIL)
		{
			sprintf(hbuf, "%s: Name server timeout",
				shortenstring(name, 33));
			CurEnv->e_message = newstr(hbuf);
		}
		return s->s_namecanon.nc_cname;
	}

	/*
	**  If first character is a bracket, then it is an address
	**  lookup.  Address is copied into a temporary buffer to
	**  strip the brackets and to preserve name if address is
	**  unknown.
	*/

	if (*name != '[')
	{
		extern bool getcanonname();

		if (tTd(9, 1))
			printf("host_map_lookup(%s) => ", name);
		s->s_namecanon.nc_flags |= NCF_VALID;		/* will be soon */
		(void) strcpy(hbuf, name);
		if (getcanonname(hbuf, sizeof hbuf - 1, TRUE))
		{
			if (tTd(9, 1))
				printf("%s\n", hbuf);
			cp = map_rewrite(map, hbuf, strlen(hbuf), av);
			s->s_namecanon.nc_cname = newstr(cp);
			return cp;
		}
		else
		{
			register struct hostent *hp;

			if (tTd(9, 1))
				printf("FAIL (%d)\n", h_errno);
			s->s_namecanon.nc_errno = errno;
			s->s_namecanon.nc_herrno = h_errno;
			switch (h_errno)
			{
			  case TRY_AGAIN:
				if (UseNameServer)
				{
					sprintf(hbuf, "%s: Name server timeout",
						shortenstring(name, 33));
					message("%s", hbuf);
					if (CurEnv->e_message == NULL)
						CurEnv->e_message = newstr(hbuf);
				}
				*statp = EX_TEMPFAIL;
				break;

			  case HOST_NOT_FOUND:
				*statp = EX_NOHOST;
				break;

			  case NO_RECOVERY:
				*statp = EX_SOFTWARE;
				break;

			  default:
				*statp = EX_UNAVAILABLE;
				break;
			}
			s->s_namecanon.nc_stat = *statp;
			if (*statp != EX_TEMPFAIL || UseNameServer)
				return NULL;

			/*
			**  Try to look it up in /etc/hosts
			*/

			hp = gethostbyname(name);
			if (hp == NULL)
			{
				/* no dice there either */
				s->s_namecanon.nc_stat = *statp = EX_NOHOST;
				return NULL;
			}

			s->s_namecanon.nc_stat = *statp = EX_OK;
			cp = map_rewrite(map, hp->h_name, strlen(hp->h_name), av);
			s->s_namecanon.nc_cname = newstr(cp);
			return cp;
		}
	}
	if ((cp = strchr(name, ']')) == NULL)
		return (NULL);
	*cp = '\0';
	in_addr = inet_addr(&name[1]);

	/* nope -- ask the name server */
	hp = gethostbyaddr((char *)&in_addr, sizeof(struct in_addr), AF_INET);
	s->s_namecanon.nc_errno = errno;
	s->s_namecanon.nc_herrno = h_errno;
	s->s_namecanon.nc_flags |= NCF_VALID;		/* will be soon */
	if (hp == NULL)
	{
		s->s_namecanon.nc_stat = *statp = EX_NOHOST;
		return (NULL);
	}

	/* found a match -- copy out */
	cp = map_rewrite(map, hp->h_name, strlen(hp->h_name), av);
	s->s_namecanon.nc_stat = *statp = EX_OK;
	s->s_namecanon.nc_cname = newstr(cp);
	return cp;
}
/*
**  ANYNET_NTOA -- convert a network address to printable form.
**
**	Parameters:
**		sap -- a pointer to a sockaddr structure.
**
**	Returns:
**		A printable version of that sockaddr.
*/

char *
anynet_ntoa(sap)
	register SOCKADDR *sap;
{
	register char *bp;
	register char *ap;
	int l;
	static char buf[100];

	/* check for null/zero family */
	if (sap == NULL)
		return "NULLADDR";
	if (sap->sa.sa_family == 0)
		return "0";

	switch (sap->sa.sa_family)
	{
#ifdef MAYBENEXTRELEASE		/*** UNTESTED *** UNTESTED *** UNTESTED ***/
#ifdef NETUNIX
	  case AF_UNIX:
	  	if (sap->sunix.sun_path[0] != '\0')
	  		sprintf(buf, "[UNIX: %.64s]", sap->sunix.sun_path);
	  	else
	  		sprintf(buf, "[UNIX: localhost]");
		return buf;
#endif
#endif

#ifdef NETINET
	  case AF_INET:
		return inet_ntoa(((struct sockaddr_in *) sap)->sin_addr);
#endif

	  default:
	  	/* this case is only to ensure syntactic correctness */
	  	break;
	}

	/* unknown family -- just dump bytes */
	(void) sprintf(buf, "Family %d: ", sap->sa.sa_family);
	bp = &buf[strlen(buf)];
	ap = sap->sa.sa_data;
	for (l = sizeof sap->sa.sa_data; --l >= 0; )
	{
		(void) sprintf(bp, "%02x:", *ap++ & 0377);
		bp += 3;
	}
	*--bp = '\0';
	return buf;
}
/*
**  HOSTNAMEBYANYADDR -- return name of host based on address
**
**	Parameters:
**		sap -- SOCKADDR pointer
**
**	Returns:
**		text representation of host name.
**
**	Side Effects:
**		none.
*/

char *
hostnamebyanyaddr(sap)
	register SOCKADDR *sap;
{
	register struct hostent *hp;
	int saveretry;

#ifdef NAMED_BIND
	/* shorten name server timeout to avoid higher level timeouts */
	saveretry = _res.retry;
	_res.retry = 3;
#endif /* NAMED_BIND */

	switch (sap->sa.sa_family)
	{
#ifdef NETINET
	  case AF_INET:
		hp = gethostbyaddr((char *) &sap->sin.sin_addr,
			sizeof sap->sin.sin_addr,
			AF_INET);
		break;
#endif

#ifdef NETISO
	  case AF_ISO:
		hp = gethostbyaddr((char *) &sap->siso.siso_addr,
			sizeof sap->siso.siso_addr,
			AF_ISO);
		break;
#endif

#ifdef MAYBENEXTRELEASE		/*** UNTESTED *** UNTESTED *** UNTESTED ***/
	  case AF_UNIX:
		hp = NULL;
		break;
#endif

	  default:
		hp = gethostbyaddr(sap->sa.sa_data,
			   sizeof sap->sa.sa_data,
			   sap->sa.sa_family);
		break;
	}

#ifdef NAMED_BIND
	_res.retry = saveretry;
#endif /* NAMED_BIND */

	if (hp != NULL)
		return hp->h_name;
	else
	{
		/* produce a dotted quad */
		static char buf[512];

		(void) sprintf(buf, "[%s]", anynet_ntoa(sap));
		return buf;
	}
}

# else /* DAEMON */
/* code for systems without sophisticated networking */

/*
**  MYHOSTNAME -- stub version for case of no daemon code.
**
**	Can't convert to upper case here because might be a UUCP name.
**
**	Mark, you can change this to be anything you want......
*/

char **
myhostname(hostbuf, size)
	char hostbuf[];
	int size;
{
	register FILE *f;

	hostbuf[0] = '\0';
	f = fopen("/usr/include/whoami", "r");
	if (f != NULL)
	{
		(void) fgets(hostbuf, size, f);
		fixcrlf(hostbuf, TRUE);
		(void) fclose(f);
	}
	return (NULL);
}
/*
**  GETAUTHINFO -- get the real host name asociated with a file descriptor
**
**	Parameters:
**		fd -- the descriptor
**
**	Returns:
**		The host name associated with this descriptor, if it can
**			be determined.
**		NULL otherwise.
**
**	Side Effects:
**		none
*/

char *
getauthinfo(fd)
	int fd;
{
	return NULL;
}
/*
**  MAPHOSTNAME -- turn a hostname into canonical form
**
**	Parameters:
**		map -- a pointer to the database map.
**		name -- a buffer containing a hostname.
**		avp -- a pointer to a (cf file defined) argument vector.
**		statp -- an exit status (out parameter).
**
**	Returns:
**		mapped host name
**		FALSE otherwise.
**
**	Side Effects:
**		Looks up the host specified in name.  If it is not
**		the canonical name for that host, replace it with
**		the canonical name.  If the name is unknown, or it
**		is already the canonical name, leave it unchanged.
*/

/*ARGSUSED*/
char *
host_map_lookup(map, name, avp, statp)
	MAP *map;
	char *name;
	char **avp;
	char *statp;
{
	register struct hostent *hp;

	hp = gethostbyname(name);
	if (hp != NULL)
		return hp->h_name;
	*statp = EX_NOHOST;
	return NULL;
}

#endif /* DAEMON */
