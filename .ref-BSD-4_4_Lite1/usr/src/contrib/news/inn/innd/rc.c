/*  $Revision: 1.31 $
**
**  Routines for the remote connect channel.  Create an Internet stream socket
**  that processes connect to.  If the incoming site is not one of our feeds,
**  then we pass the connection off to the standard nntp daemon.
*/
#include "innd.h"
#include <netdb.h>


#if	!defined(NETSWAP)
#if	!defined(htons)
extern unsigned short	htons();
#endif	/* !defined(htons) */
#if	!defined(htonl)
extern unsigned long	htonl();
#endif	/* !defined(htonl) */
#endif	/* !defined(NETSWAP) */

#define COPYADDR(dest, src) \
	    (void)memcpy((POINTER)dest, (POINTER)src, (SIZE_T)sizeof (INADDR))

/*
**  A remote host has an address and a password.
*/
typedef struct _REMOTEHOST {
    char	*Name;
    INADDR	Address;
    char	*Password;
    char	**Patterns;
} REMOTEHOST;

STATIC INADDR		*RCmaster;
STATIC int		RCnmaster;
STATIC char		*RCslaveflag;
STATIC char		RCnnrpd[] = _PATH_NNRPD;
STATIC char		RCnnrqd[] = _PATH_NNQRD;
STATIC char		RCnntpd[] = _PATH_NNTPD;
STATIC CHANNEL		*RCchan;
STATIC REMOTEHOST	*RCpeerlist;
STATIC int		RCnpeerlist;
STATIC REMOTEHOST	*RCnolimitlist;
STATIC int		RCnnolimitlist;


/*
**  See if the site properly entered the password.
*/
BOOL
RCauthorized(cp, pass)
    register CHANNEL	*cp;
    char		*pass;
{
    register REMOTEHOST	*rp;
    register int	i;

    for (rp = RCpeerlist, i = RCnpeerlist; --i >= 0; rp++)
	/* SUPPRESS 112 *//* Retrieving long where char is stored */
	if (cp->Address.s_addr == rp->Address.s_addr) {
	    if (rp->Password[0] == '\0' || EQ(pass, rp->Password))
		return TRUE;
	    syslog(L_ERROR, "%s bad_auth", inet_ntoa(cp->Address));
	    return FALSE;
	}

    if (!AnyIncoming)
	/* Not found in our table; this can't happen. */
	syslog(L_ERROR, "%s not_found", inet_ntoa(cp->Address));

    /* Anonymous hosts should not authenticate. */
    return FALSE;
}


/*
**  See if a host is in the "nolimit" file.
*/
BOOL
RCnolimit(cp)
    register CHANNEL	*cp;
{
    register REMOTEHOST	*rp;
    register int	i;

    for (rp = RCnolimitlist, i = RCnnolimitlist; --i >= 0; rp++)
	/* SUPPRESS 112 *//* Retrieving long where char is stored */
	if (cp->Address.s_addr == rp->Address.s_addr)
	    return TRUE;
    return FALSE;
}


/*
**  Is this an address of the master?
*/
BOOL
RCismaster(addr)
    INADDR		addr;
{
    register INADDR	*ip;
    register int	i;

    if (AmSlave)
	for (i = RCnmaster, ip = RCmaster; --i >= 0; ip++)
	    /* SUPPRESS 112 *//* Retrieving long where char is stored */
	    if (addr.s_addr == ip->s_addr)
		return TRUE;
    return FALSE;
}


/*
**  Hand off a descriptor to NNRPD.
*/
void
RChandoff(fd, h)
    int		fd;
    HANDOFF	h;
{
    STRING	argv[6];
    char	buff[SMBUF];
    int		i;

    if (SetNonBlocking(fd, FALSE) < 0)
	syslog(L_ERROR, "%s cant nonblock %d in RChandoff %m", LogName, fd);
    switch (h) {
    default:
	syslog(L_ERROR, "%s internal RChandoff %d type %d", LogName, fd, h);
	/* FALLTHROUGH */
    case HOnnrpd:	argv[0] = RCnnrpd;	break;
    case HOnnrqd:	argv[0] = RCnnrqd;	break;
    case HOnntpd:	argv[0] = RCnntpd;	break;
    }
    argv[1] = "-s                                                ";
    i = 2;
    if (NNRPReason) {
	(void)sprintf(buff, "-r%s", NNRPReason);
	argv[i++] = buff;
    }
    if (NNRPTracing)
	argv[i++] = "-t";
    if (RCslaveflag)
	argv[i++] = RCslaveflag;
    argv[i] = NULL;

    /* Call NNRP; don't send back a QUIT message if Spawn fails since  
     * that's a major error we want to find out about quickly. */
    (void)Spawn(fd, fd, fd, argv);
}


/*
**  Read function.  Accept the connection and either create an NNTP channel
**  or spawn an nnrpd to handle it.
*/
STATIC FUNCTYPE
RCreader(cp)
    CHANNEL		*cp;
{
    int			fd;
    struct sockaddr_in	remote;
    int			size;
    register int	i;
    register REMOTEHOST	*rp;
    CHANNEL		*new;
    char		*name;

    if (cp != RCchan) {
	syslog(L_ERROR, "%s internal RCreader wrong channel 0x%x not 0x%x",
	    LogName, cp, RCchan);
	return;
    }

    /* Get the connection. */
    size = sizeof remote;
    if ((fd = accept(cp->fd, (struct sockaddr *)&remote, &size)) < 0) {
	syslog(L_ERROR, "%s cant accept RCreader %m", LogName);
	return;
    }

    /* See if it's one of our servers. */
    for (name = NULL, rp = RCpeerlist, i = RCnpeerlist; --i >= 0; rp++)
	/* SUPPRESS 112 *//* Retrieving long where char is stored */
	if (rp->Address.s_addr == remote.sin_addr.s_addr) {
	    name = rp->Name;
	    break;
	}

    /* If not a server, and not allowing anyone, hand him off. */
    if (i >= 0)
	new = NCcreate(fd, rp->Password[0] != '\0');
    else if (AnyIncoming)
	new = NCcreate(fd, FALSE);
    else {
	RChandoff(fd, HOnntpd);
	if (close(fd) < 0)
	    syslog(L_ERROR, "%s cant close %d %m", LogName, fd);
	return;
    }

    /* SUPPRESS 112 *//* Retrieving long where char is stored */
    new->Address.s_addr = remote.sin_addr.s_addr;
    syslog(L_NOTICE, "%s connected %d",
	name ? name : inet_ntoa(new->Address), new->fd);
}


/*
**  Write-done function.  Shouldn't happen.
*/
STATIC FUNCTYPE
RCwritedone()
{
    syslog(L_ERROR, "%s internal RCwritedone", LogName);
}


/*
**  Read in the file listing the hosts we take news from, and fill in the
**  global list of their Internet addresses.  On modern systems a host can
**  have multiple addresses, so we take care to add all of them to the list.
**  We can distinguish between the two because h_addr is a #define for the
**  first element of the address list in modern systems, while it's a field
**  name in old ones.
*/
STATIC void
RCreadfile(list, count, filename)
    REMOTEHOST		**list;
    int			*count;
    char		*filename;
{
    static char		NOPASS[] = "";
    char		buff[SMBUF];
    register FILE	*F;
    register char	*p;
    struct hostent	*hp;
    register int	i;
    register REMOTEHOST	*rp;
    register int	j;
    char		*pass;
    char		*pats;
    int			errors;

    /* Free anything that might have been there. */
    if (*list) {
	for (rp = *list, i = *count; --i >= 0; rp++) {
	    DISPOSE(rp->Name);
	    DISPOSE(rp->Password);
	    if (rp->Patterns)
		DISPOSE(rp->Patterns);
	}
	DISPOSE(*list);
	*list = NULL;
	*count = 0;
    }

    /* Open the server file, count the lines. */
    if ((F = fopen(filename, "r")) == NULL) {
	syslog(L_FATAL, "%s cant read %s %m", LogName, filename);
	exit(1);
    }
    for (i = 1; fgets(buff, sizeof buff, F) != NULL; )
	if (buff[0] != COMMENT_CHAR && buff[0] != '\n')
	    i++;
    *count = i;
    rp = *list = NEW(REMOTEHOST, *count);
#if	!defined(DO_HAVE_UNIX_DOMAIN)
    rp->Address.s_addr = inet_addr(LOOPBACK_HOST);
    rp->Name = COPY("localhost");
    rp->Password = COPY(NOPASS);
    rp->Patterns = NULL;
    rp++;
#endif	/* !defined(DO_HAVE_UNIX_DOMAIN) */

    /* Now read the file to add all the hosts. */
    (void)fseek(F, (OFFSET_T)0, SEEK_SET);
    for (errors = 0; fgets(buff, sizeof buff, F) != NULL; ) {
	/* Ignore blank and comment lines. */
	if ((p = strchr(buff, '\n')) != NULL)
	    *p = '\0';
	if ((p = strchr(buff, COMMENT_CHAR)) != NULL)
	    *p = '\0';
	if (buff[0] == '\0')
	    continue;
	if ((pass = strchr(buff, ':')) != NULL) {
	    *pass++ = '\0';
	    if ((pats = strchr(pass, ':')) != NULL)
		*pats++ = '\0';
	    else
		pats = NULL;
	}
	else {
	    pass = NOPASS;
	    pats = NULL;
	}

	/* Was host specified as as dotted quad? */
	if ((rp->Address.s_addr = inet_addr(buff)) != (unsigned long)-1) {
	    rp->Name = COPY(buff);
	    rp->Password = COPY(pass);
	    rp->Patterns = pats ? CommaSplit(COPY(pats)) : NULL;
	    rp++;
	    continue;
	}

	/* Host specified as a text name? */
	if ((hp = gethostbyname(buff)) == NULL) {
	    syslog(L_ERROR, "%s cant gethostbyname %s %m", LogName, buff);
	    errors++;
	    continue;
	}

#if	defined(h_addr)
	/* Count the addresses and see if we have to grow the list. */
	for (i = 0; hp->h_addr_list[i]; i++)
	    continue;
	if (i == 0) {
	    syslog(L_ERROR, "%s no_address %s %m", LogName, buff);
	    errors++;
	    continue;
	}
	if (i == 1) {
	    /* Just one, no need to grow. */
	    COPYADDR(&rp->Address, hp->h_addr_list[0]);
	    rp->Name = COPY(hp->h_name);
	    rp->Password = COPY(pass);
	    rp->Patterns = pats ? CommaSplit(COPY(pats)) : NULL;
	    rp++;
	    continue;
	}

	/* Note the relative position, grow the array, and restore it. */
	j = rp - *list;
	*count += i - 1;
	RENEW(*list, REMOTEHOST, *count);
	rp = *list + j;

	/* Add all the hosts. */
	for (i = 0; hp->h_addr_list[i]; i++) {
	    COPYADDR(&rp->Address, hp->h_addr_list[i]);
	    rp->Name = COPY(hp->h_name);
	    rp->Password = COPY(pass);
	    rp->Patterns = pats ? CommaSplit(COPY(pats)) : NULL;
	    rp++;
	}
#else
	/* Old-style, single address, just add it. */
	COPYADDR(&rp->Address, hp->h_addr);
	rp->Name = COPY(hp->h_name);
	rp->Password = COPY(pass);
	rp->Patterns = pats ? CommaSplit(COPY(pats)) : NULL;
	rp++;
#endif	/* defined(h_addr) */
    }
    *count = rp - *list;

    if (fclose(F) == EOF)
	syslog(L_ERROR, "%s cant fclose %s %m", LogName, filename);

    if (errors)
	syslog(L_ERROR, "%s bad_hosts %d in %s", LogName, errors, filename);
}


void
RCreadlist()
{
    static char	INNDHOSTS[] = _PATH_INNDHOSTS;
    char	name[sizeof _PATH_INNDHOSTS + sizeof ".nolimit"];
    struct stat	Sb;

    RCreadfile(&RCpeerlist, &RCnpeerlist, INNDHOSTS);
    FileGlue(name, INNDHOSTS, '.', "nolimit");
    if (stat(name, &Sb) >= 0)
	RCreadfile(&RCnolimitlist, &RCnnolimitlist, name);
}



/*
**  Find the name of a remote host we've connected to.
*/
char *
RChostname(cp)
    register CHANNEL	*cp;
{
    static char		buff[SMBUF];
    register REMOTEHOST	*rp;
    register int	i;

    for (rp = RCpeerlist, i = RCnpeerlist; --i >= 0; rp++)
	/* SUPPRESS 112 *//* Retrieving long where char is stored */
	if (cp->Address.s_addr == rp->Address.s_addr)
	    return rp->Name;
    (void)strcpy(buff, inet_ntoa(cp->Address));
    return buff;
}


/*
**  Is the remote site allowed to post to this group?
*/
BOOL
RCcanpost(cp, group)
    register CHANNEL	*cp;
    register char	*group;
{
    register REMOTEHOST	*rp;
    register BOOL	match;
    register BOOL	subvalue;
    register char	**argv;
    register char	*pat;
    register int	i;

    for (rp = RCpeerlist, i = RCnpeerlist; --i >= 0; rp++) {
	/* SUPPRESS 112 *//* Retrieving long where char is stored */
	if (cp->Address.s_addr != rp->Address.s_addr)
	    continue;
	if (rp->Patterns == NULL)
	    break;
	for (match = TRUE, argv = rp->Patterns; (pat = *argv++) != NULL; ) {
	    subvalue = *pat != SUB_NEGATE;
	    if (!subvalue)
		pat++;
	    if (wildmat(group, pat))
		match = subvalue;
	}
	return match;
    }
    return TRUE;
}


/*
**  Create the channel.
*/
void
RCsetup(i, master)
    register int	i;
    char		*master;
{
    struct sockaddr_in	server;
    struct hostent	*hp;
    INADDR		a;
    char		buff[SMBUF];
#if	defined(SO_REUSEADDR)
    int			on;
#endif	/* defined(SO_REUSEADDR) */

    if (i < 0) {
	/* Create a socket and name it. */
	if ((i = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
	    syslog(L_FATAL, "%s cant socket RCreader %m", LogName);
	    exit(1);
	}
#if	defined(SO_REUSEADDR)
	on = 1;
	if (setsockopt(i, SOL_SOCKET, SO_REUSEADDR,
		(caddr_t)&on, sizeof on) < 0)
	    syslog(L_ERROR, "%s cant setsockopt RCreader %m", LogName);
#endif	/* defined(SO_REUSEADDR) */
	(void)memset((POINTER)&server, 0, sizeof server);
	server.sin_port = htons(NNTP_PORT);
	server.sin_family = AF_INET;
	server.sin_addr.s_addr = htonl(INADDR_ANY);
	if (bind(i, (struct sockaddr *)&server, sizeof server) < 0) {
	    syslog(L_FATAL, "%s cant bind RCreader %m", LogName);
	    exit(1);
	}
    }

    /* Set it up to wait for connections. */
    if (listen(i, MAXLISTEN) < 0) {
	syslog(L_FATAL, "%s cant listen RCreader %m", LogName);
	exit(1);
    }
    RCchan = CHANcreate(i, CTremconn, CSwaiting, RCreader, RCwritedone);
    syslog(L_NOTICE, "%s rcsetup %s", LogName, CHANname(RCchan));
    RCHANadd(RCchan);

    /* Get the list of hosts we handle. */
    RCreadlist();

    /* If we have a master, get all his addresses. */
    AmSlave = master != NULL;
    if (AmSlave) {
	/* Dotted quad? */
	if ((a.s_addr = inet_addr(master)) != (unsigned long)-1) {
	    RCnmaster = 1;
	    RCmaster = NEW(INADDR, 1);
	    COPYADDR(&RCmaster[0], &a);
	}
	else {
	    /* Must be a text name. */
	    if ((hp = gethostbyname(master)) == NULL) {
		syslog(L_FATAL, "%s cant gethostbyname %s %m", LogName, master);
		exit(1);
	    }
#if	defined(h_addr)
	    /* Count the addresses. */
	    for (i = 0; hp->h_addr_list[i]; i++)
		continue;
	    if (i == 0) {
		syslog(L_FATAL, "%s no_address %s %m", LogName, master);
		exit(1);
	    }
	    RCnmaster = i;
	    RCmaster = NEW(INADDR, RCnmaster);
	    for (i = 0; hp->h_addr_list[i]; i++)
		COPYADDR(&RCmaster[i], hp->h_addr_list[i]);
#else
	    RCnmaster = 1;
	    RCmaster = NEW(INADDR, 1)
	    COPYADDR(&RCmaster[0], &a);
#endif	/* defined(h_addr) */
	}

	/* Set flag for nnrp. */
	(void)sprintf(buff, "-S%s", master);
	RCslaveflag = COPY(buff);
    }
}


/*
**  Cleanly shut down the channel.
*/
void
RCclose()
{
    register REMOTEHOST	*rp;
    register int	i;

    CHANclose(RCchan, CHANname(RCchan));
    RCchan = NULL;
    if (RCpeerlist) {
	for (rp = RCpeerlist, i = RCnpeerlist; --i >= 0; rp++) {
	    DISPOSE(rp->Name);
	    DISPOSE(rp->Password);
	    if (rp->Patterns)
		DISPOSE(rp->Patterns);
	}
	DISPOSE(RCpeerlist);
	RCpeerlist = NULL;
	RCnpeerlist = 0;
    }

    if (RCmaster) {
	DISPOSE(RCmaster);
	RCmaster = NULL;
	RCnmaster = 0;
    }
}
