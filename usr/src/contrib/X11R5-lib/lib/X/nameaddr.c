/* $XConsortium: nameaddr.c,v 1.5 91/09/12 13:41:04 rws Exp $ */
/*	nameaddr.c - included by Xstreams.c			*/
/*	Used for System V Release 4.0 networking code		*/

/* Copyright (c) 1990, 1991 UNIX System Laboratories, Inc. 
 * Copyright 1991 Massachusetts Institute of Technology
 * Copyright 1988, 1989 AT&T, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that copyright notice and this permission
 * notice appear in supporting documentation, and that the name of
 * AT&T, USL, or MIT not be used in advertising or publicity
 * pertaining to distribution of the software without specific,
 * written prior permission.  AT&T, USL, and MIT make no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * AT&T, USL, AND MIT DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN
 * NO EVENT SHALL AT&T, USL, OR MIT BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
 * IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#ifdef XSTREAMS_COMPILE /* magic symbol to avoid lint problems */

static void   *handlep = NULL;
static int    family = 0;
static char   tcphalf[4];
static int    *tcpfamilyp = NULL;
static char   **tcpremoteaddrp = NULL;
static int    *tcpremoteaddrlenp = NULL;

static int SetupNetworkStream();
static int BindAndListen();
static int BindAndConnect();
static int addentry();

/* Routines for handling TLI streams */
static int
InitializeNetPath()
{
	if(handlep == NULL && (handlep = setnetpath()) == NULL)
	{
		nc_perror("Cannot set network selection path\n");
		return(-1);
	}
	return(1);

}

_XsSetupTliStream(display, dummy)
    char *display;
    void *dummy;
{
	int	i, n;
	int	fd, type;
	struct	utsname  machine;
	struct listenCall *tmp;
	int	nameserver();
	static int firstime=1;

	PRMSG("Calling SetupTliStream()\n",0,0);

	if(InitializeNetPath() < 0)
		return(-1);

	dispno = display;

	if(uname(&machine) < 0){
		t_error("Cannot get nodename");
		return(-2);
		}
		
	bind_req.addr.buf = req_buf;

/* pcc */
	if (firstime)	{
		Network._nnets = X_TLI_STREAM;
		firstime = 0;
	}

/*	type = Network._nnets;	*/

	bind_ret.addr.buf = ret_buf;
	call.addr.buf	  = call_buf;
	bind_req.addr.maxlen = MAXLEN;
	bind_ret.addr.maxlen = MAXLEN;
	call.addr.maxlen     = MAXLEN;

	fd = SetupNetworkStream(
			machine.nodename, 
			atoi(display), 
			BindAndListen, 
			&type, 
			handlep
			);
	
	if( fd < 0)
	{
         		PRMSG("SetupNetworkStream failed\n",0,0);
                        return(-1);
	}

	_XsTypeOfStream[fd] = type;

/*
 * set up call save list for general network listen service
 */
	for (i = 0; i < LISTEN_QUE_SIZE; ++i) 
	{
	    if((tmp = (struct listenCall *) malloc(sizeof(struct listenCall))) == NULL)
	    {
			PRMSG( "malloc failed\n",0,0);
			exit(1);
	    }
	    if((tmp->CurrentCall = (struct t_call *) t_alloc(fd,T_CALL,T_ALL)) == NULL)
            {
			PRMSG( "t_alloc failed\n",0,0);
			exit(1);
	    }
	    Que(&Network.FreeList[type], tmp, CLEAR);
	}

	if(Network._npeers > 0 && Network._peer == NULL)
	{
		register	i;
		register	char	*ptr;
		int		n;

		n =  (Network._npeers + 1) * 
			(sizeof(int) + sizeof(char *) + (1 + UNAME_LENGTH));

		PRMSG("Allocating %d chars for %d peeers names", 
							n, Network._npeers);

		if((ptr = malloc(n)) == NULL){
			fprintf(stderr,"Cannot malloc space for peers names\n");
			exit(1);
		}

		
		Network._peerlen = (int *) ptr;
		ptr += Network._npeers * sizeof(int);
		Network._peer = (char **) ptr; 
		ptr += Network._npeers * sizeof(char *);
		for(i= 0; i< Network._npeers; i++)
		{
			Network._peerlen[i] = 0;
			Network._peer[i]    = ptr;
#ifdef DEBUG
/*
			fprintf(stderr, "peer[%d] is %u; peerlen[%d] is %u\n",
				i, Network._peer[i], i, &Network._peerlen[i]);
*/
#endif
			ptr += (1 + UNAME_LENGTH);
		}
	}
	PRMSG("SetupTliStream () (success) fd = %d\n", fd,0);
	return(fd);
}

int
_XsCallTliServer(host, idisplay)
    char *host;
    int	idisplay;
{
	int	fd, type;
	int	nameserver ();
	static  char	firsttime = 1;

	PRMSG("Calling CallTliServer()\n",0,0);

	if(firsttime == 1)
	{
		firsttime = 0;
		SetupNetworkInfo();
		Network._nnets = X_TLI_STREAM;
	}
	
	sprintf(_dispno, "%d", idisplay);
	dispno = _dispno;


	if((fd = SetupNetworkStream(
			host, 
			idisplay, 
			BindAndConnect, 
			&type, 
			handlep)) < 0)
	{
			PRMSG("SetupNetworkStream failed\n",0,0);
			return(-1);
	}

	if(ioctl(fd, I_POP, "timod") < 0)
	{
	    PRMSG("failed to pop timod\n",0,0);
	}
	if(ioctl(fd, I_PUSH, "tirdwr") < 0)
	{
         	t_close(fd);
		return(-1);
	}

	PRMSG("A Connection has been established to %s ... \n", host,0);
	_XsTypeOfStream[fd] = type;
        if (_XsInputBuffer[fd].DataBuffer == NULL)
            if ((_XsInputBuffer[fd].DataBuffer = (char *) malloc(BUFFERSIZE)) == NULL)
               {
	       errno = ENOMEM;
               perror("Client can't connect to remote server");
               return (-1);
               }
	_XsInputBuffer[fd].LastBytePtr = 0;
	_XsInputBuffer[fd].FirstBytePtr = 0;
	PRMSG("CallTliServer() returns success\n",0,0);

	return(fd);
}


int _XMakeStreamsConnection (name, idisplay, retries,
			     familyp, serveraddrlenp, serveraddrp)
    char	*name;
    int	idisplay;
    int	retries;
    int	*familyp;		/* return */
    int	*serveraddrlenp;	/* return */
    char **serveraddrp;		/* return */
{
	char	netype[128], sysname[128], nodname[128];
	char	*procname = "Xlib/_XMakeStreamsConnection";
	struct	utsname	 machine;
	int	fd; 

	PRMSG("GetConnectionType(%s)\n", name, 0);

        if(uname(&machine) < 0){
		t_error("Cannot get nodename");
		return(-1);
		}
	if(
		name == NULL || 
		strcmp(name, "") == 0 ||
		strcmp(name, "unix") == 0 ||
		strcmp(name, "local") == 0 ||
		strcmp(name, machine.nodename) == 0 
	  )
	{
	    fd = ((*_XsStream[X_NAMED_STREAM].CallTheListener)
		  ("unix", idisplay, "local"));

	    if (fd >= 0) {
		*familyp = FamilyLocal;
		*serveraddrlenp = strlen (machine.nodename);
		*serveraddrp = (char *) Xmalloc ((*serveraddrlenp) + 1);
		if (!*serveraddrp) {
		    *serveraddrlenp = 0;
		} else {
		    strcpy (*serveraddrp, machine.nodename);
		}
	    }
	    return fd;
	}

	if((handlep = setnetpath()) == NULL)
	{
         	nc_perror("Cannot set network selection path\n");
		return(-1);
	}
	/* For backward compatibility, we have to pass authorization
	   data in global variables.  Ugh. */
	tcpfamilyp = familyp;
	tcpremoteaddrp = serveraddrp;
	tcpremoteaddrlenp = serveraddrlenp;

	fd = (*_XsStream[X_TLI_STREAM].CallTheListener)(name, idisplay);
	return(fd);
}


static int
SetupNetworkStream(host, dispno, action, typtr, handlep)
    char *host;
    int	dispno;
    int	(*action)();
    int	*typtr;
    void *handlep;
{
	int	i;
	char	service[MAX_AUTO_BUF_LEN];
	int	fd, type;
	struct nd_hostserv  nd_hostserv; 
	struct netconfig   *netconfigp = NULL;
	struct nd_addrlist *nd_addrlistp = NULL;
	struct netbuf	   *netbufp = NULL;
 

#ifdef DEBUG
fprintf(stderr, "Calling SetupNetworkStream(%s, %d)\n", host, dispno);
#endif

        sprintf(service , "xserver%d", dispno);
	nd_hostserv.h_host = host;
	nd_hostserv.h_serv = service;
#ifdef DEBUG
fprintf(stderr, "Trying to get the binding address for service %s on %s\n", 
					service, host);
#endif
	while((netconfigp = getnetpath(handlep)) != NULL)
	{
#ifdef DEBUG
	  fprintf(stderr, "Trying to bind using %s\n", netconfigp->nc_device);
#endif
	  if(netdir_getbyname(netconfigp, &nd_hostserv, &nd_addrlistp) == 0)
	  {

#ifdef DEBUG
	  fprintf(stderr, "There are %d ways\n", nd_addrlistp->n_cnt);
#endif
            netbufp = nd_addrlistp->n_addrs;
            for(i=0; i< nd_addrlistp->n_cnt; i++)
            {
		
#ifdef DEBUG
		fprintf(stderr, "Address: len %d maxlen %d \n", 
			netbufp->len, netbufp->maxlen);
#endif
		if( strcmp(netconfigp->nc_netid, "starlan") == 0 )
		{
			register char *from, *to;
			int	i, len;

			from = to = netbufp->buf;
			len = 0;
			for(i=0; i< netbufp->len; i++, from++)
				if(*from != '.')
				{
					*to++ = *from;	
					len++;
				}
			*to = '\0';
			netbufp->len = len;
		}
		
#ifdef DEBUG
		fprintf(stderr, "Address: maxlen %d buf ", netbufp->maxlen);
		dumpBytes(netbufp->len, netbufp->buf);
#endif
		if((fd = (*action)(netconfigp->nc_device, netbufp)) < 0)
		{
			netbufp++;
			continue;
		}
		if(
                   strcmp(netconfigp->nc_protofmly, "inet") == 0 &&
                   strcmp(netconfigp->nc_proto    , "tcp") == 0
          	  )
		{
			memcpy(tcphalf, netbufp->buf, 4);
			if (tcpfamilyp != NULL) {
			    *tcpfamilyp = FamilyInternet;
			    *tcpremoteaddrlenp = 4;
			    *tcpremoteaddrp = Xmalloc(*tcpremoteaddrlenp);
			    /* This is a kludge.  What is the right way to get
			       this info out? */
			    memcpy(*tcpremoteaddrp, netbufp->buf+4,
				   *tcpremoteaddrlenp);
#ifdef DEBUG
			    fprintf(stderr, "tcp remote addr = %0x\n",
				    *(long *)*tcpremoteaddrp);
#endif
			}
		}
		type = 0;
		for(i=X_TLI_STREAM; i< Network._nnets; i++)
		 if(strcmp(Network._net[i]->nc_netid, netconfigp->nc_netid) == 0)
		 {
			type = i;
			break;
		 }
		if(type == 0)
		{
			Network._net[Network._nnets] = netconfigp;
			type = Network._nnets++;
		}
		*typtr = type;
	  	/* free(netconfigp) the right way */
		(void) netdir_free((char *)nd_addrlistp, ND_ADDRLIST);

		return(fd);
            }
	    /* free(nd_addrlistp) the right way */
	    (void) netdir_free((char *)nd_addrlistp, ND_ADDRLIST);
	  }
#ifdef DEBUG
	  else netdir_perror("netdir_getbyname() failed");
#endif
	}
	return(-1);
}


static int
BindAndListen(clonedev, netbufp)
    char	*clonedev;
    struct   netbuf *netbufp;
{
	int	fd;
	struct  t_bind bindbuf;


	bindbuf.addr.buf = netbufp->buf;
        bindbuf.addr.len = netbufp->len;
        bindbuf.addr.maxlen = netbufp->maxlen;

			
	if ((fd = t_open(clonedev,  O_RDWR, NULL)) < 0)
	{
		fprintf(stderr, "Cannot open %s\n", clonedev);
		return(-1);
	}
#ifdef DEBUG
	fprintf(stderr, "opening device %s\n", clonedev);
#endif

	bindbuf.qlen = 8;
	if(t_bind (fd, &bindbuf, NULL) < 0)
	{
		t_error("t_bind failed");
		close(fd);
		return(-1);
	}
	return(fd);
}

static int
BindAndConnect(clonedev, netbufp)
    char	*clonedev;
    struct   netbuf *netbufp;
{
	int	fd;
	struct  t_call callbuf;

	callbuf.addr.buf = netbufp->buf;
	callbuf.addr.len = netbufp->len;
	callbuf.addr.maxlen = netbufp->maxlen;
			
	callbuf.opt.buf = NULL;
	callbuf.opt.len = 0;
	callbuf.opt.maxlen = 0;
			
	callbuf.udata.buf = NULL;
	callbuf.udata.len = 0;
	callbuf.udata.maxlen = 0;

	if ((fd = t_open(clonedev,  O_RDWR, NULL)) < 0)
	{
		fprintf(stderr, "Cannot open %s\n", clonedev);
		return(-1);
	}
	
#ifdef DEBUG
	fprintf(stderr, "Connecting to <%s> through device %s\n", 
					callbuf.addr.buf, clonedev);
#endif
	if(t_bind(fd, NULL, NULL) < 0)	
	{
		t_error("t_bind failed");
		t_close(fd);
		return(-1);
	}
	if(t_connect(fd, &callbuf, NULL) < 0)	
	{
		t_error("t_connect failed");
		checkNewEvent(fd);
		t_close(fd);
		return(-1);
	}
	return(fd);
}



/*extern	char	*calloc(), *realloc();*/
extern  char    *program;
static int	network;
static int	nextentry;

static char *makePacket();
static char *staticalloc();
static char    *TheEnd;
static char    *inbuf;
static int     inlen;
static int     nhosts;
static int	nHosts;
static int     flags = 0;
static struct netconfig   *netconfigp = NULL; 

static int
CallTheNameServer(service, nettype, arg1, arg2, arg3)
    int service;
    struct netconfig   *nettype;
    char    **arg1, **arg2;
    int     *arg3;
{
	int	n,m, len;
	char	*ptr, *net;
	int	*iptr;

	flags = service;
	netconfigp = nettype;
	ptr = *arg1;

	iptr = (int *) ptr;
	inlen = iptr[0];

#ifdef DEBUG
fprintf(stderr,"inlen = %d\n", inlen);
#endif
	ptr += sizeof(int);
	nhosts = iptr[1];
#ifdef DEBUG
fprintf(stderr,"nhosts = %d\n", nhosts);
#endif

	inbuf = ptr + sizeof(int);
	TheEnd = &inbuf[inlen];
#ifdef DEBUG
	write(2, inbuf, inlen);
#endif
        nextentry = ((xHostEntry *) inbuf)->length;
        *arg2 = (char *) makePacket(&len);
	if(arg3 != NULL)
		*arg3 = nHosts;

#ifdef DEBUG
fprintf(stderr, "CallTheNameserver return %d\n", len);
#endif
	return(len);
}



static int	bufsize = 512;

static char	*getnextentry();

static struct nd_addrlist *
GetHostServiceByName(host, dispno)
    char *host;
    int	dispno;
{
    struct nd_hostserv  nd_hostserv;
    struct nd_addrlist *nd_addrlistp = NULL;
    struct netbuf	   *netbufp = NULL;
    char	service[MAX_AUTO_BUF_LEN];
    
    sprintf(service , "xserver%d", dispno);
    nd_hostserv.h_host = host;
    nd_hostserv.h_serv = service;
    
    if(netdir_getbyname(netconfigp, &nd_hostserv, &nd_addrlistp) == 0)
	return(nd_addrlistp);
    else	return(NULL); 
}

static int
ConvertName(pktptr, n, entry, len)
    char **pktptr, *entry;
    int	n, len;
{
    struct hostent *hp;
    unsigned long	address;
    int	port;
    char    *ptr;
    int	rndlen;
    struct nd_addrlist *nd_addrlistp = NULL;
    struct netbuf   *netbufp;
    char   *addr;
#ifdef DEBUG
    fprintf(stderr, "in ConvertName %s\n", entry);
#endif
    if((nd_addrlistp = GetHostServiceByName(entry, atoi(dispno))) == NULL)
	return(n);
    netbufp = nd_addrlistp->n_addrs;       /* the netbufs */
    /*
      xhost needs only the last four bytes of the address
      over TCP/IP. 
      */
    if(	
       strcmp(netconfigp->nc_protofmly, "inet") == 0 && 
       strcmp(netconfigp->nc_proto    , "tcp") == 0
       ){
	addr = &netbufp->buf[4];
	len = 4;
	family = 1;
    }
    else
    {
	family = 0;
	addr = netbufp->buf;
	len = netbufp->len;
    }
    rndlen = ((sizeof(xHostEntry) + len + 3) >> 2) << 2;
    
    if((*pktptr = staticalloc(*pktptr, n+rndlen)) == NULL)
    {
	(void) netdir_free((char *)nd_addrlistp, ND_ADDRLIST);
	return(-1);
    }
    
    ptr = &(*pktptr)[n];
    ((xHostEntry *)ptr)->family = family;
    ((xHostEntry *)ptr)->length = len;
    ptr += sizeof(xHostEntry);
    
    memcpy(ptr, addr, len);
    netdir_free((char *)nd_addrlistp, ND_ADDRLIST);
    
#ifdef DEBUG
    ptr[len] = '\0';
    fprintf(stderr, "creating address for host %s address<%d>\n", entry, ptr);
#endif
    
    return(n+rndlen);
}

static struct nd_hostservlist *
GetHostServiceByAddr(addr, len)
    char *addr;
    int	len;
{
    struct nd_hostservlist *nd_hostservlist;
    struct netbuf	   netbuf;
    
    netbuf.buf = addr;
    netbuf.len = len;
    netbuf.maxlen = len;
    
    if(netdir_getbyaddr(netconfigp, &nd_hostservlist, &netbuf) == 0)
	return(nd_hostservlist);
    else	return(NULL);
}


static int
ConvertCallToName(pktptr, n, entry, len)
    char **pktptr, *entry;
    int	n, len;
{
    int	l, rl;
    char	*ptr;
    struct nd_hostservlist *nd_hostservlist;
    
    if((nd_hostservlist = GetHostServiceByAddr(entry, len)) == NULL)
	return(n);
    
    l = strlen(nd_hostservlist->h_hostservs->h_host);
    
    rl = ((sizeof(xHostEntry) + l + 3) >> 2) << 2;
    
    if((*pktptr = staticalloc(*pktptr, n+rl)) == NULL)
    {
	(void) netdir_free((char *)nd_hostservlist, ND_HOSTSERVLIST);
	return(-1);
    }
    
    ptr = &(*pktptr)[n];
    ((xHostEntry *)ptr)->family = 0;
    ((xHostEntry *)ptr)->length = l;
    
    ptr += sizeof(xHostEntry);
    
    sprintf(ptr, nd_hostservlist->h_hostservs->h_host);
    (void) netdir_free((char *)nd_hostservlist, ND_HOSTSERVLIST);
    
#ifdef DEBUG
    fprintf(stderr, "getting the name for host %s\n", ptr);
#endif
    
    return(rl+n);
}

static int
ConvertAddress(pktptr, n, entry, len)
    char **pktptr, *entry;
    int	n, len;
{
    register i;
    char	*ptr;
    int     l, rl; 
    struct nd_hostservlist *nd_hostservlist;
    char	*name;
    char	tcpaddr[8], *addr;
    char	addrbuf[MAX_AUTO_BUF_LEN];
    
#ifdef DEBUG
    entry[len] = '\0';
    fprintf(stderr, "Converting address %s in %s format\n",
	    entry, netconfigp->nc_netid);
#endif
    if(
       strcmp(netconfigp->nc_protofmly, "inet") == 0 &&
       strcmp(netconfigp->nc_proto    , "tcp") == 0
       ){
	addr = tcpaddr;
	memcpy(tcpaddr, tcphalf, 4);
	memcpy(&tcpaddr[4], entry, 4);
	len = 8;
#ifdef DEBUG
	fprintf(stderr, "port %d, family %d\n",
		*(short *) &tcpaddr[2], *(short *) tcpaddr);
#endif
    }
    else
    {
	addr = entry;
    }
    
    if((nd_hostservlist = GetHostServiceByAddr(addr, len)) == NULL)
    {
	int	i;
	
	for(i=0; i< len; i++)
	    if(entry[i] < ' ' || entry[i] > 0177)
		break;
	if(i < len)
	{
	    sprintf(addrbuf, "%d", *(int *) entry);
	    name = addrbuf;
	    len = strlen(name);
	}
	else	name = entry;
	entry[len] = '\0';
	l = len + 1;
    }
    else
    {
	name = nd_hostservlist->h_hostservs->h_host;
	l = strlen(name) +1;
    }
    rl = ((sizeof(xHostEntry) + l + 3) >> 2) << 2;
    
    if((*pktptr = staticalloc(*pktptr, n+rl)) == NULL)
    {
	if(nd_hostservlist != NULL)
	    (void) netdir_free((char *)nd_hostservlist, ND_HOSTSERVLIST);
	return(-1);
    }
    
    ptr = &(*pktptr)[n];
    ((xHostEntry *)ptr)->family = 0;
    ((xHostEntry *)ptr)->length = l;
    ptr += sizeof(xHostEntry);
    
    memcpy(ptr, name, l);	
    if(nd_hostservlist != NULL)
	(void) netdir_free((char *)nd_hostservlist, ND_HOSTSERVLIST);
    
#ifdef DEBUG
    fprintf(stderr, "getting the name for host %s\n", name);
#endif
    
    return(n+rl);
}

static char *
getnextentry(plen)
    int	*plen;
{
    char	*ptr;
    int	n = nextentry;
    
#ifdef DEBUG
    fprintf(stderr,"In getnextentry()\n");
#endif
    if(inbuf >= TheEnd)
    {
#ifdef DEBUG
	fprintf(stderr,"In getnextentry() end of buffer\n");
#endif
	*plen = -1;
	return(NULL);	
    }
    
    *plen = nextentry;
    family = ((xHostEntry *) inbuf)->family;
    ptr = inbuf + sizeof(xHostEntry);
    inbuf += ((sizeof(xHostEntry) + *plen + 3) >> 2) << 2;
    nextentry = ((xHostEntry *) inbuf)->length;
    ptr[*plen] = '\0';
    return(ptr);
}

static char *
makePacket(plen)
    int *plen;
{
    char *pktptr = NULL, *ptr;
    int	len;
    int	n = 0, m;
    
    
#ifdef DEBUG
    fprintf(stderr,"In makePacket()\n");
#endif
    
    for(nHosts = 0; nHosts < nhosts;)
    {
	ptr = getnextentry(&len);
	if(len < 0)
	    break;
	if(len == 0 || ptr == NULL)
	    continue;	
	m = addentry(&pktptr, n, ptr, len);
	if(m > n){
	    nHosts++;
	    n = m;
	}
    }
#ifdef DEBUG
    fprintf(stderr, "packet size is %d\n", n);
#endif
    
    *plen = n;
    
    return(pktptr);
}

static int
addentry(pktptr, n, entry, len)
    char **pktptr, *entry;
    int	n, len;
{
    
#ifdef DEBUG
    fprintf(stderr, "in addEntry %s\n", entry);
#endif
    
    switch(flags)
    {
    case	ConvertNameToNetAddr:
	return(ConvertName(pktptr, n, entry, len));
    case	ConvertNetAddrToName:
	return(ConvertAddress(pktptr, n, entry, len));
    case	ConvertTliCallToName:
	return(ConvertCallToName(pktptr, n, entry, len));
    }
    return(-1);
}

static char *
staticalloc(ptr, size)
    char *ptr;
    int	size;
{
    
	if(ptr == NULL)
	{
		if(bufsize < size)
			bufsize = size;
		ptr = malloc(bufsize);
	}
	if(bufsize < size)
	{
		bufsize = size + 512;
		ptr = realloc(ptr, bufsize);
	}
	return(ptr);
}

static int
OpenVirtualCircuit(lfd)
    int     lfd;
{
	char	*clonedev;
	int	fd;

       	clonedev = Network._net[_XsTypeOfStream[lfd]]->nc_device;

	if ((fd = t_open(clonedev,  O_RDWR, NULL)) < 0)
	{
		fprintf(stderr, "Cannot open %s\n", clonedev);
		return(-1);
	}
	
	if(t_bind(fd, NULL, NULL) < 0)	
	{
		t_error("t_bind failed");
		t_close(fd);
		return(-1);
	}
	return(fd);
}

#endif /* XSTREAMS_COMPILE */
