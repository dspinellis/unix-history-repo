/* $XConsortium: nameserver.c,v 1.3 91/07/23 11:50:04 rws Exp $ */
/*	nameserver.c - included by Xstreams.c			*/
/*	Used for System V Release 3.2 networking code ONLY	*/

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

static int svr4plus = 0;
static char clonedev[MAX_AUTO_BUF_LEN];

static int OpenAndBind();
static int Read();

/* Routines for handling TLI streams */

_XsSetupTliStream(display, stype)
    char *display;
    char *stype;
{
	int	i, n;
	int	fd, type;
	struct	utsname  machine;
	struct listenCall *tmp;
	int	nameserver();
	static int first=1;

	PRMSG("Calling SetupTliStream()\n",0,0);

        if(NameServer < 0 &&
		 (NameServer = OpenLocalServer(NAME_SERVER_NODE)) < 0)
	{
                        return(-1);
	}

	dispno = display;

	if(uname(&machine) < 0){
		t_error("Cannot get nodename");
		return(-2);
		}
		
	bind_req.addr.buf = req_buf;
	n = strlen(stype) +1;

/* pcc */
	if(first)	{
		Network._nnets = X_TLI_STREAM;
		first = 0;
	}
	type = Network._nnets++;

	if((Network._net[type] = malloc(n)) == NULL){
             		PRMSG( "malloc failed\n",0,0);
			return(-2);
			}

	bcopy(stype, Network._net[type], n);

	bind_ret.addr.buf = ret_buf;
	call.addr.buf	  = call_buf;
	bind_req.addr.maxlen = MAXLEN;
	bind_ret.addr.maxlen = MAXLEN;
	call.addr.maxlen     = MAXLEN;

	fd = OpenAndBind(machine.nodename, atoi(display), MAXCONNECTIONS, Network._net[type], type);
	
	if( fd < 0){
		PRMSG("Cannot OpenAndBind %s", machine.nodename,0);
		free(Network._net[type]);
		return(-2);
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
			return(-1);
	    }
	    if((tmp->CurrentCall = (struct t_call *) t_alloc(fd,T_CALL,T_ALL)) == NULL)
            {
			PRMSG( "t_alloc failed\n",0,0);
			return(-1);
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
			fprintf(stderr, "peer[%d] is %u; peerlen[%d] is %u\n",
				i, Network._peer[i], i, &Network._peerlen[i]);
#endif
			ptr += (1 + UNAME_LENGTH);
		}
	}
	PRMSG("SetupTliStream () (success) fd = %d\n", fd,0);
	return(fd);
}
_XsCallTliServer(host, idisplay, nettype)
    char *host;
    int	idisplay;
    char *nettype;
{
	int	fd, i, t;
	PFV	savef;
	int	netlen, type;
	char	*retptr, *ptr;
	char	first = 1;
	char	netbuffer[MAX_AUTO_BUF_LEN];
	static	char	firstime = 1;

	PRMSG("Calling CallTliServer()\n",0,0);

        if(NameServer<0 && (NameServer = OpenLocalServer(NAME_SERVER_NODE)) < 0)
	{
			return(-1);
	}

	if(firstime)
	{
		SetupNetworkInfo();
		Network._nnets = X_TLI_STREAM;
		firstime = 0;
	}
	
	sprintf(_dispno, "%d", idisplay);
	dispno = _dispno;

	savef = signal (SIGALRM, dummy);

/* 
 * Give up after MAX_TRIES tries or for CONNECT_TIME seconds or an error
 * occurred which comes first.
*/

	retptr = NULL;

#define MAX_TRIES	3
#define CONNECT_TIME    10

	alarm (CONNECT_TIME);

	if(svr4plus)
	{
		int	naddrs, n, j;

		if(GetNetworkInfo (-1, nettype, ConvertNameToTliCall,
                      addheader(host, strlen(host)), &retptr, &naddrs) <= 0)
		{
                	fprintf(stderr, "Cannot create address for system %s \n", 
			host);
			t = -1;
			goto outofloop;
		}

                call.opt.len = 0;
                call.opt.maxlen = 0;
                call.opt.buf = NULL;

                call.udata.len = 0;
                call.udata.maxlen = 0;
                call.udata.buf = NULL;
#ifdef DEBUG
fprintf(stderr, "We got %d addresses for host %s\n", naddrs, host);
fprintf(stderr, "NETPATH sent to daemon is %s\n", nettype);
#endif
		ptr = retptr;
		for(i=0; i< naddrs; i++)
		{
                        call.addr.len = ((xHostEntry *) ptr)->length;
                        call.addr.maxlen = ((xHostEntry *) ptr)->length;
                        call.addr.buf = (ptr+sizeof(xHostEntry));

			call.addr.buf[call.addr.len] = '\0';
#ifdef DEBUG
			fprintf(stderr, "ADDRESS LENGTH IS %d\n", call.addr.len);
			fprintf(stderr, "Address returned is <%s>\n",call.addr.buf);
#endif
			ptr += (((sizeof(xHostEntry) + call.addr.len+3) >> 2) << 2);

			n =  ((xHostEntry *) ptr)->length;
			if(n > 0)
                                nettype = (ptr+sizeof(xHostEntry));
			else	nettype = NULL;
		
			ptr += (((sizeof(xHostEntry) + n+3) >> 2) << 2);
			n = ((xHostEntry *) ptr)->length;
			sprintf(clonedev, "%s", (ptr+sizeof(xHostEntry)));

			ptr += (((sizeof(xHostEntry) + n+3) >> 2) << 2);
#ifdef DEBUG
            		fprintf(stderr, "Clonedev is %s\n", clonedev);
            		fprintf(stderr, "netid is %s\n", nettype);
#endif
			
			fd = OpenAndBind(NULL, -1, 0, nettype, X_TLI_STREAM);
			if(fd  < 0)
			{
				PRMSG("Openandbind failed\n",0,0);
				continue;	
			}
			t = -1;

	                PRMSG("Connecting to %s ... \n", host, 0);
			if((t = t_connect(fd, &call, NULL)) < 0)
			{
       	          		if(t_errno == TLOOK)
       	                 	{
       	                  		checkNewEvent(fd);
       	                         	t_close(fd);
       	                 	}
       	                 	else
       	                 	{
       	                  		t_error("t_connect failed");
       	                         	t_close(fd);
       	                 	}
			} else break;

		}
	}
	else
	for(i=0; i < MAX_TRIES;i++)
	{
	
		if((fd = OpenAndBind(NULL, -1, 0, nettype, X_TLI_STREAM)) < 0)
		{
				PRMSG("Openandbind failed\n",0,0);
				break;	
		}
	 	if(first)
		{
			first = 0;

	   		if( GetNetworkInfo (-1, nettype, ConvertNameToTliCall,
				 addheader(host, strlen(host)),  &retptr, NULL) <= 0)
			{
				fprintf(stderr,
					"Cannot create address for system %s\n",host);
				t = -1;
				goto outofloop;
	   		}

			ptr = retptr;

	   		call.addr.len = ((xHostEntry *) ptr)->length;
	   		call.addr.maxlen = ((xHostEntry *) ptr)->length;
			call.addr.buf = (ptr+sizeof(xHostEntry));
		
			call.addr.buf[call.addr.len] = '\0';
#ifdef DEBUG
			fprintf(stderr, "ADDRESS LENGTH IS %d\n", call.addr.len);
			fprintf(stderr, "Address returned is <%s>\n",call.addr.buf);
#endif
			ptr += (((sizeof(xHostEntry) + call.addr.len+3) >> 2) << 2);

			call.opt.len = ((xHostEntry *) ptr)->length;
			call.opt.maxlen = ((xHostEntry *) ptr)->length;
			if(call.opt.len > 0)
				call.opt.buf = (ptr+sizeof(xHostEntry));
			else	call.opt.buf = NULL;
		
			ptr += (((sizeof(xHostEntry) + call.opt.len+3) >> 2) << 2);

			call.udata.len = ((xHostEntry *) ptr)->length;
			call.udata.maxlen = ((xHostEntry *) ptr)->length;
			if(call.udata.len > 0){
				call.udata.buf = (ptr+sizeof(xHostEntry));
#ifdef DEBUG
			fprintf(stderr, "ADDRESS LENGTH IS %d\n", call.udata.len);
                        fprintf(stderr, "Address returned is <%s>\n",call.udata.buf);
#endif
			}
			else	call.udata.buf = NULL;
#ifdef DEBUG
			fprintf(stderr, "addrlen %d optlen %d udatalen %d\n",
					call.addr.len,
					call.opt.len,
					call.udata.len);
#endif
		}

		t = -1;

		PRMSG("Connecting to %s ... \n", host, 0);
		if((t = t_connect(fd, &call, NULL)) < 0)
		{
			if(t_errno == TLOOK)
			{
				checkNewEvent(fd);
		        	t_close(fd);
				continue;
			}
			else
			{
				t_error("t_connect failed");
				t_close(fd);
				break;
			}
		} else break;
	}

outofloop:

#undef MAX_TRIES
#undef CONNECT_TIME

	alarm (0);
	signal (SIGALRM, savef);

	close(NameServer);
	NameServer = -1;
	netlen = strlen(nettype);
	if(netlen > 127)
	{
		netlen = 127;
		nettype[netlen] = '\0';
	}
	memcpy(netbuffer, nettype, netlen + 1);
	if(retptr != NULL)
		free(retptr);
	if(t < 0)
	{
		close(fd);
		return(-1);
	}

/*
	if (t_rcvconnect (fd, &call) < 0) {
		if(t_errno == TLOOK)
			checkNewEvent(fd);
		t_close(fd);
		t_error ("t_rcvconnect failed!");
		return(-1);
	}
*/

	if(ioctl(fd, I_POP, "timod") < 0)
	{
	PRMSG("failed to pop timod\n", 0, 0);
	}
	if(ioctl(fd, I_PUSH, "tirdwr") < 0)
	{
         	t_close(fd);
		return(-1);
	}

        if (_XsInputBuffer[fd].DataBuffer == NULL)
            if ((_XsInputBuffer[fd].DataBuffer = (char *) malloc(BUFFERSIZE)) == NULL)
               {
		        errno = ENOMEM;
               		perror("Client can't connect to remote server");
			close(fd);
               		return (-1);
               }
	_XsInputBuffer[fd].LastBytePtr = 0;
	_XsInputBuffer[fd].FirstBytePtr = 0;
	type = -1;
	for(i= X_TLI_STREAM; i< Network._nnets; i++)
	{
		if(strcmp(nettype, Network._net[i]) == 0)
		{
			type = i;
			break;
		}
	}
	if(type < 0)
	{
		Network._net[Network._nnets] = malloc(netlen+1);
		if(Network._net[Network._nnets] == NULL)
		{
		        errno = ENOMEM;
               		perror("Client can't connect to remote server");
			close(fd);
               		return (-1);
               }
	       memcpy(Network._net[Network._nnets], netbuffer, netlen+1);
	       type = Network._nnets++;
	}
	_XsTypeOfStream[fd] = type;
	PRMSG("A Connection has been established to %s ... \n", host,0);
	PRMSG("CallTliServer() returns success\n",0,0);

	return(fd);
}

static int
OpenAndBind(name, port, maxcon, nettype, type)
    char *name;
    int	port, maxcon, type;
    char *nettype;
{
	char	bind_buf[MAX_AUTO_BUF_LEN];
	int	i, fd;
	char	*retptr;
	
	if(!svr4plus)
		sprintf(clonedev, "/dev/%s", nettype);
	if(name != NULL)
		PRMSG("OpenAndBind name %s, clonedev is %s \n",
				 name, clonedev);

	/* point to the virtual circuit */

	if ((fd = t_open(clonedev,  O_RDWR, NULL)) < 0)
	{
		fprintf(stderr, "Cannot open %s\n", clonedev);
#ifdef DEBUG
		t_error("t_open 1 failed");
#endif
		return(-1);
	}

	_XsTypeOfStream[fd] = type;
	/* fill in the request call structure with necessary infomation */

	if(name != NULL)
	{
	   if(GetNetworkInfo (-1, nettype, ConvertNameToTliBind, 
		addheader(name, strlen(name)), &retptr, NULL)<0)
	   {
		PRMSG("Cannot create address for system %s \n", name, 0);
		return(-1);
	   }
	   bind_req.addr.buf = bind_buf;
	   bind_req.addr.len = ((xHostEntry *) retptr)->length;
	   bcopy (retptr+sizeof(xHostEntry), bind_buf, bind_req.addr.len);
	   free(retptr);
	   bind_buf[bind_req.addr.len] = '\0';
#ifdef DEBUG
	   fprintf(stderr, "ADDRESS LENGTH IS %d\n", bind_req.addr.len);
	   fprintf(stderr, "Address returned is <%s>\n", bind_buf);
#endif
	   bind_req.qlen = maxcon;

	/* bind the name to the transport endpoint.  This operation will */
	/* take some time if the name is not already in the local name  */
	/* table or if the name is not a group name   */
#ifdef SHARELIB
#define t_bind (*_libX_t_bind)
#endif
		i = t_bind (fd, &bind_req, NULL) ;
	}
	else 	i = t_bind (fd, NULL, NULL);

	if(i < 0)
	{
		t_error("t_bind failed");
		close(fd);
		return(-1);
	}

#ifdef SHARELIB
#undef t_bind
#endif
	if(name != NULL)
		PRMSG("OpenAndBind(%s, %d) (success)\n", name, maxcon);
	return(fd);
}

static char *erazeComment(line)
    char *line;
{
 	char	*ptr = line;

        while(*ptr <= ' ' && *ptr != '\0')
                        ptr++;
/*
 *	If you want to check the version do it here
 *if( strncmp(ptr, "#VERSION", 8) == 0)
 *			return(NULL);
 */
   	if(*ptr == '\0' || *ptr == '#'){
                        return(NULL);
                        }
	line = ptr;
	while(*ptr != '\0' && *ptr != '#')
                        ptr++;
	*ptr = '\0';
	return(line);
}


#define XNETDB "lib/Xconnections"

int _XMakeStreamsConnection (name, idisplay, retries,
			     familyp, serveraddrlenp, serveraddrp)
    char	*name;
    int	idisplay;
    int	retries;
    int	*familyp;		/* return */
    int	*serveraddrlenp;	/* return */
    char **serveraddrp;		/* return */
{
	struct	utsname	 machine;
	register	i;
	FILE	*file;
	char	*line, *ptr,buf[160];
	int	fd, nfound = 0, n;
	static  char	netype[MAX_AUTO_BUF_LEN], nodname[MAX_AUTO_BUF_LEN];
	char *home;
	char	sysname[128] ;
	char	*procname = "Xlib/_XMakeStreamsConnection";



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
	  )	{
		
	    /*
	     * auth information for local connection set above
	     */

	    fd = ((*_XsStream[X_LOCAL_STREAM].CallTheListener)
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

        if(machine.release[0] !=  '3')
	{
		char	*netpath;

		svr4plus = 1;
#ifdef DEBUG
		fprintf(stderr,
"sysname <%s>, nodename <%s>, release <%s>, version <%s>, machine <%s>\n",
			machine.sysname ,
			machine.nodename,
			machine.release,
			machine.version,
			machine.machine);
#endif

		if((netpath = (char *) getenv("NETPATH")) == NULL)
				netpath = "";
		return (*_XsStream[X_TLI_STREAM].CallTheListener)
				(name, idisplay, netpath);
	}

	file = fopen(home = GetXWINHome(XNETDB), "r");
	if(file == NULL){
		fprintf(stderr, "Cannot open %s\n", home);
		return(-1);
		}
	while((line = fgets(buf, 160, file)) != NULL)
	{
               	if((n = strlen(line)) > 1)
                              line[n-1] = '\0';
		if((ptr = erazeComment(line)) == NULL)
			continue;

		n = sscanf(ptr, "%s%s%s", sysname, nodname, netype);
		if(
			n >= 3 &&
			strcmp(name, sysname) == 0
		  ){
			nfound++;
		        fd = (*_XsStream[X_TLI_STREAM].CallTheListener)
						(nodname, idisplay, netype);
			if(fd >= 0)
			{
				fclose(file);
				return(fd);
			}
		   }
		else if(strcmp(sysname, "*") == 0 || strcmp(nodname, "*") == 0) 
		   {
			sprintf(nodname, name);
			fd = (*_XsStream[X_TLI_STREAM].CallTheListener)     
                                                (nodname, idisplay, netype);
			fclose(file);
                        return(fd);
		   }
	}
	fclose(file);
	if(!nfound)
	{
		fprintf(stderr, "There is no entry for %s in %s\n",
					name, home);
	}
	return(-1);
}


#ifdef DEBUG
dump(buf, len)
    char *buf;
    int	len;
{
 	int	i;
	if(buf != NULL)
		for(i=0; i< len; i++)
                        fprintf(stderr, "<%o> ", buf[i]);
	fprintf(stderr, "\n");
	fflush(stderr);
}

#endif


#define	NSECONDS	60

static int	_errflag = 0;

/* ARGSUSED */
static SIGNAL_T OnAlarm(i)
    int i;
{
	_errflag = 1;
}

static int
CallTheNameServer(service, nettype, arg1, arg2, arg3)
    char	*nettype, **arg1, **arg2;
    int	service, *arg3;
{
	int	m, n, ret;
	char   *ptr, *p;
	char	buf[MAX_AUTO_BUF_LEN];

PRMSG("In CallTheNameServer, \n", 0, 0);

	if(NameServer < 0)
		return(-1);
	if(_errflag)
	{
		int	flags;

		if((flags = fcntl(NameServer, F_GETFL)) == -1)
			flags = O_RDWR;
		fcntl (NameServer, F_SETFL, O_NDELAY);
		while(read (NameServer, buf, MAX_AUTO_BUF_LEN) > 0);
		if(errno != EAGAIN && errno != EINTR)
		{
			fprintf(stderr, "errno %d while reading the nameserver\n",
					errno);
			close(NameServer);
			NameServer = -1;
			return(-1);
		}
		fcntl (NameServer, F_SETFL, flags);
		_errflag = 0;	
	}
	
	ret = n = (* (int *) (*arg1)) + 2*sizeof(int);

        ptr = buf;
	m = HEADERSIZE + strlen(nettype)+ 1;
	m = (((m +3) >>2)<<2);

	*(int *) ptr = n+m;
	ptr += sizeof(int);
	*(int *) ptr = m;
	ptr += sizeof(int);
	*(int *) ptr = service;
	ptr += sizeof(int);
	*(int *) ptr = atoi(dispno);
	ptr += sizeof(int);
	*(int *) ptr = strlen(nettype);
	ptr += sizeof(int);
	sprintf(ptr, nettype);

  	p = malloc(m + n);
	if(p == NULL)
		return(-1);
	memcpy(p, buf, m);
	memcpy(p+m, *arg1, n);
	
	signal(SIGALRM, OnAlarm);
	alarm(NSECONDS); 

	if(write(NameServer, p, m+n) != m+n){
		fprintf(stderr, "write error\n");
		ret = -1;
		close(NameServer);
		NameServer = -1;
		}
	else if(Read(NameServer, buf, 2*sizeof(int)) == 0)
	{
		ret = -1;
#ifdef DEBUG
		fprintf(stderr, "Server fails to read header from nameserver\n");
#endif
	}
	else {
		ptr = buf;
		ret = *(int *) buf;
		ptr += sizeof(int);
		if(*(int *) ptr <= 0)
		{
#ifdef DEBUG
			fprintf(stderr, "No Of entries returned <= 0\n");
#endif
			ret = -1;
			goto theend;
		}
		if(arg3 != NULL){
			*arg3 = *(int *) ptr;
#ifdef DEBUG
			fprintf(stderr, "No Of Entries returned {%d}\n", *arg3);
#endif
			}
		ptr = *arg2 = (char *) malloc(ret );
#ifdef DEBUG
		if(ptr == NULL)
			fprintf(stderr, "MALLOC returns NULL\n");
#endif
		if(ptr == NULL)
		{
			errno = ENOMEM;
			ret = -1;
		}
		else {
			if(Read(NameServer, ptr, ret) == 0)
			{
				fprintf(stderr, 
					"Server fails to read %d chars\n", ret);
				free(*arg2);
				ret = -1;
			}
		}
	}
theend:
	alarm(0); 
	free(p);
	return(ret);
}


static int
Read(fd, buf, count)
    int	fd, count;
    char	*buf;
{
	int	n;
	while((n = read(fd, buf, count)) > 0)
	{
		if(n == count)
		{
			return(1);
		}
		buf += n;
		count -= n;
	}
	return(0);
}

static int
OpenVirtualCircuit(lfd)
    int	lfd;
{
	return(OpenAndBind(NULL, -1, 0,
		 	Network._net[_XsTypeOfStream[lfd]], _XsTypeOfStream[lfd]));
}

#endif /* XSTREAMS_COMPILE */
