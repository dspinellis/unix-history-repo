/* $XConsortium: Xstreams.h,v 1.6 91/07/19 23:22:53 gildea Exp $ */
/*	Copyright (c) 1990, 1991 UNIX System Laboratories, Inc. */
/*	Copyright (c) 1988 AT&T */
/*	  All Rights Reserved	*/

/*
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of AT&T or USL not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  AT&T and USL make no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * AT&T and USL DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN
 * NO EVENT SHALL AT&T or USL BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
 * OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */


#ifndef _XSTREAMS_H_
#define _XSTREAMS_H_
/*
 Xstreams.h (C header file)
	Acc: 575304412 Tue Apr 26 09:46:52 1988
	Mod: 574017273 Tue Apr 26 12:14:33 1988
	Sta: 574017273 Tue Apr 26 12:14:33 1988
	Owner: 2011
	Group: 1985
	Permissions: 644
*/
/*
	START USER STAMP AREA
*/
/*
	END USER STAMP AREA
*/



#define	MEM_ALLIGN(ptr) ((((unsigned) (ptr + 3)) >> 2) << 2)

#define	CONNECT_TIMEOUT		60
#define	MAX_AUTO_BUF_LEN	256
#define	MAX_DISP_DIGITS		20
#define	MAX_NETS	8

typedef struct _host {
	char	host_name[32];
	int	host_len;
	struct _host *next;
} HOST;

/*
 * Structure for handling multiple connection requests on the same stream.
 */

struct listenCall {
	struct t_call *CurrentCall;
	struct listenCall *NextCall;
};

struct listenQue {
	struct listenCall *QueHead;
	struct listenCall *QueTail;
};

#define EMPTY(p)	(p->QueHead == (struct listenCall *) NULL)


typedef struct {
	int	flags;
	char	type;
	int	display;
	char	*inputbuf;
	int	buflen;
	int	bufptr;
	int	msglen;
	} IOBUFFER;

typedef struct {
	int	_nnets;
#ifdef SVR4
	struct netconfig *_net[MAX_NETS];
#else
	char	*_net[MAX_NETS];
#endif
        struct listenQue FreeList[MAX_NETS];
	struct listenQue PendingQue[MAX_NETS];
	int	_npeers;
	char	**_peer;
	int	*_peerlen;
	HOST	*_validhosts;
	} networkInfo;


typedef struct _Xstream {
	int	(*SetupTheListener)();
	int	(*ConnectNewClient)();
	int	(*CallTheListener)();
	int	(*ReadFromStream)();
	int	(*BytesCanBeRead)();
	int	(*WriteToStream)();
	int	(*CloseStream)();
	int	(*CreateAddress)();
	union ext {
		int	(*NameServer)();
		networkInfo *NetInfo;
		} u;	
	} Xstream;

/* old shared libraries have the names already fixed */
#ifdef USL_COMPAT
#define _XsStream		xstream
#define _XReadV			_readv
#define _XWriteV		_writev
#define _XSelect 		XSelect
#define _XsErrorCall		ErrorCall		
#define	_XsSetupLocalStream	SetupLocalStream
#define	_XsConnectLocalClient	ConnectLocalClient
#define	_XsOpenSpServer		OpenSpServer
#define	_XsReadLocalStream	ReadLocalStream
#define	_XsConnectTliClient	ConnectTliClient
#define _XsSetupTliStrean	SetupTliStream
#define _XsCallTliServer	CallTliServer
#define	_XsCallLocalServer	CallLocalServer
#define _XsTypeOfStream		TypeOfStream
#ifdef SVR4
#define	_XsSetupNamedStream	SetupNamedStream
#define _XsSetupSpStream	SetupSpStream
#endif /* SVR4 */
#endif /* USL_COMPAT */

extern Xstream _XsStream[];

#define NO_BUFFERING	0
#define BUFFERING	1

/* Network services */

#define OpenDaemonConnection	0
#define	PEER_NAME		1
#define	PEER_ALLOC		2
#define	PEER_FREE		3
#define	ConvertNetAddrToName	4
#define	ConvertNameToNetAddr	5
#define	ConvertNameToTliCall	6
#define	ConvertTliCallToName	7
#define	ConvertNameToTliBind	8

#define	UNAME_LENGTH	14

#define X_LOCAL_STREAM	0
#define X_NAMED_STREAM	1
/* Enhanced Application Compatibility Support */
#define X_SP_STREAM	2
/* End Enhanced Application Compatibility Support */

#define X_TLI_STREAM	3
#define CLOSED_STREAM	-1

/*
	The following are defined in X.h. Any changes to FamilyUname
	should take X.h into consideration.
*/
		/* protocol families */

		/*

		#define FamilyInternet		0
		#define FamilyDECnet		1
		#define FamilyChaos		2

		*/

#define FamilyUname	3

#define X_TCP_PORT		6000

#define NAMED_LISTENER "/dev/X/Nserver"
#define LOCAL_LISTENER "/dev/X/server"
/* Enhanced Application Compatibility Support */
/* End Enhanced Application Compatibility Support */

#define	NAME_SERVER_NODE "/dev/X/nameserver"
#define XNETSPECDIR	"lib/net"
#define XROOTDIR "/usr/X"

#define	MAX_SIMUL_TLI_CALLS	20

#define SetupNetworkInfo()   _XsStream[X_LOCAL_STREAM].u.NetInfo = &Network; \
	_XsStream[X_NAMED_STREAM].u.NetInfo = &Network; \
/* Enhanced Application Compatibility Support */ \
/* End Enhanced Application Compatibility Support */ \
	_XsStream[X_TLI_STREAM].u.NameServer = nameserver

#define NetworkInfo (_XsStream[X_LOCAL_STREAM].u.NetInfo)
#define GetNetworkInfo (*_XsStream[X_TLI_STREAM].u.NameServer)
#define validhosts _XsStream[X_LOCAL_STREAM].u.NetInfo->_validhosts

/*
 *	header of messages sent by X to the nameserver 
 *      1st int: the size of the entire message.
 *	2nd int: the size of the header itself.
 *  	3rd int: the service number.
 *      4th int: the display number.
 * 	5th int: the length of the network name.
 */
 
#define HEADERSIZE	(5*sizeof(int))
#endif /* _XSTREAMS_H_ */
