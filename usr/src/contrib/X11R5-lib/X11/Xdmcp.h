/* $XConsortium: Xdmcp.h,v 1.8 91/07/23 22:28:07 keith Exp $ */
/*
 * Copyright 1989 Network Computing Devices, Inc., Mountain View, California.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of N.C.D. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  N.C.D. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 */

#ifndef _XDMCP_H_
#define _XDMCP_H_
#define XDM_PROTOCOL_VERSION	1
#define XDM_UDP_PORT		177
#define XDM_MAX_MSGLEN		8192
#define XDM_MIN_RTX		2
#define XDM_MAX_RTX		32
#define XDM_RTX_LIMIT		7
#define XDM_KA_RTX_LIMIT	4
#define XDM_DEF_DORMANCY	(3 * 60)	/* 3 minutes */
#define XDM_MAX_DORMANCY	(24 * 60 * 60)	/* 24 hours */

typedef enum {
    BROADCAST_QUERY = 1, QUERY, INDIRECT_QUERY, FORWARD_QUERY,
    WILLING, UNWILLING, REQUEST, ACCEPT, DECLINE, MANAGE, REFUSE, 
    FAILED, KEEPALIVE, ALIVE 
} xdmOpCode;

typedef enum {
    XDM_QUERY, XDM_BROADCAST, XDM_INDIRECT, XDM_COLLECT_QUERY,
    XDM_COLLECT_BROADCAST_QUERY, XDM_COLLECT_INDIRECT_QUERY,
    XDM_START_CONNECTION, XDM_AWAIT_REQUEST_RESPONSE,
    XDM_AWAIT_MANAGE_RESPONSE, XDM_MANAGE, XDM_RUN_SESSION, XDM_OFF,
    XDM_AWAIT_USER_INPUT, XDM_KEEPALIVE, XDM_AWAIT_ALIVE_RESPONSE
} xdmcp_states;

#ifdef NOTDEF
/* table of hosts */

#define XDM_MAX_STR_LEN 21
#define XDM_MAX_HOSTS 20
struct xdm_host_table {
  struct sockaddr_in sockaddr;
  char name[XDM_MAX_STR_LEN];
  char status[XDM_MAX_STR_LEN];
};
#endif /* NOTDEF */

typedef CARD8	*CARD8Ptr;
typedef CARD16	*CARD16Ptr;
typedef CARD32	*CARD32Ptr;

typedef struct _ARRAY8 {
    CARD16	length;
    CARD8Ptr	data;
} ARRAY8, *ARRAY8Ptr;

typedef struct _ARRAY16 {
    CARD8	length;
    CARD16Ptr	data;
} ARRAY16, *ARRAY16Ptr;

typedef struct _ARRAY32 {
    CARD8	length;
    CARD32Ptr	data;
} ARRAY32, *ARRAY32Ptr;

typedef struct _ARRAYofARRAY8 {
    CARD8	length;
    ARRAY8Ptr	data;
} ARRAYofARRAY8, *ARRAYofARRAY8Ptr;

typedef struct _XdmcpHeader {
    CARD16  version, opcode, length;
} XdmcpHeader, *XdmcpHeaderPtr;

typedef struct _XdmcpBuffer {
    BYTE    *data;
    int	    size;		/* size of buffer pointed by to data */
    int	    pointer;		/* current index into data */
    int	    count;		/* bytes read from network into data */
} XdmcpBuffer, *XdmcpBufferPtr;

typedef struct _XdmAuthKey {
    BYTE    data[8];
} XdmAuthKeyRec, *XdmAuthKeyPtr;


/* implementation-independent network address structure.
   Equiv to sockaddr* for sockets and netbuf* for STREAMS. */

typedef char *XdmcpNetaddr;


extern int XdmcpWriteCARD8(),		XdmcpWriteCARD16();
extern int XdmcpWriteCARD32();
extern int XdmcpWriteARRAY8(),		XdmcpWriteARRAY16();
extern int XdmcpWriteARRAY32(),		XdmcpWriteARRAYofARRAY8();
extern int XdmcpWriteHeader(),		XdmcpFlush();

extern int XdmcpReadCARD8(),		XdmcpReadCARD16();
extern int XdmcpReadCARD32();
extern int XdmcpReadARRAY8(),		XdmcpReadARRAY16();
extern int XdmcpReadARRAY32(),		XdmcpReadARRAYofARRAY8();
extern int XdmcpReadHeader(),		XdmcpFill();

extern int  XdmcpReadRemaining();

extern void XdmcpDisposeARRAY8(),	XdmcpDisposeARRAY16();
extern void XdmcpDisposeARRAY32(),	XdmcpDisposeARRAYofARRAY8();

extern int XdmcpCopyARRAY8();

extern int XdmcpARRAY8Equal();

#ifdef HASXDMAUTH
extern void XdmcpGenerateKey();
extern void XdmcpIncrementKey();
extern void XdmcpDecrementKey();
extern void XdmcpWrap();
extern void XdmcpUnwrap();
#endif

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

#ifndef Xalloc
#ifndef xalloc
extern long *Xalloc (), *Xrealloc ();
extern void Xfree();
#endif
#endif
#endif /* _XDMCP_H_ */
