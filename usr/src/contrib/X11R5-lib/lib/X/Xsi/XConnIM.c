/*
 * $XConsortium: XConnIM.c,v 1.18 92/07/29 13:55:35 rws Exp $
 */

/*
 * Copyright 1990, 1991 by OMRON Corporation
 * Copyright 1991 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of OMRON and MIT not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  OMRON and MIT make no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as is" without express or implied warranty.
 *
 * OMRON AND MIT DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL OMRON OR MIT BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE. 
 *
 *	Author:	Seiji Kuwari	OMRON Corporation
 *				kuwa@omron.co.jp
 *				kuwa%omron.co.jp@uunet.uu.net
 */				

/*
 * This is an OS dependent file. this should work on 4.3BSD.
 */
#include "Xlibint.h"
#include "Xlibnet.h"
#include "Xi18nint.h"
#include "XIMlibint.h"
#include <X11/Xos.h>
#ifdef TCPCONN
#include <sys/socket.h>
#endif

#ifdef	UNIXCONN
#include <sys/un.h>
#ifndef	XIM_UNIX_PATH
#define	XIM_UNIX_PATH	"/tmp/.X11-unix/XIM"
#endif	/* XIM_UNIX_PATH */
#endif	/* UNIXCONN */

/*
 * Attempts to connect to an input manager, given atom id.
 */
Bool
_XipConnectIM (im, im_atom, displaybuf)
    XipIM im;
    Atom im_atom;
    char *displaybuf;
{
#ifdef STREAMSCONN
    im->fd = -1;
    return(False);
#else
    char		im_hostname[256];/* Input manager host name buffer */
    Atom		actual_type;
    int			actual_format;
    unsigned long	nitems;
    unsigned long	byte_after;
    unsigned char	*prop;
#ifdef	UNIXCONN
    struct sockaddr_un	saddr;		/* UNIX domain socket address */
#endif	/* UNIXCONN */
#ifdef TCPCONN
    struct sockaddr_in	saddr_in;	/* INET domain socket address */
    struct hostent	*hp;
    unsigned short	port;
#endif
    int			sd = -1;	/* File disclipter */
    int			indian;
    ximConnClient	client;
    ximNormalReply	reply;
    unsigned long	i;
    unsigned short	s;
    int			inet_ok = 0;

    im->fd = -1;
    /*
     * Get a property of input manager. Format of the property is
     *		char hostname[128] ( host name )
     *		unsigned short portnumber    ( port number )
     *		long major_version    ( major version of protocol )
     *		long minor_version    ( minor version of protocol )
     */
    XGetWindowProperty(im->core.display, DefaultRootWindow(im->core.display),
		       im_atom, 0L, 256L, 0, AnyPropertyType,
		       &actual_type, &actual_format, &nitems,
		       &byte_after, &prop);
    bcopy((char *)prop, im_hostname, hname_size);
    bcopy((char *)(prop + offset_of_portnumber), (char *)&s, portnumber_size);
#ifdef TCPCONN
    port = ntohs(s);
#endif
    bcopy((char *)(prop + offset_of_version), (char *)&i, version_size);
    im->major_version = (long)ntohl(i);
    bcopy((char *)(prop + offset_of_minor_version), (char *)&i, version_size);
    im->minor_version = (long)ntohl(i);
    Xfree((char *)prop);
    if (im->major_version != XIM_MAJOR_VERSION) return(False);
    if (!(im->minor_version >= XIM_MINOR_VERSION)) return(False);

#ifdef TCPCONN
    /*
     * Attempts to open INET domain socket.
     */
    if (hp = gethostbyname(im_hostname)) {
	bzero((char *)&saddr_in, (int)sizeof(saddr_in));
	bcopy(hp->h_addr, (char *)&saddr_in.sin_addr, hp->h_length);
	saddr_in.sin_family = AF_INET;
	saddr_in.sin_port = htons(port);
	if ((sd = socket(AF_INET, SOCK_STREAM, 0)) >= 0) {
	    if (connect(sd, &saddr_in, sizeof(saddr_in)) >= 0) {
		inet_ok = 1;
	    } else {
		close(sd);
		sd = -1;
	    }
	}
    }
#endif
#ifdef	UNIXCONN
    /*
     * Attempts to open UNIX domain socket.
     */
    if (inet_ok == 0) {
	saddr.sun_family = AF_UNIX;
	strcpy(saddr.sun_path, XIM_UNIX_PATH);
	if ((sd = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
	    return(False);
	}
	if (connect(sd, &saddr, strlen(saddr.sun_path)+sizeof(saddr.sun_family)) < 0) {
	    close(sd);
	    return(False);
	}
    }
#endif	/* UNIXCONN */
    if (sd == -1) return(False);

    /*
     * Send the display name to the input manager. 
     */
    indian = 1;
    if (*(char *) &indian) {
	client.byteOrder = 'l';
    } else {
	client.byteOrder = 'B';
    }
    client.length = strlen(displaybuf);

    im->fd = sd;
    if ((_XipWriteToIM(im, (char *)&client, sizeof(ximConnClient)) < 0) ||
	(_XipWriteToIM(im, (char *)displaybuf, (int)client.length) < 0) ||
	(_XipFlushToIM(im) < 0)) {
	return(False);
    }


    /*
     * Now see, if connection was accepted.
     */
    if (_XipReadFromIM(im, (char *)&reply, sz_ximNormalReply) < 0) {
	return(False);
    }
    if (reply.state != 0) {
	close(sd);
	return(False);
    }
    return(True);
#endif
}

/*
 * Disconnect from the input manager.
 */

void
_XipDisconnectIM(server)
    int server;
{
    (void) close(server);
}

