/* tp4.h - TP4 abstractions */

/* 
 * $Header: /f/osi/h/RCS/tp4.h,v 7.6 91/02/22 09:25:13 mrose Interim $
 *
 *
 * $Log:	tp4.h,v $
 * Revision 7.6  91/02/22  09:25:13  mrose
 * Interim 6.8
 * 
 * Revision 7.5  91/01/07  12:39:31  mrose
 * update
 * 
 * Revision 7.4  90/07/27  08:44:49  mrose
 * update
 * 
 * Revision 7.3  90/02/19  13:09:27  mrose
 * update
 * 
 * Revision 7.2  89/12/19  22:26:13  mrose
 * touch-up
 * 
 * Revision 7.1  89/12/19  16:18:02  mrose
 * dgram
 * 
 * Revision 7.0  89/11/23  21:56:06  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#ifndef	_TP4_
#define	_TP4_

/*  */

#ifdef	BSD_TP4
#ifndef	SOCK_STREAM
#include <sys/socket.h>
#endif
#include <netiso/iso.h>
#include <netiso/iso_errno.h>
#include <netiso/tp_user.h>

union sockaddr_osi {
    struct sockaddr_iso    osi_sockaddr;
    char osi_bigaddr[104];
};

union osi_control_msg {
    struct {
	struct cmsghdr ocm_cmhdr;
	char	ocm_cmdata[128 - sizeof (struct cmsghdr)];
    }    ocm_control;

    char ocm_data[128];
};

int	gen2tp4 (), tp42gen ();


#define	CLTS			/* have CL-mode transport service */

#ifndef	_DGRAM_
#include "dgram.h"
#endif

int	start_clts_server ();
#define	start_clts_client	start_clts_server

#define	join_clts_server(fd,sock) \
		join_dgram_aux ((fd), (struct sockaddr *) (sock), 0)
#define	join_clts_client(fd,sock) \
		join_dgram_aux ((fd), (struct sockaddr *) (sock), 1)

#define	read_clts_socket	read_dgram_socket
#define	write_clts_socket	write_dgram_socket
#define	close_clts_socket	close_dgram_socket

#define	select_clts_socket	select_dgram_socket
#define	check_clts_socket	check_dgram_socket
#endif

/*    SunLink OSI */

#ifdef SUN_TP4
#if	defined(SUNLINK_6_0) && !defined(SUNLINK_5_2)
#define	SUNLINK_5_2
#endif

#ifndef	SUNLINK_6_0
#include <sys/ieee802.h>
#else
#include <net/if_ieee802.h>
#endif
#ifndef	SOCK_STREAM
#include <sys/socket.h>
#endif
#include <netosi/osi.h>
#ifdef	SUNLINK_5_2
#include <netosi/osi_profile.h>
#endif
#include <netosi/osi_addr.h>
#include <netosi/osi_error.h>
#include <netosi/tp_event.h>


#define	MSG_OOB         0x1	/* process out-of-band data */


struct tp4pkt {
    union {
	TP_MSG		  tp_msg;
	TP_MSG_CONNECT	  tp_connect;
	TP_MSG_DATA	  tp_data;
	TP_MSG_X_DATA	  tp_x_data;
	TP_MSG_DISCONNECT tp_disconnect;
    } tp_un;
#define tp4_event	tp_un.tp_msg.tp_event
#define tp4_called 	tp_un.tp_connect.dst_address
#define tp4_calling 	tp_un.tp_connect.src_address
#define tp4_expedited	tp_un.tp_connect.expedited_selected
#define tp4_qos		tp_un.tp_connect.tp_qos
#define tp4_eot		tp_un.tp_data.eot
#define tp4_reason	tp_un.tp_disconnect.reason
};

struct tp4pkt *newtp4pkt ();
#define	freetp4pkt(tp)	cfree ((char *) (tp))

int	gen2tp4 (), tp42gen ();
#endif

/*  */

#ifdef	BSD_TP4
#define	close_tp4_socket	close
#define	select_tp4_socket	selsocket
#endif

#ifdef	SUN_TP4
#define	close_tp4_socket	close
#define	select_tp4_socket	selsocket
#endif

int	close_tp4_socket ();
int	select_tp4_socket ();

#endif

