/* x25.h - X.25 abstractions */

/*
 * $Header: /f/osi/h/RCS/x25.h,v 7.6 91/03/09 11:54:12 mrose Exp $
 *
 * Contributed by John Pavel, Department of Trade and Industry/National
 * Physical Laboratory in the UK
 *
 *
 * $Log:	x25.h,v $
 * Revision 7.6  91/03/09  11:54:12  mrose
 * update
 * 
 * Revision 7.5  91/02/22  09:25:18  mrose
 * Interim 6.8
 * 
 * Revision 7.4  91/01/10  16:12:30  mrose
 * bsd44
 * 
 * Revision 7.3  91/01/07  12:39:34  mrose
 * update
 * 
 * Revision 7.2  90/11/21  11:32:16  mrose
 * sun
 * 
 * Revision 7.1  89/12/07  01:08:00  mrose
 * queued writes
 * 
 * Revision 7.0  89/11/23  21:56:10  mrose
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


#ifndef _X25_
#define _X25_

/*
 *
 * #defines for generic addressing & TSEL encoded addresses.
 *
 */

#ifndef _INTERNET_
#include <sys/socket.h>
#endif

#ifdef  SUN_X25
#include <sys/ioctl.h>
#include <sys/param.h>
#include <sys/mbuf.h>
#include <sundev/syncstat.h>
#include <netx25/x25_pk.h>
#include <netx25/x25_ctl.h>
#include <netx25/x25_ioctl.h>
#endif

#include <sys/protosw.h>
#include <sys/domain.h>
#include <sys/socketvar.h>
#include <sys/errno.h>
#include <net/if.h>

#ifdef  CAMTEC
#include <cci.h>
typedef struct  ccontrolp CONN_DB;
#undef  NTPUV
#define NTPUV   2               /* CAMTEC allows only 2 iov's per read/write */
#endif

#ifdef  CAMTEC_CCL
#include <sys/ioctl.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <netccl/ccl.h>

typedef struct ccliovec CONN_DB;
#endif


#ifdef UBC_X25
#include <netccitt/x25_sockaddr.h>
#ifdef BSD44
#include <sys/ioctl.h>
#endif

#define         X25_PACKETSIZE  128

typedef struct x25_sockaddr CONN_DB;
#endif

/*  */

#ifdef  SUN_X25
#define close_x25_socket     close
#define select_x25_socket    selsocket
#define read_x25_socket      read
#define write_x25_socket     write
#endif

#if     defined(UBC_X25) || defined(CAMTEC_CCL)
#define close_x25_socket     close
#define select_x25_socket    selsocket
#endif

#ifndef	SUN_X25
#define RECV_DIAG 0
#define DIAG_TYPE 1
#define WAIT_CONFIRMATION 2
#endif

int     start_x25_client ();
int     start_x25_server ();
int     join_x25_client ();
int     join_x25_server ();
int     read_x25_socket ();
int     write_x25_socket ();
int     close_x25_socket ();
int     select_x25_socket ();

struct NSAPaddr *if2gen();
CONN_DB *gen2if();


#define ADDR_LOCAL      0
#define ADDR_REMOTE     1
#define ADDR_LISTEN     2
#define SEPARATOR ':'


#define MAXNSDU 2048			/* must be equal to largest TP0 TPDU */
#endif
