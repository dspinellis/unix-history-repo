#ifndef CKCNET_H
#define CKCNET_H

/* Network types */

#define NET_NONE 0			/* None */
#define NET_TCPB 1			/* TCP/IP Berkeley (socket) */
#define NET_TCPA 2			/* TCP/IP AT&T (streams) */
#define NET_SX25 3			/* SUNOS SunLink X.25 */
#define NET_DEC  4			/* DECnet */
#define NET_VPSI 5			/* VAX PSI */
#define NET_PIPE 6			/* LAN Manager Named Pipe */

/* Network virtual terminal protocols */

#define NP_NONE 0			/* None (async) */
#define NP_TELNET 1			/* TCP/IP telnet */
#define NP_VTP 2			/* ISO Virtual Terminal Protocol */
#define NP_X3 3				/* CCITT X.3 */
#define NP_X28 4			/* CCITT X.28 */
#define NP_X29 5			/* CCITT X.28 */
#define NP_CTERM 20			/* DEC CTERM */
#define NP_LAT 21			/* DEC LAT */
/* others here... */

/* Basic network function prototypes, common to all. */

_PROTOTYP( int netopen, (char *, int *, int) );
_PROTOTYP( int netclos, (void) );
_PROTOTYP( int netflui, (void) );
_PROTOTYP( int nettchk, (void) );
_PROTOTYP( int netbreak, (void) );
_PROTOTYP( int netinc, (int) );
_PROTOTYP( int nettol, (char *, int) );
_PROTOTYP( int nettoc, (char) );

/*
  SunLink X.25 support by Marcello Frutig, Catholic University,
  Rio de Janeiro, Brazil, 1990.

  Maybe this can be adapted to VAX PSI and other X.25 products too.
*/
#ifndef SUNOS4				/* Only valid for SUNOS4 */
#ifdef SUNX25
#undef SUNX25
#endif /* SUNX25 */
#endif /* SUNOS4 */

#ifdef SUNX25

#ifndef NETCONN				/* SUNX25 implies NETCONN */
#define NETCONN
#endif /* NETCONN */

#define MAXPADPARMS                18	/* Number of PAD parameters */
#define MAXCUDATA		   12	/* Max length of X.25 call user data */
#define X29PID			    1   /* X.29 protocol ID */
#define X29PIDLEN		    4   /* X.29 protocol ID length */

#define X29_SET_PARMS               2
#define X29_READ_PARMS              4
#define X29_SET_AND_READ_PARMS      6
#define X29_INVITATION_TO_CLEAR     1
#define X29_PARAMETER_INDICATION    0
#define X29_INDICATION_OF_BREAK     3
#define X29_ERROR                   5

#define INVALID_PAD_PARM            1

#define PAD_BREAK_CHARACTER         0

#define PAD_ESCAPE                  1
#define PAD_ECHO                    2
#define PAD_DATA_FORWARD_CHAR       3
#define PAD_DATA_FORWARD_TIMEOUT    4
#define PAD_FLOW_CONTROL_BY_PAD     5
#define PAD_SUPPRESSION_OF_SIGNALS  6
#define PAD_BREAK_ACTION            7
#define PAD_SUPPRESSION_OF_DATA     8
#define PAD_PADDING_AFTER_CR        9
#define PAD_LINE_FOLDING           10
#define PAD_LINE_SPEED             11
#define PAD_FLOW_CONTROL_BY_USER   12
#define PAD_LF_AFTER_CR            13
#define PAD_PADDING_AFTER_LF       14
#define PAD_EDITING                15
#define PAD_CHAR_DELETE_CHAR       16
#define PAD_BUFFER_DELETE_CHAR     17
#define PAD_BUFFER_DISPLAY_CHAR    18

#define MAXIX25 MAX_USER_DATA*7
#define MAXOX25 MAX_USER_DATA

#include <sys/ioctl.h>			/* X.25 includes */
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/mbuf.h>
#include <sys/socket.h>
#include <sys/protosw.h>
#include <sys/domain.h>
#include <sys/socketvar.h>
#include <net/if.h>
#include <sundev/syncstat.h>
#include <netx25/x25_pk.h>
#include <netx25/x25_ctl.h>
#include <netx25/x25_ioctl.h>

/* C-Kermit X.3 / X.25 / X.29 / X.121 support functions */

_PROTOTYP( VOID shopad, (void) );
_PROTOTYP( VOID initpad, (void) );
_PROTOTYP( VOID setpad, (CHAR *, int) );
_PROTOTYP( VOID readpad, (CHAR *, int, CHAR *) );
_PROTOTYP( int qbitpkt, (CHAR *, int) );
_PROTOTYP( VOID setqbit, (void) );
_PROTOTYP( VOID resetqbit, (void) );
_PROTOTYP( VOID breakact, (void) );
_PROTOTYP( int pkx121, (char *, CHAR *) );
_PROTOTYP( VOID x25oobh, (void) );
_PROTOTYP( int x25diag, (void) );
_PROTOTYP( int x25intr, (char) );
_PROTOTYP( int x25reset, (char, char) );
_PROTOTYP( int x25clear, (void) );
_PROTOTYP( int x25stat, (void) );
_PROTOTYP( int x25in, (int, CHAR *) );
_PROTOTYP( int x25inl, (CHAR *, int, int, CHAR) );

#endif /* SUNX25 */

/* DEC TCP/IP for (Open)VMS, previously known as UCX */

#ifdef DEC_TCPIP			/* DEC_TCPIP implies TCPSOCKET */
#ifndef TCPSOCKET
#define TCPSOCKET
#endif /* TCPSOCKET */
#ifndef VMSTCPIP
#define VMSTCPIP
#endif /* VMSTCPIP */
#endif /* DEC_TCPIP */

/* TGV/SRI MultiNet, TCP/IP for VAX/VMS */

#ifdef MULTINET				/* MULTINET implies TCPSOCKET */
#ifndef TCPSOCKET
#define TCPSOCKET
#endif /* TCPSOCKET */
#ifndef VMSTCPIP
#define VMSTCPIP
#endif /* VMSTCPIP */
#ifndef TGVORWIN			/* MULTINET and WINTCP */
#define TGVORWIN			/* share a lot of code... */
#endif /* TGVORWIN */
#endif /* MULTINET */

/* Wollongong TCP/IP for VAX/VMS */

#ifdef WINTCP				/* WINTCP implies TCPSOCKET */
#ifndef TCPSOCKET
#define TCPSOCKET
#endif /* TCPSOCKET */
#ifndef VMSTCPIP
#define VMSTCPIP
#endif /* VMSTCPIP */
#ifndef TGVORWIN			/* WINTCP and MULTINET */
#define TGVORWIN			/* share a lot of code... */
#endif /* TGVORWIN */
#endif /* WINTCP */

/* Wollongong TCP/IP for AT&T Sys V */

#ifdef WOLLONGONG			/* WOLLONGONG implies TCPSOCKET */
#ifndef TCPSOCKET			/* Don't confuse WOLLONGONG */
#define TCPSOCKET			/* (which is for UNIX) with */
#endif /* TCPSOCKET */			/* WINTCP, which is for VMS! */
#endif /* WOLLONGONG */

#ifdef EXCELAN				/* EXCELAN implies TCPSOCKET */
#ifndef TCPSOCKET
#define TCPSOCKET
#endif /* TCPSOCKET */
#endif /* EXCELAN */

#ifdef INTERLAN				/* INTERLAN implies TCPSOCKET */
#ifndef TCPSOCKET
#define TCPSOCKET
#endif /* TCPSOCKET */
#endif /* INTERLAN */

/* Telnet protocol */

#ifdef TCPSOCKET			/* TCPSOCKET implies TNCODE */
#ifndef TNCODE				/* Which means... */
#define TNCODE				/* Compile in telnet code */
#endif /* TNCODE */
#endif /* TCPSOCKET */

#ifdef SUNX25				/* SUNX25 implies TCPSOCKET */
#ifndef TCPSOCKET			/* But doesn't imply TNCODE */
#define TCPSOCKET
#endif /* TCPSOCKET */
#endif /* SUNX25 */

/* This is the TCPSOCKET section... */

#ifdef TCPSOCKET

#ifndef NETCONN				/* TCPSOCKET implies NETCONN */
#define NETCONN
#endif /* NETCONN */

/* BSD sockets library header files */

#ifdef UNIX				/* UNIX section */

#ifdef INTERLAN				/* Racal-Interlan TCP/IP */
#include <interlan/socket.h>
#include <interlan/il_types.h>
#include <interlan/telnet.h>
#include <interlan/il_errno.h>
#include <interlan/in.h>
#include <interlan/telnet.h>
#else					/* Normal BSD TCP/IP library */
#ifndef HPUX
#include <arpa/telnet.h>
#endif /* HPUX */
#include <sys/socket.h>
#ifdef WOLLONGONG
#include <sys/in.h>
#else
#include <netinet/in.h>
#endif /* WOLLONGONG */
#endif /* INTERLAN */

#ifndef EXCELAN
#include <netdb.h>
#ifndef INTERLAN
#ifdef WOLLONGONG
#define minor				/* Do not include <sys/macros.h> */
#include <sys/inet.h>
#else
#ifndef OXOS
#ifndef HPUX
#include <arpa/inet.h>
#endif /* HPUX */
#else /* OXOS */
/* In too many releases of X/OS, <arpa/inet.h> declares inet_addr() as
 * ``struct in_addr''.  This is definitively wrong, and could cause
 * core dumps.  Instead of including that bad file, inet_addr() is
 * correctly declared here.  Of course, all the declarations done there
 * has been copied here.
 */
unsigned long inet_addr();
char	*inet_ntoa();
struct	in_addr inet_makeaddr();
unsigned long inet_network();
#endif /* OXOS */
#endif /* WOLLONGONG */
#endif /* INTERLAN */
#endif /* EXCELAN */

#ifdef EXCELAN				/* Excelan TCP/IP */
#include <ex_errno.h>
#endif /* EXCELAN */

#ifdef I386IX				/* Interactive Sys V R3 network. */
/* #define TELOPTS */			/* This might need defining. */
#define ORG_NLONG ENAMETOOLONG		/* Resolve conflicting symbols */
#undef ENAMETOOLONG			/* in <errno.h> and <net/errno.h> */
#define ORG_NEMPTY ENOTEMPTY
#undef ENOTEMPTY
#include <net/errno.h>
#undef ENAMETOOLONG
#define ENAMETOOLONG ORG_NLONG
#undef ENOTEMPTY
#define ENOTEMPTY ORG_NEMPTY
#endif /* I386IX */
/*
  Data type of the inet_addr() function...
  We define INADDRX if it is of type struct inaddr.
  If it is undefined, unsigned long is assumed.
  Look at <arpa/inet.h> to find out.  The following known cases are
  handled here.  Other systems that need it can be added here, or else
  -DINADDRX can be included in the CFLAGS on the cc command line.
*/
#ifdef DGUX540				/* Data General UX 5.40 */
#define INADDRX
#endif /* DGUX540 */
#ifdef DU2				/* DEC Ultrix 2.0 */
#define INADDRX
#endif /* DU2 */

#else /* Not UNIX */

#ifdef VMS				/* VAX/VMS section */

#ifdef WINTCP				/* TWG WIN/TCP for VMS */
#include <errno.h>
#include "twg$tcp:[netdist.include.sys]types.h"
#include "twg$tcp:[netdist.include.sys]socket.h"
#include "twg$tcp:[netdist.include]netdb.h"
#include "twg$tcp:[netdist.include.sys]domain.h"
#include "twg$tcp:[netdist.include.sys]protosw.h"
#include "twg$tcp:[netdist.include.netinet]in.h"
#include "twg$tcp:[netdist.include.sys]ioctl.h"
#endif /* WINTCP */

#ifdef MULTINET				/* TGV MultiNet */
#include "multinet_root:[multinet.include]errno.h"
#include "multinet_root:[multinet.include.sys]types.h"
#include "multinet_root:[multinet.include.sys]socket.h"
#include "multinet_root:[multinet.include]netdb.h"
#include "multinet_root:[multinet.include.netinet]in.h"
#include "multinet_root:[multinet.include.sys]ioctl.h"
#endif /* MULTINET */

#ifdef DEC_TCPIP
#include <in.h>
#include <netdb.h>
#include <socket.h>
#include "ckvioc.h"
#define socket_errno errno
#define bzero(s,n) memset(s,0,n) 
#define bcopy(h,a,l) memmove(a,h,l)
#define socket_read 	read
#define socket_write 	write
#define socket_ioctl	ioctl
#define socket_close    close
#endif /* DEC_TCPIP */

#endif /* VMS */
#endif /* UNIX */
#endif /* TCPSOCKET */

#ifdef TNCODE				/* If we're compiling telnet code... */
/*
  Make sure telnet symbols are defined; can't rely on library header files
  for any of them.
*/
#ifndef IAC				/* First the telnet commands */
#define IAC 255
#endif /* IAC */
#ifndef DONT
#define DONT 254
#endif /* DONT */
#ifndef DO
#define DO 253
#endif /* DO */
#ifndef WONT
#define WONT 252
#endif /* WONT */
#ifndef WILL
#define WILL 251
#endif /* WILL */
#ifndef SB
#define SB 250
#endif /* SB */
#ifndef BREAK
#define BREAK 243
#endif /* BREAK */
#ifndef SE
#define SE 240
#endif /* SE */

#ifndef TELOPT_ECHO			/* Then the options */
#define TELOPT_ECHO 1
#endif /* TELOPT_ECHO */
#ifndef TELOPT_SGA
#define	TELOPT_SGA 3
#endif /* TELOPT_SGA */
#ifndef TELOPT_STATUS
#define	TELOPT_STATUS 5
#endif /* TELOPT_STATUS */
#ifndef TELOPT_TTYPE
#define	TELOPT_TTYPE 24
#endif /* TELOPT_TTYPE */
#ifndef NTELOPTS
#define	NTELOPTS 24
#endif /* NTELOPTS */

/* Telnet protocol functions defined in C-Kermit */

_PROTOTYP( int tn_ini, (void) );	/* Telnet protocol support */
_PROTOTYP( int tn_sopt, (int, int) );
_PROTOTYP( int tn_doop, (CHAR, int, int (*)(int) ) );
_PROTOTYP( int tn_sttyp, (void) );
_PROTOTYP( int tnsndbrk, (void) );

#endif /* TNCODE */

#endif /* CKCNET_H */
