/* port.h
   Header file for routines which manipulate ports.

   Copyright (C) 1991, 1992 Ian Lance Taylor

   This file is part of the Taylor UUCP package.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   The author of the program may be contacted at ian@airs.com or
   c/o AIRS, P.O. Box 520, Waltham, MA 02254.

   $Log: port.h,v $
   Revision 1.13  1992/03/12  19:56:10  ian
   Debugging based on types rather than number

   Revision 1.12  1992/03/10  21:47:39  ian
   Added protocol command for ports

   Revision 1.11  1992/03/08  04:56:21  ian
   Peter da Silva: added ``lockname'' command for ports

   Revision 1.10  1992/03/04  00:36:44  ian
   Michael Richardson: better chat script debugging

   Revision 1.9  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.8  1991/12/17  23:14:08  ian
   T. William Wells: allow dialer complete and abort to be chat scripts

   Revision 1.7  1991/12/15  03:42:33  ian
   Added tprocess_chat_cmd for all chat commands, and added CMDTABTYPE_PREFIX

   Revision 1.6  1991/11/14  03:20:13  ian
   Added seven-bit and reliable commands to help when selecting protocols

   Revision 1.5  1991/11/13  20:38:00  ian
   Added TCP port type for connections over TCP

   Revision 1.4  1991/11/11  23:47:24  ian
   Added chat-program to run a program to do a chat script

   Revision 1.3  1991/11/11  16:59:05  ian
   Eliminate fread_port_info, allow NULL pflock arg to ffind_port

   Revision 1.2  1991/11/11  00:39:45  ian
   Open port in seven bit mode, added fport_set to change to eight bit

   Revision 1.1  1991/09/10  19:47:55  ian
   Initial revision

   */

#ifndef PORT_H

#define PORT_H

#include "sysdep.h"

/* Information kept about using stdin as a port.  */

struct sstdin_port
{
  struct ssysdep_stdin_port s;
};

/* The smodem_port structure holds the information we keep about a modem
   port.  */

struct smodem_port
{
  /* The device name.  If NULL, use the port name instead.  */
  const char *zdevice;
  /* The device name to output dialer instructions to.  If NULL, use
     the normal port.  */
  const char *zdial_device;
  /* The default baud rate.  */
  long ibaud;
  /* The low baud rate, if a range is used.  */
  long ilowbaud;
  /* The high baud rate, if a range is used.  */
  long ihighbaud;
  /* Whether to wait for carrier detect after dialing.  */
  boolean fcarrier;
  /* The name of the dialer to use.  */
  const char *zdialer;
  /* Specific dialer information, if zdialer is NULL.  */
  struct sdialer *qdialer;
  /* System dependent modem port stuff.  */
  struct ssysdep_modem_port s;
};

/* Information kept about a direct port.  */

struct sdirect_port
{
  /* The device to use (if NULL, use the port name).  */
  const char *zdevice;
  /* The baud rate to communicate at.  */
  long ibaud;
  /* System dependent direct port stuff.  */
  struct ssysdep_direct_port s;
};

#if HAVE_TCP

/* Information kept about a TCP port.  */

struct stcp_port
{
  /* The file descriptor.  */
  int o;
  /* The TCP port number to use.  */
  const char *zport;
};

#endif /* HAVE_TCP */

/* Types of ports.  The order of this enumeration must be the same
   as the initializer for asPortcmds and azPtype_names.  */

enum tporttype
{
  PORTTYPE_STDIN,
  PORTTYPE_MODEM,
  PORTTYPE_DIRECT
#if HAVE_TCP
  , PORTTYPE_TCP
#endif
};

/* Port settings.  Currently we only have seven bit and eight bit
   ports.  The only real difference between these is that seven bit
   ports can do XON/XOFF.  */

enum tportsetting
{
  PORTSETTING_EIGHT,
  PORTSETTING_SEVEN
};

/* Reliability bits.  These bits are used to decide which protocol to
   run.  A given protocol will have a set of these bits, and each of
   them must be turned on for the port before we will permit that
   protocol to be used.  Of course, this can always be overridden by
   using the ``protocols'' command in the system file.  Reliability
   bits can be specified by both ports and dialers.  */

/* Whether a set of reliability bits is given (0 means no
   information).  */
#define RELIABLE_SPECIFIED (01)

/* Whether the connection is eight bit transparent.  */
#define RELIABLE_EIGHT (02)

/* Whether the connection is error-free.  */
#define RELIABLE_RELIABLE (04)

/* Whether the connection is end-to-end reliable (e.g. TCP).  */
#define RELIABLE_ENDTOEND (010)

/* Information kept about a port.  */

struct sport
{
  /* The name of the port.  */
  const char *zname;
  /* The type of the port.  */
  enum tporttype ttype;
  /* The protocol string for the port.  */
  const char *zprotocols;
  /* The number of protocol parameter entries.  */
  int cproto_params;
  /* The protocol parameter string.  */
  struct sproto_param *qproto_params;
  /* The set of reliability bits.  */
  int ireliable;
  /* The lock file name to use.  */
  const char *zlockname;
  /* The type specific information.  */
  union
    {
      struct sstdin_port sstdin;
      struct smodem_port smodem;
      struct sdirect_port sdirect;
#if HAVE_TCP
      struct stcp_port stcp;
#endif
    } u;
};

/* Information kept about a dialer.  */

struct sdialer
{
  /* The name.  */
  const char *zname;
  /* Chat script.  */
  struct schat_info schat;
  /* The ``wait for dialtone'' string.  */
  const char *zdialtone;
  /* The ``pause'' string.  */
  const char *zpause;
  /* Whether the dialer supports carrier.  */
  boolean fcarrier;
  /* How many seconds to wait for carrier (if fcarrier TRUE).  */
  int ccarrier_wait;
  /* Whether to toggle DTR.  */
  boolean fdtr_toggle;
  /* Whether to wait after toggling DTR.  */
  boolean fdtr_toggle_wait;
  /* The chat script to use when a call is complete.  */
  struct schat_info scomplete;
  /* The chat script to use when a call is aborted.  */
  struct schat_info sabort;
  /* Number of protocol parameters.  */
  int cproto_params;
  /* The protocol parameters.  */
  struct sproto_param *qproto_params;
  /* The set of reliability bits.  */
  int ireliable;
};

/* A command table holds the functions which implement actions for
   each different kind of port.  */

struct sportcmds
{
  /* Lock the port.  The fin argument is TRUE if the port is to be
     used for an incoming call, FALSE for an outgoing call.  */
  boolean (*pflock) P((struct sport *qport, boolean fin));
  /* Open the port.  */
  boolean (*pfopen) P((struct sport *qport, long ibaud,
		       boolean fwait));
  /* Close the port.  */
  boolean (*pfclose) P((struct sport *qport, boolean fsuccess));
  /* Reset the port so that another call may be accepted.  */
  boolean (*pfreset) P((struct sport *qport));
  /* Dial a number on a port (may be NULL).  This set *pcproto_params
     and *pqproto_params to the protocol parameters of the dialer, if
     any, and *pireliable to the reliable code of the dialer (a hack
     to avoid reading dialer information twice).  */
  boolean (*pfdial) P((struct sport *qport, const struct ssysteminfo *qsys,
		       int *pcproto_params,
		       struct sproto_param **pqproto_params,
		       int *pireliable));
  /* Read data from the port, with a timeout in seconds.  When called
     *pclen is the length of the buffer; on successful return *pclen
     is the number of bytes read into the buffer.  The cmin argument
     is the minimum number of bytes to read before returning ahead
     of a timeout.  */
  boolean (*pfread) P((struct sport *qport, char *zbuf, int *pclen,
		       int cmin, int ctimeout, boolean freport));
  /* Write data to the port.  */
  boolean (*pfwrite) P((struct sport *qport, const char *zbuf,
			int clen));
  /* Read and write data to the port.  This reads and writes data
     until either all passed in data has been written or the read
     buffer has been filled.  When called *pcread is the size of
     the read buffer and *pcwrite is the number of bytes to write;
     on successful return *pcread is the number of bytes read and
     *pcwrite is the number of bytes written.  */
  boolean (*pfio) P((struct sport *qport, const char *zwrite,
		     int *pcwrite, char *zread, int *pcread));
  /* Send a break character.  This may be NULL.  */
  boolean (*pfbreak) P((struct sport *qport));
  /* Change the port setting.  This may be NULL.  */
  boolean (*pfset) P((struct sport *qport, enum tportsetting tset));
  /* Run a chat program on a port.  */
  boolean (*pfchat) P((struct sport *qport, const char *zprog));
  /* Get the baud rate of a port.  */
  long (*pibaud) P((struct sport *qport));
};

/* Only one port can be open during an execution of uucico, and we
   keep it in this global variable.  */

extern struct sport *qPort;

/* The table of port functions.  The order of entries in this table
   must match the tporttype enumeration.  */

extern const struct sportcmds asPortcmds[];

/* Port functions.  */

/* Process a port command.  The pvar argument should point to a pointer
   to type struct sport.  */
extern enum tcmdtabret tprocess_port_cmd P((int cargs, char **azargs,
					    pointer pvar, const char *zerr));

/* Read dialer information.  */
extern boolean fread_dialer_info P((const char *zdialer,
				    struct sdialer *qdialer));

#if HAVE_V2_CONFIG
/* Read information from the V2 configuration files.  */

extern boolean fv2_find_port P((const char *zname, long ibaud,
				long ihighbaud, struct sport *qport,
				boolean (*pflock) P((struct sport *,
						     boolean fin)),
				boolean *pffound));
#endif /* HAVE_V2_CONFIG */

#if HAVE_BNU_CONFIG
/* Read information from the BNU configuration files.  */

extern boolean fbnu_find_port P((const char *zname, long ibaud,
				 long ihighbaud, struct sport *qport,
				 boolean (*pflock) P((struct sport *,
						      boolean fin)),
				 boolean *pffound));
extern boolean fbnu_read_dialer_info P((const char *zdialer,
					struct sdialer *qdialer));
#endif /* HAVE_BNU_CONFIG */

/* Lock a port.  The fin argument is TRUE if the port is to be used
   for an incoming call; certains type of Unix locking need this
   information because they need to open the port.  */
extern boolean fport_lock P((struct sport *qport, boolean fin));

/* Find and lock a port.  If the port name is NULL, the matching is
   done only on the baud rate.  If the baud rate is 0, the matching is
   done only on the port name.  Otherwise the port must match both
   name and baud rate.  If ihighbaud is not 0, the baud rate of the
   port must fall between ibaud and ihighbaud.  */
extern boolean ffind_port P((const char *zname, long ibaud,
			     long ihighbaud, struct sport *qport,
			     boolean (*pflock) P((struct sport *,
						  boolean fin)),
			     boolean freport));

/* Open a port.  If ibaud is 0, the natural baud rate of the port is
   used.  If ihighbaud is not 0, fport_open figures out what baud rate
   to use based on the port's baud rate.  */
extern boolean fport_open P((struct sport *qport, long ibaud,
			     long ihighbaud, boolean fwait));

/* Close and unlock a port.  The fsuccess argument is TRUE if the
   conversation on the port was completed normally, FALSE if it is
   being aborted.  */
extern boolean fport_close P((boolean fsuccess));

/* Reset a port such that another call may be accepted.  */
extern boolean fport_reset P((void));

/* Dial out on a port.  This is permitted to return a set of protocol
   parameters and reliability bits for the dialer (yes, it's a hack;
   this permits protocol parameters to set for particular modems).  */
extern boolean fport_dial P((const struct ssysteminfo *qsys,
			     int *pcproto_params,
			     struct sproto_param **pqproto_params,
			     int *pireliable));

/* Read from a port.
   zbuf -- buffer to read bytes into
   *pclen on call -- length of zbuf
   *pclen on successful return -- number of bytes read
   cmin -- minimum number of bytes to read before returning ahead of timeout
   ctimeout -- timeout in seconds, 0 if none
   freport -- whether to report errors.  */
extern boolean fport_read P((char *zbuf, int *pclen, int cmin,
			     int ctimeout, boolean freport));

/* Write to a port.  */
extern boolean fport_write P((const char *zbuf, int cbytes));

/* Read and write to a port.  This reads and writes data until either
   all passed-in data has been written or the read buffer is full.

   zwrite -- buffer to write bytes from
   *pcwrite on call -- number of bytes to write
   *pcwrite on successful return -- number of bytes written
   zread -- buffer to read bytes into
   *pcread on call -- size of read buffer
   *pcread on successful return -- number of bytes read.  */
extern boolean fport_io P((const char *zwrite, int *pcwrite,
			   char *zread, int *pcread));

/* Send a break character to a port.  */
extern boolean fport_break P((void));

/* Change the settings of a port.  */
extern boolean fport_set P((enum tportsetting tset));

/* Get the baud rate of a port.  */
extern long iport_baud P((void));

/* Do a chat script with a system.  */
extern boolean fchat P((const struct schat_info *qchat,
			const struct ssysteminfo *qsys,
			const struct sdialer *qdial,
			const char *zphone, boolean ftranslate,
			const char *zport, long ibaud));

/* Allow no carrier during a chat script.  */
extern boolean fport_no_carrier P((void));

/* Require carrier during a chat script.  */
extern boolean fport_need_carrier P((void));

/* Run a chat program on a port.  */
extern boolean fport_run_chat P((const char *zprog));

/* Modem routines used when dialing.  */

/* Begin dialing out.  This should open the dialer device if there is
   one, toggle DTR if requested and possible, and tell the port to
   ignore carrier.  It should return FALSE on error.  */
extern boolean fsysdep_modem_begin_dial P((struct sport *qport,
					   struct sdialer *qdial));

/* Tell the modem to ignore carrier.  This is called when \M is
   encountered in a dialer chat script.  It should return FALSE on
   error.  */
extern boolean fsysdep_modem_no_carrier P((struct sport *qport));

/* Tell the modem that carrier is required.  This is called when \m is
   encountered in a dialer chat script.  It should return FALSE on
   error.  */
extern boolean fsysdep_modem_need_carrier P((struct sport *qport));

/* Finish dialing out on a modem.  This should close the dialer device
   if there is one.  If the dialer and the port both support carrier,
   the port should be told to pay attention to carrier.  If it is
   possible to wait for carrier to come on, and the dialer and the
   port both the port support carrier, it should wait until carrier
   comes on.  */
extern boolean fsysdep_modem_end_dial P((struct sport *qport,
					 struct sdialer *qdial));

/* Port routines for stdin ports.  */

extern boolean fsysdep_stdin_lock P((struct sport *qport, boolean fin));
extern boolean fsysdep_stdin_open P((struct sport *qport, long ibaud,
				     boolean fwait));
extern boolean fsysdep_stdin_close P((struct sport *qport,
				      boolean fsuccess));
extern boolean fsysdep_stdin_reset P((struct sport *qport));
extern boolean fsysdep_stdin_read P((struct sport *qport, char *zread,
				     int *pcread, int cmin,
				     int ctimeout, boolean freport));
extern boolean fsysdep_stdin_write P((struct sport *qport,
				      const char *zwrite, int cwrite));
extern boolean fsysdep_stdin_io P((struct sport *qport,
				   const char *zwrite, int *pcwrite,
				   char *zread, int *pcread));
extern boolean fsysdep_stdin_break P((struct sport *qport));
extern boolean fsysdep_stdin_set P((struct sport *qport,
				    enum tportsetting tset));
extern boolean fsysdep_stdin_chat P((struct sport *qport,
				     const char *zprog));
extern long isysdep_stdin_baud P((struct sport *qport));

/* Port routines for modem ports.  */

extern boolean fsysdep_modem_lock P((struct sport *qport, boolean fin));
extern boolean fsysdep_modem_open P((struct sport *qport, long ibaud,
				     boolean fwait));
extern boolean fsysdep_modem_close P((struct sport *qport,
				      boolean fsuccess));
extern boolean fsysdep_modem_reset P((struct sport *qport));
extern boolean fmodem_dial P((struct sport *qport,
			      const struct ssysteminfo *qsys,
			      int *pcproto_params,
			      struct sproto_param **pqproto_params,
			      int *pireliable));
extern boolean fsysdep_modem_read P((struct sport *qport, char *zread,
				     int *pcread, int cmin,
				     int ctimeout, boolean freport));
extern boolean fsysdep_modem_write P((struct sport *qport,
				      const char *zwrite, int cwrite));
extern boolean fsysdep_modem_io P((struct sport *qport,
				   const char *zwrite, int *pcwrite,
				   char *zread, int *pcread));
extern boolean fsysdep_modem_break P((struct sport *qport));
extern boolean fsysdep_modem_set P((struct sport *qport,
				    enum tportsetting tset));
extern boolean fsysdep_modem_chat P((struct sport *qport,
				     const char *zprog));
extern long isysdep_modem_baud P((struct sport *qport));

/* Port routines for direct connections.  */

extern boolean fsysdep_direct_lock P((struct sport *qport, boolean fin));
extern boolean fsysdep_direct_open P((struct sport *qport, long ibaud,
				      boolean fwait));
extern boolean fsysdep_direct_close P((struct sport *qport,
				       boolean fsuccess));
extern boolean fsysdep_direct_reset P((struct sport *qport));
extern boolean fsysdep_direct_read P((struct sport *qport, char *zread,
				      int *pcread, int cmin,
				      int ctimeout, boolean freport));
extern boolean fsysdep_direct_write P((struct sport *qport,
				       const char *zwrite, int cwrite));
extern boolean fsysdep_direct_io P((struct sport *qport,
				    const char *zwrite, int *pcwrite,
				    char *zread, int *pcread));
extern boolean fsysdep_direct_break P((struct sport *qport));
extern boolean fsysdep_direct_set P((struct sport *qport,
				     enum tportsetting tset));
extern boolean fsysdep_direct_chat P((struct sport *qport,
				      const char *zprog));
extern long isysdep_direct_baud P((struct sport *qport));

#if HAVE_TCP

/* Port routines for TCP ports.  */

extern boolean ftcp_lock P((struct sport *qport, boolean fin));
extern boolean ftcp_open P((struct sport *qport, long ibaud,
			    boolean fwait));
extern boolean ftcp_close P((struct sport *qport, boolean fsuccess));
extern boolean ftcp_reset P((struct sport *qport));
extern boolean ftcp_dial P((struct sport *qport,
			    const struct ssysteminfo *qsys,
			    int *pcproto_params,
			    struct sproto_param **pqproto_params,
			    int *pireliable));
extern boolean fsysdep_tcp_read P((struct sport *qport, char *zread,
				   int *pcread, int cmin,
				   int ctimeout, boolean freport));
extern boolean fsysdep_tcp_write P((struct sport *qport,
				    const char *zwrite, int cwrite));
extern boolean fsysdep_tcp_io P((struct sport *qport,
				 const char *zwrite, int *pcwrite,
				 char *zread, int *pcread));
extern boolean fsysdep_tcp_chat P((struct sport *qport,
				   const char *zprog));
extern long itcp_baud P((struct sport *qport));

#endif /* HAVE_TCP */

#endif
