/*
 * Copyright (c) 1984, 1985, 1986, 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)ns_proto.c	7.4 (Berkeley) 6/28/90
 */

#include "param.h"
#include "socket.h"
#include "protosw.h"
#include "domain.h"
#include "mbuf.h"

#include "ns.h"

/*
 * NS protocol family: IDP, ERR, PE, SPP, ROUTE.
 */
int	ns_init();
int	idp_input(), idp_output(), idp_ctlinput(), idp_usrreq();
int	idp_raw_usrreq(), idp_ctloutput();
int	spp_input(), spp_ctlinput();
int	spp_usrreq(), spp_usrreq_sp(), spp_ctloutput();
int	spp_init(), spp_fasttimo(), spp_slowtimo();
extern	int raw_usrreq();

extern	struct domain nsdomain;

struct protosw nssw[] = {
{ 0,		&nsdomain,	0,		0,
  0,		idp_output,	0,		0,
  0,
  ns_init,	0,		0,		0,
},
{ SOCK_DGRAM,	&nsdomain,	0,		PR_ATOMIC|PR_ADDR,
  0,		0,		idp_ctlinput,	idp_ctloutput,
  idp_usrreq,
  0,		0,		0,		0,
},
{ SOCK_STREAM,	&nsdomain,	NSPROTO_SPP,	PR_CONNREQUIRED|PR_WANTRCVD,
  spp_input,	0,		spp_ctlinput,	spp_ctloutput,
  spp_usrreq,
  spp_init,	spp_fasttimo,	spp_slowtimo,	0,
},
{ SOCK_SEQPACKET,&nsdomain,	NSPROTO_SPP,	PR_CONNREQUIRED|PR_WANTRCVD|PR_ATOMIC,
  spp_input,	0,		spp_ctlinput,	spp_ctloutput,
  spp_usrreq_sp,
  0,		0,		0,		0,
},
{ SOCK_RAW,	&nsdomain,	NSPROTO_RAW,	PR_ATOMIC|PR_ADDR,
  idp_input,	idp_output,	0,		idp_ctloutput,
  idp_raw_usrreq,
  0,		0,		0,		0,
},
{ SOCK_RAW,	&nsdomain,	NSPROTO_ERROR,	PR_ATOMIC|PR_ADDR,
  idp_ctlinput,	idp_output,	0,		idp_ctloutput,
  idp_raw_usrreq,
  0,		0,		0,		0,
},
};

struct domain nsdomain =
    { AF_NS, "network systems", 0, 0, 0, 
      nssw, &nssw[sizeof(nssw)/sizeof(nssw[0])] };

