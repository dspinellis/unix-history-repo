/* pingpong.c - listen and call out at the same time */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/pingpong/RCS/pingpong.c,v 7.1 91/02/22 09:28:52 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/pingpong/RCS/pingpong.c,v 7.1 91/02/22 09:28:52 mrose Interim $
 *
 *
 * $Log:	pingpong.c,v $
 * Revision 7.1  91/02/22  09:28:52  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:01:11  mrose
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

#include <stdio.h>
#include <isode/isoaddrs.h>
#include <isode/tsap.h>

struct PSAPaddr * pingaddr;
struct PSAPaddr * pongaddr;
int cn_state = NOTOK;
int cn_sd = NOTOK;

fd_set	wfds, rfds;
int	nfds = 0;
int	doneit = 0;
int	retry = 1;

main (argc,argv)
int argc;
char ** argv;
{

	do_args (argc,argv);

	start_listener ();

	printf ("Started to listen\n");

	ping_address ();

	wait_for_result ();

	printf ("Got Result\n");

	stop_nicely ();
}

do_args (argc,argv)
int argc;
char ** argv;
{
	char * myname;

	if (myname = rindex (argv[0], '/'))
		myname++;
	if (myname == NULL || *myname == NULL)
		myname = argv[0];

	if (argc != 3) {
		(void) fprintf (stderr,"Usage pingpong listen_address call_address\n");
		exit (-1);
	}

	isodetailor (myname, 0);

	if ((pongaddr = str2paddr (argv[1])) == NULLPA) {
		(void) fprintf (stderr,"Invalid listen address %s\n", argv[1]);
		exit (-1);
	}

	if ((pingaddr = str2paddr (argv[2])) == NULLPA) {
		(void) fprintf (stderr,"Invalid call address %s\n", argv[2]);
		exit (-1);
	}
}


start_listener ()
{
	struct TSAPdisconnect	  td_s;
	struct TSAPdisconnect	* td = &(td_s);

	if(TNetListen(&pongaddr->pa_addr.sa_addr, td) == NOTOK) {
		ts_advise ("TNetListen", td);
		exit (-2);
	}
}

ping_address ()
{
	struct TSAPstart tss;
	register struct TSAPstart *ts = &tss;
	struct TSAPdisconnect   tds;
	register struct TSAPdisconnect  *td = &tds;
	struct TSAPconnect tcs;
	struct TSAPconnect *tc = &tcs;

	cn_state = TAsynConnRequest (NULLTA, &pingaddr->pa_addr.sa_addr, 0,
				     NULLCP, ts -> ts_cc, &ts -> ts_qos,
				     tc, td, 1);

	cn_sd = tc -> tc_sd;
	printf ("Starting ping on %d state ", cn_sd);
	updatemask ();
	if (cn_state == NOTOK)
		ts_advise ("TAsynConnRequest", td);
	else
		retry = 0;
}


wait_for_result ()
{
	int	vecp = 0;
	char    *vec[4];
	int	i;
	struct TSAPdisconnect	td_s;
	struct TSAPdisconnect	*td = &td_s;
	struct TSAPconnect tcs;
	struct TSAPconnect *tc = &tcs;
	struct TSAPstart tss;
	struct TSAPstart *ts = &tss;
	fd_set ifds, ofds;

	for (;;) {

		ifds = rfds;
		ofds = wfds;
		printf ("TNetAccept nfds=%d rfds=0x%x wfds=0x%x\n", nfds,
			rfds.fds_bits[0], wfds.fds_bits[0]);
		if(TNetAccept(&vecp, vec, nfds, &ifds, &ofds, NULLFD,
			      NOTOK, td) == NOTOK)
		{
			ts_advise ("TNetAccept failed", td);
			exit (-3);
		}
		if (retry)
			ping_address ();

		if (vecp > 0) {
			if (TInit (vecp, vec, ts, td) == NOTOK) {
				ts_advise ("TInit failed failed", td);
				exit (-1);
			}
			if (TConnResponse (ts->ts_sd, NULLTA, 0, NULLCP, 0,
					   &tc -> tc_qos, td) == NOTOK) {
				ts_advise ("TConnResponse", td);
				exit (-1);
			}
			printf ("Connection accepted on %d\n", ts -> ts_sd);
			FD_SET (ts -> ts_sd, &rfds);
			if (ts -> ts_sd >= nfds)
				nfds = ts -> ts_sd + 1;
		}

		for (i = 0; i < nfds; i++) {
			if (FD_ISSET (i, &ofds) || FD_ISSET (i, &ifds)) {
				if (i == cn_sd)
					progress_connection ();
				else {
					if (sink_data (i) != OK) {
						FD_CLR (i, &rfds);
						if (doneit ++ > 0)
							return;
					}
					
				}
			}
		}
		if (cn_state == DONE) {
			if (doneit ++ > 0)
				return;
			cn_state = NOTOK;
		}
	}
}

sink_data (sd)
int	sd;
{
	struct TSAPdisconnect tds;
	struct TSAPdisconnect *td = &tds;
	struct TSAPdata txs, *tx = &txs;
	
	if (TReadRequest (sd, tx, OK, td) == NOTOK) {
		if (td -> td_reason = DR_NORMAL)
			ts_advise ("Normal disconnection", td);
		else	ts_advise ("TReadRequest", td);
		return NOTOK;
	}
	TXFREE (tx);
	return OK;
}

progress_connection ()
{			
	struct TSAPdisconnect	td_s;
	struct TSAPdisconnect	*td = &td_s;
	struct TSAPconnect tcs;
	struct TSAPconnect *tc = &tcs;

	switch(cn_state)
	{
	    case CONNECTING_1:
		printf ("CONNECTING_1 -> ");
		cn_state = TAsynRetryRequest(cn_sd,tc,td);
		if (cn_state == NOTOK)
			ts_advise ("\nTAsynRetryRequest", td);
		updatemask ();
		break;
	    case CONNECTING_2:
		printf ("CONNECTING_2 -> ");
		cn_state = TAsynRetryRequest(cn_sd,tc,td);
		if (cn_state == NOTOK)
			ts_advise ("\nTAsynRetryRequest", td);
		updatemask();
		break;
	    case NOTOK:
		printf ("NOTOK\n");
		updatemask ();
		break;
	    case DONE:
		printf ("DONE->");
		updatemask ();
		break;
	    default:
		printf ("cn_state weird\n");
		exit (-4);
	}
}

stop_nicely ()
{
	struct TSAPdisconnect	  td_s;
	struct TSAPdisconnect	* td = &(td_s);

	(void) TNetClose (&pongaddr->pa_addr.sa_addr, td);
}


updatemask ()
{
	struct TSAPdisconnect	  td_s;
	struct TSAPdisconnect	* td = &(td_s);

	if (cn_sd != NOTOK) {
		FD_CLR (cn_sd, &rfds);
		FD_CLR (cn_sd, &wfds);
		if (cn_sd >= nfds)
			nfds = cn_sd + 1;
	}
	switch (cn_state) {
	    case NOTOK:
		printf ("NOTOK\n");
		break;

	    default:
		printf ("weird!\n");
		break;

	    case CONNECTING_1:
		printf ("CONNECTING_1\n");
		FD_SET (cn_sd, &wfds);
		break;

	    case CONNECTING_2:
		printf ("CONNECTING_2\n");
		FD_SET (cn_sd, &rfds);
		break;
	    case DONE:
		printf ("DONE\n");
		(void) TDiscRequest (cn_sd, NULLCP, 0, td);
		cn_sd = NOTOK;
		printf ("Disconnect sent\n");
		break;
	}
}

ts_advise (str, td)
char	*str;
struct TSAPdisconnect *td;
{
	if (td -> td_cc > 0)
		printf ("%s : %s [%*.*s]\n", str, TErrString (td -> td_reason),
			td -> td_cc, td -> td_cc, td -> td_data);
	else	printf ("%s : %s\n", str, TErrString (td -> td_reason));
}

