/* dgram.h - datagram (CL-mode TS) abstractions */

/* 
 * $Header: /f/osi/h/RCS/dgram.h,v 7.2 91/02/22 09:24:36 mrose Interim $
 *
 *
 * $Log:	dgram.h,v $
 * Revision 7.2  91/02/22  09:24:36  mrose
 * Interim 6.8
 * 
 * Revision 7.1  91/01/07  12:39:23  mrose
 * update
 * 
 * Revision 7.0  89/12/19  15:13:52  mrose
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


#ifndef	_DGRAM_
#define	_DGRAM_

#define	MAXDGRAM	8192


int	join_dgram_aux ();
int	read_dgram_socket ();
int	write_dgram_socket ();
int	close_dgram_socket ();
int	select_dgram_socket ();
int	check_dgram_socket ();

#endif
