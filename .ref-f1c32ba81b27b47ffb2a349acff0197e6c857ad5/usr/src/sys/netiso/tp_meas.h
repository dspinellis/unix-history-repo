/***********************************************************
				Copyright IBM Corporation 1987

                      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of IBM not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * ARGO Project, Computer Sciences Dept., University of Wisconsin - Madison
 */
/*	@(#)tp_meas.h	7.4 (Berkeley) %G% */
#ifdef TP_PERF_MEAS
#define tpmeas(a, b, t, c, d, e) \
	Tpmeas((u_int)(a), (u_int)(b), t, (u_int)(c), (u_int)(d), (u_int)(e))

struct tp_Meas {
	int			tpm_tseq;
	u_char		tpm_kind;
	u_short 	tpm_ref;
	u_short		tpm_size;
	u_short		tpm_window;
	u_int		tpm_seq;
	struct timeval	tpm_time;
};

#define TPMEASN 4000
extern int tp_Measn;
extern struct tp_Meas tp_Meas[];

/*
 * the kinds of events for packet tracing are:
 */
#define TPtime_from_session	0x01
#define TPtime_to_session	0x02
#define TPtime_ack_rcvd		0x03 
#define TPtime_ack_sent		0x04
#define TPtime_from_ll		0x05
#define TPtime_to_ll		0x06
#define TPsbsend			0x07 
#define TPtime_open			0x08
#define TPtime_open_X		0x28 /* xtd format */
#define TPtime_close		0x09

#endif TP_PERF_MEAS
