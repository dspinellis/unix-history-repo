/*
 * various control values used by GAP
 * $Header: gapcontrols.h,v 2.0 85/11/21 07:23:02 jqj Exp $
 *
 * $Log:	gapcontrols.h,v $
 * Revision 2.0  85/11/21  07:23:02  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.2  85/11/20  13:59:16  jqj
 * added symbolic entries for Gap connection types
 * 
 */

/*
 * some of the service types used by GAP TTY services
 * to identify themselves.
 */

#define TTYService_any 0	/* matches any service */
#define TTYService_sa 1		/* Used by system administrator interface */
#define TTYService_exec 2	/* Used by Mesa and other TTY executives */
#define TTYService_its 3	/* Used by ITS */
#define TTYService_sender 4	/* Diagnostic service...sends continuously */
	/* Numbers above 64K for experimental and non-Xerox use. */

/*
 * Controls used in GAP connection streams.  In-band controls appear
 * as nonstandard SPP packet types.  Out-of-band controls appear as
 * Attn packets with a single byte of data, whose value is the OOB
 * control.
 */

/* generic controls, both in-band and out-of-band */

#define GAPCTLabortGetTransaction 0306	/* oob with mark */
#define GAPCTLabortMark 0315		/* inband */
#define GAPCTLabortPutTransaction 0307	/* oob with mark */
#define GAPCTLareYouThere 0304		/* oob */
#define GAPCTLaudibleSignal 0303	/* oob */
#define GAPCTLcleanup 0320		/* inband, oob */
#define GAPCTLdisconnect 0312		/* inband */
#define GAPCTLendOfTransaction 0310	/* inband */
#define GAPCTLendOfTransparentData 0314	/* inband */
#define GAPCTLexcessiveRetransmissions 0333 /* oob with mark */
#define GAPCTLiAmHere 0305		/* oob */
#define GAPCTLinterrupt 0301		/* oob */
#define GAPCTLnone 0300			/* inband, of course */
#define GAPCTLremoteNotResponding 0331	/* oob with mark */
#define GAPCTLresume 0302		/* oob */
#define GAPCTLtransparentDataFollows 0313 /* inband */
#define GAPCTLyourTurnToSend 0311	/* inband */
#define GAPCTLunchained3270 0335	/* inband */
#define GAPCTLreadModified3270 0336	/* inband */
#define GAPCTLstatus3270 0337		/* inband */
#define GAPCTLtestRequest3270 0340	/* inband */

#define GAPCTLsscpData 0345		/* inband? */
#define GAPCTLreadModifiedAll3270 0344	/* inband? */
#define GAPCTLread3270 0345		/* inband? */


/* status bytes -- all are oob */

#define GAPCTLmediumDown 0322
#define GAPCTLmediumUp 0321
#define GAPCTLnoGetForData 0325
#define GAPCTLourAccessIDRejected 0323
#define GAPCTLunsupportedProtocolFeature 0326
#define GAPCTLunexpectedRemoteBeharior 0327
#define GAPCTLunexpectedSoftwareFailure 0330
#define GAPCTLweRejectedAccessID 0324
#define GAPCTLpuActive 0347
#define GAPCTLpuInactive 0359
