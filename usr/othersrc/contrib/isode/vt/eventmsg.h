/* eventmsg.h - VTPM: states */

/* 
 * $Header: /f/osi/vt/RCS/eventmsg.h,v 7.1 91/02/22 09:47:56 mrose Interim $
 *
 *
 * $Log:	eventmsg.h,v $
 * Revision 7.1  91/02/22  09:47:56  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:31:29  mrose
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


char	*eventname[] = {
	"* NOT USED *",
	"VASSreq",
	"VASSrsp",
	"VBRKreq",
	"VBRKrsp",
	"VDACKreq",
	"VDATreq_u",
	"VDATreq_h",
	"VDATreq_n",
	"VDATreq_sqtr",
	"VDELreq",
	"VENEGreq",
	"VENEGrsp",
	"VGVTreq",
	"VNACCreq",
	"VNINVreq",
	"VNOFFreq",
	"VNREJreq",
	"VRELreq",
	"VRELrsp",
	"VRQTreq",
	"VSNEQreq",
	"VSNEQrsp",
	"VSWPreq",
	"VSWPrsp",
	"VUABreq",
	"APQ",
	"ASQ",
	"ASR",
	"AUQ",
	"BKQ",
	"BKR",
	"DAQ",
	"DLQ",
	"EDQ",
	"ENQ",
	"ENR",
	"GTQ",
	"NAQ",
	"NDQ_ntr",
	"NDQ_tr",
	"NJQ",
	"NIQ",
	"NOQ",
	"RLQ",
	"RLR",
	"RTQ",
	"SPQ",
	"SPR",
	"SNQ",
	"SNR",
	"UDQ",
	"AUTO",
	"VTAB",
	"PAB",
	"VACKind",
	"VASSind",
	"VASScnf",
	"VBRKind",
	"VBRKcnf",
	"VDATind_u",
	"VDATind_h",
	"VDATind_n",
	"VDATind_Vnt",
	"VDELind",
	"VENEGind",
	"VENEGcnf",
	"VGVTind",
	"VNINVind",
	"VNOFFind",
	"VNACCind",
	"VNREJind",
	"VPABind",
	"VRELind",
	"VRELcnf",
	"VRQTind",
	"VSNEGind",
	"VSNEGcnf",
	"VSWPind",
	"VSWPcnf",
	"VUABind",
	"NDQseq_ntr",
	"NDQseq_tr",
	"NDQ_x_tr",
	"NDQ_x_ntr"
	 };
