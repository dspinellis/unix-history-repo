/*
 * HP display types
 */

#ifndef __HPDISPLAYS_H__
#define __HPDISPLAYS_H__

/* allowed types for the gcid */

#define GCID_GATORBOX		8
#define GCID_TOPCAT		9
#define GCID_CATSEYE		9
#define GCID_RENAISSANCE	10
#define GCID_FIREEYE		11
#define GCID_HYPERION		12
#define GCID_DAVINCI		14

/* hardware ids  -- to distinguish topcats/catseyes */

#define ID2_TC  2
#define ID2_LCC 5
#define ID2_HRC 6
#define ID2_HRM 7
#define ID2_CC  9

#ifdef __STDC__
#define VOLATILE volatile
#else
#ifdef __GNUC__
#define VOLATILE __volatile
#else
#define VOLATILE
#endif
#endif

#define vu_char VOLATILE u_char

struct hp_grfreg {
	char	gr_pad0;
	vu_char	gr_id;		/* +0x01 */
	char	gr_pad1[0x13];
	vu_char	gr_id2;		/* +0x15 */
	char	gr_pad2;
	vu_char	gr_bits;	/* +0x17 */
};

#define getGpHardware(pScreen) \
    ((struct hp_grfreg *) (getPrivScreenPtr(pScreen)->pHardwareScreen))

#endif
