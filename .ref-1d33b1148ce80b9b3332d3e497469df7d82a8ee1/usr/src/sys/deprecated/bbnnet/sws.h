#define SWS_QFF_MAX    100
#define SWS_QFF_MIN	 5	/* a guess */
#define SWS_QFF_DEC	20	/* applied to connection on ICMP quench */

/*
 * The following were determined by measurement.
 */
#define SWS_IMP		75
#define SWS_ETHER	65

/*
 * Should be for network most likely to be redirected toward.
 */
#define SWS_QFF_DEF	SWS_IMP
