/*	if_hy.h	4.1	83/02/20	*/

/*
 * Network Systems Corporation Hyperchannel
 *	routing database
 */

#define HYRSIZE  37	/* max number of adapters in routing tables */

struct hyroute {
	time_t	hyr_lasttime;		/* last update time */
	struct hy_hash {
		u_short hyr_flags;	/* status flags - see below */
		u_short hyr_key;		/* desired address */
		union {
			/*
			 * direct entry (can get there directly)
			 */
			struct {
				u_short hyru_dst;		/* adapter number & port */
				u_short hyru_ctl;		/* trunks to try */
				u_short hyru_access;	/* access code (mostly unused) */
			} hyr_d;
#define	hyr_dst		hyr_u.hyr_d.hyru_dst
#define	hyr_ctl		hyr_u.hyr_d.hyru_ctl
#define	hyr_access	hyr_u.hyr_d.hyru_access
			/*
			 * indirect entry (one or more hops required)
			 */
			struct {
				u_char hyru_pgate;	/* 1st gateway slot */
				u_char hyru_egate;	/* # gateways */
				u_char hyru_nextgate;	/* gateway to use next */
			} hyr_i;
#define	hyr_pgate	hyr_u.hyr_i.hyru_pgate
#define	hyr_egate	hyr_u.hyr_i.hyru_egate
#define	hyr_nextgate	hyr_u.hyr_i.hyru_nextgate
		} hyr_u;
	} hyr_hash[HYRSIZE];
	u_char hyr_gateway[256];
};

#ifdef KERNEL
struct hyroute hy_route[NHY];
#endif

#define HYR_INUSE	0x01	/* entry in use */
#define HYR_DIR		0x02	/* direct entry */
#define HYR_GATE	0x04	/* gateway entry */

#define HYRHASH(x) (((x) ^ ((x) >> 16)) % HYRSIZE)

#define HYSETROUTE	('H'<<8) | 0x80
#define HYGETROUTE	('H'<<8) | 0x81
