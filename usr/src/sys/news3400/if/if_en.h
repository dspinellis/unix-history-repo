/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: if_en.h,v 4.300 91/06/09 06:25:56 root Rel41 $ SONY
 *
 *	@(#)if_en.h	7.2 (Berkeley) %G%
 */

/*
 * Structure of an Ethernet header -- receive format
 */
struct en_rheader {
	u_char	enr_dhost[6];		/* Destination Host */
	u_char	enr_shost[6];		/* Source Host */
	u_short	enr_type;		/* Type of packet */
};

#ifdef KERNEL
/*
 * Ethernet software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * es_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 * We also have, for each interface, a IOP interface structure, which
 * contains information about the IOP resources held by the interface:
 * map registers, buffered data paths, etc.  Information is cached in this
 * structure for use by the if_iop.c routines in running the interface
 * efficiently.
 */
struct	en_softc {
	struct	arpcom es_ac;		/* Ethernet common part */
	struct	ifnews es_ifnews;	/* IOP resources */
	int	es_flags;
#ifdef NOTDEF /* KU:XXX */
	int	es_oactive;		/* # of active output buffers */
#endif
	int	es_interval;		/* interval of watchdog */
	caddr_t	es_bpf;
};

#define	es_if	es_ac.ac_if		/* network-visible interface */
#define	es_addr	es_ac.ac_enaddr		/* hardware Ethernet address */

#define	ENF_RUNNING	0x01		/* board is running */
#define	ENF_SETADDR	0x02		/* physical address is changed */

#define	ENWATCHINTERVAL	60		/* once every 60 seconds */

#endif /* KERNEL */
