/* 
 * Copyright (C) Dirk Husemann, Computer Science Department IV, 
 * 		 University of Erlangen-Nuremberg, Germany, 1990, 1991, 1992
 * Copyright (c) 1992   Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dll.h	7.1 (Berkeley) %G%
 */

/* 
 * We define the additional PRC_* codes in here
 */
#ifdef KERNEL
#ifndef PRC_IFUP
#define PRC_IFUP		   3
#endif
#define PRC_CONNECT_INDICATION     8
#define PRC_CONNECT_REQUEST        9
#define PRC_DISCONNECT_REQUEST     10
#define PRC_DISCONNECT_INDICATION  11
#define PRC_RESET_REQUEST          12
#endif

/*
 * Data link layer configuration --- basically a copy of the relevant parts
 * of x25config, implemented to become a little bit more network
 * layer independent. (Probably only used for casting et al.)
 */
struct dllconfig {
       u_short dllcfg_unused0:4,
               dllcfg_unused1:4,
               dllcfg_trace:1,     /* link level tracing flag */
               dllcfg_window:7;    /* link level window size */
       u_short dllcfg_xchxid:1,    /* exchange XID (not yet) */
               dllcfg_unused2:7;   /* here be dragons */
};

struct dll_ctlinfo {
	union {
		struct {
			struct	dllconfig *dctli_up_cfg;
			u_char	dctli_up_lsap;
		} CTLI_UP;
		struct {
			caddr_t dctli_down_pcb;
			struct rtentry *dctli_down_rt;
			struct dllconfig *dctli_down_llconf;
		} CTLI_DOWN;
	} CTLIun;
};
#define dlcti_cfg  CTLIun.CTLI_UP.dctli_up_cfg
#define dlcti_lsap CTLIun.CTLI_UP.dctli_up_lsap
#define dlcti_pcb  CTLIun.CTLI_DOWN.dctli_down_pcb
#define dlcti_rt   CTLIun.CTLI_DOWN.dctli_down_rt
#define dlcti_conf CTLIun.CTLI_DOWN.dctli_down_llconf
