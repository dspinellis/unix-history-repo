/*
 * Copyright (c) 1984, 1985, 1986, 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *      @(#)spp_debug.h	7.2 (Berkeley) %G%
 */

struct	spp_debug {
	u_long	sd_time;
	short	sd_act;
	short	sd_ostate;
	caddr_t	sd_cb;
	short	sd_req;
	struct	spidp sd_si;
	struct	sppcb sd_sp;
};

#define	SA_INPUT 	0
#define	SA_OUTPUT	1
#define	SA_USER		2
#define	SA_RESPOND	3
#define	SA_DROP		4

#ifdef SANAMES
char	*sanames[] =
    { "input", "output", "user", "respond", "drop" };
#endif

#define	SPP_NDEBUG 100
struct	spp_debug spp_debug[SPP_NDEBUG];
int	spp_debx;
