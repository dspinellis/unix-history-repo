/*	acutab.c	4.1	81/05/09	*/
#include "tip.h"

extern int df_dialer(), df_disconnect(), df_abort(),
	   bizf_dialer(), biz_disconnect(), biz_abort(),
	   bizw_dialer(), dn_dialer(), dn_disconnect(),
	   dn_abort();

acu_t acutable[] = {
#if BIZCOMP
	"bizf",	bizf_dialer,	biz_disconnect,		biz_abort,
	"bizw", bizw_dialer,	biz_disconnect,		biz_abort,
#endif
#if DF02
	"df02",	df_dialer,	df_disconnect,		df_abort,
#endif
#if DN11
	"dn11",	dn_dialer,	dn_disconnect,		dn_abort,
#endif
	0,	0,		0,			0
};

