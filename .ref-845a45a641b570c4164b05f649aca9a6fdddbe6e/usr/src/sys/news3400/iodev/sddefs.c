/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: sddefs.c,v 4.300 91/06/09 06:38:25 root Rel41 $ SONY
 *
 *	@(#)sddefs.c	7.2 (Berkeley) %G%
 */

#include "sd.h"
#if NSD > 0

/*
 * Copyright (c) 1989- by SONY Corporation.
 */
/*
 * SD device configuration file
 */

#include <sys/param.h>
#include <news3400/iodev/sdreg.h>
#include <news3400/iodev/scsireg.h>
#include <news3400/iodev/scu.h>

char	revs_all[] = "????";

/*******************************/
/* disk partition informations */
/*******************************/

/* BEGIN sizes */
struct size wren3_101_sizes[PNUM] = {
	   15884,	     0,		/* A = cyl   0 thru  93 */
	   33440,      94*34*5,		/* B = cyl  94 thru 290 */
	  169150,	     0,		/* C = cyl   0 thru 994 */
	   15884,     291*34*5,		/* D = cyl 291 thru 384 */
	   55936,     385*34*5,		/* E = cyl 385 thru 714 */
	   47600,     715*34*5,		/* F = cyl 715 thru 994 */
	  119680,     291*34*5,		/* G = cyl 291 thru 994 */
	       0,	     0,		/* H	Not use		*/
	};
struct size wren3_101_hh_sizes[PNUM] = {
	   15884,	     0,		/* A =  15884 */
	   33440,	 15884,		/* B =  33440 */
	  178850,	     0,		/* C = 178850 */
	   15884,	 49324,		/* D =  15884 */
	   55936,	 65208,		/* E =  55936 */
	   57706,	121144,		/* F =  57706 */ /* F = C-(A+B+H+D+E) */
	  129526,	 49324,		/* G = 129526 */ /* G = D+E+F */
	  113642,	 65208,		/* H = 113642 */ /* H = E+F */
	};
struct size wren3_182_sizes[PNUM] = {
	   15884,	     0,		/* A = cyl   0 thru  51 */
	   33440,      52*34*9,		/* B = cyl  52 thru 161 */
	  304470,	     0,		/* C = cyl   0 thru 994 */
	   15884,     162*34*9,		/* D = cyl 162 thru 213 */
	   55936,     214*34*9,		/* E = cyl 214 thru 396 */
	  182988,     397*34*9,		/* F = cyl 397 thru 994 */
	  254898,     162*34*9,		/* G = cyl 162 thru 994 */
	       0,	     0,		/* H	Not use		*/
	};
/* END sizes */

/*************************/
/* device specifications */
/*************************/
struct sdst sdst_unknown =
 /*   ns, nt,  nspc,  ncyl, rps, xxx,	    sizes */
    {  1,  1,     1,     1, 60,  0,    calc_disk_sizes }; /* UNKNOWN_DISK */
struct sdst sdst_wren3_101 =
    { 34,  5, 34* 5,   995, 60,  0,    wren3_101_sizes }; /* WREN3_101 */
struct sdst sdst_wren3_101_hh =
    { 35,  5, 35* 5,  1022, 60,  0, wren3_101_hh_sizes }; /* WREN3_101_HH */
struct sdst sdst_wren3_182 =
    { 34,  9, 34* 9,   995, 60,  0,    wren3_182_sizes }; /* WREN3_182 */
struct sdst sdst_wren3_182_old =
    { 35,  9, 35* 9,   967, 60,  0,    wren3_182_sizes }; /* WREN3_182_OLD */
struct sdst sdst_wren4_286 =
    { 52,  9, 52* 9,  1365, 60,  0,    calc_disk_sizes }; /* WREN4_286 */
struct sdst sdst_wren5_170_hh =
    { 52,  5, 52* 5,  1544, 60,  0,    calc_disk_sizes }; /* WREN5_170_HH */
struct sdst sdst_wren6_415_hh =
    { 70,  7, 70* 7,  1756, 60,  0,    calc_disk_sizes }; /* WREN6_415_HH */
struct sdst sdst_st1480n =
    { 70,  9, 70* 9,  1476, 74,  0,    calc_disk_sizes }; /* ST1480N */
struct sdst sdst_dk515c_78 =
    { 69, 14, 69*14,  1356, 60,  0,    calc_disk_sizes }; /* DK515C_78 */
struct sdst sdst_dk516c_16 =
    { 81, 15, 81*15,  2165, 60,  0,    calc_disk_sizes }; /* DK516C_16 */
struct sdst sdst_dk312c_20 =
    { 38, 10, 38*10,  1076, 60,  0,    calc_disk_sizes }; /* DK312C_20 */
struct sdst sdst_dk312c_25 =
    { 38, 12, 38*12,  1076, 60,  0,    calc_disk_sizes }; /* DK312C_25 */
struct sdst sdst_xt_8760s =
    { 54, 15, 54*15,  1632, 60,  0,    calc_disk_sizes }; /* XT_8760S */
struct sdst sdst_srd2040z =
    { 33,  4, 33* 4,   608, 60,  0,    calc_disk_sizes }; /* SRD-2040Z */
struct sdst sdst_smo_s501 =
    { 31,  1, 31* 1, 18678, 40,  0,    calc_disk_sizes }; /* SMO_S501 */
struct sdst sdst_smo_s501_iso =
    { 31,  1, 31* 1, 18646, 40,  0,    calc_disk_sizes }; /* SMO_S501_ISO */

/***************************************************************/

/************************************/
/* normal Error Recovery Parameters */
/************************************/
char erp_wren3_old[] =					/* WREN3 old firmware */
    { SDM_PG_ERR, 0x06, SDM_DCR, 0, 0x08, 0, 0, 0xff };
char erp_wren3[] =					/* WREN3 */
    { SDM_PG_ERR, 0x06, SDM_AWRE|SDM_DCR, 0x09, 0x08, 0, 0, 0xff };
char erp_wren4567[] =					/* WREN4567 */
    { SDM_PG_ERR, 0x06, SDM_AWRE|SDM_DCR, 0x09, 0x0b, 0, 0, 0xff };
char erp_st1480n[] =					/* ST1480N */
    { SDM_PG_ERR, 0x0a, SDM_AWRE|SDM_DCR, 0x09, 0x0b, 0, 0, 0, 3, 0, 0xff, 0xff };
char erp_dk515c_78[] =					/* DK515C_78 */
    { SDM_PG_ERR, 0x06, SDM_DCR, 0x0a, 0, 0, 0, 0 };
char erp_dk516c_16[] =					/* DK516C_16 */
    { SDM_PG_ERR, 0x0a, SDM_DCR, 0x0a, 0, 0, 0, 0, 0x0a, 0, 0x01, 0xf4 };
char erp_dk312c_25[] =					/* DK312C_25 */
    { SDM_PG_ERR, 0x06, SDM_DCR, 0x0a, 0, 0, 0, 0 };
char erp_xt_8760s[] =					/* XT_8760S */
    { SDM_PG_ERR, 0x06, SDM_DCR, 0x03, 0x0b, 0, 0, 0xff };
char erp_srd2040z[] =					/* SRD-2040Z */
    { SDM_PG_ERR, 0x06, SDM_DCR, 0x03, 0x13, 0, 0, 0 };
char erp_smo_s501[] =					/* SMO_S501 */
    { SDM_PG_ERR, 0x06, SDM_AWRE, 0x02, 0, 0, 0, 0 };

/*********************************/
/* max Error Recovery Parameters */
/*********************************/
char max_erp_wren3_old[] =				/* WREN3 old firmware */
    { SDM_PG_ERR, 0x06, 0, 0x1b, 0x08, 0, 0, 0xff };
char max_erp_wren3[] =					/* WREN3 */
    { SDM_PG_ERR, 0x06, 0, 0x1b, 0x08, 0, 0, 0xff };
char max_erp_wren4567[] =				/* WREN4567 */
    { SDM_PG_ERR, 0x06, SDM_AWRE, 0x1b, 0x0b, 0, 0, 0xff };
char max_erp_st1480n[] =				/* ST1480N */
    { SDM_PG_ERR, 0x0a, SDM_AWRE, 0x1b, 0x0b, 0, 0, 0, 3, 0, 0xff, 0xff };
char max_erp_dk515c_78[] =				/* DK515C_78 */
    { SDM_PG_ERR, 0x06, 0, 0x10, 0, 0, 0, 0 };
char max_erp_dk516c_16[] =				/* DK516C_16 */
    { SDM_PG_ERR, 0x0a, 0, 0x10, 0, 0, 0, 0, 0x10, 0, 0xff, 0xff };
char max_erp_dk312c_25[] =				/* DK312C_25 */
    { SDM_PG_ERR, 0x06, 0, 0x10, 0, 0, 0, 0 };
char max_erp_xt_8760s[] =				/* XT_8760S */
    { SDM_PG_ERR, 0x06, 0, 0x1b, 0x0b, 0, 0, 0xff };
char max_erp_srd2040z[] =				/* SRD-2040Z */
    { SDM_PG_ERR, 0x06, SDM_ARRE|SDM_AWRE, 0x1b, 0x13, 0, 0, 0 };
char max_erp_smo_s501[] =				/* SMO_S501 */
    { SDM_PG_ERR, 0x06, SDM_AWRE, 0x10, 0, 0, 0, 0 };

/********************/
/* Other Parameters */
/********************/
char cache_off_wren4old[] =	/* WREN4 old */
    { SDM_PG_CACHE2, 0x0e, 0x01, 0xff, 0x34, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
char cache_on_wren4567[] =	/* WREN4567 */
    { SDM_PG_CACHE2, 0x0e, 0x11, 0xff, 0x34, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
char cache1_on_st1480n[] =	/* ST1480N */
    { SDM_PG_CACHE1, 0x0a, 0, 0, 0, 0, 0, 0, 0, 0x78, 0, 0 };
char cache2_on_st1480n[] =	/* ST1480N */
    { SDM_PG_CACHE2, 0x0e, 0x51, 0x00, 0x00, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
char cache_on_dk515c_78[] =	/* DK515C_78 */
    { SDM_PG_CACHE1, 0x0a, 0, 0, 0x02, 0, 0, 0, 0, 0x60, 0, 0x60 };
char modif_dk515c_78[] =	/* DK515C_78 */
    { SDM_PG_MODIFY, 0x0a, 0, 0, 0, 0, SDM_RING|0x01, SDM_ESDT, 0, 0, 0, 0 };
char cache_on_dk516c_16[] =	/* DK516C_16 */
    { SDM_PG_CACHE1, 0x0a, 0, 0, 0x08, 0, 0, 0, 0, 0x80, 0, 0x80 };
char modif_dk516c_16[] =	/* DK516C_16 */
    { SDM_PG_MODIFY, 0x1e, 0, 0, 0, 0, 4, SDM_STOD|SDM_ESDT, 0, 0, 0, 0x0e,
	0xff, 0, 0, 0, 0x50, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
char cache_on_dk312c[] =	/* DK312C_25 */
    { SDM_PG_CACHE1, 0x0a, 0, 0, 0x02, 0, 0, 0, 0, 0x60, 0, 0x60 };
char cache_off_dk312c[] =	/* DK312C_25 */
    { SDM_PG_CACHE1, 0x0a, 1, 0, 0x02, 0, 0, 0, 0, 0x60, 0, 0x60 };
char cache_on_xt_8760s[] =	/* XT_8760S */
    { SDM_PG_CACHE2, 0x0e, 0x11, 0xff, 0x59, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

/************** OTHER PAGES SPECIFICATIONS ********************/

char *other_wren4old[] =   { cache_off_wren4old, NULL };
char *other_wren4567[] =   { cache_on_wren4567,  NULL };
char *other_st1480n[] =    { cache1_on_st1480n, cache2_on_st1480n,  NULL };
char *other_dk515c_78[] =  { cache_on_dk515c_78, modif_dk515c_78, NULL };
char *other_dk516c_16[] =  { cache_on_dk516c_16, modif_dk516c_16, NULL };
char *other_dk312c_on[] =  { cache_on_dk312c,    modif_dk515c_78, NULL };
char *other_dk312c_off[] = { cache_off_dk312c,   modif_dk515c_78, NULL };
char *other_xt_8760s[] =   { cache_on_xt_8760s,  NULL };

/**************************************************************/

/*************************/
/*			 */
/* SD device information */
/*			 */
/*************************/
struct sddevinfo sddevinfo[] = {
    {					/* UNKNOWN DISK */
	1,				/* identify length */
	"*",				/* vendor & product ID */
	revs_all,			/* revision */
	-1,				/* drive capacity */
	"UNKNOWN",			/* device nickname */
	UNKNOWN_DISK,			/* device type code */
	0,				/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_unknown,			/* device specifications */
	NULL,				/* normal Error Recovery Parameters */
	NULL,				/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	FMT_DLF_PHYS,			/* Format options */
    },
    {					/* CDC WREN III 94161-5 */
	15,				/* identify length */
	"CDC     94161-5",		/* vendor & product ID */
	"6296",				/* revision */
	0x029509,			/* drive capacity */
	"hd101",			/* device nickname */
	WREN3_101,			/* device type code */
	FIRM_AWRE,			/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren3_101,		/* device specifications */
	erp_wren3,			/* normal Error Recovery Parameters */
	max_erp_wren3,			/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	0,				/* Format options */
    },
    {					/* CDC WREN III 94161-86 */
	16,				/* identify length */
	"CDC     94161-86",		/* vendor & product ID */
	"6296",				/* revision */
	0x029509,			/* drive capacity */
	"hd101",			/* device nickname */
	WREN3_101,			/* device type code */
	FIRM_AWRE,			/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren3_101,		/* device specifications */
	erp_wren3,			/* normal Error Recovery Parameters */
	max_erp_wren3,			/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	0,				/* Format options */
    },
    {					/* CDC WREN III 94161-9 */
	15,				/* identify length */
	"CDC     94161-9",		/* vendor & product ID */
	"6296",				/* revision */
	0x04a5dd,			/* drive capacity */
	"hd182",			/* device nickname */
	WREN3_182,			/* device type code */
	FIRM_AWRE,			/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren3_182,		/* device specifications */
	erp_wren3,			/* normal Error Recovery Parameters */
	max_erp_wren3,			/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	0,				/* Format options */
    },
    {					/* CDC WREN III 94161-155 */
	17,				/* identify length */
	"CDC     94161-155",		/* vendor & product ID */
	"6296",				/* revision */
	0x04a556,			/* drive capacity */
	"hd182",			/* device nickname */
	WREN3_182,			/* device type code */
	FIRM_AWRE,			/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren3_182,		/* device specifications */
	erp_wren3,			/* normal Error Recovery Parameters */
	max_erp_wren3,			/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	0,				/* Format options */
    },
    {					/* CDC WREN III 94161-155 */
	17,				/* identify length */
	"CDC     94161-155",		/* vendor & product ID */
	"6296",				/* revision */
	0x04a5dd,			/* drive capacity */
	"hd182",			/* device nickname */
	WREN3_182,			/* device type code */
	FIRM_AWRE,			/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren3_182,		/* device specifications */
	erp_wren3,			/* normal Error Recovery Parameters */
	max_erp_wren3,			/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	0,				/* Format options */
    },
    {					/* CDC WREN III 94161-156 */
	17,				/* identify length */
	"CDC     94161-156",		/* vendor & product ID */
	"6296",				/* revision */
	0x04a5dd,			/* drive capacity */
	"hd182",			/* device nickname */
	WREN3_182,			/* device type code */
	FIRM_AWRE,			/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren3_182,		/* device specifications */
	erp_wren3,			/* normal Error Recovery Parameters */
	max_erp_wren3,			/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	0,				/* Format options */
    },
    {					/* CDC WREN III 94161-5 */
	15,				/* identify length */
	"CDC     94161-5",		/* vendor & product ID */
	revs_all,			/* revision */
	0x029509,			/* drive capacity */
	"hd101",			/* device nickname */
	WREN3_101,			/* device type code */
	0,				/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren3_101,		/* device specifications */
	erp_wren3_old,			/* normal Error Recovery Parameters */
	max_erp_wren3_old,		/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	0,				/* Format options */
    },
    {					/* CDC WREN III 94161-86 */
	16,				/* identify length */
	"CDC     94161-86",		/* vendor & product ID */
	revs_all,			/* revision */
	0x029509,			/* drive capacity */
	"hd101",			/* device nickname */
	WREN3_101,			/* device type code */
	0,				/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren3_101,		/* device specifications */
	erp_wren3_old,			/* normal Error Recovery Parameters */
	max_erp_wren3_old,		/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	0,				/* Format options */
    },
    {					/* CDC WREN III 94161-9 */
	15,				/* identify length */
	"CDC     94161-9",		/* vendor & product ID */
	revs_all,			/* revision */
	0x04a5dd,			/* drive capacity */
	"hd182",			/* device nickname */
	WREN3_182,			/* device type code */
	0,				/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren3_182,		/* device specifications */
	erp_wren3_old,			/* normal Error Recovery Parameters */
	max_erp_wren3_old,		/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	0,				/* Format options */
    },
    {					/* CDC WREN III 94161-155 */
	17,				/* identify length */
	"CDC     94161-155",		/* vendor & product ID */
	revs_all,			/* revision */
	0x04a556,			/* drive capacity */
	"hd182",			/* device nickname */
	WREN3_182,			/* device type code */
	0,				/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren3_182,		/* device specifications */
	erp_wren3_old,			/* normal Error Recovery Parameters */
	max_erp_wren3_old,		/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	0,				/* Format options */
    },
    {					/* CDC WREN III 94161-155 */
	17,				/* identify length */
	"CDC     94161-155",		/* vendor & product ID */
	revs_all,			/* revision */
	0x04a5dd,			/* drive capacity */
	"hd182",			/* device nickname */
	WREN3_182,			/* device type code */
	0,				/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren3_182,		/* device specifications */
	erp_wren3_old,			/* normal Error Recovery Parameters */
	max_erp_wren3_old,		/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	0,				/* Format options */
    },
    {					/* CDC WREN III 94161-156 */
	17,				/* identify length */
	"CDC     94161-156",		/* vendor & product ID */
	revs_all,			/* revision */
	0x04a5dd,			/* drive capacity */
	"hd182",			/* device nickname */
	WREN3_182,			/* device type code */
	0,				/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren3_182,		/* device specifications */
	erp_wren3_old,			/* normal Error Recovery Parameters */
	max_erp_wren3_old,		/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	0,				/* Format options */
    },
    {					/* CDC WREN III HH 94211-5 */
	15,				/* identify length */
	"CDC     94211-5",		/* vendor & product ID */
	revs_all,			/* revision */
	0x02baa2,			/* drive capacity */
	"hd101_hh",			/* device nickname */
	WREN3_101_HH,			/* device type code */
	0,				/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren3_101_hh,		/* device specifications */
	erp_wren3_old,			/* normal Error Recovery Parameters */
	max_erp_wren3_old,		/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	0,				/* Format options */
    },
    {					/* CDC WREN III HH 94216-5 */
	15,				/* identify length */
	"CDC     94216-5",		/* vendor & product ID */
	revs_all,			/* revision */
	0x02baa2,			/* drive capacity */
	"hd101_hh",			/* device nickname */
	WREN3_101_HH,			/* device type code */
	FIRM_AWRE,			/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren3_101_hh,		/* device specifications */
	erp_wren3,			/* normal Error Recovery Parameters */
	max_erp_wren3,			/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	0,				/* Format options */
    },
    {					/* CDC WREN IV 94171-9 */
	15,				/* identify length */
	"CDC     94171-9",		/* vendor & product ID */
	"6981",				/* revision */
	0x08f40c,			/* drive capacity */
	"hd286",			/* device nickname */
	WREN4_286,			/* device type code */
	0,				/* firmware spec */
	0x4b,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren4_286,		/* device specifications */
	erp_wren4567,			/* normal Error Recovery Parameters */
	max_erp_wren4567,		/* max Error Recovery Parameters */
	other_wren4old,			/* Other Parameters */
	FMT_DLF_PHYS,			/* Format options */
    },
    {					/* CDC WREN IV 94171-9 */
	15,				/* identify length */
	"CDC     94171-9",		/* vendor & product ID */
	"8794",				/* revision */
	0x08f40c,			/* drive capacity */
	"hd286",			/* device nickname */
	WREN4_286,			/* device type code */
	0,				/* firmware spec */
	0x4b,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren4_286,		/* device specifications */
	erp_wren4567,			/* normal Error Recovery Parameters */
	max_erp_wren4567,		/* max Error Recovery Parameters */
	other_wren4old,			/* Other Parameters */
	FMT_DLF_PHYS,			/* Format options */
    },
    {					/* CDC WREN IV 94171-9 */
	15,				/* identify length */
	"CDC     94171-9",		/* vendor & product ID */
	"8814",				/* revision */
	0x08f40c,			/* drive capacity */
	"hd286",			/* device nickname */
	WREN4_286,			/* device type code */
	0,				/* firmware spec */
	0x4b,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren4_286,		/* device specifications */
	erp_wren4567,			/* normal Error Recovery Parameters */
	max_erp_wren4567,		/* max Error Recovery Parameters */
	other_wren4old,			/* Other Parameters */
	FMT_DLF_PHYS,			/* Format options */
    },
    {					/* CDC WREN IV 94171-9 */
	15,				/* identify length */
	"CDC     94171-9",		/* vendor & product ID */
	revs_all,			/* revision */
	0x08f40c,			/* drive capacity */
	"hd286",			/* device nickname */
	WREN4_286,			/* device type code */
	FIRM_CACHE_ON|FIRM_SYNCTR|FIRM_AWRE,	/* firmware spec */
	0x4b,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren4_286,		/* device specifications */
	erp_wren4567,			/* normal Error Recovery Parameters */
	max_erp_wren4567,		/* max Error Recovery Parameters */
	other_wren4567,			/* Other Parameters */
	FMT_DLF_PHYS,			/* Format options */
    },
    {					/* CDC WREN V HH 94221-5 */
	15,				/* identify length */
	"CDC     94221-5",		/* vendor & product ID */
	revs_all,			/* revision */
	0x05564a,			/* drive capacity */
	"hd170_hh",			/* device nickname */
	WREN5_170_HH,			/* device type code */
	FIRM_CACHE_ON|FIRM_SYNCTR|FIRM_AWRE,	/* firmware spec */
	0x3f,				/* MIN synchronous transfer period */
	15,				/* MAX synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren5_170_hh,		/* device specifications */
	erp_wren4567,			/* normal Error Recovery Parameters */
	max_erp_wren4567,		/* max Error Recovery Parameters */
	other_wren4567,			/* Other Parameters */
	0,				/* Format options */
    },
    {					/* IMPRIMIS WREN VI HH 94241-7 */
	15,				/* identify length */
	"IMPRIMIS94241-7",		/* vendor & product ID */
	"0207",				/* revision */
	0xcfa13,			/* drive capacity */
	"hd415_hh",			/* device nickname */
	WREN6_415_HH,			/* device type code */
	FIRM_CACHE_ON|FIRM_SYNCTR|FIRM_AWRE,	/* firmware spec */
	0x35,				/* MIN synchronous transfer period */
	15,				/* MAX synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_wren6_415_hh,		/* device specifications */
	erp_wren4567,			/* normal Error Recovery Parameters */
	max_erp_wren4567,		/* max Error Recovery Parameters */
	other_wren4567,			/* Other Parameters */
	FMT_DLF_PHYS,			/* Format options */
    },
    {					/* SEAGATE ST1480N */
	15,				/* identify length */
	"SEAGATE ST1480 ",		/* vendor & product ID */
	revs_all,			/* revision */
	0xcb40f,			/* drive capacity */
	"hd406",			/* device nickname */      /* ???? */
	ST1480N,			/* device type code */
	FIRM_CACHE_ON|FIRM_AWRE,	/* firmware spec */
	0x35,				/* MIN synchronous transfer period */
	0,				/* MAX synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_st1480n,			/* device specifications */
	erp_st1480n,			/* normal Error Recovery Parameters */
	max_erp_st1480n,		/* max Error Recovery Parameters */
	other_st1480n,			/* Other Parameters */
	FMT_DLF_PHYS,			/* Format options */
    },
    {					/* HITACHI DK515C-78 */
	14,				/* identify length */
	"HITACHI DK515C",		/* vendor & product ID */
	revs_all,			/* revision */
	0x13fcc8,			/* drive capacity */
	"hd639",			/* device nickname */
	DK515C_78,			/* device type code */
	FIRM_CACHE_ON|FIRM_SYNCTR,	/* firmware spec */
	0x3e,				/* MIN synchronous transfer period */
	8,				/* MAX synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_dk515c_78,		/* device specifications */
	erp_dk515c_78,			/* normal Error Recovery Parameters */
	max_erp_dk515c_78,		/* max Error Recovery Parameters */
	other_dk515c_78,		/* Other Parameters */
	FMT_DLF_BLK,			/* Format options */
    },
    {					/* HITACHI DK516C-16 */
	14,				/* identify length */
	"HITACHI DK516C",		/* vendor & product ID */
	revs_all,			/* revision */
	0x2800a4,			/* drive capacity */
	"hd1280",			/* device nickname */
	DK516C_16,			/* device type code */
	FIRM_CACHE_ON|FIRM_SYNCTR,	/* firmware spec */
	0x3e,				/* synchronous transfer period */
	15,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_dk516c_16,		/* device specifications */
	erp_dk516c_16,			/* normal Error Recovery Parameters */
	max_erp_dk516c_16,		/* max Error Recovery Parameters */
	other_dk516c_16,		/* Other Parameters */
	FMT_DLF_BLK,			/* Format options */
    },
    {					/* HITACHI DK312C-20 */
	14,				/* identify length */
	"HITACHI DK312C",		/* vendor & product ID */
	"CM73",				/* revision */
	0x063d30,			/* drive capacity */
	"hd199",			/* device nickname */
	DK312C_20,			/* device type code */
	FIRM_CACHE_ON|FIRM_SYNCTR,	/* firmware spec */
	0x3e,				/* MIN synchronous transfer period */
	7,				/* MAX synchronous transfer offset */
	0,				/* synchronous transfer register set */
	&sdst_dk312c_20,		/* device specifications */
	erp_dk312c_25,			/* normal Error Recovery Parameters */
	max_erp_dk312c_25,		/* max Error Recovery Parameters */
	other_dk312c_off,		/* Other Parameters */
	FMT_DLF_BLK,			/* Format options */
    },
    {					/* HITACHI DK312C-25 */
	14,				/* identify length */
	"HITACHI DK312C",		/* vendor & product ID */
	"CM73",				/* revision */
	0x077ca0,			/* drive capacity */
	"hd239",			/* device nickname */
	DK312C_25,			/* device type code */
	FIRM_CACHE_ON|FIRM_SYNCTR,	/* firmware spec */
	0x3e,				/* MIN synchronous transfer period */
	7,				/* MAX synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_dk312c_25,		/* device specifications */
	erp_dk312c_25,			/* normal Error Recovery Parameters */
	max_erp_dk312c_25,		/* max Error Recovery Parameters */
	other_dk312c_off,		/* Other Parameters */
	FMT_DLF_BLK,			/* Format options */
    },
    {					/* HITACHI DK312C-20 */
	14,				/* identify length */
	"HITACHI DK312C",		/* vendor & product ID */
	"CM76",				/* revision */
	0x063d30,			/* drive capacity */
	"hd199",			/* device nickname */
	DK312C_20,			/* device type code */
	FIRM_CACHE_ON|FIRM_SYNCTR,	/* firmware spec */
	0x3e,				/* MIN synchronous transfer period */
	7,				/* MAX synchronous transfer offset */
	0,				/* synchronous transfer register set */
	&sdst_dk312c_20,		/* device specifications */
	erp_dk312c_25,			/* normal Error Recovery Parameters */
	max_erp_dk312c_25,		/* max Error Recovery Parameters */
	other_dk312c_off,		/* Other Parameters */
	FMT_DLF_BLK,			/* Format options */
    },
    {					/* HITACHI DK312C-25 */
	14,				/* identify length */
	"HITACHI DK312C",		/* vendor & product ID */
	"CM76",				/* revision */
	0x077ca0,			/* drive capacity */
	"hd239",			/* device nickname */
	DK312C_25,			/* device type code */
	FIRM_CACHE_ON|FIRM_SYNCTR,	/* firmware spec */
	0x3e,				/* MIN synchronous transfer period */
	7,				/* MAX synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_dk312c_25,		/* device specifications */
	erp_dk312c_25,			/* normal Error Recovery Parameters */
	max_erp_dk312c_25,		/* max Error Recovery Parameters */
	other_dk312c_off,		/* Other Parameters */
	FMT_DLF_BLK,			/* Format options */
    },
    {					/* HITACHI DK312C-20 */
	14,				/* identify length */
	"HITACHI DK312C",		/* vendor & product ID */
	revs_all,			/* revision */
	0x063d30,			/* drive capacity */
	"hd199",			/* device nickname */
	DK312C_20,			/* device type code */
	FIRM_CACHE_ON|FIRM_SYNCTR,	/* firmware spec */
	0x3e,				/* MIN synchronous transfer period */
	7,				/* MAX synchronous transfer offset */
	0,				/* synchronous transfer register set */
	&sdst_dk312c_20,		/* device specifications */
	erp_dk312c_25,			/* normal Error Recovery Parameters */
	max_erp_dk312c_25,		/* max Error Recovery Parameters */
	other_dk312c_on,		/* Other Parameters */
	FMT_DLF_BLK,			/* Format options */
    },
    {					/* HITACHI DK312C-25 */
	14,				/* identify length */
	"HITACHI DK312C",		/* vendor & product ID */
	revs_all,			/* revision */
	0x077ca0,			/* drive capacity */
	"hd239",			/* device nickname */
	DK312C_25,			/* device type code */
	FIRM_CACHE_ON|FIRM_SYNCTR,	/* firmware spec */
	0x3e,				/* MIN synchronous transfer period */
	7,				/* MAX synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_dk312c_25,		/* device specifications */
	erp_dk312c_25,			/* normal Error Recovery Parameters */
	max_erp_dk312c_25,		/* max Error Recovery Parameters */
	other_dk312c_on,		/* Other Parameters */
	FMT_DLF_BLK,			/* Format options */
    },
    {					/* MAXTOR XT-8760S B5A */
	16,				/* identify length */
	"MAXTOR  XT-8760S",		/* vendor & product ID */
	"B5A ",				/* revision */
	0x13bc99,			/* drive capacity */
	"xt_8760s",			/* device nickname */
	XT_8760S,			/* device type code */
	FIRM_CACHE_ON|FIRM_SYNCTR,	/* firmware spec */
	0x35,				/* MIN synchronous transfer period */
	15,				/* MAX synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_xt_8760s,			/* device specifications */
	erp_xt_8760s,			/* normal Error Recovery Parameters */
	max_erp_xt_8760s,		/* max Error Recovery Parameters */
	other_xt_8760s,			/* Other Parameters */
	FMT_DLF_PHYS,			/* Format options */
    },
    {					/* MAXTOR XT-8760S B3C */
	16,				/* identify length */
	"MAXTOR  XT-8760S",		/* vendor & product ID */
	"B3C ",				/* revision */
	0x13bc99,			/* drive capacity */
	"xt_8760s",			/* device nickname */
	XT_8760S,			/* device type code */
	0,				/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_xt_8760s,			/* device specifications */
	erp_xt_8760s,			/* normal Error Recovery Parameters */
	max_erp_xt_8760s,		/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	FMT_DLF_PHYS,			/* Format options */
    },
    {					/* SONY SRD2040Z */
	16,				/* identify length */
	"SONY    SRD2040Z",		/* vendor & product ID */
	revs_all,			/* revision */
	0x014124,			/* drive capacity */
	"hd41",				/* device nickname */
	SRD_2040Z,			/* device type code */
	0,				/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_srd2040z,			/* device specifications */
	erp_srd2040z,			/* normal Error Recovery Parameters */
	max_erp_srd2040z,		/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	FMT_DLF_BLK,			/* Format options */
    },
    {					/* SONY SMO-S501 SONY format */
	16,				/* identify length */
	"SONY    SMO-C501",		/* vendor & product ID */
	"1.??",				/* revision */
	0x08d5c0,			/* drive capacity */
	"od282",			/* device nickname */
	SMO_S501,			/* device type code */
	FIRM_AWRE,			/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_smo_s501,			/* device specifications */
	erp_smo_s501,			/* normal Error Recovery Parameters */
	max_erp_smo_s501,		/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	FMT_DLF_PHYS,			/* Format options */
    },
    {					/* SONY SMO-S501 ISO format */
	16,				/* identify length */
	"SONY    SMO-C501",		/* vendor & product ID */
	"2.??",				/* revision */
	0x08cde7,			/* drive capacity */
	"od282iso",			/* device nickname */
	SMO_S501_ISO2,			/* device type code */
	FIRM_AWRE,			/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_smo_s501_iso,		/* device specifications */
	erp_smo_s501,			/* normal Error Recovery Parameters */
	max_erp_smo_s501,		/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	FMT_DLF_PHYS,			/* Format options */
    },
    {					/* SONY SMO-S501 ISO format */
	16,				/* identify length */
	"SONY    SMO-C501",		/* vendor & product ID */
	"2.??",				/* revision */
	0x08d1e7,			/* drive capacity */
	"od282i",			/* device nickname */
	SMO_S501_ISO,			/* device type code */
	FIRM_AWRE,			/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_smo_s501_iso,		/* device specifications */
	erp_smo_s501,			/* normal Error Recovery Parameters */
	max_erp_smo_s501,		/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	FMT_DLF_PHYS,			/* Format options */
    },
    {					/* SONY SMO-S501 SONY format */
	16,				/* identify length */
	"SONY    SMO-C501",		/* vendor & product ID */
	"2.??",				/* revision */
	0x08d5c0,			/* drive capacity */
	"od282",			/* device nickname */
	SMO_S501,			/* device type code */
	FIRM_AWRE,			/* firmware spec */
	0,				/* synchronous transfer period */
	0,				/* synchronous transfer offset */
	NULL,				/* device setup commands */
	&sdst_smo_s501,			/* device specifications */
	erp_smo_s501,			/* normal Error Recovery Parameters */
	max_erp_smo_s501,		/* max Error Recovery Parameters */
	NULL,				/* Other Parameters */
	FMT_DLF_PHYS,			/* Format options */
    },
    {					/* End of table */
	-1,
    }
};
#endif /* NSD > 0 */
