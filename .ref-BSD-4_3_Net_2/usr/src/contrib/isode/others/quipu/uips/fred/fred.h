/* fred.h - definitions for fred */

/* 
 * $Header: /f/osi/others/quipu/uips/fred/RCS/fred.h,v 7.12 91/03/09 11:54:52 mrose Exp $
 *
 *
 * $Log:	fred.h,v $
 * Revision 7.12  91/03/09  11:54:52  mrose
 * update
 * 
 * Revision 7.11  91/02/22  09:30:45  mrose
 * Interim 6.8
 * 
 * Revision 7.10  91/02/19  09:21:15  mrose
 * ufn
 * 
 * Revision 7.9  91/01/10  04:02:58  mrose
 * update
 * 
 * Revision 7.8  90/11/01  22:02:47  mrose
 * update
 * 
 * Revision 7.7  90/10/28  23:21:05  mrose
 * server
 * 
 * Revision 7.6  90/06/11  10:55:21  mrose
 * UFN
 * 
 * Revision 7.5  90/03/08  08:05:08  mrose
 * phone
 * 
 * Revision 7.4  90/01/16  20:43:29  mrose
 * last check-out
 * 
 * Revision 7.3  90/01/11  18:36:33  mrose
 * real-sync
 * 
 * Revision 7.2  89/12/14  18:49:03  mrose
 * KIS project
 * 
 * Revision 7.1  89/12/13  20:01:49  mrose
 * errfp
 * 
 * Revision 7.0  89/11/23  22:08:57  mrose
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


#include <stdio.h>
#include "general.h"
#include "manifest.h"
#include "tailor.h"

/*    MAIN */

extern int  interrupted;
extern int  oneshot;

extern	SFP	istat;
extern	SFP	qstat;

extern LLog _fred_log, *fred_log;


void	adios (), advise ();
int	ask (), getline ();
char   *strdup ();

/*    DATA */

extern int	bflag;
extern int	boundP;
extern int	debug;
extern int	fflag;
extern int	kflag;
extern int	mail;
extern int	nametype;
extern int	network;
extern int	phone;
extern int	query;
extern int	readonly;
extern int	soundex;
extern int	timelimit;
extern int	ufn_options;
extern int	verbose;
extern int	watch;

extern int	didbind;
extern int	dontpage;
extern int	rcmode;
extern int	runcom;
extern int	runsys;

extern char    *manager;
extern char    *pager;
extern char    *server;

extern char    *myarea;
extern char    *mydn;
extern char    *myhome;
extern char    *myuser;


extern FILE    *stdfp;
extern FILE    *errfp;


#define	EOLN	(network && !mail ? "\r\n" : "\n")

/*    DISPATCH */

struct dispatch {
    char   *ds_name;
    IFP	    ds_fnx;

    int	    ds_flags;
#define	DS_NULL	0x00
#define	DS_USER	0x01
#define	DS_SYOK	0x02

    char   *ds_help;
};

/*    MISCELLANY */

struct area_guide {
    int	    ag_record;
#define	W_ORGANIZATION	0x01
#define	W_UNIT		0x02
#define	W_LOCALITY	0x03
#define	W_PERSON	0x04
#define	W_DSA		0x05
#define	W_ROLE		0x06
    char   *ag_key;

    char   *ag_search;

    char   *ag_class;
    char   *ag_rdn;

    char   *ag_area;
};

extern int  area_quantum;
extern struct area_guide areas[];

/*    WHOIS */

extern char *whois_help[];

/*  */

extern int  errno;

