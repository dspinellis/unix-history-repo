/* mpkt.h - defines the report format for management */

/* 
 * $Header: /f/osi/h/RCS/mpkt.h,v 7.1 91/02/22 09:24:50 mrose Interim $
 *
 *
 * $Log:	mpkt.h,v $
 * Revision 7.1  91/02/22  09:24:50  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:55:50  mrose
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

#include "isoaddrs.h"


struct MReport {
    u_short type;
#define OPREQIN         1
#define OPREQOUT        2
#define USERDT          3
#define USERDR          4
#define DISCREQ         5
#define PROTERR         6
#define CONGEST         7
#define CONFIGBAD       8
#define OPREQINBAD      9
#define OPREQOUTBAD     10
#define SOURCEADDR      11
#define	STARTLISTEN	12
#define	ENDLISTEN	13

    long    id;		/* process id */
    u_short cid;        /* connection fd */

    union {
	struct {
	    int a, b, c, d, e, f;
	} gp;

	struct {
	    int	    tsel_len;
	    char    tsel[TSSIZE];
	    struct NSAPaddr nsap;
	} taddr;
    } u;
};

int	TManGen ();
