/* sector5.h - VTPM: FSM sector 5 definitions */

/* 
 * $Header: /f/osi/vt/RCS/sector5.h,v 7.1 91/02/22 09:48:10 mrose Interim $
 *
 *
 * $Log:	sector5.h,v $
 * Revision 7.1  91/02/22  09:48:10  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:31:41  mrose
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


typedef struct expl_ptr {
#define NULLCOORD  -1
	int xval;	/* if they don't exist = NULLCOORD */
	int yval;
	int zval;
} EXPL_PTR;

#define NOBKTOK	   -1  		/* for token in S mode */

typedef struct bkq_content {
	int token_val; /* 0 initiator, 1 acceptor, 2 accChoice or nobktok */
	EXPL_PTR ExplPtr;
} BKQ_content;


typedef struct bkr_content {
	int token_val; /* 0 initiator, 1 acceptor or nobktok */
	EXPL_PTR ExplPtr;
} BKR_content;

typedef struct br_cnt {
	BKQ_content	BKQcont;
	BKR_content	BKRcont;
	EXPL_PTR 	ExPtr;
} BRcnt;

