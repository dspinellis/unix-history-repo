/*******************************************************************
*                                                                  *
*    File: CIFPLOT/alloc3.c                                        *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

#include <stdio.h>
#include "defs.h"
#include "globals.h"
#include "parser_defs.h"
#include "structs.h"
#include "out_structs.h"

IMPORT GetQueue();
IMPORT PutQueue();
IMPORT InitQueue();
IMPORT sbrk();


#define REQUESTSIZE 64

Queue FreeLItems,FreeLTrans,FreeLIEdges,FreeLDesc,FreeLComm,FreeLEdges;
Queue FreeLText,FreeHolders;

int acnt0,acnt1,acnt2,acnt3,acnt4,acnt5,acnt6,acnt7,acnt8,acnt9;
int bcnt0,bcnt1,bcnt2,bcnt3,bcnt4,bcnt5,bcnt6,bcnt7,bcnt8,bcnt9;

InitGet()
{
    InitQueue(&FreeLItems);
    InitQueue(&FreeLTrans);
    InitQueue(&FreeLIEdges);
    InitQueue(&FreeLEdges);
    InitQueue(&FreeLDesc);
    InitQueue(&FreeLComm);
    InitQueue(&FreeLText);
    InitQueue(&FreeHolders);
    }

int *
Get(q,n,id)
Queue *q;
int n,id;
{
    int i,*d;
    char *p;
#ifdef ADEBUG
    switch(id) {
	case 0x1234de5c:
		acnt1++;
		break;
	case 0x1234ed9e:
		acnt2++;
		break;
	case 0x1235ed9e:
		acnt3++;
		break;
	case 0x123417e4:
		acnt4++;
		break;
	case 0x123417e3:
		acnt5++;
		break;
	case 0x12347ec7:
		acnt6++;
		break;
	case 0x1234c033:
		acnt7++;
		break;
	case 0x123477a8:
		acnt8++;
		break;
	default:
		acnt0++;
		break;
	}
#endif

    if((p=(char *) GetQueue(q)) == NIL) {
	p = (char *) sbrk(REQUESTSIZE*(sizeof(int) + n));
	if(((int) p) <= 0) Error("No more memory",INTERNAL);
	for(i=0; i < REQUESTSIZE * (sizeof(int) + n); i += (sizeof(int) + n))
	    PutQueue(&(p[i]),q);
	p = (char *) GetQueue(q);
	}
    d = (int *) p;
    *d = id;
    return(++d);
    }

nedge *
AllocHolder()
/* This is not handle by get because ID's are not used and
 * storage is returned by concating onto the end of the queue */
{
    nedge *p;
    int i;

    if((p = (nedge *) GetQueue(&FreeHolders)) == NIL) {
	p = (nedge *) sbrk(REQUESTSIZE*sizeof(nedge));
	if(((int) p) <= 0) Error("No more memory",INTERNAL);
	for(i=0; i<REQUESTSIZE; i++) {
		PutQueue(&(p[i]),&FreeHolders);
		}
	p = (nedge *) GetQueue(&FreeHolders);
	}
    return(p);
    }

FreeGet(p,q,id)
int *p;
Queue *q;
int id;
{
#ifdef ADEBUG
    switch(id) {
	case 0x1234de5c:
		bcnt1++;
		break;
	case 0x1234ed9e:
		bcnt2++;
		break;
	case 0x1235ed9e:
		bcnt3++;
		break;
	case 0x123417e4:
		bcnt4++;
		break;
	case 0x123417e3:
		bcnt5++;
		break;
	case 0x12347ec7:
		bcnt6++;
		break;
	case 0x1234c033:
		bcnt7++;
		break;
	case 0x123477a8:
		bcnt8++;
		break;
	default:
		bcnt0++;
		break;
	}
#endif
    if(*(--p) != id)
	Error("Bad ID number in Free",INTERNAL);
    PutQueue(p,q);
    }
