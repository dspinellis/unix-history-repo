/*

Copyright (c) 1986, 1987 by Hewlett-Packard Company
Copyright (c) 1986, 1987 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

HEWLETT-PACKARD MAKES NO WARRANTY OF ANY KIND WITH REGARD
TO THIS SOFWARE, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
PURPOSE.  Hewlett-Packard shall not be liable for errors 
contained herein or direct, indirect, special, incidental or 
consequential damages in connection with the furnishing, 
performance, or use of this material.

This software is not subject to any license of the American
Telephone and Telegraph Company or of the Regents of the
University of California.

*/
/***********************************************************************
 *  file: hpbuf.h
 *
 *
 *  ******************************************************************
 *  *  (c) Copyright Hewlett-Packard Company, 1986.  All rights are  *
 *  *  reserved.  Copying or other reproduction of this program      *
 *  *  except for archival purposes is prohibited without prior      *
 *  *  written consent of Hewlett-Packard Company.		     *
 *  ******************************************************************
 *
 *  Program purpose -- include file for off-screen memory allocator
 *
 *		Hewlett Packard -- Corvallis Workstation Operation
 *		Project -- port of X to HP9000S300
 *		Dan Garfinkel -- MTS
 *              Bob Leichner -- MTS - port of allocator to X11
 *
 */

#include "X.h"

/*
 * Type definitions
 */

#ifndef _HPBUF_DEF
#define _HPBUF_DEF

typedef struct _cnode {
    struct _cnode * parent;     /* parent node */
    struct _cnode *c0, *c1, *c2;    /* pointers to either cnodes or XHPchunks */
    short t0, t1, t2;           /* types of c0, c1, and c2 */
                                /*   One of -- USED, UNUSED, CNODE */
} hpCnode;

typedef struct _chunk {
    short x, y;                 /* x, y position in the frame buffer */
    short h, w;                 /* height and width of chunk */
    struct _chunk * prev;       /* previous chunk in the linked list */
    struct _chunk * next;       /* next chunk in the linked list */
    hpCnode * parent;           /* parent of this chunk */
} hpChunk;

typedef struct {
    hpChunk * used;
    hpChunk * unused;
    int initialized;
    int firstcol;
} hpBufAllocInfo;
typedef hpBufAllocInfo *hpBufAllocInfoPtr;

extern hpChunk *hpBufAlloc();
extern void hpBufFree();
extern void hpBufAllocInit();

#endif _HPBUF_DEF
