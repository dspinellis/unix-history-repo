/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 *
 *	@(#)condevs.h	4.10 (Berkeley) %G%
 */

#include "uucp.h"
#include <errno.h>
#include <setjmp.h>
#include <signal.h>
#include <sgtty.h>
#ifdef VMSDTR	/* Modem control on vms(works dtr) */
#include <eunice/eunice.h>
#define TT$M_MODEM	0x00200000 /* These should be in a '.h' somewhere */
#define SS$_NORMAL	0x00000001
#define IO$_SETMODE	0x00000023
#define IO$_SENSEMODE	0x00000027
#endif

extern char devSel[];	/* name to pass to delock() in close */
extern int errno, next_fd;
extern jmp_buf Sjbuf;
extern void alarmtr();
int nulldev(), nodev(), Acuopn(), diropn(), dircls();

#ifdef DATAKIT
int dkopn();
#endif

#ifdef DN11
int dnopn(), dncls();
#endif

#ifdef HAYES
int hyspopn(), hystopn(), hyscls();
#endif

#ifdef HAYES2400
int hyspopn24(), hystopn24(), hyscls24();
#endif

#ifdef HAYESQ
int hysqopn(), hysqcls();  /* a version of hayes that doesn't use ret codes */
#endif

#ifdef NOVATION
int novopn(), novcls();
#endif

#ifdef CDS224
int cdsopn224(), cdscls224();
#endif

#ifdef DF02
int df2opn(), df2cls();
#endif

#ifdef DF112
int df12popn(), df12topn(), df12cls();
#endif

#ifdef PNET
int pnetopn();
#endif

#ifdef VENTEL
int ventopn(), ventcls();
#endif

#ifdef PENRIL
int penopn(), pencls();
#endif

#ifdef	UNETTCP
#define TO_ACTIVE	0
int unetopn(), unetcls();
#endif

#ifdef BSDTCP
int bsdtcpopn(), bsdtcpcls();
#endif

#ifdef VADIC
int vadopn(), vadcls();
#endif

#ifdef VA212
int va212opn(), va212cls();
#endif

#ifdef VA811S
int va811opn(), va811cls();
#endif

#ifdef VA820
int va820opn(), va820cls();
#endif

#ifdef	RVMACS
int rvmacsopn(), rvmacscls();
#endif

#ifdef	VMACS
int vmacsopn(), vmacscls();
#endif

#ifdef MICOM
int micopn(), miccls();
#endif

#ifdef SYTEK
int sykopn(), sykcls();
#endif

#ifdef ATT2224
int attopn(), attcls();
#endif
