/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ozan Yigit.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extr.h	5.2 (Berkeley) %G%
 */

extern ndptr hashtab[];		/* hash table for macros etc.  */
extern char buf[];		/* push-back buffer	       */
extern char *bp;		/* first available character   */
extern char *endpbb;		/* end of push-back buffer     */
extern stae mstack[];		/* stack of m4 machine         */
extern char *ep;		/* first free char in strspace */
extern char *endest;		/* end of string space	       */
int sp; 			/* current m4  stack pointer   */
int fp; 			/* m4 call frame pointer       */
extern FILE *infile[];		/* input file stack (0=stdin)  */
extern FILE *outfile[];		/* diversion array(0=bitbucket)*/
extern FILE *active;		/* active output file pointer  */
extern char *m4temp;		/* filename for diversions     */
extern int ilevel;		/* input file stack pointer    */
extern int oindex;		/* diversion index..	       */
extern char *null;		/* as it says.. just a null..  */
extern char *m4wraps;		/* m4wrap string default..     */
extern char lquote;		/* left quote character  (`)   */
extern char rquote;		/* right quote character (')   */
extern char scommt;		/* start character for comment */
extern char ecommt;		/* end character for comment   */
