/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ozan Yigit.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)extr.h	5.2 (Berkeley) 6/1/90
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
