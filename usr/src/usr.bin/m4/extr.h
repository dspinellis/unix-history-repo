/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ozan Yigit.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
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
