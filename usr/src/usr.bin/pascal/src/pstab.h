/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 *	@(#)pstab.h	8.1 (Berkeley) 6/6/93
 */

    /*
     *	subtypes within the stab type N_PC
     *
     *	subtypes N_PSO and N_PSOL are	.stabs	name,N_PC,0,subtype,checksum
     *	others subtypes are		.stabs	name,N_PC,0,subtype,line
     */
#define	N_PSO		0x1	/* source file name */
#define	N_PSOL		0x2	/* include file name */
#define	N_PGLABEL	0x3	/* global label */
#define	N_PGCONST	0x4	/* global constant */
#define	N_PGTYPE	0x5	/* global type */
#define	N_PGVAR		0x6	/* global variable */
#define	N_PGFUNC	0x7	/* global function */
#define	N_PGPROC	0x8	/* global procedure */
#define	N_PEFUNC	0x9	/* external function */
#define	N_PEPROC	0xa	/* external procedure */
#define	N_PLDATA	0xb	/* library variable */
#define	N_PLTEXT	0xc	/* library routine */

    /*
     *	checksums are used to check if included files have changed.
     *	we also use them to check that .o files are up to date with
     *	the libraries.
     *	if a checksum is less than the flag checksum,
     *	then the checksum (and therefore the .o file) is out of date.
     *	if a checksum is equal to the flag checksum,
     *	then no furthur checking of the checksum is done.
     *	this is for the 2nd and subsequent times a file is stabed.
     *	to declare a flag day, increment this value. (also be sure 
     *  to update this value in ../utilities/externs.awk)
     */
#define	N_FLAGCHECKSUM	1
