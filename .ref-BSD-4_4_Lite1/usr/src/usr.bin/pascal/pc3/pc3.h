/*-
 * Copyright (c) 1980, 1982, 1983, 1993
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
 *	@(#)pc3.h	8.1 (Berkeley) 6/6/93
 */

    /*
     *	a symbol table entry.
     */
struct symbol {
    char		*name;			/* pointer to string table */
    short		desc;			/* symbol description */
    int			lookup;			/* whether new or old */
    struct symbol	*fromp;			/* its defining .p file */
    union {					/* either */
	struct {				/*   for a symbol, */
	    struct symbol	*fromi;		/*     its defining .i file */
	    long		iline;		/*     the .i file line */
	    struct symbol	*rfilep;	/*     its resolving file */
	    long		rline;		/*     resolving file line */
	}		sym_str;
	long		checksum;		/*   for a file, its checksum */
    }			sym_un;
};

    /*
     *	struct for an argument .o file.
     */
struct fileinfo {
    FILE		*file;
    char		*name;
    off_t		nextoffset;
};

    /*
     *	old archive magic for error detection.
     */
#define	OARMAG	0177545

    /*
     *	this is used to trim pointers into the range of a mod of a prime.
     */
#define	SHORT_ABS( n )	( n & 077777 )

    /*
     *	a prime number which gets sizeof( struct symboltableinfo )
     *	up to a multiple of BUFSIZ.
     */
#define	SYMBOLPRIME	1021

    /*
     *	number of entries used in this symbol table,
     *	a chain to the next symbol table,
     *	and the entries. (pointers to struct symbols.)
     */
struct symboltableinfo {
    long			used;
    struct symboltableinfo	*chain;
    struct symbol		*entry[ SYMBOLPRIME ];
};

    /*
     *	if new struct symbols are needed,
     *	allocate this much space and hack it up into struct symbols.
     */
#define	SYMBOLALLOC	BUFSIZ

    /*
     *	a prime number which gets sizeof( struct stringtableinfo )
     *	up to a multiple of BUFSIZ.
     */
#define	STRINGPRIME	1021

    /*
     *	number of entries used in this string table,
     *	a chain to the next string table,
     *	and the entries. (pointers to the character table.)
     */
struct stringtableinfo {
    long			used;
    struct stringtableinfo	*chain;
    char			*entry[ STRINGPRIME ];
};

    /*
     *	if more character table space is needed,
     *	allocate this much and hack it up into strings.
     */
#define	CHARALLOC	BUFSIZ

    /*
     *	uninitialized pointer
     */
#define	NIL	0

    /*
     *	an enumeration for error types
     */
#define	NONE	0
#define	WARNING	1
#define ERROR	2
#define	FATAL	3

    /*
     *	an enumeration for lookups
     */
#define	NEW	0
#define	OLD	1

    /*
     *	booleans
     */
#define	BOOL	int
#define	FALSE	0
#define	TRUE	1

    /*
     *	function types.
     */
struct symbol	*entersymbol();
struct symbol	*symbolalloc();
long		stringhash();
char		*enterstring();
char		*charalloc();
BOOL		nextelement();
time_t		mtime();
char		*classify();
char		*article();
