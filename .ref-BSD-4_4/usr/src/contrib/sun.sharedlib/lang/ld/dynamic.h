/*
 * This source code is a product of Sun Microsystems, Inc. and is provided
 * for unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this source code without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * THIS PROGRAM CONTAINS SOURCE CODE COPYRIGHTED BY SUN MICROSYSTEMS, INC.
 * SUN MICROSYSTEMS, INC., MAKES NO REPRESENTATIONS ABOUT THE SUITABLITY
 * OF SUCH SOURCE CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT
 * EXPRESS OR IMPLIED WARRANTY OF ANY KIND.  SUN MICROSYSTEMS, INC. DISCLAIMS
 * ALL WARRANTIES WITH REGARD TO SUCH SOURCE CODE, INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN
 * NO EVENT SHALL SUN MICROSYSTEMS, INC. BE LIABLE FOR ANY SPECIAL, INDIRECT,
 * INCIDENTAL, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING
 * FROM USE OF SUCH SOURCE CODE, REGARDLESS OF THE THEORY OF LIABILITY.
 * 
 * This source code is provided with no support and without any obligation on
 * the part of Sun Microsystems, Inc. to assist in its use, correction, 
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS
 * SOURCE CODE OR ANY PART THEREOF.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California 94043
 */

/*      @(#)dynamic.h 1.12 69/12/31 SMI;   */

/* Copyright (c) 1991 by Sun Microsystems, Inc. */

#define	LHSIZ	31
#define RTHS 126
#define MAXLIB 126

struct libentry {
        char *addr;             /* address where lib is mapped to */
        char *name;             /* path name of library */
        struct libentry *next;
};

struct dynamic {
        struct	libentry *lib_entry;	/* ptr to list of libraries */
        int	ds;			/* size of data link table */
        int	js;			/* size of data jump table */
        int	rs;			/* size of relocation */
        int	hs;			/* size of symbol hash table */
        int	ss;			/* size of symbol */
        int	sts;			/* size of symbol strings */
        int	ts;			/* size of text aera */
        int	lib;			/* number of libraries */
        int	libstr;			/* lenght of libraries strings */
	int	got_off;		/* offset in GOT of 0'th entry */
};

/*
 * +++++ for sun2 we need to fix the problem where
 * displacement (jb_un.jb.location) is greater than 64k
 */
#if	TARGET==SUN4
struct jbind {
	int jb_inst[3];		/* need 4 instructions for jump slot */
};
#endif
#if	TARGET==SUN3 || TARGET==SUN2
struct jbind {
	unsigned short code;
	unsigned short cl_hi;
	unsigned short cl_low;
	short reloc_index;
};
#endif

struct fshash {
	int fssymbno;		/* ordinal symbol number */
	int next;		/* index to the hash array pointed by fs_hash */
};

#undef relocation_info
#if	TARGET== SUN4
#	define relocation_info	reloc_info_sparc
#	define r_symbolnum	r_index
#else	/* !sun4 , thus mc68000 */
#	define relocation_info	reloc_info_68k
#endif  /* mc68000 */


/* 
 * this structure is used to build the hash and the symbol tables for
 * runtime linking.
 */
struct runtime {
	int	*dt;			/* pointer to data linkage table */
	struct	jbind *jt;		/* pointer to jump linkage table */
	struct	relocation_info *rp; 	/* ptr to first runtime reloc entry */
	int	rl;			/* no of reloc needed for runtime */
	struct	fshash *hp;		/* first entry of hash table */
	struct	nlist *sp;		/* first entry of runtime symb table */
	struct	fshash *hp_last;	/* last entry of hash table */
	struct	nlist *sp_last;		/* last entry of symbol table */
	char	*fsstr;			/* pointer to strings aera */
	int	hp_ind;			/* index to the next hash entry */
	struct	nlist *spp;		/* ptr to next available symbol entry */
	int	*dtp;			/* ptr to next available data slot */
	struct	jbind *jtp;		/* ptr to next available jump slot */
	int	dto;			/* offset in bytes into data table */
	int	jto;			/* offset in bytes into jump table */
	struct	relocation_info *rpp; 	/* ptr to next available reloc entry */
	struct	dynamic *dp;		/* ptr to dynamic structure */
	int	fsoff;			/* current offset to fast symbol aera */
	int	fsalloc;		/* space allocated for fast symbols */
	char	*searchpath;		/* search pathname */
	int	spthlen;		/* search pathname string len */
	int	us;			/* number of undefined symbols */
	char	*libname;		/* pointer to the shlib name list */
	int	lko_i;			/* index into lko array */
	long	buckets;		/* number of hash buckets needed */
	struct	link_object lko[MAXLIB];/* link object array */
};

struct dslot {
   	int js;				/* jump table slots */
   	int ds;				/* data table slots */
   	int ss;				/* static table slots */
};

struct rl {
	int rl_d;	/* total no of reloc for data segment */
	int rl_de;	/* no of reloc to external symb in data segment */
	int rl_t;	/* total no of non pic reloc for text segment */ 
	int rl_te;	/* no of non pic reloc to external in text segment */ 
};

struct ssymbol {
	struct ssymbol	*ss_next;	/* next symbol */
	char		*ssp;		/* pointer to symbol */
	int		ssflag;		/* object or process symbol */
};

#if	TARGET==SUN4
#define SETHI 0x03000000		/* sethi %hi(val),%g1 */
#define SETHIG0 0x01000000		/* sethi %hi(val),%g0 */
#define ORIT 0x82106000                 /* or %g1,val,%g1 */
#define JMPI 0x81c06000			/* jmpl %g1 */
#define NOP 0x01000000			/* nop */
#define BA 0x10800000			/* branch always */
#define TRAP 0x91d02001
#define SAVE 0x9de3bfa0			/* save minimun frame */
#define CALL 0x40000000			/* call instuction */
#define lalign(i) (i%(sizeof (double)) ? i + ((sizeof (double))-(i%(sizeof (double)))) : i)

#endif
#if	TARGET==SUN3 || TARGET==SUN2

#define lalign(i) (i%(sizeof (long)) ? i + ((sizeof (long))-(i%(sizeof (long)))) : i)
#define NOP 0x4e71
#define JSR 0x4eb9
#define JUMP 0x4ef9
#define TRAP 0x4e4f
#if 	TARGET==SUN2
#define JBSR 0x6100
#else
#define JBSR 0x61ff
#endif
#endif

#define D_NAME "__DYNAMIC"
#define LDPATH "LD_LIBRARY_PATH"

#define v1 ld_un.ld_1
#define v2 ld_un.ld_2
