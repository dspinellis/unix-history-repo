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

#ifndef lint
static	char sccsid[] = "@(#)incl.c 1.11 69/12/31 Copyr 1986 Sun Micro";
#endif

/*
 * Copyright (c) 1986, 1991 by Sun Microsystems, Inc.
 */

#include <stdio.h>
#include <a.out.h>
#include <stab.h>
#include <ctype.h>

/*
 * Info about each include file
 */
struct incl {
	char	*name;			/* Name of header file */
	int	nstatic;		/* # of statics in the file */
	int	nsymbols;		/* # of symbols in header file */
	int	checksum;		/* Checksum for header file */
	int	ord;			/* Ordinal # of header file */
	int	exclude;		/* Flag: include/exclude syms */
	struct	incl	*next;		/* Linked Hash list */
};

#define NINCL_STACK	10		/* Size of the include stack */
#define INCL_HASH	64		/* Size of header file hash table */

static	struct	incl	**incl_stack;		/* Include file stack */
static	int	stack_size;			/* Size of incl_stack */
static	struct	incl	**stkp;			/* Stack pointer */
static	struct	incl	*incl_table[INCL_HASH];	/* Table saved for pass 2 */
static	struct	incl	*cur_incl;		/* Current header file */
static	int	obj_ord;			/* Saved value of header_num */

extern	int	ssize;			/* Size of symbol section */
extern	int	header_num;		/* Ordinal value of next header file */

struct	incl	*find_ord();
char	*strcpy();
char	*mymalloc();
char	*realloc();

/*
 * Allocate the intial include file stack.
 * The stack will grow if necessary.
 */
alloc_inclstack()
{
	if ((incl_stack = (struct incl **) mymalloc(NINCL_STACK*sizeof(struct incl *))) == 0)
		error(1, "ran out of memory (alloc_inclstack)");
	stkp = incl_stack;
	stack_size = NINCL_STACK;
}
	
/*
 * Beginning a new object file.  Remember the ordinal number
 * of the next header file in case this object file is not 
 * used and everything needs to be ripped out and thrown away.
 */
new_obj1()
{
	obj_ord = header_num;
}

/*
 * Enter a new include file.
 * Stack the current one, if it is active.
 * Allocate a structure and initialize it.
 */
start_incl1(sym, ord)
struct	nlist	*sym;
int	ord;
{
	char	*name;
	int	n;
	register char *namespace;

	if (stkp > &incl_stack[stack_size]) {
		n = stkp - incl_stack;
		stack_size += NINCL_STACK;
		if ((incl_stack = (struct incl **) realloc(incl_stack, 
		    stack_size * sizeof(struct incl *))) == 0)
			error(1, "ran out of memory (start_incl1)");
		stkp = &incl_stack[n];
	}
	*stkp++ = cur_incl;
	name = sym->n_un.n_name;
	if ((cur_incl = (struct incl *) mymalloc(sizeof(struct incl))) == 0)
		error(1, "ran out of memory (start_incl1)");
	if ((namespace = mymalloc(strlen(name) + 1)) == 0)
		error(1, "ran out of memory (start_incl1)");
	cur_incl->name = strcpy(namespace, name);
	cur_incl->nstatic = 0;
	cur_incl->nsymbols = 0;
	cur_incl->checksum = 0;
	cur_incl->ord = ord;
	cur_incl->exclude = 0;
	cur_incl->next = NULL;
}

/*
 * Found the end of an include file.
 * Hash on its name and enter it into the
 * has table.  Pass two will find it by
 * its name and ordinal number.
 * Also, pop one off of the stack.
 */
end_incl1()
{
	int	h;

	h = hash(cur_incl->name);
	cur_incl->next = incl_table[h];
	incl_table[h] = cur_incl;
	cur_incl = *--stkp;
}

/*
 * Found a dbx symbol in pass one.
 * If there is an active header file, include the info.
 * The checksum is the summation of all the letters, underscores,
 * and equals signs in the n_name field of the nlist 
 * structure.
 *
 * The equals signs are counted because they are present whenever
 * new type is defined.  We want to insure that each inclusion of
 * a header file defines the same number of types.
 */
stab1(sym)
struct	nlist	*sym;
{
	register char	*cp;

	if (cur_incl == NULL) {
		return;
	}
	cur_incl->nsymbols++;
	if (sym->n_type == N_STSYM || sym->n_type == N_LCSYM || sym->n_type == N_EXCL ) {
		cur_incl->nstatic++;
	}
	cp = sym->n_un.n_name;
	if (sym->n_type != N_EXCL && cp != NULL) {
		while (*cp != '\0') {
			if (isalpha(*cp) || *cp == '_' || *cp == '=') {
				cur_incl->checksum += *cp;
			}
			cp++;
		}
	}
}

/*
 * An object file was not used.
 * Must remove any header files it had from the hash table.
 */
incl_free()
{
	struct	incl	*ip;
	struct	incl	*next;
	struct	incl	**ipp;
	struct	incl	**bpatch;
	int	nhdrs;
	int	removed;

	if (obj_ord == header_num) {
		return;
	}
	removed = 0;
	nhdrs = header_num - obj_ord;
	for (ipp = incl_table; ipp < &incl_table[INCL_HASH]; ipp++) {
		bpatch = ipp;
		for (ip = *ipp; ip != NULL; ip = next) {
			next = ip->next;
			if (ip->ord >= obj_ord) {
				*bpatch = ip->next;
				free((char *) ip);
				removed++;
			} else {
				bpatch = &ip->next;
			}
		}
	}
	if (removed != nhdrs) {
		fprintf(stderr, "incl_free removed %d nhdrs %d\n",
			removed, nhdrs);
	}
	header_num = obj_ord;
}

/*
 * See what header files can be excluded from the final output.
 * The important goal here is to determine how many symbols
 * will be excluded so that "ssize" can be adjusted.  "Ssize"
 * determines the value that will be stored into the a_syms
 * field of the a.out header and also determines where the
 * string table will begin.
 */
merge_headers()
{
	register struct	incl	*ip;
	register struct	incl	**ipp;

	for (ipp = incl_table; ipp < &incl_table[INCL_HASH]; ipp++) {
		for (ip = *ipp; ip != NULL; ip = ip->next) {
			ip->exclude = find_prev(ip);
			if (ip->exclude) {
				ssize -= (ip->nsymbols + 1) * 
					sizeof(struct nlist);
			}
		}
	}
	cur_incl = NULL;
}

/*
 * Found the beginning of a header file in pass2.
 * Find the struct for the header file created in pass1.
 * See if there is an identical struct for a header file
 * with a lower ordinal value.  If so, the symbols in this
 * file can be discarded.
 */
start_incl2(sym, ord)
struct	nlist	*sym;
int	ord;
{
	struct 	incl	*ip;
	int	r;

	if (stkp > &incl_stack[NINCL_STACK]) {
		error(1, "include stack too deep");
	}
	*stkp++ = cur_incl;
	cur_incl = find_ord(sym, ord);
	sym->n_value = cur_incl->checksum;
	if (cur_incl->exclude) {
		sym->n_type = N_EXCL;
	}
	return(cur_incl->exclude);
}

end_incl2()
{
	cur_incl = *--stkp;
	return(cur_incl != NULL && cur_incl->exclude);
}

/*
 * Find a header file with a given ordinal number.
 */
struct	incl	*
find_ord(sym, ord)
struct	nlist	*sym;
int	ord;
{
	register struct	incl	*ip;
	int	h;

	h = hash(sym->n_un.n_name);
	for (ip = incl_table[h]; ip != NULL; ip = ip->next) {
		if (ip->ord == ord) {
			return(ip);
		}
	}
	error(1, "no include table entry for header file '%s'",
	    sym->n_un.n_name);
}

/*
 * Try to find an identical header file with a lower ordinal number.
 */
find_prev(incl)
register struct	incl	*incl;
{
	register struct	incl	*ip;
	int	h;

	if (incl->nstatic > 0) {
		return(0);
	}
	h = hash(incl->name);
	for (ip = incl_table[h]; ip != NULL; ip = ip->next) {
		if (ip->nsymbols == incl->nsymbols &&
		    ip->nstatic == 0 &&
		    ip->checksum == incl->checksum &&
		    ip->ord < incl->ord && 
		    strcmp(incl->name, ip->name) == 0) {
			return(1);
		}
	}
	return(0);
}

/*
 * Compute a hash from a string.
 * Simply sum the characters and mod.
 */
hash(str)
char	*str;
{
	register int	sum;
	register char	*cp;

	sum = 0;
	for (cp = str; *cp; cp++) {
		sum += *cp;
	}
	return(sum % INCL_HASH);
}
