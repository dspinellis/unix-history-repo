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
static        char sccsid[] = "@(#)rtsetup.c 1.18 69/12/31"; /* from UCB 5.4 85/11/26 */
#endif not lint

/* Copyright (c) 1991 by Sun Microsystems, Inc. */

#include <sys/types.h>
#include <a.out.h>
#include <link.h>
#include "dynamic.h"
#include "reloc_info.h"
#include <sys/param.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/mman.h>

char *mymalloc();
extern int picflag;

fs_init(rt, sb)
	register struct runtime *rt;
	register int sb;		/* no of symbols for runtime linking */
{
	register int i;
	long buckets;

	/* buckets = sb/4; */
	buckets = (sb < 4 ? sb : sb/4);

	rt->hp = (struct fshash *) mymalloc((buckets+sb)*sizeof(struct fshash));
	rt->sp = (struct nlist *) mymalloc(sb * sizeof (struct nlist));
	rt->hp_last = rt->hp + (buckets+sb);
	rt->sp_last = rt->sp + sb;
	rt->fsalloc = lalign(rt->fsalloc);
	rt->fsstr = (char *) calloc(rt->fsalloc, 1);
	if (rt->hp == 0 || rt->sp == 0 || rt->fsstr == 0)
		error(1, "out of memory in buildfs");
	rt->spp = rt->sp;
	for (i = 0; i < buckets; i++) {
		(rt->hp+i)->fssymbno = -1;	/* mark it empty with -1 */
		(rt->hp+i)->next = 0;		/* init with null index ptr */
	}
	rt->hp_ind = buckets;
	rt->buckets = buckets;
}

/* 
 * this routine does a bunch of initializations for runtime linking
 */
rt_init(rt)
	register struct runtime *rt;
{
	register int i;

	i = totalsymb();
	fs_init(rt, i);
	rt->rl = calcreloc();
	if ((rt->rp = (struct relocation_info *) calloc (rt->rl,
	    sizeof (struct relocation_info))) == 0)
		error(1, "out of memory for dynamic relocation");
	rt->rpp = rt->rp;
}

dp_init(rt)
	register struct runtime *rt;
{
	register struct dynamic *dp = rt->dp;
	register int i;

	i = totalsymb();
	dp->rs = rt->rl * sizeof (struct relocation_info);
	dp->ss = i * sizeof (struct nlist);
	dp->sts = rt->fsalloc;
	dp->hs = rt->hp_ind * sizeof (struct fshash);
	
	/*
	 * jump table entries needed to be on an even boundary
	 */
	dp->libstr = lalign(dp->libstr);
}

/*
 * initialize for data and jump linkage. 
 */
dj_init(rt, slp, flag)
	register struct runtime *rt;
	register struct dslot *slp;
	register int flag; /* if nonzero then set it up for runtime linking */
{
	register struct dynamic *dp = rt->dp;

	/*
	 * for runtime linking we need one extra slot in the data linkage
	 * table to store the address of symbol __DYNAMIC. This is done
	 * so that the runtime linker can find its own dynamic structure
	 * before it can relocate itself.
	 * We also need an extra slot in the jump linkage table which
	 * contained the direct jump to the runtime binder (all the
	 * jump entries do a pc relative jump to this slot).
	 */
	if (slp->ds || slp->js) {
		slp->ds += 1;
		slp->js += 1;
	}

	/*
	 * Calculate the offset into the GOT storage area for the
	 * GOT symbol itself.  In some implementations, the GOT may
	 * be accessed by a signed displacement, this fact is adjusted
	 * for here.
	 */
	dp->got_off = slp->ds + slp->ss;
	if ((slp->ds + slp->ss) > MAX_GOT_SIZE)  {
		if (picflag == 1)
			error(1, 
			    "GLOBAL_OFFSET_TABLE overflown: need to use -PIC");
		dp->got_off = 0;
	} else {
		if (dp->got_off > (MAX_GOT_SIZE / 2))
			dp->got_off = ((MAX_GOT_SIZE / 2) - dp->got_off) *
					sizeof (int);
		else
			dp->got_off = 0;
	}

	/* 
	 * be careful here: the number of bytes for jump slots 
	 * are machine dependent. +++++++
	 */
	dp->ds = (slp->ds + slp->ss) * sizeof(int);
	dp->js = slp->js * sizeof(struct jbind);		
	if ((rt->dt = (int *) calloc(dp->ds, sizeof(int))) == 0)
		error(1, "can't allocate space for data linkage table");
	rt->dtp = rt->dt;
	rt->dto = dp->got_off;
	if (dp->js != 0) {
		if ((rt->jt = (struct jbind  *) calloc(dp->js,
		    sizeof(struct jbind))) == 0)
			error(1, "can't allocate space for jump linkage table");
		if (flag) {
			rt->jtp = rt->jt + 1;
			rt->jto = sizeof(struct jbind);
#if	TARGET==SUN4
			setupjs(rt->jt, 0);
#endif
#if	TARGET==SUN3 || TARGET==SUN2
			rt->jt->code = JUMP;
#endif
	    	} else {
			rt->jtp = rt->jt;
			rt->jto = 0;
		}
	}
}

init_lkd(lkdp, rt, ts, db)
	register struct link_dynamic *lkdp;
	register struct runtime *rt;
	int ts;					/* text size */
	int db;					/* database */
{
	int off;
	int i;
	int j;
	int k = 0;
	char *cp = rt->libname;

	lkdp->ld_version = 2;
	lkdp->v2->ld_got = db + sizeof(struct link_dynamic) + 
	    sizeof(struct ld_debug) + sizeof(struct link_dynamic_2);
	lkdp->v2->ld_plt = lkdp->v2->ld_got + rt->dp->ds;
	lkdp->v2->ld_got += abs(rt->dp->got_off);
	lkdp->v2->ld_plt_sz = rt->dp->js;

	lkdp->v2->ld_rel = ts;
	lkdp->v2->ld_hash = lkdp->v2->ld_rel + rt->dp->rs;
	lkdp->v2->ld_stab = lkdp->v2->ld_hash + rt->dp->hs;
	lkdp->v2->ld_symbols = lkdp->v2->ld_stab + rt->dp->ss;
	if (rt->searchpath)
		lkdp->v2->ld_rules = lkdp->v2->ld_symbols + rt->fsalloc;
	lkdp->v2->ld_symb_size = rt->fsalloc;
	i = rt->dp->lib;
	if (i == 0) 
		lkdp->v2->ld_need = 0;
	else {
		if (lkdp->v2->ld_rules)
			lkdp->v2->ld_need = lkdp->v2->ld_rules +
			    lalign(rt->spthlen);
		else
			lkdp->v2->ld_need = lkdp->v2->ld_symbols + rt->fsalloc;
		off = lkdp->v2->ld_need + (i * sizeof(struct link_object));
		j = 0;
		while (i > 0) {
			rt->lko[j].lo_name = off + k;
			k += strlen(cp) + 1;
			cp = rt->libname + k;
			i--;
			if (i == 0)
				rt->lko[j].lo_next = 0;
			else
				rt->lko[j].lo_next = lkdp->v2->ld_need + 
				    ((j+1)*sizeof(struct link_object));
			j++;
		}
	}
}

#define skipblank(cp)	while ( *cp == ' '  || *cp == '\n' ) cp++;
#define GLOB_SYMB	"object global"

struct ssymbol *ssymbol_p;

/*
 * this routine read in symbols from a file of the following format:
 * 	object global s1, s2, s3, ...., sn;
 * any reference to theses symbols will then be reduced to a relative
 * relocation.
 */
int
getsymb(filename)
	char		*filename;
{
	int 		fd;
	struct stat 	sb;
	char		*buf;
	char		*cp;
	char		*cp1;
	struct ssymbol	*nsp;
	struct ssymbol	*csp;
	char		sc;


	/*
	 * open file
	 */
	if ((fd = open(filename, O_RDONLY)) == -1)
		error(1, "open symbol file %s failed", filename);
	if (fstat(fd, &sb) == -1)
		error(1, "fstat symbol file %s failed", filename);
	buf = mmap(0, sb.st_size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);

	/*
	 * look for key word "object global"
	 */
	cp = buf;
	while ( *cp == ' ' ) cp++;
	if (!strcmp(cp, GLOB_SYMB))
		error(1, "missing key word (object global) in symbol file");
	cp += strlen(GLOB_SYMB);
	skipblank(cp);

	/*
	 * loop until ";", the marker for the end of the list is found.
	 */
	csp = ssymbol_p;
	cp1 = cp;
	for (;;) {
		if (*cp != ',' && *cp != ';') {
			cp++;
			continue;
		}
			
		sc = *cp;
		*cp++ = '\0';
		nsp = (struct ssymbol *) malloc( sizeof(struct ssymbol));
		nsp->ssp = cp1;
		nsp->ss_next = 0;
		if (csp)
			csp->ss_next = nsp; 
		else
			ssymbol_p = nsp;
		csp = nsp;
		skipblank(cp);
		cp1 = cp;
		if (sc == ';')
			break;
	} 
}
