/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)rec.c	5.2 (Berkeley) 4/6/87";
#endif not lint

#include "whoami.h"
#include "0.h"
#include "tree.h"
#include "opcode.h"
#include "align.h"
#include "tree_ty.h"

    /*
     *	set this to TRUE with adb to turn on record alignment/offset debugging.
     */
bool	debug_records = FALSE;
#define	DEBUG_RECORDS(x)	if (debug_records) { x ; } else

/*
 * Build a record namelist entry.
 * Some of the processing here is somewhat involved.
 * The basic structure we are building is as follows.
 *
 * Each record has a main RECORD entry,
 * with an attached chain of fields as ->chain;
 * these enclude all the fields in all the variants of this record.
 * Fields are cons'ed to the front of the ->chain list as they are discovered.
 * This is for reclook(), but not for sizing and aligning offsets.
 *
 * If there are variants to the record, NL_TAG points to the field which
 * is the tag.  If its name is NIL, the tag field is unnamed, and is not
 * allocated any space in the record.
 * Attached to NL_VARNT is a chain of VARNT structures
 * describing each of the variants.  These are further linked
 * through ->chain.  Each VARNT has, in ->range[0] the value of
 * the associated constant, and each points at a RECORD describing
 * the subrecord through NL_VTOREC.  These pointers are not unique,
 * more than one VARNT may reference the same RECORD.
 *
 * On the first pass, we traverse the parse tree and construct the namelist
 * entries.  This pass fills in the alignment of each record (including
 * subrecords (the alignment of a record is the maximum of the alignments
 * of any of its fields).
 * A second pass over the namelist entries fills in the offsets of each field
 * based on the alignments required.  This second pass uses the NL_FIELDLIST
 * chaining of fields, and the NL_TAG pointer and the NL_VARNT pointer to get
 * to fields in the order in which they were declared.
 * This second pass can not be folded into the first pass,
 * as the starting offset of all variants is the same,
 * so we must see all the variants (and especially must know their alignments)
 * before assigning offsets.  With the alignments calculated (by the first
 * pass) this can be done in one top down pass, max'ing over the alignment of
 * variants before assigning offsets to any of them.
 */

/*
 * P0 points to the outermost RECORD for name searches.
 */
struct	nl *P0;

struct nl *
tyrec(r, off)
	struct tnode *r;
	int	      off;
{
	struct nl	*recp;

	DEBUG_RECORDS(fprintf(stderr,"[tyrec] off=%d\n", off));
	    /*
	     *	build namelist structure for the outermost record type.
	     *	then calculate offsets (starting at 0) of the fields
	     *	in this record and its variant subrecords.
	     */
	recp = tyrec1(r, TRUE);
	rec_offsets(recp, (long) 0);
	return recp;
}

/*
 * Define a record namelist entry.
 * r is the tree for the record to be built.
 * first is a boolean indicating whether this is an outermost record,
 * for name lookups.
 * p is the record we define here.
 * P0was is a local which stacks the enclosing value of P0 in the stack frame,
 * since tyrec1() is recursive.
 */
struct nl *
tyrec1(r, first)
	register struct tnode *r;	/* T_FLDLST */
	bool first;
{
	register struct nl *p, *P0was;

	DEBUG_RECORDS(fprintf(stderr,"[tyrec1] first=%d\n", first));
	p = defnl((char *) 0, RECORD, NLNIL, 0);
	P0was = P0;
	if (first)
		P0 = p;
#ifndef PI0
	p->align_info = A_MIN;
#endif
	if (r != TR_NIL) {
		fields(p, r->fldlst.fix_list);
		variants(p, r->fldlst.variant);
	}
	P0 = P0was;
	return (p);
}

/*
 * Define the fixed part fields for p.
 * hang them, in order, from the record entry, through ->ptr[NL_FIELDLIST].
 * the fieldlist is a tconc structure, and is manipulated 
 * just like newlist(), addlist(), fixlist() in the parser.
 */
fields(p, r)
	struct nl *p;
	struct tnode *r;	/* T_LISTPP */
{
	register struct tnode	*fp, *tp, *ip;
	struct nl	*jp;
	struct nl	*fieldnlp;

	DEBUG_RECORDS(fprintf(stderr,"[fields]\n"));
	for (fp = r; fp != TR_NIL; fp = fp->list_node.next) {
		tp = fp->list_node.list;
		if (tp == TR_NIL)
			continue;
		jp = gtype(tp->rfield.type);
		line = tp->rfield.line_no;
		for (ip = tp->rfield.id_list; ip != TR_NIL;
				    ip = ip->list_node.next) {
		    fieldnlp = deffld(p, (char *) ip->list_node.list, jp);
		    if ( p->ptr[NL_FIELDLIST] == NIL ) {
			    /* newlist */
			p->ptr[NL_FIELDLIST] = fieldnlp;
			fieldnlp->ptr[NL_FIELDLIST] = fieldnlp;
		    } else {
			    /* addlist */
			fieldnlp->ptr[NL_FIELDLIST] =
				p->ptr[NL_FIELDLIST]->ptr[NL_FIELDLIST];
			p->ptr[NL_FIELDLIST]->ptr[NL_FIELDLIST] = fieldnlp;
			p->ptr[NL_FIELDLIST] = fieldnlp;
		    }
		}
	}
	if ( p->ptr[NL_FIELDLIST] != NIL ) {
		/* fixlist */
	    fieldnlp = p->ptr[NL_FIELDLIST]->ptr[NL_FIELDLIST];
	    p->ptr[NL_FIELDLIST]->ptr[NL_FIELDLIST] = NIL;
	    p->ptr[NL_FIELDLIST] = fieldnlp;
	}
}

/*
 * Define the variants for RECORD p.
 */
variants(p, r)
	struct nl *p;
	register struct tnode *r;	/* T_TYVARPT */
{
	register struct tnode *vc, *v;
	struct nl *vr;
	struct nl *ct;

	DEBUG_RECORDS(fprintf(stderr,"[variants]\n"));
	if (r == TR_NIL)
		return;
	ct = gtype(r->varpt.type_id);
	if ( ( ct != NLNIL ) && ( isnta( ct , "bcsi" ) ) ) {
	    error("Tag fields cannot be %ss" , nameof( ct ) );
	}
	line = r->varpt.line_no;
	/*
	 * Want it even if r[2] is NIL so
	 * we check its type in "new" and "dispose"
	 * calls -- link it to NL_TAG.
	 */
	p->ptr[NL_TAG] = deffld(p, r->varpt.cptr, ct);
	for (vc = r->varpt.var_list; vc != TR_NIL; vc = vc->list_node.next) {
		v = vc->list_node.list;
		if (v == TR_NIL)
			continue;
		vr = tyrec1(v->tyvarnt.fld_list, FALSE);
#ifndef PI0
		DEBUG_RECORDS(
		    fprintf(stderr,
			"[variants] p->align_info %d vr->align_info %d\n",
			p->align_info, vr->align_info));
		if (vr->align_info > p->align_info) {
		    p->align_info = vr->align_info;
		}
#endif
		line = v->tyvarnt.line_no;
		for (v = v->tyvarnt.const_list; v != TR_NIL;
				v = v->list_node.next)
			(void) defvnt(p, v->list_node.list, vr, ct);
	}
}

/*
 * Define a field in subrecord p of record P0
 * with name s and type t.
 */
struct nl *
deffld(p, s, t)
	struct nl *p;
	register char *s;
	register struct nl *t;
{
	register struct nl *fp;

	DEBUG_RECORDS(fprintf(stderr,"[deffld] s=<%s>\n", s));
	if (reclook(P0, s) != NIL) {
#ifndef PI1
		error("%s is a duplicate field name in this record", s);
#endif
		s = NIL;
	}
	    /*
	     *	enter the field with its type
	     */
	fp = enter(defnl(s, FIELD, t, 0));
	    /*
	     *	if no name, then this is an unnamed tag,
	     *	so don't link it into reclook()'s chain.
	     */
	if (s != NIL) {
		fp->chain = P0->chain;
		P0->chain = fp;
#ifndef PI0
		    /*
		     * and the alignment is propagated back.
		     */
		fp->align_info = align(t);
		DEBUG_RECORDS(
		    fprintf(stderr,
			"[deffld] fp->align_info %d p->align_info %d \n",
			fp->align_info, p->align_info));
		if (fp->align_info > p->align_info) {
		    p->align_info = fp->align_info;
		}
#endif
		if (t != NIL) {
			P0->nl_flags |= t->nl_flags & NFILES;
			p->nl_flags |= t->nl_flags & NFILES;
		}
	}
	return (fp);
}

/*
 * Define a variant from the constant tree of t
 * in subrecord p of record P0 where the casetype
 * is ct and the variant record to be associated is vr.
 */
struct nl *
defvnt(p, t, vr, ct)
	struct nl *p, *vr;
	struct tnode *t;	/* CHAR_CONST or SIGN_CONST */
	register struct nl *ct;
{
	register struct nl *av;

	gconst(t);
	if (ct != NIL && incompat(con.ctype, ct , t )) {
#ifndef PI1
		cerror("Variant label type incompatible with selector type");
#endif
		ct = NIL;
	}
	av = defnl((char *) 0, VARNT, ct, 0);
#ifndef PI1
	if (ct != NIL)
		uniqv(p);
#endif not PI1
	av->chain = p->ptr[NL_VARNT];
	p->ptr[NL_VARNT] = av;
	av->ptr[NL_VTOREC] = vr;
	av->range[0] = con.crval;
	return (av);
}

#ifndef PI1
/*
 * Check that the constant label value
 * is unique among the labels in this variant.
 */
uniqv(p)
	struct nl *p;
{
	register struct nl *vt;

	for (vt = p->ptr[NL_VARNT]; vt != NIL; vt = vt->chain)
		if (vt->range[0] == con.crval) {
			error("Duplicate variant case label in record");
			return;
		}
}
#endif

/*
 * See if the field name s is defined
 * in the record p, returning a pointer
 * to it namelist entry if it is.
 */
struct nl *
reclook(p, s)
	register struct nl *p;
	char *s;
{

	if (p == NIL || s == NIL)
		return (NIL);
	for (p = p->chain; p != NIL; p = p->chain)
		if (p->symbol == s)
			return (p);
	return (NIL);
}

    /*
     *	descend namelist entry for a record and assign offsets.
     *	fields go at the next higher offset that suits their alignment.
     *	all variants of a record start at the same offset, which is suitable
     *	for the alignment of their worst aligned field.  thus the size of a 
     *	record is independent of whether or not it is a variant
     *	(a desirable property).
     *	records come to us in the namelist, where they have been annotated
     *	with the maximum alignment their fields require.
     *	the starting offset is passed to us, and is passed recursively for
     *	variant records within records.
     *	the final maximum size of each record is recorded in the namelist
     *	in the value[NL_OFFS] field of the namelist for the record.
     *
     *	this is supposed to match the offsets used by the c compiler
     *	so people can share records between modules in both languages.
     */
rec_offsets(recp, offset)
    struct nl	*recp;		/* pointer to the namelist record */
    long	offset;		/* starting offset for this record/field */
{
    long	origin;		/* offset of next field */
    struct nl	*fieldnlp;	/* the current field */
    struct nl	*varntnlp;	/* the current variant */
    struct nl	*vrecnlp;	/* record for the current variant */

    if ( recp == NIL ) {
	return;
    }
    origin = roundup((int) offset,(long) recp->align_info);
    if (origin != offset) {
	fprintf(stderr,
		"[rec_offsets] offset=%d recp->align_info=%d origin=%d\n",
		offset, recp->align_info, origin);
	panic("rec_offsets");
    }
    DEBUG_RECORDS(
	fprintf(stderr,
	    "[rec_offsets] offset %d recp->align %d origin %d\n",
	    offset, recp->align_info, origin));
	/*
	 *	fixed fields are forward linked though ->ptr[NL_FIELDLIST]
	 *	give them all suitable offsets.
	 */
    for (   fieldnlp = recp->ptr[NL_FIELDLIST];
	    fieldnlp != NIL;
	    fieldnlp = fieldnlp->ptr[NL_FIELDLIST] ) {
	origin = roundup((int) origin,(long) align(fieldnlp->type));
	fieldnlp->value[NL_OFFS] = origin;
	DEBUG_RECORDS(
	    fprintf(stderr,"[rec_offsets] symbol %s origin %d\n",
		    fieldnlp->symbol, origin));
	origin += lwidth(fieldnlp->type);
    }
	/*
	 *	this is the extent of the record, so far
	 */
    recp->value[NL_OFFS] = origin;
	/*
	 *	if we have a tag field, we have variants to deal with
	 */
    if ( recp->ptr[NL_TAG] ) {
	    /*
	     *	if tag field is unnamed, then don't allocate space for it.
	     */
	fieldnlp = recp->ptr[NL_TAG];
	if ( fieldnlp->symbol != NIL ) {
	    origin = roundup((int) origin,(long) align(fieldnlp->type));
	    fieldnlp->value[NL_OFFS] = origin;
	    DEBUG_RECORDS(fprintf(stderr,"[rec_offsets] tag %s origin %d\n",
				    fieldnlp->symbol, origin));
	    origin += lwidth(fieldnlp->type);
	}
	    /*
	     *	find maximum alignment of records of variants
	     */
	for (	varntnlp = recp->ptr[NL_VARNT]; 
		varntnlp != NIL;
		varntnlp = varntnlp -> chain ) {
	    vrecnlp = varntnlp->ptr[NL_VTOREC];
	    DEBUG_RECORDS(
		fprintf(stderr,
			"[rec_offsets] maxing variant %d align_info %d\n",
			varntnlp->value[0], vrecnlp->align_info));
	    origin = roundup((int) origin,(long) vrecnlp->align_info);
	}
	DEBUG_RECORDS(
	    fprintf(stderr, "[rec_offsets] origin of variants %d\n", origin));
	    /*
	     *	assign offsets to fields of records of the variants
	     *	keep maximum length of the current record.
	     */
	for (	varntnlp = recp->ptr[NL_VARNT]; 
		varntnlp != NIL;
		varntnlp = varntnlp -> chain ) {
	    vrecnlp = varntnlp->ptr[NL_VTOREC];
		/*
		 *	assign offsets to fields of the variant.
		 *	recursive call on rec_offsets.
		 */
	    rec_offsets(vrecnlp,origin);
		/*
		 *	extent of the record is the
		 *	maximum extent of all variants
		 */
	    if ( vrecnlp->value[NL_OFFS] > recp->value[NL_OFFS] ) {
		recp->value[NL_OFFS] = vrecnlp->value[NL_OFFS];
	    }
	}
    }
	/*
	 *	roundup the size of the record to its alignment
	 */
    DEBUG_RECORDS(
	fprintf(stderr,
		"[rec_offsets] recp->value[NL_OFFS] %d ->align_info %d\n",
		recp->value[NL_OFFS], recp->align_info));
    recp->value[NL_OFFS] = roundup(recp->value[NL_OFFS],(long) recp->align_info);
}
