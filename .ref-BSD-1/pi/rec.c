#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.0 August 1977
 */

#include "whoami"
#include "0.h"
#include "tree.h"
#include "opcode.h"

/*
 * Build a record namelist entry.
 * Some of the processing here is somewhat involved.
 * The basic structure we are building is as follows.
 *
 * Each record has a main RECORD entry, with an attached
 * chain of fields as ->chain;  these include all the fields in all
 * the variants of this record.
 *
 * Attached to NL_VARNT is a chain of VARNT structures
 * describing each of the variants.  These are further linked
 * through ->chain.  Each VARNT has, in ->range[0] the value of
 * the associated constant, and each points at a RECORD describing
 * the subrecord through NL_VTOREC.  These pointers are not unique,
 * more than one VARNT may reference the same RECORD.
 *
 * The involved processing here is in computing the NL_OFFS entry
 * by maxing over the variants.  This works as follows.
 *
 * Each RECORD has two size counters.  NL_OFFS is the maximum size
 * so far of any variant of this record;  NL_FLDSZ gives the size
 * of just the FIELDs to this point as a base for further variants.
 *
 * As we process each variant record, we start its size with the
 * NL_FLDSZ we have so far.  After processing it, if its NL_OFFS
 * is the largest so far, we update the NL_OFFS of this subrecord.
 * This will eventually propagate back and update the NL_OFFS of the
 * entire record.
 */

/*
 * P0 points to the outermost RECORD for name searches.
 */
struct	nl *P0;

tyrec(r, off)
	int *r, off;
{

	tyrec1(r, off, 1);
}

/*
 * Define a record namelist entry.
 * R is the tree for the record to be built.
 * Off is the offset for the first item in this (sub)record.
 */
tyrec1(r, off, first)
	register int *r;
	int off;
	char first;
{
	register struct nl *p, *P0was;

	p = defnl(0, RECORD, 0, 0);
	P0was = P0;
	if (first)
		P0 = p;
	p->value[NL_FLDSZ] = p->value[NL_OFFS] = off;
	if (r != NIL) {
		fields(p, r[2]);
		variants(p, r[3]);
	}
	P0 = P0was;
	return (p);
}

/*
 * Define the fixed part fields for p.
 */
fields(p, r)
	struct nl *p;
	int *r;
{
	register int *fp, *tp, *ip;
	struct nl *jp;

	for (fp = r; fp != NIL; fp = fp[2]) {
		tp = fp[1];
		if (tp == NIL)
			continue;
		jp = gtype(tp[3]);
		line = tp[1];
		for (ip = tp[2]; ip != NIL; ip = ip[2])
			deffld(p, ip[1], jp);
	}
}

/*
 * Define the variants for RECORD p.
 */
variants(p, r)
	struct nl *p;
	int *r;
{
	register int *vc, *v;
	int *vr;
	struct nl *ct;

	if (r == NIL)
		return;
	ct = gtype(r[3]);
	line = r[1];
	/*
	 * Want it even if r[2] is NIL so
	 * we check its type in "new" and "dispose"
	 * calls -- link it to NL_TAG.
	 */
	p->value[NL_TAG] = deffld(p, r[2], ct);
	for (vc = r[4]; vc != NIL; vc = vc[2]) {
		v = vc[1];
		if (v == NIL)
			continue;
		vr = tyrec1(v[3], p->value[NL_FLDSZ], 0);
		if (vr->value[NL_OFFS] > p->value[NL_OFFS])
			p->value[NL_OFFS] = vr->value[NL_OFFS];
		line = v[1];
		for (v = v[2]; v != NIL; v = v[2])
			defvnt(p, v[1], vr, ct);
	}
}

/*
 * Define a field in subrecord p of record P0
 * with name s and type t.
 */
deffld(p, s, t)
	struct nl *p;
	char *s;
	struct nl *t;
{
	register struct nl *fp;

	if (reclook(P0, s) != NIL) {
		error("%s is a duplicate field name in this record", s);
		s = NIL;
	}
	fp = enter(defnl(s, FIELD, t, p->value[NL_OFFS]));
	if (s != NIL) {
		fp->chain = P0->chain;
		P0->chain = fp;
		p->value[NL_FLDSZ] = p->value[NL_OFFS] =+ even(width(t));
		if (t != NIL) {
			P0->nl_flags =| t->nl_flags & NFILES;
			p->nl_flags =| t->nl_flags & NFILES;
		}
	}
	return (fp);
}

/*
 * Define a variant from the constant tree of t
 * in subrecord p of record P0 where the casetype
 * is ct and the variant record to be associated is vr.
 */
defvnt(p, t, vr, ct)
	struct nl *p;
	int *t;
	struct nl *vr, *ct;
{
	register struct nl *av;

	gconst(t);
	if (ct != NIL && incompat(con.ctype, ct)) {
		cerror("Variant label type incompatible with selector type");
		ct = NIL;
	}
	av = defnl(0, VARNT, ct, 0);
	if (ct != NIL)
		uniqv(p);
	av->chain = p->value[NL_VARNT];
	p->value[NL_VARNT] = av;
	av->value[NL_VTOREC] = vr;
	av->range[0] = con.crval;
	return (av);
}

/*
 * Check that the constant label value
 * is unique among the labels in this variant.
 */
uniqv(p)
	struct nl *p;
{
	register struct nl *vt;

	for (vt = p->value[NL_VARNT]; vt != NIL; vt = vt->chain)
		if (vt->range[0] == con.crval) {
			error("Duplicate variant case label in record");
			return;
		}
}

/*
 * See if the field name s is defined
 * in the record p, returning a pointer
 * to it namelist entry if it is.
 */
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
