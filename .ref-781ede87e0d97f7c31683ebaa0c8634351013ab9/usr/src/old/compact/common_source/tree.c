#ifndef lint
static char sccsid[] = "@(#)tree.c	4.4 (Berkeley) %G%";
#endif

#include "compact.h"

insert(ch)
	int ch;
{
	register struct node *pp;
	register struct son *pson, *bson;
	union cio d;
	register struct index *wt;

	wt = NEW;
	pp = bottom++;
	pson = &pp->sons[RIGHT];
	bson = &bottom->sons[LEFT];
	bottom->fath.fp = pp;
	in[ch].flags = (SEEN | FBIT);
	d.integ = bson->sp.ch = pson->sp.ch;
	in[ch].fp = in[d.integ].fp = pson->sp.p = wt->pt = bottom;
	bottom->fath.flags = (LLEAF | RLEAF | FBIT);
	pp->fath.flags &= ~RLEAF;
	in[d.integ].flags = SEEN;

	bson->count = pson->count;
	bson->top = pson->top;
	bson++;
	bson->sp.ch = ch;
	bson->count = 0;
	bson->top = pson->top->next = wt;
	wt->next = NULL;
}

uptree(ch)
	int ch;
{
	register struct node *r;
	union treep q, s;
	int rs, ts, rflags, tflags;
	longint rc, qc, sc;
	struct node *t;
	register struct son *rson, *tson;
	register struct index *rt, *qt, *st;

	r = in[ch].fp;
	rs = in[ch].flags & FBIT;

	do {
		rson = &r->sons[rs];
		rc = ++rson->count;
		rt = rson->top;
		for (;;) {
			if (rs) {
				s.p = r + 1;
				if (r == bottom) {
					sc = rc - 2;
					st = NULL;
				} else {
					sc = (r+1)->sons[LEFT].count;
					st = (r+1)->sons[LEFT].top;
				}
				qc = r->sons[LEFT].count;
				qt = r->sons[LEFT].top;
			} else {
				s.p = r;
				sc = r->sons[RIGHT].count;
				st = r->sons[RIGHT].top;
				if (r == dict) {
					qc = rc + 1;
					qt = head;
					break;
				} else {
					qc = (r-1)->sons[RIGHT].count;
					qt = (r-1)->sons[RIGHT].top;
				}
			}
			if (rc <= qc)
				break;

			t = qt->pt;
			ts = LEFT;
			tson = &t->sons[LEFT];
			if (rc <= tson->count) {
				tson++;
				ts++;
			}

			/* exchange pointers of (t, ts) and (r, rs) */
			q.ch = tson->sp.ch;
			s.ch = rson->sp.ch;
			tson->sp.ch = s.ch;
			rson->sp.ch = q.ch;
			exch(t, ts, q.ch, r, rs);
			exch(r, rs, s.ch, t, ts);

			rflags = (rs ? RLEAF : LLEAF);
			tflags = (ts ? RLEAF : LLEAF);
			if (((r->fath.flags & rflags) << rs) ^ ((t->fath.flags & tflags) << ts)) {
				r->fath.flags ^= rflags;
				t->fath.flags ^= tflags;
			}

			tson->count++;
			rson->count--;
			if (ts)
				qt->pt++;
			r = t;
			rs = ts;
			rson = tson;
		}

		if (rc == qc) {
			rson->top = qt;
			if (rc > sc + 1) {
				qt->next = st;
				/* dispose of rt */
				rt->next = flist;
				flist = rt;
			} else
				st->pt = s.p;
		} else if (rc == sc + 1) {
			/* create new index at rt */
			rt = NEW;
			rt->next = st;
			rt->pt = r;
			qt->next = rt;
			if (st)
				st->pt = s.p;
			rson->top = rt;
		}
		rs = r->fath.flags & FBIT;
		r = r->fath.fp;
	} while (r);
	dirp = head->next;
	dirq = dirp->next;
}

exch(v, vs, x, w, ws)
	struct node *v, *w;
	union treep x;
	int vs, ws;
{

	if (v->fath.flags & (vs ? RLEAF : LLEAF)) {
		in[x.ch].fp = w;
		in[x.ch].flags &= ~01;
		if (ws)
			in[x.ch].flags |= ws;
	} else {
		x.p->fath.fp = w;
		x.p->fath.flags &= ~01;
		if (ws)
			x.p->fath.flags |= ws;
	}
}
