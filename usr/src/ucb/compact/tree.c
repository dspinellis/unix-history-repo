#include "compact.h"


insert (ch)
int ch;
{
	register struct node *pp;
	register int c;
	union cio d;
	register struct index *qt, *wt;

	c = ch;
	wt = NEW;
	pp = bottom++;
	bottom -> fath . fp = pp;
	in [c] . flags = (SEEN | FBIT);
	d . integ = bottom -> sp [0] . ch = pp -> sp [1] . ch;
	in [c] . fp = in [d . integ] . fp = pp -> sp [1] . p = wt -> pt = bottom;
	bottom -> fath . flags = (LLEAF | RLEAF | FBIT);
	pp -> fath . flags &= ~RLEAF;
	in [d . integ] . flags = SEEN;

	bottom -> count [0] = pp -> count [1];
	qt = pp -> top [1];
	bottom -> top [0] = qt;
	bottom -> sp [1] . ch = c;
	bottom -> count [1] = 0;
	bottom -> top [1] = qt -> next = wt;
	wt -> next = NULL;
}

uptree (ch)
int ch;
{
	register struct node *r;
	union treep q, s;
	int rs, qs, ss, ts;
	longint rc, qc, sc;
	struct node *t;
	register struct index *rt, *qt, *st;

	r = in [ch] . fp;
	rs = in [ch] . flags & FBIT;

	do {
		(r -> count [rs])++;
		rc = r -> count [rs];
		rt = r -> top [rs];
	
		for ( ; ; ) {
			qs = ss = 1 - rs;
			s . p = r + rs;
			sc = (s . p) -> count [ss];
			st = (s . p) -> top [ss];
	
			if (rs)
				if (r == bottom) {
					sc = rc - 2;
					st = NULL;
				}
				else;
			else if (r == dict) {
				qc = rc + 1;
				qt = head;
				break;
			}

			q . p = r - qs;
			qc = (q . p) -> count [qs];
			qt = (q . p) -> top [qs];
			if (rc <= qc) break;

			t = qt -> pt;
			ts = (rc <= t -> count [0] ? 1 : 0);

			/* exchange pointers of (t, ts) and (r, rs) */

			q . ch = t -> sp [ts] . ch;	/*  {					*/
			s . ch = r -> sp [rs] . ch;	/*  {					*/
			t -> sp [ts] . ch = s . ch;	/*  {					*/
			r -> sp [rs] . ch = q . ch;	/*  { change code when Cory gets v. 7	*/
							/*  {					*/
			exch (t, ts, q . ch, r, rs);	/*  {					*/
			exch (r, rs, s . ch, t, ts);	/*  {					*/

			qs = (rs ? RLEAF : LLEAF);
			ss = (ts ? RLEAF : LLEAF);
			if (((r -> fath . flags & qs) << rs) ^ ((t -> fath . flags & ss) << ts)) {
				r -> fath . flags ^= qs;
				t -> fath . flags ^= ss;
			}

			(t -> count [ts])++;
			(r -> count [rs])--;
			(qt -> pt) += ts;
			r = t;
			rs = ts;
		}

		if (rc == qc) {
			r -> top [rs] = qt;
			if (rc > sc + 1) {
				qt -> next = st;

				/* dispose of rt */

				rt -> next = flist;
				flist = rt;
			}
			else st -> pt = s . p;
		}

		else if (rc == sc + 1) {

			/* create new index at rt */

			rt = NEW;
			rt -> next = st;
			rt -> pt = r;
			qt -> next = rt;
			if (st) st -> pt = s . p;
			r -> top [rs] = rt;
		}

		rs = r -> fath . flags & FBIT;
		r = r -> fath . fp;

	} while (r);
	dirp = head -> next;
	dirq = dirp -> next;
}

exch (v, vs, x, w, ws)
struct node *v, *w;
union treep x;
int vs, ws;
{

	if (v -> fath . flags & (vs ? RLEAF : LLEAF)) {
		in [x . ch] . fp = w;
		in [x . ch] . flags &= ~01;
		if (ws) in [x . ch] . flags |= ws;
	}
	else {
		(x . p) -> fath . fp = w;
		(x . p) -> fath . flags &= ~01;
		if (ws) (x . p) -> fath . flags |= ws;
	}
}
