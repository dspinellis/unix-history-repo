#ifndef lint
static	char *sccsid = "@(#)misc.c	1.2 83/07/20";
#endif
#include "externs.h"

#define distance(x,y) (abs(x) >= abs(y) ? abs(x) + abs(y)/2 : abs(y) + abs(x)/2)

/* XXX */
range(from, to)
struct ship *from, *to;
{
	register bow1r, bow1c, bow2r, bow2c;
	int stern1r, stern1c, stern2c, stern2r;
	register int bb, bs, sb, ss, result;

	if (!to->file->dir)
		return 30000;
	stern1r = bow1r = from->file->row;
	stern1c = bow1c = from->file->col;
	stern2r = bow2r = to->file->row;
	stern2c = bow2c = to->file->col;
	result = bb = distance(bow2r - bow1r, bow2c - bow1c);
	if (bb < 5) {
		stern2r += dr[to->file->dir];
		stern2c += dc[to->file->dir];
		stern1r += dr[from->file->dir];
		stern1c += dc[from->file->dir];
		bs = distance((bow2r - stern1r), (bow2c - stern1c));
		sb = distance((bow1r - stern2r), (bow1c - stern2c));
		ss = distance((stern2r - stern1r) ,(stern2c - stern1c));
		result = min(bb, min(bs, min(sb, ss)));
	}
	return result;
}

struct ship *
closestenemy(from, side, anyship)
register struct ship *from;
char side, anyship;
{
	register struct ship *sp;
	register char a;
	int olddist = 30000, dist;
	struct ship *closest = 0;

	a = capship(from)->nationality;
	foreachship(sp) {
		if (sp == from)
			continue;
		if (sp->file->dir == 0)
			continue;
		if (a == capship(sp)->nationality && !anyship)
			continue;
		if (side && gunsbear(from, sp) != side)
			continue;
		dist = range(from, sp);
		if (dist < olddist) {
			closest = sp;
			olddist = dist;
		}
	}
	return closest;
}
