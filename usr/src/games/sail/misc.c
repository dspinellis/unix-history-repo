#ifndef lint
static	char *sccsid = "@(#)misc.c	2.1 83/10/31";
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
		return -1;
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

angle(dr, dc)
register dr, dc;
{
	register i;

	if (dc >= 0 && dr > 0)
		i = 0;
	else if (dr <= 0 && dc > 0)
		i = 2;
	else if (dc <= 0 && dr < 0)
		i = 4;
	else
		i = 6;
	dr = abs(dr);
	dc = abs(dc);
	if ((i == 0 || i == 4) && dc * 2.4 > dr) {
		i++;
		if (dc > dr * 2.4)
			i++;
	} else if ((i == 2 || i == 6) && dr * 2.4 > dc) {
		i++;
		if (dr > dc * 2.4)
			i++;
	}
	return i % 8 + 1;
}

gunsbear(from, to)		/* checks for target bow or stern */
register struct ship *from, *to;
{
	int Dr, Dc, i;
	register ang;

	Dr = from->file->row - to->file->row;
	Dc = to->file->col - from->file->col;
	for (i = 2; i; i--) {
		if ((ang = angle(Dr, Dc) - from->file->dir + 1) < 1)
			ang += 8;
		if (ang >= 2 && ang <= 4)
			return 'r';
		if (ang >= 6 && ang <= 7)
			return 'l';
		Dr += dr[to->file->dir];
		Dc += dc[to->file->dir];
	}
	return 0;
}

portside(from, on, quick)
register struct ship *from, *on;
int quick;			/* returns true if fromship is */
{				/* shooting at onship's starboard side */
	register ang;
	register Dr, Dc;

	Dr = from->file->row - on->file->row;
	Dc = on->file->col - from->file->col;
	if (quick == -1) {
		Dr += dr[on->file->dir];
		Dc += dc[on->file->dir];
	}
	ang = angle(Dr, Dc);
	if (quick != 0)
		return ang;
	ang = (ang + 4 - on->file->dir - 1) % 8 + 1;
	return ang < 5;
}

colours(sp)
register struct ship *sp;
{
	register char flag;

	if (sp->file->struck)
		flag = '!';
	if (sp->file->explode)
		flag = '#';
	if (sp->file->sink)
		flag = '~';
	if (sp->file->struck)
		return flag;
	flag = *countryname[capship(sp)->nationality];
	return sp->file->FS ? flag : tolower(flag);
}

#ifdef notdef
#define PI 3.1415926535

float contable[8] =
	{ 1.5708, 0.7854, 0.0, -0.7854, -1.5708, -2.3562, -PI, 2.3562 };

int tantable[40] = {
	0,100,197,291,381,
	464,540,610,675,733,
	785,833,876,915,951,
	983,1012,1039,1064,1086,
	1107,1126,1144,1161,1176,
	1190,1204,1216,1227,1239,
	1249,1259,1268,1277,1285,
	1293,1300,1307,1313,1470
};

double
arctan(y,x)
int y,x;
{
	int sx, sy;
	register int index;

	sy = y < 0 ? -1 : 1;
	sx = x < 0 ? -1 : 1;
	y *= sy;
	x *= sx;
	if (!x)
		return (double) PI/2 * sy;
	index = 10 * y / x + 0.5;
	if (index > 39)
		index = 39;
	return (double)
		sy * (sx < 0 ? PI : 0 + sx*((float)tantable[index]/1000));
}
#endif
