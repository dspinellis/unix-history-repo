#ifndef lint
static	char *sccsid = "@(#)main.c	1.1 83/03/17";
#endif
#include "externs.h"

#define distance(x,y) (abs(x) >= abs(y) ? abs(x) + abs(y)/2 : abs(y) + abs(x)/2)

range(fromship, toship)
register int fromship, toship;
{
    int bow1r, bow1c, bow2r, bow2c;
    int stern1r, stern1c, stern2c, stern2r;
    register int bb, bs, sb, ss, result;

    if (fromship > scene[game].vessels
	    || toship > scene[game].vessels) /* just in case */
	return(30000);
    if (!pos[toship].dir)
	return(30000);
    stern1r = bow1r = pos[fromship].row;
    stern1c = bow1c = pos[fromship].col;
    stern2r = bow2r = pos[toship].row;
    stern2c = bow2c = pos[toship].col;
    result = bb = distance((bow2r - bow1r), (bow2c - bow1c));
    if (bb < 5)
	{
	drdc(&stern2r, &stern2c, pos[toship].dir);
	drdc(&stern1r, &stern1c, pos[fromship].dir);
	bs = distance((bow2r - stern1r) ,(bow2c - stern1c));
	sb = distance((bow1r - stern2r) ,(bow1c - stern2c));
	ss = distance((stern2r - stern1r) ,(stern2c - stern1c));
	result = min(bb, min(bs, min(sb, ss)));
	}
    return(result);
}

drdc(dr, dc, dir)
register int *dr, *dc;
int dir;
{
    switch (dir)
	{
	case 1:
	    (*dr)++;
	    break;
	case 2:
	    (*dr)++;
	    (*dc)--;
	    break;
	case 3:
	    (*dc)--;
	    break;
	case 4:
	    (*dr)--;
	    (*dc)--;
	    break;
	case 5:
	    (*dr)--;
	    break;
	case 6:
	    (*dr)--;
	    (*dc)++;
	    break;
	case 7:
	    (*dc)++;
	    break;
	case 8:
	    (*dr)++;
	    (*dc)++;
	    break;
	}
}
