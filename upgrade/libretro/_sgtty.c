#include <sgtty.h>

/* map to new sgtty format */
_stty(unit, buf)
	int unit;
	struct sgttyb *buf;
{
	register int oldflags, i;

	oldflags = buf->sg_flags;
	if ((buf->sg_flags & XTABS) == XTABS)
		buf->sg_flags &= ~XTABS, buf->sg_flags |= 2;
	i = stty(unit, buf);
	buf->sg_flags = oldflags;
	return (i);
}

_gtty(unit, buf)
	int unit;
	struct sgttyb *buf;
{
	register int i;

	i = gtty(unit, buf);
	if (buf->sg_flags & 2)
		buf->sg_flags |= XTABS;
	buf->sg_flags &= ~2;
	return (i);
}
