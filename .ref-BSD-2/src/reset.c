/* Copyright (c) 1979 Regents of the University of California */
/*
 * reset - set the teletype mode bits to be sensible, erase to ^H, kill to @
 *
 * Kurt Shoens
 *
 * Very useful after crapping out in raw.
 */
#include <sgtty.h>

main() {
	struct sgttyb buf;
	gtty(2, &buf);
	buf.sg_flags |= XTABS|ECHO|CRMOD|ANYP;
	buf.sg_flags &= ~(RAW|CBREAK|VTDELAY);
	buf.sg_erase = 010;
	buf.sg_kill = '@';
	ioctl(2, TIOCSETN, &buf);
}
