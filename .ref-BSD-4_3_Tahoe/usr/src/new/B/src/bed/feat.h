/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: feat.h,v 2.4 85/08/22 16:02:40 timo Exp $ */

/*
 * B editor -- #define features, to make it easy to turn them off.
 */

#define BTOP 1 /* Editor used as front-end to interpreter */
	/*
	 * (This used to be an optional feature of the editor, but is now
	 * the only officially acknowledged way of using the B system.
	 * Non-UNIX systems may have no choice but to turn it off, because
	 * it heavily depends on features like forks and pipes.
	 */

#ifndef SMALLSYS
/*
 * The #define SMALLSYS squeezes out some lesser important debugging
 * code, while leaving out the following #defines turns off various
 * features which can be missed (according to the author).
 * They are roughly sorted on amount of code saved, greatest
 * saving first.
 */

#define SAVEBUF 1 /* Save Copy Buffer on file between edit sessions */
#define USERSUGG 1 /* Give suggestions for user-defined commands */
#define SAVEPOS 1 /* Save focus position between edit sessions */
#define FILEARGS 1 /* Allow 'stand-alone' editor to edit single units */
#define RECORDING 1 /* [record] and [playback] commands */
#define SCROLLBAR 1 /* Show scroll bar if unit > screen */
#define SHOWBUF 1 /* Shows contents of copy buffer if locked */

/*
 * The following feature used to fit, even on a (our) PDP-11/45.
 * And as it is very useful for novice users, you might try to let it stay.
 */
#define HELPFUL 1 /* Print help blurb on ESC-? or ? */

#endif !SMALLSYS

/*
 * On compilers that implement C according to the Kernighan and Ritchie book,
 * but not the unix v7 extensions, turn off the following definition.
 */

#define STRUCTASS 1 /* C compiler knows structure assignment */
