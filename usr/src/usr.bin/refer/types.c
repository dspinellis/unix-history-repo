#ifndef lint
static char *sccsid = "@(#)types.c	4.1 (Berkeley) 5/6/83";
#endif

#if LONG
# define ptr long
# define uptr long
# define getp getl
# define putp putl
# define MONE -1L
extern long getl();
#else
# define ptr int
# define uptr unsigned
# define getp getw
# define putp putw
# define MONE -1
#endif
