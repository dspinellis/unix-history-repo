/* Copyright (c) 1979 Regents of the University of California */

/* static char sccsid[] = "@(#)machdep.h 1.2 3/6/81"; */

#ifdef VAX
#define pushaddr push4
#define popaddr (char *)pop4
#else
#define pushaddr push2
#define popaddr (char *)pop2
#endif VAX
