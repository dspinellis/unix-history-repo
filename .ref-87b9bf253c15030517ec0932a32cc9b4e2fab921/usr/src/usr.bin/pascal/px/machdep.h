/* Copyright (c) 1979 Regents of the University of California */

/* static char sccsid[] = "@(#)machdep.h 1.3 %G%"; */

#ifdef ADDR32
#define pushaddr push4
#define popaddr (char *)pop4
#endif ADDR32
#ifdef ADDR16
#define pushaddr push2
#define popaddr (char *)pop2
#endif ADDR16
