/* Copyright (c) 1979 Regents of the University of California */

/* static	char sccsid[] = "@(#)align.h 1.2 %G%"; */

    /*
     *	alignment of various types in bytes.
     *	sizes are found using sizeof( type ).
     */
#ifdef vax
#   define A_CHAR	1
#   define A_INT	4
#   define A_FLOAT	4
#   define A_DOUBLE	4
#   define A_LONG	4
#   define A_SHORT	2
#   define A_POINT	4
#   define A_STRUCT	1
#   define A_STACK	4
#   define A_FILET	4
#   define A_SET	4
#   define A_MIN	1
#   define A_MAX	4
#endif vax
#ifdef mc68000
#   define A_CHAR	1
#   define A_INT	2
#   define A_FLOAT	2
#   define A_DOUBLE	2
#   define A_LONG	2
#   define A_SHORT	2
#   define A_POINT	2
#   define A_STRUCT	2
#   define A_STACK	2
#   define A_FILET	2
#   define A_SET	2
#   define A_MIN	1
#   define A_MAX	2
#endif mc68000
