/* $Header: getarg.h,v 1.1 85/03/14 15:32:56 nicklin Exp $ */

/*
 * Get command line argument
 *
 * Author: Peter J. Nicklin
 */

/*
 * Argument syntax: `-xargument' or `-x argument'
 */
#define GETARG(p) ((p[1] != '\0') ? ++p : (--argc, *++argv))

/*
 * Argument syntax: `-xargument'
 *
 * #define GETARG(p) (++p)
 */
