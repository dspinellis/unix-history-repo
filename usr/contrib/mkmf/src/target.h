/* $Header: target.h,v 1.1 85/03/14 15:38:49 nicklin Exp $ */

/*
 * Target definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Target struct
 */
typedef struct _target
	{
	int type;			/* prog, lib, or other target type */
	int dest;			/* target destination flag */
	} TARGET;
