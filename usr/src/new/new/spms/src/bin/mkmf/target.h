/* $Header$ */

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
