/*	quota_charges.c	Basser ?.? Melb 4.1	82/07/16	*/

/*
 *	MUSH Share scheduling charges
 */

#if	MUSH
#include "../h/param.h"
#include "../h/share.h"

struct sh_consts	shconsts =
{
	 0		/* flags */
	,35		/* maxnice */
	,4.0		/* usagefactor */
	,2.0		/* ratefactor */
	,880		/* syscall */
	,500		/* bio */
	,11		/* tio */
	,1000		/* tic */
	,10		/* click */
	,300		/* pgin */
	,0		/* current share per usage */
	,0		/* current share per rate */
	,USAGEK		/* usage decay factor */
};
#endif
