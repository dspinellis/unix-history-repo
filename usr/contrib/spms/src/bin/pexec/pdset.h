/* $Header$ */

/*
 * Project directory set definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Project directory type label block
 */
typedef struct _typblk
	{
	short t_exist;			/* type label exist for this dir? */
	short t_prior;			/* type label priority */
	} TYPBLK;
/*
 * Project directory attribute block
 */
typedef struct _pdset
	{
	char *ppath;			/* project directory project pathname */
	char *rpath;			/* project directory regular pathname */
	char *project;			/* project directory's project */
	TYPBLK *typblk;			/* project directory's type labels */
	} PDSET;
/*
 * Project directory type label statistics (for each set of type labels)
 */
typedef struct _types
	{
	char *t_name;			/* type label name */
	short t_ntl;			/* number of type labels */
	short t_itlp;			/* initial type label priority */
	short t_sort;			/* sort on this type label? */
	} TYPES;
