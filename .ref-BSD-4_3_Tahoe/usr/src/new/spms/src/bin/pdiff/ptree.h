/* $Header$ */

/*
 * project comparison tree definitions
 *
 * Author: Peter J. Nicklin
 */

typedef struct _pnode
	{
	char   *alias;			/* project directory alias */
	char   *pd1;			/* project directory pathname */
	char   *pd2;			/* project directory pathname */
	struct _pnode *left;		/* left child */
	struct _pnode *right;		/* right child */
	} PTREE;
