/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * treesearch() returns the number of occurrences of a key in a binary tree.
 */
#include "tree.h"
#include "null.h"

treesearch(p, key)
	TREE *p;			/* current node pointer */
	char *key;			/* pointer to key */
{
	int comp;			/* compare key values */
	int strcmp();			/* string comparison */

	if (p != NULL)
		{
		if ((comp = strcmp(key, p->key)) < 0)
			return(treesearch(p->left, key));
		else if (comp > 0)
			return(treesearch(p->right, key));
		else
			return(p->count);
		}
	return(0);
}
