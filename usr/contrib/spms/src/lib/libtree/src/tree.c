/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * tree() searchs for a key in a binary tree. If the search is
 * unsuccessful a new node is added to the tree.
 */
#include "tree.h"
#include "null.h"

TREE *
tree(p, key)
	TREE *p;			/* current node pointer */
	char *key;			/* pointer to key */
{
	TREE *talloc();			/* allocate a binary tree node */
	char *strsav();			/* save a string */
	int comp;			/* compare key values */
	int strcmp();			/* string comparison */

	if (p == NULL)
		{			/* a new key has arrived */
		if ((p = talloc()) == NULL ||
		    (p->key = strsav(key)) == NULL)
			fatal("out of memory");
		p->count = 1;
		p->left = p->right = NULL;
		}
	else if ((comp = strcmp(key, p->key)) < 0)
		p->left = tree(p->left, key);
	else if (comp > 0)
		p->right = tree(p->right, key);
	else if (comp == 0)
		p->count++;
	return(p);
}



/*
 * talloc() allocates memory for a binary tree node.
 */
static TREE *
talloc()
{
	char *malloc();			/* memory allocator */

	return((TREE *) malloc(sizeof(TREE)));
}
