/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * treerm() removes a node from a binary tree. If key is null, the entire
 * tree is removed.
 */
#include "tree.h"
#include "null.h"

static TREE *q;				/* node to be deleted */

TREE *
treerm(p, key)
	TREE *p;			/* current node pointer */
	char *key;			/* pointer to key */
{
	int comp;			/* compare key values */
	int strcmp();			/* string comparison */
	TREE *rmtl();			/* delete left subtree rightmost node */

	if (p != NULL)
		if (key == NULL)
			{
			/* remove entire tree */
			p->left = treerm(p->left, key);
			p->right = treerm(p->right, key);
			free(p->key);
			free((char *) p);
			p = NULL;
			}
		else if ((comp = strcmp(key, p->key)) < 0)
			{
			if (p->left != NULL)
				p->left = treerm(p->left, key);
			}
		else if (comp > 0)
			{
			if (p->right != NULL)
				p->right = treerm(p->right, key);
			}
		else	{
			q = p;
			if (q->right == NULL)
				p = q->left;
			else if (q->left == NULL)
				p = q->right;
			else
				p->left = rmtl(q->left);
			free(q->key);
			free((char *) q);
			}
	return(p);
}



/*
 * rmtl() descends along the rightmost branch of the left subtree of the
 * element q to be deleted, and then it replaces the relevant information
 * (key and count) in q by the corresponding values of the rightmost
 * component r of that left subtree, after which r may be removed.
 */
TREE *
rmtl(r)
	TREE *r;			/* leaf to be removed */
{
	if (r->right != NULL)
		{
		r->right = rmtl(r->right);
		}
	else	{
		q->key = r->key;
		q->count = r->count;
		q = r;
		r = r->left;
		}
	return(r);
}
