/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */
#include "null.h"
#include "ptree.h"
/*
 * ptree() searchs for a project directory name in a binary tree. If the
 * search is unsuccessful, a new node is added to the tree.
 */
PTREE *
ptree(p, alias, pd1, pd2)
	PTREE *p;			/* current node pointer */
	char *alias;			/* project directory alias */
	char *pd1;			/* project directory pathname */
	char *pd2;			/* project directory pathname */
{
	char *strsav();			/* save a string somewhere */
	int comp;			/* compare key values */
	int strcmp();			/* string comparison */
	PTREE *palloc();		/* allocate a tree node */

	if (p == NULL)
		{			/* a new alias has arrived */
		if ((p = palloc()) == NULL || (p->alias = strsav(alias)) == NULL)
			goto nomemory;
		if (pd1 != NULL)
			{
			if ((p->pd1 = strsav(pd1)) == NULL)
				goto nomemory;
			p->pd2 = NULL;
			}
		else	{
			if ((p->pd2 = strsav(pd2)) == NULL)
				goto nomemory;
			p->pd1 = NULL;
			}
		p->left  = p->right = NULL;
		}
	else if ((comp = strcmp(alias, p->alias)) < 0)
		p->left = ptree(p->left, alias, pd1, pd2);
	else if (comp > 0)
		p->right = ptree(p->right, alias, pd1, pd2);
	else if (comp == 0)
		if ((p->pd2 = strsav(pd2)) == NULL)
			{
nomemory:		warn("out of memory");
			exit(2);
			}
	return(p);
	}



/*
 * ptreerm() removes a project directory tree.
 */
void
ptreerm(p)
	PTREE *p;			/* current node pointer */
{
	if (p != NULL)
		{
		if (p->left != NULL)
			ptreerm(p->left);
		if (p->right != NULL)
			ptreerm(p->right);
		free(p->alias);
		if (p->pd1 != NULL)
			free(p->pd1);
		if (p->pd2 != NULL)
			free(p->pd2);
		free((char *) p);
		}
}



/*
 * palloc allocates memory for a project tree node.
 */
static PTREE *
palloc()
{
	char *malloc();			/* memory allocator */

	return((PTREE *) malloc(sizeof(PTREE)));
	}
