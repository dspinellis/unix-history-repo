/* $Header$ */

/*
 * Binary tree definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * The basic node for a binary tree.
 */
typedef struct tnode
	{
	char *key;			/* points to a key */
	int count;			/* number of occurrences */
	struct tnode *left;		/* left child */
	struct tnode *right;		/* right child */
	} TREE;
/*
 * Functions defined for binary trees
 */
extern int treesearch();		/* tree search */
extern TREE *tree();			/* tree search and insert */
extern TREE *treerm();			/* tree deletion */
