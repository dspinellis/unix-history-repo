/* turbo.h - your comments here */

/* 
 * $Header: /f/osi/h/quipu/RCS/turbo.h,v 7.2 91/03/09 11:54:10 mrose Exp $
 *
 *
 * $Log:	turbo.h,v $
 * Revision 7.2  91/03/09  11:54:10  mrose
 * update
 * 
 * Revision 7.1  91/02/22  09:26:08  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/12/19  09:42:48  mrose
 * *** empty log message ***
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#ifndef QUIPUTURBO
#define QUIPUTURBO

#include "quipu/config.h"
#include "quipu/name.h"

/*
 * this structure represents a generic avl tree node.
 */

typedef struct avlnode {
	caddr_t		avl_data;
	char		avl_bf;
	struct avlnode	*avl_left;
	struct avlnode	*avl_right;
} Avlnode;

#define NULLAVL	((Avlnode *) NULL)

/* balance factor values */
#define LH 	-1
#define EH 	0
#define RH 	1

/* avl routines */
#define avl_getone(x)	(x == 0 ? 0 : (x)->avl_data)
#define avl_onenode(x)	(x == 0 || ((x)->avl_left == 0 && (x)->avl_right == 0))
extern int	avl_insert();
extern caddr_t	avl_delete();
extern caddr_t	avl_find();
extern caddr_t	avl_getfirst();
extern caddr_t	avl_getnext();
extern int	avl_dup_error();
extern int	avl_apply();

/* apply traversal types */
#define AVL_PREORDER	1
#define AVL_INORDER	2
#define AVL_POSTORDER	3
/* what apply returns if it ran out of nodes */
#define AVL_NOMORE	-6

#ifdef TURBO_INDEX

/*
 * this structure represents an attribute index.  the index is composed
 * of the attribute type, a count of the number of different values in
 * the tree, and a pointer to the root of the tree of attribute values.
 * these nodes are linked together (one node for each attribute type being
 * optimized) in an avl tree in each index node.  there is also
 * a tree and count for soundex values of the attribute.
 */

typedef struct index {
					/* entry associated with this index */
					/* sibling => parent		    */
	DN		i_dn;		/* subtree => base		    */

					/* for subtree index: descendants   */
					/* not held locally		    */
	struct entry	**i_nonleafkids;

					/* for both: aliases that escape    */
					/* the scope of the index	    */
	struct entry	**i_nonlocalaliases;

	AttributeType	i_attr;		/* the attribute type		    */
	int		i_count;	/* number of entries in this tree   */
	int		i_scount;	/* number of ents in soundex tree   */
	Avlnode		*i_root;	/* tree of values		    */
	Avlnode		*i_sroot;	/* tree of soundex values	    */
} Index;

#define NULLINDEX	((Index *) 0)

typedef struct {
	struct entry	*ep_entry;
	int		ep_count;
} eptr_node;

typedef struct index_node {
	caddr_t		in_value;
	struct entry	**in_entries;
	int		in_num;
} Index_node;

#define NULLINDEXNODE	((Index_node *) 0)

#define get_subtree_index(x) \
	((Index *) avl_find( subtree_index, (caddr_t) (x), idn_cmp ))

#define get_sibling_index(x) \
	((Index *) avl_find( sibling_index, (caddr_t) (x), idn_cmp ))

#endif /* TURBO_INDEX */
#endif /* QUIPUTURBO */
