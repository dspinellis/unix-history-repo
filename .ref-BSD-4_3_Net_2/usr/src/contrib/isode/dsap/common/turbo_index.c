/* turbo_index.c */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/turbo_index.c,v 7.1 91/02/22 09:20:38 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/turbo_index.c,v 7.1 91/02/22 09:20:38 mrose Interim $
 *
 *
 * $Log:	turbo_index.c,v $
 * Revision 7.1  91/02/22  09:20:38  mrose
 * Interim 6.8
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


#include <stdio.h>
#include "quipu/config.h"
#include "quipu/attr.h"
#include "quipu/entry.h"
#include "quipu/turbo.h"
#include "logger.h"

#ifdef TURBO_INDEX

extern LLog * log_dsap;

AttributeType	*turbo_index_types;	/* array of attributes to optimize */
int		turbo_index_num;	/* number of attributes to optimize */
Avlnode		*subtree_index;		/* array of subtree indexes */
Avlnode		*sibling_index;		/* array of sibling indexes */
int		optimized_only;		/* only allow indexed searches */

static index_cmp( a, b )
Index_node	*a;
Index_node	*b;
{
	return( AttrV_cmp( (AttributeValue) a->in_value,
	    (AttributeValue) b->in_value ) );
}

static sindex_cmp( a, b )
Index_node	*a;
Index_node	*b;
{
	return( strcmp( (char *) a->in_value, (char *) b->in_value ) );
}

index_soundex_cmp( code, node )
char		*code;
Index_node	*node;
{
	return( strcmp( code, (char *) node->in_value ) );
}

index_soundex_prefix( code, node, len )
char		*code;
Index_node	*node;
int		len;
{
	return( strncmp( code, (char *) node->in_value, len ) );
}

substring_prefix_cmp( val, node, len )
AttributeValue	val;
Index_node	*node;
int		len;
{
	return(strncmp((char *) val->av_struct,
	    (char *) (((AttributeValue) node->in_value)->av_struct), len));
}

substring_prefix_case_cmp( val, node, len )
AttributeValue	val;
Index_node	*node;
int		len;
{
	return(lexnequ((char *) val->av_struct,
	    (char *) (((AttributeValue) node->in_value)->av_struct), len));
}

indexav_cmp( av, node )
AttributeValue	av;
Index_node	*node;
{
	return( AttrV_cmp( av, (AttributeValue) node->in_value ) );
}

static index_dup( node, dup )
Index_node	*node;
Index_node	*dup;
{
	int	i, j;
	Entry	tmp1, tmp2;

	/* check for duplicates */
	for ( i = 0; i < node->in_num; i++ ) {
		if ( node->in_entries[ i ] == dup->in_entries[ 0 ] )
			return( NOTOK );
		if (node->in_entries[i] > dup->in_entries[0])
			break;
	}

	node->in_entries = (struct entry **) realloc( (char *)node->in_entries, 
	    (unsigned) (sizeof(struct entry *) * (node->in_num + 1) ));
	node->in_num++;

	tmp1 = dup->in_entries[0];
	for (j = i; j < node->in_num; j++) {
		tmp2 = node->in_entries[j];
		node->in_entries[j] = tmp1;
		tmp1 = tmp2;
	}

	return( NOTOK );
}

static indexav_free( node )
Index_node	*node;
{
	AttrV_free( (AttributeValue) node->in_value );
	free( (char *) node->in_entries );
	free( (char *) node );
}

static soundex_free( node )
Index_node	*node;
{
	free( (char *) node->in_value );
	free( (char *) node->in_entries );
	free( (char *) node );
}

static index_free( pindex )
Index	*pindex;
{
	dn_free( pindex->i_dn );
	(void) avl_free( pindex->i_root, indexav_free );
	(void) avl_free( pindex->i_sroot, soundex_free );
	free( (char *) pindex );
}

/* ARGSUSED */
static i_dup( a )
Index	*a;
{
	return( NOTOK );
}

static i_cmp( a, b )
Index	*a;
Index	*b;
{
	return( dn_order_cmp( a->i_dn, b->i_dn ) );
}

idn_cmp( a, b )
DN	a;
Index	*b;
{
	return( dn_order_cmp( a, b->i_dn ) );
}

/*
 * siblings - return 1 if a and b are siblings, 0 otherwise
 */

static siblings( a, b )
DN      a;
DN      b;
{
        for ( ; a && b; a = a->dn_parent, b = b->dn_parent ) {
                if ( dn_cmp( a, b ) == NOTOK ) {
                        if ( a->dn_parent == NULLDN && b->dn_parent == NULLDN )
                                return( 1 );
                        else
                                return( 0 );
                }
        }

        if ( a != NULLDN || b != NULLDN )
                return( 0 );

        return( 1 );
}

/*
 * prefix - returns the following:
 *      -1      =>      a is a prefix of b
 *      0       =>      a and b are equal
 *      1       =>      b is a prefix of a
 *      2       =>      neither a nor b is a prefix of the other
 */

static prefix( a, b )
DN      a;
DN      b;
{
        for ( ; a && b; a = a->dn_parent, b = b->dn_parent )
                if ( dn_comp_cmp( a, b ) == NOTOK )
                        return( 2 );    /* neither is prefix */

        if ( a == NULLDN && b == NULLDN )
                return( 0 );            /* they are equal */
        else if ( a == NULLDN )
                return( -1 );           /* a is a prefix of b */
        else
                return( 1 );            /* b is a prefix of a */
}

static Index *new_index( dn )
DN	dn;
{
	Index	*pindex;
	int	i;

	pindex = (Index *) malloc( (unsigned) (sizeof(Index) * turbo_index_num ));

	for ( i = 0; i < turbo_index_num; i++ ) {
		pindex[ i ].i_attr = turbo_index_types[ i ];
		pindex[ i ].i_count = 0;
		pindex[ i ].i_scount = 0;
		pindex[ i ].i_root = NULLAVL;
		pindex[ i ].i_sroot = NULLAVL;
		pindex[ i ].i_nonleafkids = (struct entry **) 0;
		pindex[ i ].i_nonlocalaliases = (struct entry **) 0;
		pindex[ i ].i_dn = dn_cpy( dn );
	}

	return( pindex );
}

#ifdef DEBUG

/* ARGSUSED */
static print_soundex_node( n, ps )
Index_node	*n;
int		ps;
{
	int	i;

	(void) printf( "\t(%s)\n",n->in_value );
	for ( i = 0; i < n->in_num; i++ )
		(void) printf( "\t\t%s\n",
		    n->in_entries[i]->e_name->rdn_av.av_struct);
	return( OK );
}

#endif DEBUG

/*
 * add_nonlocalalias - add entry e to the list of nonlocal aliases
 * kept with index index.
 */

static add_nonlocalalias( e, pindex )
Index		*pindex;
struct entry	*e;
{
	struct entry	**tmp;
	int		i;

	if ( pindex == NULLINDEX )
		return;

	if ( pindex->i_nonlocalaliases == (struct entry **) 0 ) {
		pindex->i_nonlocalaliases = (struct entry **) malloc(
		    sizeof(struct entry *) * 2 );
		pindex->i_nonlocalaliases[ 0 ] = (struct entry *) 0;
	}

	/* first, check for duplicates */
	for ( i = 0, tmp = pindex->i_nonlocalaliases;
	    *tmp != (struct entry *) 0; 
	    tmp++, i++ ) {
		if ( *tmp == e )
			return;
	}

	pindex->i_nonlocalaliases = (struct entry **) realloc(
	    (char *)pindex->i_nonlocalaliases, (unsigned) sizeof(struct entry *) * (i + 2) );

	pindex->i_nonlocalaliases[ i ] = e;
	pindex->i_nonlocalaliases[ i + 1 ] = NULLENTRY;
}

/* 
 * addnonleafkids - add entry e to the list of nonlocal kids kept
 * in index index.
 */

static add_nonleafkid( e, pindex )
Index		*pindex;
struct entry	*e;
{
	struct entry	**tmp;
	int		i;

	if ( pindex == NULLINDEX )
		return;

	if ( pindex->i_nonleafkids == (struct entry **) 0 ) {
		pindex->i_nonleafkids = (struct entry **) malloc( sizeof(Entry)
		    * 2 );
		pindex->i_nonleafkids[ 0 ] = (struct entry *) 0;
	}

	/* first, check for duplicates */
	for ( i = 0, tmp = pindex->i_nonleafkids; *tmp != (struct entry *) 0; 
	    tmp++, i++ ) {
		if ( *tmp == e )
			return;
	}

	pindex->i_nonleafkids = (struct entry **) realloc(
	    (char *)pindex->i_nonleafkids, (unsigned)sizeof(struct entry *) * (i + 2) );

	pindex->i_nonleafkids[ i ] = e;
	pindex->i_nonleafkids[ i + 1 ] = NULLENTRY;
}

/*
 * delete_nonleafkid - delete a reference to nonleaf child entry e
 * in index index.
 */

static delete_nonleafkid( e, pindex )
struct entry	*e;
Index		*pindex;
{
	int		i, j;
	struct entry	**tmp;

	if ( pindex->i_nonleafkids == (struct entry **) 0 ) {
		LLOG(log_dsap, LLOG_EXCEPTIONS, ("Index has no non-leaf entries"));
		return;
	}

	for ( i = 0, tmp = pindex->i_nonleafkids; *tmp; tmp++, i++ )
		if ( *tmp == e ) break;

	if ( *tmp == (struct entry *) 0 ) {
		LLOG(log_dsap, LLOG_EXCEPTIONS, ("Cannot find non-leaf entry"));
		return;
	}

	for ( j = i + 1; pindex->i_nonleafkids[ j ]; j++ )
		pindex->i_nonleafkids[ j - 1 ] = pindex->i_nonleafkids[ j ];

	return;
}

/*
 * delete_nonlocalalias - delete a reference to nonlocal alias entry e
 * in index index.
 */

static delete_nonlocalalias( e, pindex )
struct entry	*e;
Index		*pindex;
{
	int		i, j;
	struct entry	**tmp;

	if ( pindex->i_nonlocalaliases == (struct entry **) 0 ) {
		LLOG(log_dsap, LLOG_EXCEPTIONS, ("index has no non-local aliases"));
		return;
	}

	for ( i = 0, tmp = pindex->i_nonlocalaliases; *tmp; tmp++, i++ )
		if ( *tmp == e ) break;

	if ( *tmp == (struct entry *) 0 ) {
		LLOG(log_dsap, LLOG_EXCEPTIONS, ("Cannot find non-local alias"));
		return;
	}

	for ( j = i + 1; pindex->i_nonlocalaliases[ j ]; j++ )
		pindex->i_nonlocalaliases[ j - 1 ] =
		    pindex->i_nonlocalaliases[ j ];

	return;
}

/*
 * turbo_attr_insert -- mark entry e as having attribute value val in
 * index for attribute type attr.
 */

static turbo_attr_insert( pindex, e, attr, values )
Index		*pindex;
Entry		e;
AttributeType	attr;
AV_Sequence	values;
{
	int		i;
	AV_Sequence	av;
	Index_node	*imem;
	char		*word, *code;
	char		*next_word();
	IFP		approxfn();
	int		soundex_match();

	/* find the appropriate index */
	for ( i = 0; i < turbo_index_num; i++ )
		if ( AttrT_cmp( pindex[ i ].i_attr, attr ) == 0 )
			break;

	if ( i == turbo_index_num ) {
		LLOG( log_dsap, LLOG_EXCEPTIONS, ("turbo_attr_insert: cannot find optimized attribute") );
		return;
	}

	/* insert all values */
	for ( av = values; av != NULLAV; av = av->avseq_next ) {
		imem = (Index_node *) malloc( sizeof(Index_node) );
		imem->in_value = (caddr_t) AttrV_cpy( &av->avseq_av );
		imem->in_entries = (struct entry **) malloc( sizeof(struct
		    entry *) );
		imem->in_entries[ 0 ] = (struct entry *) e;
		imem->in_num = 1;

		/*
		 * Now we insert the entry into the appropriate index.
		 * If the attribute has a soundex approximate matching
		 * function, we insert the entry into the appropriate
		 * soundex index for that attribute.
		 */

		/* a return of OK means it was the first one inserted */
		if ( avl_insert( &pindex[ i ].i_root, (caddr_t) imem, index_cmp,
		    index_dup ) == OK ) {
			pindex[ i ].i_count++;
			imem = NULLINDEXNODE;
		} else {
			AttrV_free( (AttributeValue) imem->in_value );
			free( (char *) imem->in_entries );
			free( (char *) imem );
		}

		if ( approxfn( attr->oa_syntax ) != soundex_match )
			continue;

		for ( word = (char *) av->avseq_av.av_struct; word;
		    word = next_word( word ) ) {
			code = NULL;
			soundex( word, &code );
			imem = (Index_node *) malloc( sizeof(Index_node) );
			imem->in_value = (caddr_t) code;
			imem->in_entries = (struct entry **) malloc(
			    sizeof(struct entry *) );
			imem->in_entries[ 0 ] = (struct entry *) e;
			imem->in_num = 1;

			if ( avl_insert( &pindex[i].i_sroot, (caddr_t) imem, sindex_cmp,
			    index_dup ) == OK ) {
				pindex[ i ].i_scount++;
			} else {
				free( (char *) imem->in_value );
				free( (char *) imem->in_entries );
				free( (char *) imem );
			}
		}
	}
}

/*
 * turbo_add2index -- search through the given entry's attribute list for
 * attrs to optimize. if an attr to optimize is found, we add that attribute 
 * along with a pointer to the corresponding entry to the appropriate 
 * attribute index.
 */

turbo_add2index( e )
Entry	e;		/* the entry these attrs belong to */
{
	Entry		parent, tmp;
	Attr_Sequence	as, entry_find_type();
	DN		pdn, dn, tmpdn;
	int		opt, pcmp, nonleaf;
	Index		*subindex;
	Index		*sibindex;
	extern AttributeType at_masterdsa, at_slavedsa;

	if ( sibling_index == (Avlnode *) 0 && subtree_index == (Avlnode *) 0 )
		return;

	nonleaf = (entry_find_type( e, at_masterdsa ) != NULLATTR ||
	    entry_find_type( e, at_slavedsa ) != NULLATTR);

	parent = e->e_parent;
	pdn = get_copy_dn( parent );
	dn = get_copy_dn( e );

	sibindex = get_sibling_index( pdn );

	/* for each attribute in the entry... */
	for ( as = e->e_attributes; as != NULLATTR; as = as->attr_link ) {
		opt = turbo_isoptimized( as->attr_type );

		/* sibling index */
		if ( sibindex && opt ) {
			(void) turbo_attr_insert( sibindex, e, as->attr_type,
			    as->attr_value );

			if ( e->e_alias && (! siblings( dn, sibindex->i_dn )) )
				add_nonlocalalias( e, sibindex );
		}

		/* could be a subtree index with all parents & this node */
		for ( tmp = e; tmp->e_parent; tmp = tmp->e_parent ) {
			tmpdn = get_copy_dn( tmp );
			if ( subindex = get_subtree_index( tmpdn ) ) {
				if ( opt ) {
					(void) turbo_attr_insert( subindex, e,
					    as->attr_type, as->attr_value );

					if ( e->e_alias ) {
						pcmp = prefix( subindex->i_dn,
						    e->e_alias );
						if ( pcmp != 0 && pcmp != -1 ) {
							add_nonlocalalias(e,
							    subindex);
						}
					}
				}

				if ( nonleaf ) {
					add_nonleafkid(e, subindex);
					nonleaf = 0;
				}
			}
			dn_free( tmpdn );
		}
	}

	dn_free( dn );
	dn_free( pdn );
	return;
}

/*
 * turbo_attr_delete -- delete entry e from index for values of attribute
 * attr.
 */

static turbo_attr_delete( pindex, e, attr, values )
Index		*pindex;
Entry		e;
AttributeType	attr;
AV_Sequence	values;
{
	int		i, j, k;
	AV_Sequence	av;
	Index_node	*node, *imem;
	struct entry	**p;
	char		*code, *word;
	char		*next_word();

	/* find the appropriate index */
	for ( i = 0; i < turbo_index_num; i++ )
		if ( AttrT_cmp( pindex[ i ].i_attr, attr ) == 0 )
			break;

	if ( i == turbo_index_num ) {
		LLOG( log_dsap, LLOG_EXCEPTIONS, ("turbo_attr_delete: cannot find optimized attribute") );
		return;
	}

	/* delete all values */
	for ( av = values; av != NULLAV; av = av->avseq_next ) {
		node = (Index_node *) avl_find( pindex[ i ].i_root, 
		    (caddr_t) &av->avseq_av, (IFP)indexav_cmp );

		if ( node == NULLINDEXNODE ) {
			LLOG( log_dsap, LLOG_EXCEPTIONS, ("Optimized attribute value not found!\n") );
			continue;
		}

		/* find the entry we want to delete */
		p = node->in_entries;
		for ( j = 0; j < node->in_num; j++, p++ )
			if ( *p == (struct entry *) e )
				break;

		if ( j == node->in_num ) {
			LLOG( log_dsap, LLOG_EXCEPTIONS, ("Optimized av entry not found") );
			continue;
		}

		if ( --(node->in_num) == 0 ) {
			imem = (Index_node *) avl_delete( &pindex[ i ].i_root,
			    (caddr_t) &av->avseq_av, indexav_cmp );
			( void ) AttrV_free( (AttributeValue) imem->in_value );
			( void ) free( (char *) imem->in_entries );
			( void ) free( (char *) imem );
			pindex[ i ].i_count--;
		} else {
			for ( k = j; k < node->in_num; k++ )
				node->in_entries[ k ] =
				    node->in_entries[ k + 1 ];
			node->in_entries = (struct entry **)
			    realloc( (char *) node->in_entries, (unsigned) node->in_num
			    * sizeof(struct entry *) );
		}

		/* if there's a soundex index, delete from that too */
		if ( pindex[i].i_sroot == NULLAVL )
			continue;

		for ( word = av->avseq_av.av_struct; word != NULL;
		    word = next_word( word ) ) {
			code = NULL;
			soundex( word, &code );

			/*
			 * not finding the node is ok if the entry happens
			 * to be the only one with this code and was deleted
			 * on a previous pass through this loop.  we hope.
			 */

			if ((imem = (Index_node *) avl_find(pindex[i].i_sroot,
			    code, index_soundex_cmp)) == NULLINDEXNODE) {
				free(code);
				continue;
			}

			/* find the entry */
			p = imem->in_entries;
			for ( j = 0; j < imem->in_num; j++, p++ )
				if ( *p == (struct entry *) e )
					break;

			/*
			 * not finding the entry is this is ok for the soundex
			 * index since an entry can appear more than once and
			 * might have already been deleted on a previous pass
			 */

			if ( j == imem->in_num )
				continue;

			if ( --(imem->in_num) == 0 ) {
				imem = (Index_node *)
				    avl_delete( &pindex[ i ].i_sroot,
				    (caddr_t) code, index_soundex_cmp );

				free( (char *) imem->in_value );
				free( (char *) imem->in_entries );
				free( (char *) imem );
			} else {
				for ( k = j; k < imem->in_num; k++ )
					imem->in_entries[ k ] =
					    imem->in_entries[ k+1 ];

				imem->in_entries = (struct entry **)
				    realloc( (char *) imem->in_entries,
				    (unsigned) imem->in_num * sizeof(struct entry *) );
			}
			free(code);
		}
	}
}

/*
 * turbo_index_delete -- delete attribute index entries for the given
 * entry from the attribute index associated with the entry's parent
 * node.
 */

turbo_index_delete( e )
Entry	e;
{
	Entry		parent, tmp;
	Attr_Sequence	as;
	DN		pdn, dn, tmpdn;
	Index		*subindex;
	Index		*sibindex;
	int		pcmp, nonleaf;

	if ( subtree_index == NULLAVL && sibling_index == NULLAVL )
		return;

	nonleaf = (! isleaf(e));

	parent = e->e_parent;
	pdn = get_copy_dn( parent );
	dn = get_copy_dn( e );

	sibindex = get_sibling_index( pdn );

	/* for each attribute in the entry... */
	for ( as = e->e_attributes; as != NULLATTR; as = as->attr_link ) {
		if ( turbo_isoptimized( as->attr_type ) == 0 )
			continue;

		/* sibling index */
		if ( sibindex ) {
			(void) turbo_attr_delete( sibindex, e, as->attr_type,
			    as->attr_value );
		}

		for ( tmp = e; tmp; tmp = tmp->e_parent ) {
			tmpdn = get_copy_dn( tmp );
			if ( subindex = get_subtree_index( tmpdn ) ) {
				(void) turbo_attr_delete( subindex, e,
				    as->attr_type, as->attr_value );
			}
			dn_free( tmpdn );
		}
	}

	/* now delete references in nonleafkids and nonlocalaliases... */
	if ( sibindex && e->e_alias && (! siblings( dn, sibindex->i_dn )) )
		delete_nonlocalalias( e, sibindex );

	dn_free( pdn );
	dn_free( dn );

	if ( nonleaf == 0 && e->e_alias == NULLDN )
		return;

	for ( tmp = e; tmp; tmp = tmp->e_parent ) {
		tmpdn = get_copy_dn( tmp );
		if ( subindex = get_subtree_index( tmpdn ) ) {
			if ( e->e_alias ) {
				pcmp = prefix( subindex->i_dn, e->e_alias );
				if ( pcmp != 0 && pcmp != -1 )
					delete_nonlocalalias( e, subindex );
			}

			if ( nonleaf )
				delete_nonleafkid( e, subindex );
		}
		dn_free( tmpdn );
	}

	return;
}

/*
 * turbo_isoptimized -- return TRUE if attr is to be optimized, FALSE
 * otherwise.
 */

turbo_isoptimized( attr )
AttributeType	attr;
{
	int	i;

	for ( i = 0; i < turbo_index_num; i++ ) {
		if ( AttrT_cmp( attr, turbo_index_types[ i ] ) == 0 )
			return( 1 );
	}

	return( 0 );
}

/*
 * turbo_optimize -- add attribute attr to the list of attributes to be 
 * optimized this routine creates an empty index and arranges for the 
 * attribute to be optimized during loading.
 */

turbo_optimize( attr )
char	*attr;
{
	AttributeType	a;

	if ( (a = str2AttrT( attr )) == NULLAttrT ) {
		LLOG(log_dsap, LLOG_EXCEPTIONS, ("Bad attribute type (%s)", attr));
		return;
	}

	if ( subtree_index || sibling_index )
		LLOG(log_dsap, LLOG_EXCEPTIONS, ("WARNING: optimized attributes MUST be specified before subtree or sibling index"));

	if ( turbo_index_types == (AttributeType *) 0 )
		turbo_index_types = (AttributeType *) malloc(
		    sizeof(AttributeType *));
	else
		turbo_index_types = (AttributeType *) realloc(
		    (char *) turbo_index_types, (unsigned) (turbo_index_num + 1) *
		    sizeof(AttributeType *));

	if ( turbo_index_types == (AttributeType *) 0 )
		fatal(66, "turbo_optimize: malloc failed!\n");

	turbo_index_types[ turbo_index_num ] = AttrT_cpy(a);
	turbo_index_num++;

	return;
}

/*
 * index_subtree - arrange for the subtree starting at tree to be indexed.
 */

index_subtree( tree )
char	*tree;
{
	DN		dn, str2dn();
	Index		*pindex;

	if ( (dn = str2dn( tree )) == NULLDN ) {
		LLOG( log_dsap, LLOG_EXCEPTIONS, ("Invalid subtree (%s)\n", tree) );
		return;
	}

	pindex = new_index( dn );
	dn_free( dn );
	if ( avl_insert( &subtree_index, (caddr_t) pindex, i_cmp, i_dup ) == NOTOK ) {
		LLOG(log_dsap, LLOG_EXCEPTIONS, ("Subtree index for %s already exists\n", tree));
		index_free( pindex );
	}

	return;
}

/*
 * index_siblings - arrange for the children of parent to be indexed.
 */

index_siblings( parent )
char	*parent;
{
	DN		dn, str2dn();
	Index		*pindex;

	if ( (dn = str2dn( parent )) == NULLDN ) {
		LLOG( log_dsap, LLOG_EXCEPTIONS, ("Invalid parent (%s)\n", parent) );
		return;
	}

	pindex = new_index( dn );
	dn_free( dn );
	if ( avl_insert( &sibling_index, (caddr_t) pindex, i_cmp, i_dup ) == NOTOK ) {
		LLOG(log_dsap, LLOG_EXCEPTIONS, ("Sibling index for %s already exists\n", parent));
		index_free( pindex );
	}

	return;
}

#endif /* turbo_index */
