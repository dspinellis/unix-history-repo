/* turbo_search.c - DSA search of the directory using attribute index */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/turbo_search.c,v 7.1 91/02/22 09:40:09 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/RCS/turbo_search.c,v 7.1 91/02/22 09:40:09 mrose Interim $
 *
 *
 * $Log:	turbo_search.c,v $
 * Revision 7.1  91/02/22  09:40:09  mrose
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



#include "quipu/util.h"
#include "quipu/entry.h"
#include "quipu/list.h"
#include "quipu/ds_search.h"
#include "quipu/turbo.h"
#include "config.h"

#ifdef TURBO_INDEX

extern LLog    		*log_dsap;
extern int		size;
extern char		qctx;
extern AttributeType	*turbo_index_types;
extern int		turbo_index_num;
extern Avlnode		*subtree_index;
extern Avlnode		*sibling_index;
int			idn_cmp();

static struct search_kid_arg	*g_ska;
static int			g_toplevel;
static int			g_count;

static EntryInfo	*turbo_filterkids();
static EntryInfo	*turbo_item();
static EntryInfo	*turbo_and();
static EntryInfo	*turbo_or();
static EntryInfo	*eis_union();
static EntryInfo	*eis_intersection();
static void		subtree_refer();

Attr_Sequence	eis_select();
EntryInfo	*filterentry();

/*
 * turbo_sibling_search - search a sibling index
 */

turbo_sibling_search( e, ska )
Entry			e;
struct search_kid_arg	*ska;
{
	EntryInfo		*list;
	Entry			*tmp;
	DN			dn;
	Index			*pindex;

	*ska->ska_einfo = NULLENTRYINFO;

	if ( e->e_leaf )
		return;

	if ( e->e_children == NULLAVL ) {
		search_refer( ska->ska_arg, e, ska->ska_local, ska->ska_refer,
		    ska->ska_ismanager );
		return;
	}

	dn = get_copy_dn( e );
	if ( (pindex = get_sibling_index( dn )) == NULLINDEX ) {
		LLOG( log_dsap, LLOG_EXCEPTIONS, ("Cannot find sibling index") );
		dn_free( dn );
		return;
	}
	dn_free( dn );

	list = turbo_filterkids( e, ska->ska_arg->sra_filter, ska, pindex, 1 );

	if ( *ska->ska_einfo == NULLENTRYINFO )
		*ska->ska_einfo = list;
	else if ( list != NULLENTRYINFO )
		entryinfo_append( *ska->ska_einfo, list );

	if ( ska->ska_arg->sra_searchaliases && pindex->i_nonlocalaliases
	    != (Entry *) 0 ) {
		for ( tmp = pindex->i_nonlocalaliases; *tmp; tmp++ )
			(void) do_alias( ska->ska_arg, *tmp, ska->ska_local );
	}
}

/*
 * turbo_subtree_search - search a subtree index
 */

turbo_subtree_search( e, ska )
Entry			e;
struct search_kid_arg	*ska;
{
	EntryInfo	*list;
	Entry		*tmp;
	DN		dn;
	Index		*pindex;

	dn = get_copy_dn( e );
	if ( (pindex = get_subtree_index( dn )) == NULLINDEX ) {
		LLOG( log_dsap, LLOG_EXCEPTIONS, ("Cannot find subtree index") );
		dn_free( dn );
		return;
	}
	dn_free( dn );

	list = turbo_filterkids( e, ska->ska_arg->sra_filter, ska, pindex, 1 );

	if ( *ska->ska_einfo == NULLENTRYINFO )
		*ska->ska_einfo = list;
	else if ( list != NULLENTRYINFO )
		entryinfo_append( *ska->ska_einfo, list );

	/*
	 * at this point, anything held locally below this point has been
	 * searched.  we now search through the nonleaf children recursively
	 * for one whose children are not held locally, referring any that
	 * we find.  next, we search through the list of nonlocal aliases
	 * searching for one that matches (if dereferencing is allowed).
	 */

	if ( pindex->i_nonleafkids != (Entry *) 0 )
		subtree_refer( pindex, ska );

	if ( ska->ska_arg->sra_searchaliases && pindex->i_nonlocalaliases
	    != (Entry *) 0 ) {
		for ( tmp = pindex->i_nonlocalaliases; *tmp; tmp++ )
			(void) do_alias( ska->ska_arg, *tmp, ska->ska_local );
	}
}

static void subtree_refer( pindex, ska )
Index			*pindex;
struct search_kid_arg	*ska;
{
	Entry	*tmp;

	for ( tmp = pindex->i_nonleafkids; *tmp != NULLENTRY; tmp++ ) {
		if ( ((*tmp)->e_children != NULLAVL
		    && (*tmp)->e_allchildrenpresent == FALSE)
		    || (*tmp)->e_children == NULLAVL ) {
			if ( check_acl( (*ska->ska_local)->st_bind, ACL_READ,
			    (*tmp)->e_acl->ac_child, NULLDN ) == OK ) {
                                search_refer( ska->ska_arg, *tmp, 
				    ska->ska_local, ska->ska_refer, 
				    ska->ska_ismanager);
			}
		}
	}
}

static EntryInfo *turbo_filterkids( e, f, ska, pindex, toplevel )
Entry			e;
Filter			f;
struct search_kid_arg	*ska;
Index			*pindex;
int			toplevel;
{
	if ( e == NULLENTRY || f == NULLFILTER ) {
		LLOG( log_dsap, LLOG_EXCEPTIONS, ("bad turbo_filterkids pars") );
		return( NULLENTRYINFO );
	}

	switch ( f->flt_type ) {
	case FILTER_ITEM:
		return( turbo_item( e, &f->FUITEM, ska, pindex, toplevel ) );
	case FILTER_AND:
		return( turbo_and( e, f->FUFILT, ska, pindex, toplevel ) );
	case FILTER_OR:
		return( turbo_or( e, f->FUFILT, ska, pindex, toplevel ) );
	default:
		LLOG( log_dsap, LLOG_EXCEPTIONS,
		    ("turbo_filterkids: cannot filter type (%d)",
		    f->flt_type) );
		return( NULLENTRYINFO );
	}
	/* NOT REACHED */
}

static eis_merge( ei, eilist )
EntryInfo	*ei;
EntryInfo	**eilist;
{
	int		cmp;
	EntryInfo	*eitmp;

	/* 
	 * add this entry to the result list. the list is
	 * ordered here to make it easier to do and's and 
	 * or's later on.
	 */

	if ( *eilist == NULLENTRYINFO ) {
		*eilist = ei;
		return( OK );
	} else if ((cmp = dn_order_cmp(ei->ent_dn, (*eilist)->ent_dn)) == 0) {
		entryinfo_free(ei, 0);
		return( NOTOK );
	} else if ( cmp < 0 ) {
		ei->ent_next = *eilist;
		*eilist = ei;
		return( OK );
	}

	eitmp = *eilist;
	cmp = -1;
	while ( eitmp->ent_next != NULLENTRYINFO ) {
		if ((cmp = dn_order_cmp(ei->ent_dn, eitmp->ent_next->ent_dn))
		    <= 0)
			break;
		eitmp = eitmp->ent_next;
	}

	if ( cmp == 0 ) {
		entryinfo_free(ei, 0);
		return( NOTOK );
	}

	ei->ent_next = eitmp->ent_next;
	eitmp->ent_next = ei;

	return( OK );
}

static entry_collect( node, eilist )
Index_node	*node;
EntryInfo	**eilist;
{
	int		i;
	EntryInfo	*ei;

	for ( i = 0; i < node->in_num; i++ ) {
		if (g_ska->ska_arg->sra_searchaliases &&
		    node->in_entries[i]->e_alias != NULLDN)
			continue;

		if ((ei = filterentry(g_ska->ska_arg, node->in_entries[i],
		    (*g_ska->ska_local)->st_bind)) == NULLENTRYINFO)
			continue;

		/*
		 * size will have been decremented by filterentry, so
		 * we need to undo this here if necessary.
		 */

		if ( eis_merge( ei, eilist ) != OK || g_toplevel == 0 )
			size++;

		if ( size <= 0 ) {
			size--;
			return( NOTOK );
		}
	}
	return( OK );
}

static build_indexnode( node, bignode )
Index_node	*node;
Index_node	**bignode;
{
	int	i, j, k;
	int	found, last;
	Entry	tmp1, tmp2;

	if (*bignode == NULLINDEXNODE) {
		*bignode = (Index_node *) malloc(sizeof(Index_node));
		(*bignode)->in_entries = (Entry *) 0;
		(*bignode)->in_num = 0;
	}

	if ((*bignode)->in_entries == (Entry *) 0) {
		(*bignode)->in_entries = (Entry *) malloc((unsigned)
		    (node->in_num * sizeof(Entry)));
	} else {
		(*bignode)->in_entries = (Entry *) realloc(
		    (char *)(*bignode)->in_entries, (unsigned)
		    ((*bignode)->in_num * sizeof(Entry)
		    + node->in_num * sizeof(Entry)));
	}

	last = 0;
	for (i = 0; i < node->in_num; i++) {
		found = 1;
		for (j = last; j < (*bignode)->in_num; j++) {
			/* duplicate? */
			if (node->in_entries[i] == (*bignode)->in_entries[j]) {
				found = 0;
				break;
			}
			if ((*bignode)->in_entries[j] > node->in_entries[i])
				break;
		}

		if (j == (*bignode)->in_num) {
			(*bignode)->in_entries[j] = node->in_entries[i];
			(*bignode)->in_num++;
		} else if (found) {
			(*bignode)->in_num++;
			tmp1 = node->in_entries[i];
			for (k = j; k < (*bignode)->in_num; k++) {
				tmp2 = (*bignode)->in_entries[k];
				(*bignode)->in_entries[k] = tmp1;
				tmp1 = tmp2;
			}
		}
		last = j;
	}

	return(OK);
}

/* ARGSUSED */
static EntryInfo *turbo_item( e, f, ska, pindex, toplevel )
Entry			e;
struct filter_item	*f;
struct search_kid_arg	*ska;
Index			*pindex;
int			toplevel;
{
	int		i, k;
	int		done;
	int		len;
	AV_Sequence	av;
	Attr_Sequence	as;
	Index_node	*node;
	EntryInfo	*eilist;
	char		*word, *code, *small;
	char		*next_word();
	int		index_soundex_cmp(), index_soundex_prefix();
	int		substring_prefix_cmp(), substring_prefix_case_cmp();
	int		indexav_cmp(), build_indexnode(), free();

	if ( pindex == NULLINDEX ) {
		return( NULLENTRYINFO );
	}

	for ( i = 0; i < turbo_index_num; i++ ) {
		if ( AttrT_cmp( pindex[ i ].i_attr, f->UNAVA.ava_type )
		    == 0 )
			break;
	}

	if ( i == turbo_index_num ) {
		LLOG( log_dsap, LLOG_EXCEPTIONS, ("can't find index") );
		return( NULLENTRYINFO );
	}

	eilist = NULLENTRYINFO;
	g_ska = ska;
	switch ( f->fi_type ) {
	case FILTERITEM_EQUALITY:
		node = (Index_node *) avl_find( pindex[ i ].i_root,
		    (caddr_t) f->UNAVA.ava_value, indexav_cmp );

		if ( node == ((Index_node *) 0) )
			break;

		g_toplevel = toplevel;
		(void) entry_collect( node, &eilist );
		break;

	case FILTERITEM_APPROX:
		g_toplevel = 0;
		g_count = 0;
		small = NULL;
		for ( word = (char *) f->UNAVA.ava_value->av_struct; word;
		    word = next_word( word ) ) {
			g_count++;
			code = NULL;
			soundex( word, &code );

			if (small == NULL || strlen(code) > strlen(small))
				small = code;
			else
				free(code);
		}

		if (small == NULL)
			break;
#ifdef SOUNDEX_PREFIX
		/*
		 * now traverse the index (smartly) and build a giant
		 * Index_node to be used below
		 */

		node = NULLINDEXNODE;
		(void) avl_prefixapply(pindex[i].i_sroot,
		    (caddr_t) small, build_indexnode, (caddr_t) &node,
		    index_soundex_prefix, strlen(small));
#else
		node = (Index_node *) avl_find( pindex[ i ].i_sroot,
		    (caddr_t) small, index_soundex_cmp );
#endif

		/* we found nothing */
		if (node == NULLINDEXNODE)
			break;

		/*
		 * now we build the result list by applying the filter
		 * to the node we found above.
		 */

		g_toplevel = toplevel;
		(void) entry_collect(node, &eilist);

#ifdef SOUNDEX_PREFIX
		free((char *) node->in_entries);
		free((char *) node);
#endif
		break;

	case FILTERITEM_SUBSTRINGS:
		if (f->UNSUB.fi_sub_initial == NULLAV) {
			LLOG(log_dsap, LLOG_EXCEPTIONS, ("turbo_item: non-optimized substring filter"));
			break;
		}

		node = NULLINDEXNODE;
		len = strlen(f->UNSUB.fi_sub_initial->avseq_av.av_struct);
		if (case_exact_match(f->UNSUB.fi_sub_type->oa_syntax)) {
			(void) avl_prefixapply(pindex[i].i_root,
			    (caddr_t) &f->UNSUB.fi_sub_initial->avseq_av,
			    build_indexnode, (caddr_t) &node,
			    substring_prefix_cmp, len);
		} else {
			(void) avl_prefixapply(pindex[i].i_root,
			    (caddr_t) &f->UNSUB.fi_sub_initial->avseq_av,
			    build_indexnode, (caddr_t) &node,
			    substring_prefix_case_cmp, len);
		}

		if (node == NULLINDEXNODE)
			break;

		g_toplevel = toplevel;
		(void) entry_collect(node, &eilist);

		free((char *) node->in_entries);
		free((char *) node);
		break;

	case FILTERITEM_PRESENT:
		g_toplevel = toplevel;
		(void) avl_apply( pindex[ i ].i_root, entry_collect,
		    (caddr_t) &eilist, NOTOK, AVL_INORDER );
		break;

	default:	/* handle other cases some day */
		LLOG( log_dsap, LLOG_EXCEPTIONS,
		    ("turbo_item: filter type %d not supported\n",
		    f->fi_type) );
		break;
	}

	return( eilist );
}

static EntryInfo *turbo_and( e, f, ska, pindex, toplevel )
Entry			e;
Filter			f;
struct search_kid_arg	*ska;
Index			*pindex;
int			toplevel;
{
	EntryInfo	*tmp, *result = NULLENTRYINFO;
	Filter		nextf;

	nextf = f;
	if ( nextf->flt_next == NULLFILTER && toplevel ) {
		result = turbo_filterkids( e, nextf, ska, pindex, toplevel );
	} else {
		g_toplevel = 0;
		result = turbo_filterkids( e, nextf, ska, pindex, 0 );
	}

	nextf = nextf->flt_next;
	while ( nextf != NULLFILTER ) {
		tmp = turbo_filterkids( e, nextf, ska, pindex, 0 );
		if ( nextf->flt_next == NULLFILTER )
			g_toplevel = toplevel;
		else
			g_toplevel = 0;
		result = eis_intersection( result, tmp );
		nextf = nextf->flt_next;
	}

	return( result );
}

static EntryInfo *turbo_or( e, f, ska, pindex, toplevel )
Entry			e;
Filter			f;
struct search_kid_arg	*ska;
Index			*pindex;
int			toplevel;
{
	EntryInfo	*result = NULLENTRYINFO;
	EntryInfo	*tmp;
	Filter		nextf;

	nextf = f;
	while ( nextf != NULLFILTER ) {
		tmp = turbo_filterkids( e, nextf, ska, pindex, toplevel );
		g_toplevel = toplevel;
		result = eis_union( result, tmp );
		if ( size < 0 )
			break;
		nextf = nextf->flt_next;
	}

	return( result );
}

static EntryInfo *eis_union( a, b )
EntryInfo	*a;
EntryInfo	*b;
{
	int		cmp, done;
	EntryInfo	*result, *rtail;
	EntryInfo	*l, *g;
	EntryInfo	*save;
	int		dn_order_cmp();

	if ( a == NULLENTRYINFO )
		return( b );

	if ( b == NULLENTRYINFO )
		return( a );

	if ( dn_order_cmp( a->ent_dn, b->ent_dn ) < 0 ) {
		l = a;
		g = b;
	} else {
		l = b;
		g = a;
	}

	/* merge the two ordered lists, removing duplicates */
	done = 0;
	result = NULLENTRYINFO;
	while ( l && g && !done ) {
		if ( dn_order_cmp( g->ent_dn, l->ent_dn ) < 0 ) {
			save = l;
			l = g;
			g = save;
		}

		cmp = -1;
		while ( l && g && (cmp = dn_order_cmp( l->ent_dn, g->ent_dn ))
		    <= 0 ) {
			if ( g_toplevel && --size < 0 ) {
				done = 1;
				break;
			}

			if ( result == NULLENTRYINFO ) {
				result = l;
				l = l->ent_next;
				result->ent_next = NULLENTRYINFO;
				rtail = result;
			} else {
				rtail->ent_next = l;
				rtail = l;
				l = l->ent_next;
				rtail->ent_next = NULLENTRYINFO;
			}

			/*
			 * duplicate - we've added the one from l already so
			 * we just delete the one from g here.
			 */

			if ( cmp == 0 ) {
				save = g->ent_next;

				dn_free( g->ent_dn );
				as_free( g->ent_attr );
				free( (char *) g );

				g = save;
			}
		}
	}

	/* see if we stopped because of a size limit */
	if ( g_toplevel && size < 0 ) {
		if ( l ) {
			entryinfo_free( l,0 );
		}
		if ( g ) {
			entryinfo_free( g,0 );
		}
	/* otherwise we stopped because the list(s) ran out */
	} else if ( l != NULLENTRYINFO || (l = g) != NULLENTRYINFO ) {
		while ( l ) {
			if ( g_toplevel && --size < 0 )
				break;
			rtail->ent_next = l;
			rtail = l;
			l = l->ent_next;
		}
	}

	return( result );
}

static EntryInfo *eis_intersection( a, b )
EntryInfo	*a;
EntryInfo	*b;
{
	int		cmp;
	EntryInfo	*next, *save;
	EntryInfo	*l, *g;
	EntryInfo	*result, *tailresult = NULLENTRYINFO;
	DN		ldn, gdn, savedn;
	int		dn_order_cmp();

	if ( a == NULLENTRYINFO || b == NULLENTRYINFO ) {
		return( NULLENTRYINFO );
	}

	l = a;
	g = b;
	result = NULLENTRYINFO;
	while ( l != NULLENTRYINFO && g != NULLENTRYINFO ) {
		ldn = l->ent_dn;
		gdn = g->ent_dn;

		if ( dn_order_cmp( gdn, ldn ) < 0 ) {
			save = g;
			g = l;
			l = save;
			savedn = gdn;
			gdn = ldn;
			ldn = savedn;
		}

		cmp = -1;
		while ( l != NULLENTRYINFO ) {
			ldn = l->ent_dn;

			if ( (cmp = dn_order_cmp( ldn, gdn )) >= 0 )
				break;

			save = l->ent_next;
			dn_free( l->ent_dn );
			as_free( l->ent_attr );
			free( (char *) l );

			l = save;
		}

		if ( cmp == 0 ) {
			if ( g_toplevel && (--size < 0) )
				break;

			save = l->ent_next;
			dn_free( l->ent_dn );
			as_free( l->ent_attr );
			free( (char *) l );

			if ( result == NULLENTRYINFO )
				result = g;
			else
				tailresult->ent_next = g;
			tailresult = g;

			l = save;
			g = g->ent_next;
			tailresult->ent_next = NULLENTRYINFO;
		}
	}

	while ( l != NULLENTRYINFO ) {
		next = l->ent_next;
		dn_free( l->ent_dn );
		as_free( l->ent_attr );
		free( (char *) l );
		l = next;
	}
	while ( g != NULLENTRYINFO ) {
		next = g->ent_next;
		dn_free( g->ent_dn );
		as_free( g->ent_attr );
		free( (char *) g );
		g = next;
	}

	return( result );
}

#endif /* TURBO_INDEX */
