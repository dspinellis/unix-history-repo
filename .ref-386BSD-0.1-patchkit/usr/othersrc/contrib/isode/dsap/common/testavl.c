/* testavl.c - Test Tim Howes AVL code */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/testavl.c,v 7.1 91/02/22 09:20:32 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/testavl.c,v 7.1 91/02/22 09:20:32 mrose Interim $
 *
 *
 * $Log:	testavl.c,v $
 * Revision 7.1  91/02/22  09:20:32  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/12/20  11:44:57  mrose
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


#include <sys/types.h>
#include <stdio.h>

#include "manifest.h"
#include "quipu/util.h"
#include "quipu/attr.h"
#include "quipu/turbo.h"

main( argc, argv )
int	argc;
char	**argv;
{
	Avlnode	*tree = NULLAVL;
	char	command[ 10 ];
	char	name[ 80 ];
	char	*p;
	int	free();

	printf( "> " );
	while ( fgets( command, sizeof( command ), stdin ) != NULL ) {
		switch( *command ) {
		case 'n':	/* new tree */
			( void ) avl_free( tree, free );
			tree = NULLAVL;
			break;
		case 'p':	/* print */
			( void ) myprint( tree );
			break;
		case 't':	/* traverse with first, next */
			printf( "***\n" );
			for ( p = (char * ) avl_getfirst( tree );
			    p != NULL; p = (char *) avl_getnext( tree, p ) )
				printf( "%s\n", p );
			printf( "***\n" );
			break;
		case 'f':	/* find */
			printf( "data? " );
			if ( fgets( name, sizeof( name ), stdin ) == NULL )
				exit( 0 );
			name[ strlen( name ) - 1 ] = '\0';
			if ( (p = (char *) avl_find( tree, name, strcmp ))
			    == NULL )
				printf( "Not found.\n\n" );
			else
				printf( "%s\n\n", p );
			break;
		case 'i':	/* insert */
			printf( "data? " );
			if ( fgets( name, sizeof( name ), stdin ) == NULL )
				exit( 0 );
			name[ strlen( name ) - 1 ] = '\0';
			if ( avl_insert( &tree, strdup( name ), strcmp, 
			    avl_dup_error ) != OK )
				printf( "\nNot inserted!\n" );
			break;
		case 'd':	/* delete */
			printf( "data? " );
			if ( fgets( name, sizeof( name ), stdin ) == NULL )
				exit( 0 );
			name[ strlen( name ) - 1 ] = '\0';
			if ( avl_delete( &tree, name, strcmp ) == NULL )
				printf( "\nNot found!\n" );
			break;
		case 'q':	/* quit */
			exit( 0 );
			break;
		case '\n':
			break;
		default:
			printf("Commands: insert, delete, print, new, quit\n");
		}

		printf( "> " );
	}
	/* NOTREACHED */
}

static ravl_print( root, depth )
Avlnode	*root;
int	depth;
{
	int	i;

	if ( root == 0 )
		return;

	ravl_print( root->avl_right, depth+1 );

	for ( i = 0; i < depth; i++ )
		printf( "   " );
	printf( "%s %d\n", root->avl_data, root->avl_bf );

	ravl_print( root->avl_left, depth+1 );
}

myprint( root )
Avlnode	*root;
{
	printf( "********\n" );

	if ( root == 0 )
		printf( "\tNULL\n" );
	else
		( void ) ravl_print( root, 0 );

	printf( "********\n" );
}
