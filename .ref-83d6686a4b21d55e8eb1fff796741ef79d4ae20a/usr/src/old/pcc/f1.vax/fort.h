/*	fort.h	4.1	85/03/19	*/

/*	machine dependent file  */

label( n ){
	printf( "L%d:\n", n );
	}

tlabel(){
	lccopy( 2 );
	printf( ":\n" );
	}
