/*	fort.h	1.1	86/01/11	*/


/*	machine dependent file  */

label( n ){
	printf( "L%d:\n", n );
	}

tlabel(){
	lccopy( 2 );
	printf( ":\n" );
	}
