
/*	machine dependent file  */

label( n ){
	printf( "L%d:\n", n );
	}

tlabel(){
	lccopy( 2 );
	printf( ":\n" );
	}
