/* gc.h   structures used by the gc analysis routines 
 *	  this is a temporary file
 */

struct gchead
	{  int version;	/* version number of this dump file */
	   int lowdata;	/* low address of sharable lisp data */
	   int dummy,dummy2,dummy3; 	/* to be used later	*/
	};

