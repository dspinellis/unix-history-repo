
static char *sccsid = "@(#)dvadvise.c	35.1 5/6/81";

/* dummy vadvise function - used when creating a lisp which will run
   on an operating system without vadvise.
*/
vadvise() 
{ return(0); }
