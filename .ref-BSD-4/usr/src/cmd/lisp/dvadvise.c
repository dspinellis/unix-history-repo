
static char *sccsid = "@(#)dvadvise.c	34.1 10/3/80";

/* dummy vadvise function - used when creating a lisp which will run
   on an operating system without vadvise.
*/
vadvise() 
{ return(0); }
