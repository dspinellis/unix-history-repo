/* This routine is replaced by some modules if they need to do
 * cleanup.  All exits in the code call done rather than exit.
 */

done(status)
{
	exit(status);
}
