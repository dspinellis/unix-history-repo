/*
 * r1bindex(str, chr) stands for Right plus 1 or Beginning index of
 *      chr in str.  I.e. return ptr 1 past LAST occurance of chr in
 *      str, OR beginning of the string if str doesn't contain chr.
 */

char *
r1bindex(str, chr)
register char *str;
register int chr;
{
	register char *cp;

	for(cp = str; *cp; cp++) ;
	--cp;
	while(cp >= str && *cp != chr)
		--cp;
	return ++cp;
}
