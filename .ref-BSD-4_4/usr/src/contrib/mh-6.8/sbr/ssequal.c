/* ssequal.c - initially equal? */


ssequal (substr, str)
register char  *substr,
               *str;
{
    if (!substr)
	substr = "";
    if (!str)
	str = "";

    while (*substr)
	if (*substr++ != *str++)
	    return 0;
    return 1;
}
