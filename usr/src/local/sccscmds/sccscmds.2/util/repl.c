static char Sccsid[] "@(#)repl	3.1";
/*
	Replace each occurrence of `old' with `new' in `str'.
	Return `str'.
*/

repl(str,old,new)
char *str;
char old,new;
{
	return(trnslat(str, &old, &new, str));
}
