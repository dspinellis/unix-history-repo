/**
 **	input character from standard input.
 **	the global variable "cin" may be set to the descriptor
 **		of the file to be input from.  Its default is zero.
 **/

getchar()
{
	extern int	cin;

	return (cgetc(cin));
}
