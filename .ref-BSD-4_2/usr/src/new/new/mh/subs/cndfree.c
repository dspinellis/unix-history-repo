/* Conditional free -- perform a free call if the address passed
 * is in free storage;  else NOP
 */


cndfree(addr)
char *addr;
{
	extern char end;

	if(addr >= &end) free(addr);
}
