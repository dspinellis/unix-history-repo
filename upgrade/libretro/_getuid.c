/* getuid is supposed to return full word (integer) uid */
_getuid()
{

	return (getuid() & 0377);
}
