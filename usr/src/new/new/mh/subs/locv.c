char *locv(longint)
long longint;
{
	static char locvbuf[12];

	sprintf(locvbuf, "%ld", longint);
	return locvbuf;
}
