int *copyip(ipf, ipt)
register int *ipf, *ipt;
{
	while((*ipt = *ipf)  && *ipf++ != -1)
		ipt++;
	*ipt = 0;
	return(ipt);
}

