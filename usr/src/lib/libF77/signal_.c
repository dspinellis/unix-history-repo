signal_(sigp, procp)
int *sigp, (**procp)();
{
return( signal(*sigp, *procp) );
}
