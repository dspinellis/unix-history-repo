signal_(sigp, procp)
int *sigp, (**procp)();
{
int sig, proc;
sig = *sigp;
proc = *procp;

return( signal(sig, proc) );
}
