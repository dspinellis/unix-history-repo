time_t getlogtime DCLPROTO((struct utmp *u,int inout));
void watchlog2 DCLPROTO((int inout,struct utmp *u,char *fmt));
void watchlog DCLPROTO((int inout,struct utmp *u,char **w,char *fmt));
int ucmp DCLPROTO((struct utmp *u,struct utmp *v));
void readwtab DCLPROTO((void));
void dowatch DCLPROTO((void));
int bin_log DCLPROTO((char *nam,char **argv,char *ops,int func));
