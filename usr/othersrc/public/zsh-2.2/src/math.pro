int zzlex DCLPROTO((void));
int notzero DCLPROTO((int a));
void op DCLPROTO((int what));
void bop DCLPROTO((int tk));
long mathevall DCLPROTO((char *s,int prek,char **ep));
long matheval DCLPROTO((char *s));
long mathevalarg DCLPROTO((char *s,char **ss));
void mathparse DCLPROTO((int pc));
