#include <stdio.h>
#include "../h/config.h"
#define BIN "/usr/new/lib/icon/icont"
#define ICONX "ICONX=/usr/new/iconx"
#define MAXARGS 20
#define PATHSIZE 100	/* max length of a fully qualified file name */
#ifndef UTRAN
#define UTRAN "%s/utran"
#endif
#ifndef ULINK
#define ULINK "%s/ulink"
#endif
char **rfiles;
main(argc,argv)
int argc; char **argv;
{
 	char **tfiles;
	char **lfiles;
	char **execlist;
	char *tflags[MAXARGS];
	char *lflags[MAXARGS];
	char *ourenv[10];
	char **xargs;
	int ntf, nlf, nrf, ntflags, nlflags, cflag, quiet, rc;
	char **arg;
	char *sflag;
	char *base, *getbase();
	char *u1, *u2, *xfile;
	char *rindex(), *mkname();
	char cmd[PATHSIZE];
	
	rfiles = (char **)calloc(2*(argc+10), sizeof(char **));
	tfiles = (char **)calloc(argc+10, sizeof(char **));
	lfiles = (char **)calloc(argc+10, sizeof(char **));
	execlist = (char **)calloc(2*(argc+10), sizeof(char **));
	ourenv[0] = ICONX;
	tflags[ntflags++] = "utran";
	lflags[nlflags++] = "ulink";
	rfiles[nrf++] = "rm";
	rfiles[nrf++] = "-f";
	xfile = "";
	
	for (arg = &argv[1]; arg <= &argv[argc-1]; arg++) {
		if ((*arg)[0] == '-') switch ((*arg)[1]) {
			case '\0': /* "-" */
				tfiles[ntf++] = *arg;
				lfiles[nlf++] = rfiles[nrf++]
				  = "stdin.u1";
				rfiles[nrf++] = "stdin.u2";
				break;
			case 's':
				tflags[ntflags++] = "-s";
				quiet++;
				break;
			case 'o':
				lfiles[nlf++] = "-o";
				xfile = lfiles[nlf++] = *++arg;
				break;
			case 'x':
				xargs = arg++;
				goto argsdone;
			case 'c':
				cflag++;
				break;
			default:
				lflags[nlflags++] = tflags[ntflags++] = *arg;
				break;
				}
		else if (suffix(*arg,".icn")) {
			tfiles[ntf++] = *arg;
			base = getbase(*arg,".icn");
			u1 = mkname(base,".u1");
			u2 = mkname(base,".u2");
			lfiles[nlf++] = rfiles[nrf++] = u1;
			rfiles[nrf++] = u2;
			}
		else if (suffix(*arg,".u1")) {
			lfiles[nlf++] = *arg;
			}
		else {
			fprintf(stderr,"%s: bad argument '%s'\n",argv[0],*arg);
			exit(1);
			}
		}
argsdone:
	if (nlf == 0)
		usage(argv[0]);
	if (!xfile[0])
		xfile = getbase(lfiles[0],".u1");
	
	if (ntf != 0) {
		lcat(execlist,tflags,tfiles);
		sprintf(cmd,UTRAN,BIN);
		runit(cmd,execlist,ourenv);
		}
	if (cflag) {
		exit(0);
		}
	if (!quiet)
		fprintf(stderr,"Linking:\n");
	execlist[0] = 0;
	lcat(execlist,lflags,lfiles);
	sprintf(cmd,ULINK,BIN);
	runit(cmd,execlist,ourenv);
	docmd("/bin/rm",rfiles,ourenv);
	chmod(xfile,0755);
	if (xargs) {
		if (!quiet)
			fprintf(stderr,"Executing:\n");
		xargs[0] = xfile;
#ifdef VAX
		execv(xfile,xargs);
#endif VAX
#ifdef PDP11
		execlist[0] = "iconx";
		execlist[1] = 0;
		lcat(execlist,xargs,0);
		execv(index(ICONX,'=')+1,execlist);
#endif PDP11
		}
}
runit(c,a,e)
char *c; char **a, **e;
{
	int rc;
	if ((rc = docmd(c,a,e)) != 0) {
		docmd("/bin/rm",rfiles,e);
		exit(1);
		}
}
suffix(name,suf)
char *name,*suf;
{
	return !strcmp(suf,rindex(name,'.'));
}
char *
mkname(name,suf)
char *name,*suf;
{
	char *p, *malloc();
	
	p = malloc(16);
	strcpy(p,name);
	strcat(p,suf);
	return p;
}
char *
getbase(name,suf)
char *name,*suf;
{
	char *f,*e, *rindex(), *p, *malloc();
	
	if (!(f = rindex(name,'/')))
		f = name;
	else
		f++;
	e = rindex(f,'.');
	p = malloc(16);
	strncpy(p,f,e-f);
	return p;
}
plist(title,list)
char *title, **list;
{
	char **p;
	printf("\n%s\n",title);
	for (p = list; *p; p++)
		printf("'%s'\n",*p);
}
lcat(c,a,b)
int c[],a[],b[];
{
	int cp,p;
	
	cp = p = 0;
	while (c[cp])
		cp++;
	while (c[cp] = a[p++])
		cp++;
	p = 0;
	if (b)
		while (c[cp++] = b[p++]);
}
usage(p)
char *p;
{
	fprintf(stderr,"usage: %s [-c] [-m] [-t] [-u] file ... [-x args]\n",p);
	exit(1);
}
docmd(cmd,argv,envp)
char *cmd, **argv, **envp;
{
	int rc, stat;
#ifdef VAX
	rc = vfork();
#else
	rc = fork();
#endif
	if (rc == -1) {
		fprintf(stderr,"No more processes\n");
		return 255;
		}
	if (rc == 0) {
		execve(cmd,argv,envp);
		fprintf(stderr,"exec failed on %s\n",cmd);
		_exit(255);
		}
	while (rc != wait(&stat));
	return (stat>>8) & 0xff;
}
