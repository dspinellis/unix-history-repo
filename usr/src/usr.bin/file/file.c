#ifndef lint
static	char sccsid[] = "@(#)file.c	4.19 (Berkeley) %G%";
#endif
/*
 * file - determine type of file
 */

#include <sys/param.h>
#include <sys/stat.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <a.out.h>

extern int	errno;

int in;
int i  = 0;
char buf[BUFSIZ];
char *troff[] = {	/* new troff intermediate lang */
	"x","T","res","init","font","202","V0","p1",0};
char *fort[] = {
	"function","subroutine","common","dimension","block","integer",
	"real","data","double",0};
char *asc[] = {
	"chmk","mov","tst","clr","jmp",0};
char *c[] = {
	"int","char","float","double","struct","extern",0};
char *as[] = {
	"globl","byte","align","text","data","comm",0};
char *sh[] = {
	"fi", "elif", "esac", "done", "export",
	"readonly", "trap", "PATH", "HOME", 0 };
char *csh[] = {
	"alias", "breaksw", "endsw", "foreach", "limit",  "onintr",
	"repeat", "setenv", "source", "path", "home", 0 };
int	ifile;

main(argc, argv)
char **argv;
{
	FILE *fl;
	register char *p;
	char ap[MAXPATHLEN + 1];

	if (argc < 2) {
		fprintf(stderr, "usage: %s file ...\n", argv[0]);
		exit(3);
	}
		
	if (argc>1 && argv[1][0]=='-' && argv[1][1]=='f') {
		if ((fl = fopen(argv[2], "r")) == NULL) {
			perror(argv[2]);
			exit(2);
		}
		while ((p = fgets(ap, sizeof ap, fl)) != NULL) {
			int l = strlen(p);
			if (l>0)
				p[l-1] = '\0';
			type(p);
			if (ifile>=0)
				close(ifile);
		}
		exit(1);
	}
	while(argc > 1) {
		ifile = -1;
		type(argv[1]);
		fflush(stdout);
		argc--;
		argv++;
		if (ifile >= 0)
			close(ifile);
	}
	exit(0);
}

type(file)
char *file;
{
	int j,nl;
	char ch;
	struct stat mbuf;
	char slink[MAXPATHLEN + 1];

	if (lstat(file, &mbuf) < 0) {
		fprintf(stderr, "file: %s: %s\n", file, strerror(errno));
		return;
	}
	switch (mbuf.st_mode & S_IFMT) {
	case S_IFLNK:
		printf("%s:\tsymbolic link", file);
		j = readlink(file, slink, sizeof slink - 1);
		if (j >= 0) {
			slink[j] = '\0';
			printf(" to %s", slink);
		}
		printf("\n");
		return;

	case S_IFDIR:
		printf("%s:\t", file);
		if (mbuf.st_mode & S_ISVTX)
			printf("append-only ");
		printf("directory\n");
		return;

	case S_IFCHR:
	case S_IFBLK:
		printf("%s:\t%s special (%d/%d)\n", file,
		    (mbuf.st_mode&S_IFMT) == S_IFCHR ? "character" : "block",
		     major(mbuf.st_rdev), minor(mbuf.st_rdev));
		return;

	case S_IFSOCK:
		printf("%s:\tsocket\n", file);
		return;
	}

	ifile = open(file, 0);
	if(ifile < 0) {
		fprintf(stderr, "file: %s: %s\n", file, strerror(errno));
		return;
	}
	printf("%s:\t", file);
	in = read(ifile, buf, BUFSIZ);
	if(in == 0){
		printf("empty\n");
		return;
	}
	switch(*(int *)buf) {

	case 0413:
		printf("demand paged ");

	case 0410:
		printf("pure ");
		goto exec;

	case 0411:
		printf("jfr or pdp-11 unix 411 executable\n");
		return;

	case 0407:
exec:
		if (mbuf.st_mode & S_ISUID)
			printf("set-uid ");
		if (mbuf.st_mode & S_ISGID)
			printf("set-gid ");
		if (mbuf.st_mode & S_ISVTX)
			printf("sticky ");
		printf("executable");
		if(((int *)buf)[4] != 0) {
			printf(" not stripped");
			if(oldo(buf))
				printf(" (old format symbol table)");
		}
		printf("\n");
		return;

	case 0177555:
		printf("very old archive\n");
		return;

	case 0177545:
		printf("old archive\n");
		return;

	case 070707:
		printf("cpio data\n");
		return;
	}

	if (buf[0] == '#' && buf[1] == '!' && shellscript(buf+2, &mbuf))
		return;
	if (buf[0] == '\037' && buf[1] == '\235') {
		if (buf[2]&0x80)
			printf("block ");
		printf("compressed %d bit code data\n", buf[2]&0x1f);
		return;
	}
	if(strncmp(buf, "!<arch>\n__.SYMDEF", 17) == 0 ) {
		printf("archive random library\n");
		return;
	}
	if (strncmp(buf, "!<arch>\n", 8)==0) {
		printf("archive\n");
		return;
	}
	if (mbuf.st_size % 512 == 0) {	/* it may be a PRESS file */
		lseek(ifile, -512L, 2);	/* last block */
		if (read(ifile, buf, BUFSIZ) > 0 && *(short *)buf == 12138) {
			printf("PRESS file\n");
			return;
		}
	}
	i = 0;
	if(ccom() == 0)goto notc;
	while(buf[i] == '#'){
		j = i;
		while(buf[i++] != '\n'){
			if(i - j > 255){
				printf("data\n"); 
				return;
			}
			if(i >= in)goto notc;
		}
		if(ccom() == 0)goto notc;
	}
check:
	if(lookup(c) == 1){
		while((ch = buf[i++]) != ';' && ch != '{')if(i >= in)goto notc;
		printf("c program text");
		goto outa;
	}
	nl = 0;
	while(buf[i] != '('){
		if(buf[i] <= 0)
			goto notas;
		if(buf[i] == ';'){
			i++; 
			goto check; 
		}
		if(buf[i++] == '\n')
			if(nl++ > 6)goto notc;
		if(i >= in)goto notc;
	}
	while(buf[i] != ')'){
		if(buf[i++] == '\n')
			if(nl++ > 6)goto notc;
		if(i >= in)goto notc;
	}
	while(buf[i] != '{'){
		if(buf[i++] == '\n')
			if(nl++ > 6)goto notc;
		if(i >= in)goto notc;
	}
	printf("c program text");
	goto outa;
notc:
	i = 0;
	while(buf[i] == 'c' || buf[i] == '#'){
		while(buf[i++] != '\n')if(i >= in)goto notfort;
	}
	if(lookup(fort) == 1){
		printf("fortran program text");
		goto outa;
	}
notfort:
	i=0;
	if(ascom() == 0)goto notas;
	j = i-1;
	if(buf[i] == '.'){
		i++;
		if(lookup(as) == 1){
			printf("assembler program text"); 
			goto outa;
		}
		else if(buf[j] == '\n' && isalpha(buf[j+2])){
			printf("roff, nroff, or eqn input text");
			goto outa;
		}
	}
	while(lookup(asc) == 0){
		if(ascom() == 0)goto notas;
		while(buf[i] != '\n' && buf[i++] != ':')
			if(i >= in)goto notas;
		while(buf[i] == '\n' || buf[i] == ' ' || buf[i] == '\t')if(i++ >= in)goto notas;
		j = i-1;
		if(buf[i] == '.'){
			i++;
			if(lookup(as) == 1){
				printf("assembler program text"); 
				goto outa; 
			}
			else if(buf[j] == '\n' && isalpha(buf[j+2])){
				printf("roff, nroff, or eqn input text");
				goto outa;
			}
		}
	}
	printf("assembler program text");
	goto outa;
notas:
	for(i=0; i < in; i++)if(buf[i]&0200){
		if (buf[0]=='\100' && buf[1]=='\357')
			printf("troff (CAT) output\n");
		else
			printf("data\n"); 
		return;
	}
	if (mbuf.st_mode&((S_IEXEC)|(S_IEXEC>>3)|(S_IEXEC>>6))) {
		if (mbuf.st_mode & S_ISUID)
			printf("set-uid ");
		if (mbuf.st_mode & S_ISGID)
			printf("set-gid ");
		if (mbuf.st_mode & S_ISVTX)
			printf("sticky ");
		if (shell(buf, in, sh))
			printf("shell script");
		else if (shell(buf, in, csh))
			printf("c-shell script");
		else
			printf("commands text");
	} else if (troffint(buf, in))
		printf("troff intermediate output text");
	else if (shell(buf, in, sh))
		printf("shell commands");
	else if (shell(buf, in, csh))
		printf("c-shell commands");
	else if (english(buf, in))
		printf("English text");
	else
		printf("ascii text");
outa:
	while(i < in)
		if((buf[i++]&0377) > 127){
			printf(" with garbage\n");
			return;
		}
	/* if next few lines in then read whole file looking for nulls ...
		while((in = read(ifile,buf,BUFSIZ)) > 0)
			for(i = 0; i < in; i++)
				if((buf[i]&0377) > 127){
					printf(" with garbage\n");
					return;
				}
		/*.... */
	printf("\n");
}

oldo(cp)
char *cp;
{
	struct exec ex;
	struct stat stb;

	ex = *(struct exec *)cp;
	if (fstat(ifile, &stb) < 0)
		return(0);
	if (N_STROFF(ex)+sizeof(off_t) > stb.st_size)
		return (1);
	return (0);
}



troffint(bp, n)
char *bp;
int n;
{
	int k;

	i = 0;
	for (k = 0; k < 6; k++) {
		if (lookup(troff) == 0)
			return(0);
		if (lookup(troff) == 0)
			return(0);
		while (i < n && buf[i] != '\n')
			i++;
		if (i++ >= n)
			return(0);
	}
	return(1);
}
lookup(tab)
char *tab[];
{
	char r;
	int k,j,l;
	while(buf[i] == ' ' || buf[i] == '\t' || buf[i] == '\n')i++;
	for(j=0; tab[j] != 0; j++){
		l=0;
		for(k=i; ((r=tab[j][l++]) == buf[k] && r != '\0');k++);
		if(r == '\0')
			if(buf[k] == ' ' || buf[k] == '\n' || buf[k] == '\t'
			    || buf[k] == '{' || buf[k] == '/'){
				i=k;
				return(1);
			}
	}
	return(0);
}
ccom(){
	char cc;
	while((cc = buf[i]) == ' ' || cc == '\t' || cc == '\n')if(i++ >= in)return(0);
	if(buf[i] == '/' && buf[i+1] == '*'){
		i += 2;
		while(buf[i] != '*' || buf[i+1] != '/'){
			if(buf[i] == '\\')i += 2;
			else i++;
			if(i >= in)return(0);
		}
		if((i += 2) >= in)return(0);
	}
	if(buf[i] == '\n')if(ccom() == 0)return(0);
	return(1);
}
ascom(){
	while(buf[i] == '/'){
		i++;
		while(buf[i++] != '\n')if(i >= in)return(0);
		while(buf[i] == '\n')if(i++ >= in)return(0);
	}
	return(1);
}

english (bp, n)
char *bp;
{
# define NASC 128
	int ct[NASC], j, vow, freq, rare;
	int badpun = 0, punct = 0;
	if (n<50) return(0); /* no point in statistics on squibs */
	for(j=0; j<NASC; j++)
		ct[j]=0;
	for(j=0; j<n; j++)
	{
		if ((u_char)bp[j]<NASC)
			ct[bp[j]|040]++;
		switch (bp[j])
		{
		case '.': 
		case ',': 
		case ')': 
		case '%':
		case ';': 
		case ':': 
		case '?':
			punct++;
			if ( j < n-1 &&
			    bp[j+1] != ' ' &&
			    bp[j+1] != '\n')
				badpun++;
		}
	}
	if (badpun*5 > punct)
		return(0);
	vow = ct['a'] + ct['e'] + ct['i'] + ct['o'] + ct['u'];
	freq = ct['e'] + ct['t'] + ct['a'] + ct['i'] + ct['o'] + ct['n'];
	rare = ct['v'] + ct['j'] + ct['k'] + ct['q'] + ct['x'] + ct['z'];
	if (2*ct[';'] > ct['e']) return(0);
	if ( (ct['>']+ct['<']+ct['/'])>ct['e']) return(0); /* shell file test */
	return (vow*5 >= n-ct[' '] && freq >= 10*rare);
}

shellscript(buf, sb)
	char buf[];
	struct stat *sb;
{
	register char *tp;
	char *cp, *xp, *index();

	cp = index(buf, '\n');
	if (cp == 0 || cp - buf > in)
		return (0);
	for (tp = buf; tp != cp && isspace(*tp); tp++)
		if (!isascii(*tp))
			return (0);
	for (xp = tp; tp != cp && !isspace(*tp); tp++)
		if (!isascii(*tp))
			return (0);
	if (tp == xp)
		return (0);
	if (sb->st_mode & S_ISUID)
		printf("set-uid ");
	if (sb->st_mode & S_ISGID)
		printf("set-gid ");
	if (strncmp(xp, "/bin/sh", tp-xp) == 0)
		xp = "shell";
	else if (strncmp(xp, "/bin/csh", tp-xp) == 0)
		xp = "c-shell";
	else
		*tp = '\0';
	printf("executable %s script\n", xp);
	return (1);
}

shell(bp, n, tab)
	char *bp;
	int n;
	char *tab[];
{

	i = 0;
	do {
		if (buf[i] == '#' || buf[i] == ':')
			while (i < n && buf[i] != '\n')
				i++;
		if (++i >= n)
			break;
		if (lookup(tab) == 1)
			return (1);
	} while (i < n);
	return (0);
}
