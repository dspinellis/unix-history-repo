/*
 * 4.2BSD TCP/IP server for uucico - VMS version
 * uucico's TCP channel causes this server to be run at the remote end.
 *
 *	Lou Salkind
 *	New York University
 */

#include <stdio.h>
#include <signal.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/wait.h>
#include <pwd.h>

#include <eunice/eunice.h>
#include <vms/uafdef.h>
#include <vms/pcbdef.h>
#include <vms/phddef.h>
#include <vms/jibdef.h>
#include <vms/arbdef.h>
#include <vms/prvdef.h>


struct uaf uaf;			/* VMS UAF record for local user */
struct passwd *getpwnam();
char *getenv(), *sprintf();

main(argc, argv)
int argc;
char **argv;
{
	struct servent *sp;
	char user[64];
	char passwd[64];
	char *p;
	char ebuf[30];
	struct passwd *pwd;
	struct hostent *hp;
	struct sockaddr_in from;
	struct sockaddr_in *fromp;
	unsigned int *q;
	extern Set_VMS_UAF_Info();
	int s;

	{
		/*
		 *	Make stdin/stdout/stderr read/write
		 */
		if (FD_FAB_Pointer[0]->status == AUTO_OPENR)
			FD_FAB_Pointer[0]->status = AUTO_OPENRW;
		close(1); close(2);
		dup(0); dup(0);
	}

	(void) signal(SIGINT, SIG_DFL);
	(void) signal(SIGQUIT, SIG_DFL);
	(void) signal(SIGTERM, SIG_DFL);

	/*
	 * Get stuff out of the environment
	 */
	fromp = &from;
	q = (unsigned int *)fromp;
	if ((p = getenv("SYS$ERROR")) == NULL) {
		fprintf(stderr, "Can not translate SYS$ERROR\n");
		exit(1);
	}
	sscanf(p, "%x,%x,%x,%x", &q[0], &q[1], &q[2], &q[3]);

	fromp->sin_port = ntohs((u_short)fromp->sin_port);
	if (fromp->sin_family != AF_INET) {
		fprintf(stderr, "uucpd: malformed from address\n");
		exit(1);
	}
	hp = gethostbyaddr(&fromp->sin_addr, sizeof (struct in_addr),
		fromp->sin_family);
	if (hp == 0) {
		fprintf(stderr, "Host name for your address unknown\n");
		exit(1);
	}
	alarm(60);
	if (readline(user, sizeof user) < 0) {
		fprintf(stderr, "user read\n");
		exit(0);
	}
	if (readline(passwd, sizeof passwd) < 0) {
		fprintf(stderr, "passwd read\n");
		exit(0);
	}

	alarm(0);
	setpwent();
	if (!(validate_vms_user(user, passwd, &uaf) & 1) ||
	     (uaf.uaf$b_flags & UAF$M_DISACNT)) {
		fprintf(stderr, "Login incorrect.\n");
		exit(1);
	}
	pwd = getpwnam(user);
	if (pwd == NULL) {
		fprintf(stderr, "Login incorrect.\n");
		exit(1);
	}
	endpwent();
	if (chdir(pwd->pw_dir) < 0) {
		fprintf(stderr, "No remote directory.\n");
		exit(1);
	}
	sys$cmkrnl(Set_VMS_UAF_Info, 0); /* Set the VMS environment info */
	sprintf(ebuf, "-h%s", inet_ntoa(from.sin_addr));
	if (vfork() == 0) {
		execl("/usr/lib/uucp/uucico", "uucico", ebuf, (char *)0);
		perror("uucico server: execl");
		_exit(1);
	}
	wait(&s);
	exit(0);
}

/*
 *
 *	KERNEL Mode routine to stuff the PCB/JIB for this process with
 *	the correct quotas/information from the VMS UAF record.
 *
 */
Set_VMS_UAF_Info()
{
	extern struct PCB *ctl$gl_pcb;
	extern struct PHD *ctl$gl_phd;
	extern char ctl$t_username[], ctl$t_account[];
	extern unsigned int ctl$gq_procpriv[2];
	register struct PCB *pcb;
	register struct JIB *jib;
	register struct ARB *arb;
	register struct PHD *phd;
	register int i;

	/*
	 *	Get PCB and JIB pointers
	 */
	pcb = ctl$gl_pcb;
	phd = ctl$gl_phd;
	jib = pcb->pcb$l_jib;
	arb = pcb->pcb$l_arb;
	/*
	 *	Set our execution priority
	 */
	sys$setpri(0,0,uaf.uaf$b_pri,0);

	/* DEFAULT DIRECTORY/DISK ALREADY SET */

	/*
	 *	Set username (space padded)
	 */
	for(i = 0; i < sizeof(uaf.uaf$t_username); i++) {
			jib->jib$t_username[i] = uaf.uaf$t_username[i];
			ctl$t_username[i] = uaf.uaf$t_username[i];
	}
	for(i = sizeof(uaf.uaf$t_username); i < sizeof(jib->jib$t_username); i++) {
			jib->jib$t_username[i] = ' ';
	}
	/*
	 *	Set account (blank padded) [to ctl region as well???]
	 */
	for(i = 0; i < sizeof(uaf.uaf$t_account); i++) {
			jib->jib$t_account[i] = uaf.uaf$t_account[i];
			ctl$t_account[i] = uaf.uaf$t_account[i];
	}
	for(i = sizeof(uaf.uaf$t_account); i < sizeof(jib->jib$t_account); i++) {
			jib->jib$t_account[i] = ' ';
	}
	/*
	 *	Set UIC
	 */
	arb->arb$w_grp = pcb->pcb$w_grp = uaf.uaf$w_grp;
	arb->arb$w_mem = pcb->pcb$w_mem = uaf.uaf$w_mem;

	/* SET PROCESS NAME TO USERNAME?? */

	/*
	 *	Set quotas
	 */
	i = (uaf.uaf$w_biolm - pcb->pcb$w_biolm);
	pcb->pcb$w_biolm += i;
	pcb->pcb$w_biocnt += i;

	i = (uaf.uaf$w_diolm - pcb->pcb$w_diolm);
	pcb->pcb$w_diolm += i;
	pcb->pcb$w_diocnt += i;

	i = uaf.uaf$l_bytlm ? uaf.uaf$l_bytlm : uaf.uaf$w_bytlm;
	i = i - jib->jib$l_bytlm;
	jib->jib$l_bytlm += i;
	jib->jib$l_bytcnt += i;

	jib->jib$w_prclim = uaf.uaf$w_prccnt;

	i = (uaf.uaf$w_fillm - jib->jib$w_fillm);
	jib->jib$w_fillm += i;
	jib->jib$w_filcnt += i;

	i = *((int *)&uaf.uaf$w_pgflquota[0]);
	i = (i - jib->jib$l_pgflquota);
	jib->jib$l_pgflquota += i;
	jib->jib$l_pgflcnt += i;

	i = (uaf.uaf$w_tqcnt - jib->jib$w_tqlm);
	jib->jib$w_tqlm += i;
	jib->jib$w_tqcnt += i;

	i = (uaf.uaf$w_enqlm - jib->jib$w_enqlim);
	jib->jib$w_enqlim += i;
	jib->jib$w_enqcnt += i;

	i = (uaf.uaf$w_shrfillm - jib->jib$w_shrflim);
	jib->jib$w_shrflim += i;
	jib->jib$w_shrfcnt += i;

	i = (uaf.uaf$l_pbytlm - jib->jib$l_pbytlim);
	jib->jib$l_pbytlim += i;
	jib->jib$l_pbytcnt += i;

	/*
	 *	Fill in privileges
	 */
	i = *((int *)&uaf.uaf$l_priv[0]);
	i &= ~(1<<PRV$V_BYPASS);	/* TOO DANGEROUS TO GIVE TO USER */
	pcb->pcb$q_priv[0]     = i;
	arb->arb$q_priv[0]     = i;
	jib->jib$q_priv[0]     = i;
	phd->phd$q_authpriv[0] = i;
	ctl$gq_procpriv[0]     = i;
	i = *((int *)&uaf.uaf$l_priv[2]);
	pcb->pcb$q_priv[1]     = i;
	arb->arb$q_priv[1]     = i;
	jib->jib$q_priv[1]     = i;
	phd->phd$q_authpriv[1] = i;
	ctl$gq_procpriv[1]     = i;
}


/*
 *	Read a line from standard input (NETWORK)
 */
readline(p, n)
	register char *p;
	register int n;
{
	char c;

	while (n-- > 0) {
		if (read(0, &c, 1) <= 0)
			return(-1);
		c &= 0177;
		if (c == '\n' || c == '\r') {
			*p = '\0';
			return(0);
		}
		*p++ = c;
	}
	return(-1);
}

