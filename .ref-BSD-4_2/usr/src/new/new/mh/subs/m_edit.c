#include "mh.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <stdio.h>
#include "strings.h"

struct msgs *mp;

char *rindex();

m_edit(ed, file, use, altmsg)
char **ed, *file, *altmsg;
int use;
{
	/* Exec editor.  Normal exit returns 0.
	 * To abort, returns -1.  To try again, returns -2
	 */

	static char *edsave;
	static int  reedit;
	struct stat stbuf;
	int retstat;
	register char *cp;
	int pid, wpid;
	int intr;
	union { int statint;
		struct {char lobyte, hibyte;} statby;
	} status;

	if(!reedit) {                   /* set initial editor */
		if(!*ed && (*ed = m_find("editor")) == NULL)
			*ed = sysed;
	} else
		if(!*ed) {              /* no explicit editor */
			*ed = edsave;
			cp = rindex(*ed, '/');
			if(cp == 0)
				cp = *ed;
			cp = concat(cp, "-next", 0);
			if((cp = m_find(cp)) != NULL)
				*ed = cp;
		}
	intr = (int) signal(SIGINT, SIG_IGN);
	if((pid = fork()) == 0) {
		if(altmsg) {
			unlink("@");
			link(altmsg, "@");    /* An easy handle on cur msg */
		}
		m_update();
		fflush(stdout);
		signal(SIGINT, (int (*)()) intr);
		execlp(*ed, *ed, file, 0);
		fprintf(stderr, "Can't exec the editor:  ");
		perror(*ed);  done(-1);
	} else if(pid == -1) {
		fprintf(stderr, "No forks!\n");
		retstat = -1;
		goto leave;
	} else
		while((wpid = wait(&status)) != -1 && wpid != pid) ;
	signal(SIGINT, (int (*)()) intr);
	if(status.statint) {
		if((status.statby.hibyte == -1) ||      /* Can't exec editor */
		   (reedit && !status.statby.lobyte)) { /*2nd edit.Aborted by user*/
			retstat = -2;
			goto leave;
		}
		fprintf(stderr, "[%s aborted--%s ", invo_name(), file);
		if(!use && status.statby.hibyte) {      /* edit aborted by user */
			unlink(file);
			fprintf(stderr, "deleted]\n");
		} else                          /* 'use' or system abort */
			fprintf(stderr, "preserved]\n");
		retstat = -2;
		goto leave;
	}
	reedit++;
	retstat = 0;
	if(altmsg && !mp->msgflags&READONLY) {
		stat("@", &stbuf);
		if(stbuf.st_nlink == 1)  /*@'s been edited by Ned*/
			if(unlink(altmsg) == -1 || link("@", altmsg) == -1){
				fprintf(stderr, "Can't update %s from @ file!\n",altmsg);
				retstat = 0;
				goto leave;
			}
	}
 leave:
	edsave = getcpy(*ed);
	*ed = 0;
	unlink("@");          /* Remove this extra link */
	return(retstat);
}
