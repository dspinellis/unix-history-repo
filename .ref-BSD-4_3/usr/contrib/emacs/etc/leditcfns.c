#include <sgtty.h>
#include <signal.h>
#define STRLEN 100
static char str[STRLEN+1] = "%?emacs"; /* extra char for the null */

switch_to_proc(){
    char *ptr = str;
    while (*ptr) ioctl(0, TIOCSTI, ptr++);
    ioctl(0, TIOCSTI, "\n");
    kill(getpid(), SIGTSTP);
    }

set_proc_str(ptr) char *ptr; {
    if (strlen(ptr) <= STRLEN)
	strcpy(str, ptr);
    else
	printf("string too long for set-proc-str: %s\n", ptr);
    }
