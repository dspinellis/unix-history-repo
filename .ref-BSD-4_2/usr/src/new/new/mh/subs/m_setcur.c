#include "mh.h"
#include <stdio.h>

struct  msgs *mp;

m_setcur(num)
{
	char buf[6];
	register int i;
	register char *cp1;

	if(mp->msgflags&READONLY) {
		m_replace(cp1 = concat("cur-",mp->foldpath,0), m_name(num));
		free(cp1);
	} else {
		strcpy(buf, m_name(num));
		cp1 = buf + strlen(buf);
		*cp1++ = '\n';
		if(strcmp(current, "cur"))
			error("\"current\" got Clobbered!! Tell B. Borden");
		if((i = creat(current, 0660)) >= 0) {
			write(i, buf, cp1-buf);
			close(i);
		}
	}
}
