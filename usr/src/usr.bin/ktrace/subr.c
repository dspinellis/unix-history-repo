#include "ktrace.h"

getfacs(s)
	char *s;
{
	int facs = 0;

	while (*s) {
		switch(*s) {
		case 'c':
			facs |= KTRFAC_SYSCALL | KTRFAC_SYSRET;
			break;
		case 'n':
			facs |= KTRFAC_NAMEI;
			break;
		case 'g':
		case 'd':
			facs |= KTRFAC_GENIO;
			break;
#ifdef notyet
		case 's':
			facs |= KTRFAC_SIGNAL;
			break;
#endif
		case '+':
			facs |= DEF_FACS;
			break;
		case 'a':
			facs = KTRFAC_SYSCALL | KTRFAC_SYSRET | KTRFAC_GENIO;
			break;
		default:
			return (-1);
		}
		s++;
	}
	return (facs);
}
