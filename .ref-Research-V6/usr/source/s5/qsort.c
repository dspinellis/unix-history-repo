
int	(*qscmp)();
int	qses;

qsort(a, n, es, fc)
char *a;
int n, es;
int (*fc)();
{
	qscmp = fc;
	qses = es;
	qs1(a, a+n*es);
}

qs1(a, l)
char *a, *l;
{
	register char *i, *j, *es;
	char *lp, *hp;
	int n, c;


	es = qses;

start:
	if((n=l-a) <= es)
		return;


	n = ((n/(2*es))*es) & 077777;
	hp = lp = a+n;
	i = a;
	j = l-es;


	for(;;) {
		if(i < lp) {
			if((c = (*qscmp)(i, lp)) == 0) {
				qsexc(i, lp =- es);
				continue;
			}
			if(c < 0) {
				i =+ es;
				continue;
			}
		}

loop:
		if(j > hp) {
			if((c = (*qscmp)(hp, j)) == 0) {
				qsexc(hp =+ es, j);
				goto loop;
			}
			if(c > 0) {
				if(i == lp) {
					qstexc(i, hp =+ es, j);
					i = lp =+ es;
					goto loop;
				}
				qsexc(i, j);
				j =- es;
				i =+ es;
				continue;
			}
			j =- es;
			goto loop;
		}


		if(i == lp) {
			if(lp-a >= l-hp) {
				qs1(hp+es, l);
				l = lp;
			} else {
				qs1(a, lp);
				a = hp+es;
			}
			goto start;
		}


		qstexc(j, lp =- es, i);
		j = hp =- es;
	}
}

qsexc(i, j)
char *i, *j;
{
	register char *ri, *rj, c;
	int n;

	n = qses;
	ri = i;
	rj = j;
	do {
		c = *ri;
		*ri++ = *rj;
		*rj++ = c;
	} while(--n);
}

qstexc(i, j, k)
char *i, *j, *k;
{
	register char *ri, *rj, *rk;
	char	c;
	int	n;

	n = qses;
	ri = i;
	rj = j;
	rk = k;
	do {
		c = *ri;
		*ri++ = *rk;
		*rk++ = *rj;
		*rj++ = c;
	} while(--n);
}
