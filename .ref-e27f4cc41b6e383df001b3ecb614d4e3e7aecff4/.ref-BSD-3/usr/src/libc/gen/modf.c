double
modf(arg,ent)
double arg;
double *ent;
{
	int neg;
	int i;
	double two30 = 1073741824.;
	double big = 72057594037927936.;  /*2^56*/
	double x, temp;
	double ldexp();
	double frexp();
	long l;

	neg = 1;
	if(arg<0){
		neg = -1;
		arg = -arg;
		}
	if(arg>big){
		*ent = neg*arg;
		return(0);
		}
	if(arg<1){
		*ent = 0;
		return(neg*arg);
		}

	temp = 0;
	while(arg>two30){
		x = frexp(arg,&i);
		if(arg<0.5){
			arg = 2*arg;
			i = i-1;
			}
		x = ldexp(0.5,i);
		arg = arg - x;
		temp = temp + x;
		}

	l = arg;
	arg = arg-l;
	temp = temp+l;
	*ent = neg*temp;
	return(neg*arg);
	}
