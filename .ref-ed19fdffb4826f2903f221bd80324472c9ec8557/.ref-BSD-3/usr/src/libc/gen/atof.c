union {
	short foo0[4];
	double big;
} bar0 /* = {0x5c80,0,0,0} */ ; /* 2**56 as double floating point */
union {
	short foo1[4];
	double huge;
} bar1 /* = {0x8000,0,0,0} */ ; /* reserved operand */

double atof(p) register char *p; {
register double exp,val;
register char c; register int dpdig;
int scale; char sign,esign;

abort(); /* THE REAL ROUTINE IS atofo.s !!!!! */
while ((c= *p++)==' ');	/* skip leading spaces */
sign=0;
if (c=='-') ++sign;	/* mark negative */
else if (c=='+') ;	/* ignore plus */
else --p;		/* get back on track */

val=0; dpdig=0;
/* true value is aproximately	((-1)**sign) * val * (10 ** dpdig) */
while ((c= *p++)<='9' && c>='0')
	if(val<bar0.big) {val *= 10; val += c-'0';}
	else ++dpdig;
if (c=='.')
	while ((c= *p++)<='9' && c>='0')
		if(val<bar0.big) {--dpdig; val *= 10; val += c-'0';}
if (sign) val = -val; /* sign has been taken care of, if  val  in range */
scale=0;
if (c=='E' || c=='e') {/* scale factor */
	esign=0;
	if ((c= *p++)=='-') ++esign;
	else if (c=='+');
	else --p;
	while ((c= *p++)<='9' && c>='0') {scale *= 10; scale += c-'0';}
	if (esign) scale = -scale;
}
dpdig += scale;
if (dpdig==0) return(val);
esign=0; if (dpdig<0) {++esign; dpdig = -dpdig;}
if (dpdig>38) if (esign) return(0); else return(sign? -bar1.huge : bar1.huge);
exp=1; while (dpdig) {
	if (dpdig==21) {exp *= 1.0e+21; break;}
	exp *= 10; --dpdig;
}
if (esign) return(val/exp); return(val*exp);
}
