#ifndef __HAVE_MATH_CONVEX__
#define __HAVE_MATH_CONVEX__

#define HUGE 	8.98846567431157854e+307	/* max double in native mode */

#define HUGE_VAL 8.98846567431157854e+307

#define M_E		2.71828182845904523536
#define M_LN10		2.30258509299404568402
#define M_LN2		0.69314718055994530942
#define M_LOG10E	0.43429448190325182765
#define M_LOG2E		1.44269504088896340736
#define M_PI		3.14159265358979323846
#define M_PI_2		1.57079632679489661923
#define M_PI_4		0.78539816339744830962
#define M_SQRT1_2	0.70710678118654752440
#define M_SQRT2		1.41421356237309504880
#define M_1_PI		0.31830988618379067154
#define M_2_PI		0.63661977236758134308
#define M_2_SQRTPI	1.12837916709551257390

extern __const__ double acos (double);
extern __const__ double asin (double);
extern __const__ double atan (double);
extern __const__ double atan2 (double, double);
extern double atof (__const__ char *);
extern __const__ double cabs ();
extern __const__ double ceil (double);
extern __const__ double cos (double);
extern __const__ double cosh (double);
extern __const__ double dcvtid (double);
extern __const__ double exp (double);
extern __const__ double fabs (double);
extern __const__ double floor (double);
extern double frexp (double, int *);
extern __const__ double gamma (double);
extern __const__ double hypot (double, double);
extern __const__ double idcvtd (double);
extern __const__ double ircvtr (double);
extern __const__ double j0 (double);
extern __const__ double j1 (double);
extern __const__ double jn (int, double);
extern __const__ double ldexp (double, int);
extern __const__ double log (double);
extern __const__ double log10 (double);
extern __const__ double fmod (double, double);
extern double modf (double, double *);
extern __const__ double pow (double, double);
extern __const__ double rcvtir (double);
extern __const__ double sacos (double);
extern __const__ double sasin (double);
extern __const__ double satan (double);
extern __const__ double satan2 (double);
extern __const__ double scabs ();
extern __const__ double scos (double);
extern __const__ double scosh (double);
extern __const__ double sexp (double);
extern __const__ double sfabs (double);
extern __const__ double shypot (double, double);
extern __const__ double sin (double);
extern __const__ double sinh (double);
extern __const__ double slog (double);
extern __const__ double slog10 (double);
extern __const__ double spow (double);
extern __const__ double sqrt (double);
extern __const__ double ssin (double);
extern __const__ double ssinh (double);
extern __const__ double ssqrt (double);
extern __const__ double stan (double);
extern __const__ double stanh (double);
extern __const__ double tan (double);
extern __const__ double tanh (double);
extern __const__ double y0 (double);
extern __const__ double y1 (double);
extern __const__ double yn (int, double);
extern __const__ long int ipow (int, int);
extern __const__ long long int lpow (long long int, long long int);

#define fabs(x)		 __builtin_fabs(x)

#ifdef __convex__

#define frexp(x,y)	__inline_frexp(x,y)
#define ldexp(x,y)	__inline_ldexp(x,y)

#ifdef __convex_c2__

#define ceil(x)		__inline_ceil (x)
#define cos(x)		__inline_cos (x)
#define exp(x)		__inline_exp (x)
#define floor(x)	__inline_floor (x)
#define log(x)		__inline_log (x)
#define log10(x)	__inline_log10 (x)
#define modf(x,y)	__inline_modf ((x), (y))
#define sin(x)		__inline_sin (x)
#define sqrt(x)		__inline_sqrt (x)

#endif __convex_c2__

__inline__ static __const__ double __inline_ceil (double x)
{
  double z;
  __asm__ ("frint.d %1,%0" : "=d" (z) : "d" (x));
  if (z < x) z += 1.0;
  return z;
}

__inline__ static __const__ double __inline_cos (double x)
{
  double z;
  __asm__ ("cos.d %0" : "=d" (z) : "0" (x));
  return z;
}

__inline__ static __const__ double __inline_exp (double x)
{
  double z;
  __asm__ ("exp.d %0" : "=d" (z) : "0" (x));
  return z;
}

__inline__ static __const__ double __inline_floor (double x)
{
  double z;
  __asm__ ("frint.d %1,%0" : "=d" (z) : "d" (x));
  if (z > x) z -= 1.0;
  return z;
}

__inline__ static __const__ double __inline_frexp (double x, int *np)
{
  union u {double d; unsigned long long ll;} u;
  if ((u.d = x) == 0)
    *np = 0;
  else
    {
      *np = ((u.ll >> 52) & 03777) - 02000;
      u.ll = (u.ll & 0x800fffffffffffffLL) | ((union u) {0.5}).ll;
    }
  return u.d;
}

__inline__ static __const__ double __inline_ldexp (double x, int n)
{
  extern int errno;
  union {double d; long long ll; unsigned sexp : 12;} u;
  if ((u.d = x) != 0)
    {
      int exp = n + (u.sexp & 03777);
      if (exp <= 0)
	u.ll = 0, errno = 34;
      else if (exp > 03777)
	u.ll |= 0x7fffffffffffffffLL, errno = 34;
      else
	u.ll += (long long) n << 52;
    }
  return u.d;
}

__inline__ static __const__ double __inline_log (double x)
{
  double z;
  __asm__ ("ln.d %0" : "=d" (z) : "0" (x));
  return z;
}

__inline__ static __const__ double __inline_log10 (double x)
{
  return M_LOG10E * __inline_log (x);
}

__inline__ static __const__ double __inline_modf (double x, double *np)
{
  double intpart;
  __asm__ ("frint.d %1,%0" : "=d" (intpart) : "d" (x));
  *np = intpart;
  return x - intpart;
}

__inline__ static __const__ double __inline_sin (double x)
{
  double z;
  __asm__ ("sin.d %0" : "=d" (z) : "0" (x));
  return z;
}

__inline__ static __const__ double __inline_sqrt (double x)
{
  double z;
  __asm__ ("sqrt.d %0" : "=d" (z) : "0" (x));
  return z;
}

#endif __convex__

#endif /* __HAVE_MATH_CONVEX__ */
