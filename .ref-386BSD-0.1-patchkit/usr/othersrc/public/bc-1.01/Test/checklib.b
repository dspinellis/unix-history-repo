define t (x,y,d,s,t) {
   auto u, v, w, i, b, c;

   if (s >= t) {
     "Bad Scales. Try again.
";   return;
   }

   for (i = x; i < y; i += d) {
     scale = s;
     u = f(i);
     scale = t;
     v = f(i);
     scale = s;
     w = v / 1;
     b += 1;
     if (u != w) {
       c += 1;
"
Failed:  
"
       "  index = "; i;
       "  val1 = "; u;
       "  val2 = "; v;
"
"
     }
   }

"
Total tests:    "; b;
"
Total failures: "; c;
"
Percent failed: "; scale = 2; c*100/b;

}


"
Checking e(x)"
define f(x) {
  return (e(x))
}
"
scale = 10"
j = t(-50,50,1,10,14)
"
scale = 20"
j = t(-50,50,1,20,24)

"
Checking l(x)"
define f(x) {
  return (l(x))
}
"
scale = 10"
j = t(1,10000,100,10,14)
"
scale = 20"
j = t(1,10000,100,20,24)

"
Checking s(x)"
define f(x) {
  return (s(x))
}
"
scale = 10"
j = t(0,8*a(1),.01,10,14)
"
scale = 20"
j = t(1,8*a(1),.01,20,24)

"
Checking a(x)"
define f(x) {
  return (a(x))
}
"
scale = 10"
j = t(-100,100,1,10,14)
"
scale = 20"
j = t(-100,100,1,20,24)

"
Checking j(n,x)"
define f(x) {
  return (j(n,x))
}
"
n=0, scale=10"
n=0
j = t(0,30,.1,10,14)
"
n=1, scale=10"
n=1
j = t(0,30,.1,10,14)
"
n=0, scale=20"
n=0
j = t(0,30,.1,20,24)
"
n=1, scale=20"
n=1
j = t(0,30,.1,20,24)

