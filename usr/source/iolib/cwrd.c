cwrite (buff, buffp1, len, fn)
char *buff, *buffp1; int len, fn;
{
int unit, nwr;
unit = buffp1-buff;
len =* unit;
nwr = write (fn, buff, len);
return (nwr < 0 ? -1 : nwr/unit);
}
cread (buff, buffp1, len, fn)
char *buff, *buffp1; int len, fn;
{
int unit, nrd;
unit = buffp1 - buff;
len =* unit;
nrd = read(fn, buff, len);
return (nrd < 0 ? -1 : nrd/unit);
}
