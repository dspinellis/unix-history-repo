program math(input,output);

label	1;

type
chrint=-120..120;
sint=-1000..1000;
ray=array[1..2] of record
  first :real;
  second :alfa;
  end;

var
i :chrint;
j :sint;
k :integer;
l, m :real;
ary :ray;

procedure skip;

begin
goto 1;
end;


begin
j:=5;
k:=10;
l:=15.0;
i:=j;
i:=k;
j:=k;
k:=j;
l:=j;
l:=k;
i:=10;
j:=1000;
k:=100000;
l:=1.24e+16;
ary[1].first:=50.2;
ary[1].second:='oh boy';
m:=j+j;
m:=k+k;
m:=l+l;
m:=j+k;
m:=k+j;
m:=j+l;
m:=l+j;
m:=l+k;
m:=k+l;
m:=j-j;
m:=k-k;
m:=l-l;
m:=j-k;
m:=k-j;
m:=j-l;
m:=l-j;
m:=l-k;
m:=k-l;
m:=j*j;
m:=k/k*k;
m:=l*l;
m:=j*k;
m:=k*j;
m:=j*l;
m:=l*j;
m:=l*k;
m:=k*l;
m:=j/j;
m:=ary[1].first/l;
m:=j/k;
m:=k/j;
m:=j/l;
m:=l/j;
m:=l/k;
m:=k/l;
m:=j div j;
m:=k div k;
m:=j div k;
m:=k div j;
m:=j mod j;
m:=k mod k;
m:=j mod k;
m:=k mod j;
m:=-j;
m:=-k;
m:=-l;
m:=abs(-j)+abs(k)+abs(-l);
m:=abs(j)+abs(-k)+abs(l);
if (k=k) and
   (j<k) and
   (k>=j) and
   (l=l) and
   (j<=l) and
   (k<>l) and
   (l>j) and
   (l>k) and
   (ary[1].second=ary[1].second) then
writeln((ary[1].second):7,'it works !!!');
skip;
writeln('error');
1:
ary[2]:=ary[i-9];
writeln('hex k = ',k:1 hex,'   octal k = ',k oct);
writeln('octal j = ',j:1 oct,'   hex j = ',j hex);
i:=1;
end.
