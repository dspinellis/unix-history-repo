: loop
if $1x = x exit
echo $1:
ed $1
g/Ship.damage\[\(.*\)]/s//damaged(\1)/
w
q
shift
goto loop
