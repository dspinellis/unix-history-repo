
{ if(begin != 1)
  { if($1 != "") flags[$1] = "on"
    if($2 != "") flags[$2] = "on"
    if($3 != "") flags[$3] = "on"
    if($4 != "") flags[$4] = "on"
    if($5 != "") flags[$5] = "on"
    skip = 0
    begin = 1
    next } }
/#ifdef/||/#elseif/ { if (flags[$2] != "") skip = -1; else skip = 1
	   	      print $0
	   	      next }
/#ifndef/ { if (flags[$2] != "") skip = 1; else skip = -1
	   	      print $0
	   	      next }
/#else/ { skip = -skip; print $0; next}
/#endif/ { skip = 0 ; print $0; next}
{ if(skip > 0)
    { if(substr($0,1,1) != "#") print "#" $0; else print $0;
      next }}
{ if(skip < 0)
    { if(substr($0,1,1) == "#") 
          print substr($0,2,length($0)-1) 
	  else print $0
      next }}
{ print $0 }


