{ AN OPEN CHALLENGE TO ALL PASCAL COMPILERS:  Compile this program
   correctly and produce the correct output. 

  Send responses, counter-challenges, etc., to:
   Tom Pennello
   Computer and Information Sciences
   University of California
   Santa Cruz, CA.  95064
}

program p(output);
type integer = -32767..32768; node = 0..4500;
var Debug: boolean;

procedure EachUSCC( { of relation R }
      { Pass in an iterator to generate the nodes to be searched,
        the relation on the nodes, a procedure to do information
        propagation when V R W is discovered, a procedure
        to take each SCC found, and a procedure to yield each
        node in the graph. 
        We require that the nodes be of a scalar type so that
        an array may be indexed by them.  Also passed in is the
        upper bound of the node type. }
      procedure EachUnode(procedure P(T:node)); { Yields each node in graph }
      procedure EachUnodeUtoUsearch(procedure SearchU(V:node));
      procedure R(V:node; procedure DoUsuccessor(W:node));
      procedure Propagate(V,W:node); { called when V R W is discovered }
      procedure TakeUSCC(Root:node; procedure Each(procedure P(T:node)));
      LastUnode: integer
      ); 

   type
      A = array[node] of integer; { range 0..Infinity (below) }
   var N: ^A;  SP: integer;
      Stack: array[node {1..LastUnode}] of node;
      Infinity: integer; { LastUnode+1 }

   procedure P(T:node); begin N^[T] := 0; end;
   
   procedure Search(V:node);
      var I,T:integer;
      procedure DoUsuccessor(W:node);
         begin
         Search(W);
         if N^[W] < N^[V] then N^[V] := N^[W];
         Propagate(V,W);
         end;

      { EachUmember is yielded by EachUSCC when an SCC has been found. }
      procedure EachUmember(procedure P(TU:node));
         var I:integer;
         begin { yield each member of current SCC }
         for I := SP downto T do P(Stack[I]);
         end;

      procedure YieldUSCC; 
         begin
if Debug then writeln('YieldUSCC passes',V,' to TakeUSCC');
         TakeUSCC(V,EachUmember);
         end; 

      begin
      if N^[V] = 0 then begin { stack the node }
if Debug then writeln('stacking ',V);
         SP := SP+1;
         Stack[SP] := V;  N^[V] := SP;
if Debug then writeln('Doing successors of ',V);
         R(V,DoUsuccessor);
if Debug then writeln('Now checking if ',V,' is an SCC root');
         if V = Stack[N^[V]] then begin { V is root of an SCC }
            T := N^[V];
if Debug then writeln(V,' is an SCC root; SP=',SP,' T=',T);
            for I := SP downto T do N^[Stack[I]] := Infinity;
            if SP <> T then begin
if Debug then writeln('Yield SCC should pass ',V,' out to TakeUSCC');
               YieldUSCC;
               end;
            SP := T-1;
            end;
         end;
      end; 
   begin
   Infinity := LastUnode+1;
   new(N); EachUnode(P);
   SP := 0;
   EachUnodeUtoUsearch(Search);
   dispose(N);
   end;

procedure Outer; { needed to produce bug in Berkeley Pascal compiler }
   procedure q;
      procedure EachUnodeUtoUsearch(procedure Search(T:node));
         begin 
         Search(1);
         end;
      procedure EachUnode(procedure P(T:node));
         begin P(1); P(2);
         end;
      procedure R(V:node; procedure P(W:node));
         begin 
         { Defines graph with edges 1->2 and 2->1 }
         { Thus, the graph contains one SCC:  [1,2] }
         case V of
            1: P(2); 
            2: P(1);
            end;
         end;
      procedure Propagate(V,W:node); begin end;
      procedure TakeUSCC(Root:node; procedure Each(procedure P(T:node)));
         procedure P(T:node); begin write(T); end;
         begin
         writeln('TakeUSCC receives V=',Root,' from YieldUSCC');
         writeln('The SCC''s constituents are:');
         Each(P); writeln;
         end;
      begin
      EachUSCC(EachUnode,EachUnodeUtoUsearch,R,Propagate,TakeUSCC,2);
      end;

procedure Doit;
   begin
   q;
   end;

begin
Doit;
end;

begin
Debug := true;
Outer;
end.

{----------------------------------------------------------------
  An alternate version of this program, written in a language 
  supporting iterators, iterators as parameters, iterators
  as yielded results of iterators, and the ability to yield more
  than one thing, might be as follows:
 ----------------------------------------------------------------

iterator EachUSCC(
   iterator EachUnode:node;
   iterator EachUnodeUtoUsearch:node;
   iterator R(V:node):node;
   procedure Propagate(V,W:node);
   LastUnode:node
                 ):
   (Root:node; iterator EachUnodeUinUSCC:node);
   # EachUSCC yields two results:  the Root of the SCC,
   # and an iterator that yields each member of that SCC.

   type A = aray[node] of integer;
   var N: ^A; SP: integer;
       Stack: array[node] of node;
       Infinity: integer;
 
   procedure Search(V);
      var T: integer;
      iterator EachUmember:node;
         begin
         for I := SP downto T do P(Stack[I]);
         end;
      begin
      if N^[V] = 0 then begin
         SP := SP+1;  Stack[SP] := V;  N^[V] := SP;
         for W in R(V) do begin   # Search successors of V.
            Search(W);
            if N^[W] < N^[V] then N^[V] := N^[W];
            Propagate(V,W);
            end;
         if V = Stack[N^[V]] then begin 
            T := N^[V];           # V is an SCC root.
            for I := SP downto T do N^[Stack[I]] := Infinity;
            if SP <> T then       # Non-trivial SCC.
               yield(V,EachUmember);
            SP := T-1;
            end;
         end;
      end;

   begin
   Infinity := LastUnode+1;
   new(N);
   for T in EachUnode() do N^[T] := 0;  # for loops declare their
   SP := 0;                             # control variable as a constant.
   for V in EachUnodeUtoUsearch() do Search(V);
   dispose(N);
   end;

# Sample use of EachUSCC:

iterator EachUnodeUtoUsearch:node;
   begin 
   yield(1);
   end;
iterator EachUnode:node;
   begin
   yield(1); yield(2);
   end;
iterator R(V:node):node;
   begin
   if V = 1 then yield(2) else yield(1);
   end;
procedure Propagate(V,W:node); begin end;

procedure UseUEachUSCC;
   begin
   for (Root,EachUmember) in 
       EachUSCC(EachUnode,EachUnodeUtoUsearch,R,Propagate,TakeUSCC,2) do
      begin
      writeln('Root of received SCC is ',Root);
      writeln('Constituents of the SCC are:');
      for I in EachUmember() do write(I);
      writeln;
      end;
   end;
}
