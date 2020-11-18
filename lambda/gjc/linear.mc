/*  */

eval_when([translate],compgrind:true,transcompile:true);

eval_when([translate,loadfile,batch,demo],
 matchdeclare(a,nonz(),b,freeof(x)));

nonz(e):=is(e#0 and freeof(x,e));

defmatch(lin,a*x+b,x);

