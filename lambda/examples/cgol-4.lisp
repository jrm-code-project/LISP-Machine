%;;; -*- Mode:LISP; Package:CGOL; Readtable:CGOL; Base:10 -*-%

% Print an expression in CGOL-readable notation. %

prefix "DEFEXT"
        0
        ["DEFUN",[TOKEN & ADVANCE,"EXT"], getvarlist & check ";",right]$

special lbd, rbd, depth $

define "SCHARPOS";
 !(SEND STANDARD-OUTPUT :READ-CURSORPOS :CHARACTER) $


define "PRINTEOL";
if scharpos > 2*depth then newline;
2*depth - scharpos lotsof tyo 32 $

define "QUERIFY1" x;
x and (x := car x . querify1 cdr x;
       if car x < 48 or 57 < car x < 97 or 122 < car x then 63 . x else x) $

define "QUERIFY" x;
let y = if length x > 1 or x isin !'(33 34 58 63) then querify1 x else x;
if y and 47 < car x < 58 then 63 . y else y $

define "LCPRINC" x, 1;
princ if numberp x then x
      else maknam querify((\c; if 64<c<91 then c+32 else c)[exploden x]) $

define "CGOLPRIN1" x,1;  let ?*nopoint=t, terpri=t; cgolprin2(x, 0, 0, 0) $

define "CGOLPRINT" x, 1; newline; cgolprin1 x; princ "?$"; newline $

define "PARENTHESIZE" x;  princ "("; cgolprin2(x, -1, 0, depth); princ ")" $

define "CGOLPRIN2"(x, lbd, rbd, depth);
if atom x then
        (if not numberp x and symbolp x and (getl(x, !'(nud led lbp)) OR FUNBOUNDP(X))
         then princ "#";
         lcprinc x)
else if atom last x then (princ "!"; prin1 x)
else if symbolp car x and "EXT" of car x exists then (apply(it, [cdr x]); t)
else if not(atom car x) and caar x = "LAMBDA" then
  if rbd>0 then parenthesize x
  else (printeol; princ "let ";
        lbd:=-1; rbd:=0;
        cglist((\i,j;["EQUAL",i,j])[cadar x, cdr x], ", ", 0, 0);
        princ ";";
        if null cdddar x then depth:=depth+1;
        printeol;
        cglist(cddar x, "; ", 1, 0))
else naive(x) $

define "NAIVE"(x);
if rbd>25 then parenthesize x
else (if atom car x then
        (if not numberp car x and getl(car x, !'(nud led))
            and null FUNBOUNDP(car x)
         then princ "#";
         lcprinc car x)
      else cgolprin2(car x, lbd, 25, depth+1);
      if null cdr x then princ "()"
      else if cddr x or numberp car x or null(FUNBOUNDP(car x)) then
        if cddr x then (let lbd=0, rbd=0; cglist(cdr x, ", ", 0, 0))
        else parenthesize cadr x
      else (princ " "; cgolprin2(cadr x, 25, rbd, depth+1))) $

define "CGLIST"(x,d,oplb,oprb);
let lbd=lbd, rbd=rbd, paren=nil, depth=depth+1;
if length x > 1 and (lbd >= oplb or rbd > oprb) then
                (princ "("; lbd:=rbd:=0; paren:=t);
x and if null cdr x then cgolprin2(car x, lbd, rbd, depth)
  else (if d = "; " then printeol;
        cgolprin2(car x, lbd, oplb, depth);
        if d isin !'(|and | |or |) then printeol;
        princ d;
        if d = "; " then printeol;
        iter for i := cdr x step cdr i while cdr i do
          (cgolprin2(car i, oprb, oplb, depth);
           if d isin !'(|and | |or |) then printeol;
           princ d; if d = "; " then printeol)
          return cgolprin2(car i, oprb, rbd, depth));
if paren then princ ")" else t $

define "CGVLIST" x;
x and (lcprinc car x; if cdr x then (princ ","; cgvlist cdr x)) $

DEFEXT "COND" x; condprin1(x)$

define "CONDPRIN1" x;
if cdar x and caar x ne t then
  (printeol; princ "if "; cgolprin2(caar x, 2, 0, depth);
   printeol; princ "then ";
   (let lbd=2, rbd = if cdr x then 0 else rbd; cglist(cdar x, "; ", 1, 0));
   if cdr x then (printeol; princ "else "; let lbd=2; condprin1 cdr x))
else if cdr x and caar x ne t then
        (cgolprin2(caar x, lbd, 7, depth+1);
         princ " or ";
         let lbd=7; condprin1 cdr x)
else cglist(if caar x = t then cdar x else car x, "; ", 1, 0) $

define "PAIROFF" x; x and [car x, cadr x] . pairoff cddr x $

DEFEXT "SETQ" x;
  if car x = "IT" then (cgolprin2(cadr x, lbd, 10, depth); princ " exists")
  else if length x = 2 then cglist(x, " := ", 25, 1)
  else (depth:=depth-1; cglist((\y; "SETQ".y)[pairoff x], "; ", 1, 1))
$

"EXT" of "STORE" := "EXT" of "SETQ" $

DEFEXT "PUTPROP" x;
   cgolprin2(["SETQ", ["GET", car x, caddr x], cadr x], lbd, rbd, depth) $

DEFEXT "RPLACA" x;  cgsetq("CAR", x) $

DEFEXT "RPLACD" x;  cgsetq("CDR", x) $

DEFEXT "SETPLIST" x;  cgsetq("PLIST", x) $

DEFEXT "SETARG" x;  cgsetq("ARG", x) $

define "CGSETQ"(fn,x);
  cgolprin2(["SETQ", [fn, car x], cadr x], lbd, rbd, depth) $

DEFEXT "PROG2" x;
 if lbd>0 or rbd>0 then parenthesize("PROG2" . x)
 else (if car x then (cgolprin2(car x, lbd, 1, depth+1); lbd:=0; princ "; ");
       cgolprin2(cadr x, 0, if cddr x then 1 else rbd, depth+1);
       if cddr x then (princ " & "; lbd:=0; cglist(cddr x, "; ", 1, 0))) $

DEFEXT "PROGN" x;
if car x = '"COMPILE"' and depth=0 then #cgolprint[cdr x]
else cglist(x, "; ", 1, 1) $

DEFEXT "PROG" x;
  printeol; let depth=depth+1;
  if caar last x = "RETURN" then
    (let y = reverse x;
     x := nreverse(cadar y . cdr y);
     princ "new ") else princ "prog ";
  cgvlist car x; princ "; "; lbd:=0; cglist(cdr x, "; ", 1, 0) $

DEFEXT "PLUS" x;  cglist(x, "+", 20, 20) $

DEFEXT "+" x; cglist(x, " +# ", 20, 20) $

DEFEXT "+?$" x; cglist(x, " +?$ ", 20, 20) $

DEFEXT "DIFFERENCE" x; cglist(x, "-", 20, 20) $

DEFEXT "-" x; cglist(x, " -# ", 20, 20) $

DEFEXT "-?$" x; cglist(x, " -?$ ", 20, 20) $

DEFEXT "MINUS" x;  princ "-"; cgolprin2(car x, 20, rbd, depth) $

DEFEXT "ADD1" x; cgolprin2(car x, lbd, 20, depth); princ "+1" $

DEFEXT "SUB1" x; cgolprin2(car x, lbd, 20, depth); princ "-1" $

DEFEXT "1+" x; cgolprin2(car x, lbd, 20, depth); princ " +# 1" $

DEFEXT "1-" x; cgolprin2(car x, lbd, 20, depth); princ " -# 1" $

DEFEXT "TIMES" x; cglist(x, "*", 21, 21) $

DEFEXT "*" x; cglist(x, " *# ", 20, 20) $

DEFEXT "*?$" x; cglist(x, " *?$ ", 20, 20) $

DEFEXT "QUOTIENT" x;
cglist(if not atom cadr x and caadr x = "FLOAT"
        then [car x, cadadr x]
        else x,
if not atom cadr x and caadr x = "FLOAT" then "/" else "/:", 21, 21) $

DEFEXT "/" x; cglist(x, " /# ", 20, 20) $

DEFEXT "CONS" x; cglist(x, " . ", 14, 13) $

DEFEXT "APPEND" x; cglist(x, " @ ", 14, 13) $

DEFEXT "TO" x;
(let rbd = if caddr x = 1 then rbd else 18;
   cglist([car x, cadr x], " to ", 18, 18));
if caddr x ne 1 then
  (princ " by "; cgolprin2(caddr x, 18, rbd, depth+1)) $

DEFEXT "LIST" x;
  princ "["; lbd:=-1; rbd:=0; cglist(x, ", ", 0, 0); princ "]" $

DEFEXT "GATHER" x;
  princ "{"; lbd:=-1; rbd:=0; cglist(x, ", ", 0, 0); princ "}" $

DEFEXT "GET" x;
  if caadr x = "QUOTE" then cglist([cadadr x, car x], " ofq ", 26, 25)
  else cglist(reverse x, " of ", 26, 25) $

define "STRINGIFY" x; x and
(x := car x . stringify cdr x;
 if car x isin !'(27. 36.) then 63. . x
 else if car x = 34. then 34. . x
 else x) $

DEFEXT "QUOTE" x;
  if atom car x then (princ """" cat maknam stringify exploden car x cat """")
  else if cdr last car x or Land{atom[car x]} then (princ "!'"; prin1 car x)
  else (princ "'"; cgolprin2(car x, 0, 0, depth); princ "'") $

DEFEXT "LAMBDA" x;
  printeol; let depth=depth+1;
  princ "\"; cgvlist car x; princ "; "; lbd:=0; cglist(cdr x, "; ", 1, 0) $

DEFEXT "EQUAL" x; cglist(x, " = ", 10, 10) $

DEFEXT "GREATERP" x; cglist(x, " > ", 10, 10) $

DEFEXT "LESSP" x; cglist(x, " < ", 10, 10) $

DEFEXT "ZEROP" x; cgolprin2(car x, lbd, 10, depth); princ " = 0" $

DEFEXT "EQ" x; cglist(x, " eq ", 10, 10) $

DEFEXT "=" x; cglist(x, " =# ", 10, 10) $

DEFEXT ">" x; cglist(x,  " ># ", 10, 10) $

DEFEXT "<" x; cglist(x, " <# ", 10, 10) $

DEFEXT "MEMBER" x; cglist(x, " isin ", 10, 10) $

DEFEXT "AND" x;
  if oddp scharpos then princ " ";
  depth := scharpos/:2-1;
  cglist(x, "and ", 8, 8) $

DEFEXT "OR" x;
  if oddp scharpos then princ " ";
  depth := scharpos/:2-1;
  cglist(x, "or ", 7, 7) $

DEFEXT "NOT" x; princ "not "; cgolprin2(car x, 9, rbd, depth) $

DEFEXT "DEFPROP" x;
if caddr x isin !'(EXPR FEXPR MACRO) and caadr x = "LAMBDA"
then cgolprin2("DEFUN" . car x . if caddr x = "EXPR" then cdadr x
                                 else caddr x . cdadr x,
                lbd, rbd, depth)
else naive("DEFPROP" . x) $

DEFEXT "DEFUN" x;
  printeol; let depth=depth+1;
  princ "define ";
  if cadr x isin !'(expr fexpr macro) then x := cadr x . car x . cddr x;
  if car x isin !'(expr fexpr macro) then
        (lcprinc car x; princ " "; x:=cdr x);
  princ """" cat car x cat  """";
princ "("; cgvlist cadr x; princ ")";
  princ "; ";
  if flatsize cddr x > 60 then printeol;
  lbd:=0; cglist(cddr x, "; ", 1, 0) $

DEFEXT "TERPRI" x; princ "newline" $

define "INCR"(x);
  [car x, cadr x, if cadr caddr x = car x
                  then if caaddr x isin !'(PLUS +) then caddr caddr x
                       else if caaddr x isin !'(ADD1 1+) then 1
                       else throw("DO",nil)
                  else throw("DO",nil)] $

define "LIMTEST"(x, y);
for i in if car x = "OR" then cdr x else [x], j in y collect
  if car i isin !'(GREATERP >) and cadr i = car j then caddr i
  else throw("DO",nil) $

define "MKTO"(x, y); ["TO", cadr x, y, caddr x] $

DEFEXT "DO" x;
  if car x and atom car x then
      cgolprin2("DO" . [[car x, cadr x, caddr x]] . [cadddr x] . cddddr x,
                lbd, rbd, depth)
  else catch("DO",new xi,yi; (xi := incr[car x]) and (yi := limtest(caadr x, xi)) and
                  cgolprin2("MAPC" . ["FUNCTION", "LAMBDA" . car[xi] . cddr x] . mkto[xi,yi],
                            lbd, rbd, depth))
  or   (printeol; princ "iter "; let depth=depth+1;
        for i in car x do
          (printeol; princ "for "; lcprinc car i;
           if cdr i then (princ " := ";
                           cgolprin2(cadr i, 2, 0, depth+1);
                           if cddr i then
                             (princ " step "; cgolprin2(caddr i,2,0,depth+1))));
        if caadr x then
          (printeol;
           cgolprin2(if caaadr x = "NOT" then (princ " while ";
                                               cadr caadr x)
                                         else (princ "until ";
                                               caadr x), 2,
                                               if cddr x or cdadr x
                                                then 0
                                                else rbd,
                                               depth+1));
        if cddr x then (printeol; princ "do ";
                        let lbd=2, rbd=if cdadr x then 0 else rbd;
                        cglist(cddr x, "; ", 1, 0));
        if cdadr x then (printeol; princ "return ";
                         let lbd=2; cglist(cdadr x, "; ", 1, 0))) $

DEFEXT "MAPC" x; cgmap(x, " in ", " do ", "mapc") $
DEFEXT "MAPCAR" x; cgmap(x, " in ", " collect ", "mapcar") $
DEFEXT "MAPCAN" x; cgmap(x, " in ", " coalesce ", "mapcan") $
DEFEXT "MAP" x; cgmap(x, " on ", " do ", "map") $
DEFEXT "MAPLIST" x; cgmap(x, " on ", " collect ", "maplist") $
DEFEXT "MAPCON" x; cgmap(x, " on ", " coalesce ", "mapcon") $

define "CGMAP"(x, inon, disp, typ);
if atom car x or caar x not isin !'(quote function)
   or if atom cadar x then typ ne "mapcar" else caadar x ne "LAMBDA"
then (princ typ; let lbd=0, rbd=0; cglist(x, ", ", 0, 0))
else if atom cadar x and typ = "mapcar" then
  (cgolprin2(cadar x, lbd, rbd, depth);
   princ "["; let lbd=-1, rbd=0;
   cglist(cdr x, ", ", 0, 0);
   princ "]")
else (printeol;
      princ "for ";
      for vars on cadr cadar x, argts on cdr x do
        (lcprinc car vars;
         princ inon;
         cgolprin2(car argts, 2, 0, depth+1);
         if cdr vars then princ ", ");
      princ disp;
      let depth = depth+1; printeol;
      cglist(cddr cadar x, "; ", 1, 0)) $
