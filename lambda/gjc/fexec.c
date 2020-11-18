
/**************************************
 * Copyright LISP Machine, Inc. 1985  *
 *  See filename "Copyright" for      *
 * licensing and release information. *
 **************************************/

/* execute a subroutine call out of arguments passed in the heap.
   All arguments are call-by-reference, which is FORTRAN style.

   Heap format:

     0    [Opcode]
     1    [Number-Of-Arguments]
     2    [ARG1]
     3    [ARG2]
     ...
     9    [ARG9]
     10   begining of actual argument value storage heap

*/

#include "fort.h"
#include <stdio.h>

fexec(heap) int *heap;
 { int (*func)(), v[9];
   func = fload(heap[0]);
   setargs(heap,&heap[2],v,heap[1]);

   switch (heap[1])
   { case 0: (*func)(); break;
     case 1: (*func)(v[0]); break;
     case 2: (*func)(v[0],v[1]); break;
     case 3: (*func)(v[0],v[1],v[2]); break;
     case 4: (*func)(v[0],v[1],v[2],v[3]); break;
     case 5: (*func)(v[0],v[1],v[2],v[3],v[4]); break;
     case 6: (*func)(v[0],v[1],v[2],v[3],v[4],v[5]); break;
     case 7: (*func)(v[0],v[1],v[2],v[3],v[4],v[5],v[6]); break;
     case 8: (*func)(v[0],v[1],v[2],v[3],v[4],v[5],v[6],v[7]); break;
     case 9: (*func)(v[0],v[1],v[2],v[3],v[4],v[5],v[6],v[7],v[8]); break;
     default: break;}
   heap[0] = 0;}


setargs(absbase,reloffsets,v,n) int *absbase, *reloffsets, **v,n;
 { int j;
   for (j = 0;
        j<n;
        j++)
    {v[j] = reloffsets[j] + absbase;}}
