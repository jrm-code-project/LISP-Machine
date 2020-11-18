Message-ID: <8509120034.AA04869@cit-vax.ARPA>
Date: Wednesday, 11 September 1985, 20:32-EDT
From: Kevin Carosso <engvax!KVC@cit-vax.ARPA>
Subject: My benchmark collection
To: info-vax@cit-vax.ARPA

I had several requests for this stuff, so here it is.  Hope this
isn't too big for a net posting, but it's no longer than most of
the ART/ANTI-ART flamings in SF-LOVERS... :-)

This is a little collection of benchmarks I used to test my 785.
WHETSTONE.C is simply the whetstone benchmark in C.  On a 785 with
FPA, this should take about 1.3 CPU seconds.  On a 780 or 785 with
a bad MULD in the FPA this takes about 1.8 CPU seconds.  I think 750s
with FPA were about 2.7, but don't remember for sure.  We don't mess
around with the little stuff around here!

BENCH.COM, GENERIC.MAR, and TESTIT.COM comprise a nifty little
procedure I made up.  TESTIT.COM takes two parameters, a VAX
3-operand instruction and a DECnet node name.  It edits the
instruction into GENERIC.MAR (slight changes to GENERIC.MAR would
allow any instruction to be tested), assembles and links the
resulting MACRO code, and sends it over the network to the specified
system to be run.  The results of the run will end up back on
your system in a file called RESULTS.node-name_instruction.
The macro code is 10000 iterations of a sequence of 1000
register-register-register operations of the instruction.
The reason I put the instructions in line is to reduce the
percentage of code that was simply loop control.

Don't use H floating without changing GENERIC.MAR, it walks
on the loop count register and runs forever!!!

Before anyone flames at me, I will be the first to say that this
benchmark is ONLY USEFUL FOR COMPARING INSTRUCTION VERSUS
INSTRUCTION ON VARIOUS IMPLEMENTATIONS OF THE VAX ARCHITECTURE!!!!!!!!!
Do not try using this to create MIPS ratings, which are generally
meaningless anyway.  To see just how meaningless, try this with
various instructions (the I in MIPS), divide by 10 and try telling
someone which result defines MIPS for VAX.  For even more chaos,
try making up a normalization factor for something as simple as
comparing 780 to 750.  Also note that where it matters, this probably
pipelines fairly unnaturally.

There is room for expansion in TESTIT.COM, for example maybe
TESTIT really oughta crawl across the entire network, trying every
machine it can find and reporting back the results.  This could
be especially enlightening on the DEC e-net...  You never know
what ya might find there...

        "Faster than an 8600 you say?  naw..."

or perhaps

        "Slower than a uVAX-I?  Guess it never made it to marketing..."

I'd be careful on this, if your worm isn't smart it'll soon have
every system on the net running 35 bench-mark processes, with no end
in sight.  This may make you unpopular with the natives.

There are also two files here which are the output of TESTIT
on my system before and after the FPA was upgraded.  Compare
your results to these.  Your mileage may vary, but not by much.
The systems I've tested come out very close, except for one 785
that does EVERY instruction about 10% too slow...  Still looking
into that one...

        /Kevin Carosso           engvax!kvc @ CIT-VAX.ARPA
         Hughes Aircraft Co.

Cut here and execute the resulting .COM file to unpack the files.
Oh, this has long filenames.  You paleontologists with VMS 3 (or
less!) will have to edit the .COM file a bit before running...

-----------------------------------------------------------------------------
$ show default
$ write sys$output "Creating BENCH.COM"
$ create BENCH.COM
$ DECK/DOLLARS="*$*$*EOD*$*$*"
$ set nover
$ set noon
$ define sys$output sys$net
$ run bench
$ run bench
$ run bench
$ run bench
$ run bench
$ run bench
$ run bench
$ run bench
$ run bench
$ run bench
$ exit
*$*$*EOD*$*$*
$ write sys$output "Creating GENERIC.MAR"
$ create GENERIC.MAR
$ DECK/DOLLARS="*$*$*EOD*$*$*"
        .Title  generic - Generic bench marker

;
; Bench-mark instructions.  Replace the single occurance of ASTERISKS with the
; instruction to be tested.  The instruction must be floating point 3-operand
; with sources in r4 and r6, destination in r8.
;
        .Entry  start, 0

        movl    #^F1235.533, r4
        movl    #^F3.5, r6
        movl    #10000, r10

        calls   #0, G^LIB$INIT_TIMER

loop:   .Repeat 1000
        ****    r4, r6, r8
        .Endr

        decl    r10
        beql    done
        brw     loop

done:   calls   #0, G^LIB$SHOW_TIMER
        ret

        .End    start
*$*$*EOD*$*$*
$ write sys$output "Creating RESULTS.ENGVAX_MULD3_AFTER"
$ create RESULTS.ENGVAX_MULD3_AFTER
$ DECK/DOLLARS="*$*$*EOD*$*$*"
 ELAPSED: 00:00:28.40  CPU: 0:00:28.37  BUFIO: 0  DIRIO: 0  FAULTS: 0
 ELAPSED: 00:00:28.74  CPU: 0:00:28.40  BUFIO: 0  DIRIO: 0  FAULTS: 1
 ELAPSED: 00:00:28.40  CPU: 0:00:28.37  BUFIO: 0  DIRIO: 0  FAULTS: 0
 ELAPSED: 00:00:28.44  CPU: 0:00:28.39  BUFIO: 0  DIRIO: 0  FAULTS: 0
 ELAPSED: 00:00:28.39  CPU: 0:00:28.37  BUFIO: 0  DIRIO: 0  FAULTS: 0
 ELAPSED: 00:00:28.39  CPU: 0:00:28.37  BUFIO: 0  DIRIO: 0  FAULTS: 0
 ELAPSED: 00:00:28.42  CPU: 0:00:28.36  BUFIO: 0  DIRIO: 0  FAULTS: 1
 ELAPSED: 00:00:28.43  CPU: 0:00:28.33  BUFIO: 0  DIRIO: 0  FAULTS: 0
 ELAPSED: 00:00:29.15  CPU: 0:00:28.36  BUFIO: 0  DIRIO: 0  FAULTS: 1
 ELAPSED: 00:00:29.44  CPU: 0:00:28.34  BUFIO: 0  DIRIO: 0  FAULTS: 0
*$*$*EOD*$*$*
$ write sys$output "Creating RESULTS.ENGVAX_MULD3_BEFORE"
$ create RESULTS.ENGVAX_MULD3_BEFORE
$ DECK/DOLLARS="*$*$*EOD*$*$*"
 ELAPSED: 00:02:49.36  CPU: 0:02:44.53  BUFIO: 0  DIRIO: 0  FAULTS: 1
 ELAPSED: 00:02:45.99  CPU: 0:02:44.70  BUFIO: 0  DIRIO: 0  FAULTS: 0
 ELAPSED: 00:02:47.24  CPU: 0:02:44.64  BUFIO: 0  DIRIO: 0  FAULTS: 1
 ELAPSED: 00:03:18.59  CPU: 0:02:45.40  BUFIO: 0  DIRIO: 0  FAULTS: 0
 ELAPSED: 00:02:48.42  CPU: 0:02:44.87  BUFIO: 0  DIRIO: 0  FAULTS: 0
 ELAPSED: 00:02:46.14  CPU: 0:02:44.89  BUFIO: 0  DIRIO: 0  FAULTS: 1
 ELAPSED: 00:02:46.70  CPU: 0:02:44.98  BUFIO: 0  DIRIO: 0  FAULTS: 0
 ELAPSED: 00:02:51.44  CPU: 0:02:45.04  BUFIO: 0  DIRIO: 0  FAULTS: 0
 ELAPSED: 00:02:53.15  CPU: 0:02:44.85  BUFIO: 0  DIRIO: 0  FAULTS: 0
 ELAPSED: 00:02:54.97  CPU: 0:02:44.66  BUFIO: 0  DIRIO: 0  FAULTS: 1
*$*$*EOD*$*$*
$ write sys$output "Creating TESTIT.COM"
$ create TESTIT.COM
$ DECK/DOLLARS="*$*$*EOD*$*$*"
$ !
$ ! Run the bench_mark on the known systems.
$ !
$       on warning then goto done
$       prev_dir = f$environment ("DEFAULT")
$       set default ctc:[kvc.bench.test]
$ !
$ Get_inst:
$       if P1 .eqs. "" then inquire P1 "Instruction"
$       if P1 .eqs. "" then goto Get_inst
$ !
$ Get_node:
$       if P2 .eqs. "" then inquire P2 "Node"
$       if P2 .eqs. "" then goto Get_node
$ !
$       unique = f$time() - " " - "-" - "-" - " " - ":" - ":" - "."
$       instruction = P1
$       node = P2
$       test_file = "TEST_" + instruction + "_" + unique
$       edit_file = "edit_" + unique + ".tmp"
$ !
$ ! Edit it into the generic bench marker, to produce this one.
$ !
$       open/write e 'edit_file
$       write e "s/****/''instruction'/wh"
$       write e "exit"
$       close e
$ !
$       define/user sys$output _nla0:
$       edit/edt/command='edit_file'/nojournal/output='test_file'.mar -
 generic.mar
$ !
$       macro 'test_file /nolist
$       link 'test_file /nosysshr/nomap ! Avoid ident mismatch errors
$ !
$ ! Move the stuff to the target node
$ !
$       copy/noconcatenate bench.com,'test_file'.exe 'node'::bench
$ !
$ ! Now run it, and save the results in a test file.
$ !
$       type/output=results.'node'_'instruction' 'node'::"task=bench"
$ !
$ ! Now clean things up.
$ !
$       delete 'node'::bench.exe;,'node'::bench.com;
$ Done:
$       set noon
$       delete 'edit_file';*
$       delete 'test_file'.*;
$       set default 'prev_dir
$       exit
*$*$*EOD*$*$*
$ write sys$output "Creating WHETSTONE.C"
$ create WHETSTONE.C
$ DECK/DOLLARS="*$*$*EOD*$*$*"
/*
Enclosed below is a C translation of the famous "Whetstone Benchmark"
from the original Algol version.  I have inserted printf()'s as a
compiler option.  I think this translation is accurate.  The only
numbers I have to compare with are from an old Ridge-32 machine, and
these are from a Pascal translation (I caught one error in their
translation).  If anyone has any nunbers from FORTRAN, Pascal, or Algol
versions of the Whetstone, I would very much like to see them.

                                David Hinnant
                                SCI Systems, Inc.
                                {decvax, akgua}!mcnc!rti-sel!scirtp!dfh


P.s., there is a .signature file at the end of the listing.

 *      Whetstone benchmark in C.  This program is a translation of the
 *      original Algol version in "A Synthetic Benchmark" by H.J. Curnow
 *      and B.A. Wichman in Computer Journal, Vol  19 #1, February 1976.
 *
 *      Used to test compiler optimization and floating point performance.
 *
 *      Compile by:             cc -O -s -o whet whet.c
 *      or:                     cc -O -DPOUT -s -o whet whet.c
 *      if output is desired.
 */

#define ITERATIONS      10 /* 1 Million Whetstone instructions */

#include <math>

double          x1, x2, x3, x4, x, y, z, t, t1, t2;
double          e1[4];
int             i, j, k, l, n1, n2, n3, n4, n6, n7, n8, n9, n10, n11;

main()
{

        /* initialize constants */

        t   =   0.499975;
        t1  =   0.50025;
        t2  =   2.0;

        /* set values of module weights */

        n1  =   0 * ITERATIONS;
        n2  =  12 * ITERATIONS;
        n3  =  14 * ITERATIONS;
        n4  = 345 * ITERATIONS;
        n6  = 210 * ITERATIONS;
        n7  =  32 * ITERATIONS;
        n8  = 899 * ITERATIONS;
        n9  = 616 * ITERATIONS;
        n10 =   0 * ITERATIONS;
        n11 =  93 * ITERATIONS;

/* MODULE 1:  simple identifiers */

        x1 =  1.0;
        x2 = x3 = x4 = -1.0;

        for(i = 1; i <= n1; i += 1) {
                x1 = ( x1 + x2 + x3 - x4 ) * t;
                x2 = ( x1 + x2 - x3 - x4 ) * t;
                x3 = ( x1 - x2 + x3 + x4 ) * t;
                x4 = (-x1 + x2 + x3 + x4 ) * t;
        }
#ifdef POUT
        pout(n1, n1, n1, x1, x2, x3, x4);
#endif


/* MODULE 2:  array elements */

        e1[0] =  1.0;
        e1[1] = e1[2] = e1[3] = -1.0;

        for (i = 1; i <= n2; i +=1) {
                e1[0] = ( e1[0] + e1[1] + e1[2] - e1[3] ) * t;
                e1[1] = ( e1[0] + e1[1] - e1[2] + e1[3] ) * t;
                e1[2] = ( e1[0] - e1[1] + e1[2] + e1[3] ) * t;
                e1[3] = (-e1[0] + e1[1] + e1[2] + e1[3] ) * t;
        }
#ifdef POUT
        pout(n2, n3, n2, e1[0], e1[1], e1[2], e1[3]);
#endif

/* MODULE 3:  array as parameter */

        for (i = 1; i <= n3; i += 1)
                pa(e1);
#ifdef POUT
        pout(n3, n2, n2, e1[0], e1[1], e1[2], e1[3]);
#endif

/* MODULE 4:  conditional jumps */

        j = 1;
        for (i = 1; i <= n4; i += 1) {
                if (j == 1)
                        j = 2;
                else
                        j = 3;

                if (j > 2)
                        j = 0;
                else
                        j = 1;

                if (j < 1 )
                        j = 1;
                else
                        j = 0;
        }
#ifdef POUT
        pout(n4, j, j, x1, x2, x3, x4);
#endif

/* MODULE 5:  omitted */

/* MODULE 6:  integer arithmetic */

        j = 1;
        k = 2;
        l = 3;

        for (i = 1; i <= n6; i += 1) {
                j = j * (k - j) * (l -k);
                k = l * k - (l - j) * k;
                l = (l - k) * (k + j);

                e1[l - 2] = j + k + l;          /* C arrays are zero based */
                e1[k - 2] = j * k * l;
        }
#ifdef POUT
        pout(n6, j, k, e1[0], e1[1], e1[2], e1[3]);
#endif

/* MODULE 7:  trig. functions */

        x = y = 0.5;

        for(i = 1; i <= n7; i +=1) {
                x = t * atan(t2*sin(x)*cos(x)/(cos(x+y)+cos(x-y)-1.0));
                y = t * atan(t2*sin(y)*cos(y)/(cos(x+y)+cos(x-y)-1.0));
        }
#ifdef POUT
        pout(n7, j, k, x, x, y, y);
#endif

/* MODULE 8:  procedure calls */

        x = y = z = 1.0;

        for (i = 1; i <= n8; i +=1)
                p3(x, y, &z);
#ifdef POUT
        pout(n8, j, k, x, y, z, z);
#endif

/* MODULE9:  array references */

        j = 1;
        k = 2;
        l = 3;

        e1[0] = 1.0;
        e1[1] = 2.0;
        e1[2] = 3.0;

        for(i = 1; i <= n9; i += 1)
                p0();
#ifdef POUT
        pout(n9, j, k, e1[0], e1[1], e1[2], e1[3]);
#endif

/* MODULE10:  integer arithmetic */

        j = 2;
        k = 3;

        for(i = 1; i <= n10; i +=1) {
                j = j + k;
                k = j + k;
                j = k - j;
                k = k - j - j;
        }
#ifdef POUT
        pout(n10, j, k, x1, x2, x3, x4);
#endif

/* MODULE11:  standard functions */

        x = 0.75;
        for(i = 1; i <= n11; i +=1)
                x = sqrt( exp( log(x) / t1));

#ifdef POUT
        pout(n11, j, k, x, x, x, x);
#endif
}

pa(e)
double e[4];
{
        register int j;

        j = 0;
     lab:
        e[0] = (  e[0] + e[1] + e[2] - e[3] ) * t;
        e[1] = (  e[0] + e[1] - e[2] + e[3] ) * t;
        e[2] = (  e[0] - e[1] + e[2] + e[3] ) * t;
        e[3] = ( -e[0] + e[1] + e[2] + e[3] ) / t2;
        j += 1;
        if (j < 6)
                goto lab;
}


p3(x, y, z)
double x, y, *z;
{
        x  = t * (x + y);
        y  = t * (x + y);
        *z = (x + y) /t2;
}


p0()
{
        e1[j] = e1[k];
        e1[k] = e1[l];
        e1[l] = e1[j];
}

#ifdef POUT
pout(n, j, k, x1, x2, x3, x4)
int n, j, k;
double x1, x2, x3, x4;
{
        printf("%6d%6d%6d  %5e  %5e  %5e  %5e\n",
                n, j, k, x1, x2, x3, x4);
}
#endif
*$*$*EOD*$*$*
$ exit
