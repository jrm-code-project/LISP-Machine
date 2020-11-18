@c storage-conventions.botex
@c
@c 16-Nov-87, Kent Hoult
@c 31-Mar-88, James Rauen

@chapter Storage Conventions

This chapter defines the storage conventions used for the various
types of objects in the K processor.

@section Structure of Data Words

Words in the machine consist of 33 bits. The most significant
bit (bit 32) is referred to as the BOX bit. When zero, the other 32
bits of the word contain an untyped 32 bit number. Normally only
internal system routines will access unboxed data.

@tex
\startbyf
\bif{32}    {0}
\byf{31}{ 0}{Unboxed Data}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil\byfbox}
@end tex

When the box bit is set, the word is a typed LISP value. The most
significant 6 bits (26-31) indicate the data type of the word. Bits 0-25
vary in meaning with the data type.

@tex
\startbyf
\bif{32}    {1}
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Pointer}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil\byfbox}
@end tex

@section Tables of Data Types

The six DTP (Data Type) bits can represent up to 64 distinct types.
The data type definitions currently reside in JB: K; DATA-TYPES LISP.
Data types 0 to 31 are visible data types.  (They identify valid Lisp
objects.)  Data types 32 to 63 are magic, invisible data types.  I
believe that this distinction is arbitrary, but don't quote me on
it.

Data types 26 through 31 and 49 through 63 are currently unassigned.

@subsection Visible Data Types

@settabs 6 @columns
@sp 1
@< @i[DTP]       @\ @i[DTP] @cr
@< @i[(Decimal)] @\ @i[(Binary)] @\ @i[Data Type] @cr
@sp 1
@< 0  @\ 000000 @\ $$DTP-NIL @cr
@< 1  @\ 000001 @\ $$DTP-FIXNUM @cr
@< 2  @\ 000010 @\ $$DTP-CONS @cr
@< 3  @\ 000011 @\ $$DTP-SYMBOL @cr
@sp 1
@< 4  @\ 000100 @\ $$DTP-BIGNUM @cr
@< 5  @\ 000101 @\ $$DTP-SHORT-FLOAT @cr
@< 6  @\ 000110 @\ $$DTP-SINGLE-FLOAT @cr
@< 7  @\ 000111 @\ $$DTP-DOUBLE-FLOAT @cr
@sp 1
@< 8  @\ 001000 @\ $$DTP-RATIONAL @cr
@< 9  @\ 001001 @\ $$DTP-COMPLEX @cr
@< 10 @\ 001010 @\ $$DTP-LOCATIVE @cr
@< 11 @\ 001011 @\ $$DTP-UNBOXED-LOCATIVE @cr
@sp 1
@< 12 @\ 001100 @\ $$DTP-COMPILED-FUNCTION @cr
@< 13 @\ 001101 @\ $$DTP-CODE @cr
@< 14 @\ 001110 @\ $$DTP-ARRAY @cr
@< 15 @\ 001111 @\ $$DTP-STACK-GROUP @cr
@sp 1
@< 16 @\ 010000 @\ $$DTP-INSTANCE @cr
@< 17 @\ 010001 @\ $$DTP-LEXICAL-CLOSURE @cr
@< 18 @\ 010010 @\ $$DTP-INTERPRETER-CLOSURE @cr
@< 19 @\ 010011 @\ $$DTP-LEXICAL-ENVIRONMENT @cr
@sp 1
@< 20 @\ 010100 @\ $$DTP-STRUCTURE @cr
@< 21 @\ 010101 @\ $$DTP-CHARACTER @cr
@< 22 @\ 010110 @\ $$DTP-EXTEND @cr
@< 23 @\ 010111 @\ $$DTP-ENCAPSULATION @cr
@< 24 @\ 011000 @\ $$DTP-HASH-TABLE @cr
@cleartabs

@subsection Invisible Data Types

@settabs 6 @columns
@sp 1
@< @i[DTP]       @\ @i[DTP] @cr
@< @i[(Decimal)] @\ @i[(Binary)] @\ @i[Data Type] @cr
@sp 1
@< 32 @\ 100000 @\ $$DTP-UNBOXED-HEADER @cr
@< 33 @\ 100001 @\ $$DTP-SYMBOL-HEADER @cr
@< 34 @\ 100010 @\ $$DTP-ARRAY-HEADER-SINGLE @cr
@< 35 @\ 100011 @\ $$DTP-ARRAY-HEADER-MULTIPLE @cr
@sp 1
@< 36 @\ 100100 @\ $$DTP-ARRAY-HEADER-EXTENSION @cr
@< 37 @\ 100101 @\ $$DTP-EXTERNAL-VALUE-CELL-POINTER @cr
@< 38 @\ 100110 @\ $$DTP-GC-FORWARD @cr
@< 39 @\ 100111 @\ $$DTP-ONE-Q-FORWARD @cr
@sp 1
@< 40 @\ 101000 @\ $$DTP-INDEXED-FORWARD @cr
@< 41 @\ 101001 @\ $$DTP-INSTANCE-HEADER @cr
@< 42 @\ 101010 @\ $$DTP-ARRAY-LEADER-HEADER @cr
@< 43 @\ 101011 @\ $$DTP-UNBOUND @cr
@sp 1
@< 44 @\ 101100 @\ $$DTP-HEADER-FORWARD @cr
@< 45 @\ 101101 @\ $$DTP-BODY-FORWARD @cr
@< 46 @\ 101110 @\ $$DTP-COMPILED-FUNCTION-HEADER @cr
@< 47 @\ 101111 @\ $$DTP-STRUCTURE-HEADER @cr
@< 48 @\ 110000 @\ $$DTP-HASH-TABLE-HEADER @cr
@cleartabs

@section Numbers

@subsection Fixnums

Fixnums have a data-type of $$DTP-FIXNUM. Bits 0-23 of the pointer field
contain a 24 bit two's complement value. Bits 24-25 are unused and
should be zero.

Fixnums have a range of -2**23 through (2**23 - 1).

@tex
\startbyf
\bif{32}    {1}
\byf{31}{26}{000001}
\byf{25}{24}{00}
\byf{23}{ 0}{24 Bit Two's Complement Value}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil\byfbox}
@end tex

@subsection Bignums

Bignums have a data-type of $$DTP-BIGNUM. A bignum is a pointer to a
memory structure. The memory structure consists of a header word with
data-type $$DTP-UNBOXED-HEADER and a pointer field containing the number
of words of storage used for the bignum data words. The words after the
header for a N word two's complement number stored least significant
word first. Bignums are allocated in structure space.

Bignums may use up to 2**18 words of storage for a number.  However
canonical bignums must be larger than a fixnum's range, or they should
be converted to a fixnum.

The range of bignums is -2**(2**23) to (2**(2**23) - 1).

@tex
\startbyf
\bif{32}    {1}
\byf{31}{26}{000100}
\byf{25}{ 0}{Pointer (P)}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil\byfbox}
\vskip 0.2in

\startbyf
\byf{31}{26}{100000}
\byf{25}{ 0}{Number of data words (N)}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil$P\rightarrow$\byfbox}

\startbyf
\byf{31}{ 0}{Least Significant Word}
\endbyf
\line{\hfil$P+1\rightarrow$\byfbox}

\startbyf
\byf{31}{ 0}{Middle Words $\ldots$}
\endbyf
\line{\hfil$\ldots$\byfbox}

\startbyf
\byf{31}{ 0}{Most Significant Word}
\endbyf
\line{\hfil$P+N\rightarrow$\byfbox}
@end tex

@subsection Rationals

Rational numbers consist of a numerator and denominator.  Each may be
either a fixnum or a bignum. A rational has a data-type of
$$DTP-RATIONAL. The pointer field points to a pair of words in memory
which contain the numerator and denominator pointers. The storage for
rationals is allocated in CONS space.

@tex
\startbyf
\bif{32}    {1}
\byf{31}{26}{0001000}
\byf{25}{ 0}{Pointer (P)}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil\byfbox}
\vskip 0.2in

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Fixnum or Bignum (Numerator)}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil$P\rightarrow$\byfbox}

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Fixnum or Bignum (Denominator)}
\endbyf
\line{\hfil$P+1\rightarrow$\byfbox}
@end tex

@subsection Complex

Complex numbers consist of a real and imaginary part. Each may be any
numeric type except complex. A complex has a data-type of $$DTP-COMPLEX.
The pointer field points to a pair of words in memory which contain the
real and imaginary pointers. The storage for complex numbers is
allocated in CONS space.

@tex
\startbyf
\bif{32}    {1}
\byf{31}{26}{001001}
\byf{25}{ 0}{Pointer (P)}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil\byfbox}
\vskip 0.2in

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Any number except Complex (Real Part)}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil$P\rightarrow$\byfbox}

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Any number except Complex (Imaginary Part)}
\endbyf
\line{\hfil$P+1\rightarrow$\byfbox}
@end tex

@subsection Short Floating Point

Short floating point numbers have a data-type of $$DTP-SHORT-FLOAT. The
pointer field contains the high order 26 bits of an IEEE single
precision floating point number.  This includes a sign bit, an 8 bit
excess 127 exponent, and a 17 bit mantissa.

@tex
\startbyf
\bif{32}    {1}
\byf{31}{26}{000101}
\bif{25}    {S}
\byf{24}{17}{Exponent}
\byf{16}{ 0}{Mantissa}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil\byfbox}
@end tex

@subsection Single Precision Floating Point

A single float value has a data-type of $$DTP-SINGLE-FLOAT.  The pointer
field points to a memory structure consisting a $$DTP-UNBOXED-HEADER
with a pointer field containing a length of one, and the data word
contains a single precision IEEE floating point number.

@tex
\startbyf
\bif{32}    {1}
\byf{31}{26}{000110}
\byf{25}{ 0}{Pointer (P)}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil\byfbox}

\startbyf
\byf{31}{26}{100000}
\byf{25}{ 0}{00 00000000 00000000 00000001}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil$P\rightarrow$\byfbox}

\startbyf
\bif{31}    {S}
\byf{30}{23}{Exponent}
\byf{22}{ 0}{Mantissa}
\endbyf
\line{\hfil$P+1\rightarrow$\byfbox}
\line{\hfil\byfnumbers}
@end tex

@subsection Double Precision Floating Point

A double float value has a data-type of $$DTP-DOUBLE-FLOAT.  The pointer
field points to a memory structure consisting a $$DTP-UNBOXED-HEADER
with a pointer field containing a length of two.  The first data word
contains the least significant part of an double precision IEEE floating
point number. The second word contains the most significant part of the
number.

@tex
\startbyf
\bif{32}    {1}
\byf{31}{26}{000111}
\byf{25}{ 0}{Pointer (P)}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil\byfbox}

\startbyf
\byf{31}{26}{100000}
\byf{25}{ 0}{00 00000000 00000000 00000010}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil$P\rightarrow$\byfbox}

\startbyf
\byf{31}{ 0}{Mantissa - Low}
\endbyf
\line{\hfil$P+1\rightarrow$\byfbox}

\startbyf
\bif{31}    {S}
\byf{30}{21}{Exponent}
\byf{20}{ 0}{Mantissa - High}
\endbyf
\line{\hfil$P+2\rightarrow$\byfbox}
\line{\hfil\byfnumbers}
@end tex

@section Unboxed Structures

Unboxed structures are used for storage of untyped non-array data in
memory. The only indication on the type of data in the structure is the
type of the pointer used to access it.  These structures have a header
word of type $$DTP-UNBOXED-HEADER with the pointer field containing a
count on the number of data words following the header. See bignums,
single-floats, and double-floats for examples.

@section Conses

A cons has a data type of $$DTP-CONS.  Its pointer field points to a
pair of words in memory, the car and cdr of the cons cell.  The storage
for conses is allocated in CONS space.

@tex
\startbyf
\bif{32}    {1}
\byf{31}{26}{000010}
\byf{25}{ 0}{Pointer (P)}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil\byfbox}

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Car}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil$P\rightarrow$\byfbox}

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Cdr}
\endbyf
\line{\hfil$P+1\rightarrow$\byfbox}
@end tex

@section Arrays

Arrays have two related storage formats, simple arrays and full arrays.
Simple arrays are one-dimensional arrays that are not adjustable, not
displaced, and do not have fill pointers or leaders.  They are faster to
access and require less overhead for storage than full arrays.  Full
arrays (also referred to as "hard" or "multiple" arrays) can have up to seven
dimensions, can be adjustable, can be displaced, and can have leaders
and fill pointers.

All arrays consist of an array pointer of type $$DTP-ARRAY which points
to the header of the array. The header either has type
$$DTP-ARRAY-HEADER-SINGLE, for simple arrays, or
$$DTP-ARRAY-HEADER-MULTIPLE, for hard arrays.  The data of the array
begins in the word following the header. If the array is hard, then
there are more words of header information preceding the header word in
memory.

@tex
\startbyf
\bif{32}    {1}
\byf{31}{26}{001110}
\byf{25}{ 0}{Pointer (P) to Array Header}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil\byfbox}
@end tex

@subsection Array Element Types

The array header information contains a five-bit field called the "Array
Type".  This number describes the types of elements the array can
contain.  In simple arrays, this number runs from bit 25 to bit 21 of
the array header.  In hard arrays, bits 25 to 21 of the array header are
11111, and the array type is found in bits 13 to 9 of the extension
header.

@settabs 5 @columns
@sp 1
@<          @\ @i[Bits per] @\ @i[Array] @\ @i[Range or Type] @cr
@< @i[Type] @\ @i[Element] @\ @i[Type] @\ @i[of Elements] @cr
@sp 1
@< ART-Q    @\ 32 @\ 00000 @\ Any Lisp object @cr
@< ART-1B   @\ 1  @\ 00001 @\ 0 to 1 @cr
@< ART-2BS  @\ 2  @\ 00010 @\ -2 to 1 @cr
@< ART-2B   @\ 2  @\ 00011 @\ 0 to 3 @cr
@sp 1
@< ART-4BS  @\ 4  @\ 00100 @\ -8 to 7 @cr
@< ART-4B   @\ 4  @\ 00101 @\ 0 to 15 @cr
@< ART-8BS  @\ 8  @\ 00110 @\ -128 to 127 @cr
@< ART-8B   @\ 8  @\ 00111 @\ 0 to 255 @cr
@sp 1
@< ART-16BS @\ 16 @\ 01000 @\ -32768 to 32767 @cr
@< ART-16B  @\ 16 @\ 01001 @\ 0 to 65536 @cr
@< ART-32BS @\ 32 @\ 01010 @\ -2147483648 to 2147483647 @cr
@< ART-32B  @\ 32 @\ 01011 @\ 0 to 4294967296 @cr
@sp 1
@< ART-STRING @\ 8 @\ 01100 @\ Characters @cr
@< ART-FAT-STRING @\ 16 @\ 01101 @\ Characters w/fonts @cr
@< ART-SINGLE-FLOAT @\ 32 @\ 01110 @\ Single precision floats @cr
@< ART-DOUBLE-FLOAT @\ 64 @\ 01111 @\ Double precision floats @cr
@sp 1
@< ART-CONTROL-PDL @\ ?? @\ 11100 @\ ?? @cr
@< ART-EXTRANEOUS-PDL @\ ?? @\ 11101 @\ ?? @cr
@< ART-SPECIAL-PDL @\ ?? @\ 11110 @\ ?? @cr
@< ART-HARD @\ ?? @\ 11111 @\ See array type in header @cr
@cleartabs

@subsubsection ART-Q

An ART-Q (array type 00000) array can store any lisp object in each
location. The elements of the array use one word of storage each.

The array type of ART-Q is 0 as an optimization for array
reference software since this is the most common array type.

@subsubsection Bit array types

Bit arrays store either a signed or unsigned integer in each element.
Types that are smaller than a word have multiple elements packed into
each word. The element with the lowest array index is stored in the
least significant bits of the word. When a value is read by AREF it is
converted to a fixnum (except for large 32 bit numbers which will be
bignums). The signed bit array types are stored as two's complement
numbers.

Bit arrays have array types from 00001 to 01001.

@subsubsection Strings

There are two array types used for strings, ART-STRING (array type
01100) and ART-FAT-STRING (array type 01101).

ART-STRING arrays are stored like ART-8B arrays. However data read out
is converted to a character instead of a fixnum. This array type is the
Common LISP string type.

ART-FAT-STRING arrays are stored like ART-16B arrays. Data read out is
also converted to characters. The only difference is that font and bit
information may be stored in the high 8 bits of the element.

@subsubsection Floating point

There are two types of floating point arrays, ART-SINGLE-FLOAT (array
type 01110) and ART-DOUBLE-FLOAT (array type 01111). They hold IEEE
standard format floating point values. The ART-SINGLE-FLOAT arrays
contain one number per word, where ART-DOUBLE-FLOAT arrays use two words
to contain a value.  The least significant part of the double precision
number is stored in the first word, and the most significant part in the
second word.

@subsection Simple Arrays

Simple arrays are one dimensional arrays ("vectors") of any of the
allowed array types. They are not adjustable, not displaced,
and do not have fill pointers or leaders. The header on a
simple vector uses only one word. It has a data-type of
$$DTP-ARRAY-HEADER-SINGLE (100010). The five bits below the
data-type contain the array type. The bottom 21 bits are the
number of elements in the array.

@tex
\startbyf
\bif{32}    {1}
\byf{31}{26}{001110}
\byf{25}{ 0}{Pointer (P) to Array-Header-Single}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil\byfbox}
\vskip 0.2in

\startbyf
\byf{31}{26}{100010}
\byf{25}{21}{ArType}
\byf{20}{ 0}{Number of elements (N)}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil$P\rightarrow$\byfbox}

\startbyf
\byf{31}{ 0}{First Word of Array Contents}
\endbyf
\line{\hfil$P+1\rightarrow$\byfbox}

\startbyf
\byf{31}{ 0}{Middle Words of Array Contents $\ldots$}
\endbyf
\line{\hfil$\ldots$\byfbox}

\startbyf
\byf{31}{ 0}{Last Word of Array Contents}
\endbyf
\line{\hfil$P+S\rightarrow$\byfbox}
@end tex

@subsection Hard Arrays

Hard arrays have a more complicated format than do simple arrays.  A
hard array has data type $$DTP-ARRAY (001110).  Its pointer field points
to a header with data type $$DTP-ARRAY-HEADER-MULTIPLE (100011).  The
array's data follows the header.  Additional array information appears
in memory locations @i[before] the header.  This additional information
is introduced with a header of data type $$DTP-ARRAY-HEADER-EXTENSION
(100100), and it extends from the array-header-extension header up to
the array-header-multiple header.  up

@tex
\startbyf
\bif{32}    {1}
\byf{31}{26}{001110}
\byf{25}{ 0}{Pointer (P) to Array-Header-Multiple}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil\byfbox}

\startbyf
\byf{31}{26}{100100}
\byf{25}{ 0}{Offset (K) to Main Header}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil$P-K\rightarrow$\byfbox}

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Last Leader Word}
\endbyf
\line{\hfil\byfbox}
\centerline{$\ldots$}

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{First Leader Word}
\endbyf
\line{\hfil\byfbox}

\startbyf
\byf{31}{26}{000001}
\byf{25}{ 0}{Number of Leader Words (FIXNUM)}
\endbyf
\line{\hfil\byfbox}

\startbyf
\byf{31}{26}{000001}
\byf{25}{ 0}{Displacement Offset (FIXNUM)}
\endbyf
\line{\hfil\byfbox}

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Displaced-To (ARRAY or LOCATIVE)}
\endbyf
\line{\hfil\byfbox}

\startbyf
\byf{31}{26}{000001}
\byf{25}{ 0}{Extent of ND'th Dimension (FIXNUM)}
\endbyf
\line{\hfil\byfbox}
\centerline{$\cdots$}

\startbyf
\byf{31}{26}{000001}
\byf{25}{ 0}{Extent of 2nd Dimension (FIXNUM)}
\endbyf
\line{\hfil\byfbox}

\startbyf
\byf{31}{26}{000001}
\byf{25}{ 0}{Additional Header Word (FIXNUM)}
\endbyf
\line{\hfil$P-1\rightarrow$\byfbox}

\startbyf
\byf{31}{26}{100011}
\byf{25}{21}{11111}
\byf{20}{0}{Zero'th Subscript Bound}
\endbyf
\line{\hfil$P\rightarrow$\byfbox}

\startbyf
\byf{31}{ 0}{First Word of Array Contents}
\endbyf
\line{\hfil$P+1\rightarrow$\byfbox}

\centerline{$\cdots$}

\startbyf
\byf{31}{ 0}{Last Word of Array Contents}
\endbyf
\line{\hfil$P+S\rightarrow$\byfbox}
@end tex

@subsubsection Extension Header Format

An array header extension has data type $$DTP-FIXNUM (000001).  It has
the following format:

@tex
\startbyf
\byf{31}{26}{100100}
\byf{25}{23}{S}
\bif{22}{A}
\bif{21}{F}
\bif{20}{N}
\bif{19}{D}
\bif{18}{S}
\bif{17}{L}
\byf{16}{14}{ndims}
\byf{13}{ 9}{ArType}
\bif{ 8}{S}
\byf{ 7}{ 4}{DO}
\byf{ 3}{ 0}{LO}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil\byfbox}
@end tex

The letter "S" indicates spare bits.

Bit 22 indicates if the array is adjustable.

Bit 21 indicates if the array has a fill pointer.

Bit 20 indicates if the array is a named structure.

Bit 19 indicates if the array is displaced.

Bit 17 indicates if the array has an array leader.

Bits 16 to 14 indicate the number of dimensions the array has.

Bits 13 to 9 indicate the array type.  This is the same as acode,
but 31 is an ART-ERROR (huh?)

Bits 7 to 4 indicate the displ offset.

Bits 3 to 0 indicate the leader offset.

     b) Fill Pointer

        The fill pointer is a fixnum that indicates the number of
        words currently used in an array.

     c) Displaced

        There are several types of displaced arrays. If the
        displaced by $$DTP-UNBOXED-LOCATIVE bit is set, then the
        displaced-to location contains an unboxed-locative that
        points to a memory address to displace the array to. When
        the displaced by $$DTP-ARRAY bit is set, then the displaced
        to word points to an array header into which the array is
        displaced.

        When either of the above bits are set, then the
        displaced-offset word exists and contains a fixnum that get
        added to the linearized array index.

        If the 2D displaced bit is set then the second subscript is
        multipled by the displaced-2d-offset word instead of the
        first bound when linearizing the index. This is used to map
        a rectangular region of an array used for pixel storage or
        similar rectangular mappings.

     d) Leaders

        If the has leader bit is set then the array has an array
        leader. The offset from the main header to the first word of
        the leader is in the leader-offset field.

     e) Order of hard array header elements

        Items in a hard array are stored in the following sequence.
        If the item doesn't exist (as indicated in the extension
        header) then it is left out of the sequence.

           Lowest Address -
              Leader word 0 (offset from header in extension header)
              Leader word 1
                ...
              Leader word N
              Fill Pointer
              Displaced-offset-2d
              Displaced-offset
              Displaced-to
              Dimension bound 7
                ...
              Dimension bound 1
              Extension header
              Array Header (with Dimension bound 0)
              Data ...

@section Compiled functions

A compiled function has a data type of $$DTP-COMPILED-FUNCTION (001100).
The pointer field points to a memory structure consisting of a
$$DTP-COMPILED-FUNCTION-HEADER (101110) with a pointer field containing
the number of instructions in the function.  Following the header are
six more words whose purposes are described below.  The code pointer CP
is computed from the PC by multiplying the PC by two and setting bit 25
(?).

@tex
\startbyf
\bif{32}    {1}
\byf{31}{26}{001100}
\byf{25}{ 0}{Pointer (P) to Compiled Function Header}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil\byfbox}

\startbyf
\byf{31}{26}{101110}
\byf{25}{ 0}{Number of Instructions in Function (N)}
\endbyf
\line{\hfil$P\rightarrow$\byfbox}

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Compiled Function Name}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil$P+1\rightarrow$\byfbox}

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Compiled Function Entry Points}
\endbyf
\line{\hfil$P+2\rightarrow$\byfbox}

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Compiled Function Local Refs}
\endbyf
\line{\hfil$P+3\rightarrow$\byfbox}

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Compiled Function Refs}
\endbyf
\line{\hfil$P+4\rightarrow$\byfbox}

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Compiled Function Length}
\endbyf
\line{\hfil$P+5\rightarrow$\byfbox}

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Compiled Function Code Pointer (PC)}
\endbyf
\line{\hfil$P+6\rightarrow$\byfbox}

\startbyf
\byf{31}{26}{001100}
\byf{25}{ 0}{Back Pointer to Compiled Function (P)}
\endbyf
\line{\hfil$CP-2\rightarrow$\byfbox}

\line{\hfil\byfnumbers}
\startbyf
\byf{31}{ 0}{{\#}x 7FFFFFFE}
\endbyf
\line{\hfil$CP-1\rightarrow$\byfbox}

\startbyf
\byf{31}{ 0}{First Instruction, Low Word}
\endbyf
\line{\hfil$CP\rightarrow$\byfbox}

\startbyf
\byf{31}{ 0}{First Instruction, High Word}
\endbyf
\line{\hfil$CP+1\rightarrow$\byfbox}

\startbyf
\byf{31}{ 0}{More Instructions$\ldots$}
\endbyf
\line{\hfil$\ldots$\byfbox}

\startbyf
\byf{31}{ 0}{Last Instruction, Low Word}
\endbyf
\line{\hfil$CP+2N-2\rightarrow$\byfbox}

\startbyf
\byf{31}{ 0}{Last Instruction, High Word}
\endbyf
\line{\hfil$CP+2N-1\rightarrow$\byfbox}
@end tex

@subsection Compiled function name

        This is the first word after the function header. It
        contains a symbol pointer to the name of the function.

@subsection Compiled function entry points

        This is the second word after the function header. It
        contains a array pointer to a simple vector containing the
        entry point information.

        All even locations in the vector contain the number of
        arguments required to use this entry point. The odd
        locations contain the offset to the PC for that entry point.
        There will be more than one entry point only if optional
        arguments are used by the function.

        When the function takes a rest arg, then then last even
        location will contain a negative number. If you subtract
        this number from -1 you will get the minimum number of
        arguments to use this entry point.

@subsection Compiled function local refs

        This is the third word after the function header. It
        contains an array pointer to a simple vector containing the
        local reference information. This is used to adjust the
        branch instructions inside the function when loading it.

        The local references are stored as pairs The first location
        of each pair contains the number of instructions to offset
        to the one to be adjusted.

        The second location of the pair contains the the number of
        instructions into the function that the branch is to.

@subsection Compiled function refs

        The fourth word after the function header contains an array
        pointer to a simple vector that contains the external
        reference information.

        Each entry in the vector is a triple. The first word of the
        triple contains the number of arguments that the function is
        calling with.

        The second word of the triple contains the instruction
        offset to the call instruction.

        The third word contains a symbol pointer to the function
        being referenced.

@subsection Compiled function length

        The fifth word after the function header contains the number
        of instructions in the function. This is redundant with the
        information in the function header word.

@subsection Compiled function code pointer

This is the sixth word after the function header.  It has data type
$$DTP-CODE and a PC in the pointer field.  It points to the first
instruction of the code before entry point offsetting.

@subsection Instruction back pointer

      The instruction just before the one that the code pointer
      points to contains a special illegal instruction. The top 32
      bits of this instruction are #x7fffffe and the low word
      contains a compiled function pointer back to the compiled
      function structure for this function.

      This is used to convert from a PC to a function pointer by
      scanning backwards in memory from the PC until the illegal
      instruction is found, and then following the pointer.

@subsection Changes to Format

The starting-address field has been eliminated; it and the code field
were redundant.

@section Symbols

    A symbol pointer has a type of $$DTP-SYMBOL and a pointer to
    a symbol structure in memory. The symbol structure contains
    five words, which are described below.

@tex
\startbyf
\bif{32}    {1}
\byf{31}{26}{000011}
\byf{25}{ 0}{Pointer (P)}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil\byfbox}

\startbyf
\byf{31}{26}{100001}
\byf{25}{ 0}{Pointer to Array Header of Print Name}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil$P\rightarrow$\byfbox}

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Value Cell}
\endbyf
\line{\hfil$P+1\rightarrow$\byfbox}

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Function Cell}
\endbyf
\line{\hfil$P+2\rightarrow$\byfbox}

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Package Cell}
\endbyf
\line{\hfil$P+3\rightarrow$\byfbox}

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Property List}
\endbyf
\line{\hfil$P+4\rightarrow$\byfbox}
@end tex


@subsection Symbol Header

      The first word of a symbol structure has a type of
      $$DTP-SYMBOL-HEADER. The pointer field points to an array
      header of an array containing the print name of the symbol.

@subsection Symbol Value

      The second word of a symbol structure contains the value of
      the symbol. This is any general LISP object or the special
      unbound object. The unbound object is a word with a
      data-type of $$DTP-UNBOUND and a pointer to the beginning of
      the symbol structure.

@subsection Symbol Function

      The third word of a symbol structure contains the function
      information for the symbol. This word may be one of several
      things:

@subsubsection Unbound

        The word will contain a unbound object (see symbol value) if
        there is no function associated with this symbol.

@subsubsection Compiled function

        There will be a compiled function pointer to a function
        header if the symbol refers to a compiled function.

@subsubsection Interpreted function

        For an interpreted function, the function cell will contain
        a list beginning with the symbol LISP:LAMBDA. The overall
        list is a general lambda expression.

@subsubsection Macro

        If the symbol is a macro, the function cell  contains a cons
        whose CAR is the symbol LISP:MACRO. The CDR of the cons
        contains either a compiled function pointer, or a lambda
        list if the macro is interpreted.

@subsection Symbol Package

      The fourth word of a symbol structure contains an array
      pointer to an array containing a string which is the name of
      the symbols package.
      (This will change when tha package system is installed!)

@subsection Symbol Property List

      The fifth word of the symbol strucure contains the property
      list of the symbol.

@section NIL

    NIL is a word with a data-type of $$DTP-NIL (0) and a
    pointer field of zero (ie 32 bits of zeros). This makes it
    easy to test for NIL. In memory at location zero is a
    degenerate symbol structure for NIL. All of the words of it
    are normal, except for the header. Both the header and value
    cell contain NIL (so CAR and CDR of NIL give NIL). The
    SYMBOL-NAME function knows this and handles the print name
    of NIL specially.

@tex
\startbyf
\bif{32}    {1}
\byf{31}{26}{000000}
\byf{25}{ 0}{00 00000000 00000000 00000000}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil\byfbox}
@end tex

@section T

    T is a normal symbol. The only thing odd about it is that it
    has an absolute location of five, and the SET function won't
    let you change its value.

@section Defstruct Structure Instances

A structure instance has a data type of $$DTP-STRUCTURE.  The pointer
field points to a memory structure consisting of a
$$DTP-STRUCTURE-HEADER whose pointer field contains the number of words
in the structure plus one for the name.  The first word after the header
is a symbol, the name of the structure.  The subsequent words are the
structure elements.

@tex
\startbyf
\bif{32}    {1}
\byf{31}{26}{010100}
\byf{25}{ 0}{Pointer (P)}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil\byfbox}

\startbyf
\byf{31}{26}{101111}
\byf{25}{ 0}{Number of structure elements + 1 ($N+1$)}
\endbyf
\line{\hfil\byfnumbers}
\line{\hfil$P\rightarrow$\byfbox}

\startbyf
\byf{31}{26}{000011}
\byf{25}{ 0}{Name of Structure (SYMBOL)}
\endbyf
\line{\hfil$P+1\rightarrow$\byfbox}

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{First Structure Element}
\endbyf
\line{\hfil$P+2\rightarrow$\byfbox}

\line{\hfil$\ldots$\hfil}

\startbyf
\byf{31}{26}{Data Type}
\byf{25}{ 0}{Last Structure Element}
\endbyf
\line{\hfil$P+(N+1)\rightarrow$\byfbox}
@end tex


The following is obsolete:

    Structure instances consist of a word with data-type
    $$DTP-INSTANCE and a pointer field to the instance structure
    in memory. The instance stucture has a header of type
    $$DTP-INSTANCE-HEADER and a pointer field containing the
    number of words in the stucture plus one (for the name).
    The first word after the header points to a symbol that is
    the name of the structure. All the rest of the words are the
    structure elements.

@section Undocumented So Far

locative, unboxed-locative, compiled-function, code, stack-group,
instance, lexical-closure, interpreter-closure, lexical-environment,
character, extend, encapsulation, hash-table, cons

many invisible types
