;;; -*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:CL -*-

;; User provides a piece of code which is in two parts, each of which
;; resides on one page (probably using up less than half a page)
;; This code will be written using defafuns which can jump between
;; each other, finally jumping to a constant location, the copy routine.

;; The copy routine will work from an image of the code in low memory
;; and copy it to two pages at offsets which are different and change
;; each time the routine is called.  All the other virtual pages should
;; map to a page of all halt instructions.  The unused instructions on
;; the copied pages are halt instuctions.

#|

Questions:

We need to make a type of defafun which is an alignment within a page.
Defafuns need to provide us with program length.

The user program must guarantee that it does not use A6-A15 ever. These
are reserved for the copy program so that we can both call and jump to
the user program.

We do not handle dispatch instructions.  To do this, the relocator will need to
be smarter.

Set Up.
Memory layout
Page 0
 Trap code
 Copy code
Page 2
 User1
Page 3
 User2
Page 4
 Halt instructions.
Remaining pages are possible candidates for copying to.

Begin Loop
select two new physical pages
select two new virtual pages
Map virtual pages to the physical pages
Fill the virtual pages with halt bit (using the function boundary marker)
select two new offsets in the pages

Copy each of the instructions
  Relocate the jump instructions
    scan function looking for conditional jump opcode, add offset to low 12 bits
    also, scan function looking for calls/long jumps to other function and fix them

Jump to the first Copied user function
...
Map the used virtual pages back to the halt page.
Loop.

|#

(eval-when (compile load eval)
  (global:defconst Virtual-address-original-user1 0)
  (global:defconst Virtual-address-original-user2 (1+ Virtual-address-original-user1))
  (global:defconst Copy-location-user1            2)
  (global:defconst Copy-location-user2            (1+ Copy-location-user1))
  (global:defconst Length-user1                   4) ;in 64 bit words
  (global:defconst Length-user2                   (1+ Length-user1))
  ;;these are 32 bit nubus byte addresses
  (global:defconst min-physical-address 6)
  (global:defconst max-physical-address 7)
  (global:defconst physical-address-1 8)
  (global:defconst physical-address-2 (+ physical-address-1 1))
  (global:defconst march-pass-counter 10.)
  (global:defconst halt-page-vadr 11.) ;base virtual address for halt page

  (global:defconst %%copy-offset (byte 12. -1))

  (global:defconst inst-block-size-in-bytes (* 16. 1024.))
  (global:defconst map-page-size-in-bytes 4096.)
)

;conventions for pace's code (all but relocation stuff)
;
; a0  flag for first or second user function
; a13 byte offset for function
; a14 byte address of base of page for function
; r0  extremely temporary
; r1  less temporary

(def-k-test march-driver ()
  (write-dt-ram write-dt-ram-dispatch load-dt-ram-pattern
   load-initial-datatype-ram
   vc-trap-on initialize-call-hardware )  ;vc-dt-and-ovf-trap-handler

  ;enable icache
  (movei r0 7)
  (alu or r0 r0 processor-control)
  (move processor-control r0)

  (movei gr:*trap-temp2* 0)

  ;; Global Registers set up by spy initialization.
  ;; Traps turned off by spy initialization.
  ;;
;  (open-call initialize-call-hardware ignore ())
;  (open-call load-initial-datatype-ram ignore ())

;  (movei r0 #.(logior (ash 1 (byte-position hw:%%memory-control-reset-trap-bit)) ;really active low, so this is "off"
;		      (ash 1 (byte-position hw:%%memory-control-synchronous-trap-enable))
;		      (ash 1 (byte-position hw:%%memory-control-datatype-trap-enable))
;		      (ash 1 (byte-position hw:%%memory-control-overflow-trap-enable))
;		      (ash 1 (byte-position hw:%%memory-control-asynchronous-trap-enable))
;		      (ash 1 (byte-position hw:%%memory-control-master-trap-enable))))
;  (alu or memory-control r0 memory-control)(nop)(nop)(nop)
;  (movei r0 #.(logior (ash 1 (byte-position hw:%%processor-control-heap-underflow-trap-enable))
;		      (ash 1 (byte-position hw:%%processor-control-floating-point-trap-enable))))
;  (alu or processor-control r0 processor-control)(nop)(nop)(nop)

  (movei vma-start-read-no-transport #.k-test-word-arg0-loc)
  (memory-wait)
  (move a15 md)

  ;;a0 = 0 means first user function; a0 = 1 means second
move-loop
  (movei a0 0)

select-loop

  ;;select new physical location

  ;;read current phys adr for function
  (movei r0 #.physical-address-1)
  (alu add r0 a0 r0)				;first or second pass
  (alu l+r vma-start-read-no-transport a15 r0)
  (nop)
  (movei a14 #.(+ inst-block-size-in-bytes 8))	;compute new phys address based on old one
;***
  ;;add 8 more if this is the second pass
  (alu add r0 a0 a0) ;2 or 0
  (alu add r0 r0 r0) ;4 or 0
  (alu add r0 r0 r0) ;8 or 0
  (alu add a14 a14 r0)
;***
  (alu l+r a14 a14 md)
  (move a13 a14)
  ;;split offset from rest of adr
  (movei r0 #.(- inst-block-size-in-bytes 1))
  (alu and a13 a13 r0)				;tenative offset
  (alu not-r r0 r0 r0)
  (alu and a14 a14 r0)				;tenative base

  ;;see if this is the last page by adding
  ;;the maximum size, and comparing with the max address
  (movei r1 #.inst-block-size-in-bytes)
  (alu add r1 r1 a14)

  (movei r0 #.max-physical-address)
  (alu l+r vma-start-read-no-transport a15 r0)
  (nop)

  ;;it's ok for this to be a signed test, since either both addresses will be
  ;;on the nubus (negative) or in local memory (positive)
  (alu l-r nop r1 md)
  (test br-less-than)
  (branch page-ok ())
  
  (movei r0 #.min-physical-address)
  (alu l+r vma-start-read-no-transport a15 r0)
  (nop)
  (move a14 md)

page-ok
  (move nop a0)
  (test br-equal)
  (branch page-ok-1 ())
  
  ;;this is the second pass, must not select same page as first pass
  ;;write out tenative selection
  (alu add md a14 a13)				;combine base and offset
  (movei r0 #.physical-address-2)
  (alu add vma-start-write-no-gc-trap a15 r0)
  (nop)

  (movei r0 #.physical-address-1)
  (alu l+r vma-start-read-no-transport a15 r0)
  (nop)
  ;;mask off low page offset bits
  (movei r0 #.(- inst-block-size-in-bytes 1))
  (alu not-r r0 r0 r0)
  (alu and r0 r0 md)

  (alu l-r nop a14 r0)
  (test br-equal)
  (branch select-loop ())			;if collision, just try again

page-ok-1  

  ;;see if the offset is too large, and would cause the function to cross a block boundary
  ;;do this by adding the length of the function in bytes to the proposed offset (in a13)
  ;;if the result is less than inst-block-size-in-bytes (16k) then the offset is ok.
  ;;otherwise, set the offset to 0
  (movei r0 #.length-user1)
  (alu add r0 a0 r0)				;first or second pass
  (alu l+r vma-start-read-no-transport a15 r0)
  (nop)
  (move r0 md)
  (alu add r0 r0 r0)				;double to get 32 bit words
  (alu add r0 r0 r0)				;double to get 16 bit words
  (alu add r0 r0 r0)				;double to get 8 bit bytes
  ;;r0 is function length in bytes
  (alu add r0 r0 a13)				;add in the tenative offset
  (movei r1 #.inst-block-size-in-bytes)
  (alu l-r nop r0 r1)
  (test br-less-than)
  (branch off-ok ())
  (movei a13 0)
off-ok
  ;;now a14 is address of base of physical page and a13 is offset

  ;;store updated phys adr
  (alu add md a14 a13)				;combine base and offset
  (movei r0 #.physical-address-1)
  (alu add r0 a0 r0)
  (alu add vma-start-write-no-gc-trap a15 r0)
  (nop)

select-virt
  ;;select virtual page
  (movei r0 #.Copy-location-user1)
  (alu add r0 a0 r0)				;first or second pass
  (alu l+r vma-start-read-no-transport a15 r0)
  (nop)

  (movei r0 #.(- (lisp:/ inst-block-size-in-bytes 4) 1))
  (alu not-r r0 r0 r0)
  (alu and a12 r0 md)

  (movei r0 #.(lisp:/ inst-block-size-in-bytes 4))
  (alu add a12 a12 r0)

  (movei r1 #.(lisp:/ inst-block-size-in-bytes 4))
  (alu add r1 r1 a12)

  (movei r0 #.(ash 1 26.))
  (alu l-r nop r1 r0)
  (test br-less-than)
  (branch virt-ok ())
  (movei a12 #.(+ (ash 1 25.) (lisp:/ (* 16. inst-block-size-in-bytes) 4))) ;*** could be in words, but this is safe anyway
virt-ok
  (move nop a0)
  (test br-equal)
  (branch virt-ok-1 ())
  
  ;;this is the second pass, must not select same page as first pass
  ;;write out tenative selection
  (move md a12)
  (movei r0 #.Copy-location-user2)
  (alu l+r vma-start-write-no-gc-trap a15 r0)

  (movei r0 #.Copy-location-user1)
  (alu l+r vma-start-read-no-transport a15 r0)
  (nop)

  ;;mask off low page offset bits
  (movei r0 #.(- inst-block-size-in-bytes 1))
  (alu not-r r0 r0 r0)
  (alu and r1 r0 md)
  (alu and r2 r0 a12)

  (alu l-r nop r1 r2)
  (test br-equal)
  (branch select-virt ())			;if collision, just try again

virt-ok-1
  ;;a13 is byte offset we want this time.  put it into the target virtual address
;  (setq a12 (hw:dpb (hw:ldb a13 (byte 10. 2) 0) (byte 10. 0) a12)))
  (ALU-FIELD FIELD-PASS A12 a13 a12 (BYTE 10 -2))
  ;;a12 is base of virtual page to use

  ;;write out target virtual address
  (move md a12)
  (movei r0 #.Copy-location-user1)
  (alu add r0 a0 r0)
  (alu l+r vma-start-write-no-gc-trap a15 r0)

  ;;fill in 4 map entries
  ;;read map location zero to get permission bits
;  (movei vma 0)
;  (nop)
;  (nop)
;  (nop)
;  (movei r0 #xff)
;  (alu and r0 r0 memory-map)
  (movei r0 #x8f)

  (alu add a10 a14 r0)				;phys base address + permission bits

  (movei r0 #.map-page-size-in-bytes)
  (movei r1 #.(lisp:/ map-page-size-in-bytes 4))

  (move vma a12)				;base virtual page
  (nop) (nop) (nop)
  (move memory-map a10)
  (nop) (nop) (nop)

  (alu add vma r1 vma)				;skip by a map page worth of words
  (alu add a10 a10 r0)				;skip by a map page worth of bytes
  (nop) (nop) (nop)
  (move memory-map a10)
  (nop) (nop) (nop)
 
  (alu add vma r1 vma)				;skip by a map page worth of words
  (alu add a10 a10 r0)				;skip by a map page worth of bytes
  (nop) (nop) (nop)
  (move memory-map a10)
  (nop) (nop) (nop)
 
  (alu add vma r1 vma)				;skip by a map page worth of words
  (alu add a10 a10 r0)				;skip by a map page worth of bytes
  (nop) (nop) (nop)
  (move memory-map a10)
  (nop) (nop) (nop)

  ;;store halts

  (move r1 a12)					;base virtual page
  (movei r0 #.(lisp:/ inst-block-size-in-bytes 4))
  (alu add r0 r0 r1)				;end marker
  
  (movei md #xffffffff)				;will act as halt instruction

store-halt-loop
  (move vma-start-write-no-gc-trap r1)
  (alu r+1 r1 ignore r1)
  (alu l-r nop r0 r1)
  (test br-not-equal)
  (branch store-halt-loop ())


  ;;increment a0 and go back for second function
  (move nop a0)
  (test br-not-equal)
  (branch do-relocate ())
  (alu l+1 a0 a0 a0)

  (movei r0 2)
  (alu l-r nop a0 r0)
  (test br-not-equal)
  (branch select-loop ())

  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (alu setr processor-control o10 r0)(nop)(nop)(nop)(nop)

  
  (unconditional-branch select-loop ())

do-relocate

;;;**************************************************
;;;Copy each of the instructions
;;;  Relocate the jump instructions
;;;    scan function looking for conditional jump opcode, add offset to low 12 bits
;;;    also, scan function looking for calls/long jumps to other function and fix them

  (movei a14 0)
  (movei a1 #b010) 						;;Opcode for Jump or call instruction.
  (movei a13 #.Virtual-address-original-user1)
User1/2-loop
  (alu   l+r a13 a13 a14)        				;;Increment copy-from index for user1 vs user2
  (alu   l+r vma-start-read-no-transport a13 a15 unboxed-vma unboxed-md)
  (movei a12 #.Copy-location-user1)
  (move  a13 md)  						;;Address of copy-from location
  (alu-field field-extract-r a3 ignore a13 (byte 24. -1)) 	;;Turn copy-from Address into PC type
  (alu   l+r a12 a12 a14)        				;;Increment copy-to index for user1 vs user2
  (alu   l+r vma-start-read-no-transport a12 a15 unboxed-vma unboxed-md)
  (movei a11 #.Length-user1)
  (move  a12 md)  						;;Addres of copy-to location
  (alu-field field-extract-r a2 ignore a12 (byte 24. -1)) 	;;Turn copy-to address into PC type
  (alu-field field-extract-r a5 ignore a12 %%copy-offset) 	;;Offset within the page we are copying to.
  (alu   l+r a11 a11 a14)        				;;Increment lenth index for user1 vs user2
  (alu   l+r vma-start-read-no-transport a11 a15 unboxed-vma unboxed-md)
  (movei a10 0)  					;;wait for MD; Zero length counter.
  (move  a4  md)  						;;Length of code to copy
Loop-for-each-instruction 					;;incrementing A10 before repeating
  (alu  l+1 vma-start-read-no-transport a13 ignore unboxed-vma unboxed-md)
  (alu  l+1 a10 a10 ignore)	  			;;Wait for MD; Increment inst loop counter -- a10
  (move a7 md) 							;;high inst word
  (move vma-start-read-no-transport a13 unboxed-vma unboxed-md) ;;start read of low inst word
  (alu-field field-extract-r a6 ignore a7 (byte 3. -26.)) 	;;Wait for MD; Opcode of high inst -- hw:%%i-op-code-high
  (move a8 md) 							;;low inst word
  (move vma-start-read-will-write a12 unboxed-vma unboxed-md)	;;Set up vma for copy of low inst below
;;; Is it a conditional jump?
  (alu-field aligned-field-xor nop gr:*zero* a6 (byte 2 0)) 	;;low two bits of opcode
  (alu-field field-extract-r nop ignore a7 (byte 2 -24.) br-not-equal) ;; next pc
  (branch maybe-call-or-jump (alu l+2 a13 a13 ignore br-not-equal)) ;;Increment from-address
  (branch copy-it (move md-start-write-no-gc-trap a8))		;;Copy low word inst
;;;We have a Branch inst 					;;or a CALL-Z instruction
  (unconditional-branch copy-it (alu l+r md-start-write-no-gc-trap a5 a8));;Assume this does not affect bits above 12th 
Maybe-call-or-jump
  (alu xor nop a1 a6)
  (alu l-r a0 a8 a3 br-not-equal)				;;Subtract base source address from jump address
  (branch copy-it (move md-start-write-no-gc-trap a8))		;;Copy low word inst
;;;We have a jump or call inst
  (alu l+r md-start-write-no-gc-trap a2 a0)		;;Add back base destination address to jump address
Copy-it								;;Low word is already copied.
  (alu l+1 vma-start-read-will-write a12 ignore unboxed-vma unboxed-md)
  (alu l-r nop a10 a4)
  (move md-start-write-no-gc-trap a7 br-not-equal)		;;copy high word inst
  (branch Loop-for-each-instruction (alu l+2 a12 a12 ignore)) ;;Increment To-address
;;;Check for loop again for next piece of code.
  (alu l-1 nop a14 ignore boxed)
  (movei a13 #.Virtual-address-original-user1 br-not-equal)
  (branch User1/2-loop (alu l+1 a14 a14 ignore boxed))

;;;**************************************************

  ;;flush icache
  (move a0 processor-control boxed-right)
  (movei r0 0)
  (alu-field field-pass processor-control r0 a0 hw:%%processor-control-icache-enables boxed-right)
  (alu pass-status nop ignore ignore)
  (move processor-control a0 boxed-right)

  (movei r0 #.Copy-location-user1)
  (alu add vma-start-read-no-transport a15 r0)
  (alu pass-status nop ignore ignore)
  
  ;;call user program
  ;;convert virtual address to PC (already has 25'th bit set)
  (alu-field field-extract-r nop ignore md (byte 24. -1))
  (alu pass-status nop ignore ignore)
  (alu pass-status nop ignore ignore ch-open-call next-pc-dispatch)

  ;;set up to call user2 program
  (movei r0 #.Copy-location-user2)
  (alu add vma-start-read-no-transport a15 r0)
  (alu pass-status nop ignore ignore)
  
  ;;call user2 program
  ;;convert virtual address to PC (already has 25'th bit set)
  (alu-field field-extract-r nop ignore md (byte 24. -1))
  (alu pass-status nop ignore ignore)
  (alu pass-status nop ignore ignore ch-open-call next-pc-dispatch)

  ;;increment pass counter
  (movei r0 #.march-pass-counter)
  (alu add vma-start-read-no-transport a15 r0)
  (alu pass-status nop ignore ignore)
  
  (alu r+1 md ignore md)
  (move vma-start-write-no-gc-trap vma)
  (alu pass-status nop ignore ignore)

;  (move r0 processor-control)
;  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
;  (alu setr processor-control o10 r0)(nop)(nop)(nop)(nop)

  ;;unmap user pages
  ;;get map entry for halt page
;  (movei vma #.halt-page-vadr)
;  (alu pass-status nop ignore ignore)
;  (alu pass-status nop ignore ignore)
;  (alu pass-status nop ignore ignore)
;  (move a1 memory-map)
  (movei a1 #x3c08f)

  (movei a0 0)
  (movei a2 #.(lisp:/ map-page-size-in-bytes 4))
finish-loop
  (movei r0 #.Copy-location-user1)
  (alu add r0 a0 r0)
  (alu add vma-start-read-no-transport a15 r0)
  (alu pass-status nop ignore ignore)

  (move vma md)
  (alu pass-status nop ignore ignore) (alu pass-status nop ignore ignore) (alu pass-status nop ignore ignore)
  (move memory-map a1)

  (alu add vma a2 vma)
  (alu pass-status nop ignore ignore) (alu pass-status nop ignore ignore) (alu pass-status nop ignore ignore)
  (move memory-map a1)

  (alu add vma a2 vma)
  (alu pass-status nop ignore ignore) (alu pass-status nop ignore ignore) (alu pass-status nop ignore ignore)
  (move memory-map a1)

  (alu add vma a2 vma)
  (alu pass-status nop ignore ignore) (alu pass-status nop ignore ignore) (alu pass-status nop ignore ignore)
  (move memory-map a1)

  (move nop a0)
  (test br-not-equal)
  (branch move-loop ())

  (alu l+1 a0 a0 a0)
  (unconditional-branch finish-loop ())
;  (unconditional-branch move-loop ())
)  

;  (move r0 processor-control)
;  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
;  (alu setr processor-control o10 r0)(nop)(nop)(nop)(nop)

