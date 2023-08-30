; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel) - v2.0.6 - memory.s
; ----------------------------------------------------------------------------
; Last Update: 29/08/2023  (Previous: 17/04/2021)
; ----------------------------------------------------------------------------
; Beginning: 24/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
;
; Derived from 'Retro UNIX 386 Kernel - v0.2.1.0' source code by Erdogan Tan
; memory.inc (18/10/2015)
; ****************************************************************************

; MEMORY.ASM - Retro UNIX 386 v1 MEMORY MANAGEMENT FUNCTIONS (PROCEDURES)
; Retro UNIX 386 v1 Kernel (unix386.s, v0.2.0.14) - MEMORY.INC
; Last Modification: 18/10/2015

; ///////// MEMORY MANAGEMENT FUNCTIONS (PROCEDURES) ///////////////

;;04/11/2014 (unix386.s)	
;PDE_A_PRESENT	equ	1		; Present flag for PDE
;PDE_A_WRITE	equ 	2		; Writable (write permission) flag
;PDE_A_USER	equ	4		; User (non-system/kernel) page flag
;;
;PTE_A_PRESENT	equ	1		; Present flag for PTE (bit 0)
;PTE_A_WRITE	equ 	2		; Writable (write permission) flag (bit 1)
;PTE_A_USER	equ	4		; User (non-system/kernel) page flag (bit 2)
;PTE_A_ACCESS   equ	32		; Accessed flag (bit 5) ; 09/03/2015

; 27/04/2015
; 09/03/2015
PAGE_SIZE 	equ 4096		; page size in bytes
PAGE_SHIFT 	equ 12			; page table shift count
PAGE_D_SHIFT 	equ 22	; 12 + 10	; page directory shift count
PAGE_OFF	equ 0FFFh		; 12 bit byte offset in page frame
PTE_MASK 	equ 03FFh		; page table entry mask
PTE_DUPLICATED  equ 200h		; duplicated page sign (AVL bit 0)
PDE_A_CLEAR	equ 0F000h		; to clear PDE attribute bits
PTE_A_CLEAR	equ 0F000h		; to clear PTE attribute bits
LOGIC_SECT_SIZE equ 512			; logical sector size
ERR_MAJOR_PF	equ 0E0h		; major error: page fault
ERR_MINOR_IM	equ 4 ;15/10/2016 (1->4); insufficient (out of) memory
ERR_MINOR_PV	equ 6 ;15/10/2016 (1->4); protection violation
SWP_DISK_READ_ERR 	   equ 40
SWP_DISK_NOT_PRESENT_ERR   equ 41
SWP_SECTOR_NOT_PRESENT_ERR equ 42
SWP_NO_FREE_SPACE_ERR      equ 43
SWP_DISK_WRITE_ERR         equ 44
SWP_NO_PAGE_TO_SWAP_ERR    equ 45
PTE_A_ACCESS_BIT equ 5  ; Bit 5 (accessed flag)        
SECTOR_SHIFT     equ 3  ; sector shift (to convert page block number)
; 12/07/2016
PTE_SHARED	 equ 400h		; AVL bit 1, direct memory access bit	
					; (Indicates that the page is not allocated
					; for the process, it is a shared or system
                                        ; page, it must not be deallocated!)
; 14/12/2020
; (Linear Frame Buffer - video memory mark : AVL bit 1, outside M.A.T.)
PDE_EXTERNAL	equ 400h	; Page directory entry for external memory blocks
PTE_EXTERNAL	equ 400h	; Allocated kernel pages for Linear Frame Buffer
				; (Out of memory allocation table)	

;
;; Retro Unix 386 v1 - paging method/principles
;;
;; 10/10/2014
;; RETRO UNIX 386 v1 - PAGING METHOD/PRINCIPLES
;;
;; KERNEL PAGE MAP: 1 to 1 physical memory page map
;;	(virtual address = physical address)
;; KERNEL PAGE TABLES:
;;	Kernel page directory and all page tables are
;;	on memory as initialized, as equal to physical memory
;;	layout. Kernel pages can/must not be swapped out/in.
;;
;;	what for: User pages may be swapped out, when accessing
;;	a page in kernel/system mode, if it would be swapped out,
;;	kernel would have to swap it in! But it is also may be
;;	in use by a user process. (In system/kernel mode
;;	kernel can access all memory pages even if they are
;;	reserved/allocated for user processes. Swap out/in would
;;	cause conflicts.) 
;;	
;;	As result of these conditions,
;;	all kernel pages must be initialized as equal to 
;;	physical layout for preventing page faults. 
;;	Also, calling "allocate page" procedure after
;;	a page fault can cause another page fault (double fault)
;;	if all kernel page tables would not be initialized.
;;
;;	[first_page] = Beginning of users space, as offset to 
;;	memory allocation table. (double word aligned)
;;
;;	[next_page] = first/next free space to be searched
;;	as offset to memory allocation table. (dw aligned)
;;
;;	[last_page] = End of memory (users space), as offset
;;	to memory allocation table. (double word aligned)
;;
;; USER PAGE TABLES:
;;	Demand paging (& 'copy on write' allocation method) ...
;;		'ready only' marked copies of the 
;;		parent process's page table entries (for
;;		same physical memory).
;;		(A page will be copied to a new page after
;;		 if it causes R/W page fault.)
;;
;;	Every user process has own (different)
;;	page directory and page tables.	
;;
;;	Code starts at virtual address 0, always.
;;	(Initial value of EIP is 0 in user mode.)
;;	(Programs can be written/developed as simple
;;	 flat memory programs.)
;;
;; MEMORY ALLOCATION STRATEGY:
;;	Memory page will be allocated by kernel only 
;;		(in kernel/system mode only).
;;	* After a
;;	  - 'not present' page fault
;;	  - 'writing attempt on read only page' page fault 	 	
;;	* For loading (opening, reading) a file or disk/drive
;;	* As responce to 'allocate additional memory blocks' 
;;	  request by running process.
;;	* While creating a process, allocating a new buffer,
;;	  new page tables etc.
;;
;;	At first,
;;	- 'allocate page' procedure will be called;
;,	   if it will return with a valid (>0) physical address
;;	   (that means the relevant M.A.T. bit has been RESET)	
;;	   relevant memory page/block will be cleared (zeroed).
;;	- 'allocate page' will be called for allocating page
;;	   directory, page table and running space (data/code).
;;	- every successful 'allocate page' call will decrease
;;	  'free_pages' count (pointer).
;;	- 'out of (insufficient) memory error' will be returned
;;	  if 'free_pages' points to a ZERO.
;;	- swapping out and swapping in (if it is not a new page)
;;	  procedures will be called as responce to 'out of memory'
;;	  error except errors caused by attribute conflicts.
;;	 (swapper functions)	 
;;					
;;	At second,
;;	- page directory entry will be updated then page table
;;	  entry will be updated.		
;;
;; MEMORY ALLOCATION TABLE FORMAT:
;;	- M.A.T. has a size according to available memory as
;;	  follows:
;;		  - 1 (allocation) bit per 1 page (4096 bytes)
;;		  - a bit with value of 0 means allocated page
;;		  - a bit with value of 1 means a free page
;,	- 'free_pages' pointer holds count of free pages
;;	  depending on M.A.T.
;;		(NOTE: Free page count will not be checked
;;		again -on M.A.T.- after initialization. 
;;		Kernel will trust on initial count.)
;,	- 'free_pages' count will be decreased by allocation
;;	  and it will be increased by deallocation procedures.
;;	
;;	- Available memory will be calculated during
;;	  the kernel's initialization stage (in real mode).
;;	  Memory allocation table and kernel page tables 
;;	  will be formatted/sized as result of available
;;	  memory calculation before paging is enabled.
;;
;; For 4GB Available/Present Memory: (max. possible memory size)
;;	- Memory Allocation Table size will be 128 KB.
;;	- Memory allocation for kernel page directory size 
;;	  is always 4 KB. (in addition to total allocation size
;;	  for page tables)
;;	- Memory allocation for kernel page tables (1024 tables)
;;	  is 4 MB (1024*4*1024 bytes).
;;	- User (available) space will be started 
;;	  at 6th MB of the memory (after 1MB+4MB).
;;	- The first 640 KB is for kernel's itself plus
;;	  memory allocation table and kernel's page directory
;;	  (D0000h-EFFFFh may be used as kernel space...)	
;;	- B0000h to B7FFFh address space (32 KB) will be used
;; 	  for buffers.
;;	- ROMBIOS, VIDEO BUFFER and VIDEO ROM space are reserved.
;,	  (A0000h-AFFFFh, C0000h-CFFFFh, F0000h-FFFFFh)
;;	- Kernel page tables start at 100000h (2nd MB)
;;
;; For 1GB Available Memory:
;;	- Memory Allocation Table size will be 32 KB.
;;	- Memory allocation for kernel page directory size 
;;	  is always 4 KB. (in addition to total allocation size
;;	  for page tables)
;;	- Memory allocation for kernel page tables (256 tables)
;;	  is 1 MB (256*4*1024 bytes).
;;	- User (available) space will be started 
;;	  at 3th MB of the memory (after 1MB+1MB).
;;	- The first 640 KB is for kernel's itself plus
;;	  memory allocation table and kernel's page directory
;;	  (D0000h-EFFFFh may be used as kernel space...)	
;;	- B0000h to B7FFFh address space (32 KB) will be used
;; 	  for buffers.
;;	- ROMBIOS, VIDEO BUFFER and VIDEO ROM space are reserved.
;,	  (A0000h-AFFFFh, C0000h-CFFFFh, F0000h-FFFFFh)
;;	- Kernel page tables start at 100000h (2nd MB).	
;;
;;


;;************************************************************************************
;; 
;; RETRO UNIX 386 v1 - Paging (Method for Copy On Write paging principle)
;; DEMAND PAGING - PARENT&CHILD PAGE TABLE DUPLICATION PRINCIPLES (23/04/2015)

;; Main factor: "sys fork" system call 
;;	
;; 		FORK
;;                      |----> parent - duplicated PTEs, read only pages
;;  writable pages ---->|
;;                      |----> child - duplicated PTEs, read only pages
;; 
;; AVL bit (0) of Page Table Entry is used as duplication sign 
;; 
;; AVL Bit 0 [PTE Bit 9] = 'Duplicated PTE belongs to child' sign/flag (if it is set)
;; Note: Dirty bit (PTE bit 6) may be used instead of AVL bit 0 (PTE bit 9)
;;       -while R/W bit is 0-. 
;; 
;; Duplicate page tables with writable pages (the 1st sys fork in the process):
;; # Parent's Page Table Entries are updated to point same pages as read only, 
;;   as duplicated PTE bit  -AVL bit 0, PTE bit 9- are reset/clear.
;; # Then Parent's Page Table is copied to Child's Page Table.
;; # Child's Page Table Entries are updated as duplicated child bit
;;   -AVL bit 0, PTE bit 9- is set.	  
;; 
;; Duplicate page tables with read only pages (several sys fork system calls):
;; # Parent's read only pages are copied to new child pages. 
;;   Parent's PTE attributes are not changed.
;;   (Because, there is another parent-child fork before this fork! We must not
;;    destroy/mix previous fork result).
;; # Child's Page Table Entries (which are corresponding to Parent's 
;;   read only pages) are set as writable (while duplicated PTE bit is clear). 
;; # Parent's PTEs with writable page attribute are updated to point same pages 
;;   as read only, (while) duplicated PTE bit is reset (clear).
;; # Parent's Page Table Entries (with writable page attribute) are duplicated 
;;   as Child's Page Table Entries without copying actual page.
;; # Child 's Page Table Entries (which are corresponding to Parent's writable 
;;   pages) are updated as duplicated PTE bit (AVL bit 0, PTE bit 9- is set.
;; 
;; !? WHAT FOR (duplication after duplication):
;; In UNIX method for sys fork (a typical 'fork' application in /etc/init)
;; program/executable code continues from specified location as child process, 
;; returns back previous code location as parent process, every child after 
;; every sys fork uses last image of code and data just prior the fork.
;; Even if the parent code changes data, the child will not see the changed data 
;; after the fork. In Retro UNIX 8086 v1, parent's process segment (32KB)
;; was copied to child's process segment (all of code and data) according to
;; original UNIX v1 which copies all of parent process code and data -core- 
;; to child space -core- but swaps that core image -of child- on to disk.
;; If I (Erdogan Tan) would use a method of to copy parent's core
;; (complete running image of parent process) to the child process; 
;; for big sizes, i would force Retro UNIX 386 v1 to spend many memory pages 
;; and times only for a sys fork. (It would excessive reservation for sys fork,
;; because sys fork usually is prior to sys exec; sys exec always establishes
;; a new/fresh core -running space-, by clearing all code/data content). 
;; 'Read Only' page flag ensures page fault handler is needed only for a few write
;; attempts between sys fork and sys exec, not more... (I say so by thinking 
;; of "/etc/init" content, specially.) sys exec will clear page tables and
;; new/fresh pages will be used to load and run new executable/program.
;; That is what for i have preferred "copy on write", "duplication" method
;; for sharing same read only pages between parent and child processes.
;; That is a pitty i have to use new private flag (AVL bit 0, "duplicated PTE 
;; belongs to child" sign) for cooperation on duplicated pages between a parent 
;; and it's child processes; otherwise parent process would destroy data belongs
;; to its child or vice versa; or some pages would remain unclaimed 
;; -deallocation problem-.
;; Note: to prevent conflicts, read only pages must not be swapped out... 
;; 
;; WHEN PARENT TRIES TO WRITE IT'S READ ONLY (DUPLICATED) PAGE:
;; # Page fault handler will do those:
;;   - 'Duplicated PTE' flag (PTE bit 9) is checked (on the failed PTE).
;;   - If it is reset/clear, there is a child uses same page.
;;   - Parent's read only page -previous page- is copied to a new writable page. 
;;   - Parent's PTE is updated as writable page, as unique page (AVL=0)
;;   - (Page fault handler whill check this PTE later, if child process causes to
;;     page fault due to write attempt on read only page. Of course, the previous 
;;     read only page will be converted to writable and unique page which belongs
;;     to child process.)	
;; WHEN CHILD TRIES TO WRITE IT'S READ ONLY (DUPLICATED) PAGE:
;; # Page fault handler will do those:
;;   - 'Duplicated PTE' flag (PTE bit 9) is checked (on the failed PTE).
;;   - If it is set, there is a parent uses -or was using- same page.
;;   - Same PTE address within parent's page table is checked if it has same page
;;     address or not. 
;;   - If parent's PTE has same address, child will continue with a new writable page.
;;     Parent's PTE will point to same (previous) page as writable, unique (AVL=0).	
;;   - If parent's PTE has different address, child will continue with it's 
;;     own/same page but read only flag (0) will be changed to writable flag (1) and
;;     'duplicated PTE (belongs to child)' flag/sign will be cleared/reset. 	  	
;; 
;; NOTE: When a child process is terminated, read only flags of parent's page tables
;;       will be set as writable (and unique) in case of child process was using 
;;       same pages with duplicated child PTE sign... Depending on sys fork and 
;;       duplication method details, it is not possible multiple child processes
;;       were using same page with duplicated PTEs.
;; 
;;************************************************************************************   

;; 08/10/2014
;; 11/09/2014 - Retro UNIX 386 v1 PAGING (further) draft
;;		by Erdogan Tan (Based on KolibriOS 'memory.inc')

;; 'allocate_page' code is derived and modified from KolibriOS
;; 'alloc_page' procedure in 'memory.inc' 
;; (25/08/2014, Revision: 5057) file 
;; by KolibriOS Team (2004-2012)

allocate_page:
	; 17/04/2021 - TRDOS 386 v2.0.4
	;	 (temporary modifications)
	; 01/07/2015
	; 05/05/2015
	; 30/04/2015
	; 16/10/2014
	; 08/10/2014
	; 09/09/2014 (Retro UNIX 386 v1 - beginning)
	;
	; INPUT -> none
	;
	; OUTPUT ->
	;	EAX = PHYSICAL (real/flat) ADDRESS OF THE ALLOCATED PAGE
	;	(corresponding MEMORY ALLOCATION TABLE bit is RESET)
	;
	;	CF = 1 and EAX = 0 
	; 		   if there is not a free page to be allocated	
	;
	; Modified Registers -> none (except EAX)
	;
	mov	eax, [free_pages]
	and	eax, eax
	jz	short out_of_memory
	;
	push	ebx
	push	ecx
	;
	mov	ebx, MEM_ALLOC_TBL   ; Memory Allocation Table offset
	mov	ecx, ebx
 				     ; NOTE: 32 (first_page) is initial
				     ; value of [next_page].
				     ; It points to the first available
				     ; page block for users (ring 3) ...	
				     ; (MAT offset 32 = 1024/32)	
				     ; (at the of the first 4 MB)		
	add	ebx, [next_page] ; Free page searching starts from here
				 ; next_free_page >> 5
	add	ecx, [last_page] ; Free page searching ends here
				 ; (total_pages - 1) >> 5
al_p_scan:
	cmp	ebx, ecx
	ja	short al_p_notfound
	;
	; 01/07/2015
	; AMD64 Architecture Programmer’s Manual
	; Volume 3:
	; General-Purpose and System Instructions
	;
	; BSF - Bit Scan Forward
	;
	;   Searches the value in a register or a memory location
	;   (second operand) for the least-significant set bit. 
	;   If a set bit is found, the instruction clears the zero flag (ZF)
	;   and stores the index of the least-significant set bit in a destination
	;   register (first operand). If the second operand contains 0, 
	;   the instruction sets ZF to 1 and does not change the contents of the 
	;   destination register. The bit index is an unsigned offset from bit 0 
	;   of the searched value
	;
	bsf	eax, [ebx] ; Scans source operand for first bit set (1).
			   ; Clear ZF if a bit is found set (1) and 
			   ; loads the destination with an index to
			   ; first set bit. (0 -> 31) 
			   ; Sets ZF to 1 if no bits are found set.
	jnz	short al_p_found ; ZF = 0 -> a free page has been found
			 ;
			 ; NOTE:  a Memory Allocation Table bit 
			 ;	  with value of 1 means 
			 ;	  the corresponding page is free 
			 ;	  (Retro UNIX 386 v1 feature only!)
	add	ebx, 4
			 ; We return back for searching next page block
			 ; NOTE: [free_pages] is not ZERO; so, 
			 ;	 we always will find at least 1 free page here.
        jmp     short al_p_scan
	;
al_p_notfound:
	sub	ecx, MEM_ALLOC_TBL
	mov	[next_page], ecx ; next/first free page = last page 
				 ; (deallocate_page procedure will change it)
	xor	eax, eax
	mov	[free_pages], eax ; 0
	pop	ecx
	pop	ebx
	;
; 17/04/2021
; ('swap_out' procedure call is disabled as temporary)

out_of_memory:
;	call	swap_out
;	jnc	short al_p_ok  ; [free_pages] = 0, re-allocation by swap_out
;	;
;	sub 	eax, eax ; 0
	stc
	retn

al_p_found:
	mov	ecx, ebx
	sub	ecx, MEM_ALLOC_TBL
	mov	[next_page], ecx ; Set first free page searching start
				 ; address/offset (to the next)
        dec     dword [free_pages] ; 1 page has been allocated (X = X-1) 
	;
	btr	[ebx], eax	 ; The destination bit indexed by the source value
				 ; is copied into the Carry Flag and then cleared
				 ; in the destination.
				 ;
				 ; Reset the bit which is corresponding to the 
				 ; (just) allocated page.
	; 01/07/2015 (4*8 = 32, 1 allocation byte = 8 pages)	
	shl	ecx, 3		 ; (page block offset * 32) + page index
	add	eax, ecx	 ; = page number
	shl	eax, 12		 ; physical address of the page (flat/real value)
	; EAX = physical address of memory page
	;
	; NOTE: The relevant page directory and page table entry will be updated
	;       according to this EAX value...
	pop	ecx
	pop	ebx
al_p_ok:
	retn

make_page_dir:
	; 18/04/2015
	; 12/04/2015
	; 23/10/2014
	; 16/10/2014
	; 09/10/2014 ; (Retro UNIX 386 v1 - beginning)
	;
	; INPUT ->
	;	none
	; OUTPUT ->
	;	(EAX = 0)
	;	cf = 1 -> insufficient (out of) memory error
	;	cf = 0 ->
	;	u.pgdir = page directory (physical) address of the current
	;		  process/user.
	;
	; Modified Registers -> EAX
	;
	call	allocate_page
	jc	short mkpd_error
	;
	mov	[u.pgdir], eax    ; Page dir address for current user/process
				  ; (Physical address)
clear_page:
	; 18/04/2015
	; 09/10/2014 ; (Retro UNIX 386 v1 - beginning)
	;
	; INPUT ->
	;	EAX = physical address of the page
	; OUTPUT ->
	;	all bytes of the page will be cleared
	;
	; Modified Registers -> none
	;
	push	edi
	push	ecx
	push	eax
	mov	ecx, PAGE_SIZE / 4
	mov	edi, eax
	xor	eax, eax
	rep	stosd
	pop	eax
	pop	ecx
	pop	edi
mkpd_error:
mkpt_error:
	retn

make_page_table:
	; 23/06/2015
	; 18/04/2015
	; 12/04/2015
	; 16/10/2014
	; 09/10/2014 ; (Retro UNIX 386 v1 - beginning)
	;
	; INPUT ->
	;	EBX = virtual (linear) address
	;	ECX = page table attributes (lower 12 bits)
	;	      (higher 20 bits must be ZERO)
	;	      (bit 0 must be 1)	 
	;	u.pgdir = page directory (physical) address
	; OUTPUT ->
	;	EDX = Page directory entry address
	;	EAX = Page table address
	;	cf = 1 -> insufficient (out of) memory error
	;	cf = 0 -> page table address in the PDE (EDX)
	;
	; Modified Registers -> EAX, EDX
	;
	call	allocate_page
	jc	short mkpt_error
	call	set_pde	
	jmp	short clear_page

make_page:
	; 24/07/2015
	; 23/06/2015 ; (Retro UNIX 386 v1 - beginning)
	;
	; INPUT ->
	;	EBX = virtual (linear) address
	;	ECX = page attributes (lower 12 bits)
	;	      (higher 20 bits must be ZERO)
	;	      (bit 0 must be 1)	 
	;	u.pgdir = page directory (physical) address
	; OUTPUT ->
	;	EBX = Virtual address
	;	(EDX = PTE value)
	;	EAX = Physical address
	;	cf = 1 -> insufficient (out of) memory error
	;
	; Modified Registers -> EAX, EDX
	;
	call	allocate_page
	jc	short mkp_err
	call	set_pte	
	jnc	short clear_page ; 18/04/2015
mkp_err:
	retn


set_pde:	; Set page directory entry (PDE)
	; 20/07/2015
	; 18/04/2015
	; 12/04/2015
	; 23/10/2014
	; 10/10/2014 ; (Retro UNIX 386 v1 - beginning)
	;
	; INPUT ->
	;	EAX = physical address
	;	      (use present value if EAX = 0)
	;	EBX = virtual (linear) address
	;	ECX = page table attributes (lower 12 bits)
	;	      (higher 20 bits must be ZERO)
	;	      (bit 0 must be 1)	 
	;	u.pgdir = page directory (physical) address
	; OUTPUT ->
	;	EDX = PDE address
	;	EAX = page table address (physical)
	;	;(CF=1 -> Invalid page address)
	;
	; Modified Registers -> EDX
	;
	mov	edx, ebx
	shr	edx, PAGE_D_SHIFT ; 22
	shl	edx, 2 ; offset to page directory (1024*4)
	add	edx, [u.pgdir]
	;
	and	eax, eax
	jnz	short spde_1
	;
	mov	eax, [edx]  ; old PDE value
	;test	al, 1
	;jz	short spde_2
	and	ax, PDE_A_CLEAR ; 0F000h  ; clear lower 12 bits
spde_1:
	;and	cx, 0FFFh
	mov	[edx], eax
	or	[edx], cx
	retn
;spde_2: ; error
;	stc
;	retn

set_pte:	; Set page table entry (PTE)
	; 24/07/2015
	; 20/07/2015
	; 23/06/2015
	; 18/04/2015
	; 12/04/2015
	; 10/10/2014 ; (Retro UNIX 386 v1 - beginning)
	;
	; INPUT ->
	;	EAX = physical page address
	;	      (use present value if EAX = 0)
	;	EBX = virtual (linear) address
	;	ECX = page attributes (lower 12 bits)
	;	      (higher 20 bits must be ZERO)
	;	      (bit 0 must be 1)	 
	;	u.pgdir = page directory (physical) address
	; OUTPUT ->
	;	EAX = physical page address
	;	(EDX = PTE value)
	;	EBX = virtual address
	;
	;	CF = 1 -> error
	;
	; Modified Registers -> EAX, EDX
	;
	push	eax
	mov	eax, [u.pgdir] ; 20/07/2015
	call 	get_pde
		; EDX = PDE address
		; EAX = PDE value
	pop	edx ; physical page address
	jc	short spte_err ; PDE not present
	;
	push	ebx ; 24/07/2015
	and	ax, PDE_A_CLEAR ; 0F000h ; clear lower 12 bits
			    ; EDX = PT address (physical)	
	shr	ebx, PAGE_SHIFT ; 12
	and	ebx, PTE_MASK	; 03FFh
			 ; clear higher 10 bits (PD bits)
	shl	ebx, 2   ; offset to page table (1024*4)
	add	ebx, eax
	;
	mov	eax, [ebx] ; Old PTE value
	test	al, 1
	jz	short spte_0
	or	edx, edx
	jnz	short spte_1
	and	ax, PTE_A_CLEAR ; 0F000h ; clear lower 12 bits
	mov	edx, eax
	jmp	short spte_2	
spte_0:
	; If this PTE contains a swap (disk) address,
	; it can be updated by using 'swap_in' procedure
	; only!
	and	eax, eax
	jz	short spte_1
	; 24/07/2015
	; swapped page ! (on disk)
	pop	ebx
spte_err:
	stc
	retn
spte_1: 
	mov	eax, edx
spte_2:
	or	edx, ecx
	; 23/06/2015
	mov	[ebx], edx ; PTE value in EDX
	; 24/07/2015
	pop	ebx
	retn

get_pde:	; Get present value of the relevant PDE
	; 20/07/2015
	; 18/04/2015
	; 12/04/2015
	; 10/10/2014 ; (Retro UNIX 386 v1 - beginning)
	;
	; INPUT ->
	;	EBX = virtual (linear) address
	;	EAX = page directory (physical) address
	; OUTPUT ->
	;	EDX = Page directory entry address
	;	EAX = Page directory entry value
	;	CF = 1 -> PDE not present or invalid ? 
	; Modified Registers -> EDX, EAX
	;
	mov	edx, ebx
	shr	edx, PAGE_D_SHIFT ; 22  (12+10)
	shl 	edx, 2 ; offset to page directory (1024*4)
	add	edx, eax ; page directory address (physical)
	mov	eax, [edx]
	test	al, PDE_A_PRESENT ; page table is present or not !
	jnz	short gpte_retn
	stc
gpde_retn:	
	retn

get_pte:
		; Get present value of the relevant PTE
	; 29/07/2015
	; 20/07/2015
	; 18/04/2015
	; 12/04/2015
	; 10/10/2014 ; (Retro UNIX 386 v1 - beginning)
	;
	; INPUT ->
	;	EBX = virtual (linear) address
	;	EAX = page directory (physical) address
	; OUTPUT ->
	;	EDX = Page table entry address (if CF=0)
	;	      Page directory entry address (if CF=1)
	;            (Bit 0 value is 0 if PT is not present)
	;	EAX = Page table entry value (page address)
	;	CF = 1 -> PDE not present or invalid ? 
	; Modified Registers -> EAX, EDX
	;
	call 	get_pde
	jc	short gpde_retn	; page table is not present
	;jnc	short gpte_1
	;retn
;gpte_1:
	and	ax, PDE_A_CLEAR ; 0F000h ; clear lower 12 bits
	mov	edx, ebx
	shr	edx, PAGE_SHIFT ; 12
	and	edx, PTE_MASK	; 03FFh
			 ; clear higher 10 bits (PD bits)
	shl	edx, 2 ; offset from start of page table (1024*4)
	add	edx, eax
	mov	eax, [edx]
gpte_retn:
	retn

deallocate_page_dir:
	; 15/09/2015
	; 05/08/2015
	; 30/04/2015
	; 28/04/2015
	; 17/10/2014
	; 12/10/2014 (Retro UNIX 386 v1 - beginning)
	;
	; INPUT ->
	;	EAX = PHYSICAL ADDRESS OF THE PAGE DIRECTORY (CHILD)
	;	EBX = PHYSICAL ADDRESS OF THE PARENT'S PAGE DIRECTORY
	; OUTPUT ->
	;	All of page tables in the page directory
	;	and page dir's itself will be deallocated
	;	except 'read only' duplicated pages (will be converted
	;	to writable pages).
	;
	; Modified Registers -> EAX
	;
	;
	push	esi
	push	ecx
	push	eax
	mov	esi, eax 
	xor	ecx, ecx
	; The 1st PDE points to Kernel Page Table 0 (the 1st 4MB),
	; it must not be deallocated
	mov	[esi], ecx ; 0 ; clear PDE 0
dapd_0:
	lodsd
	test	al, PDE_A_PRESENT ; bit 0, present flag (must be 1)
	jz	short dapd_1	
	and	ax, PDE_A_CLEAR ; 0F000h ; clear lower 12 (attribute) bits
	call	deallocate_page_table			
dapd_1:
	inc	ecx ; page directory entry index
	cmp	ecx, PAGE_SIZE / 4 ; 1024
	jb	short dapd_0
dapd_2:
	pop	eax
	call	deallocate_page	; deallocate the page dir's itself
	pop	ecx
	pop	esi
	retn

deallocate_page_table:
	; 29/08/2023 - TRDOS 386 v2.0.6
	; 17/04/2021 - TRDOS 386 v2.0.4
	;	 (temporary modifications)
	; 12/07/2016
	; 19/09/2015
	; 15/09/2015
	; 05/08/2015
	; 30/04/2015
	; 28/04/2015
	; 24/10/2014
	; 23/10/2014
	; 12/10/2014 (Retro UNIX 386 v1 - beginning)
	;
	; INPUT ->
	;	EAX = PHYSICAL (real/flat) ADDRESS OF THE PAGE TABLE
	;	EBX = PHYSICAL ADDRESS OF THE PARENT'S PAGE DIRECTORY
	;	(ECX = page directory entry index)
	; OUTPUT ->
	;	All of pages in the page table and page table's itself
	;	will be deallocated except 'read only' duplicated pages
	;	(will be converted to writable pages).
	;
	; Modified Registers -> EAX
	;
	push	esi
	push	edi
	push	edx
	push	eax ; *
	mov	esi, eax 
	xor	edi, edi ; 0
dapt_0:
	lodsd
	test	al, PTE_A_PRESENT ; bit 0, present flag (must be 1)
	jz	short dapt_1
	;
	test	al, PTE_A_WRITE   ; bit 1, writable (r/w) flag
				  ; (must be 1)
	jnz	short dapt_3
	; Read only -duplicated- page (belongs to a parent or a child)
        test    ax, PTE_DUPLICATED ; Was this page duplicated 
				   ; as child's page ?
	jz	short dapt_4 ; Clear PTE but don't deallocate the page!
	; check the parent's PTE value is read only & same page or not.. 
	; ECX = page directory entry index (0-1023)
	push	ebx
	push	ecx
	;shl	cx, 2 ; *4
	; 29/08/2023
	shl	ecx, 2
	add	ebx, ecx ; PDE offset (for the parent)
	mov	ecx, [ebx]
	test	cl, PDE_A_PRESENT ; present (valid) or not ?
	jz	short dapt_2	; parent process does not use this page
	and	cx, PDE_A_CLEAR ; 0F000h ; Clear attribute bits
	; EDI = page table entry index (0-1023)
	mov	edx, edi 
	;shl	dx, 2 ; *4
	; 29/08/2023
	shl	edx, 2
	add	edx, ecx ; PTE offset (for the parent)
	mov	ebx, [edx]
	test	bl, PTE_A_PRESENT ; present or not ?
	jz	short dapt_2	; parent process does not use this page
	and	ax, PTE_A_CLEAR ; 0F000h ; Clear attribute bits 
	and	bx, PTE_A_CLEAR ; 0F000h ; Clear attribute bits
	cmp	eax, ebx	; parent's and child's pages are same ?
	jne	short dapt_2	; not same page
				; deallocate the child's page
        or      byte [edx], PTE_A_WRITE ; convert to writable page (parent)
	pop	ecx
	pop	ebx
	jmp	short dapt_4

; 17/04/2021
; ('dapt_1' is disabled as temporary)
;
;dapt_1:
;	or	eax, eax	; swapped page ?
;	jz	short dapt_5	; no
;				; yes
;	shr	eax, 1
;	call	unlink_swap_block ; Deallocate swapped page block
;				  ; on the swap disk (or in file)
;	jmp	short dapt_5
dapt_2:
	pop	ecx
	pop	ebx
dapt_3:	
	; 12/07/2016
	test	ax, PTE_SHARED ; shared or direct memory access indicator
	jnz	short dapt_4   ; AVL bit 1 = 1, do not deallocate this page!
	;
	;and	ax, PTE_A_CLEAR ; 0F000h ; clear lower 12 (attribute) bits
	call	deallocate_page ; set the mem allocation bit of this page
dapt_4:
	mov	dword [esi-4], 0 ; clear/reset PTE (child, dupl. as parent)
dapt_1:	; 17/04/2021 (temporary)
dapt_5:
	inc	edi ; page table entry index
	cmp	edi, PAGE_SIZE / 4 ; 1024
	jb	short dapt_0
	;
	pop	eax ; *
	pop	edx
	pop	edi	
	pop	esi
	;
	;call	deallocate_page	; deallocate the page table's itself
	;retn

deallocate_page:
	; 15/09/2015
	; 28/04/2015
	; 10/03/2015
	; 17/10/2014
	; 12/10/2014 (Retro UNIX 386 v1 - beginning)
	;
	; INPUT -> 
	;	EAX = PHYSICAL (real/flat) ADDRESS OF THE ALLOCATED PAGE
	; OUTPUT ->
	;	[free_pages] is increased
	;	(corresponding MEMORY ALLOCATION TABLE bit is SET)
	;	CF = 1 if the page is already deallocated
	; 	       (or not allocated) before.  
	;
	; Modified Registers -> EAX
	;
	push	ebx
	push	edx
	;
	shr	eax, PAGE_SHIFT      ; shift physical address to 
				     ; 12 bits right
				     ; to get page number
	mov	edx, eax
	; 15/09/2015
	shr	edx, 3		     ; to get offset to M.A.T.
				     ; (1 allocation bit = 1 page)
				     ; (1 allocation bytes = 8 pages)
	and	dl, 0FCh 	     ; clear lower 2 bits
				     ; (to get 32 bit position)			
	;
	mov	ebx, MEM_ALLOC_TBL   ; Memory Allocation Table address
	add	ebx, edx
	and	eax, 1Fh	     ; lower 5 bits only
				     ; (allocation bit position)	 
	cmp 	edx, [next_page]     ; is the new free page address lower
				     ; than the address in 'next_page' ?
				     ; (next/first free page value)		
	jnb	short dap_1	     ; no	
	mov	[next_page], edx     ; yes
dap_1:
	bts	[ebx], eax	     ; unlink/release/deallocate page
				     ; set relevant bit to 1.
				     ; set CF to the previous bit value	
	;cmc			     ; complement carry flag	
	;jc	short dap_2	     ; do not increase free_pages count
				     ; if the page is already deallocated
				     ; before.	
        inc     dword [free_pages]
dap_2:
	pop	edx
	pop	ebx
	retn

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;; Copyright (C) KolibriOS team 2004-2012. All rights reserved. ;;
;; Distributed under terms of the GNU General Public License    ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;$Revision: 5057 $


;;align 4
;;proc alloc_page

;;        pushfd
;;        cli
;;        push    ebx
;;;//-
;;        cmp     [pg_data.pages_free], 1
;;        jle     .out_of_memory
;;;//-
;;
;;        mov     ebx, [page_start]
;;        mov     ecx, [page_end]
;;.l1:
;;        bsf     eax, [ebx];
;;        jnz     .found
;;        add     ebx, 4
;;        cmp     ebx, ecx
;;        jb      .l1
;;        pop     ebx
;;        popfd
;;        xor     eax, eax
;;        ret
;;.found:
;;;//-
;;        dec     [pg_data.pages_free]
;;        jz      .out_of_memory
;;;//-
;;        btr     [ebx], eax
;;        mov     [page_start], ebx
;;        sub     ebx, sys_pgmap
;;        lea     eax, [eax+ebx*8]
;;        shl     eax, 12
;;;//-       dec [pg_data.pages_free]
;;        pop     ebx
;;        popfd
;;        ret
;;;//-
;;.out_of_memory:
;;        mov     [pg_data.pages_free], 1
;;        xor     eax, eax
;;        pop     ebx
;;        popfd
;;        ret
;;;//-
;;endp

duplicate_page_dir:
	; 21/09/2015
	; 31/08/2015
	; 20/07/2015
	; 28/04/2015
	; 27/04/2015
	; 18/04/2015
	; 12/04/2015
	; 18/10/2014
	; 16/10/2014 (Retro UNIX 386 v1 - beginning)
	;
	; INPUT -> 
	;	[u.pgdir] = PHYSICAL (real/flat) ADDRESS of the parent's
	;		    page directory.
	; OUTPUT ->
	;	EAX =  PHYSICAL (real/flat) ADDRESS of the child's
	;	       page directory.
	;	(New page directory with new page table entries.)
	;	(New page tables with read only copies of the parent's
	;	pages.)
	;	EAX = 0 -> Error (CF = 1)
	;
	; Modified Registers -> none (except EAX)
	;
	call	allocate_page
	jc	short dpd_err
	;
	push	ebp ; 20/07/2015
	push	esi
	push	edi
	push	ebx
	push	ecx
	mov	esi, [u.pgdir]
	mov	edi, eax
	push	eax ; save child's page directory address
	; 31/08/2015
	; copy PDE 0 from the parent's page dir to the child's page dir
	; (use same system space for all user page tables) 
	movsd
	mov	ebp, 1024*4096 ; pass the 1st 4MB (system space)
	mov	ecx, (PAGE_SIZE / 4) - 1 ; 1023
dpd_0:	
	lodsd
	;or	eax, eax
        ;jnz     short dpd_1
	test	al, PDE_A_PRESENT ;  bit 0 =  1
	jnz	short dpd_1
 	; 20/07/2015 (virtual address at the end of the page table)	
	add	ebp, 1024*4096 ; page size * PTE count
	jmp	short dpd_2
dpd_1:	
	and	ax, PDE_A_CLEAR ; 0F000h ; clear attribute bits
	mov	ebx, eax
	; EBX = Parent's page table address
	call	duplicate_page_table
	jc	short dpd_p_err
	; EAX = Child's page table address
	or	al, PDE_A_PRESENT + PDE_A_WRITE + PDE_A_USER
			 ; set bit 0, bit 1 and bit 2 to 1
			 ; (present, writable, user)
dpd_2:
	stosd
	loop	dpd_0
	;
	pop	eax  ; restore child's page directory address
dpd_3:
	pop	ecx
	pop	ebx
	pop	edi
	pop	esi
	pop	ebp ; 20/07/2015
dpd_err:
	retn
dpd_p_err:
	; release the allocated pages missing (recover free space)
	pop	eax  ; the new page directory address (physical)
	mov	ebx, [u.pgdir] ; parent's page directory address 
	call 	deallocate_page_dir
	sub	eax, eax ; 0
	stc
	jmp	short dpd_3	

duplicate_page_table:
	; 17/04/2021 - TRDOS 386 v2.0.4
	;	 (temporary modifications)
	; 20/02/2017
	; 21/09/2015
	; 20/07/2015
	; 05/05/2015
	; 28/04/2015
	; 27/04/2015
	; 18/04/2015
	; 18/10/2014
	; 16/10/2014 (Retro UNIX 386 v1 - beginning)
	;
	; INPUT -> 
	;	EBX = PHYSICAL (real/flat) ADDRESS of the parent's page table.
	;       20/02/2017		 
	;	EBP = Linear address of the page (from 'duplicate_page_dir')
	;	      (Linear address = CORE + user's virtual address) 	
	; OUTPUT ->
	;	EAX = PHYSICAL (real/flat) ADDRESS of the child's page table.
	;	      (with 'read only' attribute of page table entries)
	;	20/02/2017
	;	EBP = Next linear page address (for 'duplicate_page_dir')
	;	
	;	CF = 1 -> error 
	;
	; Modified Registers -> EBP (except EAX)
	;
	call	allocate_page
	jc	short dpt_err
	;
	push	eax ; *
	push	esi
	push	edi
	push	edx
	push	ecx
	;
	mov	esi, ebx
	mov	edi, eax
	mov	edx, eax
	add	edx, PAGE_SIZE 	
dpt_0:
	lodsd
	and	eax, eax
	jz	short dpt_3
	test	al, PTE_A_PRESENT ;  bit 0 =  1
	; 17/04/2021 (temporary)
	;jnz	short dpt_1
	jz	short dpt_p_err

; 17/04/2021
; ('reload_page' procedure call is disabled as temporary)
;
;	; 20/07/2015
;	; ebp = virtual (linear) address of the memory page
;	call	reload_page ; 28/04/2015
;	jc	short dpt_p_err
dpt_1:
	; 21/09/2015
	mov	ecx, eax
	and	ax, PTE_A_CLEAR ; 0F000h ; clear attribute bits
	test	cl, PTE_A_WRITE ; writable page ?
	jnz	short dpt_2
	; Read only (parent) page
	; 	- there is a third process which uses this page -
	; Allocate a new page for the child process
	call	allocate_page
	jc	short dpt_p_err
	push	edi
	push	esi
	mov	esi, ecx
	mov	edi, eax
	mov	ecx, PAGE_SIZE/4
	rep	movsd	; copy page (4096 bytes)
	pop	esi
	pop	edi
	;

; 17/04/2021
; ('add_to_swap_queue' procedure call is disabled as temporary)
; 
;	push	ebx
;	push	eax
;	; 20/07/2015
;	mov	ebx, ebp
;	; ebx = virtual (linear) address of the memory page
;	call	add_to_swap_queue
;	pop	eax
;	pop	ebx

	; 21/09/2015
	or	al, PTE_A_USER+PTE_A_WRITE+PTE_A_PRESENT 
		; user + writable + present page
	jmp	short dpt_3
dpt_2:
	;or	ax, PTE_A_USER+PTE_A_PRESENT 
	or	al, PTE_A_USER+PTE_A_PRESENT 
		    ; (read only page!)
	mov	[esi-4], eax ; update parent's PTE
	or      ax, PTE_DUPLICATED  ; (read only page & duplicated PTE!)
dpt_3:
	stosd  ; EDI points to child's PTE  	 
	;
	add	ebp, 4096 ; 20/07/2015 (next page)
	;
	cmp	edi, edx
	jb	short dpt_0
dpt_p_err:
	pop	ecx
	pop	edx
	pop	edi
	pop	esi
	pop	eax ; *
dpt_err:
	retn

page_fault_handler:	; CPU EXCEPTION 0Eh (14) : Page Fault !
	; 17/04/2021 - TRDOS 386 v2.0.4
	;	 (temporary modifications)
	; 21/09/2015
	; 19/09/2015
	; 17/09/2015
	; 28/08/2015
	; 20/07/2015
	; 28/06/2015
	; 03/05/2015
	; 30/04/2015
	; 18/04/2015
	; 12/04/2015
	; 30/10/2014
	; 11/09/2014
	; 10/09/2014 (Retro UNIX 386 v1 - beginning)
	;
	; Note: This is not an interrupt/exception handler.
	;	This is a 'page fault remedy' subroutine 
	;	which will be called by standard/uniform
	;	exception handler.
	;
	; INPUT -> 
	;	[error_code] = 32 bit ERROR CODE (lower 5 bits are valid)
	;
	;	cr2 = the virtual (linear) address 
	;	      which has caused to page fault (19/09/2015)
	;
	; OUTPUT ->
	;	(corresponding PAGE TABLE ENTRY is mapped/set)
	;	EAX = 0 -> no error
	;	EAX > 0 -> error code in EAX (also CF = 1)
	;
	; Modified Registers -> none (except EAX)
	;	
        ;
        ; ERROR CODE:
	;	 31  .....	4   3	2   1	0
	;	+---+-- --+---+---+---+---+---+---+
	;	|   Reserved  | I | R | U | W | P |
	;	+---+-- --+---+---+---+---+---+---+
	;
	; P : PRESENT -	When set, the page fault was caused by 
    	;		a page-protection violation. When not set,
	;		it was caused by a non-present page.
	; W : WRITE   -	When set, the page fault was caused by
	;		a page write. When not set, it was caused
	;		by a page read.
	; U : USER    -	When set, the page fault was caused 
	;		while CPL = 3. 
	;		This does not necessarily mean that
	;		the page fault was a privilege violation.
	; R : RESERVD -	When set, the page fault was caused by
	;     WRITE	reading a 1 in a reserved field.
	; I : INSTRUC -	When set, the page fault was caused by
	;     FETCH	an instruction fetch
	;
	;; x86 (32 bit) VIRTUAL ADDRESS TRANSLATION
	;  31               22                  12 11                    0
	; +-------------------+-------------------+-----------------------+
       	; | PAGE DIR. ENTRY # | PAGE TAB. ENTRY # |        OFFSET         |
       	; +-------------------+-------------------+-----------------------+
	;

	;; CR3 REGISTER (Control Register 3)
	;  31                                   12             5 4 3 2   0
	; +---------------------------------------+-------------+---+-----+
      	; |                                       |  		|P|P|     |
      	; |   PAGE DIRECTORY TABLE BASE ADDRESS   |  reserved	|C|W|rsvrd|
      	; |                                       | 		|D|T|     |
   	; +---------------------------------------+-------------+---+-----+
	;
	;	PWT    - WRITE THROUGH
	;	PCD    - CACHE DISABLE		
	;
	;
	;; x86 PAGE DIRECTORY ENTRY (4 KByte Page)
	;  31                                   12 11  9 8 7 6 5 4 3 2 1 0
	; +---------------------------------------+-----+---+-+-+---+-+-+-+
      	; |                                       |     | | | | |P|P|U|R| |
      	; |     PAGE TABLE BASE ADDRESS 31..12    | AVL |G|0|D|A|C|W|/|/|P|
      	; |                                       |     | | | | |D|T|S|W| |
   	; +---------------------------------------+-----+---+-+-+---+-+-+-+
	;
        ;       P      - PRESENT
        ;       R/W    - READ/WRITE
        ;       U/S    - USER/SUPERVISOR
	;	PWT    - WRITE THROUGH
	;	PCD    - CACHE DISABLE	
	;	A      - ACCESSED	
        ;       D      - DIRTY (IGNORED)
	;	PAT    - PAGE ATTRIBUTE TABLE INDEX (CACHE BEHAVIOR)
	;	G      - GLOBAL	(IGNORED) 
        ;       AVL    - AVAILABLE FOR SYSTEMS PROGRAMMER USE
	;
	;
	;; x86 PAGE TABLE ENTRY (4 KByte Page)
	;  31                                   12 11  9 8 7 6 5 4 3 2 1 0
	; +---------------------------------------+-----+---+-+-+---+-+-+-+
      	; |                                       |     | |P| | |P|P|U|R| |
      	; |     PAGE FRAME BASE ADDRESS 31..12    | AVL |G|A|D|A|C|W|/|/|P|
      	; |                                       |     | |T| | |D|T|S|W| |
   	; +---------------------------------------+-----+---+-+-+---+-+-+-+
	;
        ;       P      - PRESENT
        ;       R/W    - READ/WRITE
        ;       U/S    - USER/SUPERVISOR
	;	PWT    - WRITE THROUGH
	;	PCD    - CACHE DISABLE	
	;	A      - ACCESSED	
        ;       D      - DIRTY
	;	PAT    - PAGE ATTRIBUTE TABLE INDEX (CACHE BEHAVIOR)
	;	G      - GLOBAL	 
        ;       AVL    - AVAILABLE FOR SYSTEMS PROGRAMMER USE
	;
	;
	;; 80386 PAGE TABLE ENTRY (4 KByte Page)
	;  31                                   12 11  9 8 7 6 5 4 3 2 1 0
	; +---------------------------------------+-----+-+-+-+-+---+-+-+-+
      	; |                                       |     | | | | | | |U|R| |
      	; |     PAGE FRAME BASE ADDRESS 31..12    | AVL |0|0|D|A|0|0|/|/|P|
      	; |                                       |     | | | | | | |S|W| |
      	; +---------------------------------------+-----+-+-+-+-+---+-+-+-+
	;
        ;       P      - PRESENT
        ;       R/W    - READ/WRITE
        ;       U/S    - USER/SUPERVISOR
        ;       D      - DIRTY
        ;       AVL    - AVAILABLE FOR SYSTEMS PROGRAMMER USE
	;
        ;       NOTE: 0 INDICATES INTEL RESERVED. DO NOT DEFINE.
	;
	;
	;; Invalid Page Table Entry
	; 31                                                           1 0
      	; +-------------------------------------------------------------+-+
      	; |                                                             | |
      	; |                          AVAILABLE                          |0|
      	; |                                                             | |
      	; +-------------------------------------------------------------+-+
	;

	push	ebx
	push	edx
	push	ecx
	;
	; 21/09/2015 (debugging)
	inc	dword [u.pfcount] ; page fault count for running process
	inc	dword [PF_Count] ; total page fault count	
	; 28/06/2015
	;mov	edx, [error_code] ; Lower 5 bits are valid
	mov	dl, [error_code]
	;
	test	dl, 1	; page fault was caused by a non-present page
			; sign
	jz	short pfh_alloc_np
	; 
	; If it is not a 'write on read only page' type page fault
	; major page fault error with minor reason must be returned without 
	; fixing the problem. 'sys_exit with error' will be needed
	; after return here!
	; Page fault will be remedied, by copying page contents
	; to newly allocated page with write permission;
	; sys_fork -> sys_exec -> copy on write, demand paging method is 
	; used for working with minimum possible memory usage. 
	; sys_fork will duplicate page directory and tables of parent  
	; process with 'read only' flag. If the child process attempts to
	; write on these read only pages, page fault will be directed here
	; for allocating a new page with same data/content. 
	;
	; IMPORTANT : Retro UNIX 386 v1 (and SINGLIX and TR-DOS)
	; will not force to separate CODE and DATA space 
	; in a process/program... 
	; CODE segment/section may contain DATA!
	; It is flat, smoth and simplest programming method already as in 
	; Retro UNIX 8086 v1 and MS-DOS programs.
	;	
	test	dl, 2	; page fault was caused by a page write
			; sign
        jz      pfh_p_err
	; 31/08/2015
	test	dl, 4	; page fault was caused while CPL = 3 (user mode)
			; sign.  (U+W+P = 4+2+1 = 7)
        jz	pfh_pv_err
	;
	; make a new page and copy the parent's page content
	; as the child's new page content
	;
	mov	ebx, cr2 ; CR2 contains the linear address 
			 ; which has caused to page fault
	call 	copy_page
        jc      pfh_im_err ; insufficient memory
	;
        jmp     pfh_cpp_ok
	;
pfh_alloc_np:
	call	allocate_page	; (allocate a new page)
        jc      pfh_im_err	; 'insufficient memory' error
pfh_chk_cpl:
	; EAX = Physical (base) address of the allocated (new) page
		; (Lower 12 bits are ZERO, because 
		;	the address is on a page boundary)
	and	dl, 4	; CPL = 3 ?
	jnz	short pfh_um
			; Page fault handler for kernel/system mode (CPL=0)		
	mov	ebx, cr3 ; CR3 (Control Register 3) contains physical address
			 ; of the current/active page directory
			 ; (Always kernel/system mode page directory, here!)
			 ; Note: Lower 12 bits are 0. (page boundary)
	jmp	short pfh_get_pde
	;
pfh_um:			; Page fault handler for user/appl. mode (CPL=3)
 	mov	ebx, [u.pgdir] ; Page directory of current/active process
			; Physical address of the USER's page directory
			; Note: Lower 12 bits are 0. (page boundary)
pfh_get_pde:
	or	dl, 3	; USER + WRITE + PRESENT or SYSTEM + WRITE + PRESENT
	mov	ecx, cr2 ; CR2 contains the virtual address 
			 ; which has been caused to page fault
			 ;
	shr	ecx, 20	 ; shift 20 bits right
	and	cl, 0FCh ; mask lower 2 bits to get PDE offset		
	;
	add	ebx, ecx ; now, EBX points to the relevant page dir entry 
	mov	ecx, [ebx] ; physical (base) address of the page table 	
	test	cl, 1	 ; check bit 0 is set (1) or not (0).
	jz	short pfh_set_pde ; Page directory entry is not valid,
			  	  ; set/validate page directory entry
	and	cx, PDE_A_CLEAR ; 0F000h ; Clear attribute bits
	mov	ebx, ecx ; Physical address of the page table
	mov	ecx, eax ; new page address (physical) 	
	jmp	short pfh_get_pte
pfh_set_pde:
	;; NOTE: Page directories and page tables never be swapped out!
	;;	 (So, we know this PDE is empty or invalid)
	;
	or	al, dl	 ; lower 3 bits are used as U/S, R/W, P flags
	mov	[ebx], eax ; Let's put the new page directory entry here !
	xor	al, al	 ; clear lower (3..8) bits
	mov	ebx, eax
	call	allocate_page	 ; (allocate a new page)
	jc	short pfh_im_err   ; 'insufficient memory' error
pfh_spde_1:
	; EAX = Physical (base) address of the allocated (new) page
	mov	ecx, eax
	call	clear_page ; Clear page content
pfh_get_pte:
	mov	eax, cr2 ; virtual address
			 ; which has been caused to page fault
	mov	edi, eax ; 20/07/2015
	shr	eax, 12	 ; shift 12 bit right to get 
			 ; higher 20 bits of the page fault address 
	and	eax, 3FFh ; mask PDE# bits, the result is PTE# (0 to 1023)
	shl	eax, 2	; shift 2 bits left to get PTE offset
	add	ebx, eax ; now, EBX points to the relevant page table entry 
; 17/04/2021 temporary
;	mov	eax, [ebx] ; get previous value of pte
;		; bit 0 of EAX is always 0 (otherwise we would not be here)

; 17/04/2021
; ('swap_in' procedure call has been disabled as temporary)
;
;	and	eax, eax
;	jz	short pfh_gpte_1
;	; 20/07/2015
;	xchg	ebx, ecx ; new page address (physical)
;	push	ebp ; 20/07/2015
;	mov	ebp, cr2
;		; ECX = physical address of the page table entry
;		; EBX = Memory page address (physical!)
;		; EAX = Swap disk (offset) address
;		; EBP = virtual address (page fault address)
;	call	swap_in
;	pop	ebp
;	jc      short pfh_err_retn
;	xchg	ecx, ebx
;		; EBX = physical address of the page table entry
;		; ECX = new page
pfh_gpte_1:
	or	cl, dl	; lower 3 bits are used as U/S, R/W, P flags
	mov	[ebx], ecx ; Let's put the new page table entry here !
pfh_cpp_ok:
; 17/04/2021
; ('add_to_swap_queue' procedure call has been disabled as temporary)
;
;	; 20/07/2015
;	mov	ebx, cr2
;	call 	add_to_swap_queue
	;
	; The new PTE (which contains the new page) will be added to 
	; the swap queue, here. 
	; (Later, if memory will become insufficient, 
	; one page will be swapped out which is at the head of 
	; the swap queue by using FIFO and access check methods.)
	;
	xor	eax, eax  ; 0
	;
pfh_err_retn:
	pop	ecx
	pop	edx
	pop	ebx
	retn 
	
pfh_im_err:
	mov	eax, ERR_MAJOR_PF + ERR_MINOR_IM ; Error code in AX
			; Major (Primary) Error: Page Fault
			; Minor (Secondary) Error: Insufficient Memory !
	jmp	short pfh_err_retn

pfh_p_err: ; 09/03/2015
pfh_pv_err:
	; Page fault was caused by a protection-violation
	mov	eax, ERR_MAJOR_PF + ERR_MINOR_PV ; Error code in AX
			; Major (Primary) Error: Page Fault
			; Minor (Secondary) Error: Protection violation !
	stc
	jmp	short pfh_err_retn

copy_page:
	; 29/08/2023 (TRDOS 386 v2.0.6)
	; 22/09/2015
	; 21/09/2015
	; 19/09/2015
	; 07/09/2015
	; 31/08/2015
	; 20/07/2015
	; 05/05/2015
	; 03/05/2015
	; 18/04/2015
	; 12/04/2015
	; 30/10/2014
	; 18/10/2014 (Retro UNIX 386 v1 - beginning)
	;
	; INPUT -> 
	;	EBX = Virtual (linear) address of source page
	;	     (Page fault address)
	; OUTPUT ->
	;	EAX = PHYSICAL (real/flat) ADDRESS OF THE ALLOCATED PAGE
	;	(corresponding PAGE TABLE ENTRY is mapped/set)
	;	EAX = 0 (CF = 1) 
	;		if there is not a free page to be allocated
	;	(page content of the source page will be copied
	;	onto the target/new page) 	
	;
	; Modified Registers -> ecx, ebx (except EAX)
	;	
	push	esi
	push	edi
	;push	ebx
	;push	ecx
	xor 	esi, esi
	shr	ebx, 12 ; shift 12 bits right to get PDE & PTE numbers
	mov	ecx, ebx ; save page fault address (as 12 bit shifted)
	shr	ebx, 8	 ; shift 8 bits right and then
	and	bl, 0FCh ; mask lower 2 bits to get PDE offset	
	mov 	edi, ebx ; save it for the parent of current process
	add	ebx, [u.pgdir] ; EBX points to the relevant page dir entry 
	mov	eax, [ebx] ; physical (base) address of the page table
	and	ax, PTE_A_CLEAR ; 0F000h ; clear attribute bits 	
	mov	ebx, ecx   ; (restore higher 20 bits of page fault address)
	and	ebx, 3FFh  ; mask PDE# bits, the result is PTE# (0 to 1023)
	;shl	bx, 2	   ; shift 2 bits left to get PTE offset
	; 29/08/2023
	shl	ebx, 2
	add	ebx, eax   ; EBX points to the relevant page table entry 
	; 07/09/2015
        test    word [ebx], PTE_DUPLICATED ; (Does current process share this
				     ; read only page as a child process?)	
	jnz	short cpp_0 ; yes
	mov	ecx, [ebx] ; PTE value
	and	cx, PTE_A_CLEAR ; 0F000h  ; clear page attributes
	jmp	short cpp_1
cpp_0:
	mov	esi, edi
	add	esi, [u.ppgdir] ; the parent's page directory entry
	mov	eax, [esi] ; physical (base) address of the page table
	and	ax, PTE_A_CLEAR ; 0F000h ; clear attribute bits
	mov	esi, ecx   ; (restore higher 20 bits of page fault address)	
	and	esi, 3FFh  ; mask PDE# bits, the result is PTE# (0 to 1023)
	;shl	si, 2	   ; shift 2 bits left to get PTE offset
	; 29/08/2023
	shl	esi, 2
	add	esi, eax   ; EDX points to the relevant page table entry  	
	mov	ecx, [esi] ; PTE value of the parent process
	; 21/09/2015
	mov	eax, [ebx] ; PTE value of the child process
	and	ax, PTE_A_CLEAR ; 0F000h ; clear page attributes	
	;
	test	cl, PTE_A_PRESENT ; is it a present/valid page ?
	jz	short cpp_3 ; the parent's page is not same page  	
	;
	and	cx, PTE_A_CLEAR ; 0F000h ; clear page attributes
	cmp	eax, ecx   ; Same page?	
	jne	short cpp_3 ; Parent page and child page are not same 
			    ; Convert child's page to writable page
cpp_1:
	call	allocate_page
	jc	short cpp_4 ; 'insufficient memory' error
	and	esi, esi    ; check ESI is valid or not
	jz	short cpp_2
		; Convert read only page to writable page 
		;(for the parent of the current process)
	;and	word [esi], PTE_A_CLEAR ; 0F000h
	; 22/09/2015
	mov	[esi], ecx
	or	byte [esi], PTE_A_PRESENT + PTE_A_WRITE + PTE_A_USER
				 ; 1+2+4 = 7
cpp_2:
	mov	edi, eax ; new page address of the child process
	; 07/09/2015
	mov	esi, ecx ; the page address of the parent process
	mov	ecx, PAGE_SIZE / 4
	rep	movsd ; 31/08/2015
cpp_3:		
	or	al, PTE_A_PRESENT + PTE_A_WRITE + PTE_A_USER ; 1+2+4 = 7
	mov	[ebx], eax ; Update PTE
	sub	al, al ; clear attributes
cpp_4:
	;pop	ecx
	;pop	ebx
	pop	edi
	pop	esi
	retn

;; 28/04/2015
;; 24/10/2014
;; 21/10/2014 (Retro UNIX 386 v1 - beginning)
;; SWAP_PAGE_QUEUE (4096 bytes)
;;
;;   0000   0001   0002   0003   ....   1020   1021   1022   1023	
;; +------+------+------+------+-    -+------+------+------+------+
;; |  pg1 |  pg2 |  pg3 |  pg4 | .... |pg1021|pg1022|pg1023|pg1024|
;; +------+------+------+------+-    -+------+------+------+------+    
;;
;; [swpq_last] = 0 to 4096 (step 4) -> the last position on the queue
;;
;; Method:
;;	Swap page queue is a list of allocated pages with physical
;;	addresses (system mode virtual adresses = physical addresses).
;;	It is used for 'swap_in' and 'swap_out' procedures.
;;	When a new page is being allocated, swap queue is updated
;;	by 'swap_queue_shift' procedure, header of the queue (offset 0)
;;	is checked for 'accessed' flag. If the 1st page on the queue
;;	is 'accessed' or 'read only', it is dropped from the list;
;;	other pages from the 2nd to the last (in [swpq_last]) shifted
;; 	to head then the 2nd page becomes the 1st and '[swpq_last]' 
;;	offset value becomes it's previous offset value - 4.
;;	If the 1st page of the swap page queue is not 'accessed'	
;;	the queue/list is not shifted.
;;	After the queue/list shift, newly allocated page is added
;;	to the tail of the queue at the [swpq_count*4] position.
;;	But, if [swpq_count] > 1023, the newly allocated page
;;	will not be added to the tail of swap page queue.  		 
;;	
;;	During 'swap_out' procedure, swap page queue is checked for
;;	the first non-accessed, writable page in the list, 
;;	from the head to the tail. The list is shifted to left 
;;	(to the head) till a non-accessed page will be found in the list.
;;	Then, this page	is swapped out (to disk) and then it is dropped
;;	from the list by a final swap queue shift. [swpq_count] value
;;	is changed. If all pages on the queue' are 'accessed', 
;;	'insufficient memory' error will be returned ('swap_out' 
;;	procedure will be failed)...
;;
;;	Note: If the 1st page of the queue is an 'accessed' page,
;;	'accessed' flag of the page will be reset (0) and that page
;;	(PTE) will be added to the tail of the queue after
;;	the check, if [swpq_count] < 1023. If [swpq_count] = 1024
;;	the queue will be rotated and the PTE in the head will be
;;	added to the tail after resetting 'accessed' bit. 
;;
;;
;;	
;; SWAP DISK/FILE (with 4096 bytes swapped page blocks)
;;
;;  00000000  00000004  00000008  0000000C   ...   size-8    size-4
;; +---------+---------+---------+---------+-- --+---------+---------+
;; |descriptr| page(1) | page(2) | page(3) | ... |page(n-1)| page(n) |
;; +---------+---------+---------+---------+-- --+---------+---------+    
;;
;; [swpd_next] = the first free block address in swapped page records
;;    		 for next free block search by 'swap_out' procedure.
;; [swpd_size] = swap disk/file size in sectors (512 bytes)
;;		 NOTE: max. possible swap disk size is 1024 GB
;; 		 (entire swap space must be accessed by using
;;		 31 bit offset address) 
;; [swpd_free] = free block (4096 bytes) count in swap disk/file space
;; [swpd_start] = absolute/start address of the swap disk/file
;;		  0 for file, or beginning sector of the swap partition
;; [swp_drv] = logical drive description table addr. of swap disk/file
;;
;; 					
;; Method:
;;	When the memory (ram) becomes insufficient, page allocation
;;	procedure swaps out a page from memory to the swap disk 
;;	(partition) or swap file to get a new free page at the memory.
;;	Swapping out is performed by using swap page queue.
;;
;; 	Allocation block size of swap disk/file is equal to page size
;;	(4096 bytes). Swapping address (in sectors) is recorded
;;	into relevant page file entry as 31 bit physical (logical)
;;	offset address as 1 bit shifted to left for present flag (0).
;;	Swapped page address is between 1 and swap disk/file size - 4.	  
;;	Absolute physical (logical) address of the swapped page is 
;;	calculated by adding offset value to the swap partition's 
;;	start address. If the swap device (disk) is a virtual disk 
;;	or it is a file, start address of the swap disk/volume is 0, 
;;	and offset value is equal to absolute (physical or logical)
;;	address/position. (It has not to be ZERO if the swap partition 
;;	is in a partitioned virtual hard disk.) 
;;
;;	Note: Swap addresses are always specified/declared in sectors, 
;;	not in bytes or	in blocks/zones/clusters (4096 bytes) as unit.
;;
;;	Swap disk/file allocation is mapped via 'Swap Allocation Table'
;;	at memory as similar to 'Memory Allocation Table'.
;;
;;	Every bit of Swap Allocation Table repsesents one swap block
;;	(equal to page size) respectively. Bit 0 of the S.A.T. byte 0
;;	is reserved for swap disk/file block 0 as descriptor block
;;	(also for compatibility with PTE). If bit value is ZERO,
;;	it means relevant (respective) block is in use, and, 
;;	of course, if bit value is 1, it means relevant (respective)
;;      swap disk/file block is free.
;;	For example: bit 1 of the byte 128 repsesents block 1025 
;;	(128*8+1) or sector (offset) 8200 on the swap disk or
;;	byte (offset/position) 4198400 in the swap file. 
;;	4GB swap space is represented via 128KB Swap Allocation Table.
;;	Initial layout of Swap Allocation Table is as follows:
;;	------------------------------------------------------------
;;	0111111111111111111111111 .... 11111111111111111111111111111
;;	------------------------------------------------------------
;;	(0 is reserved block, 1s represent free blocks respectively.)
;;	(Note: Allocation cell/unit of the table is bit, not byte)
;;
;;	..............................................................
;;
;;	'swap_out' procedure checks 'free_swap_blocks' count at first,
;;	then it searches Swap Allocation Table if free count is not
;;	zero. From begining the [swpd_next] dword value, the first bit 
;;	position with value of 1 on the table is converted to swap
;;	disk/file offset address, in sectors (not 4096 bytes block).
;;	'ldrv_write' procedure is called with ldrv (logical drive
;;	number of physical swap disk or virtual swap disk)
;;	number, sector offset (not absolute sector -LBA- number),
;;	and sector count (8, 512*8 = 4096) and buffer adress
;;	(memory page). That will be a direct disk write procedure.
;;	(for preventing late memory allocation, significant waiting). 
;;	If disk write procedure returns with error or free count of 
;;	swap blocks is ZERO, 'swap_out' procedure will return with
;;	'insufficient memory error' (cf=1). 
;;
;;	(Note: Even if free swap disk/file blocks was not zero,
;;	any disk write error will not be fixed by 'swap_out' procedure,
;;	in other words, 'swap_out' will not check the table for other
;;	free blocks after a disk write error. It will return to 
;;	the caller with error (CF=1) which means swapping is failed. 
;;
;;	After writing the page on to swap disk/file address/sector,
;;	'swap_out' procesure returns with that swap (offset) sector
;;	address (cf=0). 
;;
;;	..............................................................
;;
;;	'swap_in' procedure loads addressed (relevant) swap disk or
;;	file sectors at specified memory page. Then page allocation
;;	procedure updates relevant page table entry with 'present' 
;;	attribute. If swap disk or file reading fails there is nothing
;;	to do, except to terminate the process which is the owner of
;;	the swapped page.
;;
;;	'swap_in' procedure sets the relevant/respective bit value
;;	in the Swap Allocation Table (as free block). 'swap_in' also
;;	updates [swpd_first] pointer if it is required.
;;
;;	..............................................................	 
;;
;;	Note: If [swap_enabled] value is ZERO, that means there is not
;;	a swap disk or swap file in use... 'swap_in' and 'swap_out'
;;	procedures ans 'swap page que' procedures will not be active...
;;	'Insufficient memory' error will be returned by 'swap_out'
;;	and 'general protection fault' will be returned by 'swap_in'
;;	procedure, if it is called mistakenly (a wrong value in a PTE).		
;;

; 17/04/2021
; ('swap_in' procedure call is disabled as temporary)

swap_in:
	; 31/08/2015
	; 20/07/2015
	; 28/04/2015
	; 18/04/2015
	; 24/10/2014 (Retro UNIX 386 v1 - beginning)
	;
	; INPUT -> 
	;	EBX = PHYSICAL (real/flat) ADDRESS OF THE MEMORY PAGE
	;	EBP = VIRTUAL (LINEAR) ADDRESS (page fault address)
	;	EAX = Offset Address for the swapped page on the
	;	      swap disk or in the swap file.
	;
	; OUTPUT ->
	;	EAX = 0 if loading at memory has been successful
	;
	;	CF = 1 -> swap disk reading error (disk/file not present
	;		  or sector not present or drive not ready
	;	     EAX = Error code
	;	     [u.error] = EAX 
	;		       = The last error code for the process
	;		         (will be reset after returning to user)	  
	;
	; Modified Registers -> EAX
	;

;       cmp     dword [swp_drv], 0
;	jna	short swpin_dnp_err
;
;	cmp	eax, [swpd_size]
;	jnb	short swpin_snp_err
;
;	push	esi
;	push	ebx
;	push	ecx
;	mov	esi, [swp_drv]	
;	mov	ecx, PAGE_SIZE / LOGIC_SECT_SIZE  ; 8 !
;		; Note: Even if corresponding physical disk's sector 
;		; size different than 512 bytes, logical disk sector
;		; size is 512 bytes and disk reading procedure
;		; will be performed for reading 4096 bytes
;		; (2*2048, 8*512). 
;	; ESI = Logical disk description table address
;	; EBX = Memory page (buffer) address (physical!)
;	; EAX = Sector adress (offset address, logical sector number)
;	; ECX = Sector count ; 8 sectors
;	push	eax
;	call	logical_disk_read
;	pop	eax
;	jnc	short swpin_read_ok
;	;
;	mov	eax, SWP_DISK_READ_ERR ; drive not ready or read error
;	mov	[u.error], eax
;	jmp	short swpin_retn
;	;
;swpin_read_ok:
;	; EAX = Offset address (logical sector number)
;	call	unlink_swap_block  ; Deallocate swap block	
;	;
;	; EBX = Memory page (buffer) address (physical!)
;	; 20/07/2015
;	mov	ebx, ebp ; virtual address (page fault address)
;       and     bx, ~PAGE_OFF ; ~0FFFh ; reset bits, 0 to 11
;	mov	bl, [u.uno] ; current process number
;	; EBX = Virtual (Linear) address & process number combination
;	call	swap_queue_shift
;	; eax = 0 ; 10/06/2016 (if ebx input > 0, eax output = 0)
;	;sub	eax, eax  ; 0 ; Error Code = 0  (no error)
;	; zf = 1
;swpin_retn:
;	pop	ecx
;	pop	ebx
;	pop	esi
;	retn
;
;swpin_dnp_err:
;	mov	eax, SWP_DISK_NOT_PRESENT_ERR
;swpin_err_retn:
;	mov	[u.error], eax
;	stc
;	retn
;
;swpin_snp_err:
;	mov	eax, SWP_SECTOR_NOT_PRESENT_ERR
;	jmp	short swpin_err_retn

; 17/04/2021
; ('swap_out' procedure call is disabled as temporary)

swap_out:
	; 10/06/2016
	; 07/06/2016
        ; 23/05/2016
	; 19/05/2016 - TRDOS 386 (TRDOS v2.0)
	; 24/10/2014 - 31/08/2015 (Retro UNIX 386 v1)
	;
	; INPUT -> 
	;	none
	;
	; OUTPUT ->
	;	EAX = Physical page address (which is swapped out
	;	      for allocating a new page)
	;	CF = 1 -> swap disk writing error (disk/file not present
	;		  or sector not present or drive not ready
	;	     EAX = Error code
	;	     [u.error] = EAX 
	;		       = The last error code for the process
	;		         (will be reset after returning to user)	  
	;
	; Modified Registers -> none (except EAX)
	;

;	cmp 	word [swpq_count], 1
;       jc      swpout_im_err ; 'insufficient memory'
;
;       ;cmp    dword [swp_drv], 1
;	;jc	short swpout_dnp_err ; 'swap disk/file not present'
;
;       cmp     dword [swpd_free], 1
;       jc      swpout_nfspc_err ; 'no free space on swap disk'
;
;	push	ebx ; *
;swpout_1:
;	; 10/06/2016
;	xor	ebx, ebx ; shift the queue and return a PTE value
;	call	swap_queue_shift
;	and	eax, eax	; 0 = empty queue (improper entries)
;       jz      swpout_npts_err        ; There is not any proper PTE
;				       ; pointer in the swap queue
;	; EAX = PTE value of the page
;	; EBX = PTE address of the page
;	and	ax, PTE_A_CLEAR ; 0F000h ; clear attribute bits
;	;
;	; 07/06/2016
;	; 19/05/2016
;	; check this page is in timer events or not
;	
;swpout_timer_page_0:
;	push	edx ; **
;
;	; 07/06/2016
;	cmp	byte [timer_events], 0 
;	jna	short swpout_2
;	;
;	mov	dl, [timer_events]
;
;	push	ecx ; ***
;	push	ebx ; ****
;	mov	ebx, timer_set ; beginning address of timer event
;			       ; structures 
;swpout_timer_page_1:
;	mov	cl, [ebx]
;	or	cl, cl ; 0 = free, >0 = process number
;	jz	short swpout_timer_page_3
;	mov	ecx, [ebx+12] ; response (signal return) address
;	and	cx, PTE_A_CLEAR ; clear offset part (right 12 bits)
;				; of the response byte address, to
;				; get beginning of the page address)
;	cmp	eax, ecx
;	jne	short swpout_timer_page_2 ; not same page
;	
;	; !same page!
;	;
;	; NOTE: // 19/05/2016 // - TRDOS 386 feature only ! -
;	; This page will be used by the kernel to put timer event
;	; response (signal return) byte at the requested address;
;	; in order to prevent a possible wrong write (while
;	; this page is swapped out) on physical memory,
;	; we must protect this page against to be swapped out!
;	;
;	pop	ebx ; ****
;	pop	ecx ; ***
;	pop	edx ; **
;	jmp	short swpout_1	; do not swap out this page !
; 
;swpout_timer_page_2:
;	; 07/06/2016
;	dec	dl
;	jz	short swpout_timer_page_4
;swpout_timer_page_3:
;	;cmp	ebx, timer_set + 240 ; last timer event (15*16) 
;	;jnb	short swpout_timer_page_4
;	add	ebx, 16
;	jmp	short swpout_timer_page_1	
;
;swpout_timer_page_4:
;	pop	ebx ; ****
;	pop	ecx ; ***
;swpout_2:
;	mov	edx, ebx	       ; Page table entry address	
;	mov	ebx, eax	       ; Buffer (Page) Address				
;	;
;	call	link_swap_block
;	jnc	short swpout_3	       ; It may not be needed here	
;				       ; because [swpd_free] value
;				       ; was checked at the beginging. 	
;	pop	edx ; **
;	pop	ebx ; *
;	jmp	short swpout_nfspc_err 
;swpout_3:
;	test	eax, 80000000h ; test bit 31 (this may not be needed!)
;	jnz	short swpout_nfspc_err  ; 10/06/2016 (bit 31 = 1 !)
;	;	
;	push	esi ; **
;	push	ecx ; ***
;	push	eax ; sector address ; (31 bit !, bit 31 = 0)
;	mov	esi, [swp_drv]	
;	mov	ecx, PAGE_SIZE / LOGIC_SECT_SIZE  ; 8 !
;		; Note: Even if corresponding physical disk's sector 
;		; size different than 512 bytes, logical disk sector
;		; size is 512 bytes and disk writing procedure
;		; will be performed for writing 4096 bytes
;		; (2*2048, 8*512). 
;	; ESI = Logical disk description table address
;	; EBX = Buffer (Page) address
;	; EAX = Sector adress (offset address, logical sector number)
;	; ECX = Sector count ; 8 sectors
;	; edx = PTE address
;	call	logical_disk_write
;	; edx = PTE address
;	pop	ecx ; sector address	
;	jnc	short swpout_write_ok
;	;
;	;; call	unlink_swap_block ; this block must be left as 'in use'
;swpout_dw_err:
;	mov	eax, SWP_DISK_WRITE_ERR ; drive not ready or write error
;	mov	[u.error], eax
;	jmp	short swpout_retn
;	;
;swpout_write_ok:
;	; EBX = Buffer (page) address
;	; EDX = Page Table Entry address
;	; ECX = Swap disk sector (file block) address (31 bit)
;	shl 	ecx, 1  ; 31 bit sector address from bit 1 to bit 31 
;	mov 	[edx], ecx 
;		; bit 0 = 0 (swapped page)
;	mov	eax, ebx
;swpout_retn:
;	pop	ecx ; ***
;	pop	esi ; **
;	pop	ebx ; *
;	retn
;
;;swpout_dnp_err:
;;	mov	eax, SWP_DISK_NOT_PRESENT_ERR ; disk not present
;;	jmp	short swpout_err_retn
;swpout_nfspc_err:
;	mov	eax, SWP_NO_FREE_SPACE_ERR ; no free space
;swpout_err_retn:
;	mov	[u.error], eax
;	;stc
;	retn
;swpout_npts_err:
;	mov	eax, SWP_NO_PAGE_TO_SWAP_ERR
;	pop	ebx
;	jmp	short swpout_err_retn
;swpout_im_err:
;	mov	eax, ERR_MINOR_IM ; insufficient (out of) memory
;	jmp	short swpout_err_retn

; 17/04/2021
; ('swap_queue_shift' procedure call is disabled as temporary)

swap_queue_shift:
	; 26/03/2017
	; 10/06/2016
	; 09/06/2016 - TRDOS 386 (TRDOS v2.0)
	; 23/10/2014 - 20/07/2015 (Retro UNIX 386 v1)
	;
	; INPUT ->
	;	EBX = Virtual (linear) address (bit 12 to 31) 
	;	      and process number combination (bit 0 to 11)
	;	EBX = 0 -> shift/drop from the head (offset 0)
	;	
	; OUTPUT ->
	;	If EBX input > 0 
	;	   the queue will be shifted 4 bytes (dword),
	; 	   from the tail to the head, up to entry offset
	; 	   which points to EBX input value or nothing
	;	   to do if EBX value is not found on the queue.
	;	   (The entry -with EBX value- will be removed
	;	   from the queue if it is found.)
	;
	;	   EAX = 0		
	;
	;	If EBX input = 0
	;	   the queue will be shifted 4 bytes (dword),
	; 	   from the tail to the head, if the PTE address
	;	   which is pointed in head of the queue is marked
	;	   as "accessed" or it is marked as "non present".
	;	   (If "accessed" flag of the PTE -which is pointed
	;	   in the head- is set -to 1-, it will be reset
	;	   -to 0- and then, the queue will be rotated 
	;	   -without dropping pointer of the PTE from 
	;	   the queue- for 4 bytes on head to tail direction.
	;	   Pointer in the head will be moved into the tail,
	;	   other PTEs will be shifted on head direction.)
	;
	;	   Swap queue will be shifted up to the first
	;	   'present' or 'non accessed' page will be found
	;	   (as pointed) on the queue head (then it will be
        ;          removed/dropped from the queue).
	;
	;	   EAX (> 0) = PTE value of the page which is
	;		 (it's pointer -virtual address-) dropped
	;		 (removed) from swap queue.
	;	   EBX = PTE address of the page (if EAX > 0)
	;	         which is (it's pointer -virtual address-)
	;		 dropped (removed) from swap queue.
	;
	;	   EAX = 0 -> empty swap queue ! 
	;
	; Modified Registers -> EAX, EBX
	;
;	movzx   eax, word [swpq_count]  ; Max. 1024
;	and	ax, ax
;	jz	short swpqs_retn
;	push	edi
;	push	esi
;	push	ecx
;	mov	esi, swap_queue
;	mov	ecx, eax
;	or	ebx, ebx
;	jz	short swpqs_7
;swpqs_1:
;	lodsd
;	cmp	eax, ebx
;	je	short swpqs_2
;	loop	swpqs_1
;	; 10/06/2016
;	sub	eax, eax 
;	jmp	short swpqs_6
;swpqs_2:
;	mov	edi, esi
;	sub 	edi, 4
;swpqs_3:
;	dec	word [swpq_count]
;	jz	short swpqs_5
;swpqs_4:
;	dec 	ecx
;	rep	movsd	; shift up (to the head)
;swpqs_5:
;	xor	eax, eax
;	mov	[edi], eax
;swpqs_6:
;	pop	ecx
;	pop	esi
;	pop	edi
;swpqs_retn:
;	retn		
;swpqs_7:
;	mov	edi, esi ; head
;	lodsd
;	; 20/07/2015
;	mov	ebx, eax
;	and	ebx, ~PAGE_OFF ; ~0FFFh 
;		      ; ebx = virtual address (at page boundary)	
;	and	eax, PAGE_OFF ; 0FFFh
;		      ; ax = process number (1 to 4095)
;	cmp	al, [u.uno]
;		; Max. 16 (nproc) processes for Retro UNIX 386 v1
;	jne	short swpqs_8
;	mov	eax, [u.pgdir]
;	jmp	short swpqs_9
;swpqs_8:
;	; 09/06/2016
;	cmp	byte [eax+p.stat-1], 0
;	jna	short swpqs_3     ; free (or terminated) process
;	cmp	byte [eax+p.stat-1], 2 ; waiting
;	ja	short swpqs_3 	  ; zombie (3) or undefined ?	
;
;	;shl	ax, 2
;	shl	al, 2
;	mov 	eax, [eax+p.upage-4]
;	or	eax, eax
;	jz	short swpqs_3 ; invalid upage
;	add	eax, u.pgdir - user
;			 ; u.pgdir value for the process
;			 ; is in [eax]
;	mov	eax, [eax]
;	and	eax, eax
;	jz	short swpqs_3 ; invalid page directory
;swpqs_9:
;	push	edx
;	; eax = page directory
;	; ebx = virtual address
;	call	get_pte
;	mov	ebx, edx	; PTE address
;	pop	edx
;	; 10/06/2016
;	jc	short swpqs_13 ; empty PDE
;	; EAX = PTE value
;	test	al, PTE_A_PRESENT ; bit 0 = 1
;	jz	short swpqs_13  ; Drop non-present page
;			        ; from the queue (head)
;	test	al, PTE_A_WRITE	; bit 1 = 0 (read only)
;	jz	short swpqs_13  ; Drop read only page
;			        ; from the queue (head) 	
;	;test	al, PTE_A_ACCESS ; bit 5 = 1 (Accessed)
;	;jnz	short swpqs_11  ; present
;			        ; accessed page
;       btr     eax, PTE_A_ACCESS_BIT ; reset 'accessed' bit
;	jc	short swpqs_11  ; accessed page
;
;	dec	ecx
;	mov	[swpq_count], cx
;       jz      short swpqs_10
;		; esi = head + 4
;		; edi = head
;	rep	movsd	 ; n = 1 to k-1, [n - 1] = [n]
;swpqs_10:
;	mov	[edi], ecx ; 0
;	jmp	short swpqs_6 ; 26/03/2017
;
;swpqs_11:
;	mov	[ebx], eax     ; save changed attribute
;	; Rotation (head -> tail)
;	dec	ecx     ; entry count -> last entry number		
;	jz	short swpqs_10
;		; esi = head + 4
;		; edi = head
;	mov	eax, [edi] ; 20/07/2015
;	rep	movsd	 ; n = 1 to k-1, [n - 1] = [n]
;	mov	[edi], eax ; head -> tail ; [k] = [1]
;
;	mov	cx, [swpq_count]
;
;swpqs_12:
;	mov	esi, swap_queue ; head
;       jmp     swpqs_7
;
;swpqs_13:
;	dec	ecx
;	mov	[swpq_count], cx
;       jz      swpqs_5
;	jmp	short swpqs_12

; 17/04/2021
; ('add_to_swp_queue' procedure call is disabled as temporary)

add_to_swap_queue:
	; 20/02/2017
	; 20/07/2015
	; 24/10/2014 (Retro UNIX 386 v1 - beginning)
	;
	; Adds new page to swap queue
	; (page directories and page tables must not be added
	; to swap queue)	
	;
	; INPUT ->
	;	EBX = Linear (Virtual) addr for current process
	;	[u.uno]
	;	20/02/2017
	;	(Linear address = CORE + user's virtual address)
	;
	; OUTPUT ->
	;	EAX = [swpq_count]
	;	      (after the PTE has been added)
	;	EAX = 0 -> Swap queue is full, (1024 entries)
	;	      the PTE could not be added.
	;
	; Modified Registers -> EAX
	;
;	push	ebx
;       and     bx, ~PAGE_OFF ; ~0FFFh ; reset bits, 0 to 11
;	mov	bl, [u.uno] ; current process number
;	call	swap_queue_shift ; drop from the queue if
;				 ; it is already on the queue
;		; then add it to the tail of the queue
;	movzx	eax, word [swpq_count]
;	cmp	ax, 1024
;	jb	short atsq_1
;	sub	ax, ax
;	pop	ebx
;	retn
;atsq_1:
;	push	esi
;	mov	esi, swap_queue
;	and	ax, ax
;	jz	short atsq_2
;	shl	ax, 2	; convert to offset
;	add	esi, eax
;	shr	ax, 2
;atsq_2:
;	inc	ax
;	mov	[esi], ebx ; Virtual address + [u.uno] combination
;	mov	[swpq_count], ax
;	pop	esi
;	pop	ebx
;	retn

; 17/04/2021
; ('unlink_swap_block' procedure call is disabled as temporary)

unlink_swap_block:
	; 15/09/2015
	; 30/04/2015
	; 18/04/2015
	; 24/10/2014 (Retro UNIX 386 v1 - beginning)
	;
	; INPUT -> 
	;	EAX = swap disk/file offset address
	;	      (bit 1 to bit 31)
	; OUTPUT ->
	;	[swpd_free] is increased
	;	(corresponding SWAP DISK ALLOC. TABLE bit is SET)
	;
	; Modified Registers -> EAX
	;
;	push	ebx
;	push	edx
;	;
;	shr	eax, SECTOR_SHIFT+1  ;3+1 ; shift sector address to 
;				     ; 3 bits right
;				     ; to get swap block/page number
;	mov	edx, eax
;	; 15/09/2015
;	shr	edx, 3		     ; to get offset to S.A.T.
;				     ; (1 allocation bit = 1 page)
;				     ; (1 allocation bytes = 8 pages)
;	and	dl, 0FCh 	     ; clear lower 2 bits
;				     ; (to get 32 bit position)			
;	;
;	mov	ebx, swap_alloc_table ; Swap Allocation Table address
;	add	ebx, edx
;	and	eax, 1Fh	     ; lower 5 bits only
;				     ; (allocation bit position)	 
;	cmp 	eax, [swpd_next]     ; is the new free block addr. lower
;				     ; than the address in 'swpd_next' ?
;				     ; (next/first free block value)		
;	jnb	short uswpbl_1	     ; no	
;	mov	[swpd_next], eax     ; yes	
;uswpbl_1:
;	bts	[ebx], eax	     ; unlink/release/deallocate block
;				     ; set relevant bit to 1.
;				     ; set CF to the previous bit value	
;	cmc			     ; complement carry flag	
;	jc	short uswpbl_2	     ; do not increase swfd_free count
;				     ; if the block is already deallocated
;				     ; before.	
;       inc     dword [swpd_free]
;uswpbl_2:
;	pop	edx
;	pop	ebx
;	retn

; 17/04/2021
; ('ink_swap_block' procedure call is disabled as temporary)

link_swap_block:
	; 01/07/2015
	; 18/04/2015
	; 24/10/2014 (Retro UNIX 386 v1 - beginning)
	;
	; INPUT -> none
	;
	; OUTPUT ->
	;	EAX = OFFSET ADDRESS OF THE ALLOCATED BLOCK (4096 bytes)
	;	      in sectors (corresponding 
	;	      SWAP DISK ALLOCATION TABLE bit is RESET)
	;
	;	CF = 1 and EAX = 0 
	; 		   if there is not a free block to be allocated	
	;
	; Modified Registers -> none (except EAX)
	;

;	;mov	eax, [swpd_free]
;	;and	eax, eax
;	;jz	short out_of_swpspc
;	;
;	push	ebx
;	push	ecx
;	;
;	mov	ebx, swap_alloc_table ; Swap Allocation Table offset
;	mov	ecx, ebx
;	add	ebx, [swpd_next] ; Free block searching starts from here
;				 ; next_free_swap_block >> 5
;	add	ecx, [swpd_last] ; Free block searching ends here
;				 ; (total_swap_blocks - 1) >> 5
;lswbl_scan:
;	cmp	ebx, ecx
;	ja	short lswbl_notfound
;	;
;	bsf	eax, [ebx] ; Scans source operand for first bit set (1).
;			   ; Clears ZF if a bit is found set (1) and 
;			   ; loads the destination with an index to
;			   ; first set bit. (0 -> 31) 
;			   ; Sets ZF to 1 if no bits are found set.
;	; 01/07/2015
;	jnz	short lswbl_found ; ZF = 0 -> a free block has been found
;			 ;
;			 ; NOTE:  a Swap Disk Allocation Table bit 
;			 ;	  with value of 1 means 
;			 ;	  the corresponding page is free 
;			 ;	  (Retro UNIX 386 v1 feaure only!)
;	add	ebx, 4
;			 ; We return back for searching next page block
;			 ; NOTE: [swpd_free] is not ZERO; so, 
;			 ;	 we always will find at least 1 free block here.
;	jmp    	short lswbl_scan
;	;
;lswbl_notfound:	
;	sub	ecx, swap_alloc_table
;	mov	[swpd_next], ecx ; next/first free page = last page 
;				 ; (unlink_swap_block procedure will change it)
;	xor	eax, eax
;	mov	[swpd_free], eax
;	stc
;lswbl_ok:
;	pop	ecx
;	pop	ebx
;	retn
;	;
;;out_of_swpspc:
;;	stc
;;	retn
;
;lswbl_found:
;	mov	ecx, ebx
;	sub	ecx, swap_alloc_table
;	mov	[swpd_next], ecx ; Set first free block searching start
;				 ; address/offset (to the next)
;       dec     dword [swpd_free] ; 1 block has been allocated (X = X-1) 
;	;
;	btr	[ebx], eax	 ; The destination bit indexed by the source value
;				 ; is copied into the Carry Flag and then cleared
;				 ; in the destination.
;				 ;
;				 ; Reset the bit which is corresponding to the 
;				 ; (just) allocated block.
;	shl	ecx, 5		 ; (block offset * 32) + block index
;	add	eax, ecx	 ; = block number
;	shl	eax, SECTOR_SHIFT ; 3, sector (offset) address of the block
;				 ; 1 block =  8 sectors
;	;
;	; EAX = offset address of swap disk/file sector (beginning of the block)
;	;
;	; NOTE: The relevant page table entry will be updated
;	;       according to this EAX value...
;	;
;	jmp	short lswbl_ok

; 17/04/2021
; ('logical_disk_read' procedure call is disabled as temporary)

logical_disk_read:
	; 20/07/2015
	; 09/03/2015 (temporary code here)
	;
	; INPUT ->
	; 	ESI = Logical disk description table address
	; 	EBX = Memory page (buffer) address (physical!)
	; 	EAX = Sector adress (offset address, logical sector number)
	; 	ECX = Sector count
	;
	;
;	retn

; 17/04/2021
; ('logical_disk_write' procedure call is disabled as temporary)

logical_disk_write:
	; 20/07/2015
	; 09/03/2015 (temporary code here)
	;
	; INPUT ->
	; 	ESI = Logical disk description table address
	; 	EBX = Memory page (buffer) address (physical!)
	; 	EAX = Sector adress (offset address, logical sector number)
	; 	ECX = Sector count
	;
;	retn

get_physical_addr:
	; 17/04/2021 - TRDOS 386 v2.0.4
	;	(temporary modifications)
	;
	; 26/03/2017
	; 20/02/2017
	; 27/05/2016 - TRDOS 386 (TRDOS v2.0)
	; 18/10/2015
	; 29/07/2015
	; 20/07/2015
	; 04/06/2015
	; 20/05/2015
	; 28/04/2015
	; 18/04/2015
	; Get physical address
	;     (allocates a new page for user if it is not present)
	;	
	; (This subroutine is needed for mapping user's virtual 
	; (buffer) address to physical address (of the buffer).)
	; ('sys write', 'sys read' system calls...)
	;
	; INPUT ->
	;	EBX = virtual address
	;	u.pgdir = page directory (physical) address
	;
	; OUTPUT ->
	;	EAX = physical address 
	;	EBX = linear address	
	;	EDX = physical address of the page frame
	;	      (with attribute bits)
	;	ECX = byte count within the page frame
	;
	; Modified Registers -> EAX, EBX, ECX, EDX
	;
	add	ebx, CORE ; 18/10/2015
get_physical_addr_x: ; 27/05/2016
	mov	eax, [u.pgdir]
	call	get_pte
		; EDX = Page table entry address (if CF=0)
	        ;       Page directory entry address (if CF=1)
		;       (Bit 0 value is 0 if PT is not present)
		; EAX = Page table entry value (page address)
		;	CF = 1 -> PDE not present or invalid ? 
	jnc	short gpa_1
	;
	call	allocate_page
	jc	short gpa_im_err  ; 'insufficient memory' error
gpa_0:
	call 	clear_page
	; EAX = Physical (base) address of the allocated (new) page
	or	al, PDE_A_PRESENT + PDE_A_WRITE + PDE_A_USER ; 4+2+1 = 7
			   ; lower 3 bits are used as U/S, R/W, P flags
			   ; (user, writable, present page)	
	mov	[edx], eax ; Let's put the new page directory entry here !
	mov	eax, [u.pgdir]	
	call	get_pte
	jc	short gpa_im_err ; 'insufficient memory' error
gpa_1:
	; EAX = PTE value, EDX = PTE address
	test 	al, PTE_A_PRESENT
	jnz	short gpa_3 ; 26/03/2017
	or	eax, eax
	jz	short gpa_7  ; Allocate a new page

; 17/04/2021 (TRDOS 386 v2.0.4)
; ('reload_page' procedure call is disabled as temporary)
	jmp	short gpa_im_err  ; temporary !

	; 20/07/2015
;	push	ebp
;	mov	ebp, ebx ; virtual (linear) address
;	; reload swapped page
;	call	reload_page ; 28/04/2015
;	pop	ebp
;	jc	short gpa_retn
gpa_2:
	; 26/03/2017
	; 20/02/2017
	; If a page will contain a Signal Response Byte
	; it must not be swapped out, because
	; timer service or irq callback service
	; will write a signal return/response byte 
	; directly by using physical address of Signal
	; Response Byte.(Even if process is not running,
	; or it is running with swapped out pages.)
	;
	; 'no_page_swap' will be set by 'systimer' or
	; 'syscalbac' sistem functions/calls. (*)
	;
	cmp	byte [no_page_swap], 0
	jna	short gpa_4 ; this page can be swapped out
	; this page must not be swapped out
	; but 'no_page_swap' must be reset here
	; immediately for other callers (*)
	; (otherwise, swap queue would not be long enough) 
	call	gpa_8 ; 26/03/2017
	jmp	short gpa_5
gpa_3: 
	; 26/03/2017
	cmp	byte [no_page_swap], 0
	jna	short gpa_6 ; this page can be swapped out
	call	gpa_8
	jmp	short gpa_6

gpa_im_err:	
	mov	eax, ERR_MINOR_IM ; Insufficient memory (minor) error!
				  ; Major error = 0 (No protection fault)	
	retn
gpa_4:
; 17/04/2021 (TRDOS 386 v2.0.4)
; ('add_to_swap_queue' procedure call is disabled as temporary)

	; 20/07/2015
	; 20/05/2015
	; add this page to swap queue
;	push	eax 
;	; EBX = Linear (CORE+virtual) address ; 20/02/2017 
;	call 	add_to_swap_queue
;	pop	eax
gpa_5:
		; PTE address in EDX
		; virtual address in EBX
	; EAX = memory page address
	or	al, PTE_A_PRESENT + PTE_A_USER + PTE_A_WRITE
				  ; present flag, bit 0 = 1
				  ; user flag, bit 2 = 1	
				  ; writable flag, bit 1 = 1
	mov	[edx], eax  ; Update PTE value
gpa_6:
	; 18/10/2015
	mov	ecx, ebx
	and	ecx, PAGE_OFF
	mov 	edx, eax
	and	ax, PTE_A_CLEAR
	add	eax, ecx
	neg	ecx ; 1 -> -1 (0FFFFFFFFh), 4095 (0FFFh) -> -4095
	add	ecx, PAGE_SIZE
	clc
gpa_retn:
	retn
gpa_7:	
	call	allocate_page
	jc	short gpa_im_err ; 'insufficient memory' error
	call	clear_page
	jmp	short gpa_2

gpa_8: ; 26/03/2017
	mov	byte [no_page_swap], 0

	retn	; 17/04/2021 (temporary)

; 17/04/2021 (TRDOS 386 v2.0.4)
; ('swap_queue_shift' procedure call is disabled as temporary)
;	
;	push	ebx
;	push	eax ; 26/03/2017
;       and     bx, ~PAGE_OFF ; ~0FFFh ; reset bits, 0 to 11
;	mov	bl, [u.uno] ; current process number
;	call	swap_queue_shift ; drop from the queue if
;				 ; it is already on the queue
;	pop	eax ; 26/03/2017
;	pop	ebx
;
;	retn

; 17/04/2021
; ('reload_page' procedure call is disabled as temporary)

reload_page:
	; 20/07/2015
	; 28/04/2015 (Retro UNIX 386 v1 - beginning)
	;
	; Reload (Restore) swapped page at memory
	;
	; INPUT -> 
	;	EBP = Virtual (linear) memory address
	;	EAX = PTE value (swap disk sector address)
	;	(Swap disk sector address = bit 1 to bit 31 of EAX)	
	; OUTPUT ->
	;	EAX = PHYSICAL (real/flat) ADDRESS OF RELOADED PAGE
	;
	;	CF = 1 and EAX = error code
	;
	; Modified Registers -> none (except EAX)
	;
;	shr	eax, 1   ; Convert PTE value to swap disk address 
;	push	ebx      ;
;	mov	ebx, eax ; Swap disk (offset) address	
;	call	allocate_page
;	jc	short rlp_im_err
;	xchg 	eax, ebx	
;	; EBX = Physical memory (page) address
;	; EAX = Swap disk (offset) address
;	; EBP = Virtual (linear) memory address
;	call	swap_in
;	jc	short rlp_swp_err  ; (swap disk/file read error)
;	mov	eax, ebx	
;rlp_retn:
;	pop	ebx
;	retn
;	
;rlp_im_err:	
;	mov	eax, ERR_MINOR_IM ; Insufficient memory (minor) error!
;				  ; Major error = 0 (No protection fault)	
;	jmp	short rlp_retn
;
;rlp_swp_err:
;	mov 	eax, SWP_DISK_READ_ERR ; Swap disk read error !
;	jmp	short rlp_retn

copy_page_dir:
	; 17/04/2021 (temporary modifications)
	; 19/09/2015
	; temporary - 07/09/2015
	; 07/09/2015 (Retro UNIX 386 v1 - beginning)
	;
	; INPUT -> 
	;	[u.pgdir] = PHYSICAL (real/flat) ADDRESS of the parent's
	;		    page directory.
	; OUTPUT ->
	;	EAX =  PHYSICAL (real/flat) ADDRESS of the child's
	;	       page directory.
	;	(New page directory with new page table entries.)
	;	(New page tables with read only copies of the parent's
	;	pages.)
	;	EAX = 0 -> Error (CF = 1)
	;
	; Modified Registers -> none (except EAX)
	;
	call	allocate_page
	jc	short cpd_err
	;
	push	ebp ; 20/07/2015
	push	esi
	push	edi
	push	ebx
	push	ecx
	mov	esi, [u.pgdir]
	mov	edi, eax
	push	eax ; save child's page directory address
	; copy PDE 0 from the parent's page dir to the child's page dir
	; (use same system space for all user page tables) 
	movsd
	mov	ebp, 1024*4096 ; pass the 1st 4MB (system space)
	mov	ecx, (PAGE_SIZE / 4) - 1 ; 1023
cpd_0:	
	lodsd
	;or	eax, eax
        ;jnz     short cpd_1
	test	al, PDE_A_PRESENT ;  bit 0 =  1
	jnz	short cpd_1
 	; (virtual address at the end of the page table)	
	add	ebp, 1024*4096 ; page size * PTE count
	jmp	short cpd_2
cpd_1:	
	and	ax, PDE_A_CLEAR ; 0F000h ; clear attribute bits
	mov	ebx, eax
	; EBX = Parent's page table address
	call	copy_page_table
	jc	short cpd_p_err
	; EAX = Child's page table address
	or	al, PDE_A_PRESENT + PDE_A_WRITE + PDE_A_USER
			 ; set bit 0, bit 1 and bit 2 to 1
			 ; (present, writable, user)
cpd_2:
	stosd
	loop	cpd_0
	;
	pop	eax  ; restore child's page directory address
cpd_3:
	pop	ecx
	pop	ebx
	pop	edi
	pop	esi
	pop	ebp
cpd_err:
	retn
cpd_p_err:
	; release the allocated pages missing (recover free space)
	pop	eax  ; the new page directory address (physical)
	mov	ebx, [u.pgdir] ; parent's page directory address 
	call 	deallocate_page_dir
	sub	eax, eax ; 0
	stc
	jmp	short cpd_3	

copy_page_table:
	; 17/04/2021 (temporary modifications)
	; 19/09/2015
	; temporary - 07/09/2015
	; 07/09/2015 (Retro UNIX 386 v1 - beginning)
	;
	; INPUT -> 
	;	EBX = PHYSICAL (real/flat) ADDRESS of the parent's page table.
	;	EBP = page table entry index (from 'copy_page_dir')
	; OUTPUT ->
	;	EAX = PHYSICAL (real/flat) ADDRESS of the child's page table.
	;	EBP = (recent) page table index (for 'add_to_swap_queue')	
	;	CF = 1 -> error 
	;
	; Modified Registers -> EBP (except EAX)
	;
	call	allocate_page
	jc	short cpt_err
	;
	push	eax ; *
	;push 	ebx
	push	esi
	push	edi
	push	edx
	push	ecx
	;
	mov	esi, ebx
	mov	edi, eax
	mov	edx, eax
	add	edx, PAGE_SIZE 	
cpt_0:
	lodsd
	test	al, PTE_A_PRESENT ;  bit 0 = 1
	;jnz	short cpt_1 (*)
	; 17/04/2021 (temporary (*)
	;and	eax, eax (*)
	jz	short cpt_2  ; 17/04/2021
	
; 17/04/2021
; ('reload_page' procedure call is disabled as temporary)
;
;	; ebp = virtual (linear) address of the memory page
;	call	reload_page ; 28/04/2015
;	jc	short cpt_p_err
cpt_1:
	and	ax, PTE_A_CLEAR ; 0F000h ; clear attribute bits
	mov	ecx, eax
	; Allocate a new page for the child process
	call	allocate_page
	jc	short cpt_p_err
	push	edi
	push	esi
	mov	esi, ecx
	mov	edi, eax
	mov	ecx, PAGE_SIZE/4
	rep	movsd	; copy page (4096 bytes)
	pop	esi
	pop	edi
	; 

; 17/04/2021
; ('add_to_swap_queue' procedure call is disabled as temporary)	
;
;	push	ebx
;	push	eax
;	mov	ebx, ebp
;	; ebx = virtual address of the memory page
;	call	add_to_swap_queue
;	pop	eax
;	pop	ebx
	;
	;or	ax, PTE_A_USER+PTE_A_PRESENT 
	or	al, PTE_A_USER+PTE_A_WRITE+PTE_A_PRESENT 
cpt_2:
	stosd  ; EDI points to child's PTE  	 
	;
	add	ebp, 4096 ; 20/07/2015 (next page)
	;
	cmp	edi, edx
	jb	short cpt_0
cpt_p_err:
	pop	ecx
	pop	edx
	pop	edi
	pop	esi
	;pop	ebx
	pop	eax ; *
cpt_err:
	retn

allocate_memory_block:
	; 01/05/2017
	; 28/04/2017
	; 25/04/2017
	; 01/04/2016, 02/04/2016, 03/04/2016
	; 13/03/2016, 14/03/2016
	; 12/03/2016 (TRDOS 386 = TRDOS v2.0)
	; Allocating contiguous memory pages (in the kernel's memory space)
	;
	; INPUT -> 
	;	EAX = Beginning address (physical)
	;	EAX = 0 -> Allocate memory block from the first proper aperture	
	;	ECX = Number of bytes to be allocated
	;
	; OUTPUT ->
	; 	1) cf = 0 -> successful
	;	EAX = Beginning (physical) address of the allocated memory block
	;	ECX = Number of allocated bytes (rounded up to page borders) 
	;	2) cf = 1 -> unsuccessful
	;	 2.1) If EAX > 0 -> 
	;	      (Number of requested pages is more than # of free pages
	;	       but contiguous free pages -the aperture- is not enough!)	   	
	;	      EAX = Beginning address of available aperture
	;		    (one of all aperture with max. aperture size/length)		
	;	      ECX = Size of available aperture (memory block) in bytes
	;	 2.2) If EAX = 0 -> Out of memory error 
	;	            (number of free pages is less than requested number)
	;	      ECX = Total number of free bytes (free pages * 4096) 
	;		    (It is not number of contiguous free bytes)	
	;
	; (Modified Registers -> EAX, ECX)
	;
	; PURPOSE: Loading a file at memory for copying or running etc.
	; If this procedure returns with cf is set, ECX contains maximum
	; available space and EAX contains the beginning address of it.
	; If EAX has zero, ECX contains total number of free bytes.
	; If requested block has been successfully allocated (by rounding up to
	; the last page border), it must be deallocated later by using
	; 'deallocate_memory_block' procedure.    

	push	edx ; *
	mov	edx, PAGE_SIZE - 1   ; 4095
	add	eax, edx
	add	ecx, edx
	shr	ecx, PAGE_SHIFT	     ; 12

	; ECX = number of contiguous pages to be allocated
	mov	edx, [free_pages]
	; 01/05/2017
	;or	ecx, ecx
	;jz	short amb3
	; If ECX=0, set cf to 1 and return with max. available mem block size

	cmp	ecx, edx
	ja	short amb_3

	shr	eax, PAGE_SHIFT      ; 12

	mov	edx, eax 	     ; page number
	shr	edx, 3		     ; to get offset to M.A.T.
				     ; (1 allocation bit = 1 page)
				     ; (1 allocation bytes = 8 pages)
	and	dl, 0FCh 	     ; clear lower 2 bits
				     ; (to get 32 bit position)	
	push	ebx ; **
amb_0:
	mov	[mem_ipg_count], ecx ; initial (reset) value of page count
	mov	[mem_pg_count], ecx
	xor	ecx, ecx ; 0
	mov	[mem_aperture], ecx ; 0
	mov	[mem_max_aperture], ecx ; 0
	
	mov	ebx, MEM_ALLOC_TBL   ; Memory Allocation Table address.
	cmp	edx, [next_page]     ; Is the beginning page address lower
				     ; than the address in 'next_page' ?
				     ; (the first/next free page of user space)		
	jb	short amb_1
	cmp 	edx, [last_page]     ; is the beginning page address higher
				     ; than the address in 'last_page' ?
				     ; (end of the memory)		
	jna	short amb_2	     ; no	
amb_1:
	mov	edx, [next_page]     ; M.A.T. offset (1 M.A.T. byte = 8 pages)
amb_2:
	add	ebx, edx

	; 28/04/2017
	;xor	ecx, ecx
	bsf	ecx, [ebx]	     ; 0 to 31
	mov	eax, edx
	shl	eax, 3		     ; *8	
	add	eax, ecx	     ; beginning page number

	mov	[mem_pg_pos], eax    ; beginning page no (for curr. mem. aperture)
 	mov	[mem_max_pg_pos], eax ; beginning page no for max. mem. aperture

	and	eax, 1Fh	     ; lower 5 bits only (0 to 31)
				     ; (allocation bit position)	 
	jnz	short amb_4	     ; 0
	mov	cl, 32
	jmp	short amb_10

amb_3:	; out_of_memory
	xor	eax, eax ; 0
	mov	ecx, edx ; free pages
	shl	ecx, PAGE_SHIFT
	pop	edx ; *
	stc
	retn	
amb_4:
	mov	edx, [ebx]
	mov	cl, al ; 1 to 31
	shr	edx, cl
	mov	eax, edx
amb_5:
	shr	eax, 1 ; (***)
	jnc	short amb_7
	inc	dword [mem_aperture]
	dec	dword [mem_pg_count]
	jz	short amb_15
amb_6:
	; 28/04/2017
	inc	cl
	cmp	cl, 32
	jnb	short amb_9
	jmp	short amb_5
amb_7:
	push	eax ; (***) allocation bits (in shifted status)
	call	amb_26 ; set maximum memory aperture (free memory block size)
	pop	eax ; (***)
	jmp	short amb_6
amb_8:
	; 28/04/2017
	mov	cl, 32
amb_9:
	mov	edx, ebx
	sub	edx, MEM_ALLOC_TBL
	cmp	edx, [last_page]
	jnb	short amb_14 ; contiguous pages not enough
	add	ebx, 4
amb_10:
	mov	eax, [ebx]
	and 	eax, eax
        jz      short amb_11 ; there is not a free page bit in this alloc dword
	inc	eax ; 0FFFFFFFFh -> 0
	jz	short amb_12 ; all of bits are set (32 free pages)
	dec	eax
	sub	cl, cl ; 0
	jmp	short amb_5
amb_11:
	call	amb_26 ; set maximum memory aperture (free memory block size)
	jmp	short amb_9	
amb_12:
	cmp	[mem_pg_count], ecx ; 32
	jnb	short amb_13
	mov	ecx, [mem_pg_count]
amb_13:
	add	[mem_aperture], ecx
	sub	[mem_pg_count], ecx
	jna	short amb_15
	jmp	short amb_9 ; 01/05/2017
amb_14:
	call	amb_26 ; 28/04/2017
	mov	eax, [mem_max_pg_pos] ; begin address of max. mem aperture	
	mov	ecx, [mem_max_aperture] ; max. (largest) memory aperture
	stc
        jmp     amb_25

amb_15: ; OK !
	mov	eax, [mem_pg_pos]    ; Beginning address as page number
	mov	ecx, [mem_aperture]  ; Free contiguous page count (>=1)
amb_16:
	; allocate contiguous memory pages (via memory allocation table bits)
	mov	edx, eax
	; 25/04/2017
	shr	edx, 3		 ; 8 pages in one allocation byte
	and	dl, 0FCh	 ; clear lower 2 bits
				 ; (for dword/32bit positioning)	

	mov	ebx, MEM_ALLOC_TBL
	add	ebx, edx
	and	eax, 1Fh ; 31
	; 03/04/2016
	mov	edx, 32
	sub	dl, al
	cmp	edx, ecx	 ; ecx >= 1
	jna	short amb_17
	mov	edx, ecx
amb_17:
	sub	ecx, edx
	push	ecx ; ***
	mov	ecx, edx
amb_18:		
	btr	[ebx], eax	 ; The destination bit indexed by the source value
				 ; is copied into the Carry Flag and then cleared
				 ; in the destination.
	dec     dword [free_pages] ; 1 page has been allocated (X = X-1) 
	dec	ecx
	jz	short amb_19
	inc	al
	jmp	short amb_18
amb_19:	
	pop	ecx ; ***
	and	ecx, ecx ; 0 ?
	jz	short amb_22	
	; 01/04/2016
	mov	al, 32
amb_20:
	add	ebx, 4
	cmp	ecx, eax ; 32
	jnb	short amb_21
	; ECX < 32
	sub	al, al ; 0
	push	eax ; 0 ***
	jmp	short amb_18
amb_21:
	sub	[free_pages], eax   ; [free_pages] = [free_pages] - 32
	mov	dword [ebx], 0	    ; reset 32 bits
	sub	ecx, eax ; 32
	jnz	short amb_20
amb_22:
	mov	eax, [mem_pg_pos]   ; Beginning address as page number
	mov	ecx, [mem_aperture] ; Free contiguous page count
	; [next_page] update
	mov	edx, eax
	; 03/04/2016
	shr	edx, 3		     ; to get offset to M.A.T.
				     ; (1 allocation bit = 1 page)
				     ; (1 allocation bytes = 8 pages)
	and	dl, 0FCh 	     ; clear lower 2 bits
				     ; (to get 32 bit position)	
	cmp	edx, [next_page] ; first free page pointer offset
	ja	short amb_25
	mov	ebx, MEM_ALLOC_TBL
	cmp	dword [ebx+edx], 0
	ja	short amb_24
	mov	edx, eax
	add	edx, ecx
	shr	edx, 3
	and	dl, 0FCh
amb_23:
	cmp	dword [ebx+edx], 0
	ja	short amb_24
	add	edx, 4
	cmp	edx, [last_page]    ; last page pointer offset
	jna	short amb_23
	mov	edx, [first_page]   ; (for) beginning of user's space
amb_24:
	mov	[next_page], edx
amb_25:
	pushf
	shl	eax, PAGE_SHIFT	     ; convert to phy. address in bytes
	shl	ecx, PAGE_SHIFT	     ; convert to byte counts
	popf
	pop	ebx ; **
	pop	edx ; *
	retn

amb_26:	; set maximum free memory aperture (free memory block size) 
	mov	edx, ebx ; current address
	sub	edx, MEM_ALLOC_TBL ; MAT beginning address
	; 02/04/2016 
	shl	edx, 3 ; MAT byte offset * 8 = page number base
	add	edx, ecx ; current page number (ecx = 0 to 32)
	;
	mov	eax, [mem_aperture]
	and	eax, eax
        jz      short amb_27
        mov     dword [mem_aperture], 0
	cmp	eax, [mem_max_aperture]
	jna	short amb_27
	mov	[mem_max_aperture], eax
	; 25/04/2017
	mov	eax, [mem_pg_pos]
	; EAX = Beginning page number of the max. aperture 
	mov	[mem_max_pg_pos], eax
amb_27: 
	mov	[mem_pg_pos], edx ; current page

	mov	eax, [mem_ipg_count] ; initial (reset) value of page count
	mov	[mem_pg_count], eax

	retn

deallocate_memory_block:
	; 03/04/2016
	; 14/03/2016 (TRDOS 386 = TRDOS v2.0)
	; Deallocating contiguous memory pages (in the kernel's memory space)
	;
	; INPUT -> 
	;	EAX = Beginning address (physical)
	;	ECX = Number of bytes to be deallocated
	;
	; OUTPUT ->
	;	Memory Allocation Table bits will be updated
	;	[free_pages] will be changed (increased)
	;
	; (Modified Registers -> EAX, ECX)
	;
	; PURPOSE: Unloading/Freeing a file -or an allocated memory block- 
	; at memory after copying, running, saving, reading, writing etc.
	;

	push	edx ; *
	push	ebx ; **

	shr	eax, PAGE_SHIFT	     ; 12
	shr	ecx, PAGE_SHIFT	     ; 12

	; EAX = Beginning page number
	; ECX = Number of contiguous pages to be deallocated
damb_0:
	; deallocate contiguous memory pages (via memory allocation table bits)
	mov	edx, eax
	shr	edx, 3		     ; to get offset to M.A.T.
				     ; (1 allocation bit = 1 page)
				     ; (1 allocation bytes = 8 pages)
	and	dl, 0FCh 	     ; clear lower 2 bits
				     ; (to get 32 bit position)	
	cmp	edx, [next_page] ; next free page
	jnb	short damb_1
	mov	[next_page], edx
damb_1:
	mov	ebx, MEM_ALLOC_TBL
	add	ebx, edx
	and	eax, 1Fh ; 31

	; 03/04/2016
	mov	edx, 32
	sub	dl, al
	cmp	edx, ecx
	jna	short damb_2
	mov	edx, ecx
damb_2:
	sub	ecx, edx
	push	ecx ; ***
	mov	ecx, edx
damb_3:		
	bts	[ebx], eax	     ; unlink/release/deallocate page
				     ; set relevant bit to 1.
				     ; set CF to the previous bit value	
	inc     dword [free_pages]   ; 1 page has been deallocated (X = X+1) 
	dec	ecx
	jz	short damb_4
	inc	al
	jmp	short damb_3
damb_4:	
	pop	ecx ; ***
	and	ecx, ecx ; 0 ?
	jz	short damb_7
	; 03/04/2016
	mov	al, 32
damb_5:
	add	ebx, 4
	cmp	ecx, eax ; 32
	jnb	short damb_6
	; ECX < 32
	sub	al, al ; 0
	push	eax ; 0 ***
	jmp	short damb_3
damb_6:
	add	[free_pages], eax ; [free_pages] = [free_pages] + 32
	mov	dword [ebx], 0FFFFFFFFh ; set 32 bits
	sub	ecx, eax ; 32
	jnz	short damb_5
damb_7:
	pop	ebx ; **
	pop	edx ; *
	retn

direct_memory_access:
	; 17/04/2021 (temporary modifications)
	; 22/07/2017
	; 12/05/2017
	; 16/07/2016
	; 12/07/2016 (TRDOS 386 = TRDOS v2.0)
	; This processure will be called to map
	; user's (ring 3) page tables to access phsical
	; (flat/linear) memory addresses, directly (without
	;  kernel's data transfer functions).
	; 
	; Purpose: Video memory access and shared memory access.
	;
	; INPUT -> 
	;	EAX = Beginning address (physical).
	;	EBX = User's buffer address ; 12/05/2017
	;	ECX = Number of contiguous pages to be mapped.
	; OUTPUT ->
	;	User's page directory and pages tables
	;	will be updated.
	;	
	;	If an old page table entry has valid page address, 
	;	that page will be deallocated just before PTE will
	;	be changed for direct (1 to 1) memory page access.
	;
	;	If old PTE value points to a swapped page,
        ;       that page (block) will be unlinked on swap disk. 
	;
	;	Newly allocated pages (except page tables) will not
	;	be applied to Memory Allocation Table.
	;	AVL bit 1 (PTE bit 10) of page table entry will be
	;	used to indicate shared (direct) memory page; then,
	;	this page will not be deallocated later during
	;	process termination. (Memory Allocation Table and
	;	free memory count will not be affected.
	;	(Except deallocating page table's itself.)
	;	
	;      CF = 1 -> error (EAX = error code)
	;      CF = 0 -> success (EAX = beginning address)
	;
	;; (Modified Registers -> none)
	; Modified registers: ebp, edx, ecx, ebx, esi, edi	
	;

	;push	ebp
	;push	ebx
	;push	ecx
	;push	edx
	and	ax, PTE_A_CLEAR ; clear page offset
	push	eax
	;and	ecx, ecx ; page count
	;jz	dmem_acc_7  ; 'insufficient memory' error
	mov	ebp, eax
	add	ebx, CORE ; 12/05/2017
dmem_acc_0: 
	mov	[base_addr], ebx ; 12/05/2017
	mov	eax, [u.pgdir] ; page dir address (physical)
	call	get_pte
		; EDX = Page table entry address (if CF=0)
	        ;       Page directory entry address (if CF=1)
		;       (Bit 0 value is 0 if PT is not present)
		; EAX = Page table entry value (page address)
		;	CF = 1 -> PDE not present or invalid ? 	
	jnc	short dmem_acc_1
	;
	call	allocate_page
	;jc	dmem_acc_7  ; 'insufficient memory' error
	; 17/04/2021
	jc	short _dmem_acc_7
	;
	call 	clear_page
	; EAX = Physical (base) address of the allocated (new) page
	or	al, PDE_A_PRESENT + PDE_A_WRITE + PDE_A_USER ; 4+2+1 = 7
			   ; lower 3 bits are used as U/S, R/W, P flags
			   ; (user, writable, present page)	
	mov	[edx], eax ; Let's put the new page directory entry here !
	mov	eax, [u.pgdir]	
	call	get_pte
        ;jc	dmem_acc_7 ; 'insufficient memory' error
	; 17/04/2021
	jnc	short dmem_acc_1
_dmem_acc_7:
	jmp	dmem_acc_7
dmem_acc_1:
	; EAX = PTE value, EDX = PTE address
	test 	al, PTE_A_PRESENT
	;jnz	short dmem_acc_2   ; 17/04/2021 (*)
	; 17/04/2021 (temporary)
	jz	short short dmem_acc_6  ; ! temporary ! (*)	

; 17/04/2021
; (following code is disabled as temporary)
;
;	or	eax, eax
;	jz	short dmem_acc_6   ; Change PTE
;	shr	eax, 1		; swap disk block (8 sectors) address
;	; unlink swap disk block
;	call	unlink_swap_block
;	jmp	short dmem_acc_6

dmem_acc_2:
	test	al, PTE_A_WRITE   ; bit 1, writable (r/w) flag
				  ; (must be 1)
	jnz	short dmem_acc_4
	; Read only -duplicated- page (belongs to a parent or a child)
        test    ax, PTE_DUPLICATED ; Was this page duplicated 
				   ; as child's page ?
	jz	short dmem_acc_5 ; Change PTE but don't deallocate the page!

	;push	edi
	;push	esi

	push	ecx
	;push	ebx
	mov	ebx, [u.ppgdir] ; parent's page dir address (physical)
	
	; check the parent's PTE value is read only & same page or not.. 
	mov	edi, ebp
	shr	edi, PAGE_D_SHIFT ; 22
	; EDI = page directory entry index (0-1023)
	mov	esi, ebp
	shr	esi, PAGE_SHIFT ; 12	
	and	esi, PTE_MASK
	; ESI = page table entry index (0-1023)

	shl	di, 2 ; * 4
	add	ebx, edi ; PDE offset (for the parent)
	mov	ecx, [edi]
	test	cl, PDE_A_PRESENT ; present (valid) or not ?
	jz	short dmem_acc_3	; parent process does not use this page
	and	cx, PDE_A_CLEAR ; 0F000h ; Clear attribute bits
	shl	si, 2 ; *4 
	add	esi, ecx ; PTE offset (for the parent)
	mov	ebx, [esi]
	test	bl, PTE_A_PRESENT ; present or not ?
	jz	short dmem_acc_3	; parent process does not use this page
	and	ax, PTE_A_CLEAR ; 0F000h ; Clear attribute bits 
	and	bx, PTE_A_CLEAR ; 0F000h ; Clear attribute bits
	cmp	eax, ebx	; parent's and child's pages are same ?
	jne	short dmem_acc_3	; not same page
				; deallocate the child's page
        or      byte [esi], PTE_A_WRITE ; convert to writable page (parent)
	;pop	ebx
	pop	ecx
	jmp	short dmem_acc_5
dmem_acc_3:
	;pop	ebx
	pop	ecx
dmem_acc_4:	
	test	ax, PTE_SHARED ; shared or direct memory access indicator
	jnz	short dmem_acc_5   ; AVL bit 1 = 1, do not deallocate this page!
	;
	;and	ax, PTE_A_CLEAR ; 0F000h ; clear lower 12 (attribute) bits
	call	deallocate_page
dmem_acc_5:
	;pop	esi
	;pop	edi
dmem_acc_6:
	mov	eax, ebp ; physical page (offset=0) address
	; EAX = memory page address
	; EDX = PTE entry address (physical)
	or	ax, PTE_A_PRESENT+PTE_A_USER+PTE_A_WRITE+PTE_SHARED
			; present flag, bit 0 = 1
			; user flag, bit 2 = 1	
			; writable flag, bit 1 = 1
			; direct memory access flag, bit 10 = 1
			; (This page must not be deallocated!)
	mov	[edx], eax  ; Update PTE value
	dec	ecx ; remain count of contiguous pages
	jz	short dmem_acc_8
	add	ebp, PAGE_SIZE ; next physical page address
	; 22/07/2017
	;mov	eax, ebp
	; 12/05/2017
	mov	ebx, [base_addr] ; linear address (virtual+CORE)
	add	ebx, PAGE_SIZE	; next linear address
        jmp     dmem_acc_0
dmem_acc_7:  ; ERROR ! 
	mov	dword [esp], ERR_MINOR_IM 
		; Insufficient memory (minor) error!
		; Major error = 0 (No protection fault)	
	; cf = 1
dmem_acc_8:
	pop	eax
	;pop	edx
	;pop	ecx
	;pop	ebx
	;pop	ebp
	retn

deallocate_user_pages:
	; 20/05/2017
	; 15/05/2017
	; 20/02/2017
	; 19/02/2017 (TRDOS 386 = TRDOS v2.0)
	;
	; Deallocate virtually contiguous user pages (memory block)
	; (caller: 'sysdalloc' system call)
	;
	; INPUT ->
	;	EBX = VIRTUAL ADDRESS (beginning address)
	;	ECX = byte count
	;	[u.pgdir] = user's page directory
	;	[u.ppdir] = parent's page directory
	;
	; OUTPUT ->
	;    If CF = 0 	
	;	EAX = Deallocated memory bytes
	;	  (Even if shared or read only pages will not be
	;	   deallocated on M.A.T., this byte count will be
	;	   returned as virtually deallocated bytes; in fact
	;	   virtually deallocated user pages * 4096.) 
	;	EBX = Virtual address (as rounded up)
	;    If CF = 1    	
	;	EAX = 0 (there is not any deallocated pages)
	;
	; Note: Empty page tables will not be deallocated!!!
	;     (they will be deallocated at process termination stage)
	;
	; Modified Registers -> EAX, EDX, ESI, EDI, EBX, ECX, EBP
	;
	mov	esi, ebx
	mov	edi, esi
	add	edi, ecx
	add	esi, PAGE_SIZE - 1  ; 4095 (round up)	
	shr	esi, PAGE_SHIFT
	shr	edi, PAGE_SHIFT
	mov	eax, edi ; end page
	sub	eax, esi ; end page - start page
	jna	da_u_pd_err  ; < 1
	mov	ebx, esi
	shl	ebx, PAGE_SHIFT ; virtual address (as rounded up)
	push	ebx ; *
	mov	ecx, eax ; page count
	shl	eax, PAGE_SHIFT ; byte count as adjusted
	push	eax ; **
	mov	ebx, [u.pgdir] ; physical addr of user's page dir
 	add	esi, CORE/PAGE_SIZE 
	mov	edi, esi
	and	edi, PTE_MASK ; PTE entry in the page table
	push	edi ; *** ; PTE index (of page directory)
	shr	esi, PAGE_D_SHIFT - PAGE_SHIFT ; 22-12=10
	mov	edx, esi 
	; EDX = PDE index
	shl	esi, 2 ; convert PDE index to dword offset
	add	esi, ebx ; add page directory address
da_u_pd_1:
	lodsd
	;
	mov	ebp, esi ; 20/02/2017
	; EBP = next PDE address
	;
	test	al, PDE_A_PRESENT ; bit 0, present flag (must be 1)
	jz	da_u_pd_3 ; 20/05/2017	
	and	ax, PDE_A_CLEAR ; 0F000h ; clear lower 12 (attribute) bits
	; EAX = PHYSICAL (flat) ADDRESS OF THE PAGE TABLE
	mov	edi, [esp] ; ***
	; EDI = PTE index (of complete page directory)
	;and	edi, PTE_MASK ; PTE entry in the page table
	shl	edi, 2 ; convert PTE index to dword offset
	mov	esi, edi ; PTE offset in page table (0-4092)
	add	esi, eax ; now, esi points to requested PTE
da_u_pt_0:
	lodsd
	test	al, PTE_A_PRESENT ; bit 0, present flag (must be 1)
	jz	short da_u_pt_1
	;
	test	al, PTE_A_WRITE   ; bit 1, writable (r/w) flag
				  ; (must be 1)
	jnz	short da_u_pt_3
	; Read only -duplicated- page (belongs to a parent or a child)
        test    ax, PTE_DUPLICATED ; Was this page duplicated 
				   ; as child's page ?
	jz	short da_u_pt_4 ; Clear PTE but don't deallocate the page!
	;
	; check the parent's PTE value is read only & same page or not.. 
	; EDX = page directory entry index (0-1023)
	push	edx ; ****
	; EDI = page table entry offset (0-4092)
	mov	ebx, [u.ppgdir] ; page directory of the parent process
	shl	dx, 2 ; *4 
	add	ebx, edx ; PDE address (for the parent)
	mov	edx, [ebx] ; page table address
	test	dl, PDE_A_PRESENT ; present (valid) or not ?
	jz	short da_u_pt_2	; parent process does not use this page
	and	dx, PDE_A_CLEAR ; 0F000h ; Clear attribute bits
	; EDI = page table entry offset (0-4092)
	add	edi, edx	; PTE address (for the parent)
	mov	ebx, [edi]
	test	bl, PTE_A_PRESENT ; present or not ?
	jz	short da_u_pt_2	; parent process does not use this page
	and	ax, PTE_A_CLEAR ; 0F000h ; Clear attribute bits 
	and	bx, PTE_A_CLEAR ; 0F000h ; Clear attribute bits
	cmp	eax, ebx	; parent's and child's pages are same ?
	jne	short da_u_pt_2	; not same page
				; deallocate the child's page
        or      byte [edi], PTE_A_WRITE ; convert to writable page (parent)
	pop	edx ; ****
	jmp	short da_u_pt_4

; 17/04/2021
; ('da_u_pt_1' is disabled as temporary)

;da_u_pt_1:
;	or	eax, eax	; swapped page ?
;	jz	short da_u_pt_5	; no
;				; yes
;	shr	eax, 1
;	call	unlink_swap_block ; Deallocate swapped page block
;				  ; on the swap disk (or in file)
;	jmp	short da_u_pt_5
da_u_pt_2:
	pop	edx ; ****
da_u_pt_3:
	test	ax, PTE_SHARED	; shared or direct memory access indicator
	jnz	short da_u_pt_4	; AVL bit 1 = 1, do not deallocate this page!
	;
	;and	ax, PTE_A_CLEAR ; 0F000h ; clear lower 12 (attribute) bits
	call	deallocate_page ; set the mem allocation bit of this page
da_u_pt_4:
	mov	dword [esi-4], 0 ; clear/reset PTE (child, dupl. as parent)
; 17/04/2021 (temporary)
da_u_pt_1:
da_u_pt_5:
	; 20/05/2017
	pop	eax ; *** PTE index (of page directory)
	dec	ecx ; remain page count
	jz	short da_u_pd_4
	inc	eax ; next PTE
	and	ax, PTE_MASK ; PTE entry index in the page table
	push	eax ; *** (save again)
	;mov	edi, eax
	;and 	di, PTE_MASK
	;cmp	edi, PAGE_SIZE / 4 ; 1024
	;jnb	short da_u_pd_2
	mov	edi, eax
	shl	edi, 2 ; convert index to dword offset
	;test	ax, PTE_MASK ; 3FFh
	or	eax, eax
	jnz	short da_u_pt_0 ; 1-1023
da_u_pd_2:
	inc	edx
	; 20/05/2017
	and	dx, PTE_MASK  ; 3FFh
	jz	short da_u_pd_4  ; 0 (1024)
	;cmp	edx, 1024
	;jnb	short da_u_pd_4
	mov	esi, ebp ; 20/02/2017
	jmp	da_u_pd_1
da_u_pd_3:
	; 15/05/2017 (empty page directory entry)
	sub	ecx, 1024
	ja	short da_u_pd_2 ; 20/05/2017
da_u_pd_4:
	pop	eax ; **
	pop	ebx ; *
	retn

da_u_pd_err:
	xor	eax, eax
	stc	
	retn

allocate_user_pages:
	; 20/05/2017
	; 01/05/2017, 02/05/2017, 15/05/2017
	; 04/03/2017
	; 20/02/2017 (TRDOS 386 = TRDOS v2.0)
	;
	; Allocate physically contiguous user pages (memory block)
	; (caller: 'sysalloc' system call)
	;
	; Note: This procedure does not alloc a page's itself
	;	(page bit) on Memory Allocation Table.
	;	(allocate_memory_block is needed before this proc)
	;
	; INPUT ->
	;	EAX = PHYSICAL ADDRESS (beginning address)
	;	EBX = VIRTUAL ADDRESS (beginning address)
	;	ECX = byte count (>=4096)
	;	[u.pgdir] = user's page directory
	;	
	;	Note: All addresses are (must be) already adjusted
	;	to page	borders, otherwise, lower 12bits of addresses
	;	and byte count would be truncated.
	;
	; OUTPUT ->
	;	none
	;
	;	CF = 1 -> insufficient memory error
	;
	; Note: All pages will be allocated in physical page order 
	;	from the beginning page address. 
	;	* A new page table will be added to the page dir
	;	  when the requested PDE is invalid.
	;	* Those pages will not be added to swap queue
	;	  because main purpose of this allocation is to
	;	  set a direct memory access (DMA controller) buffer.
	;	 (Swapping out a page in a DMA buffer would be wrong!)
	;	* Previous content of page tables (PTEs) would be
	;	  (should be) deallocated before entering this
	;	  procedure. So, new page table entries (PTEs)
	;	  directly will be written without checking
	;	  their previous content.	
	;	* Only solution to increase free memory by removing
	;	  that non-swappable memory block is to terminate
	;	  the process or to wait until the process will 
	;	  deallocate that memory block as itself. ('sysdalloc')
	;	  (No problem, if the process does not grab all of
	;	  -very big amount of- free memory by using
	;	  'sysalloc' system call!?)
	;	  (Even if the process has grabbed all of free memory, 
	;	  no problem if the process is not running in 
	;	  multitasking mode. No problem in multitasking
	;	  mode if there is not another process which is running
	;	  or waiting or sleeping for an event as it's pages
	;	  are swapped-out. But a new process can not start to
	;	  run if all of free memory has beeen allocated 
	;	  by running processes. Deallocation -'sysdalloc'- 
	;	  or terminate a running process is needed 
	;	  in order to run a new process.) 
	;
	; Modified Registers -> EAX, EDX, ESI, EDI, EBX, ECX, EBP
	;

	; 01/05/2017
	and	ax, ~PAGE_OFF
	and	bx, ~PAGE_OFF
	; 02/05/2017 
	mov	ebp, 0FFFFF000h ; 4 Giga Bytes - 4096 Bytes (for Stack)
	shr	ecx, PAGE_SHIFT ; page count
	cmp	ecx, 1
	jb	short a_u_im_retn
	mov	edx, eax
	add	edx, ecx
	jc	short a_u_im_retn
	cmp	ebp, edx
	jb	short a_u_im_retn
	mov	edx, ebx
	add	edx, CORE
	jc	short a_u_im_retn	
	add	edx, ecx
	jc	short a_u_im_retn
	cmp	ebp, edx
	jb	short a_u_im_retn
	;
	mov	ebp, eax ; physical address
	mov	esi, ebx
	add	esi, CORE ; start of user's memory (4M) 
	shr	esi, PAGE_SHIFT ; higher 20 bits of the linear address
	;shr	ecx, PAGE_SHIFT ; page count
	mov	ebx, [u.pgdir] ; physical addr of user's page dir
	mov	edi, esi
	and	edi, PTE_MASK ; PTE entry index in the page table
	push	edi  ; * ; PTE index (in page directory)
	shr	esi, PAGE_D_SHIFT - PAGE_SHIFT ; 22-12=10
	mov	edx, esi 
	; EDX = PDE index
	shl	esi, 2 ; convert PDE index to dword offset
	add	esi, ebx ; add page directory address
a_u_pd_0:
	lodsd
	;
	mov	ebx, esi ; next PDE address
	;
	test	al, PDE_A_PRESENT ; bit 0, present flag (must be 1)
	jnz	short a_u_pd_2
	;
	; empty PDE (it does not point to valid page table address)
	call	allocate_page  ; (allocate a new page table)
	jnc	short a_u_pd_1 ; OK... now, we have a new page table.
	; cf = 1
	; There is not a free memory page to allocate a new page table !!!
	pop	esi ; *
a_u_im_retn:
	retn	; return to 'sysalloc' with 'insufficient memory' error
	;
a_u_pd_1: ; clear the new page table content 
	; EAX = Physical (base) address of the new page table
	call	clear_page ; Clear page content
	;
	or	al, PDE_A_PRESENT + PDE_A_WRITE + PDE_A_USER
		 ; set bit 0, bit 1 and bit 2 to 1
		 ; (present, writable, user)
	mov	[esi-4], eax
a_u_pd_2:
	and	ax, PDE_A_CLEAR ; 0F000h ; clear lower 12 (attribute) bits
	; EAX = PHYSICAL (flat) ADDRESS OF THE PAGE TABLE
	mov	edi, [esp] ; *
	; EDI = PTE index (of page directory)
	;and	edi, PTE_MASK ; PTE entry index in the page table	
	; EBX = next PDE address
	mov	esi, edi ; PTE index in page table (0-1023)
	shl	edi, 2 ; convert PTE index to dword offset
	add	edi, eax ; now, edi points to requested PTE
a_u_pt_0:
	; 02/05/2017
	mov	eax, [edi]
	;
	test	al, PTE_A_PRESENT ; bit 0, present flag (must be 1)
	jz	short a_u_pt_1
	;
	test	al, PTE_A_WRITE   ; bit 1, writable (r/w) flag
				  ; (must be 1)
	jnz	short a_u_pt_3
	; Read only -duplicated- page (belongs to a parent or a child)
        test    ax, PTE_DUPLICATED ; Was this page duplicated 
				   ; as child's page ?
	jz	short a_u_pt_4	; Clear PTE but don't deallocate the page!
	;
	; check the parent's PTE value is read only & same page or not.. 
	; EDX = page directory entry index (0-1023)
	push	edx ; **
	push	ebx ; ***
	; ESI = page table entry index (0-1023)
	;push	esi ; **** ; 20/05/2017
	mov	ebx, [u.ppgdir] ; page directory of the parent process
	shl	dx, 2 ; *4 
	add	ebx, edx ; PTE address,0 (for the parent)
	mov	edx, [ebx] ; page table address
	test	dl, PDE_A_PRESENT ; present (valid) or not ?
	jz	short a_u_pt_2	; parent process does not use this page
	and	dx, PDE_A_CLEAR ; 0F000h ; Clear attribute bits
	shl	si, 2 ; *4
	; ESI = page table entry offset (0-4092)
	add	esi, edx	; PTE address (for the parent)
	mov	ebx, [esi]
	test	bl, PTE_A_PRESENT ; present or not ?
	jz	short a_u_pt_2	; parent process does not use this page
	and	ax, PTE_A_CLEAR ; 0F000h ; Clear attribute bits 
	and	bx, PTE_A_CLEAR ; 0F000h ; Clear attribute bits
	cmp	eax, ebx	; parent's and child's pages are same ?
	jne	short a_u_pt_2	; not same page
				; deallocate the child's page
        or      byte [esi], PTE_A_WRITE ; convert to writable page (parent)
	;pop	esi ; **** ; 20/05/2017
	pop	ebx ; ***
	pop	edx ; **
	jmp	short a_u_pt_4
a_u_pt_1:
	or	eax, eax	; swapped page ?
	jz	short a_u_pt_4	; no
				; yes
	shr	eax, 1
	call	unlink_swap_block ; Deallocate swapped page block
				  ; on the swap disk (or in file)
	jmp	short a_u_pt_4
a_u_pt_2:
	;pop	esi ; **** ; 20/05/2017
	pop	ebx ; ***
	pop	edx ; **
a_u_pt_3:
	test	ax, PTE_SHARED	; shared or direct memory access indicator
	jnz	short a_u_pt_4	; AVL bit 1 = 1, do not deallocate this page!
	;
	;and	ax, PTE_A_CLEAR ; 0F000h ; clear lower 12 (attribute) bits
	call	deallocate_page ; set the mem allocation bit of this page
	;
a_u_pt_4:
	mov	eax, ebp ; physical address
	or	al, PTE_A_PRESENT + PTE_A_WRITE + PTE_A_USER ; 04/03/2017
	stosd
	pop	esi ; * ; 20/05/2017
	dec	ecx ; remain page count
	jz	short a_u_pd_5
	add	ebp, PAGE_SIZE
	inc	esi ; next PTE (index)
	; 20/05/2017
	;cmp	esi, PAGE_SIZE/4 ; 1024
	;jb	short a_u_pt_0
	and	si, PTE_MASK ; 3FFh (0 to 1023)
	push	esi ; *
	jnz	short a_u_pt_0 ; > 0 (<1024)
a_u_pd_3:
	inc	edx
;	cmp	edx, 1024
;	jnb	short a_u_pd_4 ; 02/05/2017 (error!, ecx > 0)
	mov	esi, ebx ; the next PDE address
	jmp	a_u_pd_0
a_u_pd_4:
	; 02/05/2017
;	stc
a_u_pd_5:
	; 20/05/2017
	;pop	edi ; *
	retn

allocate_lfb_pages_for_kernel:
	; 15/12/2020
	; 14/12/2020 - TRDOS 386 v2.0.3
	; Set kernel page tables for linear frame buffer 
	; (this procedure will be called by kernel only)
	;
	; Input:
	;	[LFB_ADDR] = linear frame buffer base address
 	;	[LFB_SIZE] = linear frame buffer size in bytes
	; Output:
	;	none
	;	cf = 1 -> error
	;
	; Modified registers: eax, ecx, edx, edi

	mov	edi, [LFB_ADDR]
	mov	edx, [LFB_SIZE]

	shr	edi, 22	; convert address to page number
			; and then convert it to PDE entry offset
			; (1 PDE is for 4MB, 22 bit shift)

	shl	di, 2	; * 4 for offset 

	;add	edx, 4095
	shr	edx, 12	; convert LFB size to LFB page count
	
	mov	ecx, edx ; * ; LFB page count

	add	ecx, 1023 ; page count + 1023
	shr	ecx, 10 ; convert to page directory entry count	
			; (page table count)
	push	ecx ; **
	shl	ecx, 12 ; convert to byte count

	xor	eax, eax ; first available pages
	
	; allocate contiguous memory block for these kernel pages
	
	call	allocate_memory_block
	; eax = start address of (contiguous) memory block
	pop	ecx ; ** ; PDE count
	jnc	short a_lfb_k_1
	; error (cf=1)
	retn
a_lfb_k_1:
	; Allocate (new) page tables in kernel's page directory
	push	ecx ; PDE (page table) count
	push	eax ; start address of contiguous memory pages
		    ; (at page boundary)	
	; edi = 1st page directory entry offset
	add	edi, [k_page_dir] ; Kernel's Page Dir Address
a_lfb_k_2:
	or	ax, PDE_A_PRESENT + PDE_A_WRITE + PDE_EXTERNAL
				; supervisor + read&write + present 	
				; + external memory block (LFB)
	stosd
	add	eax, 4096
	loop	a_lfb_k_2
	
	pop	edi ; start addr of contiguous memory pages
	pop	ecx ; page table (PDE) count

	; Allocate pages in (new) kernel page tables
	
	; (Note: page tables are contiguous in pyhsical memory)
	shl	ecx, 10 ; * 1024, convert to (total) PTE count 
	
	mov	eax, [LFB_ADDR]
	; edx = LFB page count
	;and	ax, ~4095  ; lw of LFB address is 0
a_lfb_k_3:	
	or	ax, PTE_A_PRESENT + PTE_A_WRITE + PTE_EXTERNAL
				; supervisor + read&write + present 	
				; + external memory block (LFB)
	stosd
	dec	edx
	jz	short a_lfb_k_4 ; LFB size has been completed (!?)
	add	eax, 4096	
	loop	a_lfb_k_3

	retn
	
a_lfb_k_4:
	; clear PTEs for empty/free pages 
	; 	(if there are after LFB !?)
	xor	eax, eax ; clear page table entry (empty)
	rep	stosd
	retn

;deallocate_lfb_pages_for_kernel:
	; 15/12/2020
	; 14/12/2020 - TRDOS 386 v2.0.3
	; Reset/Release kernel page tables
	;	 which are used for linear frame buffer 
	; (this procedure will be called by kernel only)
	;
	; Input:
	;	[LFB_ADDR] = linear frame buffer base address
 	;	[FFB_SIZE] = linear frame buffer size in bytes
	; Output:
	;	none
	;
	; Modified registers: eax, ecx, edi

	;mov	edi, [LFB_ADDR]
	;mov	ecx, [LFB_SIZE]
	;
	;shr	edi, 22	; convert address to page number
	;		; and then convert it to PDE entry offset
	;		; (1 PDE is for 4MB, 22 bit shift)
	;
	;shl	di, 2	; * 4 for offset 
	;
	;;add	ecx, 4095
	;shr	ecx, 12	; convert LFB size to page count  
	;
	;add	ecx, 1023 ; page count + 1023
	;shr	ecx, 10 ; convert to page directory entry count	
	;		; (page table count)
	;push	ecx ; *
	;shl	ecx, 12 ; convert to byte count
	;
	;xor	eax, eax ; first available pages
	;
	;; deallocate contiguous memory block for kernel pages
	;
	;call	deallocate_memory_block
	;
	;pop	ecx ; * ; PDE count
	;
	;; Release/Free PDEs (page tables) in kernel's page dir
	;; edi = 1st page directory entry offset
	;add	edi, [k_page_dir] ; Kernel's Page Dir Address
	;sub	eax, eax ; clear (also invalidate)
	;rep	stosd
	;
	;retn

; /// End Of MEMORY MANAGEMENT FUNCTIONS ///

;; Data:

; 09/03/2015
;swpq_count: dw 0 ; count of pages on the swap que
;swp_drv:    dd 0 ; logical drive description table address of the swap drive/disk
;swpd_size:  dd 0 ; size of swap drive/disk (volume) in sectors (512 bytes). 		  				
;swpd_free:  dd 0 ; free page blocks (4096 bytes) on swap disk/drive (logical)
;swpd_next:  dd 0 ; next free page block
;swpd_last:  dd 0 ; last swap page block