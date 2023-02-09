; *****************************************************************************
;
; MEM_INIT.ASM [ Initialization procs of TRDOS Memory Allocation & Management ]
; by Erdogan Tan, Last Update: 04/09/2011 
; 05/07/2011
; 14/04/2011, 30/05/2011, 02/06/2011, 11/06/2011, 12/06/2011, 19/06/2011
; 17/01/2010
; 13/06/2009, 14/07/2009, 22/08/2009, 21/09/2009, 25/10/2009
;
; TRDOS.ASM (include mem_init.asm)
;
; *****************************************************************************

MEMAT_SEGMENT equ 50h

proc_memory_init   proc    near
                ; 05/07/2011
                ; 12/06/2011
                ; 02/06/2011
                ; 25/10/2009 UMB allocation

                call proc_enable_A20_Line
                ; stc= Error, al=FFh
                ; cls= OK, al=0

                mov byte ptr [A20_Line_Enabled], al

		;  13/06/2009

                mov ax, MEMAT_SEGMENT
                mov es, ax
                xor di, di

                mov si, offset MEMAT_STR
                mov cx, 22
                rep movsb

                xor ax, ax  ; FREE
                mov cx, 1269
                rep stosw

               ; xor ch, ch

                mov ax, 0A0Ah
                mov cl, 128
                rep stosw
 
                mov ax, 0B0Bh
                mov cl, 128
                rep stosw

                mov ax, 0C0Ch
                mov cl, 128
                rep stosw

                mov ax, 0D0Dh
                mov cl, 128
                rep stosw

                mov ax, 0E0Eh
                mov cl, 128
                rep stosw

                mov ax, 0F0Fh
                mov cl, 128
                rep stosw
                
                mov al, byte ptr [A20_Line_Enabled]
                mov ah, al
                mov cx, 128
                rep stosw

                inc byte ptr [A20_Line_Enabled] 
 
                mov cx, KERNEL_FILESIZE  ; + 100h ; 12/06/2011
               
               ; xor bx, bx
                mov bx, 352 ; 02/06/2011, The first free segment 
                mov ax, 0101h  ; al= 01h, Kernel
                
                xor dx, dx ; 05/07/2011, segment boundary check is not necessary
                call proc_allocate_memory
                ;Return: BX= begin segment, DX= count of allocated units (100h bytes)
                ; CX= Allocation size, same value with CX input
                ; clc & AH= 0, no error, AL = Allocation type, same with input
                ; At the memory initialization time, any error return is not possible
                ; so, we can use return values directly, 
                ; without any error consideration.

               ; 12/06/2011
                mov ax, bx
                shr ax, 1
                shr ax, 1
                shr ax, 1
                shr ax, 1
                dec ax
                add ax, dx

               ; mov di, MEMAT_SEGMENT
               ; mov es, di
                mov di, ax
                mov al, 2
                stosb ; Store 02h (kernel/System Stack sign) 
                      ; at the end of kernel space of the memory allocation table
               
                mov cx, KERNEL_FILESIZE ; 12/06/2011
                mov si, 100h
                mov es, bx
                xor di, di
                rep movsb

                mov bx, dx
                mov ax, 256
                mul bx
                dec ax
                dec ax  
                              
                ; Return: ES is new code segment of the Kernel
                ;         AX = The last word of kernel's stack (sp)
                ;         BX = Kernel space in 100h bytes (includes kernel's stack)                
                ;         CX = 0
                ;         DX = 0 (If dx>0, kernel space is more than 64K, error)
                ;         SI = end of initial kernel space (DS:SI)
                ;         DI = end of new kernel space except stack (ES:DI) 
                ;

                retn

proc_memory_init endp
               
proc_allocate_memory proc near
                ; 05/07/2011 -> major modification -> segment boundary check
                ;   for preventing 'DMA crossed 64KB segment boundary' error  
                ; 19/06/2011
                ; 11/06/2011
                ; 30/05/2011 -> Allocation Size as allocation units
                ; 17/01/2010
                ; 25/10/2009
		; 13/06/2009
                ;
                ; INPUT:
                ; (DS=CS)
                ; CX= Allocation Size in bytes (AH=1) or in 256 bytes (AH=0)
                ; BX= Beginning Segment
                ; AH= 0 Allocate memory from beginning segment, consequently
                ;       Allocation size is in 100h bytes
                ; AH= 1 Allocate memory from beginning segment, consequently
                ;       Allocation size is in bytes  
                ; AL= Allocation Type
                ; DL= Segment Boundary check (test bl, dl) ; 05/07/2011
                ; DH=0 -> Boundary check/and result must be 0. 
                ; DH>0 -> Boundary check/and result must not be 0. 
                ; OUTPUT:
                ; stc (cf is set) = Could not be allocated
                ; AH= Error number, CX= Allocation size, same as input
                ; AL= Allocation Type
                ; DX= Count of (allocated) allocation units (100h bytes)
                ; clc (cf is reset) = Allocated, AH=0, no error
                ; CX and AL is not changed
                ; BX= Allocation segment
                ; stc -> ah = 2 -> 
                ;     DX = Count of max. free consequtive allocation units
                ;     BX = Segment of max. free consequtive allocation units   
                ;
loc_check_allocation_type_0: 
                cmp ah, 2  ; 30/05/2011
                jb short loc_check_allocation_type_1
loc_invalid_allocation_call:
                mov ah, 1 ; AH=1 -> ERR: Invalid calling type
                stc
                retn

loc_check_allocation_type_1:
                ; 05/07/2011
                mov word ptr [SegmentBoundaryCheck], dx
                ; 11/06/2011
                ;or cx, cx
                ;jz short loc_invalid_allocation_call

                mov byte ptr [Allocation_Type], al

                mov word ptr [AllocationSegment], bx
                xor dx, dx
                mov word ptr [max_free_conseq_units], dx

                push cx
                or ah, ah
                jz short loc_allocate_consequtive_units

loc_allocate_consequtive_bytes:
               ; 19/06/2011  
                mov ax, cx
                mov cx, 255
                add ax, cx
                adc dx, 0
                inc cx ; mov cx, 256 
                div cx
                mov cx, ax  ; Count of allocation units
                xor dx, dx

loc_allocate_consequtive_units:  ; 30/05/2011
              ; 19/06/2011 
                mov ax, bx
                push bx
                mov bx, 16
                div bx
               ; 11/06/2011 
                ; cx = Count of allocation units
                pop bx
                push es
                push di
                push ds
                push si
                xor dx, dx
                mov si, MEMAT_SEGMENT
 		mov ds, si
                mov si, ax
loc_MEMAT_check_free_units_1:
                cmp si, 4351
                ja short loc_MEMAT_allocation_no_free_space
                lodsb
                cmp al, 0
                ja short loc_MEMAT_check_free_units_1
              ; 11/06/2011 
                mov di, si
                dec di
                ; 05/07/2011
                mov bx, word ptr CS:[SegmentBoundaryCheck]
                or bh, bh
                jnz short loc_MEMAT_check_free_units_5
                and bx, di
                jnz short loc_MEMAT_check_free_units_1
               ;
loc_MEMAT_check_free_units_6:
                mov bx, di 
                inc dx 
                shl bx, 1
                shl bx, 1
                shl bx, 1
                shl bx, 1
                 ; bx= beginning segment
                ; cmp cx, 1
                cmp dx, cx  ; Comparison for requested allocation size
                jnb short loc_MEMAT_allocation_OK
loc_MEMAT_check_free_units_2:
                cmp si, 4351
                ja short loc_MEMAT_allocation_no_free_space
                lodsb
                ; 17/01/2010 bugfix
                ; cmp al, 0
                ; ja short pass_MEMAT_check_free_units  
                cmp al, 0
                ja short loc_MEMAT_check_free_units_3
               ; 11/06/2011 
                inc dx
pass_MEMAT_check_free_units:
                cmp dx, cx  ; Comparison for requested allocation size
                jb short loc_MEMAT_check_free_units_2

loc_MEMAT_allocation_OK:
                ; 11/06/2011
                ; dx = cx = Count of requested allocation units
		push ds
                pop es 
                pop si
                pop ds
                mov al, byte ptr [Allocation_Type]
                rep stosb 
                pop di
                pop es               
                pop cx
                xor ah, ah
                retn 

loc_MEMAT_check_free_units_3:
                ; 11/06/2011
                cmp dx, word ptr CS:[max_free_conseq_units]
                jna short loc_MEMAT_check_free_units_4
                mov word ptr CS:[max_free_conseq_units], dx 
                mov word ptr CS:[AllocationSegment], bx 
loc_MEMAT_check_free_units_4:
               ; 11/06/2011
               ; 17/01/2010 
                xor dx, dx
                jmp short loc_MEMAT_check_free_units_1

loc_MEMAT_check_free_units_5:
               ; 05/07/2011
                xor bh, bh
                and bx, di
                jz short loc_MEMAT_check_free_units_1
                jmp short loc_MEMAT_check_free_units_6   
                
loc_MEMAT_allocation_no_free_space:
               ; 11/06/2011
 		pop si
                pop ds
                pop di
                pop es 
                mov al, byte ptr [Allocation_Type]
                pop cx
                mov ah, 2 ; Error -> No enough consequtive units
                mov bx, word ptr [AllocationSegment] 
                mov dx, word ptr [max_free_conseq_units] 
                stc
                retn   
 
MEMAT_STR: db 'MEMORYALLOCATIONTABLE', 16h 
Allocation_Type: db 0
A20_Line_Enabled: db 0

AllocationSegment: dw 0  ; 11/06/2011

SegmentBoundaryCheck: dw 0 ; 05/07/2011

proc_allocate_memory endp


proc_enable_A20_line proc near
                ; Erdogan Tan, 11-6-2009
                ; INPUT: none
                ; OUTPUT:
                ; stc= Error, High Memory is not usable...
                ; AL=0 high memory (FFFF:0000 to FFFF:FFFF) is usable
                ; AL= FFh high memory is not usable
 
                ; Procedure origin: Erdogan Tan 30/12/2000 CENTRAL.ASM
          
enable_a20_line:
                mov  al, 0D1h
                out  64h, al

                xor cx, cx
loc_64h_in_al_1:
                in al, 64h
                and al, 2
                loopnz loc_64h_in_al_1
                jnz short return_from_enabling_A20_line_with_error

                mov al, 0DFh
                out 60h, al

               ;xor cx, cx
loc_64h_in_al_2:
                in al, 64h
                and al, 2
                loopnz loc_64h_in_al_2
                jnz short return_from_enabling_A20_line_with_error
                
                xor al, al    ; 11-6-2009
                retn          ; 11-6-2009
 
return_from_enabling_A20_line_with_error:
                mov al, 0FFh  ; 11-6-2009
                stc           ; 11-6-2009

                retn
       
proc_enable_A20_line endp


proc_deallocate_memory proc near
                ; 16/07/2011 -> always clc return
                ; 25/10/2009
		; 21/09/2009
                ; 17/07/2009
                
                ; INPUT:
                ; BX= Beginning Segment
                ; AH= 1 Deallocate memory from beginning segment, as consequtive
                ; AL= Allocation Type which will be deallocated
                ; OUTPUT:
                ; CX= Count of deallocated memory allocation units
                ; BX= The first segment which is deallocated 

                cmp ah, 1
                je short loc_deallocate_consequtive_units
                xor cx, cx
                retn 

loc_deallocate_consequtive_units:
                mov cx, bx
                shr cx, 1
                shr cx, 1
                shr cx, 1
                shr cx, 1   
                cmp cx, 22
                jnb short pass_deallocate_memory_fix_cx
                mov cx, 22
pass_deallocate_memory_fix_cx:
                push ds 
                push si
                mov si, MEMAT_SEGMENT
 		mov ds, si              
                mov si, cx
                xor cx, cx 
loc_check_allocation_unit_deallocated:  
                cmp si, 4351
                ja short loc_deallocate_consequtive_units_retn
                cmp byte ptr [SI], al
                je short loc_deallocate_current_memory_unit
                inc si 
                jmp short loc_check_allocation_unit_deallocated
loc_deallocate_current_memory_unit:
                mov bx, si
                shl bx, 1
                shl bx, 1
                shl bx, 1
                shl bx, 1
loc_loop_deallocate_current_memory_unit:
                mov byte ptr [SI], 0
                inc si
                inc cx
                cmp si, 4351
                ja short loc_deallocate_consequtive_units_retn
                cmp byte ptr [SI], al
               ;16/07/2011 
                ja short loc_deallocate_consequtive_units_retn
                je short loc_loop_deallocate_current_memory_unit
                clc 
loc_deallocate_consequtive_units_retn:
                pop si
                pop ds
                retn 

proc_deallocate_memory endp


proc_get_free_memory proc near
		; 25/10/2009
                ; INPUT:
                ; AX= Beginning Segment
                ; OUTPUT:
                ; AX= Total free allocation units (100h bytes)
                ; DX= Max. Free consequtive allocation units (100h bytes)
                ; CX= Total usable (conventional TRDOS) memory (100h bytes)
                ; BX= First free segment (valid if < FFFFh)
                ; cf= 1 -> no free memory

                push ds
               ;push si

                xor dx, dx
                mov word ptr [max_free_conseq_units], dx
                mov word ptr [free_memory_units], dx

                mov cx, 4096
                
               ;mov bl, byte ptr [A20_Line_Enabled]
               ;or bl, bl
               ;jz short pass_pgfm_add_high_memory_space 
               ;add cx, 256
                mov bh, byte ptr [A20_Line_Enabled]
                xor bl, bl
                add cx, bx                
;pass_pgfm_add_high_memory_space:

                push cx

                shr ax, 1
                shr ax, 1
                shr ax, 1
                shr ax, 1 
                
                sub cx, ax
                mov si, ax
            
                xor bx, bx
                dec bx  ; mov bx, 0FFFFh

                mov ax, MEMAT_SEGMENT
                mov ds, ax
    
loc_get_memat_allocation_unit_type:
                lodsb
                or al, al
                jnz short pass_pgfm_set_first_free_segment_1
                mov bx, si
                dec bx
                shl bx, 1
                shl bx, 1
                shl bx, 1
                shl bx, 1
                jmp short pass_pgfm_first_free_segment_2
 
pass_pgfm_set_first_free_segment_1:
                loop loc_get_memat_allocation_unit_type
                stc
                jmp short loc_pgfm_retn_1

loc_get_memat_allocation_unit_type_next:
                lodsb
                or al, al
                jnz short pass_pgfm_inc_free_count
pass_pgfm_first_free_segment_2:
                inc dx
                inc word ptr CS:[free_memory_units]
                loop loc_get_memat_allocation_unit_type_next
                cmp dx, word ptr CS:[max_free_conseq_units]
                jnb short loc_pgfm_retn_1
                clc 
                jmp short loc_pgfm_retn_0
pass_pgfm_inc_free_count:
                or dx, dx
                jz short pass_change_max_free_conseq_units_2  
                cmp dx, word ptr CS:[max_free_conseq_units]
                jna short pass_change_max_free_conseq_units_1
                mov word ptr CS:[max_free_conseq_units], dx
pass_change_max_free_conseq_units_1:                   
                xor dx, dx 
pass_change_max_free_conseq_units_2:
                loop loc_get_memat_allocation_unit_type_next
loc_pgfm_retn_0:
                mov dx, word ptr CS:[max_free_conseq_units]
loc_pgfm_retn_1:
                pop cx

               ;pop si
                pop ds
                
                mov ax, word ptr [free_memory_units]
                                        
                retn

max_free_conseq_units: dw 0
free_memory_units: dw 0

proc_get_free_memory endp

proc_return_memory_block_allocation_status proc near
                ; 04/09/2011 Modification, BugFix 
                ; 13/04/2011
                ; INPUT:
                ; BX= Segment Address
                ; AH= 0 -> Return status only
                ; AH > 0 -> Return maximum available block size 
                ; OUTPUT:
                ; AL= Allocation Type
                ; AH is not changed
                ; DX= Allocation Block Size (100h bytes)
                ; BX= Segment Address
                ; CX is not changed (AH=0)
                ; CX= Maximum Available Block Size (AH>0) 
                ;     (100h bytes) for same segment

loc_rmbas_start:
                mov byte ptr CS:[max_abs_return], ah
                push ds
                push si
                push cx
                xor dx, dx
                mov ax, bx
                add ax, 15 
                adc dx, 0
                mov cx, 16
                div cx
                xor cx, cx
                mov si, MEMAT_SEGMENT
 		mov ds, si
                mov si, ax
loc_rmbas_check_0:
                cmp si, 4351
                ja short pass_rmbas_check_mamb_sign
                lodsb
                mov byte ptr CS:[Allocation_Type], al
                inc cx
loc_rmbas_check_1:
                lodsb
                cmp al, byte ptr CS:[Allocation_Type]
                jne short pass_rmbas_check_mamb_sign
loc_rmbas_check_2:
                cmp si, 4351
                ja short pass_rmbas_check_mamb_sign
                inc cx
                jmp short loc_rmbas_check_1

pass_rmbas_check_mamb_sign:
                mov dx, cx ; Count of consequtive allocation units
              ; 04/09/2011
                mov ah, byte ptr CS:[max_abs_return]
                and ah, ah
                jnz short loc_rmbas_check_3
                pop cx
                jmp short loc_rmbas_check_7 
loc_rmbas_check_3:
                or al, al
                jnz short loc_rmbas_check_6
loc_rmbas_check_4:
                inc cx
                cmp si, 4351
                ja short loc_rmbas_check_6
loc_rmbas_check_5:
                lodsb
                or al, al
                jz short loc_rmbas_check_4              
loc_rmbas_check_6:
                pop si ; pushed cx
loc_rmbas_check_7:
                pop si
                pop ds
                mov al, byte ptr CS:[Allocation_Type]
                retn

max_abs_return: db 0 

proc_return_memory_block_allocation_status endp


