; *****************************************************************************
;
; INT_21H.ASM  [ TRDOS INT 21h Handler ]
; Copyright (C) 2005 Erdogan TAN  [ 29/01/2005 ]      Last Update: 08/11/2011 
;
; 05/11/2011, 07/11/2011
; 10/07/2011, 07/08/2011, 04/09/2011, 05/09/2011, 19/09/2011, 24/09/2011	
; 09/04/2011, 10/04/2011, 14/04/2011, 15/04/2011, 17/04/2011, 05/07/2011 
; 19/02/2011, 26/02/2011, 05/03/2011, 12/03/2011, 13/03/2011, 27/03/2011
; 27/01/2011, 03/02/2011, 07/02/2011, 08/02/2011, 11/02/2011, 16/02/2011
; 14/11/2009
; TRDOS.ASM (include INT_21H.ASM)
;
; *****************************************************************************

; TRDOS Function Calls
;
; (Code optimization will be needed at the last stage...)

trdos_int21h_routine:
               ; 19/02/2011
               ; 03/02/2011 
                push ds
                pop word ptr CS:[INT_DS]
                push cs
                pop ds

                mov word ptr [Interrupt_Number], 21h ; 17/04/2011

                pop word ptr [INT_IP]
                pop word ptr [INT_CS]
                mov word ptr [INT_SPBP], sp
                pop word ptr [INT_Flags]
                                
                mov word ptr [INT_BP], bp
                mov word ptr [INT_SP], sp
                
                push ss
                pop word ptr [INT_SS]
 
                push es
                pop word ptr [INT_ES]

                mov word ptr [INT_SI], si
                mov word ptr [INT_DI], di
                 
                mov word ptr [INT_BX], bx
                mov word ptr [INT_CX], cx   
                mov word ptr [INT_DX], dx
                mov word ptr [INT_AX], ax

                push word ptr [INT_Flags]
              ; 19/02/2011
                mov bp, word ptr [INT_SPBP] 
                and word ptr [BP], 0FFFEh ; clc
              ; 08/11/2011
                mov bp, word ptr [INT_BP]
               ;  

                push word ptr [INT_CS]
                push word ptr [INT_IP]

                push word ptr [INT_DS]
                pop ds
               ;

                cmp ah, 9
                jne short pass_int21h_printstr
                push dx
                push ax
                push bx
                mov ah, 0Fh ; Get Video Display Mode
                int 10h
                ;AH= Number of columns
                ;AL= Video mode
                ;BH= Current video page
                mov word ptr CS:[Program_VideoMode],ax 
                mov byte ptr CS:[Program_VideoPage],bh
loc_print_$:          
                ; Print String (with $ terminator)
                ; Input -> DS:DX = String Address
                mov     BX, DX
                mov     AL, byte ptr [BX]   ; DS:[BX]
                cmp     AL,'$'            
                je      short loc_int21h_9h_return ; AL = $ : end of string
		mov     AH,0Eh
                mov     BX, word ptr CS:[Program_CharColorAtr] ; BH = Video page                   
                int     10h                     ; BIOS Service func ( ah ) = 0Eh
						; Write char as TTY
						;AL-char BH-page BL-color
                inc     DX
                jmp     short loc_print_$

loc_int21h_1h_getchar:
                ; Output: al, keyboard input with echo
                ; AL= ASCII Code
                ; AH= Scan Code
                push dx
                push ax
                xor ah,ah
                int 16h
                pop dx
                mov dl, al
                mov ax, dx
                push ax
                push bx
                jmp short loc_int21h_1h_echo
loc_int21h_2h_printchar:
                ; Int 21h Function 2h Input: DL, char
                push dx
                push ax
                push bx
loc_int21h_1h_echo:
                mov ah, 0Fh ; Get Video Display Mode
                int 10h
                ;AH= Number of columns
                ;AL= Video mode
                ;BH= Current video page
                mov word ptr CS:[Program_VideoMode],ax 
                mov byte ptr CS:[Program_VideoPage],bh
                mov BL, byte ptr CS:[Program_CharColorAtr]
                mov AL, DL
                mov AH,0Eh
                int 10h                     ; BIOS Service func ( ah ) = 0Eh
                                            ; Write char as TTY
                                            ;AL-char BH-page BL-color
loc_int21h_9h_return:
                pop bx
                pop ax
                pop dx
loc_int21h_return_1:
               ; 05/11/2011
               ; 03/02/2011
                mov bp, word ptr CS:[INT_BP]
               ;sti
                iret

loc_get_trdos_version:
                ; INT 21h Function AH= 30h
                ; Return:
                ; AL= Major version number
                ; AH= Minor version number
                ; BX:CX= Build Number
                mov ax, 10 ; TRDOS Version 1.0
                xor bx, bx
                xor cx, cx
                jmp short loc_int21h_return_1
loc_get_return_code:
               ; INT 21h Function AH=4Dh, Get Return Code
               ; temporary
                xor ax,ax
                jmp short loc_int21h_return_1
pass_int21h_printstr:
                ; INT 21h Function: 0 = Terminate Program
                cmp ah, 0
                je run_com_INT20h_handler
                ; INT 21h Function: 1 = Keyboard Input
                cmp ah, 1
                je short loc_int21h_1h_getchar
                ; INT 21h Function: 2 = Display Output
                cmp ah, 2
                je short loc_int21h_2h_printchar
                ; INT 21h Function: 4Ch = Terminate Program
                cmp ah, 4Ch
                je run_com_INT20h_handler
                cmp ah, 4Ah
                je loc_int21h_modify_alloc_mem
                cmp ah, 4Bh
                jne short loc_check_function_35h ; Temporary
                ;08/11/2011 Temporary 
                or al, al
                jz loc_int21h_function_4Bh
                jmp loc_other_dos_functions 
                ;  
loc_check_function_35h:
                cmp ah, 35h
                je loc_get_interrupt_vector ; SHORT
                cmp ah, 25h
                je loc_set_interrupt_vector ; short
                cmp ah, 2Ch
                je loc_int21h_get_time  ; SHORT
                cmp ah, 2Dh
                je loc_int21h_set_time
                cmp ah, 2Ah
                je loc_int21h_get_date
                cmp ah, 2Bh
                je loc_int21h_set_date
                cmp ah, 30h
                je short loc_get_trdos_version
                cmp ah, 4Dh
                je short loc_get_return_code
              ; 04/09/2011 
                cmp ah, 48h
                je loc_int21h_allocate_memory
                cmp ah, 49h
                je loc_int21h_deallocate_memory
               ; 27/01/2011
                cmp ah, 4Eh
                je loc_int21h_find_first_file
               ;
                cmp ah, 4Fh
                je loc_int21h_find_next_file
                ja loc_int21h_check_71h ; 08/11/2011 
               ; 26/02/2011
            	cmp ah, 3Dh
            	je loc_int21h_open_file
               ; 05/03/2011
                cmp ah, 3Eh
                je loc_int21h_close_file
                cmp ah, 3Fh
                je loc_int21h_read_file
               ; 13/03/2011
                cmp ah, 3Ch
                je loc_int21h_create_file
                cmp ah, 40h
                je loc_int21h_write_file
                cmp ah, 0Ah
                je loc_int21h_buffered_keyboard_input

               ; 27/03/2011
                cmp ah, 19h
                je loc_int21h_get_current_drive
                cmp ah, 47h
                je loc_int21h_get_current_directory

               ; 09/04/2011
                cmp ah, 42h
                je loc_INT21h_seek_file 
               ; 10/04/2011
                cmp ah, 1Ah
                je loc_INT21h_set_DTA
                cmp ah, 41h
                je loc_INT21h_delete_file
               ; 05/09/2011
                cmp ah, 7
                je loc_int21h_direct_char_input 
                cmp ah, 8
                je loc_int21h_direct_char_input_ctrl_c
               ; 07/11/2011
                cmp ah, 44h
                jne short loc_int21h_check_56h
                or al, al ; temporary 07/011/2011
                jz loc_int21h_Device_IOCTL
               ; 08/11/2011
                jmp short loc_other_dos_functions
  
               ;jmp dword ptr CS:[INT21h_Offset]

loc_int21h_check_56h:
               ; 19/09/2011
                cmp ah, 56h
                je loc_INT21h_rename_file

loc_other_dos_functions:
               ; 17/04/2011
                push cs
                pop ds
                jmp loc_invalid_function_call ; IFC.ASM 

loc_int21h_check_71h: ; 08/11/2011
                cmp ah, 71h
                ja short loc_other_dos_functions
                jb short loc_int21h_check_56h
                xor al, al

                mov byte ptr CS:[INT21h_Err_Code], al
                mov bp, word ptr CS:[INT_SPBP] ; Flags
                or byte ptr [BP], 1 ; stc
                mov bp, word ptr CS:[INT_BP]
   
                iret 

loc_get_interrupt_vector:
                ; Get Interrupt Vector
                ; Input -> AL= Interrupt Number
                ; Output -> ES:BX= Interrupt Vector
                xor ah, ah
                mov bx, ax
                shl bx, 1
                shl bx, 1
                xor al, al
                push ds
                mov ds, ax
                mov es, word ptr [BX]+2
                mov bx, word ptr [BX]
                pop ds
                jmp loc_int21h_return_2
loc_set_interrupt_vector:
                ; Set Interrupt Vector
                ; Input -> AL= Interrupt Number
                ; DS:DX= Interrupt Vector
                xor ah, ah
                push es
                push bx
                mov bx, ax
                shl bx, 1
                shl bx, 1
                xor al, al
                mov es, ax
                mov word ptr ES:[BX], dx
                mov word ptr ES:[BX]+2, ds
                pop bx
                pop es
                jmp short loc_int21h_return_2
loc_int21h_get_time:
                ; INT 21h Function AH= 2Ch
                ; Output:
                ; CH= Hour (0-23)
                ; CL= Minute (0-59)
                ; DH= Second (0-59)
                ; DL= Standard Time = 0
                ; DL= Daylight Savings Time = 1
                mov ah, 02h
                int 1Ah
                mov al, ch
                and al, 0Fh
                mov ah, ch
                shr ah,1
                shr ah,1
                shr ah,1
                shr ah,1
                aad
                mov ch, al
                mov al, cl
                and al, 0Fh
                mov ah, cl
                shr ah,1
                shr ah,1
                shr ah,1
                shr ah,1
                aad
                mov cl, al
                mov al, dh
                and al, 0Fh
                mov ah, dh
                shr ah,1
                shr ah,1
                shr ah,1
                shr ah,1
                aad
                mov dh, al
                jmp short loc_int21h_return_2
loc_int21h_set_time:
                ; INT 21h Function AH= 2Dh
                ; CH= Hour (0-23)
                ; CL= Minute (0-59)
                ; DH= Second (0-59)
                ; DL= Standard Time = 0
                ; DL= Daylight Savings Time = 1
                mov al, ch
                aam
                shl ah, 1
                shl ah, 1
                shl ah, 1
                shl ah, 1
                or ah,al
                mov ch, ah
                mov al, cl
                aam
                shl ah, 1
                shl ah, 1
                shl ah, 1
                shl ah, 1
                or ah,al
                mov cl, ah
                mov al, dh
                aam
                shl ah, 1
                shl ah, 1
                shl ah, 1
                shl ah, 1
                or ah,al
                mov dh, ah
                push cx
                push dx
                mov ah, 02h
                int 1Ah
                mov al, dl
                pop dx
                pop cx
                mov dl, al
                mov ah, 03h
                int 1Ah
loc_int21h_return_3:
                xor ax, ax
loc_int21h_return_2:
               ; 03/02/2011
               ; 08/11/2011 CS: prefix
                mov bp, word ptr CS:[INT_BP]
               ;sti
                iret

loc_int21h_get_date:
                ; INT 21h Function AH= 2Ah
                ; Output:
                ; CX= Year (1980-2099)
                ; DH= Month (1-12)
                ; DL= Day (1-31)
                mov ah, 04h
                int 1Ah
                mov al, ch
                and al, 0Fh
                mov ah, ch
                shr ah,1
                shr ah,1
                shr ah,1
                shr ah,1
                aad
                mov ch, al
                mov al, cl
                and al, 0Fh
                mov ah, cl
                shr ah,1
                shr ah,1
                shr ah,1
                shr ah,1
                aad
                mov cl, al
                mov al, ch
                mov ah, 100
                mul ah
                xor ch, ch
                add ax, cx
                mov cx, ax
                mov al, dh
                and al, 0Fh
                mov ah, dh
                shr ah,1
                shr ah,1
                shr ah,1
                shr ah,1
                aad
                mov dh, al
                mov al, dl
                and al, 0Fh
                mov ah, dl
                shr ah,1
                shr ah,1
                shr ah,1
                shr ah,1
                aad
                mov dl, al
                jmp short loc_int21h_return_2

loc_int21h_set_date:
                ; INT 21h Function AH= 2Bh
                ; CX= Year (1980-2099)
                ; DH= Month (1-12)
                ; DL= Day (1-31)
                mov ax, cx
                mov cl, 100
                div cl
                mov cl, ah
                mov ch, al
                mov al, cl
                aam
                shl ah, 1
                shl ah, 1
                shl ah, 1
                shl ah, 1
                or ah,al
                mov cl, ah
                mov al, ch
                aam
                shl ah, 1
                shl ah, 1
                shl ah, 1
                shl ah, 1
                or ah,al
                mov ch, ah
                mov al, dh
                aam
                shl ah, 1
                shl ah, 1
                shl ah, 1
                shl ah, 1
                or ah,al
                mov dh, ah
                mov al, dl
                aam
                shl ah, 1
                shl ah, 1
                shl ah, 1
                shl ah, 1
                or ah,al
                mov dl, ah
                mov ah, 05h
                int 1Ah
                jmp loc_int21h_return_3

loc_int21h_modify_alloc_mem:
               ; INT 21h Function AH= 4Ah
               ; Modify Allocated Memory
               ; Input: ES -> Address of the block 
               ;        (Paragraph) to modify allocation size
               ; BX: New allocation size in paragraphs (with 16 bytes unit)
               ; 04/09/2011
               ; 15/04/2011 -> PuSh & PoP check and corrections 
               ; 14/04/2011 
               push ds ;ps1
               push cx ;ps2
               push dx ;ps3
               push bx ;ps4
                
              ;push cs ;ps5
              ;pop ds ;pp5
               mov ax, bx
               xor dx, dx
               add ax, 15
               mov cx, 16  
               div cx
               push ax ;ps5 ; new allocation size in 100h bytes
               mov ah, 1 ; 04/09/2011 -> return max. available block size
               mov bx, es
               call proc_return_memory_block_allocation_status
              ; 04/09/2011 
               pop bx ;pp5 ; pushed ax
               cmp cx, bx
               jnb short loc_int21h_modify_alloc_mem_continue
             
loc_int21h_modify_alloc_mem_insuff_mem:
             ; 04/09/2011  
               pop ax ;pp4
               shl cx, 1
               shl cx, 1
               shl cx, 1
               shl cx, 1
               push cx ;ps4 
               mov ax, 08h ; Insufficient memory
                
loc_int21h_modify_alloc_mem_err_retn:
               push bp ;ps5
               mov bp, word ptr CS:[INT_SPBP] ; Flags
               or byte ptr [BP], 1 ; stc    
               pop bp ;pp5
                
loc_int21h_modify_alloc_mem_retn:
               pop bx ;pp4
               pop dx ;pp3
               pop cx ;pp2 
               pop ds ;pp1

               iret

loc_int21h_modify_alloc_mem_continue:
               cmp dx, bx 
               je short loc_int21h_modify_alloc_mem_retn
               jb short loc_int21h_modify_alloc_mem_extend

loc_int21h_modify_alloc_mem_shorten: ; shrink
               sub dx, bx
               shl dx, 1
               shl dx, 1
               shl dx, 1
               shl dx, 1
               mov bx, es
               add bx, dx
              ; mov ah, 1 
               call proc_deallocate_memory
               mov bx, es
               xor ax, ax  
               jmp short loc_int21h_modify_alloc_mem_retn               
 
loc_int21h_modify_alloc_mem_extend:
               push cs ;ps5
               pop ds  ;pp5

               xchg bx, dx
               sub dx, bx
               mov cx, dx  ; allocation size (in 256 bytes)
               shl dx, 1
               shl dx, 1
               shl dx, 1
               shl dx, 1
               mov bx, es
               add bx, dx
               xor ah, ah  ; allocation size in 256 bytes
               xor dx, dx  ; no segment boundary check   
               call proc_allocate_memory
              ;jc short loc_int21h_modify_alloc_mem_insuff_mem
               xor ax, ax 
               jmp short loc_int21h_modify_alloc_mem_retn

loc_int21h_allocate_memory:
               ; INT 21h Function AH= 48h
               ; Allocate Memory
               ; Input: BX -> Requested block size (in paragraphs) 
               ; 04/09/2011
               push ds ;ps1
               push cx ;ps2
               push dx ;ps3
               push bx ;ps4

               push cs ;ps5
               pop ds ;pp5
              
               xor dx, dx
               mov al, byte ptr [Int21h_MemAlloc]
               cmp al, 0FFh ;11111111b (8 allocations)
               jnb short loc_int21h_alloc_mem_insuff_mem 
               
               push ax ;ps5 

               inc dh ; 1
                mov cx, 8 
loc_int21h_alloc_memory_shift:
               shr al, 1
               jnc short loc_int21h_alloc_memory_shift_ok
               inc dl
               shl dh, 1 
               loop loc_int21h_alloc_memory_shift

loc_int21h_alloc_memory_shift_ok:
               push dx ;ps6
               mov ax, bx
               xor dx, dx
               add ax, 15
               mov cl, 16  
               div cx
               mov cx, ax ; count of 256 bytes to be allocated
               pop ax ;pp6, pushed dx
               pop dx ;pp5, pushed ax
               or dl, ah ; OR alloc byte with new alloc bit
               add al, 0F8h ; Allocation code from F8 to FFh
               push dx ;ps5
               xor ah, ah
               mov dx, 1 ; 512 byte boundary check
                       ; 'and' result must be zero
               call proc_allocate_memory
               pop dx ;pp5
               jc short loc_int21h_alloc_mem_insuff_mem_shift
               mov byte ptr [Int21h_MemAlloc], dl
               mov ax, bx
               jmp short loc_int21h_alloc_mem_retn    

loc_int21h_alloc_mem_insuff_mem_shift:
               shl dx, 1
               shl dx, 1
               shl dx, 1
               shl dx, 1
loc_int21h_alloc_mem_insuff_mem:
               pop ax ;pp4
               push dx ;ps4
                       ;max. possible alloc size in paragraphs 
               mov ax, 08h ; Insufficient memory
loc_int21h_alloc_mem_err_retn:
               push bp ;ps5
               mov bp, word ptr CS:[INT_SPBP] ; Flags
               or byte ptr [BP], 1 ; stc    
               pop bp ;pp5
                
loc_int21h_alloc_mem_retn:
               pop bx ;pp4
               pop dx ;pp3
               pop cx ;pp2  
               pop ds ;pp1

               iret

INT21h_MemAlloc: db 0

loc_int21h_deallocate_memory:
               ; INT 21h Function AH= 49h
               ; Deallocate Memory
               ; Input: ES -> Mem segment to be deallocated 
               ; 04/09/2011
              ;push ds ;ps1
               push cx ;ps2
               push dx ;ps3
               push bx ;ps4

              ;push cs ;ps5
              ;pop ds ;pp5

               mov bx, es              
               xor ah, ah
               call proc_return_memory_block_allocation_status

               cmp al, 0F8h
               jb short loc_int21h_dealloc_mem_invalid_mem_block

              ;inc ah ; 1
               mov ah, 1
              ;mov bx, es 
               call proc_deallocate_memory
               xor ah, ah
               mov cx, ax
               mov al, 1   
               sub cl, 0F8h
               jz short loc_int21h_dealloc_mem_shift_ok  
loc_int21h_dealloc_mem_shift:
               shl al, cl
loc_int21h_dealloc_mem_shift_ok:
               not al
               and byte ptr CS:[Int21h_MemAlloc], al
               xor al, al
loc_int21h_dealloc_mem_retn:
               pop bx ;pp4
               pop dx ;pp3
               pop cx ;pp2 
              ;pop ds ;pp1

               iret

loc_int21h_dealloc_mem_invalid_mem_block:
               mov ax, 09h ; Invalid memory block address

               push bp ;ps5
               mov bp, word ptr CS:[INT_SPBP] ; Flags
               or byte ptr [BP], 1 ; stc    
               pop bp ;pp5

               jmp short loc_int21h_dealloc_mem_retn

loc_int21h_function_4Bh:
               ; 08/11/2011
               ; 14/11/2009
               ; INT 21h Function AH= 4Bh
               ; Load and Run Executable File
               ; INPUT
               ; AL= Mode (AL=0 Normal execution)
               ; ES:BX= Pointer to parameters block
               ;    BX+0 = EnvironmentSegment
               ;    BX+2 = CommandTail
               ;    BX+6 = not used (FCB_1)
               ;    BX+10 = not used (FCB_2)   
               ; DS:DX= DOS Dot File Name Address
               ; 03/02/2011 
                push dx
                push ax
                push cx
                push si
                push di
                push ds
                push bp ; 08/11/2011 push pos. change
                push es ; 08/11/2011
                push bx ; 08/11/2011
                mov word ptr CS:[StackPointer21h], sp
                mov word ptr CS:[StackSegment21h], ss
                inc byte ptr CS:[INT21h_4Bh]
                push cs
                pop  es
                mov si, dx
                ; DS:SI -> ASCIIZ Pathname string beginning address

                ; 14/11/2009
                call proc_check_exe_filename_ext
                jnc short loc_int21h_load_executable_file
               ; 08/11/2011
                push cs
                pop ds 
                xor ax, ax
                mov byte ptr [INT21h_Err_Code], al
                mov byte ptr [INT21h_FFF_Valid], al ; 05/02/2011
loc_int21h_4Bh_stc_iret:
                dec byte ptr [INT21h_4Bh]
                mov ss, word ptr [StackSegment21h]
                mov sp, word ptr [StackPointer21h]
               ; 03/02/2011
               ; 08/11/2011 CS: prefix has been removed  
                mov bp, word ptr [INT_SPBP] ; Flags
                or byte ptr [BP], 1 ; stc   
               ;
                pop bx ; 08/11/2011 push pos. change
                pop es ; 08/11/2011
                pop bp ; 08/11/2011
                pop ds
                pop di
                pop si
                pop cx
                pop dx ;ax
                pop dx
                iret

loc_int21h_load_executable_file:
                mov byte ptr CS:[EXE_ID], al
                mov cx, bx ; Char count of the file <= 12
                mov di, offset INT21h_File_name
                push di
                rep movsb
                pop si
                push cs
                pop ds 
                mov ax, 04h
                call proc_open_file
                jnc short loc_INT21h_run_loaded_executable_file
               ;27/01/2011  
                mov byte ptr [INT21h_Err_Code], al
                jmp short loc_int21h_4Bh_stc_iret

loc_INT21h_run_loaded_executable_file:
                mov word ptr [Run_Segment], ax
                mov word ptr [Run_Offset], 100h
               ; 08/11/2011
                push es
                push bx
               ; 24/09/2011
                mov dx, ax
                mov ax, word ptr [PSP_Address]
                mov ds, dx
                mov bx, 22 ; pspParentId
                mov word ptr [BX], ax
                mov word ptr CS:[PSP_Address], dx               
               ; 08/11/2011
                pop si ; pushed bx
                pop ds ; pushed es
                push ds
                push si  
                mov es, dx
                mov ax, word ptr [SI] ; Environment segment
                and ax, ax 
                jna short loc_INT21h_rlef_move_cmd_tail
loc_INT21h_rlef_move_env_segment:
                mov di, 44 ; pspEnvironment
                mov word ptr ES:[DI], ax 
loc_INT21h_rlef_move_cmd_tail:
                inc si
                inc si
                mov dx, word ptr [SI]+2 ; Command tail, segment
                or dx, dx
                jz short loc_INT21h_rlef_move_cmd_tail_ok
                mov ax, word ptr [SI] ; Command tail, offset
                mov si, ax
                mov ds, dx 
                mov al, byte ptr [SI]
                or al, al ; Command tail byte count
                jz short loc_INT21h_rlef_move_cmd_tail_ok
 
                mov cx, 78
loc_INT21h_rlef_move_cmd_tail_next:
                mov di, 128 ; Command tail (PSP)  
                lodsb
                stosb
                cmp al, 13 ; Carriage return 
                jna short loc_INT21h_rlef_move_cmd_tail_ok                        
                loop loc_INT21h_rlef_move_cmd_tail_next
                mov al, 13 ; 0Dh
                stosb
loc_INT21h_rlef_move_cmd_tail_ok:
                pop bx ; Pointer to open file size, offset
                pop es ; Pointer to open file size, segment
                mov ax, word ptr ES:[BX]
                mov dx, word ptr ES:[BX]+2
                push cs
                pop ds
                mov word ptr [Run_File_Size], ax
                mov word ptr [Run_File_Size]+2, dx 
                jmp loc_run_loaded_executable_file_INT21h_4Bh

;End Of INT 21h Function AH=4Bh Load & Run Executable File

loc_INT21h_find_first_file:
               ; 07/08/2011 BugFix for attributes > 0 input  
               ; 11/02/2011 
               ; 05/02/2011
               ; 27/01/2011
               ; INT 21h Function AH= 4Eh
               ; Find First File
               ; INPUT
               ; CX= Attributes
               ; DS:DX= Pointer to filename
                 push es 
                 push ds
                 push di
                 push si
                 push bx
                 push dx 
                 push cx
                 mov ax, 1
                 mov si, dx
                ; 07/08/2011
                 mov byte ptr CS:[INT21h_FFF_Attributes], cl
                ; 
                 call proc_set_working_path
                 jc short loc_INT21h_find_first_file_stc_retn
                ; 03/02/2011
                 pop ax ; cx, attributes
                 push ax
                 or ax, ax
                 jz short loc_INT21h_find_first_file_pass_attr
                 mov ah, 10h
                 test al, 8h
                 jnz short loc_INT21h_find_first_file_chk_dir_attr
                 or ah, 8h
loc_INT21h_find_first_file_chk_dir_attr:
                 and al, 10h ; Directory
                 jz short loc_INT21h_find_first_file_pass_attr
                 and ah, 8h
                 xor al, al
loc_INT21h_find_first_file_pass_attr:
                 mov word ptr [INT21h_AttribMask], ax ; 05/02/2011
                 mov si, offset FindFile_Name
                 call proc_find_first_file
                 jc short loc_INT21h_find_first_file_stc_retn

                ; 07/08/2011
                 mov dx, 2 ; File not found

loc_find_next_file_ok:
                 ; 07/08/2011
                 mov al, byte ptr [INT21h_FFF_Attributes]
                 or al, al
                 jz short loc_INT21h_find_first_file_attr_ok
                 xor al, 0FFh
                 and al, cl
                 jz short loc_INT21h_find_first_file_attr_ok
                 mov ax, dx
                 ;
                 mov bp, word ptr [INT_SPBP] ; Flags
                 or byte ptr [BP], 1 ; stc   
                 jmp short loc_INT21h_find_first_file_err_retn
                  
loc_INT21h_find_first_file_attr_ok:
                ;mov si, offset FindFile_DirEntry
                 mov di, word ptr [DTA_Address]
                 mov ax, word ptr [DTA_Address]+2
                 mov es, ax 
                ;mov es, word ptr [DTA_Address]+2
                ; 11/02/2011
                 mov cx, 21
                 xor al, al
                 rep stosb
                ;
                 add si, 11 ; DirEntry_Attr
                 movsb
                 add si, 10 ; DirEntry_WrtTime
                 movsw
                 movsw      ; DirEntry_WrtDate
                 add si, 2
                 movsw      ; DirEntry_Filesize
                 movsw
                 mov si, offset FindFile_DirEntry
                 call Get_File_Name
                ; ds - es exchange 
                 push cs
                 pop ds
                 inc byte ptr [INT21h_FFF_Valid] ; 05/02/2011
                 ; al = 0

loc_INT21h_find_first_file_err_retn:
                 mov byte ptr [INT21h_Err_Code], al      
loc_INT21h_find_first_file_retn:
                 push ax
                 call proc_reset_working_path
                 pop ax
loc_INT21h_find_next_file_retn: 
                 pop cx
                 pop dx
                 pop bx
                 pop si
                 pop di
                 mov bp, word ptr [INT_BP]
                 pop ds
                 pop es    
                 iret

loc_INT21h_find_first_file_stc_retn:
                 mov byte ptr [INT21h_Err_Code], al
                 mov al, 2 ; mov ax, 2
loc_INT21h_find_next_file_stc_retn:
                 ; 03/02/2011
                  mov bp, word ptr [INT_SPBP] ; Flags
                  or byte ptr [BP], 1 ; stc   
                 ;
                 jmp short loc_INT21h_find_first_file_retn  

loc_INT21h_find_next_file:
               ; 07/08/2011 
               ; 19/02/2011
               ; 16/02/2011 (Call Find_Next_File) BugFix
               ; 11/02/2011
               ; 08/02/2011 
               ; 07/02/2011 
               ; 05/02/2011
               ; INT 21h Function AH= 4Fh
               ; Find Next File
               ; INPUT -> none
                 push es 
                 push ds
                 push di
                 push si
                 push bx
                 push dx 
                 push cx

                 push cs
                 pop ds

                 cmp byte ptr [INT21h_FFF_Valid], 0
                 ja short loc_find_next_file_check_cdrv

                 mov ax, 18h
                 mov bp, word ptr [INT_SPBP] ; Flags
                 or byte ptr [BP], 1 ; stc
                 jmp short loc_INT21h_find_next_file_retn  

loc_find_next_file_check_cdrv: 
                ; 19/02/2011
                 mov bl, byte ptr [Current_Drv]
                 xor bh, bh
                 mov word ptr [SWP_DRV], bx
                 mov dl, byte ptr [FindFile_Drv]
                 cmp dl, bl
                 jne short loc_find_next_file_chg_cdrv

                ; mov bh, byte ptr [Current_Drv]
                ; xor bl, bl
                 xchg bh, bl
                 mov si, offset Logical_DosDisks
                 add si, bx
                 jmp short loc_find_next_file_check_cdir1

loc_find_next_file_chg_cdrv: 
                 inc byte ptr [SWP_DRV_chg]

                 call proc_change_current_drive
                 jc loc_INT21h_find_next_file_load_sub_dir_err

loc_find_next_file_check_cdir1:
                ; 19/02/2011
                ; 08/02/2011
                 mov ax, word ptr [FindFile_DirCluster]
                 mov dx, word ptr [FindFile_DirCluster]+2

                 mov cx, ax
                 or cx, dx
                 jnz short loc_find_next_file_check_cdir2
               
                 cmp byte ptr [Current_FATType], 2
                 ja short loc_find_next_file_stc_18h_retn1
                 cmp byte ptr [Current_FATType], 1
                 jb short loc_find_next_file_stc_18h_retn1

                 cmp byte ptr [DirBuff_ValidData], bl ; 0
                 jna short loc_INT21h_find_next_file_load_root_dir 

                 cmp ax, word ptr [DirBuff_Cluster]
                 jne short loc_INT21h_find_next_file_load_root_dir
                 cmp dx, word ptr [DirBuff_Cluster]+2
                ;jne short loc_INT21h_find_next_file_load_root_dir
                 je short loc_find_next_file_check_cdir_OK 

                ;cmp byte ptr [Current_Dir_Level], 0
                ;ja short loc_find_next_file_stc_18h_retn1  
                ;jna short loc_find_next_file_check_cdir_OK 

loc_INT21h_find_next_file_load_root_dir:
                 inc byte ptr [SWP_DRV_chg]
                 call proc_load_FAT_root_directory
                 jnc short loc_find_next_file_check_cdir_OK
                 mov byte ptr [INT21h_Err_Code], al
                 jmp loc_INT21h_find_next_file_stc_retn
                 
loc_find_next_file_stc_18h_retn1:
                 mov ax, 18h
loc_find_next_file_stc_18h_retn2:
                 mov byte ptr [INT21h_FFF_Valid], 0FFh 
                 jmp short loc_INT21h_find_next_file_load_sub_dir_err

loc_INT21h_find_next_file_check_error_no:
                 mov byte ptr [INT21h_FFF_Valid], 0
                 cmp al, 18h
                 jne short loc_INT21h_find_next_file_load_sub_dir_err
                 jmp short loc_find_next_file_stc_18h_retn2
                   
loc_find_next_file_check_cdir2:
                 cmp byte ptr [DirBuff_ValidData], bl ; 0
                 jna short loc_INT21h_find_next_file_load_sub_dir

                 cmp ax, word ptr [DirBuff_Cluster]
                 jne short loc_INT21h_find_next_file_load_sub_dir
                 cmp dx, word ptr [DirBuff_Cluster]+2
                 je short loc_find_next_file_check_cdir_OK

loc_INT21h_find_next_file_load_sub_dir:
                 inc byte ptr [SWP_DRV_chg]
                 cmp byte ptr [Current_FATType], 1
                 jnb short loc_INT21h_find_next_file_load_fat_dir

loc_INT21h_find_next_file_load_fs_dir:
                 call proc_load_FS_sub_directory
                 jnc short loc_find_next_file_check_cdir_OK ; 07/02/2011
loc_INT21h_find_next_file_load_sub_dir_err:
                 mov byte ptr [INT21h_Err_Code], al
                 jmp loc_INT21h_find_next_file_stc_retn

loc_INT21h_find_next_file_load_fat_dir:
                 call proc_load_FAT_sub_directory                  
                 jc short loc_INT21h_find_next_file_load_sub_dir_err

loc_find_next_file_check_cdir_OK:
                 mov ax, word ptr [INT21h_AttribMask]
                 mov word ptr [FindFile_AttributesMask], ax
                 xor cx, cx ; 16/02/2011
                 call proc_find_next_file
                 jc short loc_INT21h_find_next_file_check_error_no

                ; 07/08/2011
                 mov dx, 18h ; No more files
                 jmp loc_find_next_file_ok

loc_INT21h_open_file:
               ; 26/02/2011 
               ; INT 21h Function AH= 3Dh
               ; Open File
               ; INPUT
               ; AL= File Access Value
               ;     0- File Open for reading
               ;     1- File opened for writing
               ;     2- File opened for reading and writing
               ;     AL = 3,4,5 -> TRDOS extensions    
               ; DS:DX= Pointer to filename (ASCIIZ)
                 push es 
                 push ds
                 push di
                 push si
                 push bx
                 push dx 
                 push cx
                 cmp al, 5
                 jna short loc_INT21h_open_file_valid_access_no
                 mov ax, 12 ; Invalid access code
                 push cs
                 pop ds
                 jmp short loc_INT21h_open_file_err_retn
                  
loc_INT21h_open_file_valid_access_no: 
                 push ax
                 mov ax, 1
                 mov si, dx
                 call proc_set_working_path
                 pop cx
                 jc short loc_INT21h_open_file_err_retn

loc_INT21h_open_file_set_working_path_ok:
                ; push ds
                ; DS = CS  
                 mov ax, word ptr [PSP_Address]
                 push ds
                 mov ds, ax
                 mov si, OF_OFL_SN ; 4
                 mov ah, byte ptr [SI]
                 pop ds

                 mov al, cl ; Access code
                 mov si, offset FindFile_Name
                 call proc_open_file
                 jnc short loc_INT21h_open_file_retn

loc_INT21h_open_file_err_retn:
                 mov byte ptr [INT21h_Err_Code], al
                 mov bp, word ptr [INT_SPBP] ; Flags
                 or byte ptr [BP], 1 ; stc   

loc_INT21h_open_file_retn:
                 push ax
                 call proc_reset_working_path
loc_INT21h_close_file_retn:
loc_INT21h_read_file_retn:
                 pop ax

                 pop cx
                 pop dx
                 pop bx
                 pop si
                 pop di
                 mov bp, word ptr [INT_BP]
                 pop ds
                 pop es    
                 iret

loc_INT21h_close_file:
               ; 05/03/2011 
               ; INT 21h Function AH= 3Eh
               ; Close File
               ; INPUT
               ; BX= File Handle
               ; 
                 push es 
                 push ds
                 push di
                 push si
                 push bx
                 push dx 
                 push cx
                 push ax

                 push cs
                 pop ds 
                 
                 mov ax, bx 
                 call proc_close_file
                 jnc short loc_INT21h_close_file_retn

loc_INT21h_close_file_err_retn:
loc_INT21h_read_file_err_retn:
                 mov byte ptr [INT21h_Err_Code], al
                 mov bp, word ptr [INT_SPBP] ; Flags
                 or byte ptr [BP], 1 ; stc   

                 jmp short loc_INT21h_close_file_retn

loc_INT21h_read_file:
               ; 13/03/2011 
               ; 05/03/2011 
               ; INT 21h Function AH= 3Fh
               ; Read from a File
               ; INPUT
               ; BX= File Handle
               ; CX= Number of bytes to read
               ; DS:DX= Buffer address 

                 push es 
                 push ds
                 push di
                 push si
                 push bx
                 push dx 
                 push cx
                 
                 mov ax, bx 
                 mov bx, dx
                 xor dx, dx
                 push ds
                 pop es 
                 push cs ; 13/03/2011
                 pop ds  ;
                 call proc_read_file
                 push cx ; Read count
                 jnc short loc_INT21h_read_file_retn

loc_INT21h_read_file_check_eof:
                 cmp dh, 0FFh ; EOF sign
                 jnb short loc_INT21h_read_file_retn

                 pop ax
                 push ax
                 jmp short loc_INT21h_read_file_err_retn

loc_int21h_create_file:
               ; 10/07/2011
               ; 12/03/2011 
               ; INT 21h Function AH= 3Ch
               ; Create File
               ; INPUT
               ; CX= Attributes
               ; DS:DX= Address of zero terminaned path name
                 push es 
                 push ds
                 push di
                 push si
                 push bx
                 push dx 
                 push cx
                 mov ax, 1
                 mov si, dx
                 call proc_set_working_path
                 jc short loc_INT21h_create_file_stc_retn
                 xor ax, ax 
                 mov si, offset FindFile_Name
                 call proc_find_first_file
                 jnc short loc_INT21h_create_file_access_denied

loc_INT21h_create_file_chk_fff_err_no:
                 cmp al, 02h ; File not found
                 jne short loc_INT21h_create_file_stc_retn

loc_INT21h_create_file_chk_file_type:               
                 pop cx ; File/Directory attributes
                 push cx  

                 test cl, 08h ; Volume name
                 jnz short loc_INT21h_create_file_access_denied

                 mov si, offset FindFile_Name  
                 
                 test cl, 10h ; Directory
                 jnz short loc_INT21h_create_directory

                 xor dx, dx
                 xor ax, ax
                 xor bx, bx ; 10/07/2011
                 call proc_create_file
                 jc short loc_INT21h_create_file_stc_retn

                 mov al, 1 ; Access code = 1 --> open file for writing 
                 mov si, offset FindFile_Name
                 call proc_open_file
                 jc short loc_INT21h_create_file_stc_retn

loc_INT21h_create_file_no_error:                  
                 mov byte ptr [INT21h_Err_Code], 0 
loc_INT21h_create_file_retn:
                 push ax
                 call proc_reset_working_path
loc_INT21h_write_file_retn:
                 pop ax
                 pop cx
                 pop dx
                 pop bx
                 pop si
                 pop di
                 mov bp, word ptr [INT_BP]
                 pop ds
                 pop es    
                 iret

loc_INT21h_create_directory:
                 call proc_make_directory
                 jc short loc_INT21h_create_file_stc_retn

                 xor ax, ax ; File handle = 0
                 mov byte ptr [INT21h_Err_Code], al ; 0
                 jmp short  loc_INT21h_create_file_retn

loc_INT21h_create_file_access_denied:
                 mov ax, 05h ; Access denied

loc_INT21h_create_file_stc_retn:
loc_INT21h_write_file_err_retn:
                 mov byte ptr [INT21h_Err_Code], al
                 mov al, 3 ; mov ax, 3
                 mov bp, word ptr [INT_SPBP] ; Flags
                 or byte ptr [BP], 1 ; stc   
                 ;
                 jmp short loc_INT21h_create_file_retn

loc_INT21h_write_file:
               ; 13/03/2011
               ; 05/03/2011 
               ; INT 21h Function AH= 40h
               ; Write to a File
               ; INPUT
               ; BX= File Handle
               ; CX= Number of bytes to write
               ; DS:DX= Buffer address 

                 push es 
                 push ds
                 push di
                 push si
                 push bx
                 push dx 
                 push cx

                 mov ax, bx 
                 mov bx, dx
                 xor dx, dx
                 or cx, cx
                 jnz short loc_INT21h_write_file_no_truncate
loc_INT21h_write_file_truncate:
                 dec dh
loc_INT21h_write_file_no_truncate:
                 xor di, di   
                 push ds
                 pop es
                 push cs ; 13/03/2011
                 pop ds  ;
                 call proc_write_file
                 push cx ; Write count
                 jnc short loc_INT21h_write_file_retn

loc_INT21h_write_file_check_eof:
                 cmp dh, 0FFh ; EOF sign
                 jnb short loc_INT21h_write_file_retn

                 pop ax
                 push ax
                 jmp short loc_INT21h_write_file_err_retn

loc_INT21h_seek_file:
               ; 09/04/2011 
               ; INT 21h Function AH= 42h
               ; Move File Pointer
               ; INPUT
               ; AL= Moving method
               ;     0- Offset specified is from the beginning of the file
               ;     1- Offset specified is distance from the current file pointer
               ;     2- The pointer is moved to the end of file minus the specified offset
               ; BX= File Handle
               ; CX:DX= Distance to move, in bytes

                 call proc_move_file_pointer
                 jnc short loc_INT21h_seek_file_retn

loc_INT21h_seek_file_err_retn:
                 mov byte ptr [INT21h_Err_Code], al
                 mov bp, word ptr [INT_SPBP] ; Flags
                 or byte ptr [BP], 1 ; stc 
                 mov bp, word ptr [INT_BP]  

loc_INT21h_seek_file_retn:
                 iret


loc_INT21h_delete_file:
               ; 10/04/2011 
               ; INT 21h Function AH= 41h
               ; Delete File
               ; INPUT
               ; DS:DX= Pointer to filename (ASCIIZ)
                 push es 
                 push ds
                 push di
                 push si
                 push bx
                 push dx 
                 push cx
                 push ax  

                 mov ax, 1
                 mov si, dx
                 call proc_set_working_path
                 jc short loc_INT21h_delete_file_err_retn

loc_INT21h_delete_file_set_working_path_ok:
loc_int21h_delfile_find:
	        mov si, offset FindFile_Name
                mov ax, 1800h ; Except volume label and dirs
                call proc_find_first_file
                jc short loc_INT21h_delete_file_err_retn

loc_int21h_delfile_check_ambgfn:
                cmp bx, 0 ; Ambiguous filename chars used sign (BX>0)
                jna short loc_int21h_delfile_found

loc_INT21h_delete_file_not_found:
                mov ax, 02h ; File not found
                jmp short loc_INT21h_delete_file_err_retn

loc_int21h_delfile_found:
                and cl, 07h ; Attributes
                jz short loc_int21h_delfile_check_attrib_ok
                mov ax, 05h ; Access denied (Permission denied)
                jmp short loc_INT21h_delete_file_err_retn 

loc_int21h_delfile_check_attrib_ok:
                ; ch > 0 -> the long name is existing/yes
                 mov byte ptr [LongName_EntryLength], ch

loc_int21h_delfile_call_proc_del_file:
                mov bh, byte ptr [FindFile_Drv]
                mov bl, byte ptr [LongName_EntryLength]
                mov cx, word ptr [DirBuff_EntryCounter]
  	       ; ES:DI -> Directory buffer entry offset/address 
                call proc_delete_file
                jnc short loc_INT21h_delete_file_retn

loc_INT21h_delete_file_err_retn:
                 pop dx ; pushed ax
                 mov byte ptr [INT21h_Err_Code], al
                 mov bp, word ptr [INT_SPBP] ; Flags
                 or byte ptr [BP], 1 ; stc   
                 push ax                      

loc_INT21h_delete_file_retn:
                 call proc_reset_working_path

loc_INT21h_rename_file_retn1: 
                 pop ax
loc_INT21h_rename_file_retn2: 
                 pop cx
                 pop dx
                 pop bx
                 pop si
                 pop di
                 mov bp, word ptr [INT_BP]
                 pop ds
                 pop es    
                 iret

loc_INT21h_rename_file:
               ; 19/09/2011 
               ; INT 21h Function AH= 56h
               ; Rename File
               ; INPUT
               ; DS:DX= Pointer to old filename (ASCIIZ)
               ; ES:DI= Pointer to new filename (ASCIIZ) 
                 push es 
                 push ds
                 push di
                 push si
                 push bx
                 push dx 
                 push cx
                 push ax  

                 xor al, al ; INT 21h Move file sign -> 0
                 mov si, dx
                 call move_source_file_to_destination_file
                 jnc short loc_INT21h_rename_file_retn1
                        
loc_INT21h_rename_file_err_retn:
                 pop dx ; pushed ax
                 mov byte ptr [INT21h_Err_Code], al
                 mov bp, word ptr [INT_SPBP] ; Flags
                 or byte ptr [BP], 1 ; stc   
                 jmp short loc_INT21h_rename_file_retn2

loc_int21h_buffered_keyboard_input:
               ; 13/03/2011 
               ; INT 21h Function AH= 0Ah
               ; Buffered Keyboard Input
               ; INPUT
               ; DS:DX= Pointer to input buffer
               ; Offset 0 = Byte/Character count
               ; Offset 1 = Actual number of characters read  
               ; Offset 2 = Buffer starting location

                 push si
                 push bx
                 push dx 
                 push cx
                 push ax 

                 mov si, dx
                 mov al, byte ptr [SI]
                 add si, 2
                 call proc_int21h_rw_char
                 dec si
                 mov byte ptr [SI], al
                 pop ax
                 pop cx
                 pop dx
                 pop bx
                 pop si
                
                 iret

loc_int21h_get_current_drive:
               ; 27/03/2011 
               ; INT 21h Function AH= 19h
               ; Get Default Drive
               ; INPUT
               ;  none
               ; OUTPUT 
               ;  AL = Drive number (0=A,1=B, 2=C, etc.)
               ; 
                
                 mov al, byte ptr CS:[Current_Drv]
                 iret

loc_int21h_get_current_directory:
               ; 27/03/2011 
               ; INT 21h Function AH= 47h
               ; Get Current Directory
               ; INPUT
               ; DS:SI= Pointer to buffer
               ; DL = Drive number (0=Default, 1=A,2=B, 3=C, etc.)
               ; 
                
                 push es   
                 push di
                 push bx
                 push cx
                 push ax ; Perhaps the caller needs ax must not be changed !?

                 call proc_get_current_directory
                 ; DS:SI and DX are not changed, jnc -> ES=DS  
                 jc short loc_INT21h_get_current_dir_err_retn

                 pop ax 

loc_INT21h_get_current_dir_retn:
                 pop cx
                 pop bx
                 pop di
                 pop es 

                 iret
 
loc_INT21h_get_current_dir_err_retn:
                 pop cx ; pushed ax
                 mov byte ptr CS:[INT21h_Err_Code], al
                 mov bp, word ptr CS:[INT_SPBP] ; Flags
                 or byte ptr [BP], 1 ; stc
                 mov bp, word ptr CS:[INT_BP]   
                 jmp short loc_INT21h_get_current_dir_retn

loc_INT21h_set_DTA:
               ; 10/04/2011
               ; INT 21h Function AH= 1Ah
               ; Set Disk Transfer Address
               ; INPUT
               ; DS:DX= Pointer to DTA
                 
                 push ax
                 mov ax, ds
                 mov word ptr CS:[DTA_Address], dx
                 mov word ptr CS:[DTA_Address]+2, ax
                 pop ax

                 iret

loc_int21h_direct_char_input:
               ; 05/09/2011 
               ; INT 21h Function AH= 07h 
               ; Direct Character/Keyboard Input, no echo
               ; INPUT
               ; AH = 07h -> Direct char input, no ctrl-c check
               
                 xor ah, ah ; 0 
                 mov al, byte ptr CS:[INT21h_KeyCodeWaiting]
                 or al, al
                 jz short loc_int21h_direct_char_input_c1

                 mov byte ptr CS:[INT21h_KeyCodeWaiting], ah 

                 iret

loc_int21h_direct_char_input_c1:
                ;xor ah, ah
                 int 16h
loc_int21h_direct_char_input_c2:
                 and al, al
                 jnz short loc_int21h_direct_keyboard_input_retn

                 mov byte ptr CS:[INT21h_KeyCodeWaiting], ah

loc_int21h_direct_keyboard_input_retn:
                 iret

loc_int21h_direct_char_input_ctrl_c:
               ; 05/09/2011 
               ; INT 21h Function AH= 08h
               ; Direct Character/Keyboard Input, no echo
               ; INPUT
               ; AH = 08h -> Direct char input, ctrl-c check
               
                 xor ah, ah ; 0 
                 mov al, byte ptr CS:[INT21h_KeyCodeWaiting]
                 or al, al
                 jz short loc_int21h_direct_char_input_ctrl_c1

                 mov byte ptr CS:[INT21h_KeyCodeWaiting], ah 

                 iret

loc_int21h_direct_char_input_ctrl_c1:
                ;xor ah, ah
                 int 16h
             
                 cmp ax, 2E03h ; CTRL + C
                 jne short loc_int21h_direct_char_input_c2
                 
                 int 23h

                 iret  

INT21h_KeyCodeWaiting: db 0

loc_int21h_Device_IOCTL:
               ; 07/11/2011
               ; INT 21h Function AH= 44h 
               ; Device IOCTL
               ; INPUT
               ; AH = 44h
               ; AL = Sub Functions
               ; BX = File handle 
               ; AL<>0 are Invalid functions for now !               

                 or al, al
                 jnz short loc_int21h_Device_IOCTL_inv_sf 

loc_int21h_Device_IOCTL_sf0:
                 cmp bx, 8
                 ja short loc_int21h_Device_IOCTL_inv_handle

                 push bx

                 or bl, bl
                 jz short loc_int21h_Device_IOCTL_get_info

                 dec bl
                 shl bl, 1
                 shl bl, 1
                 shl bl, 1
                 shl bl, 1
                  
loc_int21h_Device_IOCTL_get_info:
                 add bx, offset Device_Files_List
                 mov ax, word ptr CS:[BX]
                 or ax, ax
                 jz short loc_int21h_Dev_IOCTL_inv_hndl_pop_bx
                 mov bx, word ptr CS:[BX+2]
                 add bx, 4
                 push ds
                 mov ds, ax
                 mov dx, word ptr [BX]
                 pop ds  
                 or dl, 192 ; ? ; bit 6 and 7
                                ; bit 6 = EOF on input
                                ; bit 7 = set -> device file
                 pop bx
                 
                 iret  
               
loc_int21h_Device_IOCTL_inv_sf:
                 mov ax, 1 ; Invalid function number
loc_int21h_Device_IOCTL_err_retn:
                 mov byte ptr CS:[INT21h_Err_Code], al
                 
                 mov bp, word ptr CS:[INT_SPBP] ; Flags
                 or byte ptr [BP], 1 ; stc
                 mov bp, word ptr CS:[INT_BP]   
                 
                 iret   

loc_int21h_Dev_IOCTL_inv_hndl_pop_bx:
                 pop bx
loc_int21h_Device_IOCTL_inv_handle:
                 mov ax, 6 ; Invalid function number
                 jmp short loc_int21h_Device_IOCTL_err_retn

INT21h_File_Name: db 13 dup(0)
INT21h_Err_Code: db 0
; 05/02/2011
INT21h_AttribMask: dw 0
INT21h_FFF_Valid: db 0
; 07/08/2011
INT21h_FFF_Attributes: db 0

proc_int21h_rw_char   proc    near
               ; 13/03/2011, derivation from BOOTFILE.ASM (03/01/2010)
               ; INPUT -> DS:SI= String Buffer
               ;          AL = String buffer size or byte/char count 
               ; OUTPUT -> DS:SI= Entered String (ASCIIZ)
               ;          AL = Byte/Character count

               mov     ah, al
               mov     word ptr [Char_Count_Start], ax
               mov     bx,7
               mov     ah,3
               int     10h
               mov     Word Ptr [Cursor_Pos], dx
int21h_read_next_char:
               xor     ah,ah
               int     16h
               and     al,al
               jz      short int21h_loc_arrow    
               cmp     al,0E0h          
               je      short int21h_loc_arrow
               cmp     al,08h
               jne     short int21h_char_return
int21h_loc_back:
               mov     bl,7
               mov     ah,3
               int     10h
               cmp     dl,byte ptr [Cursor_Pos]
               ja      short int21h_prev_column
int21h_loc_beep:
               mov     ah, 0Eh
               mov     al, 7
               int     10h
               jmp     short int21h_read_next_char
int21h_prev_column:
               dec     dl
int21h_set_cursor_pos:
               mov     ah,02h
               int     10h
               mov     bl, dl
               sub     bl,byte ptr [Cursor_Pos] 
               mov     cx,1
               mov     ah,09h
               mov     al,20h
               mov     byte ptr [SI][BX],al
int21h_loc_write_it:
               mov     bl,7
               int     10h
               mov     dx,word ptr [Cursor_Pos]
               jmp     short int21h_read_next_char
int21h_loc_arrow:    
               cmp     AH,4Bh
               je      short int21h_loc_back
               cmp     AH,53h
               je      short int21h_loc_back
               jmp     short int21h_read_next_char
int21h_char_return:
               mov     bl,7
               mov     ah,3
               int     10h
               mov     bl,dl
               sub     bl,byte ptr [Cursor_Pos] 
               cmp     al,20h
               jb      short int21h_loc_escape
               cmp     bl,63
               ja      short int21h_loc_beep
             ; cmp     al, "z"
             ; ja      short int21h_read_next_char
             ; cmp     al, "a"
             ; jb      short int21h_pass_capitalize
             ; and     al, 0DFh
int21h_pass_capitalize:
               xor     ah, ah
               mov     word ptr [SI][BX],ax
               mov     ah, 0Eh
               mov     bl, 7
               int     10h
int21h_pass_escape:
               cmp     al,0Dh
               je      short int21h_loc_return
int21h_pass_return:
               dec     byte ptr [char_Count]  
               jnz     short int21h_read_next_char
int21h_loc_return:
               mov     bl,7
               int     10h
               mov     al,0Ah
               int     10h
int21h_rw_exit:
               mov al, byte ptr [char_count_start]
               sub al, byte ptr [char_count]  
               retn

int21h_loc_escape:
               cmp     al,1Bh
               jne     short int21h_pass_escape
               stc
               jmp     short int21h_rw_exit

cursor_pos: dw 0
char_count_start: db 0
char_count: db 0

proc_int21h_rw_char   endp  


; End Of TRDOS Function Calls
