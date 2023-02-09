;*****************************************************************************
; DEV_INIT.ASM  [ TRDOS KERNEL - DEVICE DRIVER TEMPLATE & LOADER SECTION ]
; (c) 2011  Erdogan TAN  [ 09/10/2011 ]  Last Update: 07/11/2011
; 06/11/2011
; 09/10/2011, 23/10/2011, 28/10/2011, 30/10/2011, 31/10/2011, 05/11/2011
;
;

device_name_offset equ offset nul_dev_name - offset device_chain_start

Device_File_Segment equ 0
Device_File_Offset equ 2
Device_File_Status equ 4
Device_File_Type equ 6
Device_File_Drv equ 7
Device_File_FCluster equ 8
Device_File_RW_Pointer equ 12
DFL_EntrySize equ 16

; 23/10/2011
; Request header offsets
rh_len    equ 0 ; byte
rh_unit   equ 1 ; byte
rh_cmd    equ 2 ; byte
rh_status equ 3 ; word
rh_res1   equ 5 ; reserved dword
rh_res2   equ 9 ; reserved dword

; Initialization command = 0
rh0_nunits  equ 13 ; byte
rh0_brk_ofs equ 14 ; word
rh0_brk_seg equ 16 ; word
rh0_cmdl_arg_o equ 18 ; word
rh0_bpb_tbo equ 18 ; word
rh0_cmdl_arg_s equ 20 ; word
rh0_bpb_tbs equ 20 ; word
rh0_drv_ltr equ 22 ; byte 

; Media check command = 1
rh1_media     equ 13 ; byte
rh1_md_stat   equ 14 ; byte
rh1_volid_ofs equ 15 ; word
rh1_volid_seg equ 17 ; word

; Get BPB command = 2
rh2_media   equ 13 ; byte
rh2_buf_ofs equ 14 ; word
rh2_buf_seg equ 16 ; word
rh2_pbpbo   equ 18 ; word
rh2_pbpbs   equ 20 ; word

; IOCTL input command = 3
rh3_media   equ 13 ; byte
rh3_buf_ofs equ 14 ; word
rh3_buf_seg equ 16 ; word
rh3_count   equ 18 ; word
rh3_start   equ 20 ; word

; Input command = 4
rh4_media     equ 13 ; byte
rh4_buf_ofs   equ 14 ; word
rh4_buf_seg   equ 16 ; word
rh4_count     equ 18 ; word
rh4_start     equ 20 ; word
rh4_volid_ofs equ 22 ; word
rh4_volid_seg equ 24 ; word

; ND input command = 5
rh5_return equ 13 ; byte

; Input status command = 6
rh6_len    equ 0 ; byte
rh6_unit   equ 1 ; byte
rh6_cmd    equ 2 ; byte
rh6_status equ 3 ; word
rh6_res1   equ 5 ; dword
rh6_res2   equ 9 ; dword

; Input flush command = 7
rh7_len     equ 0 ; byte
rh7_unit    equ 1 ; byte
rh7_cmd     equ 2 ; byte
rh7_status  equ 3 ; word
rh7_res1    equ 5 ; dword
rh7_res2    equ 9 ; dword

; Output command = 8
rh8_media     equ 13 ; byte
rh8_buf_ofs   equ 14 ; word
rh8_buf_seg   equ 16 ; word
rh8_count     equ 18 ; word
rh8_start     equ 20 ; word
rh8_volid_ofs equ 22 ; word
rh8_volid_seg equ 24 ; word

; Output verify command = 9
rh9_media     equ 13 ; byte
rh9_buf_ofs   equ 14 ; word
rh9_buf_seg   equ 16 ; word
rh9_count     equ 18 ; word
rh9_start     equ 20 ; word
rh9_volid_ofs equ 22 ; word
rh9_volid_seg equ 24 ; word

; Output status command = 10
rh10_len    equ 0 ; byte
rh10_unit   equ 1 ; byte
rh10_cmd    equ 2 ; byte
rh10_status equ 3 ; word
rh10_res1   equ 5 ; dword
rh10_res2   equ 9 ; dword

; Output flush command = 11
rh11_len    equ 0 ; byte
rh11_unit   equ 1 ; byte
rh11_cmd    equ 2 ; byte
rh11_status equ 3 ; word
rh11_res1   equ 5 ; dword
rh11_res2   equ 9 ; dword

; IOCTL output command = 12
rh12_media   equ 13 ; byte
rh12_buf_ofs equ 14 ; word
rh12_buf_seg equ 16 ; word
rh12_count   equ 18 ; word
rh12_start   equ 20 ; word

; Open command = 13
rh13_len    equ 0 ; byte
rh13_unit   equ 1 ; byte
rh13_cmd    equ 2 ; byte
rh13_status equ 3 ; word
rh13_res1   equ 5 ; dword
rh13_res2   equ 9 ; dword

; Close command = 14
rh14_len    equ 0 ; byte
rh14_unit   equ 1 ; byte
rh14_cmd    equ 2 ; byte
rh14_status equ 3 ; word
rh14_res1   equ 5 ; dword
rh14_res2   equ 9 ; dword

; Removable command = 15
rh15_len    equ 0 ; byte
rh15_unit   equ 1 ; byte
rh15_cmd    equ 2 ; byte
rh15_status equ 3 ; word
rh15_res1   equ 5 ; dword
rh15_res2   equ 9 ; dword

; IOCTL busy command = 16
rh16_media   equ 13 ; byte
rh16_buf_ofs equ 14 ; word
rh16_buf_seg equ 16 ; word
rh16_count   equ 18 ; word

; Command = 17
rh17_len    equ 0 ; byte
rh17_unit   equ 1 ; byte
rh17_cmd    equ 2 ; byte
rh17_status equ 3 ; word
rh17_res1   equ 5 ; dword
rh17_res2   equ 9 ; dword

; Command = 18
rh18_len    equ 0 ; byte
rh18_unit   equ 1 ; byte
rh18_cmd    equ 2 ; byte
rh18_status equ 3 ; word
rh18_res1   equ 5 ; dword
rh18_res2   equ 9 ; dword

; Generic IOCTL command = 19
rh19_major   equ 13 ; byte
rh19_minor   equ 14 ; byte
rh19_SI      equ 15 ; word
rh19_DI      equ 17 ; word
rh19_pkt_ofs equ 19 ; word
rh19_pkt_seg equ 21 ; word

; Command = 20
rh20_len    equ 0 ; byte
rh20_unit   equ 1 ; byte
rh20_cmd    equ 2 ; byte
rh20_status equ 3 ; word
rh20_res1   equ 5 ; dword
rh20_res2   equ 9 ; dword

; Command = 21
rh21_len    equ 0 ; byte
rh21_unit   equ 1 ; byte
rh21_cmd    equ 2 ; byte
rh21_status equ 3 ; word
rh21_res1   equ 5 ; dword
rh21_res2   equ 9 ; dword

; Command = 22
rh22_len    equ 0 ; byte
rh22_unit   equ 1 ; byte
rh22_cmd    equ 2 ; byte
rh22_status equ 3 ; word
rh22_res1   equ 5 ; dword
rh22_res2   equ 9 ; dword

; Get device command = 23
rh23_io       equ 13 ; byte
rh23_dev_cmd  equ 14 ; byte
rh23_dev_stat equ 15 ; word
rh23_reserved equ 17 ; dword

; Set device command = 24
rh24_io       equ 13 ; byte
rh24_dev_cmd  equ 14 ; byte
rh24_dev_stat equ 15 ; word
rh24_reserved equ 17 ; dword

; IOCTL query command = 19
rh25_major   equ 13 ; byte
rh25_minor   equ 14 ; byte
rh25_SI      equ 15 ; word
rh25_DI      equ 17 ; word
rh25_pkt_ofs equ 19 ; word
rh25_pkt_seg equ 21 ; word


proc_nul_device proc near
               ; 09/10/2011

               cmp al, 1
               ja short loc_nul_dev_interrupt
               je short loc_nul_dev_strategy

               mov ax, offset device_chain_start
               retn   

device_chain_start:

; Partial derivation from "Writing ms-dos device drivers", 1992
;  by Robert S. Lai

nul_next_dev:  dd -1
nul_dev_attr:  dw 8004h
nul_dev_strg:  dw offset loc_nul_dev_strategy
nul_dev_intr:  dw offset loc_nul_dev_interrupt
nul_dev_name:  db "NUL",20h,20h,20h,20h,20h

rh_offset: dw 0
rh_segment: dw 0
		                
; Partial derivation from FreeDOS (DOS-C) KERNEL.ASM 2011-04-09 
;  by Pasquale J. Villani

loc_nul_dev_strategy:
               mov word ptr [rh_segment], es
               mov word ptr [rh_offset], bx
               retn

loc_nul_dev_interrupt:
               push bx
               lds bx, dword ptr [rh_offset]
               cmp word ptr [BX]+2, 4 ; Input command
               jne short loc_set_done_flag
               mov word ptr [BX]+12h, 0 ; Count 
loc_set_done_flag:
               or word ptr [BX]+3, 100h ; Status word
               pop bx
               push cs
               pop ds
               retn 

proc_nul_device endp

DOS_Ints_Initialized: db 0 ; 05/11/2011

Device_Files_List:
db 128 dup(0)

dev_call_address: dd 0

; 23/10/2011
RequestHeader: 
db 28 dup(0)

proc_find_load_device_file proc near
              ; 05/11/2011
              ; 31/10/2011
              ; 28/10/2011   
              ; 09/10/2011
              ; INPUT -> DS:SI = ASCIZZ file name
              ;          (Current directory)
              ;          (DS=CS)
              ; OUTPUT ->                           (ES=DS)
              ;  cf=0 : ES = Device driver file segment
              ;         SI = DFL_Entry address
              ;         AX = 0  
              ;
              ;  cf=1 : Error code in AL ...
              ;
              ;  AX, BX, CX, DX, SI, DI, ES will be changed
              ;  (ES=DS=CS) 
 
loc_check_device_list_free_entry:
        mov cx, 8
        mov di, offset Device_Files_List
        xor al, al
loc_check_dev_list_free_entry_next:
        cmp word ptr [DI], 0
        jna short loc_find_device_file_save_dfl_entry_num
        add di, DFL_EntrySize
        inc al
        loop loc_check_dev_list_free_entry_next
loc_check_free_device_entry_stc_retn:
        mov ax, 04h ; Too many open files (no file handles available)
        stc
loc_find_load_device_file_stc_retn:
        retn

loc_find_device_file_save_dfl_entry_num:
        mov byte ptr [DFL_EntryNumber], al 

loc_find_device_file:
        mov ax, 1800h ; Only files 
        call proc_find_first_file
        jc short loc_find_load_device_file_stc_retn

loc_save_device_file_size:
        mov word ptr [Open_File_Size1], ax
        mov word ptr [Open_File_Size2], dx

        mov bx, word ptr [SI][DirEntry_FstClusLO]
        mov word ptr[Open_File_FClust1], bx
        mov bx, word ptr [SI][DirEntry_FstClusHI]
        mov word ptr[Open_File_FClust2], bx

        xor bx, bx  ; bx = 0 (free space is not compared yet)
                    ; otherwise bx > 0 (free space less than required)

        or dh, dh
        jnz short loc_load_device_file_insufficient_memory_xor_ah

       ;or ax, dx
       ;jnz short loc_device_file_allocate_memory_checks

       ;mov ax, 0Eh ; 0Eh = msdos reserved error code
                    ; TRDOS error code 0Eh = Zero length
       ;stc
       ;retn  
        
loc_device_file_allocate_memory_checks:
        mov bh, byte ptr [FindFile_Drv] 
       ;mov bh, byte ptr [Current_Drv]  

        mov byte ptr [Open_File_Drive], bh

        mov si, offset Logical_DosDisks
        add si, bx
        mov bl, byte ptr [SI][LD_FATType]
        mov byte ptr [Open_File_FATType], bl
        mov bx, word ptr [SI][LD_BPB][BytesPerSec]
       ;mov ax, word ptr [Open_File_Size1]
       ;mov dx, word ptr [Open_File_Size2]

        mov cx, bx  ; Bytes per sector
        dec bx
        add ax, bx
        adc dx, 0
       ;call Rx_Dos_Div32
                    ; DX:AX = Sector count
                    ; DX = 0
        div cx  ; cx = Bytes per sector       
       ;xor dx, dx
        mov word ptr [Open_File_Sectors], ax
       ;mov bx, cx  ; Bytes per sector
       ;call proc_mul32
        mul cx

        mov word ptr [Open_File_Alloc_Size1], ax
        mov word ptr [Open_File_Alloc_Size2], dx
                        
        mov ax, word ptr [Open_File_FClust1]
        mov dx, word ptr [Open_File_FClust2]
                                         ; DX:AX = First cluster
        mov si, offset Device_Files_List
        mov bl, byte ptr [Open_File_Drive]
        mov cx, 8
loc_check_device_file_if_same:
        cmp word ptr [SI], 0
       ;jna short loc_check_device_file_if_same_next
        jna short loc_device_file_allocate_memory
        cmp byte ptr [SI]+Device_File_Drv, bl
        jne short loc_check_device_file_if_same_next
        cmp word ptr [SI]+Device_File_FCluster, ax
        jne short loc_check_device_file_if_same_next
        cmp word ptr [SI]+Device_File_FCluster+2, dx
        jne short loc_check_device_file_if_same_next

loc_check_device_file_already_loaded:
        mov ax, word ptr [SI]+Device_File_Segment
        mov es, ax
        mov ax, 0FFFFh  
        retn

loc_check_device_file_if_same_next:
        add si, DFL_EntrySize
        loop loc_check_device_file_if_same

loc_device_file_allocate_memory:
        mov ax, 352 ; 22*16, beginning segment
        call proc_get_free_memory
             	; AX= Total free allocation units (100h bytes)
            	; DX= Max. Free consequtive allocation units (100h bytes)
        	; CX= Total usable (conventional TRDOS) memory (100h bytes)
   	        ; BX= First free segment (valid if < FFFFh)
                ; cf= 1 -> no free memory 
        jnc short loc_load_device_file_compare_memory_size

loc_load_device_file_insufficient_memory_xor_ah:
        xor ah, ah
loc_load_device_file_insufficient_memory:
        mov al, 08h ; Insufficient memory 
        stc
        retn 

loc_load_device_file_compare_memory_size:
        push bx
        push dx
        mov ax, word ptr [Open_File_Alloc_Size1]
        mov dx, word ptr [Open_File_Alloc_Size2]
        mov cx, 100h
        call Rx_Dos_Div32
        ;or dx, dx
        pop dx
        pop bx
       ;jnz short loc_load_device_file_insufficient_memory_xor_ah

        cmp ax, dx      ; dx =  Max. Free consequtive allocation units
        jnb short loc_load_device_file_insufficient_memory_xor_ah
                        ; dx >= ax + 1 (100h = OFDT or PSP size)

        mov word ptr [Allocation_Size], ax

        mov byte ptr [Open_File_Type], 0D0h  ; Device files
        mov cx, word ptr [Allocation_Size] ; 100h bytes
        xor ah, ah ; mov ah, 0
        mov al, byte ptr [Open_File_Type]
        add al, byte ptr [DFL_EntryNumber]
        mov dx, 1      ; 512 byte boundary check (no odd segment)
                       ; 'and' result must be zero (dh=0)
        call proc_allocate_memory
        jc short loc_load_device_file_insufficient_memory_xor_ah 

loc_load_device_set_dfl_entry:
        mov word ptr [Open_File_Handle], bx
        mov al, byte ptr [DFL_EntryNumber]
        mov ah, DFL_EntrySize
        mul ah
        mov si, offset Device_Files_List
        add si, ax

        mov word ptr [SI], bx ; Device_File_Segment
        mov es, bx 
        xor bx, bx ; 0
        mov word ptr [SI]+2, bx ; Device_File_Offset (Header offset)

loc_load_device_file:
        mov ax, word ptr [Open_File_FClust1]
        mov dx, word ptr [Open_File_FClust2]
        mov word ptr [SI]+Device_File_FCluster, ax
        mov word ptr [SI]+Device_File_FCluster+2, dx
        mov word ptr [SI]+Device_File_Status, bx ; 0
        mov word ptr [SI]+Device_File_RW_Pointer, bx ; 0
        mov bh, byte ptr [Open_File_Drive]
        mov byte ptr [SI]+Device_File_Drv, bh

        mov cx, word ptr [Open_File_Sectors]
       ;mov bh, byte ptr [Open_File_Drive]
       ;xor bl, bl
        call proc_load_file
        push ax
        pushf
        mov al, byte ptr [DFL_EntryNumber]
        mov ah, DFL_EntrySize
        mul ah
        mov si, offset Device_Files_List
        add si, ax 
        popf 
        jnc short loc_load_device_file_ok

loc_load_device_file_check_load_error:
        mov word ptr [SI], 0  
        mov bx, word ptr [Open_File_Handle]
        mov al, byte ptr [DFL_EntryNumber]
        add al, 0D0h
        mov ah, 1 
        call proc_deallocate_memory
        pop ax
        stc
        retn
 
loc_load_device_file_ok:
        pop es 
        xor ax, ax
        retn  

DFL_EntryNumber: db 0

proc_find_load_device_file endp

proc_device_init proc near
                ; 07/11/2011 INT 20h BugFix
                ; 06/11/2011 Command line arguments
                ; 05/11/2011 DOS Interrupt Initialization
                ; 31/10/2011
		; 28/10/2011
              	; 23/10/2011
              	;	   
              	; Input : DS=CS
              	;  ES = Device driver file segment
                ;  DS:SI = DFL_Entry address
                ;  TextBuffer -> Command line arguments
              	; Output : SI, DI, CX, BX, AX, DX will be changed
                ;  (ES=DS=CS)   

                cmp byte ptr [DOS_Ints_Initialized], 0 ; 05/11/2011
                ja short loc_device_dos_int_init_ok 

loc_device_dos_int_init:
                push es
                xor ax, ax
                mov es, ax
 
                mov word ptr ES:[084h], offset trdos_int21h_routine
                push cs
                pop word ptr ES:[086h]

                mov ax, offset Run_Com_INT20h_Handler
                mov word ptr ES:[80h], ax  ; INT20h vector
                push cs
                pop word ptr ES:[82h]
                mov word ptr ES:[088h], ax ; Int 22h vector
                push cs
                pop word ptr ES:[08Ah]
                mov word ptr ES:[08Ch], ax ; Int 23h vector
                push cs
                pop word ptr ES:[08Eh]
                mov word ptr ES:[090h], ax ; Int 24h vector
                push cs
                pop word ptr ES:[092h]

                mov ax, offset trdos_ifc_handler
                mov di, 94h ; INT 25h vector
                stosw
                mov ax, cs
                stosw                
                mov ax, offset trdos_ifc_handler
                stosw
                mov ax, cs
                stosw
               ;mov di, 9Ch ; Int 27h vector offset
                mov ax, offset run_com_tsr_handler
                stosw
                mov ax, cs
                stosw                
                mov cx, 7 ; INT 28h to INT 2Eh

                mov di, 0A0h ; int 28h

loc_device_set_inv_fc_vectors1:
                mov ax, offset trdos_ifc_handler
                stosw
                mov ax, cs
                stosw
                loop loc_device_set_inv_fc_vectors1

                mov ax, offset trdos_int2f_handler
                stosw
                mov ax, cs
                stosw 
  
                mov cl, 16 ; INT 30h to INT 3Fh
loc_device_set_inv_fc_vectors2:
                mov ax, offset trdos_ifc_handler
                stosw
                mov ax, cs
                stosw
                loop loc_device_set_inv_fc_vectors2

                inc byte ptr [DOS_Ints_Initialized]

                pop es

loc_device_dos_int_init_ok:
                mov bx, es

 		mov al,'Y' ; sYs file

                cmp word ptr ES:[0], 'ZM'
                jne short loc_set_device_file_list_entry

loc_device_init_file_exe:
                dec al ; X = eXe file
                add bx, word ptr ES:[08h] ;Size of header, in paragraphs

                mov cx, word ptr ES:[06h] ;Number of relocation items
                jcxz short loc_device_exe_hdr_relocation_done
                push ax
                mov ax, bx
                push bx  
                mov bx, word ptr ES:[18h] ;Relocation table offset
loc_device_exe_hdr_relocation_cycle:
                mov di, word ptr ES:[BX]   ;Item Offset
                mov dx, word ptr ES:[BX+2] ;Item Segment (Relative]
                add dx, ax                 ;Item Segment (Absolute)
                push es
                mov es, dx
                add word ptr ES:[DI], ax ;Fixup
                pop es
                add bx, 4
                loop loc_device_exe_hdr_relocation_cycle
                pop bx
                pop ax
loc_device_exe_hdr_relocation_done:
               ;
loc_set_device_file_list_entry:
                mov byte ptr [SI]+Device_File_Type, al
                mov word ptr [SI]+Device_File_Segment, bx
               ;mov word ptr [SI]+Device_File_Offset,0
                mov ax, word ptr [SI]+Device_File_Status
                or al, 1 ; initialization stage 
                         ; (segment address = device header)
                mov word ptr [SI]+Device_File_Status, ax 
loc_reset_request_header:
                push ds
	        pop es
                mov ax, 22 ; 22 bytes in the packet (MS-DOS convention)
              	mov di, offset RequestHeader
              	stosw ; 0
              	mov cx, 8
              	xor al, al 
              	rep stosw ; 2 to 16
                mov ax, offset TextBuffer ; Command line arguments
                                          ; just after the "DEVICE "
                stosw ; 18
                mov ax, cs
                stosw ; 20
                xor ax, ax
                stosw ; 22
                stosw ; 24
                stosw ; 26    

	        mov ax, word ptr [nul_next_dev]
                mov word ptr [nul_next_dev], cx ; 0
       	        mov dx, word ptr [nul_next_dev]+2
             	mov word ptr [nul_next_dev]+2, bx                
                
                xor si, si
                mov word ptr [PSP_Address], si

               ; Only for Invalid function call return
                mov word ptr [StackSegment], ss
                mov word ptr [StackPointer], sp
                mov word ptr [BasePointer], 0 ; bp
 
       	        mov ds, bx
                mov word ptr [SI], ax
      	        mov word ptr [SI]+2, dx
        	mov ax, word ptr [SI]+8 ; Interrupt (2nd call)              
           	mov dx, word ptr [SI]+6 ; Strategy (1st call)
  
        	mov word ptr CS:[dev_call_address], dx
         	mov word ptr CS:[dev_call_address]+2, bx

                push ax ; Interrupt (2nd call) handler address  

                mov bx, offset RequestHeader

                call dword ptr CS:[dev_call_address]

                pop word ptr CS:[dev_call_address] ; ax
 
                call dword ptr CS:[dev_call_address]

loc_device_init_retn:
                push cs
                pop ds
                retn

proc_device_init endp

proc_print_device_list proc near
                ; 05/11/2011
                ; 31/10/2011
                ; 30/10/2011
                ; INPUT ->  none
                ;  DS=CS 
                ; OUTPUT -> none
                ;  AX, DX, CX, BX, SI, DI will be changed
                ;  ES=DS=CS

                ; push cs
                ; pop ds
                mov bx, offset device_chain_start

               ;push cs
               ;pop es
                  
loc_ppdl_print_device_list_row:
                mov dx, word ptr [BX]+2
                push dx
                mov ax, word ptr [BX] 
                push ax
 
                mov ax, ds
                push ax
                call proc_hex
                mov word ptr CS:[DevSegmentStr]+2, ax
                pop ax
                mov al, ah
                call proc_hex
                mov word ptr CS:[DevSegmentStr], ax
                mov ax, bx
                push ax
                call proc_hex
                mov word ptr CS:[DevOffsetStr]+2, ax
                pop ax
                mov al, ah
                call proc_hex
                mov word ptr CS:[DevOffsetStr], ax

                mov si, bx
                mov cx, 4
                add si, cx 
                lodsw
                      ; ax = Device attributes
                add si, cx
                      ; si = 10, Device name address

                mov di, offset DeviceNameStr
                rep movsw

                push cs
                pop ds

                push ax
		call proc_hex
                mov word ptr [DevAttribStr]+2, ax                 
		pop ax
                mov al, ah
                call proc_hex
                mov word ptr [DevAttribStr], ax

                mov si, offset DeviceList_Row
                call proc_printmsg  
                pop ax
                pop dx
                cmp ax, 0FFFFh
                jne short loc_ppdl_set_next_device_address
		cmp dx, 0FFFFh
                jne short loc_ppdl_set_next_device_address

                mov si, offset msg_end_of_device_list
                call proc_printmsg
                retn 

loc_ppdl_set_next_device_address:
                mov bx, ax
                mov ds, dx
                jmp loc_ppdl_print_device_list_row

DeviceList_Row: 
		db "DEVICE "
                db "NAME: " 
DeviceNameStr:  db 8 dup(0)
                db "  "
                db "Address: "
DevSegmentStr:  dd 30303030
                db "h"
                db ":"
DevOffsetStr:   dd 30303030
 		db "h"
                db "  "   
                db "Attributes: " 
DevAttribStr:   dd 30303030                 
                db "h"
                db 0Dh, 0ah, 0

msg_end_of_device_list:
                db 0Dh, 0Ah, "End of device list (-1 sign OK)." 
                db 0Dh, 0Ah, 0

proc_print_device_list endp


int2f_handler proc near
; 06/11/2011

trdos_int2f_handler:
iret

int2f_handler endp   
