;*****************************************************************************
; FILE.ASM  [ TRDOS KERNEL - COMMAND EXECUTER SECTION - FILE FUNCTIONS ]
; (c) 2009 Erdogan Tan [29/10/2009] Last Update: 09/10/2011
; ! Under Development !

; Directory Entry Structure
; 29/10/2009 (According to Microsoft FAT32 File System Specification)
DirEntry_Name equ 0
DirEntry_Attr equ 11
DirEntry_NTRes equ 12
DirEntry_CrtTimeTenth equ 13
DirEntry_CrtTime equ 14
DirEntry_CrtDate equ 16
DirEntry_LastAccDate equ 18
DirEntry_FstClusHI equ 20
DirEntry_WrtTime equ 22
DirEntry_WrtDate equ 24
DirEntry_FstClusLO equ 26
DirEntry_FileSize equ 28

; Open_File_List Structure
; 25/01/2010, Bugfix for OFDT (OFL Number offset of PSP is 4)
;             OF_OFL_SN was 0ffset 2 and it is changed to offset 4  
; 29/10/2009, Structure for max. 32 Open Files or TSR programs
OFL_Owner equ 0 ; Owner program
OFL_Drive equ 1 ; Logical dos drive of file
OFL_Cluster equ 2 ; First cluster (or FDT address) of file
OFL_Handle equ 6  ; Open File Descriptor Table (256 bytes) Segment
;
OFL_EntrySize equ 8
;
; Open File Descriptor Table (File Handle Structure)
; Open File Read/Write Parameters
OF_Type equ 0 ; db '0CD' is PSP sign otherwise OFDT
OF_Owner equ 1 ; db 0, OFL Sequence number of owner program
OF_OpenMode equ 2 ; db 0
OF_WriteStatus equ 3 ; db 0
OF_OFL_SN equ 4 ; db 0, OFL Sequence Number of file
OF_Drive equ 5 ; db 0
OF_VolumeID equ 6 ; dd 0, Check for removable disks
OF_BytesPerSec equ 10 ; dw 0
OF_Size equ 12 ; dd 0, File size at memory
OF_Pointer equ 16 ; dd 0, File seek/read/write pointer
;Above 20 bytes
;
OF_LongName equ 20 ; db 64 dup(0)
;Above 84 bytes
;
;Following 42 bytes is just from FFF structure
OF_DirEntry equ 84 ; db 32 dup(0)
OF_DirFirstCluster equ 116 ; dd 0
OF_DirCluster equ 120 ; dd 0
OF_DirEntryOffset equ 124 ; dw 0
OF_FATType equ 126 ; db 0
OF_CDIR_Level equ 127 ; db 0
; Above is 128 bytes
; Open File Current Directory
OF_CDIR equ 128 ; db 128 dup(0)
;

proc_open_file proc near
        ; 09/10/2011 
        ; 14/08/2011   
        ; 06/07/2011
        ; 05/07/2011 -> SegmentBoundaryCheck modification
        ; 19/06/2011 
        ; 31/05/2011 
        ; 30/05/2011 Multi segment memory allocation modification
        ; 21/05/2011 loc_open_file_check_load_error  
        ; 26/02/2011 access code check (no permission for SHR file writing)  
        ; 20/10/2010
        ; 19/10/2010
        ; 18/10/2010
        ; 26/09/2010 
        ; 25/09/2010 major modification for r/w 
        ; 15/08/2010 -> R/W Extensions
        ; 7/12/2009 -> error return in al (it was in ah)
        ; 08/11/2009
        ; 29/10/2009
        ; INPUT -> al = open mode
        ;           0 = File opened for reading (sector buffer)
        ;           1 = File opened for writing (sector buffer)
        ;           2 = File opened for reading and writing (sector buffer)
        ;           3 = File opened for running (full open)
        ;                          (NO duplication)
        ;           4 = File opened for running (full open)
        ;                          (duplication YES)
        ;           5 = File opened for reading (full open)
        ; 1/11/2009
        ;          ah = Owner of file (0 => mainprog/kernel) 
        ; 
        ; INPUT -> DS:SI = ASCIZZ file name address
        ;
        ; OUTPUT ->                           (ES=DS=CS)
        ;          ax = File handle (PSP/OFDT Segment)
        ;   File's Sector Buffer (Disk Transfer buffer) -> 
        ;          just after the OF description table (256 byte)
        ;          ES:BX -> Pointer to file size ; 08/11/2011
        ;          (ES=CS)   
        ;                             
        ; cf=0 ->  cx = Allocation size in 100h units (memory cells)
        ;
        ; cf=1 : Error code in AL... 

loc_open_file:        
       ; 15/08/2010 
        cmp al, 5
        ja short loc_openfile_invalid_open_mode
      
        mov cx, 32
        mov di, offset Open_Files_List
        add di, OFL_Handle
loc_check_ofl_free_entry:
        cmp word ptr [DI], 0
        jna short loc_openfile_check_rw_open_mode ; 15/08/2010
        add di, OFL_EntrySize
        loop loc_check_ofl_free_entry
        mov ah, 04h ; Too many open files (no file handles available)
loc_openfile_xchg_ah_al_stc_retn:
        xchg ah, al 
        stc
loc_openfile_stc_retn:
        retn

loc_openfile_invalid_open_mode:
       ; 15/08/2010
        mov ah, 1 ; Invalid function number
        jmp short loc_openfile_xchg_ah_al_stc_retn
         
loc_openfile_check_rw_open_mode:
       ;mov byte ptr [File_Open_Mode], al
        mov word ptr [File_Open_Mode], ax
        mov al, 33
        sub al, cl
        mov byte ptr [File_OFL_Number], al

loc_openfile_find_file:
        mov ax, 1800h ; Only files 
        call proc_find_first_file
       ; 30/05/2011
        jc short loc_openfile_stc_retn

loc_check_open_file_attr_access_code:
        ; 26/02/2011
        test cl, 7  ; system, hidden, readonly 
        jz short loc_check_open_file_if_same_0
         
        cmp byte ptr [File_Open_Mode], 2
        ja short loc_check_open_file_if_same_0
         
        cmp byte ptr [File_Open_Mode], 0
        jna short  loc_check_open_file_if_same_0
      
        mov ax, 5  ; Access denied
        stc
        retn 
       
loc_check_open_file_if_same_0:
        ; 07/11/2009
        ; 31/10/2009
       
        mov word ptr [Open_File_Size1], ax
        mov word ptr [Open_File_Size2], dx

        mov bx, word ptr [SI][DirEntry_FstClusLO]
        mov word ptr[Open_File_FClust1], bx
        mov bx, word ptr [SI][DirEntry_FstClusHI]
        mov word ptr[Open_File_FClust2], bx

        xor bx, bx  ; bx = 0 (free space is not compared yet)
                    ; otherwise bx > 0 (free space less than required)

        ; 25/09/2010 
        cmp byte ptr [File_Open_Mode], 2
        jna short pass_openfile_check_empty_file 
        ; 

        or dh, dh
        jnz loc_openfile_insufficient_memory_xor_ah

        or ax, dx
        jnz short pass_openfile_check_empty_file

        mov al, byte ptr [File_Open_Mode]
        mov ah, 0Eh ; 0Eh = msdos reserved error code
                    ; TRDOS error code 0Eh = Zero length
        xchg ah, al
        stc
        retn  
        
pass_openfile_check_empty_file:
        mov bh, byte ptr [FindFile_Drv]  ; 06/07/2011
       ;mov bh, byte ptr [Current_Drv]   ; Temporary !!!
                                         ; FindFile_Drv is not functional
                                         ; for yet; because find_first_file
                                         ; only runs for currrent drv for now
                                         ; 29/10/2009

        mov byte ptr [Open_File_Drive], bh

        push si
        
        mov si, offset Logical_DosDisks
        add si, bx
        mov al, byte ptr [SI][LD_FATType]
        mov byte ptr [Open_File_FATType], al
	cmp al, 1
        jb short loc_openfile_get_FS_vol_id
        mov bx, word ptr [SI][LD_BPB][BytesPerSec]
        cmp al, 2
        ja short loc_openfile_get_FAT32_vol_id
loc_openfile_get_FAT_vol_id:
        mov ax, word ptr [SI][LD_BPB][VolumeID]
        mov dx, word ptr [SI][LD_BPB][VolumeID]+2
        jmp short loc_openfile_move_volume_id

loc_openfile_get_FS_vol_id:
        mov ax, word ptr [SI][LD_FS_VolumeSerial]
        mov dx, word ptr [SI][LD_FS_VolumeSerial]+2
        mov bx, word ptr [SI][LD_FS_BytesPerSec]
        jmp short loc_openfile_move_volume_id
 
loc_openfile_get_FAT32_vol_id:
        mov ax, word ptr [SI][LD_BPB][FAT32_VolID]
        mov dx, word ptr [SI][LD_BPB][FAT32_VolID]+2         
          
loc_openfile_move_volume_id:
        mov word ptr [Current_VolSerial1], ax
        mov word ptr [Current_VolSerial2], dx

        pop si ; 19/10/2010
              
       ; 25/09/2010
        mov word ptr [OpenFile_BytesPerSec], bx 
        cmp byte ptr [File_Open_Mode], 3
        jb  loc_openfile_for_rw_via_buffer
       ; 
        mov ax, word ptr [Open_File_Size1]
        mov dx, word ptr [Open_File_Size2]

loc_openfile_check_com_file_sizelimit:
        ; 14/08/2011 Alloc. size modification for executables
        ; 25/11/2009 (COM file stack space allocation)
        ; 25/09/2010 read file mode (5) modification
        mov cx, bx  ; Bytes per sector
        dec bx
        cmp byte ptr [File_Open_Mode], 4
        ja short pass_openfile_check_com_file_sizelimit
        cmp byte ptr [EXE_ID], 'E'
        je short pass_openfile_check_com_file_sizelimit
        ; 14/08/2011
        or dx, dx
        jnz loc_openfile_insufficient_memory_xor_ah
        add ax, bx
        adc dx, 0
       ;call Rx_Dos_Div32
                    ; DX:AX = Sector count
                    ; DX = 0
        div cx  ; cx = Bytes per sector       
        xor dx, dx
        mov word ptr [Open_File_Sectors], ax
        mov ax, word ptr [Open_File_Size1]
        cmp ax, 65280 ; 65280 +256 (psp) > 65536
        ja short loc_openfile_insufficient_memory_xor_ah
        cmp ax, 64256 ; 65536-1024-256   
        jna short loc_fix_program_stackspace
        mov ax, 65535 
        jmp short  pass_fix_program_stackspace

loc_fix_program_stackspace:
       ; 14/08/2011
        add ax, 1024
        jmp short pass_fix_program_stackspace

pass_openfile_check_com_file_sizelimit:
       ; 14/08/2011 
        add ax, bx
        adc dx, 0
       ;call Rx_Dos_Div32
                    ; DX:AX = Sector count
                    ; DX = 0
        div cx  ; cx = Bytes per sector       
        xor dx, dx
        mov word ptr [Open_File_Sectors], ax
       ;mov bx, cx  ; Bytes per sector
       ;call proc_mul32
        mul cx

pass_fix_program_stackspace:
        mov word ptr [Open_File_Alloc_Size1], ax
        mov word ptr [Open_File_Alloc_Size2], dx
                        
        mov ax, word ptr [SI]+DirEntry_FstClusLO
        mov dx, word ptr [SI]+DirEntry_FstClusHI
                                         ; DX:AX = First cluster
        mov si, offset Open_Files_List
        mov cx, 32
loc_check_open_file_if_same_1:
        cmp byte ptr [SI]+1, ch ; 0, 09/10/2011 (bh -> ch)
        jne short pass_check_open_file_if_same_1
        cmp word ptr [SI]+2, ax
        jne short pass_check_open_file_if_same_1
        cmp word ptr [SI]+4, dx
        jne short pass_check_open_file_if_same_1

        mov di, word ptr [SI]+6
        mov es, di
        xor di, di
        cmp byte ptr [DI], 0CDh
        je  short pass_check_open_file_if_same_1
  
        add di, 4 ; OF_VolumeID
        mov ax, word ptr ES:[DI]
        mov dx, word ptr ES:[DI]+2

        cmp ax, word ptr [Current_VolSerial1]
        jne short loc_openfile_allocate_memory
        cmp dx, word ptr [Current_VolSerial2]
        jne short loc_openfile_allocate_memory
        mov al, byte ptr [File_Open_Mode]
        cmp al, 4
        jnb short loc_openfile_allocate_memory
        mov ah, 05h  ; Access denied
        xchg ah, al
        retn

pass_check_open_file_if_same_1:
        add si, OFL_EntrySize
        loop loc_check_open_file_if_same_1

loc_openfile_allocate_memory:
        mov ax, 352 ; 22*16, beginning segment
        call proc_get_free_memory
             	; AX= Total free allocation units (100h bytes)
            	; DX= Max. Free consequtive allocation units (100h bytes)
        	; CX= Total usable (conventional TRDOS) memory (100h bytes)
   	        ; BX= First free segment (valid if < FFFFh)
                ; cf= 1 -> no free memory 
        jnc short loc_open_file_compare_memory_size
loc_openfile_insufficient_memory_xor_ah:
        xor ah, ah
loc_openfile_insufficient_memory:
        mov al, 08h ; Insufficient memory 
        stc
        retn 

loc_openfile_for_rw_via_buffer:
       ; 15/08/2010
        mov ax, 352 ; 22*16, beginning segment
        call proc_get_free_memory
             	; AX= Total free allocation units (100h bytes)
            	; DX= Max. Free consequtive allocation units (100h bytes)
        	; CX= Total usable (conventional TRDOS) memory (100h bytes)
   	        ; BX= First free segment (valid if < FFFFh)
                ; cf= 1 -> no free memory 
        jc loc_openfile_insufficient_memory_xor_ah

       ; 25/09/2010
        mov cx, word ptr [OpenFile_BytesPerSec]
        mov al, ch 
        inc al ; + Open File Description Table size
        cbw
        cmp dx, ax
        jb loc_openfile_insufficient_memory_xor_ah
       ;         
        mov word ptr [Allocation_Size], ax ; 100h bytes
       ;mov byte ptr [Open_File_Type], 60h
        xor ax, ax
        mov word ptr [Open_File_Alloc_Size1], ax ; 0
        mov word ptr [Open_File_Alloc_Size2], ax ; 0
        inc ah ; mov ah, 1
       ;mov cx, word ptr [OpenFile_BytesPerSec]
        add cx, 256
       ;mov al, byte ptr [Open_File_Type]
        mov al, 60h
        add al, byte ptr [File_OFL_Number]
       ;xor dx, dx ; 05/07/2011, segment boundary check is not necessary  
        mov dx, 0FF01h ; 09/07/2011, 512 byte boundary check
                       ; 'and' result must not be zero (dh>0)
                       ; because -> PSP or OFDT is 256 bytes 
        call proc_allocate_memory
        ;BX = Open File Handle
        jmp short loc_openfile_set_ofl_entry

loc_open_file_compare_memory_size:
        push bx
        push dx
        mov ax, word ptr [Open_File_Alloc_Size1]
        mov dx, word ptr [Open_File_Alloc_Size2]
        mov cx, 100h
        call Rx_Dos_Div32
        ;or dx, dx
        pop dx
        pop bx
       ;jnz short loc_openfile_insufficient_memory_xor_ah

        cmp ax, dx      ; dx =  Max. Free consequtive allocation units
        jnb short loc_openfile_insufficient_memory_xor_ah
                        ; dx >= ax + 1 (100h = OFDT or PSP size)

        inc ax ; + OFDT or PSP size 

        mov word ptr [Allocation_Size], ax 

        mov byte ptr [Open_File_Type], 40h
        cmp byte ptr [File_Open_Mode], 5 ; Read file (full open)
        jne short loc_openfile_allocate_file_space1
loc_openfile_allocate_file_space0:
        add byte ptr [Open_File_Type], 20h
loc_openfile_allocate_file_space1:
        mov ax, word ptr [Open_File_Alloc_Size1]
        mov dx, word ptr [Open_File_Alloc_Size2]
       ; 26/10/2010 BugFix 
       ; (Add OFDT/PSP size to allocation size)
        add ax, 100h
        adc dx, 0
loc_openfile_allocate_file_space2:
       ; 30/05/2011 
        or dx, dx
        jz short loc_openfile_allocate_file_space3

loc_openfile_big_file_allocation:
       ; 30/05/2011
        mov cx, word ptr [Allocation_Size] ; 100h bytes
        xor ah, ah ; mov ah, 0
        jmp short loc_openfile_allocate_file_space4

loc_openfile_allocate_file_space3:
       ; 30/05/2011
        mov cx, ax
        mov ah, 1
loc_openfile_allocate_file_space4:
        mov al, byte ptr [Open_File_Type]
        add al, byte ptr [File_OFL_Number]
        mov dx, 0FF01h ; 05/07/2011, 512 byte boundary check
                       ; 'and' result must not be zero (dh>0)
                       ; because -> PSP or OFDT is 256 bytes 
        call proc_allocate_memory
        jc loc_openfile_insufficient_memory_xor_ah ; 06/07/2011 

loc_openfile_set_ofl_entry:
        mov word ptr [Open_File_Handle], bx
        mov al, byte ptr [File_OFL_Number]
        dec al 
        mov ah, OFL_EntrySize
        mul ah
        mov si, offset Open_Files_List
        add si, ax
        mov al, byte ptr [File_Owner]
        mov byte ptr [SI], al

        mov al, byte ptr [Open_File_Drive]
        mov byte ptr [SI]+1, al
        mov ax, word ptr [Open_File_FClust1]
        mov dx, word ptr [Open_File_FClust2]
        mov word ptr [SI]+2, ax
        mov word ptr [SI]+4, dx
        mov word ptr [SI]+6, bx ; Handle (PSP or OFDT)

       ; 20/10/2010
        mov al, byte ptr [File_Open_Mode]
        cmp al, 2
        jna short loc_openfile_set_ofdt
        cmp al, 4
        ja short loc_openfile_set_ofdt

loc_openfile_set_psp:
        ; input -> bx = program (psp) segment
        call proc_create_psp
        ; return -> bx = program (psp) segment
        mov es, bx
        mov di, 4
        mov al, byte ptr [File_OFL_Number]
        mov byte ptr ES:[DI], al
       ;xor al, al
       ;mov byte ptr ES:[DI]+1, al  
        mov ax, word ptr [Current_VolSerial1]
        mov dx, word ptr [Current_VolSerial2]
        mov word ptr ES:[DI]+2, ax
        mov word ptr ES:[DI]+4, dx 

        mov ax, word ptr [Open_File_Size1]
        mov dx, word ptr [Open_File_Size2]
        mov cx, 16
        call Rx_Dos_Div32
                    ; DX:AX = Count of paragraphs
                    ; DX = 0
        mov dx, es
        add ax, dx
        shr di, 1   ; mov di, 2
        mov word ptr ES:[DI], ax

loc_openfile_load_file:
       ; 19/06/2011
        mov bx, es
        add bx, 10h
        mov es, bx  

        mov cx, word ptr [Open_File_Sectors]
        mov ax, word ptr [Open_File_FClust1]
        mov dx, word ptr [Open_File_FClust2]
        mov bh, byte ptr [Open_File_Drive]
        xor bl, bl

        call proc_load_file
        jnc short loc_openfile_load_file_ok

loc_open_file_check_load_error:
       ;21/05/2011
        push ax
        mov ax, word ptr [Open_File_Handle]
        call proc_close_file
        pop ax
        xor cx, cx
        stc
        retn     

loc_openfile_load_file_ok:
       ; 19/06/2011
        sub ax, 10h ; OFDT segment

pass_openfile_load_file:
        push ds
        pop es

        ; 22/11/2009
        mov cx, word ptr [Allocation_Size]
        ; 08/11/2011
        mov bx, offset Open_File_Size 

        retn

loc_openfile_set_ofdt:
        mov es, bx
        mov byte ptr ES:[OF_Type], 'F'
        mov byte ptr ES:[OF_OpenMode], al ; 20/10/2010 BugFix 
        mov al, byte ptr [File_Owner]
        mov byte ptr ES:[OF_OWNER], al ; 0 => Mainprog/Kernel
       ;mov al, byte ptr [File_OFL_Number]
       ;mov byte ptr ES:[OF_OFL_SN], al
       ;mov al, byte ptr [Open_File_Drive]
       ;mov byte ptr ES:[OF_Drive], al
        mov ax, word ptr [File_OFL_Number]
        mov word ptr ES:[OF_OFL_SN], ax
        mov ax, word ptr [Current_VolSerial1]
        mov dx, word ptr [Current_VolSerial2]
        mov word ptr ES:[OF_VolumeId], ax
        mov word ptr ES:[OF_VolumeId]+2, dx
        mov bx, word ptr [OpenFile_BytesPerSec]
        mov word ptr ES:[OF_BytesPerSec], bx
        mov ax, word ptr [Open_File_Size1]
        mov dx, word ptr [Open_File_Size2]
        mov word ptr ES:[OF_Size], ax
        mov word ptr ES:[OF_Size]+2, dx
        xor ax, ax
        mov byte ptr ES:[OF_WriteStatus], 0FFh ; Reset
        mov word ptr ES:[OF_Pointer], ax
        mov word ptr ES:[OF_Pointer]+2, ax

        mov di, OF_LongName 
        mov cx, 64
       
        cmp byte ptr [FindFile_LongNameYes], 0
       ; 19/06/2011 
        ja short loc_openfile_move_longname

pass_openfile_move_longname:
        ; AL = 0
        rep stosb

loc_openfile_move_direntry:
        mov di, OF_DirEntry
        mov si, offset FindFile_DirEntry
        mov cx, 21
        rep movsw

        mov al, byte ptr [Open_File_FATType]
        mov byte ptr ES:[OF_FATType], al

loc_openfile_move_cdir:
        mov bh, byte ptr [Open_File_Drive]
        xor bl, bl
        mov si, offset Logical_DOSDisks
        add si, bx
        add si, LD_CDirLevel
        mov di, OF_CDIR_Level
        movsb
        mov cx, 64
        rep movsw

       ;26/09/2010
        cmp byte ptr [File_Open_Mode], 3
       ;20/10/2010
        jnb loc_openfile_load_file
        clc
        mov ax, es
        jmp pass_openfile_load_file

loc_openfile_move_longname:
        mov si, offset LongFileName  

        ; DS:SI : Unicode LongName
        ; ES:DI : Destination
        ; CX: Destination name length (max.)
        call proc_convert_longname_from_unicode

        jmp short loc_openfile_move_direntry

proc_open_file endp


proc_create_psp proc near
        ; 24/09/2011 
        ; 03/09/2011 -> SetEnv_Segment    
        ; 16/01/2011
        ; INPUT -> BX = PSP segment
        ; OUTPUT -> AX, CX, DI will be changed 

        mov word ptr [DTA_Address], 80h
        mov word ptr [DTA_Address]+2, bx

        push es
        mov es, bx
        xor di, di 
       ; 24/09/2011    
        mov ax, 20CDh
        stosw  
        mov ax, word ptr [Allocation_Size]
        shl ax, 1
        shl ax, 1
        shl ax, 1
        shl ax, 1
        add ax, bx    
        stosw 
        xor ax, ax
        mov cl, 3
        rep stosw
        mov ax, offset run_com_int22h_handler
        stosw
        mov ax, cs
        stosw    
        mov ax, offset run_com_int23h_handler
        stosw
        mov ax, cs
        stosw    
        mov ax, offset run_com_int24h_handler
        stosw
        mov ax, cs
        stosw    
        xor ax, ax ; 24/09/2011 
        stosw ; pspParentId = 0 for TRDOS Kernel
       ; 
        mov cl, 10
        stosw
        mov ax, word ptr [SetEnv_Segment]
        stosw
        xor ax, ax
        stosw
        stosw    
        mov al, 32
        stosw 
        xor al, al
        stosw
        stosw
        mov cl, 12
        rep stosw
        mov ax, 21CDh ; INT 21h
        stosw
        mov al, 0CBh ; RETF  
        stosb
        xor ax, ax ; 24/09/2011
        stosb
        mov cx, 86
        stosw 
        pop es

        retn  

proc_create_psp endp


proc_convert_longname_from_unicode proc near
        ; 1/11/2009 Temporary
        ;
        ; cx = max. length
loc_pclfu:       
        lodsw
        stosb 
        or al, al
        jnz short retn_from_pclfu
        loop loc_pclfu
retn_from_pclfu:
        retn

proc_convert_longname_from_unicode endp


proc_load_file  proc near
              ; 19/06/2011 ES:DI -> ES = File buffer segment
              ; 'DMA crossed 64 KB segment boundary' bugfix  
              ; 04/06/2011
              ; 20/01/2011 
              ; 19/01/2011 BugFix
              ; 16/01/2010 Load FS File
              ; 07/12/2009 -> error code in al (it was in ah)
              ; 07/11/2009
              ; INPUT ->
              ; ES = File Buffer Segment
              ; DX:AX = First Cluster Number
              ; BL = 0
              ; BH = DRV
              ; CX = Sector Count
              ; OUTPUT ->
              ; AX = File Buffer Segment
              ; STC -> Error Code in AL

             
                mov si, offset Logical_DOSDisks
                add si, bx

                cmp byte ptr [SI][LD_FATType],0
                ja short loc_load_FAT_file_sectors

               ;16/01/2010
                call proc_load_fs_file

                retn

loc_load_FAT_file_sectors:
               ; 19/06/2011
                xor bx, bx
                mov word ptr [LoadFile_BufferOff], bx
               ; 20/01/2011
               ; 19/01/2011 
                mov bx, es
                mov word ptr [LoadFile_Segment], bx
                push bx ; Segment

                mov word ptr [LoadFile_SectorCount], cx
                xor bh, bh
                mov bl, byte ptr [SI][LD_BPB][SecPerClust]
                mov word ptr [LoadFile_SecPerClust], bx

loc_load_FAT_file_next_sectors:
                cmp cx, bx
                jna short loc_loadfile_calculate_address
                mov cx, bx
loc_loadfile_calculate_address:
                mov word ptr [LoadFile_Cluster], ax
                mov word ptr [LoadFile_Cluster]+2, dx
                sub ax, 2
                sbb dx, 0
                call proc_mul32
                add ax, word ptr [SI][LD_DATABegin]
                adc dx, word ptr [SI][LD_DATABegin]+2
                mov bx, word ptr [LoadFile_BufferOff]
                ; 04/06/2011
                ; cx <= Sectors per Cluster
                call proc_disk_read
                jc short loc_load_file_stc_retn

                sub word ptr [LoadFile_SectorCount], cx
                jna short loc_load_file_retn

               ; 04/06/2011
                mov ax, word ptr [SI][LD_BPB][BytesPerSec]
                mul cx
                add ax, word ptr [LoadFile_BufferOff]
                adc dl, 0
                mov word ptr [LoadFile_BufferOff], ax
                mov al, 10h
                mul dl
                mov dx, word ptr [LoadFile_Segment]
                add dh, al
                mov word ptr [LoadFile_Segment], dx
               ;
                mov ax, word ptr [LoadFile_Cluster]
                mov dx, word ptr [LoadFile_Cluster]+2
                call proc_get_next_cluster
                jnc short loc_cont_load_file
                cmp ax, 0
                jna short loc_load_file_retn

                stc
loc_load_file_stc_retn:
               ; 20/01/2011
                pop dx ; Segment (ES input)
                retn 
loc_load_file_retn:
               ; 20/01/2011
                pop ax ; Segment (ES input)
                retn
loc_cont_load_file:
               ;04/06/2011
                mov bx, word ptr [LoadFile_Segment]
                mov es, bx
               ; 
                mov bx, word ptr [LoadFile_SecPerClust]
                mov cx, word ptr [LoadFile_SectorCount] 
                jmp short loc_load_FAT_file_next_sectors

LoadFile_SectorCount: dw 0
LoadFile_BufferOff: dw 0
LoadFile_Segment: dw 0
LoadFile_SecPerClust: dw 0
LoadFile_Cluster: dd 0

proc_load_file  endp


proc_close_file proc near
        ; 26/02/2011 BugFix (allocation type)
        ; 22/08/2010
        ; 17/11/2009
        ; INPUT -> 
        ;      AX = PSP Address, OFDT Address, File Handle
        ; OUTPUT ->
        ;      ES = DS, AX= PSP/OFDT Segment, BX= OFL Number
        ;      SI = Open File List, DI = 0, 
        ; 22/08/2010
        ; STC -> Invalid file handle (CX = PSP address)
        ;        AX = 06h
  
       ; 02/10/2010
        ; or ax, ax
        ; jz short loc_close_file_inv_handle   
loc_close_file_rfh_reset:       
        cmp ax, word ptr [RF_Handle]
        jne short loc_close_file_wfh_reset
        mov word ptr [RF_Handle], 0
loc_close_file_wfh_reset:
        cmp ax, word ptr [WF_Handle]
        jne short loc_close_file_pass_rwfh_reset
        mov word ptr [WF_Handle], 0

loc_close_file_pass_rwfh_reset:
        ; 22/08/2010
        mov es, ax
        xor bh, bh
        ; 22/06/2011 
        xor di, di
        mov bl, byte ptr ES:[DI]+OF_OFL_SN
        mov si, offset Open_Files_List
        or bl, bl
        jz short loc_close_file0
        cmp bl, 32
        ja short loc_close_file0    
       ;
        dec bl
        mov al, OFL_EntrySize
        mul bl
        add si, ax
        mov ax, es
        
        ;26/02/2011  
        inc bl
        ;22/08/2010
        cmp ax, word ptr [SI]+OFL_Handle
        je short loc_close_file1

        mov si, offset Open_Files_List 

loc_close_file0:
        push ds
        pop es
loc_close_file_inv_handle:
        mov cx, ax
        mov ax, 06h ; Invalid handle
        xor di, di
        stc
        retn

loc_close_file1:
       push ds
       push es
       push bx
 
       mov byte ptr [SI]+OFL_Drive, 0FFh
       cmp byte ptr ES:[DI], 0CDh
       mov ax, 140h  
       je short loc_closefile1_set_al_40h
loc_closefile1_set_al_60h:
       add al, 20h
loc_closefile1_set_al_40h:
       add al, bl  ; ah = 1
      ; 26/02/2011 BugFix 
       mov bx, es
       call proc_deallocate_memory
       push ds
       pop es 
       xor ax, ax
       mov word ptr [SI]+OFL_Handle, ax

       mov cx, 32
       mov si, offset Open_Files_List
loc_close_file2:
       mov bx, word ptr [SI]+OFL_Handle
       or bx, bx
       jz short pass_closefile_2
       cmp byte ptr [SI]+OFL_Drive, 0FFh
       je short pass_closefile_2
       mov al, byte ptr [SI] ; Owner
       or al, al
       jz short pass_closefile_2 
      ;cmp al, 32
      ;ja short pass_closefile_2
       dec al
       mov ah, OFL_EntrySize
       mul ah
       mov di, offset Open_Files_List
       add di, ax
       cmp word ptr [DI]+OFL_Handle, 0
       jna short pass_closefile_check_FFh
       cmp byte ptr [DI]+OFL_Drive, 0FFh
       jne short pass_closefile_2
pass_closefile_check_FFh:       
       mov byte ptr [SI]+OFL_Drive, 0FFh
pass_closefile_2:
       add si, OFL_EntrySize
       loop loc_close_file2

       mov cl, 32
       mov si, offset Open_Files_List
       xor di, di
loc_close_file3:
       cmp byte ptr [SI]+OFL_Drive, 0FFh
       jne short pass_closefile_3
       mov bx, word ptr [SI]+OFL_Handle
       or bx, bx
       jz short pass_closefile_3 
       mov es, bx
       cmp byte ptr ES:[DI], 0CDh
       mov ax, 0140h  
       je short loc_closefile3_set_al_40h
loc_closefile3_set_al_60h:
       add al, 20h
loc_closefile3_set_al_40h:
       add al, byte ptr ES:[DI]+OF_OFL_SN
      ; mov bx, es
       push cx
       call proc_deallocate_memory
       pop cx
       push ds
       pop es 
      ;xor di, di
       mov word ptr [SI]+OFL_Handle, di
pass_closefile_3:
       add si, OFL_EntrySize
       loop loc_close_file3

       mov si, offset Open_Files_List
       xor ax, ax
       mov cl, 32
     ; 22/08/2010
       mov di, si
loc_closefile_reset_OFL_entry:      
       cmp byte ptr [DI]+OFL_Drive, 0FFh
       jne short pass_closefile_reset_OFL_entry
       push cx 
       mov cx, OFL_EntrySize ; 8
       rep stosb
       pop cx 
pass_closefile_reset_OFL_entry:
       loop loc_closefile_reset_OFL_entry
      ;
       xor di, di
       pop bx 
       pop ax ; pushed ES, PSP/OFDT segment
       pop es ; pushed DS

       retn

proc_close_file endp


proc_delete_file proc near
                ; 10/04/2011 Directory entry deleting code
                ; is moved into proc_delete_directory_entry
                ; procedure in DIR.ASM (for rename file compatibility)
                ;
                ; 16/10/2010
                ;  'loc_del_update_parent_dir_lm_date' part
                ; of the procedure moved to DIR.ASM
                ; as 'proc_update_parent_dir_lmdt'
                ;
                ; 11/09/2010 'unlink clusters' part of the
                ;   procedure moved to DRV_FAT.ASM
                ;       as 'proc_truncate_cluster_chain 
                ;
                ; 21/08/2010 'calculate free space' modif.
                ; 09/08/2010 modification
                ; moved from CMD_INTR.ASM 'del' command
                ; procedure
                ;
                ; INPUT -> 
                ;       ES:DI = Directory Buffer Entry Offset
                ;          CX = Dir Buff Entry Counter
                ;          BL = Longname Entry Length
                ;          BH = Logical DOS Drv Number 
                ;

                ; 10/04/2011
                xor al, al
                mov ah, bh 

                mov si, offset Logical_DosDisks
                add si, ax

              ; 12/04/2011 
              ; 09/05/2010
                cmp byte ptr [SI][LD_FATtype], 1
                jnb short loc_del_fat_file 
              
                cmp byte ptr [SI][LD_FSType], 0A1h
                je short loc_del_fs_file

loc_del_file_invalid_format:
                xor ah, ah
                mov al, 0Bh ; Invalid Format
                stc 
                retn

loc_del_fs_file:
                ;28/02/2010
                call proc_delete_fs_file
                retn

loc_del_fat_file:
                call proc_delete_directory_entry
                jc short loc_del_file_err_retn 

loc_delfile_unlink_cluster_chain:
                call proc_truncate_cluster_chain
                ;jc short loc_del_file_err_retn

loc_delfile_return:
loc_del_file_err_retn:
                retn

proc_delete_file endp

proc_check_filename proc near
               ; 07/08/2010 push si, pop si
               ; 10/07/2010
               ; Derivation from proc_check_filename
               ; in the old TRDOS.ASM 09/02/2005
               ;
               ; INPUT -> DS:SI = Dot File Name Location
               ; OUTPUT -> stc -> error code in AL
               ;           AL= 0Bh -> Invalid file name   
               ; OUTPUT -> clc -> valid file name
               ; 
               ; AX, CX, DI will be changed
               ; (BX, DX, ES will not be changed)
               ;  
                call proc_check_invalid_filename_chars
                jc short loc_check_fn_rtn

                push si

loc_check_filename_cont:
                mov cx, 8
loc_check_filename_next_char:
                mov al, byte ptr [SI]
                cmp al, 2Eh
                jne short pass_check_fn_dot_check
loc_check_filename_ext_0:
                inc si
                mov al, byte ptr [SI]
                cmp al, 21h
                jc short loc_check_fn_pop_si_rtn
                cmp al, 2Eh
                jne short loc_check_filename_ext_1
loc_check_fn_stc_rtn:
                stc
loc_check_fn_pop_si_rtn:
                pop si
loc_check_fn_rtn:
                retn
loc_check_filename_ext_1:
                inc si
                mov al, byte ptr [SI]
                cmp al, 21h
                jc short loc_check_fn_clc_rtn
                cmp al, 2Eh
                je short loc_check_fn_stc_rtn
                inc si
                mov al, byte ptr [SI]
                cmp al, 21h
                jc short loc_check_fn_clc_rtn
                cmp al, 2Eh
                je short loc_check_fn_stc_rtn
                inc si
                mov al, byte ptr [SI]
                cmp al, 21h
                jnc short loc_check_fn_stc_rtn
loc_check_fn_clc_rtn:
                pop si
                clc
                retn
pass_check_fn_dot_check:
                cmp al, 21h
                jc short loc_check_fn_clc_rtn
                inc si
                loop loc_check_filename_next_char
                mov al, byte ptr [SI]
                cmp al, 21h
                jc short loc_check_fn_clc_rtn
                cmp al, 2Eh
                jne short loc_check_fn_stc_rtn
                jmp short loc_check_filename_ext_0

proc_check_filename endp

proc_check_invalid_filename_chars proc near
               ; 10/07/2010
               ; Derivation from proc_check_invalid_filename_chars
               ; in the old TRDOS.ASM 09/02/2005

                ; INPUT: DS:SI = ASCIIZ FileName 
                ; OUTPUT:
                ; stc -> Invalid
                ; clc -> Valid
                ; 10/02/2010
                ; AX, CX, DI will be changed

                push es
                push ds
                pop es
                push si
                mov di, offset invalid_fname_chars
                mov al, byte ptr [SI]
check_filename_next_char:
                mov cx, sizeInvFnChars
                mov di, offset invalid_fname_chars
loop_scan_invalid_filename_char:
		scasb 
                je short loc_invalid_filename 
                loop loop_scan_invalid_filename_char
                inc si
                mov al, byte ptr [SI]
               ;10/07/2010
                cmp al, 1Fh  ; 20h and above 
                ja check_filename_next_char
                xor ah, ah 
loc_check_invalid_filename_retn:
                pop si
                pop es
                retn
loc_invalid_filename:
              ; 10/07/2010
                mov ax, 0Bh ; Invalid format
                            ; Invalid file name chars
                stc
                jmp short loc_check_invalid_filename_retn

invalid_fname_chars:
		db 22h, 27h, 28h, 29h, 2Ah, 2Bh, 2Ch, 2Fh
		db 3Ah, 3Bh, 3Ch, 3Dh, 3Eh, 3Fh, 40h
		db 5Bh, 5Ch, 5Dh, 5Eh, 60h
sizeInvFnChars  equ ($ - invalid_fname_chars)                

proc_check_invalid_filename_chars endp

copy_source_file_to_destination_file proc near
                ; 02/09/2011
                ; 20/07/2011
                ; 19/07/2011 
                ; 17/07/2011
                ; 16/07/2011
                ; 15/07/2011
                ; 14/07/2011 
                ; 13/07/2011
                ; 10/07/2011
                ; 09/07/2011
                ; 19/06/2011
                ; 21/05/2011
                ; 19/05/2011
                ; 18/05/2011 Major Modification (Optimization)
                ;   Command Interpreter phase 1 enter ->
                ;           AL = 1 -> Caller is command interpreter
                ;           AL = 2 -> The second call, re-enter/continue
                ;   Phase 1 -> Check source file
                ;              'found' is required
                ;   Phase 2 -> Check destination file, 
                ;              save 'found' or 'not found' status
                ;              'permission denied' error will be return
                ;              if attributes have not for ordinary file 
                ;               without readonly attribute
                ;   Command Interpreter phase 1 return ->
                ;              DH = Source file attributes
                ;              DL = Destination file found status
                ;              AX = 0 
                ;   Command Interpeter phase 2 enter ->
                ;              AL = 2 -> Continue from the last position
                ;              AH = 
                ;   Phase 3 -> Open source file
                ;   Phase 4 -> Create destination file if it is not found
                ;   Phase 5 -> Open destination file
                ;   Phase 6 -> Read from source and write to destination
                ;   Phase 7 -> Close open files
                ;       cf=1 causes to return before the phase 7
                ;       but open file or files will be closed at return 
                ;
                ; INPUT -> DS:SI = Source File Pathname (Asciiz)
                ;          ES:DI = Destination File Pathname (Asciiz)
                ;         ; 15/05/2011
                ;          AL = 0 --> INT 21h
                ;          AL > 0 --> Command Interpreter (Question)
                ;          AL = 1 --> Question Phase
                ;          AL = 2 --> Progress Phase  
                ; OUTPUT -> clc -> OK
                ;           DX:AX = Destination file first cluster
                ;          ; 10/07/2011
                ;           CL > 0 if read file error before EOF
                ;           CH = open mode for source file (buffer/full) 
                ;   ES, BX, CX, SI, DI will be changed           
                ;           stc -> Error code in AL 
                ;
                ; 12/03/2011
                ; 28/11/2010
                ; 20/11/2010 BugFix
                ; 31/10/2010
                ; 29/10/2010 -> LMDT pointer (df lmdt = sf lmdt)
                ; 24/10/2010
                ; 17/10/2010
                ; 10/10/2010
                ; 22/08/2010
                ; 21/08/2010
                ; 14/08/2010
                ; 09/08/2010
                ; 07/08/2010
                ; 04/08/2010
                ;

               ; 18/05/2011
                cmp al, 2
               ; 21/05/2011
                je loc_csftdf2_check_cdrv
               ; 18/05/2011
                mov byte ptr CS:[copy_cmd_phase], al

                push es
                push di
loc_csftdf_parse_sf_path:
                mov ax, cs
                mov es, ax
               ; ds:si = Asciiz pathname 
                mov di, offset SourceFile_Drv
               ; 19/06/2011 
                call proc_parse_pathname
                jc short loc_csftdf_parse_sf_path_failed

loc_csftdf_parse_df_path:                
                pop si ; pushed di 
                pop ds ; pushed es
                ; es = cs 

loc_csftdf_sf_check_filename_exists:
                cmp byte ptr CS:[SourceFile_Name], 21h
                jb short loc_csftdf_sf_file_not_found_error

                mov di, offset DestinationFile_Drv
               ; 19/06/2011
                call proc_parse_pathname
                jnc short loc_csftdf_check_sf_cdrv
               
                cmp al, 1 ; File or directory name is not existing
                jna short loc_csftdf_check_sf_cdrv

loc_csftdf_parse_df_path_failed:
                stc 
loc_csftdf_push_cs_pop_ds_retn:
               ; 18/05/2011
                push cs
                pop ds
loc_csftdf_sf_error_retn: 
                retn

loc_csftdf_parse_sf_path_failed:                
               ; 19/06/2011
                pop di
                pop es
                jmp short loc_csftdf_push_cs_pop_ds_retn

loc_csftdf_sf_file_not_found_error:
                mov ax, 2 ; File not found 
                jmp short loc_csftdf_push_cs_pop_ds_retn

loc_csftdf_check_sf_cdrv:
                mov ax, cs
                mov ds, ax 

                mov bh, byte ptr [Current_Drv]
                mov byte ptr [csftdf_cdrv], bh

                mov dl, byte ptr [SourceFile_Drv]
                cmp dl, bh ; byte ptr [Current_Drv]
               ; 19/05/2011
                je short loc_csftdf_sf_check_directory

                call proc_change_current_drive
                jc short loc_csftdf_sf_error_retn

loc_csftdf_sf_check_directory:
               ; 19/05/2011 
                mov si, offset SourceFile_Directory
                cmp byte ptr [SI], 20h
                jna short loc_csftdf_find_sf

loc_csftdf_sf_change_directory:
                inc byte ptr [Restore_CDIR]
                xor ah, ah ; CD_COMMAND sign -> 0 
                call proc_change_current_directory
                jc short loc_csftdf_sf_error_retn

;loc_csftdf_sf_change_prompt_dir_string:
;               call proc_change_prompt_dir_string

loc_csftdf_find_sf:
                mov si, offset SourceFile_Name
                mov ax, 1800h ; Except volume label and dirs
                call proc_find_first_file
                jc short loc_csftdf_sf_error_retn

loc_csftdf_sf_ambgfn_check:
                cmp bx, 0 ; Ambiguous filename chars used sign (BX>0)
                ja short loc_csftdf_ambiguous_file_name_error

loc_csftdf_sf_found:
               ; 19/05/2011 
                mov word ptr [csftdf_filesize], ax
                mov word ptr [csftdf_filesize]+2, dx

                or ax, dx
                jz short loc_csftdf_sf_file_size_zero

loc_cstdf_check_sf_file_size_hw:
               ; 21/05/2011
                cmp dx, 8  ; DX:AX <= 8h:FFFFh = 576 KB
                ja short loc_check_sf_file_size_set_om_0
loc_check_sf_file_size_set_om_5:
               ; 19/05/2011
                mov byte ptr [csftdf_sf_openmode], 5 ; full open
                jmp short loc_csftdf_set_source_file_direnry

loc_csftdf_sf_file_size_zero:
                mov ax, 0Eh ; TRDOS zero length error
                stc
                retn

loc_check_sf_file_size_set_om_0:
                mov byte ptr [csftdf_sf_openmode], 0 ; sector buffer
              
loc_csftdf_set_source_file_direnry:
               ; 18/05/2011
                push ds
                pop es 
               ;mov si, offset FindFile_drv
               ;mov di, offset SourceFile_drv
               ;mov cx, 64
                mov si, offset FindFile_DirEntry
                mov di, offset SourceFile_DirEntry
                mov cx, 16
                rep movsw

loc_csftdf_sf_restore_cdrv:
                mov dl, byte ptr [csftdf_cdrv]
                cmp dl, byte ptr [Current_Drv]
                je short loc_csftdf_sf_restore_cdir
                call proc_change_current_drive 
                jnc short loc_csftdf_df_check_filename_exists

                retn   

loc_csftdf_ambiguous_file_name_error:
                mov ax, 2 ; File not found error
                stc
                retn

loc_csftdf_sf_restore_cdir:
                cmp byte ptr [Restore_CDIR], 0
                jna short loc_csftdf_df_check_filename_exists
               ; 19/05/2011
                mov ax, offset Logical_DosDisks
                add ah, dl ; byte ptr [csftdf_cdrv]
                mov si, ax
 		call proc_restore_current_directory
                jc short loc_csftdf_df_error_retn

loc_csftdf_df_check_filename_exists:
                cmp byte ptr [DestinationFile_Name], 20h
                ja short loc_csftdf_check_df_cdrv

loc_csftdf_copy_sf_name:
                mov di, offset DestinationFile_Name
                mov si, offset SourceFile_Name
                mov cx, 12
loc_csftdf_df_copy_sf_name_loop:
                lodsb
                stosb
                or al, al
                jz short loc_csftdf_check_df_cdrv             
                loop short loc_csftdf_df_copy_sf_name_loop

loc_csftdf_check_df_cdrv:
                mov dl, byte ptr [DestinationFile_Drv]
                cmp dl, byte ptr [Current_Drv]
               ; 19/05/2011
                je short loc_csftdf_df_check_directory

                call proc_change_current_drive
                jc short loc_csftdf_df_error_retn

loc_csftdf_df_check_directory:
               ; 19/05/2011
                mov si, offset DestinationFile_Directory
                cmp byte ptr [SI], 20h
                jna short loc_csftdf_find_df

loc_csftdf_df_change_directory:
                inc byte ptr [Restore_CDIR]
                xor ah, ah ; CD_COMMAND sign -> 0 
                call proc_change_current_directory
                jc short loc_csftdf_df_error_retn

;loc_csftdf_df_change_prompt_dir_string:
;               call proc_change_prompt_dir_string

loc_csftdf_find_df:
               ; 21/05/2011 
                push ds
                mov si, offset DestinationFile_Name
                xor ax, ax ; DestinationFile_AttributesMask -> any/zero
                call proc_find_first_file
                pop es 
                jc short loc_csftdf_df_check_error_code

loc_csftdf_df_ambgfn_check:
                cmp bx, 1 ; Ambiguous filename chars used sign (BX>0)
                cmc
                jc short loc_csftdf_df_error_retn
               
loc_csftdf_df_found:
                mov byte ptr [DestinationFileFound], 1

                and cl, 1Fh ; Attributes, D-V-S-H-R
               ; 19/05/2011
                jz short loc_csftdf_df_save_first_cluster

loc_csftdf_df_permission_denied_retn:                 
                mov ax, 05h ; Access/Permisson denied.
loc_csftdf_df_error_stc_retn:
                stc
loc_csftdf_df_error_retn:
                retn

loc_csftdf_df_check_error_code:
                cmp al, 2
                jne short loc_csftdf_df_error_stc_retn

                mov byte ptr [DestinationFileFound], 0

loc_csftdf_check_disk_free_size_0:
                mov ax, word ptr [SourceFile_DirEntry]+DirEntry_FileSize
                mov dx, word ptr [SourceFile_DirEntry]+DirEntry_FileSize+2

loc_csftdf_check_disk_free_size_1:
                mov bx, offset Logical_DosDisks
                add bh,  byte ptr [DestinationFile_Drv]
                mov si, bx
                mov cx, word ptr [SI][LD_FS_BytesPerSec] ; 17, LD_BPB + 0Bh
                mov bx, cx
                dec bx
                add ax, bx
                adc dx, 0
                call Rx_Dos_Div32

loc_csftdf_check_disk_free_size_hw:
                cmp dx, word ptr [SI][LD_FreeSectors]+2
                jb short loc_csftdf_check_disk_free_size_ok
                ja short loc_csftdf_df_insufficient_disk_space

loc_csftdf_check_disk_free_size_lw:
                cmp ax, word ptr [SI][LD_FreeSectors]
                jb short loc_csftdf_check_disk_free_size_ok
                ja short loc_csftdf_df_insufficient_disk_space

                cmp byte ptr [SI][LD_FATType], 0 ; FS needs FDT sector also.
                ja short loc_csftdf_check_disk_free_size_ok 

loc_csftdf_df_insufficient_disk_space:
                mov ax, 27h ; insufficient disk space
                jmp short loc_csftdf_df_error_stc_retn

loc_csftdf_df_save_first_cluster:
               ; 19/05/2011
               ; DS:SI = FindFile_DirEntry (for the old destination file)
               ; DX:AX = Old destination file size
loc_csftdf_df_check_sf_df_fcluster:
               ; 21/05/2011  
                push dx
                push ax
                mov ax, word ptr [SI]+DirEntry_FstClusLO
                mov word ptr[csftdf_df_fcluster], ax
                mov dx, word ptr [SI]+DirEntry_FstClusHI
                mov word ptr[csftdf_df_fcluster]+2, dx
loc_csftdf_df_check_sf_df_fcluster_1:
                cmp ax, word ptr [SourceFile_DirEntry]+DirEntry_FstClusLO
                jne short loc_csftdf_df_check_sf_df_fcluster_ok
loc_csftdf_df_check_sf_df_fcluster_2:               
                cmp dx, word ptr [SourceFile_DirEntry]+DirEntry_FstClusHI
                jne short loc_csftdf_df_check_sf_df_fcluster_ok
loc_csftdf_df_check_sf_df_drv:
                mov al, byte ptr [SourceFile_Drv]
                cmp al, byte ptr [DestinationFile_Drv]
                jne short loc_csftdf_df_check_sf_df_fcluster_ok
                pop ax
                pop dx
                xor ax, ax ; mov ax, 0 -> Bad command or file name !
                stc
                retn    
loc_csftdf_df_check_sf_df_fcluster_ok:
                pop ax
                pop dx
   
loc_csftdf_df_move_findfile_struct:
              ; 21/05/2011
               ; push ds
               ; pop es 
               ;mov si, offset FindFile_drv
               ;mov di, offset DestinationFile_drv
               ;mov cx, 64
               ; mov si, offset FindFile_DirEntry
                mov di, offset DestinationFile_DirEntry
                mov cx, 16
                rep movsw
               
loc_csftdf_check_disk_free_size_2:
               ; 18/05/2011                
                mov cx, word ptr [SourceFile_DirEntry]+DirEntry_FileSize
                mov bx, word ptr [SourceFile_DirEntry]+DirEntry_FileSize+2
                sub cx, ax ; DX:AX = Old destination file size
                sbb bx, dx
                jc short loc_csftdf_check_disk_free_size_ok
                mov ax, cx
                mov dx, bx
                jmp short loc_csftdf_check_disk_free_size_1    

loc_csftdf_check_disk_free_size_ok:
               ; 19/05/2011
loc_csftdf_df_restore_cdrv:
                mov dl, byte ptr [csftdf_cdrv]
                cmp dl, byte ptr [Current_Drv]
                je short loc_csftdf_df_restore_cdir
                call proc_change_current_drive 
                jnc short loc_csftdf_df_check_copy_cmd_phase

                retn   

loc_csftdf_df_restore_cdir:
                cmp byte ptr [Restore_CDIR], 0
                jna short loc_csftdf_df_check_copy_cmd_phase

                mov ax, offset Logical_DosDisks
                add ah, dl ; byte ptr [csftdf_cdrv]
                mov si, ax
 		call proc_restore_current_directory
                jc short loc_csftdf_return

loc_csftdf_df_check_copy_cmd_phase:
                mov al, byte ptr [copy_cmd_phase]
                cmp al, 1
               ; 21/05/2011
                jne short loc_csftdf2_check_cdrv
                
                xor ax, ax
                mov byte ptr [copy_cmd_phase], al ; 0

                mov dl, byte ptr [DestinationFileFound]            
                mov dh, byte ptr [SourceFile_DirEntry]+11 ; Attributes
 
loc_csftdf_return:               
                retn

loc_csftdf2_check_cdrv:
               ; 19/05/2011
                mov dh, byte ptr [Current_Drv]
                mov dl, byte ptr [SourceFile_Drv]
                cmp dl, dh
                je short loc_csftdf2_sf_check_directory

loc_csftdf2_change_drv:
                call proc_change_current_drive
                jc short loc_csftdf_return

loc_csftdf2_sf_check_directory:
               ; 19/05/2011
                mov si, offset SourceFile_Directory 
                cmp byte ptr [SI], 20h
                jna short loc_csftdf2_findload_file

loc_csftdf2_sf_change_directory:
                inc byte ptr [Restore_CDIR]
                xor ah, ah ; CD_COMMAND sign -> 0 
                call proc_change_current_directory
                jc short loc_csftdf2_return

;loc_csftdf2_change_prompt_dir_string:
;               call proc_change_prompt_dir_string

loc_csftdf2_findload_file:
                ; 10/07/2011
                xor ah, ah
                mov byte ptr [csftdf_read_err], ah ; 0 

                ; 19/05/2011
                mov al, byte ptr [csftdf_sf_openmode]
                or al, al
                jz short loc_csftdf2_open_file_mode_0 

loc_csftdf2_open_file_mode_5:
               ; 21/05/2011
                mov si, offset SourceFile_Name
               ; 19/05/2011 
               ;mov ax, 05h ; ah=0 -> Kernel is the file's owner
                mov al, 05h ; Full open for reading
                call proc_open_file
                jnc short loc_csftdf2_save_open_source_file_handle

               ;cmp ax, 08h ; ax=08h -> Insufficient memory
                cmp al, 08h ; Insufficient memory 
                jne short loc_csftdf2_im_stc_retn
                
                xor al, al ; Open for reading (via sector buffer)

loc_csftdf2_open_file_mode_0:
               ; 21/05/2011
                mov si, offset SourceFile_Name
               ; 19/05/2011
               ;mov ax, 0
                call proc_open_file
                jc short loc_csftdf2_return

loc_csftdf2_save_open_source_file_handle:
               ; 22/08/2010
                mov word ptr [csftdf_sf_filenumber], ax
               ; 09/08/2010
               ;mov word ptr [PSP_Address], ax
               
               ; 10/10/2010
               ; push ds
               ; mov ds, ax
               ; mov si, OF_Size  ; ptr to Open file size
               ; mov ax, word ptr [SI]
               ; mov dx, word ptr [SI]+2
               ; pop ds  ; pushed ds
                
               ; mov word ptr [csftdf_filesize], ax
               ; mov word ptr [csftdf_filesize]+2, dx

               ; or ax, ax
               ; jnz short loc_csftdf2_check_destination_file
               ; or dx, dx
               ; jnz short loc_csftdf2_check_destination_file

               ; 22/08/2010
               ; mov ax, word ptr [csftdf_sf_filenumber]
               ; call proc_close_file
               ;

               ; mov ax, 0Eh ; 0Eh = msdos reserved error code
                    ; TRDOS error code 0Eh = Zero length                  
;loc_csftd2f_stc_retn:
;                stc
;loc_csftdf2_return:
;                retn

loc_csftdf2_check_destination_file:
               ; 19/05/2011
loc_csftdf2_df_drv:
                mov dl, byte ptr [DestinationFile_Drv]
                cmp dl, byte ptr [Current_Drv]
                je short loc_csftdf2_df_check_directory

                call proc_change_current_drive
                jnc short loc_csftdf2_df_check_directory

loc_csftdf2_stc_return:
               ; 22/08/2010
                push ax 
                mov ax, word ptr [csftdf_sf_filenumber]
                call proc_close_file
                pop ax
loc_csftdf2_im_stc_retn:
                stc
loc_csftdf2_return:
                retn

loc_csftdf2_df_check_directory:
                mov si, offset DestinationFile_Directory  
                cmp byte ptr [SI], 20h
                jna short loc_csftdf2_df_check_found_or_not

loc_csftdf2_df_change_directory:
                inc byte ptr [Restore_CDIR]
                xor ah, ah ; CD_COMMAND sign -> 0 
                call proc_change_current_directory
                jc short loc_csftdf2_stc_return

;loc_csftdf2_df_change_prompt_dir_string:
;                  call proc_change_prompt_dir_string

loc_csftdf2_df_check_found_or_not:
               ; 19/05/2011
                mov si, offset DestinationFile_Name

                cmp byte ptr [DestinationFileFound], 0 
                ja short loc_csftdf2_open_destination_file

loc_csftdf2_create_file:
               ; 10/10/2010
                push si

                mov ax, word ptr [csftdf_filesize]
                mov dx, word ptr [csftdf_filesize]+2

                mov bx, 0FFFFh ; 10/07/2011
               ; bx = FFFFh -> If drive has FAT fs
               ; create empty file with only 1 file cluster

                ; 11/09/2010
                ; INPUT ->
                ; DX:AX -> File Size
                ; DS:SI = ASCIIZ File name
                ; CL = file attributes (12/03/2011)
                ; 10/07/2011
                ; BX = FFFFh -> empty file sign for FAT fs
                ; BX <> FFFFh -> use file size for FAT fs 
                ;
                ; OUTPUT ->
                ; DX:AX = New file's first cluster
                ; DS:SI = Logical Dos Drv Descr. Table Addr.
                ; DS:BX = offset CreateFile_Size
                ; CL = Sectors per cluster, CH = 0 
                ; stc -> error code in AL
                ; 
                xor cl, cl ; 12/03/2011 
                call proc_create_file

                pop si

               ; 31/10/2010 
                jc short loc_csftdf2_stc_return

loc_csftdf2_create_file_OK:
              ; 21/08/2010
                mov word ptr [csftdf_df_fcluster], ax
                mov word ptr [csftdf_df_fcluster]+2, dx

loc_csftdf2_open_destination_file:
               ; 10/10/2010
              ; DS:SI = ASCIIZ file name
                mov ax, 1 ; Open file for writing
                call proc_open_file
                jc short loc_csftdf2_stc_return

loc_csftdf2_save_destination_file_ofn:
               ; 22/08/2010
                mov word ptr [csftdf_df_filenumber], ax

loc_csftdf2_reset_wf_percent_pointers:
; 17/07/2011
xor ax, ax
mov byte ptr [WF_Percentage_Yes], al
mov byte ptr [WF_64K_Percentage], al
mov word ptr [wfsc], ax
mov word ptr [wf64ksc], ax
mov word ptr [wftsc], ax
mov byte ptr [wfppercent], 0

mov ax, word ptr [csftdf_filesize]
mov dx, word ptr [csftdf_filesize]+2
add ax, 511
adc dx, 0
or dx, dx
jnz short loc_csftdf2_reset_wf_percent_ptr_chk_64k
cmp ax, 5120 ; 10 sectors
jb short loc_csftdf2_write_file
jmp short loc_csftdf2_reset_wf_percent_pointers_1s

loc_csftdf2_reset_wf_percent_ptr_chk_64k:
cmp dx, 4
ja short loc_csftdf2_reset_wf_percent_ptr_64k
cmp byte ptr [Current_drv], 2 ; c:
jnb short loc_csftdf2_write_file

loc_csftdf2_reset_wf_percent_ptr_64k:
cmp dx, 1FF
jna short loc_csftdf2_reset_wf_percent_pointers_1s 
inc byte ptr [WF_64K_Percentage]
mov ax, dx
jmp short loc_csftdf2_reset_wf_percent_pointers_OK

loc_csftdf2_reset_wf_percent_pointers_1s:
mov cx, 512
div cx
loc_csftdf2_reset_wf_percent_pointers_OK:
inc byte ptr [WF_Percentage_Yes]
mov word ptr [wftsc], ax
mov ah, 0Fh
int 10h
; xor bl, bl
mov Word ptr [WF_VideoPage], bx
mov ah, 03h
int 10h
mov word ptr [WF_CursorPos], dx

               ; 10/10/2010 
loc_csftdf2_write_file:
                cmp byte ptr [csftdf_sf_openmode], 5
                je loc_csftdf2_write_full_open_file

loc_csftdf2_read_sf_write_df:
               ; 10/10/2010
loc_csftdf2_deallocate_file_rw_buffer_1:
               ; 10/10/2010
                mov bx, word ptr [File_rw_buffer]
                or bx, bx
                jz short loc_csftdf2_get_free_memory
                mov ah, 1
                mov al, 08h ; File r/w buffer sign  
                call proc_deallocate_memory
                mov word ptr [File_rw_buffer], 0
             
loc_csftdf2_get_free_memory:
                mov ax, 352 ; 22*16, beginning segment
                call proc_get_free_memory
             	; AX= Total free allocation units (100h bytes)
            	; DX= Max. Free consequtive allocation units (100h bytes)
        	; CX= Total usable (conventional TRDOS) memory (100h bytes)
   	        ; BX= First free segment (valid if < FFFFh)
                ; cf= 1 -> no free memory 
                ; 13/07/2011
                jc loc_csftdf2_insufficient_memory

loc_csftdf2_compare_memory_size:
               ; 10/10/2010
                mov ax, 100h ; 256 bytes = 1 allocation unit
                mul dx 
                cmp dx, word ptr [csftdf_filesize]+2
                jb short loc_csftdf2_compare_memory_size_less 
                ja short loc_csftdf2_compare_memory_size_more
                cmp ax, word ptr [csftdf_filesize]
                jna short loc_csftdf2_compare_memory_size_OK
loc_csftdf2_compare_memory_size_more:
                mov ax, word ptr [csftdf_filesize]
                mov dx, word ptr [csftdf_filesize]+2
loc_csftdf2_compare_memory_size_less:
loc_csftdf2_compare_memory_size_OK:
                or dx, dx
                jnz short loc_csftdf2_cmp_memory_size_64k_fix
                mov cx, ax
                jmp short loc_csftdf2_cmp_memory_size_pass_64k_fix                   
loc_csftdf2_cmp_memory_size_64k_fix:
                mov cx, 0FFF0h  ; This is 65520 byte but 
                        ; 65536 bytes will be allocated
                mov dx, 1
                xor ax, ax
loc_csftdf2_cmp_memory_size_pass_64k_fix:                
                mov word ptr [csftdf_rw_size], ax
                mov word ptr [csftdf_rw_size]+2, dx
                ; BX = First free segment
                ; CX = Allocation size in bytes
                mov ah, 1
                mov al, 08h ; File R/W buffer sign
                or dl, ah ; 1 = 05/07/2011, 512 byte boundary check 
                call proc_allocate_memory
                jc short loc_csftdf2_insufficient_memory

                mov word ptr [File_rw_buffer], bx
                mov es, bx
                xor bx, bx

loc_csftdf2_read_sf_to_rw_buff:
                mov cx, word ptr [csftdf_rw_size]
                mov dx, word ptr [csftdf_rw_size]+2
loc_csftdf2_read_sf_to_rw_buff_next: ;09/07/2011
                mov ax, word ptr [csftdf_sf_filenumber]
                ; DL:CX = number of bytes to read
                ; AX = File number/handle
                ; DH = 0 
                ; DL -> 0 to 8 (64K*DL+CX = bytes to read)
                ; ES:BX = Buffer
                call proc_read_file
                jnc short loc_csftdf2_write_df_from_rw_buff

                ; File pointer is moved
                ; DL:CX = Read bytes (same with input)
                ; DH = FFh -> EOF
                ; AX, BX, ES, DS same with input values
                ; stc -> DL:CX = read count (< requested count)
                ;        DH = FFh -> EOF
                ;        DH <> FFh -> Read error (general failure)
                ;
              ; 20/11/2010
                cmp dh, 0FFh
                je short loc_csftdf2_write_df_from_rw_buff_rerr
                or cx, cx
                jnz short loc_csftdf2_write_df_from_rw_buff_rerr
                or dl, dl
                jnz short loc_csftdf2_write_df_from_rw_buff_rerr
                xor ah, ah
                mov al, dh  ; error code in AL
              ; 28/11/2010
                or al, al
              ; 13/07/2011
                jnz short loc_csftdf2_copy_error_retn_dealloc_rwb
              ; 28/11/2010
                mov al, 25 ; 19h = msdos: Seek error 
                           ;      (trdos: File data pointing error)
              ; 13/07/2011  
                jmp short loc_csftdf2_copy_error_retn_dealloc_rwb

loc_csftdf2_write_df_from_rw_buff_rerr:
              ; 10/07/2011
loc_csftdf2_write_df_from_rw_buff:
                mov byte ptr [csftdf_read_err], dh ; 10/07/2011
                mov ax, word ptr [csftdf_df_filenumber]
                ; DL:CX = number of bytes to write
                ; AX = File number/handle
                ; DH > 0 -> EOF after write 
                ; DL -> 0 to 8 (64K*DL+CX = bytes to write)
                ; ES:BX = Buffer
               
                ; 29/10/2010 ->  DI = pointer to LMDT of the file 
                mov di, offset SourceFile_DirEntry + DirEntry_WrtTime
                
                call proc_write_file
               ; 13/07/2011
                jc short loc_csftdf2_copy_error_retn_dealloc_rwb
                
                ; File pointer is moved
                ; DL:CX = Written bytes (same with input)
                ; DH = FFh -> EOF
                ; AX, BX, ES, DS same with input values
                ;
               ; 10/07/2011 
                or dh, dh
                jz short loc_csftdf2_read_sf_to_rw_buff_next
                mov al, byte ptr [csftdf_read_err]
                or al, al
                jz short loc_csftdf2_read_sf_to_rw_buff_xor_dh
               ; 20/07/2011
                cmp al, 0FFh
                jne short loc_csftdf2_copy_error_retn_dealloc_rwb
                inc byte ptr [csftdf_read_err] ; 0
                jmp short loc_csftdf2_write_file_OK

loc_csftdf2_read_sf_to_rw_buff_xor_dh:
              ; 10/07/2011   
                xor dh, dh
                jmp short loc_csftdf2_read_sf_to_rw_buff_next

loc_csftdf2_insufficient_memory:
               ; 10/10/2010
               ; 14/07/2011 
                mov al, 08h ; Insufficient memory
loc_csftdf2_insufficient_memory_xor_ah:
                xor ah, ah
               ; 13/07/2011
                push ax 
                jmp short loc_csftdf2_copy_error_retn

loc_csftdf2_write_full_open_file:
               ; 10/10/2010
                mov ax, word ptr [csftdf_sf_filenumber]
                add ax, 10h
                mov es, ax
                xor bx, bx
                mov ax, word ptr [csftdf_df_filenumber]
                mov cx, word ptr [csftdf_filesize]
                mov dl, byte ptr [csftdf_filesize]+2
                mov dh, 0FFh ; EOF after write
                ; DX:CX = number of bytes to write
                ; AX = File number/handle
                ; DH > 0 -> EOF after write 
                ; DL -> 0 to 10h (64K*DL+CX = bytes to write)
                ; ES:BX = Buffer

               ; 29/10/2010 ->  DI = pointer to LMDT of the file 
                mov di, offset SourceFile_DirEntry + DirEntry_WrtTime
 
                call proc_write_file
               ; 13/07/2011
		jnc short loc_csftdf2_write_full_open_file_OK
               
               ; 30/10/2010
                xor ah, ah
                mov al, dh 

loc_csftdf2_copy_error_retn_dealloc_rwb:
               ; 15/07/2011
               ; 13/07/2011
                cbw
                push ax
                call loc_csftdf2_deallocate_file_rw_buffer_2
                 
loc_csftdf2_copy_error_retn:
               ; 02/09/2011
                cmp byte ptr [WF_Percentage_Yes], 0
                jna short loc_csftdf2_copy_error_pn_retn
                mov si, offset nextline
                call proc_printmsg  
               
               ; 17/07/2011
                mov byte ptr [WF_Percentage_Yes], 0
loc_csftdf2_copy_error_pn_retn:
               ; 19/05/2011
                mov ax, word ptr [csftdf_df_filenumber]
                call proc_close_file
                mov ax, word ptr [csftdf_sf_filenumber]
                call proc_close_file
                pop ax
                stc
                retn

loc_csftdf2_deallocate_file_rw_buffer_2:
               ; 13/07/2011
               ; 21/05/2011
                mov bx, word ptr [File_rw_buffer]
                ;or bx, bx
                ;jz short loc_csftdf2_deallocate_file_rw_buffer_retn
                mov ah, 1
                mov al, 08h ; File r/w buffer sign  
                call proc_deallocate_memory
                mov word ptr [File_rw_buffer], 0
loc_csftdf2_deallocate_file_rw_buffer_retn:
               ;13/07/2011
                retn

loc_csftdf2_write_file_OK:
               ; 13/07/2011
                call loc_csftdf2_deallocate_file_rw_buffer_2
loc_csftdf2_write_full_open_file_OK:
               ; 17/07/2011
                mov byte ptr [WF_Percentage_Yes], 0
               ; 21/05/2011 
                mov ax, word ptr [csftdf_df_filenumber]
                call proc_close_file
                mov ax, word ptr [csftdf_sf_filenumber]
                call proc_close_file
               
                mov ax, word ptr [csftdf_df_fcluster]
                mov dx, word ptr [csftdf_df_fcluster]+2

               ; 10/07/2011
                mov cl, byte ptr [csftdf_read_err]
                mov ch, byte ptr [csftdf_sf_openmode]

                or ch, ch
                ; jz -> source file has been copied via buffer
                ; jnz -> full open source file has been copied
 
                retn

; 19/05/2011
copy_cmd_phase: db 0
DestinationFileFound: db 0
; 18/05/2011
csftdf_cdrv: db 0
; 10/10/2010
csftdf_sf_openmode: db 0
;22/08/2010
csftdf_sf_filenumber: dw 0
csftdf_df_filenumber: dw 0
;10/10/2010
csftdf_filesize: dd 0
csftdf_df_fcluster: dd 0
csftdf_rw_size: dd 0
; 10/07/2011
csftdf_read_err: db 0

copy_source_file_to_destination_file endp

proc_create_file proc near
                ; 03/09/2011 
                ; 07/08/2011
                ; 01/08/2011
                ; 10/07/2011 create empty file for FAT file system
                ; 30/04/2011
                ; 12/03/2011 file attributes input
                ; 06/11/2010
                ; 02/11/2010
                ; 31/10/2010 -> err retn -> freespace calculation
                ; 16/10/2010
                ; 10/10/2010 
                ; 11/09/2010 
                ; 'loc_createfile_update_fat_fcluster' modif.
                ; 'loc_createfile_check_free_sectors_hw' bug fix
                ; 21/08/2010 'calculate free space' modification
                ;         and CL = SecperClust, CH = 0 return
                ; 15/08/2010 multi cluster set/reset
                ; 14/08/2010 (dir entry & first cluster set/reset)
                ; 09/08/2010
                ; derivation from proc_make_directory (24/07/2010)
                ;
                ; INPUT ->
                ; DX:AX -> File Size
                ; DS:SI = ASCIIZ File name   (DS = CS)
                ; CL = File Attributes 
                ; BX = FFFFh -> create empty file (only for FAT fs) 
                ;
                ; OUTPUT ->
                ; DX:AX = New file's first cluster
                ; DS:SI = Logical Dos Drv Descr. Table Addr.
                ; DS:BX = offset CreateFile_Size
                ; CL = Sectors per cluster, CH = 0 
                ; stc -> error code in AL
                ; 

               ; 12/03/2011
;               test cl, 18h    (directory or volume name)
;               jnz short loc_createfile_access_denied
                and cl, 07h ; S, H, R
                mov byte ptr [Createfile_attrib], cl 

               ;14/08/2010
                push si
                push bx ; 10/07/2011
                mov bh, byte ptr [Current_Drv]
                xor bl, bl
                mov si, offset Logical_DOSDisks
                add si, bx
                pop bx        

               ; 10/07/2010 -> 1st writable disk check for trdos
               ; LD_DiskType = 0 for write protection (read only) 
                cmp byte ptr [SI][LD_DiskType], 1 ; 0 = Invalid
                jnb short loc_createfile_check_file_sytem
                mov ax, 13h ; MSDOS err => Disk write-protected 
                mov dx, 0
                ; err retn: DX = 0, BX= File name offset
                ; DS:SI -> Dos drive description table address                
                retn
;loc_createfile_access_denied:
               ; 12/03/2011
;               mov ax, 05h ; access denied (invalid attributes input)
;               stc
;               retn

loc_createfile_check_file_sytem:
               ; 03/09/2011    
                mov word ptr [createfile_size], ax
                mov word ptr [createfile_size]+2, dx
               ;
                cmp byte ptr [SI][LD_FATType], 1
                jb loc_create_fs_file

loc_createfile_check_empty_FAT_file_sign:
               ; 10/07/2011
               ; bx = FFFFh -> create empty file if drive has FAT fs
                 mov cl, byte ptr [SI][LD_BPB]+SecPerClust
                 mov byte ptr [createfile_SecPerClust], cl
                 inc bx ; FFFFh -> 0 
                 or bx, bx
                 jnz short loc_createfile_check_free_sectors_hw
                 xor ax, ax
                 xor dx, dx
loc_createfile_check_free_sectors_hw:
               ; 10/07/2011
                pop bx ; pushed si, ASCIIZ file name address
               ; 03/09/2011
               ; 11/09/2010  
                cmp word ptr [SI][LD_FreeSectors]+2, 0
                ja short loc_create_fat_file_xor_cx
                   
loc_createfile_check_free_sectors_lw:
               ; 14/08/2010
                mov cx, word ptr [SI][LD_FreeSectors]
                or ch, ch
                jnz short loc_create_fat_file
                ;cmp cl, byte ptr [SI][LD_BPB]+SecPerClust
                ;10/07/2011
                cmp cl, byte ptr [createfile_SecPerClust]
                jnb short loc_create_fat_file                  
loc_createfile_insufficient_disk_space:
               ; 23/04/2011        
               ; 31/10/2010 
                mov ax, 27h
loc_createfile_gffc_retn:
                retn
               ;
loc_create_fat_file_xor_cx:
               ; 03/09/2011
                xor cx, cx
loc_create_fat_file:
               ; 03/09/2011 
               ; 14/08/2010
                mov word ptr [createfile_Name_Offset], bx
                mov word ptr [createfile_FreeSectors], cx
               ; mov al, byte ptr [SI][LD_BPB]+SecPerClust
               ; mov byte ptr [createfile_SecPerClust], al

loc_createfile_gffc_1:
                call proc_get_first_free_cluster
                jc short loc_createfile_gffc_retn

              ; 14/08/2010
                mov word ptr [createfile_FFCluster], ax
                mov word ptr [createfile_FFCluster]+2, dx

loc_createfile_locate_ffe_on_directory:
               ; Current directory fcluster <> Directory buffer cluster
               ; Current directory will be reloaded by
               ; proc_locate_current_dir_file procedure
               ;
               ; DS:SI = Logical Dos Drv Desc. Table Adress
                push si ; 30/04/2011 
                xor ax, ax
                mov cx, ax
                dec cx ; FFFFh  
                ; CX = FFFFh -> find first deleted or free entry
                 ; DS:SI would be ASCIZZ filename Address if the call
                 ; would not be for first free or deleted dir entry  
                call proc_locate_current_dir_file
                jnc loc_createfile_set_ff_dir_entry
                pop si ; 30/04/2011
                 ; DS:SI = Logical DOS Drv. Description Table Address 
                cmp ax, 2
                je short loc_createfile_add_new_cluster
loc_createfile_locate_file_stc_retn:
               ; 23/04/2011
                stc
                retn

loc_createfile_add_new_cluster:
               ; 14/08/2010
                cmp byte ptr [Current_FATType], 2
               ;cmp byte ptr [SI][LD_FATType], 2
                ja short loc_createfile_add_new_cluster_check_fsc
                cmp byte ptr [Current_Dir_Level], 1
               ;cmp byte ptr [SI][LD_CDirLevel], 1
                jnb short loc_createfile_add_new_cluster_check_fsc
               ; 09/08/2010 
                mov al, 12 ; No more files 
               ;
loc_createfile_anc_retn:
               ; 31/10/2010
                retn

loc_createfile_add_new_cluster_check_fsc:
               mov cx, word ptr [createfile_FreeSectors]
             ; 11/09/2010
               or cx, cx ; cx = 0 -> free sectors > 65535
               jz short loc_createfile_add_new_subdir_cluster
             ;
             ; xor ah, ah 
               mov al, byte ptr [createfile_SecPerClust]
               shl ax, 1 ; AX = 2 * AX
               cmp cx, ax
              ; 31/10/2010
               jb short loc_createfile_insufficient_disk_space

loc_createfile_add_new_subdir_cluster:
               ; 14/08/2010 modification
               ;mov ax, word ptr [DirBuff_Cluster]
               ;mov dx, word ptr [DirBuff_Cluster]+2 
               ;mov word ptr [createfile_LastDirCluster], ax                
               ;mov word ptr [createfile_LastDirCluster]+2, dx

                push word ptr [DirBuff_Cluster]
                pop word ptr [createfile_LastDirCluster]                
                push word ptr [DirBuff_Cluster]+2
                pop word ptr [createfile_LastDirCluster]+2       

                mov ax, word ptr [createfile_FFCluster]
                mov dx, word ptr [createfile_FFCluster]+2
                call proc_load_FAT_sub_directory 
                jc short loc_createfile_anc_retn

pass_createfile_add_new_subdir_cluster:
                mov es, bx
                xor di, di
                mov ax, 256 ; word
                mul cx ; cx =  directory buffer sector count
                mov cx, ax
                xor ax, ax ; 0
                rep stosw  ; AX to ES:DI as CX times (DI= DI+2) 
                push ds
                pop es
               ; 01/08/2011
                mov byte ptr [DirBuff_validData], 2 
                call proc_save_directory_buffer
                jc short loc_createfile_anc_retn

loc_createfile_save_added_subdir_cluster:
               ; 14/08/2010
                mov ax, word ptr [createfile_LastDirCluster]
                mov dx, word ptr [createfile_LastDirCluster]+2
                mov cx, word ptr [createfile_FFCluster]
                mov bx, word ptr [createfile_FFCluster]+2
                call proc_update_cluster
                ; 18/07/2010
                jnc short loc_createfile_save_fat_buffer_0
                or ax, ax ;AX = 0 -> cluster value is 0 or eoc
                jnz short loc_createfile_save_fat_buffer_stc_retn

loc_createfile_save_fat_buffer_0:
               ;
                mov ax, word ptr [createfile_FFCluster]
                mov dx, word ptr [createfile_FFCluster]+2

               ; 14/08/2010
                mov word ptr [createfile_LastDirCluster], ax
                mov word ptr [createfile_LastDirCluster]+2, dx
               ;
                xor cx, cx
                dec cx
                mov bx, cx ; FFFFh
                call proc_update_cluster
                ; 18/07/2010
                jnc short loc_createfile_save_fat_buffer_1
                or ax, ax
                jz short loc_createfile_save_fat_buffer_1
loc_createfile_save_fat_buffer_stc_retn:
                stc
loc_createfile_save_fat_buffer_retn:
loc_createfile_gffc_2_stc_retn:
                retn

loc_createfile_gffc_2:
              ; 14/08/2010
                call proc_get_first_free_cluster
              ; 31/10/2010 
                jc short loc_createfile_gffc_2_stc_retn

                mov word ptr [createfile_FFCluster], ax
                mov word ptr [createfile_FFCluster]+2, dx

               ; 15/08/2010
                mov word ptr [createfile_Cluster], ax
                mov word ptr [createfile_Cluster]+2, dx
               ;
                mov ax, word ptr [createfile_LastDirCluster]
                mov dx, word ptr [createfile_LastDirCluster]+2
               
                call proc_load_FAT_sub_directory 
                jc short loc_createfile_gffc_2_stc_retn

                mov es, bx
                xor di, di
              ;

loc_createfile_set_ff_dir_entry:
               ;or cx, cx
               ;jnz short loc_createfile_stc_pop_si_retn
              ; ES:DI -> Directory entry address
               mov si, word ptr [createfile_Name_Offset]
               mov ax, word ptr [createfile_FFCluster]
               mov dx, word ptr [createfile_FFCluster]+2

               mov ch, 0FFh
              ; 12/03/2011 
               mov cl, byte ptr [createfile_attrib] ; file attributes
                           ; CH > 0 -> File size is in DS:[BX]
               mov bx, offset createfile_size
  
               call proc_make_directory_entry

               pop si ; 30/04/2011

              ;push ds
              ;pop es

              ; 01/08/2011
               mov byte ptr [DirBuff_ValidData], 2 
               call proc_save_directory_buffer
               jc short loc_createfile_set_ff_dir_entry_retn

              ; 31/10/2010
                mov word ptr [WF_Counter], 0
              ; 23/04/2011
                jmp short loc_createfile_get_set_write_file_cluster

loc_createfile_stc_retn_cc_push_ax:
              ; 31/10/2010
               push ax
loc_createfile_stc_retn_cc:
               mov ax, word ptr [WF_Counter]
               or ax, ax
               jz short loc_createfile_stc_retn_pop_ax
               xor dx, dx
               mov bh, byte ptr [Current_Drv]
               mov bl, 02h ; BL= 2 -> subtract cluster
               call proc_calculate_FAT_freespace
              ; DS:SI =  Logical DOS Drive Desc. Tbl. Address 
              ;jc short loc_createfile_stc_retn_pop_ax_cf
               and cx, cx ; cx = 0 -> valid free sector count
               jz short loc_createfile_stc_retn_pop_ax
loc_createfile_stc_retn_recalc_FAT_freespace:
               mov bx, 0FF00h ; bh = 0FFh-> 
                              ; DS:SI = Logical DOS Drv DT Addr
                              ; bl = 0 -> Recalculate 
               call proc_calculate_FAT_freespace
               and cx, cx
               jz short loc_createfile_stc_retn_cc

loc_createfile_stc_retn_pop_ax:
               stc
loc_createfile_stc_retn_pop_ax_cf:
               pop ax
loc_createfile_set_ff_dir_entry_retn:
               retn 

loc_createfile_save_fat_buffer_1:
               ; Byte ptr [FAT_BuffValidData] = 2 
                call proc_save_fat_buffer
                jc short loc_createfile_save_fat_buffer_retn

                mov byte ptr [createfile_add_new_cluster], 1

                jmp loc_createfile_upd_dir_modif_date_time

               ; 07/08/2011 Relocation
;loc_createfile_stc_pop_si_retn:
               ; 30/04/2011
               ; pop si 
loc_createfile_stc_retn:
              ; 02/11/2010
loc_createfile_stc_retn_calc_FAT_freespace:
               cmp byte ptr [createfile_add_new_cluster], 1
               jb short loc_createfile_stc_retn_cc_push_ax
               stc
               retn

loc_createfile_get_set_write_file_cluster:
              ; 15/08/2010
                mov ax, word ptr [createfile_size]
                mov dx, word ptr [createfile_size]+2
                or dx, dx
                jnz short loc_createfile_get_set_wfc_cont
                or ax, ax
                jnz short loc_createfile_get_set_wfc_cont
                inc ax
                jmp loc_createfile_set_cluster_count 

loc_createfile_write_fcluster_to_disk:
                add ax, word ptr [SI][LD_DATAbegin]
                adc dx, word ptr [SI][LD_DATAbegin]+2
               ; 22/08/2010
                mov bx, offset DOSBootSectorBuff
               ; ES:BX = Sector Buffer
               ; CX = sectors per cluster

loc_createfile_wfc_to_disk_loop: 
               ; 14/08/2010
                push cx
                mov cx, 1
                call proc_disk_write
                pop cx
               ; 31/10/2010
                jc short loc_createfile_stc_retn
                loop loc_createfile_wfc_to_disk_loop

loc_createfile_update_fat_fcluster:
              ; 24/10/2010
               cmp word ptr [WF_Counter], 0
               jna short loc_createfile_update_fat_fcluster_nl
               mov ax, word ptr [createfile_PCluster]
               mov dx, word ptr [createfile_PCluster]+2
               mov cx, word ptr [createfile_Cluster]
               mov bx, word ptr [createfile_Cluster]+2
               call proc_update_cluster
               jnc short loc_createfile_update_fat_fcluster_nl
               or ax, ax ;AX = 0 -> cluster value is 0 or eoc
               jnz loc_createfile_stc_retn

loc_createfile_update_fat_fcluster_nl:
               ; 24/10/2010
               inc word ptr [WF_Counter]
              ;jz short loc_createfile_update_fat_fcluster_nl               
               mov ax, word ptr [createfile_Cluster]
               mov dx, word ptr [createfile_Cluster]+2
               xor cx, cx
               dec cx ; FFFFh
               mov bx, cx  ; FFFFh
               call proc_update_cluster
               jnc short loc_createfile_save_fat_buffer_2
               or ax, ax ;AX = 0 -> cluster value is 0 or eoc
               jz short loc_createfile_save_fat_buffer_2
loc_createfile_upd_fat_fcluster_stc_retn:
               ; 31/10/2010
               jmp loc_createfile_stc_retn

loc_createfile_get_set_wfc_cont:
                mov cx, 512 ; Bytes per sector                
                add ax, 511
                adc dx, 0
                call Rx_Dos_Div32
                xor ch, ch
                mov cl, byte ptr [createfile_SecPerClust]
                dec cl
                add ax, cx
                adc dx, 0
                inc cl 
                call Rx_Dos_Div32
                or dx, dx
                jnz short loc_createfile_wnc_inv_format_retn
loc_createfile_set_cluster_count:
                mov word ptr [createfile_CCount], ax
                mov si, offset Logical_DosDisks
                mov bh, byte ptr [Current_Drv]
                xor bl, bl 
                add si, bx                
                mov ax, word ptr [createfile_FFCluster]
                mov dx, word ptr [createfile_FFCluster]+2
                mov word ptr [createfile_Cluster], ax
                mov word ptr [createfile_Cluster]+2, dx
                mov byte ptr [createfile_add_new_cluster], 0
loc_createfile_get_set_write_file_fclust_cont:
               ; 24/10/2010
                push ax
               ; 22/08/2010
                mov di, offset DOSBootSectorBuff
               ;push ds
               ;pop es
                mov cx, 256
                xor ax, ax
                rep stosw  ; AX to ES:DI as CX times (DI= DI+2)
  	  	pop ax
               ;
                sub ax, 2
                sbb dx, 0
                xor bh, bh
                mov bl, byte ptr [createfile_SecPerClust]
                mov cx, bx
                call proc_mul32
                or bx, bx
               ; 24/10/2010 
                jz loc_createfile_write_fcluster_to_disk

loc_createfile_wnc_inv_format_retn:
                mov ax, 0Bh ; Invalid format
loc_createfile_wnc_stc_retn:
loc_createfile_save_fat_retn:
               ; 31/10/2010
               jmp loc_createfile_stc_retn

loc_createfile_save_fat_buffer_2:
               ; Byte ptr [FAT_BuffValidData] = 2
                call proc_save_fat_buffer
                jc loc_createfile_stc_retn

               ; 10/10/2010
                dec word ptr [createfile_CCount]
                jz short loc_createfile_upd_dir_modif_date_time

loc_createfile_get_set_write_next_cluster:
               ; 15/08/2010
                call proc_get_first_free_cluster
                jc loc_createfile_stc_retn
loc_createfile_get_set_write_next_cluster_1:
               ; 31/10/2010
                or dx, dx
                jz short loc_createfile_get_set_write_next_cluster_2
                cmp dx, 0FFFFh
                jne short loc_createfile_get_set_write_next_cluster_3
                cmp ax, 0FFFFh
                jb short loc_createfile_get_set_write_next_cluster_3
                mov ax, 27h ; Insufficient disk space
                jmp loc_createfile_stc_retn
                         
loc_createfile_get_set_write_next_cluster_2:
                cmp ax, 2
                jb short loc_createfile_wnc_inv_format_retn
             
loc_createfile_get_set_write_next_cluster_3:
               ; 24/10/2010
                mov cx, word ptr [createfile_Cluster]
                mov bx, word ptr [createfile_Cluster]+2
                mov word ptr [createfile_Cluster], ax
                mov word ptr [createfile_Cluster]+2, dx
                mov word ptr [createfile_PCluster], cx
                mov word ptr [createfile_PCluster]+2, bx
                jmp short loc_createfile_get_set_write_file_fclust_cont

loc_createfile_wnc_insufficient_disk_space:
                mov al, 27h ; Insufficient disk space
               ; 31/10/2010
               jmp loc_createfile_stc_retn

loc_createfile_upd_dir_modif_date_time:
              ; 16/10/2010
               call proc_update_parent_dir_lmdt

loc_createfile_calculate_FAT_freespace:
              ; 02/11/2010
              ; 24/10/2010
              ; 21/08/2010
              ; 14/08/2010
              ; 30/10/2010 
               cmp byte ptr [createfile_add_new_cluster], 0
               ja loc_createfile_gffc_2

loc_createfile_calculate_FAT_freespace_cc:
               mov ax, word ptr [WF_Counter]
               xor dx, dx
              ; 24/10/2010
               mov bh, byte ptr [Current_Drv]
               mov bl, 02h ; BL= 2 -> subtract cluster
               call proc_calculate_FAT_freespace
              ; DS:SI =  Logical DOS Drive Desc. Tbl. Address 
              ;21/08/2010
              ;jc short loc_createfile_retn
               and cx, cx ; cx = 0 -> valid free sector count
              ; 02/11/2010 
               jz short loc_createfile_retn_fcluster
loc_createfile_recalculate_FAT_freespace:
              ; 21/08/2010
               mov bx, 0FF00h ; bh = 0FFh-> 
                              ; DS:SI = Logical DOS Drv DT Addr
                              ; bl = 0 -> Recalculate 
               call proc_calculate_FAT_freespace
               and cx, cx
               jz short loc_createfile_calculate_FAT_freespace_cc
              ; 26/10/2010
               xor ch, ch   

loc_createfile_retn_fcluster:
               mov ax, word ptr [createfile_FFCluster]
               mov dx, word ptr [createfile_FFCluster]+2

               mov bx, offset createfile_size
              ; 21/08/2010
               mov cl, byte ptr [SI][LD_BPB][SecperClust]

loc_createfile_retn:
               retn

loc_create_fs_file:
               ; 03/09/2011 -> Add new (huge) sectors for huge file
                 pop bx  ; 10/07/2011, pushed si 
               ; DX:AX -> File Size
               ; DS:SI -> Logical DOS Drive DT
               ; BX -> File name offset as ASCIIZ name
                call proc_create_fs_file
                jc short loc_create_fs_file_retn

loc_create_fs_file_sc_chk:
               ; 03/09/2011  
                cmp cx, 65534
                jnb short loc_create_fs_file_huge1
                cmc
loc_create_fs_file_retn:
                retn  

loc_create_fs_file_huge1:
              ; 03/09/2011
                push dx
                push ax

                mov ax, word ptr [SI]+LD_FS_BytesPerSec
                mul cx

loc_create_fs_file_huge1_next:

cmp byte ptr [FSCF_PSActive], 0
jna short pass_create_fs_file_ans_ps0

loc_create_fs_file_ans_ps0:
push bx
push dx
push ax
mov dx, word ptr [FSCF_CursorPos]
mov bx, word ptr [FSCF_VideoPage]
mov ah, 2
int 10h
pop ax
pop dx
pop bx

pass_create_fs_file_ans_ps0:
                sub word ptr [createfile_size], ax
                sbb word ptr [createfile_size]+2, dx
                jc short loc_create_fs_file_clc_pop_retn
                mov ax, word ptr [createfile_size]
                mov dx, word ptr [createfile_size]+2
                push cx   
                mov cx, word ptr [SI]+LD_FS_BytesPerSec
                call Rx_DOS_Div32
                pop cx
                or dx, dx
                jnz short loc_create_fs_file_huge2
                or ax, ax
                jz short loc_create_fs_file_pop_retn
                inc ax
                jz short loc_create_fs_file_huge2
                dec ax
                jmp short loc_create_fs_file_huge3


loc_create_fs_file_huge2:
               ; 03/09/2011
                mov ax, 65534
loc_create_fs_file_huge3:
               ; 03/09/2011 
                mov cx, ax
                pop ax
                pop dx

loc_create_fs_file_ans_ps1:
; 03/09/2011
push bx

cmp byte ptr [FSCF_PSActive], 0
jna short loc_create_fs_file_ans

push ax

mov al, byte ptr [FSCF_PSign]
cmp al, "+"
jne short loc_create_fs_file_ans_ps2
mov al, "."
jmp short loc_create_fs_file_ans_ps3
loc_create_fs_file_ans_ps2:
mov al, "+"
loc_create_fs_file_ans_ps3:
mov byte ptr [FSCF_PSign], al
mov ah, 0Eh
mov bx, word ptr [FSCF_VideoPage]
int 10h

pop ax

loc_create_fs_file_ans:
              ; 03/09/2011
                call proc_add_new_fs_section            
                pop bx
                jc short loc_create_fs_file_retn
                push cs
                pop es
                push dx
                push ax
                mov ax, word ptr [SI]+LD_FS_BytesPerSec
                mul cx
                add word ptr [BX], ax
                adc word ptr [BX]+2, dx
                jmp short loc_create_fs_file_huge1_next

loc_create_fs_file_clc_pop_retn:
               ; 03/09/2011
                clc
loc_create_fs_file_pop_retn:
               ; 03/09/2011
                pop ax
                pop dx
                retn

; 12/03/2011
createfile_attrib: db 0 

; 14/08/2010
createfile_size: dd 0
; 09/08/2010
; followings are same with mkdir pointers
createfile_FFCluster: dd 0
createfile_Name_Offset: dw 0
createfile_add_new_cluster: db 0
createfile_Name: db 13 dup (0)
;
createfile_CDirFCluster: dd 0
createfile_cdirlevel: db 0
; 14/08/2010
createfile_SecPerClust: db 0
createfile_FreeSectors: dw 0
createfile_LastDirCluster: dd 0
; 15/08/2010
createfile_cluster: dd 0
; 24/10/2010
createfile_pcluster: dd 0
; 15/08/2010
createfile_ccount: dw 0

proc_create_file endp

proc_read_file proc near
                ; 21/07/2011 
                ; 12/07/2011 BugFix 
                ; 09/07/2011
                ; 21/05/2011
                ; 22/01/2011
                ; 09/01/2011 verification & BugFix -> OK
                ; 28/11/2010 BugFix 
                ; 20/11/2010 BugFix
                ; 23/10/2010
                ; 20/10/2010 BugFix
                ; 03/10/2010
                ; 02/10/2010 
                ; 26/09/2010 major modification
                ; 23/09/2010
                ; 22/09/2010 open mode 5 file read
                ; 20/09/2010 open mode 2 -> no fp moving
                ; 04/09/2010
                ; 28/08/2010
                ; 22/08/2010
                ;
                ; INPUT ->
                ;    DX:CX = number of bytes to read
                ;    AX = File number/handle
                ;    DH = 0 
                ;    DL -> 0 to 8 (64K*DL+CX = bytes to read)
                ;    ES:BX = Buffer
                ; OUTPUT ->
                ;    File pointer is moved
                ;    DX:CX = Read bytes (same with input)
                ;    DH = FFh -> EOF
                ;    SI, AX, BX, ES, DS same with input values
                ;    DI = 0
                ;   stc -> DL:CX = read count (< requested count)
                ;          DH = FFh -> EOF
                ;          DH <> FFh -> Read error (general failure)
                ;

loc_read_file_check_nob1:
               ; 03/10/2010 
                or cx, cx
                jnz short loc_read_file_check_nob2
                or dl, dl
                jz short loc_read_file_invalid_data
loc_read_file_check_nob2:
               ;cmp dx, 10h
               ; 21/05/2011
                cmp dx, 8
               ; 21/07/2011  
               ; ja short loc_read_file_invld_data_xor_cx
                ja short loc_read_file_invalid_data

loc_read_file_check_rf_handle:
               ; 22/09/2010
                mov ds, ax
                cmp ax, word ptr CS:[RF_Handle]
                je short loc_read_file_chk_open_mode

                mov word ptr CS:[RF_Handle], ax
             
               ; 26/09/2010
                mov al, byte ptr DS:[OF_Type]
                cmp al, 'F'
               ;je short loc_read_file_chk_open_mode
               ; 02/10/2010
                jne short loc_read_file_invalid_handle

                mov al, byte ptr DS:[OF_OFL_SN]
                or al, al
                jz short loc_read_file_invalid_handle
                cmp al, 32
                ja short loc_read_file_invalid_handle
                ;
                mov di, offset Open_Files_List
                dec al
                mov ah, OFL_EntrySize
                mul ah
                add di, ax
                mov ax, ds
                cmp ax, word ptr CS:[DI]+OFL_Handle
                je short loc_read_file_chk_open_mode

loc_read_file_invalid_handle:
              ; 21/07/2011  
                mov dh, 06h ; Invalid file handle/number
loc_read_file_invalid_drv_retn:
               ; 22/09/2010
                push cs
                pop ds
               ; 21/07/2011
                mov ax, word ptr [RF_Handle]
loc_read_file_invalid_data_retn: 
               ; 21/07/2011  
                xor dl, dl
                xor cx, cx
                xor di, di
                stc
                retn
 
;loc_read_file_invld_data_xor_cx:
               ; 21/07/2011
;               xor cx, cx
;               xor dl, dl    
loc_read_file_invalid_data:
               ; 21/07/2011  
                mov dh, 0Dh ; Data invalid
               ; 22/09/2010
                jmp short loc_read_file_invalid_data_retn

loc_read_file_chk_open_mode:
               ; 22/09/2010
                mov al, byte ptr DS:[OF_OpenMode]
                cmp al, 5
                je short loc_read_file_chk_volume_id
                or al, al
                jz short loc_read_file_chk_volume_id
                cmp al, 2
                jne short loc_read_file_invalid_handle

loc_read_file_chk_volume_id:
              ; 02/10/2010
               mov ah, byte ptr DS:[OF_Drive]  
               xor al, al
               mov di, offset Logical_DOSDisks
               add di, ax
               mov al, byte ptr CS:[DI]+LD_FATType
               or al, al
               jz short loc_read_file_chk_fs_volume_id
               cmp al, 2
               ja short loc_read_file_chk_fat32_volume_id
               add di, LD_BPB + VolumeID
               jmp short loc_read_file_chk_volume_id_cmpvlw  

loc_read_file_chk_fat32_volume_id:
               add di, LD_BPB + FAT32_VolID
               jmp short loc_read_file_chk_volume_id_cmpvlw  

loc_read_file_chk_fs_volume_id:
               add di, LD_FS_VolumeSerial

loc_read_file_chk_volume_id_cmpvlw:
               mov ax, word ptr CS:[DI]
               cmp ax, word ptr DS:[OF_VolumeId]
               jne short loc_read_file_invalid_drive

loc_read_file_chk_volume_id_cmpvhw:
               mov ax, word ptr CS:[DI]+2
               cmp ax, word ptr DS:[OF_VolumeId]+2
               je short loc_read_file_chk_valid_file_handle_OK

loc_read_file_invalid_drive:
              ; 21/07/2011 
               mov dh, 0Fh ; Invalid drive
               jmp short loc_read_file_invalid_drv_retn

loc_read_file_EOF_a_return:
               xor cx, cx
loc_read_file_xor_dl_stc_return:
               xor dl, dl
loc_read_file_stc_eof_return:
               mov dh, byte ptr [RWF_EOF]                      
loc_read_file_stc_return:
               mov ax, es
              ; 09/07/2011
               mov bx, word ptr [RWF_Buffer]+2
               mov es, bx
               mov bx, word ptr [RWF_Buffer]
              ; 
               xor di, di
               stc
               retn

loc_read_file_chk_valid_file_handle_OK:
               ; 22/09/2010 
                mov di, ds
                push cs
                pop ds
                mov byte ptr [RWF_EOF], 0FFh
               ; 09/07/2011
                mov word ptr [RWF_Buffer], bx
                mov bx, es
                mov word ptr [RWF_Buffer]+2, bx
               ;
                mov word ptr [RWF_NumberOfBytes], cx
                mov word ptr [RWF_NumberOfBytes]+2, dx
                mov es, di

loc_read_file_chk_fppos0:
              ; 26/09/2010
               mov ax, word ptr ES:[OF_Pointer]
               mov bx, dx
               mov dx, word ptr ES:[OF_Pointer]+2
loc_read_file_chk_fppos1:
               cmp dx, word ptr ES:[OF_Size]+2
               ja short loc_read_file_EOF_a_return
               jb short loc_read_file_chk_fppos3
loc_read_file_chk_fppos2:
               cmp ax, word ptr ES:[OF_Size]
               jnb short loc_read_file_EOF_a_return
loc_read_file_chk_fppos3:
              ; 28/11/2010 
               inc byte ptr [RWF_EOF] ; 0
              ; 
               or cx, cx
               jnz short loc_read_file_chk_fppos4
               or bl, bl  ; or bx, bx
              ; 09/07/2011
               jz short loc_read_file_xor_dl_stc_return

loc_read_file_chk_fppos4:
               add ax, cx  ; Number of bytes to read, lW
               adc dx, bx  ; Number of bytes to read, hW
              ; 21/07/2011 
               jc short loc_read_file_check_eof_yes   

loc_read_file_check_EOF:
                cmp dx, word ptr ES:[OF_Size]+2
                jb short loc_read_file_check_eof_no
                ja short loc_read_file_check_eof_yes
                cmp ax, word ptr ES:[OF_Size]
                jb short loc_read_file_check_eof_no
                je short loc_read_file_chk_open_mode_5
loc_read_file_check_eof_yes:
               ; 28/11/2010
                mov cx, word ptr ES:[OF_Size]
                mov bx, word ptr ES:[OF_Size]+2
                sub cx, word ptr ES:[OF_Pointer]
                sbb bx, word ptr ES:[OF_Pointer]+2

loc_read_file_chk_open_mode_5:
              ; 28/11/2010
               dec byte ptr [RWF_EOF]  ; 0FFh

loc_read_file_check_eof_no:
              ; 28/11/2010
               mov word ptr [RWF_NumberOfBytes], cx
               mov word ptr [RWF_NumberOfBytes]+2, bx
              ; 
               mov word ptr [RWF_SI], si
               cmp byte ptr ES:[OF_OpenMode], 5
               jne short loc_read_file_from_disk

loc_read_file_from_memory:
               ; 28/11/2010 BugFix
               ; 20/11/2010
               ; 03/10/2010 major modification
               ; 04/09/2010 jmp location names are changed
               ; 28/08/2010

                mov bx, word ptr [RF_Handle]
                add bx, 10h ; Read buffer segment (+256)
               ; 28/11/2010
                mov si, word ptr ES:[OF_Pointer]
                mov cx, word ptr ES:[OF_Pointer]+2
               ; 
                or cx, cx
                jz short loc_rfm_move_bytes
loc_rfm_move_bytes_upd_es_1000h:
                add bx, 1000h
                loop loc_rfm_move_bytes_upd_es_1000h
loc_rfm_move_bytes:
                mov ax, word ptr [RWF_NumberOfBytes]
                mov dx, word ptr [RWF_NumberOfBytes]+2
                mov ds, bx
                mov di, word ptr [RWF_Buffer]
                push word ptr [RWF_Buffer]+2     
                pop es
loc_rfm_move_bytes_next:
                movsb   ; 1 byte copy from DS:SI to ES:DI
                sub ax, 1
                sbb dx, 0
                or ax, ax
                jnz short loc_rfm_move_bytes_loop1
                or dl, dl ; dx, dx
                jz short loc_rfm_move_bytes_OK

loc_rfm_move_bytes_loop1:
                or si, si
                jnz short loc_rfm_move_bytes_loop2
                mov bx, ds
                add bx, 1000h
                mov ds, bx
loc_rfm_move_bytes_loop2:
                or di, di
                jnz short loc_rfm_move_bytes_next
                mov bx, es
                add bx, 1000h
                mov es, bx
                jmp short loc_rfm_move_bytes_next

loc_rfm_move_bytes_OK:
                push cs
                pop ds 
                mov ax, word ptr [RWF_NumberOfBytes]
                mov dx, word ptr [RWF_NumberOfBytes]+2

                jmp loc_read_file_move_fp_ok0

loc_read_file_load_buffer:
loc_read_file_from_disk:
                mov al, byte ptr ES:[OF_OFL_SN]
                dec al 
                mov ah, OFL_EntrySize
                mul ah
                mov si, offset Open_Files_List
                add si, ax
                mov ax, word ptr [SI]+OFL_Cluster    ; +2
                mov dx, word ptr [SI]+OFL_Cluster+2  ; +4
                mov word ptr [WF_FCluster], ax   ; WF_FDT
                mov word ptr [WF_FCluster]+2, dx ; WF_FDT

loc_rfd_convert_fp_to_fsector: 
               ; 09/07/2011 
                mov bx, offset Logical_DosDisks
                add bh, byte ptr ES:[OF_Drive]
                mov si, bx               

               ; 23/09/2010
                xor ax, ax 
                xor dx, dx
                mov word ptr [WF_Counter], ax ; 0
                mov word ptr [WF_Counter]+2, dx ; 0

                mov byte ptr ES:[OF_WriteStatus], al ;0

loc_rfd_convert_fp_to_fsector_next:              
                mov cx, word ptr ES:[OF_BytesPerSec]
                                           ; 512 or 2048
                add ax, word ptr ES:[OF_Pointer]
                adc dx, word ptr ES:[OF_Pointer]+2
                call Rx_DOS_Div32

              ; 12/09/2010
                mov cx, word ptr [WF_FCluster]   ; [WF_FDT]
                mov bx, word ptr [WF_FCluster]+2 ; [WF_FDT]+2

                cmp byte ptr [SI][LD_FATType], 0
                jna loc_rfd_fs_get_section_for_file_sector

loc_rfd_get_cluster_for_file_sector:
                  ;  DS:SI = Logical dos drv table
                  ;  DX:AX = Sector sequence number of the file
                  ;  BX:CX = First Cluster of the file
                call proc_get_cluster_for_file_sector
                  ;  DX:AX = Cluster number of the requested sector
                  ;  CX = Sector offset of the cluster
                  ;  BX = 0 
                  ;  DI = Sectors per cluster value
                  ;  stc & AX > 0 -> error except EOF
                  ;  stc & AX = 0 -> EOF ->
                  ;  Last cluster before the req. sector number
                  ;  BX:CX = The Last cluster number
                  ;  DX > 0 -> Required additional sectors
                  ; ES = same with input  
                
                jnc short loc_rfd_gcffs_no_error

loc_rfd_fs_get_section_ffs_chk_err:
                or ax, ax
                jnz short loc_rfd_stc_return_chk_rfcount

loc_rfd_stc_return_EOF:
                mov byte ptr [RWF_EOF], 0FFh

loc_rfd_stc_return_chk_rfcount:
                mov cx, word ptr [WF_Counter]
                mov dx, word ptr [WF_Counter]+2
                or cx, cx
                jnz short loc_rfd_stc_retn_ufp
                or dl, dl ; or dx, dx
                jz short loc_rfd_stc_return

loc_rfd_stc_retn_ufp:
               ; 03/10/2010
                cmp word ptr ES:[OF_OpenMode], 2 ; r/w 
                je short loc_rfd_stc_return
               ; 20/11/2010
                add word ptr ES:[OF_Pointer], cx
                adc word ptr ES:[OF_Pointer]+2, dx

loc_rfd_stc_return:
               ; 03/10/2010
                mov si, word ptr [RWF_SI]
               ; 28/11/2010
                or ax, ax
                jz loc_read_file_stc_eof_return
                mov dh, al ; error code
                jmp loc_read_file_stc_return

loc_rfd_invalid_format_retn:
               ; 21/07/2011
                mov ax, 0Bh ; Invalid (disk/fs) format
                jmp short loc_rfd_stc_return_chk_rfcount  

loc_rfd_gcffs_no_error:
               ; 23/10/2010
loc_rfd_read_FAT_disk_sector:
               ; 21/07/2011
               ; NOTE: LBA sector number must be less then
               ;       2 TB (2048 GB) limit 
               ; at 'loc_read_file_disk_sector_next' stage
               ; There must not be a FAT file sytem sector
               ; at the beyond of 2048 GB barrier.    
              ; 12/07/2011
                mov word ptr [WF_LastCluster], ax
                mov word ptr [WF_LastCluster]+2, dx
               ; 02/10/2010
                sub ax, 2
                sbb dx, 0
                mov bx, di ; Sectors Per Cluster
                call proc_mul32
               ;21/07/2011 
                or bx, bx  ; LBA sector = BX:DX:AX
                jnz short loc_rfd_invalid_format_retn
      
               ; 28/11/2010
                sub di, cx
                mov word ptr [RWF_SC], di 
                     ;di = remain sectors in the cluster  
               ;
                add ax, cx ; add sector offset to ax
                adc dx, 0
                add ax, word ptr [SI][LD_DATABegin]
                adc dx, word ptr [SI][LD_DATABegin]+2
               ; 21/07/2011
                jc short loc_rfd_invalid_format_retn 
loc_read_file_disk_sector_next:
                mov word ptr [WF_LBA], ax
                mov word ptr [WF_LBA]+2, dx

                mov bx, word ptr [RF_Handle]
                add bx, 10h ; Read buffer segment (+256)

                mov es, bx
                xor bx, bx
                mov cx, 1
                call proc_disk_read
                jc short loc_rfd_stc_return_chk_rfcount

loc_rfd_no_disk_read_error:
               ; 02/10/2010
                push es
                push word ptr [RF_Handle]
                pop es
                mov cx, word ptr ES:[OF_BytesPerSec]
                                           ; 512 or 2048
                mov ax, word ptr ES:[OF_Pointer]
                mov dx, word ptr ES:[OF_Pointer]+2
               ; 28/11/2010
                add ax, word ptr [WF_Counter]
                adc dx, word ptr [WF_Counter]+2 
               ; 
                call Rx_DOS_Div32
                sub cx, bx ; minimum 1 byte
                mov si, bx
                mov di, word ptr [RWF_Buffer]
                mov bx, word ptr [RWF_Buffer]+2                 
                add di, word ptr [WF_Counter]
loc_rfd_move_bytes_upd_es:
                push cx
                mov cx, word ptr [WF_Counter]+2
                adc cx, 0
                or cx, cx
                jz short loc_rfd_move_bytes_pass_upd_es
loc_rfd_move_bytes_upd_es_1000h:
                add bx, 1000h
                loop loc_rfd_move_bytes_upd_es_1000h
loc_rfd_move_bytes_pass_upd_es:
                pop cx
                mov es, bx
loc_rfd_move_bytes:
                mov ax, word ptr [RWF_NumberOfBytes]
                mov dx, word ptr [RWF_NumberOfBytes]+2
                pop ds ; pushed es
loc_rfd_move_bytes_next:
                movsb   ; 1 byte copy from DS:SI to ES:DI
               ;09/01/2011 
                or di, di
                jnz short pass_rfd_move_bytes_chg_ds
                mov di, es
                add di, 1000h
                mov es, di
                xor di, di   
pass_rfd_move_bytes_chg_ds:
                add word ptr CS:[WF_Counter], 1
                adc word ptr CS:[WF_Counter]+2, 0
                sub ax, 1
                sbb dx, 0 
                or ax, ax
                jnz short loc_rfd_move_bytes_loop
                or dl, dl ; dx, dx
                jz short loc_rfd_move_bytes_OK

loc_rfd_move_bytes_loop:
                loop loc_rfd_move_bytes_next

loc_rfd_move_bytes_OK:
                push cs
                pop ds 
                mov word ptr [RWF_NumberOfBytes], ax
                mov word ptr [RWF_NumberOfBytes]+2, dx

loc_read_file_check_rwf_sc: 
               ; 28/11/2010
                push word ptr [RF_Handle]
                pop es      
                or cx, cx
                jnz short loc_read_file_move_fp_ok1
                
               ; 09/07/2011
                mov bx, offset Logical_DosDisks
                add bh, byte ptr ES:[OF_Drive]
                mov si, bx
               ;  
                dec word ptr [RWF_SC]
               ; 09/07/2011
               ; jnz short loc_read_file_rwf_sc_next
                jz short loc_read_file_get_next_conseq_blocks

loc_read_file_rwf_sc_next:
              ; 28/11/2010
                mov ax, word ptr [WF_LBA]
                mov dx, word ptr [WF_LBA]+2
                add ax, 1
                adc dx, 0
                jmp loc_read_file_disk_sector_next

loc_read_file_get_next_conseq_blocks:
               ;mov ax, word ptr [WF_Counter]
               ;mov dx, word ptr [WF_Counter]+2
               ;jmp loc_rfd_convert_fp_to_fsector_next

              ; 12/07/2011
              ; 09/07/2011 
                mov ax, word ptr [WF_LastCluster] ; Current cluster/section
                mov dx, word ptr [WF_LastCluster]+2 ; Current cluster/section

                cmp byte ptr [SI][LD_FATType], 0
                jna short loc_read_file_get_next_section               
 
loc_read_file_get_next_cluster:
                ;09/07/2011
                call proc_get_next_cluster
                jc loc_rfd_fs_get_section_ffs_chk_err
                 
loc_read_file_set_next_cluster_params:
               ; 12/07/2011
               ; 09/07/2011
                mov word ptr [WF_LastCluster], ax
                mov word ptr [WF_LastCluster]+2, dx
                xor bh, bh
                mov bl, byte ptr [SI][LD_BPB]+SecPerClust
                mov word ptr [RWF_SC], bx
                sub ax, 2
                sbb dx, 0
                call proc_mul32
                add ax, word ptr [SI][LD_DATABegin]
                adc dx, word ptr [SI][LD_DATABegin]+2
               ; 21/07/2011
                jc loc_rfd_invalid_format_retn

                jmp loc_read_file_disk_sector_next
               
loc_read_file_move_fp_ok1:
               ; 20/11/2010
               ; 03/10/2010
                cmp word ptr ES:[OF_OpenMode], 2 ; r/w 
                je short loc_rfd_pass_move_fp
               ; 
              
               ; 28/11/2010
                mov ax, word ptr [WF_Counter]
                mov dx, word ptr [WF_Counter]+2

loc_read_file_move_fp_ok0:
                add word ptr ES:[OF_Pointer], ax
                adc word ptr ES:[OF_Pointer]+2, dx
loc_rfd_pass_move_fp:
                mov cx, ax
loc_read_file_return:
                or dh, byte ptr [RWF_EOF]
                mov ax, es
                mov si, word ptr [RWF_SI]
                push word ptr [RWF_Buffer]
                pop bx
                push word ptr [RWF_Buffer]+2
                pop es
                xor di, di
                retn

loc_rfd_fs_read_disk_sector_sc_reset:
                ;09/07/2011
                xor ax, ax
                jmp short loc_rfd_fs_read_disk_sector_lba


loc_read_file_get_next_section:
                ;09/07/2011
                call proc_get_next_section
                jc loc_rfd_fs_get_section_ffs_chk_err

loc_read_file_set_next_section_params:
                ; 12/07/2011
                ; 09/07/2011
                mov word ptr [WF_LastCluster], ax
                mov word ptr [WF_LastCluster]+2, dx
                or bx, bx ; sector count hw
                jnz short loc_read_file_next_section_32MB

loc_read_file_next_section_set_sc:
               ; 09/07/2011
                mov word ptr [RWF_SC], cx
                adc ax, word ptr [SI][LD_FS_BeginSector]
                adc dx, word ptr [SI][LD_FS_BeginSector]+2
               ; 21/07/2011 
                jc loc_rfd_invalid_format_retn 
                add ax, 1  ; add 1 for the 1'st data sector
                adc dx, 0
               ; 21/07/2011 
                jc loc_rfd_invalid_format_retn  
                jmp loc_read_file_disk_sector_next

loc_read_file_next_section_32MB:
               ; 09/07/2011
                xor cx, cx
                jmp short loc_read_file_next_section_set_sc

loc_rfd_fs_get_section_for_file_sector:
                  ; DS:SI = Logical dos drv table
                  ; DX:AX = Sector sequence number of the file
                  ; BX:CX = File Number (the 1st FDT address)
                call proc_get_section_for_fs_file_sector
                  ; DX:AX = Section header addr of the requested sector
                  ; BX:CX = Sector offset of the section
                  ; DI = Pointer to sector count of the section
                  ; stc & AX > 0 -> error except EOF
                  ; stc & AX = 0 -> EOF ->
                  ;  Last section before the req. sector number
                  ;  BX:CX = Header address of the last section
                  ;  DX = Required additional sectors
                  ;  DX = 0 -> Beyond of 65535 additional sectors
                jc loc_rfd_fs_get_section_ffs_chk_err
                
              ; 03/10/2010 modification
loc_rfd_fs_read_disk_sector:
                ; 12/07/2011
                ; 09/07/2011
                mov word ptr [WF_LastCluster], ax
                mov word ptr [WF_LastCluster]+2, dx
                ;22/01/2011
                ;28/11/2010
                push dx
                push ax
                mov dx, word ptr [DI]+2
                mov ax, word ptr [DI]
                sub ax, cx
                sbb dx, bx
                jnz short loc_rfd_fs_read_disk_sector_sc_reset 

loc_rfd_fs_read_disk_sector_lba: 
               ; 09/07/2011
                mov word ptr [RWF_SC], ax
                ;22/01/2011
                ;28/11/2010
                pop ax
                pop dx
                ; 09/07/2011 
                add ax, word ptr [SI][LD_FS_BeginSector]
                adc dx, word ptr [SI][LD_FS_BeginSector]+2
                ; DX:AX = Section header address, LBA
                add ax, cx
                adc dx, bx
                add ax, 1  ; add 1 for the 1'st data sector
                adc dx, 0
                jmp loc_read_file_disk_sector_next

RWF_EOF: db 0
RF_Handle: dw 0
RWF_Buffer: dd 0
RWF_NumberOfBytes: dd 0
RWF_SI: dw 0
;28/11/2010
RWF_SC: dw 0

proc_read_file endp

proc_write_file proc near
                ; 02/09/2011
                ; 21/07/2011   
                ; 17/07/2011
                ; 16/07/2011
                ; 13/07/2011 
                ; 11/07/2011 
                ; 10/07/2011
                ; 09/07/2011
                ; 21/05/2011 
                ; 20/03/2011 File Size BugFix
                ; 22/01/2011 BugFix
                ; 09/01/2011 verification -> OK
                ; 05/12/2010
                ; 28/11/2010
                ; 29/10/2010 lmdt pointer -> DI
                ; 24/10/2010
                ; 23/10/2010
                ; 20/10/2010 BugFix
                ; 19/10/2010
                ; 18/10/2010
                ; 09/10/2010
                ; 03/10/2010 major modification
                ; 23/09/2010
                ; 12/09/2010
                ; 10/09/2010
                ; 09/09/2010
                ; 08/09/2010
                ; 04/09/2010
                ; 28/08/2010
                ; 22/08/2010
                ;
                ; INPUT ->
                ;    DL:CX = number of bytes to write
                ;    AX = File number/handle
                ;    DH = > 0 (FFh) -> EOF after write
                ;    DL -> 0 to 8 (64K*DL+CX = bytes to read)
                ;    ES:BX = Buffer
                ;     DI = 0 -> Use current date&time as lmdt
                ;     DI > 0 -> lmdt value is in DS:DI (dword ptr) 
                ; OUTPUT ->
                ;    File pointer is moved
                ;    DX:CX = Written bytes (same with input)
                ;    DH = FFh -> EOF
		;    SI, AX, BX, ES, DS same with input values
                ;    DI = 0
                ;   stc -> DL:CX = write count (< requested count)
                ;          DH = FFh -> EOF
                ;          DH <> FFh -> Write error (general failure)

               ;16/07/2011
                cmp dl, 8 
                ja short loc_write_file_invalid_data
                and dh, dh ; 0 = No EOF 
                jnz short loc_Write_file_put_EOF_sign
                mov byte ptr [WF_EOF], dh ; 0, no eof
                jmp short loc_Write_file_check_WF_handle 
loc_Write_file_put_EOF_sign:                
                mov byte ptr [WF_EOF], 0FFh ; EOF
                xor dh, dh 
loc_Write_file_check_WF_handle:
               ;16/07/2011 
               ;29/10/2010
                mov word ptr [WF_LMDT_ptr], di 
               ; 22/09/2010
                mov ds, ax
                cmp ax, word ptr CS:[WF_Handle]
                je short loc_write_file_chk_open_mode

                mov word ptr CS:[WF_Handle], ax
             
               ; 26/09/2010
                mov al, byte ptr DS:[OF_Type]
                cmp al, 'F'
               ;je short loc_write_file_chk_open_mode
               ; 02/10/2010
                jne short loc_write_file_invalid_handle

                mov al, byte ptr DS:[OF_OFL_SN]
                or al, al
                jz short loc_write_file_invalid_handle
                cmp al, 32
                ja short loc_write_file_invalid_handle
                ;
                mov di, offset Open_Files_List
                dec al
                mov ah, OFL_EntrySize
                mul ah
                add di, ax
                mov ax, ds
                cmp ax, word ptr CS:[DI]+OFL_Handle
                je short loc_write_file_chk_open_mode

loc_write_file_invalid_handle:
              ; 21/07/2011  
                mov dh, 06h ; Invalid file handle/number
loc_write_file_invalid_drv_retn:
               ; 22/09/2010
                push cs
                pop ds
               ; 21/07/2011
                mov ax, word ptr [WF_Handle]
loc_write_file_invalid_data_retn: 
               ; 21/07/2011  
                xor dl, dl
                xor cx, cx
                mov di, word ptr [WF_LMDT_ptr]
                stc
                retn

loc_write_file_invalid_data:
               ; 21/07/2011  
                mov dh, 0Dh ; Data invalid
                jmp loc_read_file_invalid_data_retn

loc_write_file_invalid_drive:
              ; 21/07/2011 
               mov dh, 0Fh ; Invalid drive
               jmp short loc_write_file_invalid_drv_retn

loc_write_file_chk_open_mode:
                mov al, byte ptr DS:[OF_OpenMode]
                or al, al
                jz short loc_write_file_invalid_handle
                cmp al, 2
                ja short loc_write_file_invalid_handle

loc_write_file_chk_volume_id:
               mov ah, byte ptr DS:[OF_Drive]  
               xor al, al
               mov di, offset Logical_DOSDisks
               add di, ax
               mov al, byte ptr CS:[DI]+LD_FATType
               or al, al
               jz short loc_write_file_chk_fs_volume_id
               cmp al, 2
               ja short loc_write_file_chk_fat32_volume_id
               add di, LD_BPB + VolumeID
               jmp short loc_write_file_chk_volume_id_cmpvlw  

loc_write_file_chk_fat32_volume_id:
               add di, LD_BPB + FAT32_VolID
               jmp short loc_write_file_chk_volume_id_cmpvlw  

loc_write_file_chk_fs_volume_id:
               add di, LD_FS_VolumeSerial

loc_write_file_chk_volume_id_cmpvlw:
               mov ax, word ptr CS:[DI]
               cmp ax, word ptr DS:[OF_VolumeId]
               jne short loc_write_file_invalid_drive
loc_write_file_chk_volume_id_cmpvhw:
               mov ax, word ptr CS:[DI]+2
               cmp ax, word ptr DS:[OF_VolumeId]+2
              ; 21/07/2011 
               jne short loc_write_file_invalid_drive

loc_Write_file_chk_valid_file_handle_OK:
                mov ax, ds
                push cs
                pop ds
               ; 09/07/2011
                mov word ptr [RWF_Buffer], bx
                mov bx, es
                mov word ptr [RWF_Buffer]+2, bx
               ;
                mov word ptr [RWF_NumberOfBytes], cx
                mov word ptr [RWF_NumberOfBytes]+2, dx

              ; 20/03/2011

                mov byte ptr [RWF_EOF], 0FFh
                mov word ptr [RWF_SI], si
                mov es, ax

                mov al, byte ptr ES:[OF_OFL_SN]
                dec al 
                mov ah, OFL_EntrySize
                mul ah
                mov si, offset Open_Files_List
                add si, ax
                mov ax, word ptr [SI]+OFL_Cluster    ; +2
                mov bx, word ptr [SI]+OFL_Cluster+2  ; +4
                mov word ptr [WF_FCluster], ax   ; WF_FDT
                mov word ptr [WF_FCluster]+2, bx ; WF_FDT

               ; 09/07/2011
                mov bx, offset Logical_DosDisks
                add bh, byte ptr ES:[OF_Drive]
                mov si, bx             
               ;
                mov ax, word ptr ES:[OF_Pointer] ;15/07/2011

                or cx, cx
                jnz short loc_write_file_check_rwf_eof
                or dl, dl
                jnz short loc_write_file_check_rwf_eof
                
loc_write_file_check_eof_yes:
               ; 20/03/2011
               ; mov ax, word ptr ES:[OF_Pointer]
                mov dx, word ptr ES:[OF_Pointer]+2
                jmp loc_write_file_truncate_file
                
loc_write_file_check_rwf_eof:
                cmp byte ptr [WF_EOF], 0
                ja short loc_write_file_convert_fp_to_fsector
loc_write_file_check_eof_no:
                inc byte ptr [RWF_EOF] ; 0, not EOF
              
loc_write_file_convert_fp_to_fsector:
              ; 23/09/2010
                xor ax, ax
                xor dx, dx
                mov word ptr [WF_Counter], ax ; 0
                mov word ptr [WF_Counter]+2, dx ; 0
              
              ; 23/10/2010
                mov byte ptr ES:[OF_WriteStatus], 1

loc_write_file_convert_fp_to_fsector_next:              
              ; 23/09/2010
                mov cx, 512 ; word ptr ES:[OF_BytesPerSec]
                add ax, word ptr ES:[OF_Pointer]
                adc dx, word ptr ES:[OF_Pointer]+2
                call Rx_DOS_Div32

                push ds
                pop es

              ; 12/09/2010
                mov cx, word ptr [WF_FCluster]   ; [WF_FDT]
                mov bx, word ptr [WF_FCluster]+2 ; [WF_FDT]+2

                cmp byte ptr [SI][LD_FATType], 0
                jna loc_write_file_fs_get_section_for_file_sector

loc_write_file_get_cluster_for_file_sector:
                  ; 16/07/2011
                  ; 12/09/2010
                  ;  DS:SI = Logical dos drv table
                  ;  DX:AX = Sector sequence number of the file
                  ;  BX:CX = First Cluster of the file
                call proc_get_cluster_for_file_sector
                  ;  DX:AX = Cluster number of the requested sector
                  ;  CX = Sector offset of the cluster
                  ;  BX = 0 
                  ;  DI = Sectors per cluster value
                  ;  stc & AX > 0 -> error except EOF
                  ;  stc & AX = 0 -> EOF ->
                  ;  Last cluster before the req. sector number
                  ;  BX:CX = The Last cluster number
                  ;  DX = Required additional sectors (0 -> 64K)

               ; 10/07/2011
               ; mov word ptr [FAT_File_SecPerClust], di 

                jnc loc_write_file_gcffs_no_error

loc_write_file_get_cluster_ffs_chk_err:
              ; 09/10/2010 
                or ax, ax
                jnz short loc_write_file_stc_return_chk_rfcount
                mov byte ptr [RWF_EOF], 0FFh

loc_write_file_add_new_cluster:
              ; 10/09/2010 
                mov word ptr [WF_LastCluster], cx
                mov word ptr [WF_LastCluster]+2, bx
                mov ax, dx ; (required/additional) sector count
                dec ax
                xor dx, dx
                div di
                mov word ptr [WF_SecOffset_inLC], dx
               ; 11/07/2011
                mov dx, cx ; word ptr [WF_LastCluster]
                mov cx, ax ; cluster count - 1      
                inc cx ; cluster count
                mov ax, dx ; word ptr [WF_LastCluster]
                mov dx, bx ; word ptr [WF_LastCluster]+2

loc_write_file_add_new_cluster_loop:
                push cx  
                 ; DS:SI = Logical dos drv desc. table address
                 ; DX:AX = Last cluster
                call proc_add_new_cluster
                 ; DS:SI = Logical dos drv desc. table address
                 ; DX:AX = New Last cluster
                 ; stc -> error code in AL
                pop cx  
               ; 09/10/2010
                jc short loc_write_file_anc_error
                loop loc_write_file_add_new_cluster_loop

                mov cx, word ptr [WF_SecOffset_inLC]
               ; 22/01/2011 
                mov di, word ptr [FAT_File_SecPerClust]
               ; 16/07/2011 
                jmp short loc_write_file_gcffs_no_error

loc_write_file_anc_error:
                call proc_save_directory_buffer
                
               ; 09/10/2010
loc_write_file_ans_error:
               ; 09/10/2010
                push word ptr [WF_Handle]
                pop es

loc_write_file_stc_return_chk_rfcount:
              ; 09/07/2011
                push word ptr [WF_Handle]              
              ; 10/10/2010
                mov cx, word ptr [WF_Counter]
                mov dx, word ptr [WF_Counter]+2
                or cx, cx
                jnz short loc_write_file_stc_ufp
                or dl, dl ; or dx, dx
                jz short loc_write_file_stc_return

loc_write_file_stc_ufp:
              ; 10/10/2010
                pop ds ; WF_handle, OFDT segment
                add word ptr DS:[OF_Pointer], cx
                adc word ptr DS:[OF_Pointer]+2, dx
                push ds ; WF_handle
                push ax ; error code
                push dx  ; written bytes (hw)
                push cx  ; written bytes (lw)
               ; 24/10/2010 
                mov cx, word ptr DS:[OF_Size]
                mov dx, word ptr DS:[OF_Size]+2

loc_write_file_stc_ulmdt:
              ; 24/10/2010
                mov ax, ds
              ; 10/10/2010
                push cs
                pop ds
                mov bl, 0FFh
                call proc_update_open_file_size_lmdt
                pop cx
                pop dx
                pop ax 
                
loc_write_file_stc_return:
              ; 10/10/2010
                mov dh, al ; error code
                pop ax ; WF_handle
                mov si, word ptr [RWF_SI]
                push word ptr [RWF_Buffer]
                pop bx
                push word ptr [RWF_Buffer]+2
                pop es
               ; 29/10/2010
                mov di, word ptr [WF_LMDT_ptr]
                stc
                retn

loc_write_file_gcffs_no_error:
               ; 13/07/2011
               ; 23/10/2010
               ; 11/07/2011
                mov word ptr [WF_LastCluster], ax
                mov word ptr [WF_LastCluster]+2, dx
loc_write_file_set_disk_sector_address:
               ; 02/10/2010
                sub ax, 2
                sbb dx, 0
                mov bx, di ; Sectors per cluster
                call proc_mul32
              ; 28/11/2010
                sub di, cx
                mov word ptr [RWF_SC], di 
                     ;di = remain sectors in the cluster  
               ;
                add ax, cx ; add sector offset to ax
                adc dx, 0
               ; 09/07/2011
loc_write_file_read_FAT_disk_sector_next:
                add ax, word ptr [SI][LD_DATABegin]
                adc dx, word ptr [SI][LD_DATABegin]+2
loc_write_file_read_fs_disk_sector_next:
               ; 03/10/2010
                mov word ptr [WF_LBA], ax
                mov word ptr [WF_LBA]+2, dx

loc_write_file_set_rw_offset:
               ; 13/07/2011
                mov bx, word ptr [WF_Handle]
                mov es, bx
               ; 23/10/2010
                mov cx, 512 ; word ptr ES:[OF_BytesPerSec]
                mov ax, word ptr ES:[OF_Pointer]
                mov dx, word ptr ES:[OF_Pointer]+2
                add ax, word ptr [WF_Counter]
                adc dx, word ptr [WF_Counter]+2
                call Rx_DOS_Div32
               ; 13/07/2011
                or bx, bx
                jz short loc_write_file_pass_disk_read

loc_write_file_read_FAT_disk_sector:
               ;13/07/2011 
                mov ax, word ptr [WF_LBA]
                mov dx, word ptr [WF_LBA]+2
          
loc_write_file_read_disk_sector_next:
               ; 13/07/2011
                push bx
                push cx
                mov bx, es
               ; 23/10/2010 
                add bx, 10h ; Write buffer segment (+256)
                mov es, bx
                xor bx, bx
                mov cx, 1
                call proc_disk_read
              ; 13/07/2011
                pop cx ; 512
                pop bx ; offset in sector
                jc loc_write_file_stc_return_chk_rfcount

loc_write_file_no_disk_read_error:
               ; 13/07/2011
                sub cx, bx ; minimum 1 byte
loc_write_file_pass_disk_read: 
                mov di, bx
                push si ; 19/10/2010
                mov si, word ptr [RWF_Buffer]
                mov bx, word ptr [RWF_Buffer]+2                 
                add si, word ptr [WF_Counter]
loc_write_file_move_bytes_upd_es:
                push cx
                mov cx, word ptr [WF_Counter]+2
                adc cx, 0
                or cx, cx
                jz short loc_write_file_move_bytes_pass_upd_es
loc_write_file_move_bytes_upd_es_1000h:
                add bx, 1000h
                loop loc_write_file_move_bytes_upd_es_1000h
loc_write_file_move_bytes_pass_upd_es:
                pop cx
loc_write_file_move_bytes:
                push bx
                mov ax, word ptr [RWF_NumberOfBytes]
                mov dx, word ptr [RWF_NumberOfBytes]+2
                mov bx, word ptr [WF_Handle]
                add bx, 10h ; Write buffer segment (+256)
                pop ds
                mov es, bx
              
loc_write_file_move_bytes_next:
                movsb   ; 1 byte copy from DS:SI to ES:DI
               ; 26/10/2010 
                or si, si
                jnz short pass_write_file_move_bytes_chg_ds
                mov si, ds
                add si, 1000h
                mov ds, si
                xor si, si   
pass_write_file_move_bytes_chg_ds:
                add word ptr CS:[WF_Counter], 1
                adc word ptr CS:[WF_Counter]+2, 0
                sub ax, 1
                sbb dx, 0
               ;16/07/2011
                jnz loc_write_file_move_bytes_loop 
                or ax, ax
                jz short loc_write_file_move_bytes_OK

loc_write_file_move_bytes_loop:
                loop loc_write_file_move_bytes_next

loc_write_file_move_bytes_OK:
                pop si ; 19/10/2010
                push cs
                pop ds

                mov word ptr [RWF_NumberOfBytes], ax
                mov word ptr [RWF_NumberOfBytes]+2, dx 

loc_write_file_write_disk_sector_next:
                mov ax, word ptr [WF_LBA]
                mov dx, word ptr [WF_LBA]+2
               ;mov es, bx
                xor bx, bx
                mov cx, 1
                call proc_disk_write
                jc loc_write_file_stc_return_chk_rfcount

; 17/07/2011 printing file write percentage

cmp byte ptr [WF_Percentage_Yes], 0
jna loc_write_file_write_disk_sector_OK 

push dx
push ax

; 02/09/2011
cmp byte ptr [WF_64K_Percentage], 0
ja short loc_write_64K_sector_percentage
inc word ptr [wfsc]
mov ax,word ptr [wfsc]
jmp short loc_write_print_percentage_chk_100

loc_write_64K_sector_percentage:
; 02/09/2011
inc byte ptr [wfsc]
mov al, byte ptr [wfsc]
cmp al, 128
jb short loc_write_file_print_percentage_OK
xor al, al
mov byte ptr [wfsc], al
inc word ptr [wf64ksc] 
mov ax, word ptr [wf64ksc]

loc_write_print_percentage_chk_100:
cmp ax, word ptr [wftsc]
jb short loc_write_file_calculate_percentage
mov cx, 100
jmp short loc_write_file_set_cursor_pos

loc_write_file_calculate_percentage:
mov dx, 100
mul dx
mov cx, word ptr [wftsc]
div cx

cmp al, byte ptr [wfppercent]
je short loc_write_file_print_percentage_OK

mov byte ptr [wfppercent], al

mov cx, ax

loc_write_file_set_cursor_pos:
mov dx, word ptr [WF_CursorPos]
mov bx, word ptr [WF_VideoPage]
mov ah, 2
int 10h

mov ah, 0Eh
mov al, "%"
int 10h

cmp cx, 100
jb short loc_write_file_print_percent_0_to_99

loc_write_file_print_percent_100:
mov al, "1"
int 10h
mov al, "0"
int 10h
;mov al, "0"
int 10h
mov al, 0Dh
int 10h
mov al, 0Ah
int 10h
jmp loc_write_file_print_percentage_OK


loc_write_file_print_percent_0_to_99:
mov al, cl
aam
add ax, 3030h
mov cx, ax
mov ah, 0Eh
mov al, ch
int 10h
mov al, cl
int 10h

loc_write_file_print_percentage_OK:
pop ax
pop dx
;

loc_write_file_write_disk_sector_OK:
               ; 09/10/2010
                push word ptr [WF_Handle]
                pop es 

               ; 28/11/2010
                cmp byte ptr [RWF_NumberOfBytes]+2, 0
                ja short loc_write_file_check_rwf_sc
                cmp word ptr [RWF_NumberOfBytes], 0
                jna short loc_write_file_move_fp

loc_write_file_check_rwf_sc:
               ; 28/11/2010
                dec word ptr [RWF_SC]
               ; 09/07/2011
                jnz loc_write_file_read_fs_disk_sector_next
               ; jz short loc_write_file_get_next_conseq_blocks

;loc_write_file_rwf_sc_next:
               ; 09/07/2011
               ; mov ax, word ptr [WF_LBA]
               ; mov dx, word ptr [WF_LBA]+2
               ; add ax, 1
               ; adc dx, 0
               ; jmp loc_write_file_read_fs_disk_sector_next

loc_write_file_get_next_conseq_blocks:
               ;mov ax, word ptr [WF_Counter]
               ;mov dx, word ptr [WF_Counter]+2
               ;jmp loc_rfd_convert_fp_to_fsector_next

              ; 11/07/2011 
                mov ax, word ptr [WF_LastCluster] ; Current cluster/section
                mov dx, word ptr [WF_LastCluster]+2 ; Current cluster/section

                cmp byte ptr [SI][LD_FATType], 0
                jna loc_write_file_get_next_section               
 
loc_write_file_get_next_cluster:
                ;09/07/2011
                call proc_get_next_cluster
                jnc short loc_write_file_set_next_cluster_params

loc_write_file_get_next_chk_err:
              ; 09/07/2011 
                or ax, ax
                jnz loc_write_file_stc_return_chk_rfcount
                mov byte ptr [RWF_EOF], 0FFh

loc_write_file_gnc_add_new_cluster:
               ; 11/07/2011  
               ; 09/07/2011 
                mov word ptr [WF_SecOffset_inLC], ax ; 0

               ;mov al, byte ptr [SI][LD_BPB]+SecPerClust
                mov ax, word ptr [FAT_File_SecPerClust] 
                mov word ptr [RWF_SC], ax 

                mov ax, cx ; word ptr [WF_LastCluster]
                mov dx, bx ; word ptr [WF_LastCluster]+2

                 ; DS:SI = Logical dos drv desc. table address
                 ; DX:AX = Last cluster
                call proc_add_new_cluster
                 ; DS:SI = Logical dos drv desc. table address
                 ; DX:AX = New Last cluster
                 ; stc -> error code in AL
                jc loc_write_file_anc_error

                mov bx, word ptr [RWF_SC] ; Sectors per cluster

loc_write_file_gnc_add_new_cluster_ok:
              ; 11/07/2011
                mov word ptr [WF_LastCluster], ax
                mov word ptr [WF_LastCluster]+2, dx

              ; 09/07/2011
                sub ax, 2
                sbb dx, 0
                call proc_mul32
              
                jmp loc_write_file_read_FAT_disk_sector_next

loc_write_file_set_next_cluster_params:
                ;09/07/2011
                ;xor bh, bh
                ;mov bl, byte ptr [SI][LD_BPB]+SecPerClust
                mov bx, word ptr [FAT_File_SecPerClust] 
                mov word ptr [RWF_SC], bx
                ;10/07/2011
                jmp short loc_write_file_gnc_add_new_cluster_ok

loc_write_file_move_fp:
               ; 28/11/2010
                mov ax, word ptr [WF_Counter]
                mov dx, word ptr [WF_Counter]+2

                add word ptr ES:[OF_Pointer], ax
                adc word ptr ES:[OF_Pointer]+2, dx

               ; 20/03/2011 

loc_write_file_chk_change_file_size:
                mov ax, word ptr ES:[OF_Pointer]
                mov dx, word ptr ES:[OF_Pointer]+2
loc_write_file_change_file_size_hw:
                cmp dx, word ptr ES:[OF_Size]+2
                je short loc_write_file_change_file_size_lw
                mov word ptr ES:[OF_Size]+2, dx
                mov word ptr ES:[OF_Size], ax
                jb short loc_write_file_truncate_file_chk_eof
                ja short loc_write_file_update_lmdt
loc_write_file_change_file_size_lw:
                cmp ax, word ptr ES:[OF_Size]
                je short loc_write_file_update_lmdt
                mov word ptr ES:[OF_Size], ax
                jnb short loc_write_file_update_lmdt

loc_write_file_truncate_file_chk_eof:
               ; 20/03/2011 
                cmp byte ptr [RWF_EOF], 0
                jna short loc_write_file_update_lmdt

loc_write_file_truncate_file:
                mov cx, word ptr [WF_FCluster]   ; [WF_FDT]
                mov bx, word ptr [WF_FCluster]+2 ; [WF_FDT]+2
                ; DS:SI -> Logical dos drv description table
                call proc_truncate_file
                jc loc_write_file_stc_return_chk_rfcount
               
loc_write_file_truncate_file_OK:
                push word ptr [WF_Handle]
                pop es

loc_write_file_update_lmdt:
              ; 20/03/2011
                mov ax, word ptr [WF_Counter]
                mov dx, word ptr [WF_Counter]+2
              ; 23/10/2010
               push es
               push ax  ; written bytes (lw)
               push dx  ; written bytes (hw)

               mov cx, word ptr ES:[OF_Size]
               mov dx, word ptr ES:[OF_Size]+2
               mov bl, 0FFh
               mov ax, es ; word ptr [WF_Handle]

               mov di, word ptr [WF_LMDT_ptr]

               ; 29/10/2010 lmdt pointer -> DI
               ; 10/09/2010
               ; 08/09/2010
               ; INPUT -> 
               ;  AX = File handle
               ;  DX:CX = File size
               ;  BL = 0 -> BH = Drive number (DS:SI is not used)
               ;  BL > 0 -> BH = Any, DS:SI = DOS drv table
               ;     DI = 0 -> Use current date&time as lmdt
               ;     DI > 0 -> lmdt value is in DS:DI (dword ptr)   
               ; OUTPUT -> DS:SI = Logical DOS drv table addr.
               ;  clc (cf=0) -> updated
               ;  ES:BX -> Directory entry or FDT address
               ;  stc (cf=1) -> error code in AX
               ;  AX, CX, DX, will be changed
               
               call proc_update_open_file_size_lmdt
               pop dx
               jnc short loc_write_file_retn

loc_write_file_error_return:
               mov dh, al
loc_write_file_retn:
               pop cx
               pop ax ; pushed es, WF_handle
              ; pushf
              
               ; 20/03/201
               or dh, byte ptr [RWF_EOF]
               ;
               mov si, word ptr [RWF_SI]
               ; 09/07/2011 
               mov bx, word ptr [RWF_Buffer]+2
               mov es, bx
               mov bx, word ptr [RWF_Buffer]
               ; 29/10/2010
               ; mov di, word ptr [WF_LMDT_ptr]
              ; popf 
               retn

loc_write_file_fs_get_section_for_file_sector:
                  ; 09/10/2010 
                  ; DS:SI = Logical dos drv table
                  ; DX:AX = Sector sequence number of the file
                  ; BX:CX = File Number (the 1st FDT address)
                call proc_get_section_for_fs_file_sector
                  ; DX:AX = Section header addr of the requested sector
                  ; BX:CX = Sector offset of the section
                  ; DI = Pointer to sector count of the section
                  ; stc & AX > 0 -> error except EOF
                  ; stc & AX = 0 -> EOF ->
                  ;  Last section before the req. sector number
                  ;  BX:CX = Header address of the last section
                  ;  DX = Required additional sectors
                  ;  DX = 0 -> Beyond of 65535 additional sectors    
                jnc loc_write_file_fs_read_disk_sector

loc_write_file_fs_get_section_ffs_chk_err: 
              ; 10/09/2010
                or ax, ax
              ; 09/10/2010
                jnz loc_write_file_stc_return_chk_rfcount
                mov byte ptr [RWF_EOF], 0FFh ; 09/07/2011

loc_write_file_fs_new_section_size_calc:
              ; 16/07/2011
                push dx
                mov ax, word ptr [RWF_NumberOfBytes]
                mov dx, word ptr [RWF_NumberOfBytes]+2               
                mov di, 511
                add ax, di
                adc dx, 0
                inc di
                div di
                pop dx
                add ax, dx
                jnc short loc_write_file_fs_add_new_section                                  

                mov ax, 65535 ;Max. 65535 sectors except section header

loc_write_file_fs_add_new_section:
              ; 15/11/2010 
              ; 08/09/2010
              ; 16/07/2011  
                xchg ax, cx
                mov cx, dx
                mov word ptr [WF_SecOffset_inLC], dx ; sector offset + 1   
                mov dx, bx
              
loc_write_file_fs_add_new_section_chk_free_sectors:
               ; 05/12/2010
                cmp cx, 16
                jnb short loc_write_file_fs_add_new_section_pass_4
                cmp word ptr [SI][LD_FS_FreeSectors]+2, 256 ; >= 8 GB  
                jb short loc_write_file_fs_add_new_section_chk_8s
                mov cl, 16
                jmp short loc_write_file_fs_add_new_section_pass_4

loc_write_file_fs_add_new_section_chk_8s:
                cmp cl, 8
                jnb short loc_write_file_fs_add_new_section_pass_4
                cmp byte ptr [SI][LD_FS_FreeSectors]+2, 32 ; >= 1024 MB
                jb short loc_write_file_fs_add_new_section_chk_4s
                mov cl, 8
                jmp short loc_write_file_fs_add_new_section_pass_4 
   
loc_write_file_fs_add_new_section_chk_4s:
                cmp cl, 4
                jnb short loc_write_file_fs_add_new_section_pass_4
                cmp byte ptr [SI][LD_FS_FreeSectors]+2, 0 ; >= 32 MB
                jna short loc_write_file_fs_add_new_section_chk_3s
                mov cl, 4
                jmp short loc_write_file_fs_add_new_section_pass_4 

loc_write_file_fs_add_new_section_chk_3s:
                cmp cl, 3
                jnb short loc_write_file_fs_add_new_section_pass_4                 
                cmp word ptr [SI][LD_FS_FreeSectors], 256 ;> 128 KB
                jb short loc_write_file_fs_add_new_section_chk_2s
                mov cl, 3
                jmp short loc_write_file_fs_add_new_section_pass_4 

loc_write_file_fs_add_new_section_chk_2s:
                cmp cl, 2
                jnb short loc_write_file_fs_add_new_section_pass_4
                mov cl, 2 
                cmp byte ptr [SI][LD_FS_FreeSectors], 64 ;> 32 KB
                jnb short loc_write_file_fs_add_new_section_pass_4

loc_write_file_fs_add_new_section_set_1s:
                dec cl ; 1

loc_write_file_fs_add_new_section_pass_4:
                 ; DS:SI = Logical dos drv desc. table address
                 ; DX:AX = Last section
                 ; CX = sector count (except FDT)
               call proc_add_new_fs_section
                 ; DS:SI = Logical dos drv desc. table address
                 ; DX:AX = New Last section (next section)
                 ; ES:BX = section buffer
                 ; CX = count of sectors (except header)
                 ; stc -> error code in AL (AL = 27h -> CX valid) 
                 ;        CX = Max. contiguous sector count

             ; 15/07/2011 
               jnc short loc_write_file_fs_ans_OK
               cmp ax, 27h
               jne loc_write_file_ans_error
               cmp cx, 2
               jb loc_write_file_ans_error
               dec cx ; sector count except section header
               cmp word ptr [SI][LD_FS_FreeSectors]+2, 0
               ja short loc_write_file_fs_add_new_section_pass_4
               cmp cx, word ptr [SI][LD_FS_FreeSectors]
               jb short loc_write_file_fs_add_new_section_pass_4
               jmp loc_write_file_ans_error    

loc_write_file_fs_ans_OK:
loc_write_file_next_section_set_sc:
               ; 11/07/2011
                mov word ptr [WF_LastCluster], ax ; [WF_FDT]
                mov word ptr [WF_LastCluster]+2, dx ; [WF_FDT]
                mov word ptr [RWF_SC], cx
                jmp short loc_write_file_fs_read_disk_sector_lba

loc_write_file_fs_read_disk_sector:
               ; 11/07/2011
                mov word ptr [WF_LastCluster], ax ; [WF_FDT]
                mov word ptr [WF_LastCluster]+2, dx ; [WF_FDT]
                mov word ptr [WF_SecOffset_inLC], 1 ; + Header

                ; 09/07/2011
                ; DX:AX = Section header address, LBA
                add ax, cx
                adc dx, bx
                              
                ;22/01/2011
                ;28/11/2010
                push dx
                push ax
                mov dx, word ptr [DI]+2
                mov ax, word ptr [DI]
                sub ax, cx
                sbb dx, bx
                jnz short loc_write_file_fs_read_disk_sector_sc_reset 
                mov word ptr [RWF_SC], ax
 
loc_write_file_fs_read_disk_sector_lba_pop:
                ;22/01/2011
                ;28/11/2010
                pop ax
                pop dx 
loc_write_file_fs_read_disk_sector_lba:
                ; 11/07/2011
                ; word ptr [WF_SecOffset_inLC]  = 1 (+ header) except
                ; 'proc_get_section_for_fs_file_sector' eof return   
                ;
                add ax, word ptr [WF_SecOffset_inLC] ; sector offset + 1
                adc dx, 0
                ;
                add ax, word ptr [SI][LD_FS_BeginSector]
                adc dx, word ptr [SI][LD_FS_BeginSector]+2
                jmp loc_write_file_read_fs_disk_sector_next

loc_write_file_fs_read_disk_sector_sc_reset:
               ; 28/11/2010
                xor di, di
                mov word ptr [RWF_SC], di 
                jmp short loc_write_file_fs_read_disk_sector_lba_pop

loc_write_file_get_next_section:
               ; 11/07/2011 
                mov word ptr [WF_SecOffset_inLC], 1 ; + Header
                ;09/07/2011
                call proc_get_next_section
                jc short loc_write_file_get_next_section_chk_err

loc_write_file_set_next_section_params:
               ; 09/07/2011
                or bx, bx ; sector count hw
                jz short loc_write_file_next_section_set_sc
                xor cx, cx
                jmp short loc_write_file_next_section_set_sc

loc_write_file_get_next_section_chk_err:
              ; 09/07/2011 
                or ax, ax
                jnz loc_write_file_stc_return_chk_rfcount
                mov byte ptr [RWF_EOF], 0FFh

loc_write_file_gns_add_new_section_1:
              ; 09/07/2011 
                mov ax, word ptr [SI][LD_FS_FreeSectors]+2
                mov cx, 16 
                cmp ax, 256 ; >= 8 GB  
                jnb short loc_write_file_gns_add_new_section_6
                shr cl, 1 ; 8
loc_write_file_gns_add_new_section_2:
                cmp al, 32 ; >= 1024 MB
                jnb short loc_write_file_gns_add_new_section_6
                shr cl, 1 ; 4
loc_write_file_gns_add_new_section_3:
                or al, al ; >= 32MB
                jnz short loc_write_file_gns_add_new_section_6
                dec cl ; 3
loc_write_file_gns_add_new_section_4:
                mov ax, word ptr [SI][LD_FS_FreeSectors]
                cmp ax, 256 ; >= 128KB
                jnb short loc_write_file_gns_add_new_section_6
                dec cl ; 2
loc_write_file_gns_add_new_section_5:
                cmp ax, 64 ; >= 32KB
                jnb short loc_write_file_gns_add_new_section_6
                dec cl ; 1

loc_write_file_gns_add_new_section_6:
              ; 11/07/2011
                mov ax, word ptr [WF_LastCluster]   ; [WF_FDT]
                mov dx, word ptr [WF_LastCluster]+2 ; [WF_FDT]+2
                jmp loc_write_file_fs_add_new_section_pass_4

; 28/08/2010
WF_EOF: db 0
WF_Handle: dw 0
WF_LastCluster: dd 0
; 22/08/2010
WF_FDT:
WF_FCluster: dd 0
WF_Counter: dd 0
; 08/09/2010
WF_LBA: dd 0
; 04/09/2010
WF_SecOffset_inLC: dw 0
; 29/10/2010
WF_LMDT_ptr: dw 0

; 17/07/2011
WF_VideoPage: dw 0
WF_CursorPos: dw 0
WF_Percentage_Yes: db 0
WF_64K_Percentage: db 0
wfsc: dw 0
wftsc: dw 0
wf64ksc: dw 0
wfppercent: db 0

proc_write_file endp

proc_update_open_file_size_lmdt proc near
               ; 29/10/2010 
               ; 08/09/2010
               ; INPUT -> 
               ;  AX = File handle
               ;  DX:CX = File size
               ;  BL = 0 -> BH = Drive number (DS:SI is not used)
               ;  BL > 0 -> BH = Any, DS:SI = DOS drv table  
               ;     DI = 0 -> Use current date&time as lmdt
               ;     DI > 0 -> lmdt value is in DS:DI (dword ptr) 
               ; OUTPUT -> DS:SI = Logical DOS drv table addr.
               ;  clc (cf=0) -> updated
               ;  ES:BX -> Directory entry or FDT address
               ;  stc (cf=1) -> error code in AX
               ;  AX, CX, DX, (DI) will be changed
               
              ;29/10/2010
               or bl, bl
               jnz short loc_uoflmdt_check_FS_type
loc_uoflmdt_get_drv_tbl_addr:
               mov si, offset Logical_DOSDisks
               add si, bx
loc_uoflmdt_check_FS_type:
loc_uoflmdt_check_FAT_type:
               cmp byte ptr [SI][LD_FATType], 0
               jna short loc_uoflmdt_update_fs_file_size_lmdt
               call proc_update_open_FAT_file_size_lmdt
               retn
loc_uoflmdt_update_fs_file_size_lmdt:
               call proc_update_open_fs_file_size_lmdt
               retn 

proc_update_open_file_size_lmdt endp

proc_truncate_file proc near
               ; 30/07/2011 dword ptr [DI] return
               ; 12/09/2010
               ; 10/09/2010
               ; INPUT -> 
               ;  DX:AX = File size (in bytes)
               ;  BX:CX = First cluster number or FDT adress
               ;  DS:SI = Logical DOS drv table addr.
               ; OUTPUT -> DS:SI = Logical DOS drv table addr.
               ;  clc (cf=0) -> successfull
               ;      DI = (New) Last Cluster pointer (dd)
               ;  stc (cf=1) -> error code in AX
               ;      DI = 0 
               ;  AX, BX, CX, DX will be changed
               ;  ES=DS

loc_truncate_file_update_file_size_lmdt:
               cmp byte ptr [SI][LD_FATType], 0
               jna short loc_truncate_file_update_fs_file_size
               call proc_truncate_FAT_file
               retn
loc_truncate_file_update_fs_file_size:
               call proc_truncate_fs_file
               retn 

proc_truncate_file endp

proc_move_file_pointer proc near
               ; 09/04/2011
               ; INPUT
               ; AL= Moving method
               ;     0- Offset specified is from the beginning of the file
               ;     1- Offset specified is distance from the current file pointer
               ;     2- The pointer is moved to the end of file minus the specified offset
               ; BX= File Handle
               ; CX:DX= Distance to move, in bytes
               ; OUTPUT ->
               ;  clc (cf=0) -> successfull -> 
               ;       DX:AX = New file position   
               ;  stc (cf=1) -> error code in AX
               ;       AL = 1 -> Invalid function
               ;       AL = 6 -> Invalid handle
               ; Note: Same register values at return (except DX:AX)

loc_move_fp_check_valid_function:
                cmp al, 2
                ja short loc_move_fp_invalid_function 

loc_move_fp_check_file_handle:
                push ds
                push ax 
                mov ds, bx
 
                mov al, byte ptr DS:[OF_Type]
                cmp al, 'F'
                jne short loc_move_fp_invalid_handle

                mov al, byte ptr DS:[OF_OFL_SN]
                or al, al
                jz short loc_move_fp_invalid_handle
                cmp al, 32
                ja short loc_move_fp_invalid_handle
                ;
                mov bx, offset Open_Files_List
                dec al
                mov ah, OFL_EntrySize
                mul ah
                add bx, ax
                add bx, OFL_Handle
                mov ax, ds
                cmp ax, word ptr CS:[BX]
                jne short loc_move_fp_invalid_handle

loc_move_fp_check_moving_method:                
                pop bx  ; pushed ax, al= moving method
                cmp bl, 1
                ja short loc_move_fp_from_eof
                
                mov ax, cx
                xchg ax, dx ; now, dx:ax = distance to move 
                or bl, bl
                jz short loc_move_fp_ok

loc_move_fp_forward:
                add ax, word ptr DS:[OF_Pointer] 
                adc dx, word ptr DS:[OF_Pointer]+2
                jnc short loc_move_fp_ok
                cmp ch, 80h 
                jb short loc_move_fp_invalid_data                    
              
loc_move_fp_ok:
                mov word ptr DS:[OF_Pointer], ax
                mov word ptr DS:[OF_Pointer]+2, dx                   
loc_move_fp_retn:
                mov bx, ds
                pop ds
                retn 

loc_move_fp_from_eof:
                mov ax, word ptr DS:[OF_Size]
                mov bx, word ptr DS:[OF_Size]+2
               ; push ax
               ; or ax, bx
               ; pop ax 
               ; jz short loc_move_fp_invalid_data_stc

                add ax, dx
                adc bx, cx
                mov dx, bx
                jmp short loc_move_fp_ok  

loc_move_fp_invalid_handle:
                pop bx ; pushed ax
                mov ax, 06h ; Invalid handle
                stc
                jmp short loc_move_fp_retn

loc_move_fp_invalid_function:
                mov ax, 01h ; Invalid function
                stc
                retn

;loc_move_fp_invalid_data_stc
;                stc
loc_move_fp_invalid_data:
                mov ax, 0Dh ; Invalid data
                jmp short loc_move_fp_retn

proc_move_file_pointer endp


move_source_file_to_destination_file proc near
                ; 01/08/2011
                ; 19/06/2011
                ; 19/05/2011 
                ; 18/05/2011 
                ; 17/05/2011
                ; 16/05/2011  
                ; 15/05/2011
                ;   Phase 1 -> Check destination file, 
                ;              'not found' is required
                ;   Phase 2 -> Check source file
                ;              'found' and proper attributes is required
                ;   Phase 3 -> Make destination directory entry,
                ;       add new dir cluster or section if it is required
                ;   Phase 4 -> Delete source directory entry.
                ;       cf=1 causes to return before the phase 4.
                ;    (source file protection against any possible errors)    
                ;
                ; 08/05/2011 major modification
                ;            -> destination file deleting is removed   
                ;            for msdos move/rename compatibility.
                ;            (Access denied error will return if
                ;            the destination file is found...)
                ;   Also ... DS:SI is changed from ff struct to pathname
                ;            ES:DI is changed from ff struct to pathname    
                ; 01/05/2011
                ; 30/04/2011 
                ; 24/04/2011
                ; 23/04/2011
                ; 04/08/2010
                ; INPUT -> DS:SI = Source File Pathname (Asciiz)
                ;          ES:DI = Destination File Pathname (Asciiz)
                ;         ; 15/05/2011
                ;          AL = 0 --> INT 21h
                ;          AL > 0 --> Command Interpreter (Question)
                ;          AL = 1 --> Question Phase
                ;          AL = 2 --> Progress Phase        
                ; OUTPUT -> clc -> OK
                ;           DX:AX = Destination directory first cluster
                ;           DS:SI = Logical Dos drive description table 
                ;           BX = Destination file structure offset
                ;           CX = 0 (CX > 0 --> calculate free space error)
                ;           stc -> Error code in AL  
                ;           DS, DX, CX, BX, ES, SI, DI will be changed


               ; 15/05/2011
                cmp al, 2
                je loc_msftdf_df2_check_directory
                mov byte ptr CS:[move_cmd_phase], al ; 16/05/2011

               ; 08/05/2011
                push es
                push di
loc_msftdf_parse_sf_path:
                mov ax, cs
                mov es, ax
               ; ds:si = Asciiz pathname 
                mov di, offset SourceFile_Drv
                call proc_parse_pathname
                jnc short loc_msftdf_parse_df_path

loc_msftdf_parse_sf_path_failed:                
                pop di
                pop es
               ; 19/06/2011  
                jmp short loc_msftdf_push_cs_pop_ds_retn

loc_msftdf_parse_df_path:                
                pop si ; pushed di 
                pop ds ; pushed es
                ; es = cs
                mov di, offset DestinationFile_Drv
               ; 19/06/2011
                call proc_parse_pathname
                jnc short loc_msftdf_check_sf_drv

                cmp al, 1 ; File or directory name is not existing
                jna short loc_msftdf_check_sf_drv

loc_msftdf_parse_df_path_failed:
                stc
loc_msftdf_push_cs_pop_ds_retn:
               ; 18/05/2011
                push cs
                pop ds   
                retn 

loc_msftdf_check_sf_drv:
                mov ax, cs
                mov ds, ax
               ;17/05/2011
                mov al, byte ptr [SourceFile_Drv]
loc_msftdf_check_df_drv:
                mov dl, byte ptr [DestinationFile_Drv]
loc_msftdf_compare_sf_df_drv:
                mov bh, byte ptr [Current_Drv]
                cmp dl, al
                je short loc_msftdf_check_sf_df_drv_ok
loc_msftdf_not_same_drv:
               ; dl = source file's drive 
                mov dh, al ; destination file's drive
                xor bl, bl
                mov ax, 11h ; Not the same drive
                stc
loc_msftdf_ppn_failed:
                retn 

loc_msftdf_different_df_drv:
                and dl, 0DFh ; capitalize drive name
                jmp short loc_msftdf_compare_sf_df_drv   

loc_msftdf_check_sf_df_drv_ok:
                mov byte ptr [msftdf_sf_df_drv], dl

               ; 16/05/2011
                mov ah, dl
                xor al, al
                add ax, offset Logical_DOSDisks
                mov word ptr [msftdf_drv_offset], ax

                cmp dl, bh ; byte ptr [Current_Drv]
                je short loc_msftdf_df_check_directory

loc_msftdf_change_drv:
                call proc_change_current_drive
                jc short loc_msftdf_df_error_retn
                  
loc_msftdf_check_destination_file:
loc_msftdf_df_check_directory:
              ; 17/05/2011
                mov si, offset DestinationFile_Directory
                cmp byte ptr [SI], 20h
                jna short loc_msftdf_df_find_1
loc_msftdf_df_change_directory:
                inc byte ptr [Restore_CDIR]
                xor ah, ah ; CD_COMMAND sign -> 0 
                call proc_change_current_directory
                jc short loc_msftdf_df_error_retn

;loc_msftdf_df_change_prompt_dir_string:
;                call proc_change_prompt_dir_string
loc_msftdf_df_find_1:
                mov si, offset DestinationFile_name
               ;30/04/2011
                cmp byte ptr [SI], 20h
                jna short loc_msftdf_df_copy_sf_name
loc_msftdf_df_find_2:
                xor ax, ax ; DestinationFile_AttributesMask -> any/zero
                call proc_find_first_file
                ; 08/05/2011
                jnc short loc_msftdf_permission_denied_retn

loc_msftdf_df_check_error_code:
                cmp al, 2
                jne short loc_msftdf_df_stc_retn
              
loc_msftdf_convert_df_direntry_name:
               ; 16/05/2011
               ;  push ds
               ;  pop es   
                mov si, offset DestinationFile_Name
                mov di, offset DestinationFile_DirEntry
                call proc_convert_file_name
               ; 15/05/2011
                jmp short loc_msftdf_restore_current_dir_1

loc_msftdf_df_copy_sf_name:
               ; 08/05/2011
                mov di, si
                push di 
                mov si, offset SourceFile_name
                mov cx, 12
loc_msftdf_df_copy_sf_name_loop:
                lodsb
               ; 15/05/2011
                stosb
                or al, al
                jz short loc_msftdf_df_copy_sf_name_ok                
                loop short loc_msftdf_df_copy_sf_name_loop
loc_msftdf_df_copy_sf_name_ok:                
                pop si  
                jmp short loc_msftdf_df_find_2

loc_msftdf_df_stc_retn:
                stc
loc_msftdf_restore_cdir_failed:
loc_msftdf_df_error_retn:
                retn

loc_msftdf_restore_current_dir_1:
               ; 17/05/2011
                cmp byte ptr [Restore_CDIR], 0
                jna short loc_msftdf_sf_check_directory
 		mov si, word ptr [msftdf_drv_offset] ; 16/05/2011 
                call proc_restore_current_directory
                jc short loc_msftdf_restore_cdir_failed

              ; 17/05/2011
loc_msftdf_sf_check_directory:
                mov si, offset SourceFile_Directory
                cmp byte ptr [SI], 20h
                jna short loc_msftdf_sf_find
loc_msftdf_sf_change_directory:
                inc byte ptr [Restore_CDIR]
                xor ah, ah ; CD_COMMAND sign -> 0 
                call proc_change_current_directory
                jc short loc_msftdf_return

;loc_msftdf_sf_change_prompt_dir_string:
;                call proc_change_prompt_dir_string

loc_msftdf_sf_find:
                mov si, offset SourceFile_name  ; Offset 66
                mov ax, 1800h ; Only files ; 19/05/2011
                call proc_find_first_file
                jc short loc_msftdf_return

loc_msftdf_sf_ambgfn_check:
              ; 19/05/2011     
                or bx, bx ; Ambiguous filename chars used sign (BX>0)
                jz short loc_msftdf_sf_found

loc_msftdf_ambiguous_file_name_error:
                mov ax, 2 ; File not found error
                stc
                retn

loc_msftdf_sf_found:
                and cl, 1Fh ; Attributes, D-V-S-H-R
                jz short loc_msftdf_save_sf_structure

loc_msftdf_permission_denied_retn:
                mov ax, 05h ; Access (Permission) denied !
                stc
loc_msftdf_return:
                retn

loc_msftdf_phase_1_return:
               ; 17/05/2011
                xor ah, ah
                mov byte ptr [move_cmd_phase], ah
                mov al, 1

               ; 15/05/2011
                mov bx, offset loc_msftdf_df2_check_directory
               ;mov dx, 0FFFFh 
                retn

loc_msftdf_save_sf_structure:
               ; 15/05/2011
               ; 08/05/2011
               ; 30/04/2011
                mov al, ch ; Long name entry length
                push ds
                pop es
                mov si, offset FindFile_DirEntry
                mov di, offset SourceFile_DirEntry
                mov cx, 16
                rep movsw

loc_msftdf_df_copy_sf_parameters:
               ; 16/05/2011          
                mov si, 11
                mov di, si
                add si, offset SourceFile_DirEntry
                add di, offset DestinationFile_DirEntry
                mov cx, 21
                rep movsb

loc_msftdf_restore_current_dir_2:
               ; 16/05/2011
                cmp byte ptr [Restore_CDIR], 0
                jna short loc_msftdf_df2_check_move_cmd_phase
 		mov si, word ptr [msftdf_drv_offset]
                call proc_restore_current_directory
                jc short loc_msftdf_restore_cdir_failed

loc_msftdf_df2_check_move_cmd_phase:
                ; 17/05/2011
                cmp byte ptr [move_cmd_phase], 1
                je short loc_msftdf_phase_1_return

loc_msftdf_df2_check_directory:
               ; 15/05/2011
                mov si, offset DestinationFile_Directory
                cmp byte ptr [SI], 20h
                jna short loc_msftdf_make_dfde_locate_ffe_on_directory
loc_msftdf_df2_change_directory:
                inc byte ptr [Restore_CDIR]
                xor ah, ah ; CD_COMMAND sign -> 0 
                call proc_change_current_directory
                jc short loc_msftdf_return

;loc_msftdf_df2_change_prompt_dir_string:
;                call proc_change_prompt_dir_string

loc_msftdf_make_dfde_locate_ffe_on_directory:
               ; Current directory fcluster <> Directory buffer cluster
               ; Current directory will be reloaded by
               ; proc_locate_current_dir_file procedure
               ;
                xor ax, ax
                mov cx, ax
                dec cx ; FFFFh  
                ; CX = FFFFh -> find first deleted or free entry
                 ; DS:SI would be ASCIZZ filename Address if the call
                 ; would not be for first free or deleted dir entry  
                call proc_locate_current_dir_file
                jnc loc_msftdf_make_dfde_set_ff_dir_entry
                
                cmp ax, 2
               ; 15/05/2011
                jne short loc_msftdf_error_retn

loc_msftdf_add_new_dir_entry_check_fs:
               ; 16/05/2011
                mov si, word ptr [msftdf_drv_offset]
                mov ax, word ptr [DirBuff_Cluster]
                mov dx, word ptr [DirBuff_Cluster]+2
                cmp byte ptr [SI][LD_FATType], 0
                ja short loc_msftdf_add_new_subdir_cluster

loc_msftdf_add_new_fs_subdir_section:
               ; 01/05/2011
               ; cl=0, ch=E5h --> deleted entry, ch=0 --> free entry
               ;xor cx, cx
                xor ch, ch ; cx=0 --> add a new subdir section
                call  proc_add_new_fs_section
               ; 15/05/2011
                jc short loc_msftdf_dsfde_error_retn
                
              ; mov word ptr [createfile_LastDirCluster], ax
              ; mov word ptr [createfile_LastDirCluster]+2, dx

                call proc_load_FS_sub_directory 
                push ds
                pop es  
                ;mov bx, word ptr [Directory_Buffer] 

               ; 15/05/2011
                jnc short loc_msftdf_add_new_fs_subdir_section_ok
                retn                 

loc_msftdf_add_new_subdir_cluster:
              ; 01/05/2011
                call proc_add_new_cluster
              ; 15/05/2011
                jc short loc_msftdf_dsfde_error_retn
               
              ; mov word ptr [createfile_LastDirCluster], ax
              ; mov word ptr [createfile_LastDirCluster]+2, dx

                call proc_load_FAT_sub_directory
               ; 15/05/2011
                jnc short loc_msftdf_add_new_subdir_cluster_ok
                ; bx = Directory buffer segment

loc_msftdf_ansdc_update_parent_dir_lmdt:
loc_msftdf_make_dfde_err_upd_parent_dir_lmdt:
               ; 15/05/2011
                push ax
                call proc_update_parent_dir_lmdt
                pop ax
                stc
loc_msftdf_dsfde_restore_cdir_failed:
loc_msftdf_error_retn:
loc_msftdf_dsfde_error_retn:
                retn

loc_msftdf_add_new_fs_subdir_section_ok:
loc_msftdf_add_new_subdir_cluster_ok:
               ; 15/05/2011
                mov es, bx ; Directory buffer segment
                xor di, di

loc_msftdf_make_dfde_set_ff_dir_entry:
               ; 15/05/2011
                push word ptr [Current_Dir_FCluster]
                pop word ptr [createfile_FFCluster]
                push word ptr [Current_Dir_FCluster]+2
                pop word ptr [createfile_FFCluster]+2
               
               ; 16/05/2011
               ; es = Directory buffer segment
               ; di = Directory entry offset
                mov si,  offset DestinationFile_DirEntry
                mov cx, 16
                rep movsw
                push ds
                pop es
               ; 01/08/2011
                mov byte ptr [DirBuff_ValidData], 2 
                call proc_save_directory_buffer
                jc short loc_msftdf_make_dfde_err_upd_parent_dir_lmdt

loc_msftdf_make_dfde_update_parent_dir_lmdt:
               ; 15/05/2011
                call proc_update_parent_dir_lmdt

loc_msftdf_dsfde_restore_current_dir_1:
               ;15/05/2011
                cmp byte ptr [Restore_CDIR], 0
                jna short loc_msftdf_dsfde_check_directory
 		mov si, word ptr [msftdf_drv_offset] ; 16/05/2011
                call proc_restore_current_directory
                jc short loc_msftdf_dsfde_restore_cdir_failed

loc_msftdf_dsfde_check_directory:
                mov si, offset SourceFile_Directory
                cmp byte ptr [SI], 20h
                jna short loc_msftdf_dsfde_find_file
loc_msftdf_dsfde_change_directory:
                inc byte ptr [Restore_CDIR]
                xor ah, ah ; CD_COMMAND sign -> 0 
                call proc_change_current_directory
                jc short loc_msftdf_dsfde_error_retn

;loc_msftdf_dsfde_sf_change_prompt_dir_string:
;                call proc_change_prompt_dir_string

loc_msftdf_dsfde_find_file:
                mov si, offset SourceFile_name  ; Offset 66
                mov ax, word ptr [SI]+14 ; 80 -> SourceFile_AttributesMask
                call proc_find_first_file
                jc short loc_msftdf_dsfde_error_retn

loc_msftdf_dsfde_delete_direntry:
               ; 16/05/2011
                mov si, word ptr [msftdf_drv_offset]

               ; 01/05/2011
                cmp byte ptr [SI][LD_FATType], 0
                ja short loc_msftdf_delete_FAT_direntry
                xor bl, bl ; 16/05/2011
               ;mov bl, 'F' ; bl = 0 -> File
                ; ES:DI -> Directory buffer entry offset/address 
                call proc_delete_fs_directory_entry
               ; 16/05/2011
                jnc short loc_msftdf_dsfde_restore_current_dir_2
                retn
               ;

loc_msftdf_delete_FAT_direntry:                
                mov bl, al ; Long name entry length
                mov cx, word ptr [DirBuff_EntryCounter]
                ; ES:DI -> Directory buffer entry offset/address 

                call proc_delete_directory_entry
                jc short loc_msftdf_retn

loc_msftdf_dsfde_restore_current_dir_2:
              ; 16/05/2011
                cmp byte ptr [Restore_CDIR], 0
                jna short loc_msftdf_new_dir_fcluster_retn
 		;mov si, word ptr [msftdf_drv_offset]
                call proc_restore_current_directory
                jc short loc_msftdf_retn

loc_msftdf_new_dir_fcluster_retn:
               ; 30/04/2011
                xor cx, cx 
                mov ax, word ptr [createfile_FFCluster]
                mov dx, word ptr [createfile_FFCluster]+2
                mov bx,  offset DestinationFile_Drv ; 16/05/2011

loc_msftdf_retn:
                retn

move_cmd_phase: db 0 ; 15/05/2011
msftdf_sf_df_drv: db 0
msftdf_drv_offset: dw 0

move_source_file_to_destination_file endp

PROC_GECICIKOD2 proc near

gecicikod2:
               push dx
               push ax
               call proc_hex
               mov word ptr [FC3], ax 
               pop ax
               xchg ah, al
               call proc_hex
	       mov word ptr [FC2], ax 
               pop ax
               push ax
               call proc_hex
	       mov word ptr [FC1], ax 
               pop ax
               xchg ah, al
               call proc_hex 
               mov word ptr [FC0], ax 
               mov si, offset FFC
               call proc_printmsg
               
retn

FFC: db 'First Cluster : '
FC0: dw 3030h
FC1: dw 3030h
FC2: dw 3030h
FC3: dw 3030h
db 'h ', 0Dh, 0Ah, 0

PROC_GECICIKOD2 endp

Open_Files_List: db 256 dup(0)

File_Open_Mode: db 0
File_Owner: db 0
File_OFL_Number: db 0
Open_File_Drive: db 0
Open_File_Size:
Open_File_Size1: dw 0
Open_File_Size2: dw 0
Open_File_FClust1: dw 0
Open_File_FClust2: dw 0
Open_File_Handle: dw 0
Open_File_Type: db 0
Open_File_FATType: db 0
Open_File_Sectors: dw 0
Open_File_Alloc_Size1: dw 0
Open_File_Alloc_Size2: dw 0
Allocation_Size: dw 0
;25/09/2010
OpenFile_BytesPerSec: dw 0
;
; 10/10/2010
File_rw_buffer: dw 0

;Source File Structure (same with 'Find File' Structure)
SourceFile_Drv: db 0
SourceFile_Directory: db 65 dup(0)
SourceFile_Name: db 13 dup(0)
SourceFile_LongNameEntryLength:
SourceFile_LongNameYes: db 0 ; Sign for longname procedures
;Above 80 bytes
;is TR-DOS Source File FullName Format/Structure
SourceFile_AttributesMask: dw 0
SourceFile_DirEntry: db 32 dup(0)
SourceFile_DirFirstCluster: dd 0
SourceFile_DirCluster: dd 0
SourceFile_DirEntryOffset: dw 0
SourceFile_MatchCounter: dw 0
; 19/11/2010
SourceFile_DirEntryNumber: dw 0
; Above is 128 bytes

;Destination File Structure (same with 'Find File' Structure)
DestinationFile_Drv: db 0
DestinationFile_Directory: db 65 dup(0)
DestinationFile_Name: db 13 dup(0)
DestinationFile_LongNameEntryLength:
DestinationFile_LongNameYes: db 0 ; Sign for longname procedures
;Above 80 bytes
;is TR-DOS Destination File FullName Format/Structure
DestinationFile_AttributesMask: dw 0
DestinationFile_DirEntry: db 32 dup(0)
DestinationFile_DirFirstCluster: dd 0
DestinationFile_DirCluster: dd 0
DestinationFile_DirEntryOffset: dw 0
DestinationFile_MatchCounter: dw 0
; 19/11/2010
DestinationFile_DirEntryNumber: dw 0
; Above is 128 bytes

; 16/01/2011
EnvSegment: dw 0
DTA_Address: dd 0

; 31/05/2011
;db 256 dup(0) ; ?
