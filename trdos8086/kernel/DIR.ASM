;*****************************************************************************
; DIR.ASM  [ TRDOS KERNEL - COMMAND EXECUTER SECTION - DIRECTORY FUNCTIONS ]
; (c) 2004-2010  Erdogan TAN  [ 17/01/2004 ]  Last Update: 09/10/2011
; ! Under Development !

proc_print_directory proc near
                ; 30/10/2010 File Size string loop BugFix (>= 1GB)
                ; 03/10/2009 Current Directory String Length
                ; 19/09/2009
                ; 2005 
                ; INPUT : DS:SI = Asciiz File/Dir Name Address
                ; INPUT : CS = DS = ES

                push    si
                
                mov 	word ptr [Dir_Count],0
                mov 	word ptr [File_Count],0
                mov 	word ptr [Total_FSize], 0
                mov 	word ptr [Total_FSize]+2, 0

                call    proc_clear_screen
                                
                mov     ah, byte ptr [Current_Drv] ; DirBuff_Drv - 'A'
                mov     al, byte ptr [Current_Dir_Drv] 
               ;mov     al, ah                
               ;add     al, 'A'
                mov     byte ptr [Dir_Drive_Name], al
                xor 	al, al

                mov     si, offset Logical_DosDisks
                add 	si, ax
                mov     di, offset Vol_Name
                mov     al, byte ptr [SI][LD_FATType]
             
              ; 19/12/2009 FS Directory Listing
              ; push    ax
              ; ....
                cmp     al, 2
                ja      short pass_print_add_si_ld_FAT_vname 
   		or      al, al
                jz      short loc_print_dir_fs_volume_name
                mov     ax, word ptr [SI][LD_BPB][VolumeID]
                mov 	word ptr [Current_VolSerial1], ax  
                mov     ax, word ptr [SI][LD_BPB][VolumeID]+2
                mov 	word ptr [Current_VolSerial2], ax  
                add     si, LD_BPB+VolumeLabel
                jmp     short pass_print_add_si_ld_FAT32_vname
loc_print_dir_fs_volume_name:
                mov     ax, word ptr [SI][LD_FS_VolumeSerial]
                mov 	word ptr [Current_VolSerial1], ax  
                mov     ax, word ptr [SI][LD_FS_VolumeSerial]+2
                mov 	word ptr [Current_VolSerial2], ax  
                add     si, LD_FS_VolumeName
 		mov     cx, 32
                rep     movsw
                jmp 	short  pass_print_dir_fat_volume_name
pass_print_add_si_ld_FAT_vname:
                mov     ax, word ptr [SI][LD_BPB][FAT32_VolID]
                mov 	word ptr [Current_VolSerial1], ax  
                mov     ax, word ptr [SI][LD_BPB][FAT32_VolID]+2
                mov 	word ptr [Current_VolSerial2], ax  
                add     si, LD_BPB+FAT32_VolLab
pass_print_add_si_ld_FAT32_vname:
                mov     cx, 11
                rep     movsb
                mov 	byte ptr [DI], 0
pass_print_dir_fat_volume_name:
                mov 	al, byte ptr [Current_VolSerial1]
                call	proc_hex
                mov 	word ptr [Vol_Serial1]+2, ax
                mov 	al, byte ptr [Current_VolSerial1]+1
                call 	proc_hex
                mov 	word ptr [Vol_Serial1], ax
                mov 	al, byte ptr [Current_VolSerial2]
                call 	proc_hex
                mov 	word ptr [Vol_Serial2]+2, ax
                mov 	al, byte ptr [Current_VolSerial2]+1
                call 	proc_hex
                mov 	word ptr [Vol_Serial2], ax

                mov     si, offset Current_Dir_Root
                mov     di, offset Dir_Str_Root
                
               ;xor     ch, ch
                mov     cl, byte ptr [Current_Dir_Strlen]
                inc     cl
                cmp     cl, 64
                jna     short pass_print_dir_strlen_shorting
                inc     si
                add     si, cx
                sub     si, 64 
                inc     di
                mov     ax, '..' 
                stosw
                mov     ah, 20h   ; mov ax, ' .'
                stosw
 
pass_print_dir_strlen_shorting:
                rep     movsb

                mov     si, offset Dir_Drive_Str
                call    proc_printmsg

                mov     si, offset Vol_Serial_Header
                call    proc_printmsg

                mov     ah,0Eh
                mov     bx,07h
                mov     al,0Dh
                int     10h
                mov     al,0Ah
                int     10h
                mov     al,0Dh
                int     10h
                mov     al,0Ah
                int     10h

              ; 19/12/2009 FS Directory Listing
              ; pop     bx   ;pushed ax
              ; ....

          ; 19/12/2009 FS Directory listing 
          ;     or      bl,bl      ; bl = 0 => FS, not FAT
          ;     jnz     short loc_print_dir_first_file  

		;Print FS directory is not usable for now
                ;17/9/2009
          ;     pop     si
          ;     retn

loc_print_dir_first_file:
 		mov     byte ptr [PrintDir_RowCounter], 16
                mov     ax, word ptr [AttributesMask]
   		pop     si
                call    proc_find_first_file
                push ds
                pop es
                jc      loc_dir_ok
                 
loc_dfname_use_this:
                push    cx  ; CL = Attr = byte ptr [SI]+11
                test    cl, 10h  ; Is it a directory?
                jz      short loc_not_dir

                inc     word ptr [Dir_Count]
                push    si
                mov     cx, 10                   
                mov     si, offset Type_Dir      ; '<DIR>     '
                mov     di, offset Dir_Or_FileSize
                rep     movsb                    ; move 10 bytes
                pop     si

                jmp     short loc_dir_attribute
loc_not_dir:
                inc     word ptr [File_Count]

                add     word ptr [Total_FSize], ax
                adc     word ptr [Total_FSize]+2, dx

                mov     di,10 ; 30/10/2010 BugFix 
                        ; (mov di, 9 -> Bug for >=1 GB file size)
                mov     cx, di                  ; 16 bit divisor
loc_dir_rdivide:
                call    Rx_Dos_Div32            ; 32 bit divide
                add     bl, '0'                 ; to make visible
                dec     di
                mov     Byte Ptr [Dir_Or_FileSize][DI],bl
                cmp     ax,0
                ja      short loc_dir_rdivide
                cmp     dx,0
                ja      short loc_dir_rdivide
loc_dir_fill_space:
                cmp     di,0
                jna     short loc_dir_attribute
                dec     di
                mov     Byte Ptr [Dir_Or_FileSize][DI],20h
                jmp     short loc_dir_fill_space
loc_dir_attribute:
                pop     cx
                mov     word ptr [File_Attribute], 2020h
                mov     word ptr [File_Attribute]+2, 2020h
                cmp     cl, 20h  ; Is it an archive file?
                jb      short loc_dir_pass_arch
                mov     Byte Ptr [File_Attribute]+3,'A'
loc_dir_pass_arch:
                and     cl,7
                jz      short loc_dir_file_name
                mov     ch,cl
                and     cl,3
                cmp     ch,cl
                jna     short loc_dir_pass_s
                mov     byte ptr [File_Attribute], 'S'
loc_dir_pass_s:
                and     cl,2
                jz      short loc_dir_pass_h
                mov     byte ptr [File_Attribute]+1, 'H'
loc_dir_pass_h:
                and     ch,1
                jz      short loc_dir_file_name
                mov     byte ptr [File_Attribute]+2, 'R'
loc_dir_file_name:
                mov     bx, word ptr [SI]+18h ; Date
                mov     dx, word ptr [SI]+16h ; Time
                push    si
                mov     cx, 8
                mov     di, offset File_Name
                rep     movsb                    ; move 8 bytes
                mov     Byte Ptr [DI],20h
                inc     di
                mov     cx, 3
                rep     movsb                    ; move 3 bytes
                pop     si
Dir_Date_start:
		mov     ax, bx                   ; Date
		and     ax, 00011111b            ; Day Mask
		aam                              ; Q([AL]/10)->AH
						 ; R([AL]/10)->AL
						 ; [AL]+[AH]= Day as BCD
		or      ax, '00'                 ; Convert to ASCII
		xchg    al,ah
                mov     word ptr [File_Day],ax

		mov     ax, bx
		mov     cl, 5
		shr     ax, cl                   ; shift right 5 times
		and     ax, 00001111b            ; Month Mask
		aam
		or      ax, '00'
		xchg    ah,al
                mov     word ptr [File_Month],ax

		mov     ax, bx
		mov     cl, 9
		shr     ax, cl
		and     ax, 01111111b            ; Result = Year - 1980
		add     ax, 1980

		mov     cl, 10
		div     cl                        ; Q -> AL, R -> AH 
		or      ah, '0'
                mov     byte ptr [File_Year]+3,ah

		aam
		xchg    ah, al
		or      ah, '0'                  ; Convert to ASCII
                mov     byte ptr [File_Year]+2,ah

		aam
                xchg    al, ah
		or      ax, '00'
                mov     word ptr [File_Year],ax

Dir_Time_start:
		mov     ax, dx                   ; Time
		mov     cl, 5
		shr     ax, cl                   ; shift right 5 times
		and     ax, 0000111111b          ; Minute Mask
		aam
		or      ax, '00'
		xchg    ah,al
                mov     word ptr [File_Minute],ax

		mov     al, dh                   
		shr     al, 1
		shr     al, 1
		shr     al, 1                    ; ax = hours
		aam
		or      ax, '00'
		xchg    ah, al
                mov     word ptr [File_Hour],ax
loc_show_line:
                push    si
                mov     si, offset File_Name
                mov     cx, 46
		mov     ah, 0Eh
		mov     bx, 07h
loop_loc_dir_entry:
                lodsb                           
                int     10h                     
                loop    loop_loc_dir_entry  

                pop     si

                mov     al,0Dh
                int     10h
                mov     al,0Ah
                int     10h

                dec     byte ptr [PrintDir_RowCounter]
                jz      Pause_dir_scroll
loc_next_entry:
                call    proc_find_next_file
                push ds
                pop es
                jnc     loc_dfname_use_this

loc_dir_ok:
                mov     ax, word ptr [Dir_Count]
                mov     di, offset Decimal_Dir_Count
                cmp     ax, 10
                jb      short pass_ddc
                inc     di
                cmp     ax, 100
                jb      short pass_ddc
                inc     di
                cmp     ax, 1000
                jb      short pass_ddc
                inc     di
                cmp     ax, 10000
                jb      short pass_ddc
                inc     di
pass_ddc:
                mov     byte ptr [DI]+1, 0
loc_ddc_rediv:
                xor     dx, dx
                mov     cx, 10
                div     cx
                add     dl, "0"
                mov     byte ptr [DI], dl
                dec     di
                cmp     ax, 0
                ja      short loc_ddc_rediv

                mov     ax, word ptr [File_Count]
                mov     di, offset Decimal_File_Count
                cmp     ax, 10
                jb      short pass_dfc
                inc     di
                cmp     ax, 100
                jb      short pass_dfc
                inc     di
                cmp     ax, 1000
                jb      short pass_dfc
                inc     di
                cmp     ax, 10000
                jb      short pass_dfc
                inc     di
pass_dfc:
                mov     byte ptr [DI]+1, 0
loc_dfc_rediv:
                xor     dx, dx
                mov     cx, 10
                div     cx
                add     dl, "0"
                mov     byte ptr [DI], dl
                dec     di
                cmp     ax, 0
                ja      short loc_dfc_rediv

                mov     di, offset TFS_Dec_End
                mov     byte ptr [DI], 0
                mov     ax, word ptr [Total_FSize]
                mov     dx, word ptr [Total_FSize]+2
                mov     cx, 10
rediv_tfs_hex:
                call    Rx_Dos_Div32
                add     bl, "0"
                dec     di
                mov     byte ptr [DI], bl
                cmp     ax, 0
                ja      short rediv_tfs_hex
                cmp     dx, 0
                ja      short rediv_tfs_hex
                mov     word ptr [TFS_Dec_Begin], di
                mov     si, offset Decimal_File_Count_Header
                call    proc_printmsg
                mov     si, offset str_files
                call    proc_printmsg
                mov     si, offset str_dirs
                call    proc_printmsg
                mov     si, word ptr [TFS_Dec_Begin]
                call    proc_printmsg
                mov     si, offset str_bytes
                call    proc_printmsg

                retn

Pause_dir_scroll:
                xor    ah, ah                                           
                int    16h
                cmp    al,1Bh
                je     loc_dir_ok
                mov    byte ptr [PrintDir_RowCounter], 16 ; Reset counter
                jmp    loc_next_entry

PrintDir_RowCounter: db 0

File_Count:     dw 0
Dir_Count:      dw 0
Decimal_File_Count_Header:  db 0Dh, 0Ah
Decimal_File_Count: db 6 dup(0)
str_files:      db " file(s) & "
Decimal_Dir_Count: db 6 dup(0)
str_dirs:       db " directory(s) ", 0Dh, 0Ah, 0
Total_FSize:    dd 0
TFS_Dec_Begin:  dw 0
                db 10 dup(0)
TFS_Dec_End:    db 0
str_bytes:      db " byte(s) in file(s)", 0Dh, 0Ah, 0

proc_print_directory endp

Proc_Find_First_File proc near
     ; 09/10/2011
     ; 11/09/2011
     ; 06/02/2011 Modification (Dir_File_Name -> Dir_Entry_Name)
     ; 19/11/2010 Directory Entry Number, Longname Entry Length 
     ; 29/10/2010 BugFix 
     ; 24/01/2010 FindFile_drv <- Current_Drv 
     ; 18/10/2009 
     ; 02/10/2009 proc_convert_file_name (mov si, di)
     ; 17/09/2009
     ; 2005
     ; INPUT -> DS:SI = ASCIIZ File/Dir/Path Name Address
     ; AL = Attributes AND mask (The AND result must be equal to AL)
     ;       bit 0 = Read Only
     ;       bir 1 = Hidden
     ;       bit 2 = System
     ;       bit 3 = Volume Label
     ;       bit 4 = Directory
     ;       bit 5 = Archive
     ;       bit 6 = Reserved, must be 0
     ;       bit 7 = Reserved, must be 0
     ; AH = Attributes NAND mask (The AND result must be ZERO)
     ;
     ; OUTPUT -> CF = 1 : Error, Error Code in AX (AL)
     ; CF = 0 -> DS:SI = Directory Entry (FindFile_DirEntry) Location
     ;           ES:DI = Directory Buffer Directory Entry Location
     ;           DX:AX = File Size
     ;           CL = Attributes of The File/Directory
     ;           CH = Long Name Yes/No Status (>0 is YES)
     ;           BX > 0 : Ambiguous filename chars are used

     push cs ; 11/09/2011
     pop es ; 11/09/2011
     mov word ptr CS:[FindFile_AttributesMask], ax
     mov di, offset FindFile_DirEntry ; TR-DOS Fullfilename formatted buffer
     xor ax, ax
     mov cx, 23  ; 29/10/2010, 23 -> 22 BugFix -> 19/11/2010, 23 (+2 byte) 
     rep stosw    
  
    ; mov di, offset FindFile_Drv ; TR-DOS Fullfilename formatted buffer
    ; add di, 66 ; Dot File Name
     
     mov di, offset FindFile_Name ; FFF structure, offset 66
     push di 
    ; mov cx, 12
     mov cl, 12 ; 11/09/2011
     rep movsb
    ;xor al, al ; 09/10/2011
     stosb
     pop si
     push cs
     pop ds 
     mov di, offset Dir_Entry_Name ; Dir Entry Format File Name
     call proc_convert_file_name
    ; 02/10/2009
    ;mov si, offset Dir_Entry_Name
     mov si, di
     mov ax, word ptr [FindFile_AttributesMask]
    ;xor cx, cx
     xor cl, cl ; 11/09/2011  
     call proc_locate_current_dir_file
     jc short loc_fff_retn
     ; ES:DI = Directory Entry

    ; 19/11/2010
     xor ch, ch 
     xor dh, 0Fh
     jz short loc_fff_longname_yes
     mov byte ptr [FindFile_LongNameYes], ch
     jmp short loc_fff_longname_no

loc_fff_longname_yes:
    ;inc byte ptr [FindFile_LongNameYes]
     mov cl, byte ptr [LongName_EntryLength]  
     mov byte ptr [FindFile_LongNameEntryLength], cl

loc_fff_longname_no:
    ; 19/11/2010 optimization
    ;mov bx, word ptr [DirBuff_CurrentEntry]
     mov word ptr [FindFile_DirEntryOffset], bx
     mov bx, ax ; Ambigouos Filename chars used sign > 0

    ;24/01/2010 FindFile_Drv = Current_Drv
     mov al, byte ptr [Current_Drv]
     mov byte ptr [FindFile_Drv], al 

     mov ax, word ptr [Current_Dir_FCluster]
     mov dx, word ptr [Current_Dir_FCluster]+2
     mov word ptr [FindFile_DirFirstCluster], ax
     mov word ptr [FindFile_DirFirstCluster]+2, dx

loc_fff_fnf_return:
    ; 19/11/2010 
     mov ax, word ptr [DirBuff_Cluster]
     mov dx, word ptr [DirBuff_Cluster]+2
     mov word ptr [FindFile_DirCluster], ax
     mov word ptr [FindFile_DirCluster]+2, dx

     inc word ptr [FindFile_MatchCounter]

     push es
     push ds
     pop es
     pop ds

     push di
     mov si, di
     mov di, offset FindFile_DirEntry
     push di
     mov cl, 16
     rep movsw
     pop si
     pop di

     push ds
     pop es 
     push cs
     pop ds

     mov ax, word ptr [FindFile_DirEntry]+28 ; File Size Low Word
     mov dx, word ptr [FindFile_DirEntry]+30 ; File Size High Word

     mov cl, byte ptr [FindFile_DirEntry]+11 ; File Attributes 
     mov ch, byte ptr [FindFile_LongNameYes]

     push word ptr [DirBuff_EntryCounter]
     pop word ptr [FindFile_DirEntryNumber]

loc_fff_retn:
     retn

;FFF Structure
FindFile_Drv: db 0
FindFile_Directory: db 65 dup(0)
FindFile_Name: db 13 dup(0)
FindFile_LongNameEntryLength:
FindFile_LongNameYes: db 0 ; Sign for longname procedures
;Above 80 bytes
;is TR-DOS Source/Destination File FullName Format/Structure
FindFile_AttributesMask: dw 0
FindFile_DirEntry: db 32 dup(0)
FindFile_DirFirstCluster: dd 0
FindFile_DirCluster: dd 0
FindFile_DirEntryOffset: dw 0
FindFile_MatchCounter: dw 0
; 19/11/2010
FindFile_DirEntryNumber: dw 0
; Above is 128 bytes

;06/02/2011
Dir_Entry_Name: db 13 dup(0)
;

Proc_Find_First_File endp    

Proc_Find_Next_File proc near
     ; 06/02/2011 Modification (Dir_File_Name -> Dir_Entry_Name)
     ; 19/11/2010
     ; 29/10/2010 
     ; 27/09/2009
     ; 26/09/2009
     ; 17/09/2009
     ; 2005
     ; INPUT -> NONE, Find First File Parameters (CS=DS=ES)
     ; OUTPUT -> CF = 1 : Error, Error Code in AX (AL)
     ; CF = 0 -> DS:SI = Directory Entry (FindFile_DirEntry) Location
     ;           ES:DI = Directory Buffer Directory Entry Location
     ;           DX:AX = File Size
     ;           CL = Attributes of The File/Directory
     ;           CH = Long Name Yes/No Status (>0 is YES)
     ;           BX = Offset of FindFile (128 byte) Structure Address

                 ; 29/10/2010
                 cmp word ptr [FindFile_MatchCounter], 0
                 ja short loc_start_search_next_file
                 ;
loc_fnf_stc_retn:
                 stc
loc_fnf_ax12h_retn:
                 mov ax, 12h ; 18, No More files
;loc_fnf_retn:
                 retn

loc_start_search_next_file:
                 mov bx, word ptr [FindFile_DirEntryOffset]
                 inc bx
                 cmp bx, word ptr [DirBuff_LastEntry]
                 jna short loc_fnf_search

loc_cont_search_next_file:
                 mov bh, byte ptr [Current_Drv]
                 xor bl, bl
 		 mov si, offset Logical_DosDisks
                 add si, bx

                 ;10/01/2009 FS modification
                 cmp byte ptr [Current_Dir_Level], 0
                 jna short loc_fnf_check_FAT_type
                 cmp byte ptr [SI][LD_FATType], 1
                 jb short loc_fnf_ax12h_retn
                 jmp short loc_fnf_check_next_cluster
 
loc_fnf_check_FAT_type:
                 cmp byte ptr [SI][LD_FATType], 3
                 jb short loc_fnf_ax12h_retn

loc_fnf_check_next_cluster:
                 mov ax, word ptr [DirBuff_Cluster]
                 mov dx, word ptr [DirBuff_Cluster]+2
                 call proc_get_next_cluster
                 jnc short loc_fnf_load_next_dir_cluster
                 or ax, ax
                 jz short loc_fnf_stc_retn
                ;mov ax, 15h ;Drive not ready or read error
                 cmc ;stc
loc_fnf_retn:
                 retn

loc_fnf_load_next_dir_cluster:
                 call proc_load_FAT_sub_directory
                 jc short loc_fnf_retn
loc_fnf_search_xorcx:
                 xor bx, bx
		 mov word ptr [FindFile_DirEntryOffset], bx
		 xor cx, cx
loc_fnf_search:
                 mov si, offset Dir_Entry_Name  ; 06/02/2011
                 mov ax, word ptr [FindFile_AttributesMask]

                 call proc_find_direntry
                 jc short loc_cont_search_next_file

                ; 19/11/2010
                 xor ch, ch 
                 xor dh, 0Fh
                 jz short loc_fnf_longname_yes
                 mov byte ptr [FindFile_LongNameYes], ch
                 jmp short loc_fnf_longname_no

loc_fnf_longname_yes:
                 ;inc byte ptr [FindFile_LongNameYes]
                 mov cl, byte ptr [LongName_EntryLength]  
                 mov byte ptr [FindFile_LongNameEntryLength], cl

loc_fnf_longname_no:
                ; 19/11/2010
                ;mov bx, word ptr [DirBuff_CurrentEntry]
                 mov word ptr [FindFile_DirEntryOffset], bx
                 mov bx, ax ; Ambigouos Filename chars used sign > 0
                 jmp loc_fff_fnf_return
                 
Proc_Find_Next_File endp

proc_locate_current_dir_file proc near
                ; 14/08/2010 return optimization
                ; 25/01/2010 FS modification
                ; 26/09/2009
                ; 22/09/2009
                ; 19/09/2009
                ; 2005
                ; INPUT ->
                ;  DS:SI = DOS DirEntry Format FileName
                ;  AL = Attributes Mask 
                ;       ("AL AND EntryAttrib" must be equal to AL)
                ;  AH = Negative Attributes Mask (If AH>0)
                ;       ("AH AND EntryAttrib" must be ZERO)
                ;  CH > 0 Find First Free Dir Entry or Deleted Entry
                ;     CL = 0 -> Return the First Free Dir Entry
                ;     CL = E5h -> Return the 1st deleted entry
                ;     CL = FFh -> Return the 1st deleted or free entry
                ;     CL > 0 and CL<> E5h and CL<> FFh -> Return the first 
                ;         proper entry (which fits with Atributes Masks)
                ;  CX = 0 Find Valid File/Directory/VolumeName
                ;     ? = Any One Char
                ;     * = Every Chars
                ; OUTPUT ->
                ;  ES:DI = Directory Entry Address (in Directory Buffer)
                ;  DS:SI = DOS DirEntry Format FileName Address
                ;  CF=0 -> No Error, Proper Entry,
                ;  DL= Attributes
                ;  DH= Previous Entry Attr (LongName Check)
                ;  AL > 0 -> Ambiguous filename wildcard "?" used
                ;  AH > 0 -> Ambiguous filename wildcard "*" used
                ;  AX = 0 -> Filename full fits with directory entry
                ;  CH = The 1st Name Char of Current Dir Entry
                ;  CF=1 -> Proper entry not found, Error Code in AX/AL
                ;  CL=0 and CH=0 -> Free Entry (End Of Dir)
                ;  CL=0 and CH=E5h -> Deleted Entry fits with filters
                ;  CL >0 -> Entry not found, CH invalid
                ;  CF=0
                ;  BX = Current Directory Entry

                mov word ptr [DirBuff_EntryCounter], 0 ; Zero Based

               ;push ds
               ;pop es

                mov word ptr [CDLF_FNAddress], si
                mov word ptr [CDLF_AttributesMask], ax
                mov word ptr [CDLF_DEType], cx

                ;22/9/2009
                mov bh, byte ptr [Current_Drv]
                cmp byte ptr [DirBuff_ValidData], 0
                jna short loc_lcdf_reload_current_directory
                mov bl, byte ptr [DirBuff_Drv]
                sub bl, 'A'
                cmp bh, bl
                jne short loc_lcdf_reload_current_directory
                mov dx, word ptr [DirBuff_Cluster]
                cmp dx, word ptr [Current_Dir_FCluster]
                jne short loc_lcdf_reload_current_directory
                mov dx, word ptr [DirBuff_Cluster]+2
                cmp dx, word ptr [Current_Dir_FCluster]+2
                je short loc_cdir_locatefile_search

loc_lcdf_reload_current_directory:
                ;22/9/2009
                xor bl, bl
                mov si, offset Logical_DosDisks
                add si, bx
                call proc_reload_current_directory                 
                jnc short loc_locatefile_search_again 
                retn  
 
loc_cdir_locatefile_search:
                xor bx, bx
                call proc_find_direntry
		jnc short loc_cdir_locate_file_retn
                push ds
                pop es

loc_locatefile_check_stc_reason:
                or ch, ch
                jz short loc_cdir_locate_file_stc_retn

loc_locatefile_check_next_entryblock:
		mov bh, byte ptr [Current_Drv]
		xor bl, bl
                mov si, offset Logical_DosDisks
                add si, bx

                ;25/01/2010 FS modification
                cmp byte ptr [Current_Dir_Level], 0
                jna short loc_locatefile_check_FAT_type
            
               ; 14/08/2010  
                cmp byte ptr [Current_FATType], 1
                jnb short loc_locatefile_load_subdir_cluster
                retn  

loc_locatefile_check_FAT_type:
                cmp byte ptr [Current_FATType], 3
                jb short loc_cdir_locate_file_retn

loc_locatefile_load_subdir_cluster:
                mov ax, word ptr [DirBuff_Cluster]
                mov dx, word ptr [DirBuff_Cluster]+2
                call proc_get_next_cluster
                jnc short loc_locatefile_next_cluster
                or ax, ax
                jnz short loc_locatefile_drive_not_ready_read_err
                stc
loc_locatefile_file_notfound:
                mov ax, 2 ; File/Directory/VolName not found
                retn
loc_locatefile_drive_not_ready_read_err:
              ; mov ax, 15h ;Drive not ready or read error
loc_cdir_locate_file_stc_retn:
                cmc ;stc
loc_cdir_locate_file_retn:
                retn

loc_locatefile_next_cluster:
                call proc_load_FAT_sub_directory
               ;jc short loc_locatefile_drive_not_ready_read_err
                jc short loc_cdir_locate_file_retn 
loc_locatefile_search_again:
                mov si, word ptr [CDLF_FNAddress] 
                mov ax, word ptr [CDLF_AttributesMask]
                mov cx, word ptr [CDLF_DEType] 
                jmp short loc_cdir_locatefile_search

CDLF_FNAddress: dw 0
CDLF_AttributesMask: dw 0
CDLF_DEType: dw 0

proc_locate_current_dir_file endp

proc_find_direntry proc near
                ; 14/08/2010 'loc_find_dir_proper_direntry' relocation
                ; 18/07/2010 -> Bugfix (attributes 'and mask' AL)
                ; (longname entry was returning as found, while al=0)
                ; 13/07/2010 ('add di, 32' was duplicated) bugfix 
                ; 28/02/2010  (DirBuff_EntryCounter -> total count)
                ;
                ; 19/09/2009
                ; 2005
                ; INPUT -> DS:SI = Sub Dir or File Name Address
                ; AL = Attributes Mask 
                ;      ("AL AND EntryAttrib" must be equal to AL)
                ; AH = Negative Attributes Mask (If AH>0)
                ;      ("AH AND EntryAttrib" must be ZERO)
                ; CH > 0 Find First Free Dir Entry or Deleted Entry
                ;    CL = 0 -> Return the First Free Dir Entry
                ;    CL = E5h -> Return the 1st deleted entry
                ;    CL = FFh -> Return the 1st deleted or free entry
                ;    CL > 0 and CL<> E5h and CL<> FFh -> Return the first 
                ;         proper entry (which fits with Atributes Masks)
                ; CX = 0 Find Valid File/Directory/VolumeName
                ;    ? = Any One Char
                ;    * = Every Chars
                ; BX = Current Dir Entry
                ;
                ; RETURN -> 
                ; ES:DI = Directory Entry Address (in DirectoryBuffer)
                ; DS:SI = Sub Dir or File Name Address
                ; CF=0 -> No Error, Proper Entry,
                ; DL= Attributes
                ; DH= Previous Entry Attr (LongName Check)
                ; AL > 0 -> Ambiguous filename wildcard "?" used
                ; AH > 0 -> Ambiguous filename wildcard "*" used
                ; AX = 0 -> Filename full fits with directory entry
                ; BX= CurrentDirEntry
                ; CH = The 1st Name Char of Current Dir Entry
                ; CF=1 -> Proper entry not found, Error Code in AX/AL
                ; CL=0 and CH=0 -> Free Entry (End Of Dir)
                ; CL=0 and CH=E5h -> Deleted Entry fits with filters
                ; CL > 0 -> Entry not found, CH invalid
                ; CF
                                
                ; push  cs
                ; pop   ds

                mov     word ptr [DirBuff_CurrentEntry], bx  
                cmp     bx, word ptr [DirBuff_LastEntry]
                ja      loc_ffde_stc_retn_255

                mov     di, word ptr [Directory_Buffer]
                mov     es, di
                xor     di, di    
                mov     word ptr [FDE_AttrMask], ax

                mov     ax, 32
                              
                mov 	byte ptr [PreviousAttr], ah
		mov     word ptr [AmbiguousFileName], di

  		mul     bx
                add     di, ax

                or      ch, ch
                jnz     loc_find_free_deleted_entry_0

                or      cl, cl
                jnz     loc_ffde_stc_retn_255
               
check_find_dir_entry:
                mov     ax, word ptr [FDE_AttrMask]
                push    si
                mov     ch, byte ptr ES:[DI]
                cmp     ch, 0 ; Is it never used entry?
                jna     short pop_si_loc_find_direntry_stc_retn
                mov     dl, byte ptr ES:[DI]+0Bh
                cmp     ch, 0E5h ; Is it a deleted file?
                je      short loc_find_dir_next_entry_prevdeleted
                
                ; 17/10/2009 save longname sub component
                cmp     dl, 0Fh
                jne     short loc_check_attributes_mask
                call    proc_save_longname_sub_component
                
loc_check_attributes_mask:
                mov     dh, al
                and     dh, dl    
                cmp     al, dh
                jne     loc_find_dir_next_entry
              	and     ah, dl
                jnz     loc_find_dir_next_entry
                cmp     dl, 0Fh
                jne     short pass_direntry_attr_check
               ; 18/07/2010 Bugfix for AX=0 input and longname
                cmp     al, 0Fh ; AL = 0Fh -> find long name
                jne     loc_find_dir_next_entry
               ;
                pop     si
                xor     ax, ax
                mov     dh, byte ptr [PreviousAttr]
                mov     word ptr [DirBuff_CurrentEntry], bx
                retn

pass_direntry_attr_check:
                mov     dx, di
                mov     cx, 8
loc_lodsb_find_dir:
                lodsb
                cmp     al, '*'
                jne     short pass_fde_ambiguous1_check
                inc     byte ptr [AmbiguousFileName]+1
                jmp     short loc_check_direntry_extension
pass_fde_ambiguous1_check:
                cmp     al, '?'
                jne     short pass_fde_ambiguous2_check
                inc     byte ptr [AmbiguousFileName]
                cmp     byte ptr ES:[DI], 20h
                jna     short loc_find_dir_next_entry_dx
                jmp     short loc_scasb_find_dir_inc_di
pass_fde_ambiguous2_check:
                cmp     al, 20h
                jne     short loc_scasb_find_dir
                cmp     byte ptr ES:[DI], 20h
                jne     short loc_find_dir_next_entry_dx
                jmp     short loc_check_direntry_extension

pop_si_loc_find_direntry_stc_retn:
                pop     si
                jmp     short loc_find_direntry_stc_retn 

loc_find_dir_next_entry_prevdeleted:
                or      dl, 80h  ; Bit 7 -> deleted entry sign
                jmp     short loc_find_dir_next_entry

loc_scasb_find_dir:
                cmp     al, byte ptr ES:[DI]
                jne     short loc_find_dir_next_entry_dx
loc_scasb_find_dir_inc_di:
                inc     di
                loop    loc_lodsb_find_dir
loc_check_direntry_extension:
                pop     si
                push    si
                add     si, 8
                mov     di, dx
                add     di, 8
                mov     cl, 3
loc_lodsb_find_dir_ext:
                lodsb
                cmp     al, '*'
                jne     short pass_fde_ambiguous3_check
                inc     byte ptr [AmbiguousFileName]+1
                jmp     short loc_find_dir_proper_direntry

pass_fde_ambiguous3_check:
                cmp     al, '?'
                jne     short pass_fde_ambiguous4_check
                inc     byte ptr [AmbiguousFileName]
                cmp     byte ptr ES:[DI], 20h
                jna     short loc_find_dir_next_entry_dx
                jmp     short loc_scasb_find_dir_ext_inc_di
pass_fde_ambiguous4_check:
                cmp     al, 20h
                jne     short loc_scasb_find_dir_ext
                cmp     byte ptr ES:[DI], 20h
                jne     short loc_find_dir_next_entry_dx

                ; 14/08/2010
loc_find_dir_proper_direntry:
		xor     cl, cl
loc_find_dir_proper_direntry_1:
                pop     si
                mov     di, dx
                mov     ch, byte ptr ES:[DI]
                mov     dl, byte ptr ES:[DI]+0Bh
                mov     ax, word ptr [AmbiguousFileName]
loc_find_dir_proper_direntry_2:
                mov     dh, byte ptr [PreviousAttr]
                mov     word ptr [DirBuff_CurrentEntry], bx
                retn

loc_find_dir_next_entry_dx:
                mov     di, dx
loc_find_dir_next_entry:
                pop     si
                add     di, 32
                inc     word ptr [DirBuff_EntryCounter]
                inc     bx
                cmp     bx, word ptr [DirBuff_LastEntry]
                ja      short loc_ffde_stc_retn_255
                mov     byte ptr [PreviousAttr], dl ; LongName check
                jmp     check_find_dir_entry 

loc_scasb_find_dir_ext:
                cmp     al, byte ptr ES:[DI]
                jne     short loc_find_dir_next_entry_dx
loc_scasb_find_dir_ext_inc_di:
                inc     di
                loop    loc_lodsb_find_dir_ext
               ; 14/08/2010
                jmp short loc_find_dir_proper_direntry_1

loc_ffde_stc_retn_255:
                mov     cx, 0FFFFh
loc_find_direntry_stc_retn:
loc_check_ffde_retn_1:
                mov     ax, 2  ; File Not Found
                mov     dh, byte ptr [PreviousAttr]
                mov     word ptr [DirBuff_CurrentEntry], bx
                stc
                retn

loc_find_free_deleted_entry_0:
                mov    ax, word ptr [FDE_AttrMask]
                mov    ch, byte ptr ES:[DI]
                mov    dl, byte ptr ES:[DI]+0Bh
                or     cl, cl 
                jz     short loc_check_ffde_0_repeat
               ;cmp    cl, 0E5h
               ;je     short pass_loc_check_ffde_0_err
                cmp    cl, 0FFh
                je     short loc_find_free_deleted_entry_1
                jmp    short pass_loc_check_ffde_0_err
loc_check_ffde_0_repeat:
                or     ch, ch
                jnz    short loc_check_ffde_0_next
loc_check_ffde_retn_2:
                xor    ax, ax
                mov    dh, byte ptr [PreviousAttr]
                mov    word ptr [DirBuff_CurrentEntry], bx
                retn 
loc_check_ffde_0_next:
		inc    bx
               ; 13/07/2010 -> duplicated 'add di, 32' removed
                add    di, 32
                ; 28/02/2010 
                inc     word ptr [DirBuff_EntryCounter]
                ; 
                cmp    bx, word ptr [DirBuff_LastEntry]
                ja     short loc_ffde_stc_retn_255
		mov    byte ptr [PreviousAttr], dl
		mov    ch, byte ptr ES:[DI]
                mov    dl, byte ptr ES:[DI]+0Bh
                jmp    short loc_check_ffde_0_repeat

loc_find_free_deleted_entry_1:
                xor    dl, dl      
loc_find_free_deleted_entry_2:
                or     ch, ch  
                jz     short loc_check_ffde_retn_2
                cmp    ch, 0E5h
                je     short loc_check_ffde_retn_2
                inc    bx
                add    di, 32
                cmp    bx, word ptr [DirBuff_LastEntry]
                ja     short loc_ffde_stc_retn_255
		mov    ch, byte ptr ES:[DI]
                jmp    short loc_find_free_deleted_entry_2

pass_loc_check_ffde_0_err:
                cmp    ch, cl
                je     short loc_check_ffde_attrib 

                inc    bx
                add    di, 32
                cmp    bx, word ptr [DirBuff_LastEntry]
                ja     short loc_ffde_stc_retn_255
		mov    byte ptr [PreviousAttr], dl
		mov    ch, byte ptr ES:[DI]
                mov    dl, byte ptr ES:[DI]+0Bh
                jmp    short pass_loc_check_ffde_0_err

loc_check_ffde_attrib:
                mov    dh, al
                and    dh, dl    
                cmp    al, dh
                jne    short loc_check_ffde_0_next
                and    ah, dl
                jnz    short loc_check_ffde_0_next
                xor    cl, cl 
                jmp    short loc_check_ffde_retn_2  
                 
PreviousAttr: db 0
FDE_AttrMask: dw 0
AmbiguousFileName: dw 0

proc_find_direntry endp

proc_convert_file_name proc near
     ; 06/10/2009
     ; INPUT  -> DS:SI = Dot File Name Location
     ;           ES:DI = Dir Entry Format File Name Location
     ; OUTPUT -> ES:DI = Dir Entry Format File Name Location
     ;           DS:SI = Dot File Name Location (capitalized)
     ;           (CX and AL will be changed) 
     ; 2005
     push si  
     push di
     mov cx, 11
     mov al, 20h
     rep stosb
     pop di
     push di
     mov cx, 12
loc_check_first_dot:
     mov al, byte ptr [SI]
     cmp al, 2Eh
     jne short pass_check_first_dot
     mov byte ptr ES:[DI], al
     inc di
     inc si
     loop loc_check_first_dot
     jmp short stop_convert_file
loc_get_fchar:
     mov al, byte ptr [SI]
pass_check_first_dot:
     cmp al, 61h
     jb  short pass_name_capitalize
     cmp al, 7Ah
     ja  short pass_name_capitalize
     and al, 0DFh
     mov byte ptr [SI], al
pass_name_capitalize:
     cmp al, 21h
     jb  short stop_convert_file
     cmp al, 2Eh
     jne short pass_dot_space
add_space:
     cmp cx, 4
     jna short inc_and_loop
     inc di
     dec cx
     jmp short add_space
pass_dot_space:
     mov byte ptr ES:[DI], al
loc_after_double_dot:
     inc di
inc_and_loop:
     inc si
     loop loc_get_fchar
stop_convert_file:
     pop di
     pop si
     retn
proc_convert_file_name endp

Get_File_Name    proc near
                 ; INPUT ->  DS:SI -> Directory Entry Format File Name
                 ;           ES:DI -> DOS Dot File Name Address
                 ;
                 ; OUTPUT -> DS:SI -> DOS Dot File Name Address
                 ;           ES:DI -> Directory Entry Format File Name
                 ; 2005
                            
                 push ds
                 push es
                 push si
                 push di
                 lodsb
                 cmp al, 20h
                 jna short pass_gfn_ext
                 push si
                 stosb
                 mov cx, 7
loc_gfn_next_char:
                 lodsb
                 cmp al, 20h
                 jna short pass_gfn_fn
                 stosb
                 loop loc_gfn_next_char
pass_gfn_fn:
                 pop si
                 add si, 7
                 lodsb
                 cmp al, 20h
                 jna short pass_gfn_ext
                 mov ah, "."
                 xchg ah, al
                 stosw
                 lodsb
                 cmp al, 20h
                 jna short pass_gfn_ext
                 stosb
                 lodsb
                 cmp al, 20h
                 jna short pass_gfn_ext
                 stosb
pass_gfn_ext:                 
                 xor al, al
                 stosb
                 pop si
                 pop di
                 pop ds
                 pop es
                 retn

Get_File_Name    endp


proc_reload_current_directory proc near
               ; 13/06/2010 loc_reload_FAT_sub_directory bugfix  
               ; 24/04/2010 FS recognition, bugfix
               ; 03/10/2009 Current_FATType
               ; 23/09/2009
               ; 22/09/2009
               ; INPUT ->
               ; DS:SI = Dos drive description table address
               ;

              ;mov al, byte ptr [SI][LD_FATType]
               mov al, byte ptr [Current_FATType]
               cmp al, 2
               ja short loc_reload_FAT_sub_directory
               mov ah, byte ptr [Current_Dir_Level]
               or al, al
               jz short loc_reload_FS_directory
               or ah, ah
               jnz short loc_reload_FAT_sub_directory
loc_reload_FAT_12_16_root_directory:
               call proc_load_FAT_root_directory
               retn
loc_reload_FS_directory:
               and ah, ah
               jnz short loc_reload_FS_sub_directory 
loc_reload_FS_root_directory: 
               call proc_load_FS_root_directory
               retn
loc_reload_FS_sub_directory:
               mov ax, word ptr [Current_Dir_FCluster]
	       mov dx, word ptr [Current_Dir_FCluster]+2
               call proc_load_FS_sub_directory
               retn 
loc_reload_FAT_sub_directory:
               mov ax, word ptr [Current_Dir_FCluster]
	       mov dx, word ptr [Current_Dir_FCluster]+2
               call proc_load_FAT_sub_directory
               retn
 
proc_reload_current_directory endp


proc_change_current_directory proc near
               ; 18/09/2011
               ; 17/09/2011 
               ; 25/01/2010 Singlix FS Change DIR functionality
               ; 29/11/2009 ->
               ; INPUT -> AH = CD COMMAND (or another command) sign
               ;  cmp byte ptr [CD_COMMAND], 0CDh
               ; 'CD' command sign is for restoring cdir from 
               ;  or saving cdir to DOS Drv Description table
               ; 17/10/2009
               ; 16/10/2009 proc_parse_dir_name
               ; 14/10/2009 '.' & '..' modifications
               ; in proc_parse_dir_name
               ; 11/10/2009
               ; 04/10/2009
               ; INPUT -> DS:SI = Directory string
               ;         (ES=CS) 
               ; OUTPUT -> 
               ; ES:DI = DOS Drive Description Table (ES=CS)
               ; stc -> error
               ;  AX = Error code
               ; clc -> succesful
               ;  DS:SI = PATH_Array
               ;  DX:AX = Current Directory First Cluster
               ;  BH = Current Drive, BL = 0
               ;  
               
               ; 29/11/2009
                 mov byte ptr CS:[CD_COMMAND], ah
               ; 14/10/2009
               cmp byte ptr [SI], "/"
               jne short loc_ccd_cdir_level
               inc si
               xor al, al
               jmp short loc_ccd_parse_path_name
loc_ccd_cdir_level:
               mov al, byte ptr CS:[Current_Dir_Level]
loc_ccd_parse_path_name:
              ; 17/10/2009
               mov ah, al
               mov di, offset PATH_Array
              ; push cs
              ; pop es 

 ; Reset directory levels > cdir level
               ; is this required !?
               ;
               ; Relations:
               ; MAINPROG.ASM (pass_ccdrv_reset_cdir_FAT_fcluster)
               ; proc_parse_dir_name,
               ; proc_change_current_directory (this procedure)
               ; proc_change_prompt_dir_string 
               
               xor ch, ch
               mov cl, al
               inc cl
               shl cl, 1
	       shl cl, 1
               shl cl, 1
               shl cl, 1
               add di, cx
               mov cl, 7
               sub cl, al
               shl cl, 1
 	       shl cl, 1
               shl cl, 1
               push ax
               xor ax, ax
               rep stosw
               pop ax

              ; 17/10/2009 
               cmp byte ptr [SI], 20h
              ; 18/09/2011  
               cmc
               jnc short pass_ccd_parse_dir_name

               ; 16/10/2009
               ; push cs
               ; pop es
               mov di, offset PATH_Array
               ;push di
               ; DS:SI = Path name
               ; AL = CCD_Level
               call proc_parse_dir_name
               ; AL = CCD_Level 
               ; AH = Last_Dir_Level
               ;pop di
               push cs
               pop ds

pass_ccd_parse_dir_name:
               pushf ; 18/09/2011

              ;mov byte ptr [CCD_Level], al
              ;mov byte ptr [Last_Dir_Level], ah
               mov word ptr [CCD_Level], ax
               
               xor bl, bl
               mov bh, byte ptr [Current_Drv]
               mov si, offset Logical_DOSDisks
               add si, bx
              
               popf ; 18/09/2011 
               jc short loc_ccd_bad_path_name_retn
               
               mov word ptr [CCD_DriveDT], si
               ; 
               
               cmp al, 7
               jb short loc_ccd_load_child_dir

loc_ccd_bad_path_name_retn:
               ; 18/09/2011
               xchg si, di 
               xor ah, ah
               mov al, 18h ; DOS Error Code : Bad request structure length
               stc
loc_ccd_retn_p:
               retn

loc_ccd_load_child_dir:
               ; 16/10/2009
               ; al = CCD_Level
               or al, al
               jz short loc_ccd_load_root_dir

               ; 18/09/2011
               mov si, di  ; offset PATH_Array
               
               mov cx, ax
               cbw
              ; mov si, offset PATH_Array ; 18/09/2011
               shl al, 1
               shl al, 1
               shl al, 1
               shl al, 1
               add si, ax
               mov ax, word ptr [SI]+12
               mov dx, word ptr [SI]+14
               cmp cl, ch
               je loc_ccd_load_sub_directory
               mov word ptr [Current_Dir_FCluster], ax
               mov word ptr [Current_Dir_FCluster]+2, dx 
               ;  
loc_ccd_load_child_dir_next:
               add si, 16

               mov al, 00010000b ; 10h (Attrib AND mask)
              ;mov ah, 11001000b ; C8h
 	       mov ah, 00001000b ; 08h (Attrib NAND, AND=>zero mask)
               
               xor cx, cx  
               call proc_locate_current_dir_file
                                 ; ES=DS if cf=1 ; ES<>DS if cf=0 
               jnc short loc_ccd_set_dir_cluster_ptr

               mov di, word ptr [CCD_DriveDT] ; 18/09/2011
               mov ah, byte ptr [CCD_Level]
               
               cmp al, 3         ; AL=2 => File not found error
               jb short loc_ccd_path_not_found_retn
               stc
               retn

loc_ccd_path_not_found_retn:
               mov al, 3         ; Path not found
               retn

loc_ccd_load_FAT_root_dir:
               cmp byte ptr [Current_FATType], 2
               ja short loc_ccd_load_FAT32_root_dir

              ;mov si, word ptr [CCD_DriveDT]
              ;push si
               call proc_load_FAT_root_directory
              ;pop di ; Dos Drv Description Table
pass_ccd_load_FAT_root_dir:
               mov di, si
               mov si, offset PATH_Array
  	       push ds
	       pop es 
               jc short loc_ccd_retn_p
               xor ax, ax
               xor dx, dx
               jmp short loc_ccd_set_cdfc

loc_ccd_load_root_dir:
              ;25/01/2010
               cmp byte ptr [Current_FATType], 1
               jnb short loc_ccd_load_FAT_root_dir
loc_ccd_load_FS_root_dir:
               call proc_load_FS_root_directory
               jmp short pass_ccd_load_FAT_sub_directory

loc_ccd_load_FS_sub_directory_next:
             ; 25/01/2010
               call proc_load_FS_sub_directory
               jmp short pass_ccd_set_dir_cluster_ptr  

loc_ccd_set_dir_cluster_ptr:
             ; ES:DI = Directory Entry
               mov ax, word ptr ES:[DI]+26 ; First Cluster Low Word
               mov dx, word ptr ES:[DI]+20 ; First Cluster High Word

               push ds
               pop es
               mov si, word ptr [CCD_DriveDT]
               cmp byte ptr [Current_FATType], 1
               jb short loc_ccd_load_FS_sub_directory_next
               ;push si
               call proc_load_FAT_sub_directory
               ;pop di ; Dos Drv Description Table
pass_ccd_set_dir_cluster_ptr:
              ; mov di, si ; 18/09/2011
               mov si, offset PATH_Array
               ;push ds
               ;pop es 
               jc short loc_ccd_retn_c
               
               mov ax, word ptr [DirBuff_Cluster]
               mov dx, word ptr [DirBuff_Cluster]+2

               xor bh, bh
               inc byte ptr [CCD_Level]
               mov bl, byte ptr [CCD_Level]                   
               shl bl, 1
               shl bl, 1
               shl bl, 1
               shl bl, 1
               add si, bx
               mov word ptr [SI]+12, ax
               mov word ptr [SI]+14, dx
               jmp short loc_ccd_set_cdfc

loc_ccd_load_FAT32_root_dir:
               mov si, offset PATH_Array
               mov ax, word ptr [SI]+12
               mov dx, word ptr [SI]+14
               mov si, word ptr [CCD_DriveDT]
 
loc_ccd_load_FAT_sub_directory:
              ;push si
               call proc_load_FAT_sub_directory
              ;pop di ; Dos Drv Description Table
pass_ccd_load_FAT_sub_directory:
              ; mov di, si  ; 18/09/2011
               mov si, offset PATH_Array
               push ds
	       pop es 
               jc short loc_ccd_retn_c

               mov ax, word ptr [DirBuff_Cluster]
               mov dx, word ptr [DirBuff_Cluster]+2
loc_ccd_set_cdfc:
               xor bx, bx 
               mov cl, byte ptr [CCD_Level]
               mov byte ptr [Current_Dir_Level], cl ; 18/09/2011
               mov word ptr [Current_Dir_FCluster], ax
               mov word ptr [Current_Dir_FCluster]+2, dx
loc_ccd_check_cdlevel_lastlevel:
               mov ch, byte ptr [Last_Dir_Level]
               cmp cl, ch 
               jb loc_ccd_load_child_dir_next

               mov si, offset PATH_Array ; 17/09/2011

               ;29/11/2009 -> check cdir will be restored or saved ?
               cmp byte ptr [CD_COMMAND], 0CDh ;'CD' command or another
               je short loc_ccd_save_current_dir
               clc
loc_ccd_retn_c:
               mov di, word ptr [CCD_DriveDT] ; 18/09/2011
               retn
               ; jne -> don't save, but restore (the previous cdir) later !
               ; (saving the cdir would prevent previous cdir restoration!)

loc_ccd_load_sub_directory:
               ;25/01/2010
               mov si, word ptr [CCD_DriveDT]
               cmp byte ptr [Current_FATType], 1
               jnb short loc_ccd_load_FAT_sub_directory 
               call proc_load_FS_sub_directory
               jmp short pass_ccd_load_FAT_sub_directory 

loc_ccd_save_current_dir:
	       mov di, word ptr [CCD_DriveDT] ; 18/09/2011
              ;mov byte ptr [DI][LD_CDirLevel], cl
               push di
              ;add di, LD_CurrentDirectory
               add di, LD_CdirLevel ; 18/09/2011
               mov byte ptr [DI], cl
               inc di ; LD_CurrentDirectory 
               push si
               mov cx, 64
               rep movsw
               ; Current directory has been saved to 
               ; the DOS drive description table, cdir area !
               pop si  ; PATH_Array
               pop di  ; Dos Drv Description Table

               retn

CCD_Level: db 0
Last_Dir_Level: db 0
; 08/10/2009
CCD_DriveDT: dw 0
;
;29/11/2009
CD_COMMAND: db 0

proc_change_current_directory endp

proc_parse_dir_name proc near
                ; 18/09/2011
                ; 17/10/2009
                ; INPUT -> DS:SI = ASCIZZ Directory String
                ; AL = Current Directory Level
                ; INPUT -> ES:DI = Destination Adress
                ; (8 levels, each one 12+4 byte)
                ;  
                ; OUTPUT -> ES:DI = Dir Entry Formatted Array
                ; with zero cluster pointer at the last level
                ; (SI, BX, CX will be changed) 
                ; AH = Last Dir Level
                ; AL = Current Dir Level
                ;
               ;mov word ptr CS:[PATH_Array_Ptr], di
                mov ah, al
                mov word ptr CS:[PATH_CDLevel], ax
repeat_ppdn_check_slash:
                lodsb
                cmp al, "/"
                je short repeat_ppdn_check_slash
                cmp al, 21h
                jb short loc_ppdn_retn ; 18/09/2011
loc_ppdn_get_dir_name:
                mov cx, 12
                push es
                push di
                push cs
                pop es
                mov di, offset Dir_File_Name
repeat_ppdn_get_dir_name:
                stosb
                lodsb
                cmp al, "/"
                je short loc_check_level_dot_convert_dir_name
                cmp al, 20h
                jna short loc_ppdn_end_of_path_scan
                loop repeat_ppdn_get_dir_name
                pop di
                pop es
                stc
loc_ppdn_retn : ; 18/09/2011
                retn
loc_ppdn_end_of_path_scan:
                dec si
loc_check_level_dot_convert_dir_name:
                xor al, al
                stosb
                mov bx, si
                pop di
                pop es
                push ds
                push cs
                pop ds 
                mov si, offset Dir_File_Name
                lodsb
repeat_ppdn_name_check_dot:
                cmp al, "."
                jne short loc_ppdn_convert_sub_dir_name
repeat_ppdn_name_dot_dot:
                lodsb
                cmp al,"."
                je short loc_ppdn_dot_dot
                cmp al, 21h
                jb short pass_ppdn_convert_sub_dir_name
loc_ppdn_convert_sub_dir_name:
                mov ah, byte ptr [PATH_Level]
                cmp ah, 7
                jnb short pass_ppdn_convert_sub_dir_name
                inc ah                  
                mov byte ptr [PATH_Level], ah
                mov si, offset Dir_File_Name
               ;mov di, word ptr [PATH_Array_Ptr]
                mov al, 16
                mul ah
                or ax, ax
                jz short pass_ppdn_convert_sub_dir_name
                push di 
		add di, ax
                call proc_convert_file_name
                pop di
pass_ppdn_convert_sub_dir_name:
                mov si, bx
                pop ds
repeat_ppdn_check_last_slash:
                lodsb
                cmp al, "/"
                je short repeat_ppdn_check_last_slash
                cmp al, 21h
                jnb short loc_ppdn_get_dir_name
end_of_parse_dir_name:
                cmc  
               ;mov al, byte ptr CS:[PATH_CDLevel]
               ;mov ah, byte ptr CS:[PATH_Level]
                mov ax, word ptr CS:[PATH_CDLevel]
                retn
loc_ppdn_dot_dot:
                lodsb
                cmp al, 21h
                jb short loc_ppdn_dot_dot_prev_level
                pop ds
                jmp short end_of_parse_dir_name 
loc_ppdn_dot_dot_prev_level:                
                mov ax, word ptr [PATH_CDLevel]
                sub ah, 1
                adc ah, 0
                cmp al, ah
                jna short pass_ppdn_set_al_to_ah
                mov al, ah
pass_ppdn_set_al_to_ah:
                mov word ptr [PATH_CDLevel], ax
                jmp short pass_ppdn_convert_sub_dir_name

;PATH_Array_Ptr: dw 0
PATH_CDLevel: db 0
PATH_Level: db 0

proc_parse_dir_name endp

proc_change_prompt_dir_string proc near

               ; 27/03/2011 Major modification
               ; for INT 21h Function 47h compatibility
               ; (proc_get_current_directory procedure)   
               ; 09/10/2009
               ; INPUT/OUTPUT => none
               ; it changes current directory string/text  
               ; 2005

                mov     di, offset Current_Directory
                mov     ah, byte ptr [Current_Dir_Level]
                mov     si, offset PATH_Array
               ;push ds
               ;pop es 
                call    proc_return_current_directory_string
                mov     byte ptr [Current_Dir_Strlen], cl

                retn

proc_change_prompt_dir_string endp

proc_calculate_checksum proc near
             
               ; INPUT DS:SI = 11 byte Dos File Name location
               ; (in DOS Directory Entry Format)

               ; © Erdogan Tan [ 17-10-2009 ]
               ;  'ror al, 1' instruction

               ; © Erdogan Tan [ 20-06-2004 ]
               ; This 8086 assembly code is an original code
               ; which is adapted from C code in
               ; Microsoft FAT32 File System Specification
               ; Version 1.03, December 6, 2000
               ; Page 28

               xor al, al
               mov cx, 11
loc_next_sum:
              ;xor ah, ah
              ;test al, 1
              ;jz short pass_ah_80h
              ;mov ah, 80h
;pass_ah_80h:
              ;shr al, 1
               ror al, 1 ; 17/10/2009  
               add al, byte ptr [SI]
               inc si
              ;add al, ah
               loop loc_next_sum

               ; al= 8 bit checksum (CRC) value

               retn

proc_calculate_checksum endp

proc_find_longname proc near
                ; 24/01/2010 <- fs long file name
                ; 17/10/2009
                ; 2005

                ; INPUT -> 
                ; DS:SI = DOS short file name address
                ; for example: "filename.ext"
                ; ES=DS=CS

                ; OUTPUT ->
                ; DS:SI = ASCIZZ longname address (cf=0)
                ; cf=1, error number returns in AL
                ; AL= 0 & CF=1 -> longname not found
                ;       the file/directory has no longname
                ; 24/01/2010
                ; cf=0 -> AL = FAT Type 
               
loc_find_long_name:
               ; 17/10/2009
               ; ASCIIZ string will be returned
               ; as LongFileName
               ; reset is not needed
               ;mov cx, 33
               ;mov di, Offset LongFileName
               ;mov ax, 0
               ;rep stosw

               ;mov byte ptr [LongNameFound],0

               ;DS:SI = ASCIIZ file/directory name address
               ;   AL = Attributes AND mask 
               ;   AH = Negative attributes mask (nand)
                mov ax, 0800h 
                       ; it must not be volume name or longname

                call proc_find_first_file
                push ds
                pop es
                jc short loc_fln_retn
 
                ; 24/01/2010 
loc_fln_check_FAT_Type:
                cmp byte ptr [Current_FATType], 1
                jnb short loc_fln_check_longname_yes_sign

                ; 24/01/2010 
                call proc_get_fs_longname
                retn

loc_fln_check_longname_yes_sign:
                or ch, ch
                jnz short loc_fln_check_longnamefound_number
loc_fln_longname_not_found_retn:
                xor ax, ax  ; cf=1 & al = 0 -> longname not found
                stc
loc_fln_retn:
                retn

loc_fln_check_longnamefound_number:
                ; 'LongNameFound' is set by
                ; by 'proc_same_longname_sub_component'
                ; which is called from
                ; 'proc_find_directory_entry' 
                ; which is called from 
                ; 'proc_find_first_file'
                ; It must be 1 if the longname is valid
                cmp byte ptr [LongNameFound], 1
                jne short loc_fln_longname_not_found_retn
             
loc_fln_calculate_checksum: 
                call proc_calculate_checksum
                ; al = shortname checksum

loc_fln_longname_validation:
                ; 'LFN_Checksum' has been set already
                ; by 'proc_same_longname_sub_component'
                ; which is called from
                ; 'proc_find_directory_entry' 
                ; which is called from 
                ; 'proc_find_first_file'
                cmp byte ptr [LFN_Checksum], al
                jne short loc_fln_longname_not_found_retn

                mov si, offset LongFileName
               ; 24/01/2010
                mov al, byte ptr [Current_FATType]
                retn                
                
proc_find_longname endp

proc_save_longname_sub_component proc near
               ; 28/02/2010 longname entry length
               ; 19/10/2009 movsw
               ; 17/10/2009
               ; INPUT -> ES:DI =  Directory Entry    
               ; call from 'proc_find_directory_entry'
               ; If last entry returns with
               ; LongnameFound = 1 and
               ; LFN_Checksum = the next shortname checksum
               ; long name is valid.
               ; If a longname is longer than 65 bytes
               ; it is invalid for trdos. (>45h)
               

                push es
                push ds
                           
                push di
                push si
               ;push bx
               ;push cx
               ;push dx
                push ax
           
                mov al, byte ptr ES:[DI] ; LDIR_Order

                cmp al, 41h  ;40h (last long entry sign) +1
                jb short pass_pslnsc_last_long_entry

                ; 28/02/2010
                mov ah, al
                sub ah, 40h
                mov byte ptr [LongName_EntryLength], ah
                ;                

                cmp al, 45h  ;40h (last long entry sign) +5
                    ; Max 130 byte length is usable in TRDOS
                    ; 26*5 = 130
                ja short loc_pslnsc_retn

                and al, 07h ; 0Fh
                mov byte ptr [LongNameFound], al

                dec al
                mov cx, 26
                mul cl
                mov si, offset LongFileName
                add si, ax
                add si, cx
                    ; to make is an ASCIZZ string
                    ; with ax+26 bytes length
                mov word ptr [SI], 0   

                jmp short loc_pslsc_move_ldir_name2 

pass_pslnsc_last_long_entry:
                cmp al, 04h
                ja short loc_pslnsc_retn
                dec byte ptr [LongNameFound]
                cmp al, byte ptr [LongNameFound]
                jne short loc_pslnsc_retn 
loc_pslsc_move_ldir_name1:
                dec al
                mov ah, 26
                mul ah
loc_pslsc_move_ldir_name2:
                mov cl, byte ptr ES:[DI]+0Dh
                mov byte ptr [LFN_CheckSum], cl 
                push ds
                push es
                pop ds
                pop es
                mov si, di ; LDIR_Order
                mov di, offset LongFileName
                add di, ax
                inc si
                mov cx, 5 ; chars 1 to 5
loc_pslsc_ldir_name1:
                movsw
                loop loc_pslsc_ldir_name1 
                add si, 3
                mov cx, 6 ; char 6 to 11 
loc_pslsc_ldir_name2:
                movsw
                loop loc_pslsc_ldir_name2
                inc si
                inc si             
                movsw   ; char 12 
                movsw   ; char 13 

loc_pslnsc_retn:
                pop ax
               ;pop dx
               ;pop cx
               ;pop bx
                pop si  
                pop di                
               
                pop ds
                pop es   

                retn
                 
proc_save_longname_sub_component endp

proc_parse_pathname proc near
                ; 10/09/2011
                ; 19/06/2011
                ; 27/01/2011
                ; 20/11/2010
                ; 04/08/2010 No-path modification
                ; 21/04/2010 (loc_ppn_invalid_drive -> retn)
                ; 27-11-2009
                ; 5-12-2004
                ; Input DS:SI -> Beginning of ASCIIZ pathname string
                ;       ES:DI -> Destination Address
                ;                is TR-DOS Findfile data buffer
                ; Output, CF = 1 : Error, Error Code in AX (AL)
                ; 
                push di
                mov cx, 40
                xor ax, ax
                rep stosw ; Reset the pathname structure in TR-DOS Findfile data buffer 
                pop di

                mov ax, word ptr [SI]
                cmp ah, ':'
                je short loc_ppn_change_drive
                mov al, byte ptr CS:[Current_Drv]
                jmp short pass_ppn_change_drive

pass_ppn_cdir:
                mov si, word ptr CS:[First_Path_Pos]
                mov al, byte ptr [SI]
loc_ppn_get_filename:
                add di, 65 ; FindFile_FileName location
              ; mov cx, 12  ; TRDOS Filename length must not be more than 12 bytes
                mov cl, 12
loc_ppn_get_fnchar_next:
               ;mov byte ptr ES:[DI], al
               ;inc di
                stosb
                inc si
                mov al, byte ptr [SI]
                ; 27/01/2011
                cmp al, 21h
                ;jb short loc_ppn_get_fname_finish
                jb short loc_ppn_clc_return 
                loop short loc_ppn_get_fnchar_next
;loc_ppn_get_fname_finish:                
               ;xor al, al
               ;mov byte ptr ES:[DI], al
loc_ppn_return:
                retn

loc_ppn_change_drive:
                and al, 0DFh
                sub al, 'A'; A:
                jc short loc_ppn_invalid_drive
                cmp byte ptr CS:[Last_Dos_DiskNo], al
                jb short loc_ppn_invalid_drive

               ; 04/08/2010
                inc si
                inc si
                mov ah, byte ptr [SI]
               ; 19/06/2011 
                cmp ah, 21h
                jnb short pass_ppn_change_drive

loc_ppn_cmd_failed:
                ; 19/06/2011
                ; File or directory name is not existing
                mov byte ptr ES:[DI], al ; Drv 
                mov ax, 1
                ; TR-DOS Error Code 1h = Bad Command Argument
                ; MS-DOS Error Code 1h : Invalid Function Number
                ;stc
                ; (MainProg ErrMsg: "Bad command or file name!")
                retn

pass_ppn_change_drive:
                mov word ptr CS:[First_Path_Pos], si
                mov word ptr CS:[Last_Slash_Pos], 0
               ;mov byte ptr ES:[DI], al  ; FindFile_Drv
               ;inc di
                stosb
                mov al, byte ptr [SI]
loc_scan_ppn_dslash:
                cmp al, '/'
                jne short loc_scan_next_slash_pos
                mov word ptr CS:[Last_Slash_Pos], si
loc_scan_next_slash_pos:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                ja  short loc_scan_ppn_dslash
                cmp word ptr CS:[Last_Slash_Pos], 0
                jna short pass_ppn_cdir

                mov ax, word ptr CS:[Last_Slash_Pos]
                push ax
                mov si, word ptr CS:[First_Path_Pos]
                sub ax, si
                inc ax
               ;10/09/2011
                cmp ax, 64
                ja short loc_ppn_invalid_drive_stc
               ; 
               ;mov cx, ax
                mov cl, al
                push di ; Dest Dir String Location (65 byte)
                rep movsb
               ;mov byte ptr ES:[DI], ah ; 0, End of Dir String
                pop di
                pop si ; pushed ax
                inc si
                mov al, byte ptr [SI]
               ; 19/06/2011 
                cmp al, 21h
                jnb short loc_ppn_get_filename
loc_ppn_clc_return:
                clc
                retn

loc_ppn_invalid_drive_stc: ; 10/09/2011
                cmc
loc_ppn_invalid_drive:
                ; The Drive Letter/Char < "A" or > "Z"
                mov ax, 0Fh
                ; MS-DOS Error Code 0Fh = Disk Drive Invalid 
                ;stc
                ; (MainProg ErrMsg: "Drive not ready or read error!")
                retn
;loc_ppn_drive_not_ready:
                ; Drive is not ready or read error (from disk io process)
                ; or Drive Number > Last_Dos_DiskNo
;                mov ax, 15h
                ; MS-DOS Error Code 15h = Drive Not Ready
                ; stc
                ; (MainProg ErrMsg: "Drive not ready or read error!")
;                retn

First_Path_Pos: dw 0
Last_Slash_Pos: dw 0

proc_parse_pathname endp

proc_delete_longname proc near
                ; 01/08/2011
                ; 12/04/2010
                ; 28/03/2010
                ; 14/03/2010
                ; INPUT
                ; AX = Directory Entry Number
                ; OUTPUT
                ; clc -> ok
                ; stx -> error code in AL
                ; ES, DI, DX, AX, CX, BX will be destroyed 

                mov word ptr [DLN_EntryNumber], ax
 	       ;12/04/2010	
                mov byte ptr [DLN_40h], 40h

                call proc_locate_current_dir_entry
                jnc short loc_dln_check_attributes
                retn

loc_dln_longname_not_found:
                xor ah, ah
                mov al, 02h
                stc
                retn

loc_dln_check_attributes:
  		mov al, 0Fh
                mov ah, byte ptr ES:[DI]+0Bh
                cmp ah, al
                jne short loc_dln_longname_not_found
                mov ah, byte ptr ES:[DI]
                sub ah, byte ptr [DLN_40h]
                jna short loc_dln_longname_not_found         
               	cmp ah, 14h ; 84-64=20 -> 20*13=260 bytes
                ja short loc_dln_longname_not_found
             
                mov byte ptr ES:[DI], 0E5h
                mov byte ptr [DirBuff_ValidData], 2
               ; 12/04/2010
                mov byte ptr [DLN_40h], 0 ; 40h -> 0
                  
loc_dln_delete_next_ln_entry:
                cmp ah, 1
                jna short loc_dln_longname_retn
loc_dln_delete_next_ln_entry_0:
         	inc word ptr [DLN_EntryNumber]
		mov ax, word ptr [DLN_EntryNumber] 
                call proc_locate_current_dir_entry
                jnc short loc_dln_check_attributes

loc_dln_longname_stc_retn:
                retn 
                   
loc_dln_longname_retn:
               ;01/08/2011
 	       ;cmp byte ptr [DirBuff_ValidData], 2
               ;jne short loc_dln_longname_retn_xor_ax
                call proc_save_directory_buffer
                jc short loc_dln_longname_stc_retn

loc_dln_longname_retn_xor_ax:
                xor ax, ax
                retn

DLN_EntryNumber: dw 0
;12/04/2010
DLN_40h: db 0 ; 40h

proc_delete_longname endp

proc_locate_current_dir_entry proc near
                ; 01/08/2011
                ; 17/04/2010 BugFix
                ; 12/04/2010 BugFix
		; 28/03/2010
                ; 14/03/2010
                ; 07/03/2010
                ; INPUT ->
                ;  AX = Directory Entry Number
                ; OUTPUT ->
                ;  ES:DI -> Directory Entry
                ;  DX:AX = Dir Buff Cluster
                ;  BX = Dir Buff Entry Offset
                ;  CX = DirBuff Valid data
                ;   If clc and Cl =2 then
                ;   directory buffer modified
                ;   and must be written to disk
                ;   if cl= 1 then dir buffer 
                ;   has been written already
                ;  stc -> Error code in AL

               ; 12/04/2010
loc_locate_current_dir_entry:
                push si
		mov cx, ax
                mov dx, 32
                mul dx 
                mov word ptr [LCDE_ByteOffset], ax
                mov word ptr [LCDE_ByteOffset]+2, dx
                mov bh, byte ptr [Current_Drv]
	        xor bl, bl
                mov al, byte ptr [DirBuff_Drv]
                sub al, 'A'
  		mov si, offset Logical_DosDisks
                add si, bx
                cmp bh, al
                jne short loc_lcde_reload_current_directory
loc_lcde_cdl_check:
    	        cmp byte ptr [Current_Dir_Level], 0
                ja short loc_lcde_calc_dirbuff_cluster_offset
                mov ax, word ptr [SI][LD_BPB][RootDirEnts]
                dec ax
                xor dx, dx  
                cmp ax, cx ; cx = Directory Entry Number
                jb short loc_lcde_stc_12h_retn
                mov word ptr [LCDE_EntryOffset], cx
                xor ax, ax
                jmp short loc_lcde_check_dir_buffer_cluster

loc_lcde_stc_12h_retn:
                pop si
                mov bx, cx
                mov cx, dx
                mov ax, 12h ; AL= 12h No more files
                retn 

loc_lcde_calc_dirbuff_cluster_offset:
                mov bl, byte ptr [SI][LD_BPB][SecPerClust]
                xor bh, bh
                mov ax, word ptr [SI][LD_BPB][BytesPerSec]
                mul bx
 		or dx, dx ; If bytes per cluster > 32KB it is invalid
                jnz short loc_lcde_invalid_format
                mov cx, ax ; BYTES PER CLUSTER
                mov ax, word ptr [LCDE_ByteOffset]
                mov dx, word ptr [LCDE_ByteOffset]+2
                call RX_DOS_DIV32
                or dx, dx
                jnz short loc_lcde_invalid_format

                mov word ptr [LCDE_ClusterSN], ax

               ; 17/04/2010 
                mov ax, 32
                xchg ax, bx
                div bx 
                mov word ptr [LCDE_EntryOffset], ax
               ;

loc_lcde_get_current_sub_dir_fcluster:
                mov ax, word ptr [Current_Dir_FCluster]
                mov dx, word ptr [Current_Dir_FCluster]+2
loc_lcde_get_next_cluster:
                cmp word ptr [LCDE_ClusterSN], 0
                jna short loc_lcde_check_dir_buffer_cluster
                mov word ptr [LCDE_Cluster], ax
                mov word ptr [LCDE_Cluster]+2, dx
                call proc_get_next_cluster
                jc short loc_lcde_check_gnc_error
  		dec word ptr [LCDE_ClusterSN]
                jmp short loc_lcde_get_next_cluster

loc_lcde_reload_current_directory:
                push cx
                call proc_reload_current_directory
                pop cx
                jnc short loc_lcde_cdl_check
                pop si
                retn

loc_lcde_check_gnc_error:
                or ax, ax
                jnz short loc_lcde_drive_not_ready_read_err
                dec word ptr [LCDE_ClusterSN]
                jnz short loc_lcde_invalid_format 
                mov ax, word ptr [LCDE_Cluster]
                mov dx, word ptr [LCDE_Cluster]+2
                jmp short loc_lcde_check_dir_buffer_cluster                 

loc_lcde_invalid_format:
  		mov ax, 0Bh ; MSDOS Error code: Invalid Format
               ;mov ax, 0Dh ; MSDOS Error code: Invalid Data
               ;xor dx, dx
loc_lcde_drive_not_ready_read_err:
                stc
                pop si 
                retn  

loc_lcde_check_dir_buffer_cluster:
 		cmp ax, word ptr [DirBuff_Cluster]
                jne short loc_lcde_load_dir_cluster
                cmp dx, word ptr [DirBuff_Cluster]+2
                jne short loc_lcde_load_dir_cluster
		cmp byte ptr [DirBuff_ValidData], 0
                ja  short lcde_check_dir_buffer_cluster_next
                cmp byte ptr [Current_Dir_Level], 0    
                ja  short loc_lcde_load_dir_cluster_0
                mov ax, word ptr [SI][LD_RootBegin]
                mov dx, word ptr [SI][LD_RootBegin]+2
                jmp short loc_lcde_load_dir_cluster_1 
lcde_check_dir_buffer_cluster_next:
                mov bx, word ptr [LCDE_EntryOffset]
                cmp bx, word ptr [DirBuff_LastEntry]
                ja short loc_lcde_invalid_format 
                mov ax, 32
                mul bx
                or dx, dx
                jnz short loc_lcde_invalid_format
                mov di, word ptr [Directory_Buffer]  
                mov es, di
                mov di, ax
loc_lcde_dir_buffer_last_check:
                mov ax, word ptr [DirBuff_Cluster]
                mov dx, word ptr [DirBuff_Cluster]+2
                mov bx, word ptr [LCDE_EntryOffset]
                mov cl, byte ptr [DirBuff_ValidData]
                xor ch, ch
loc_lcde_retn:
                pop si
                retn

loc_lcde_load_dir_cluster:
               ; 01/08/2011
               ;cmp byte ptr [DirBuff_ValidData], 2
               ;jne short loc_lcde_load_dir_cluster_n2
                push dx
                push ax
                call proc_save_directory_buffer
                pop ax
                pop dx
                jc short loc_lcde_retn
loc_lcde_load_dir_cluster_n2:
                mov byte ptr [DirBuff_ValidData], 0
loc_lcde_load_dir_cluster_0:
                mov word ptr [DirBuff_Cluster], ax
               	mov word ptr [DirBuff_Cluster]+2, dx
	  	sub ax, 2
                sbb dx, 0
                xor bh, bh
                mov bl, byte ptr [SI][LD_BPB][SecPerClust]
                mov cx, bx
                call proc_mul32
                add ax, word ptr [SI][LD_DATAbegin]
                adc dx, word ptr [SI][LD_DATAbegin]+2
loc_lcde_load_dir_cluster_1:
                mov bx, word ptr [Directory_Buffer]
                push es
                mov es, bx
                xor bx, bx
                call proc_disk_read
                pop es
                jc short loc_lcde_retn
loc_lcde_validate_dirBuff:
                mov byte ptr [DirBuff_ValidData], 1
                jmp short lcde_check_dir_buffer_cluster_next

LCDE_EntryOffset: dw 0
LCDE_ClusterSN: dw 0
LCDE_Cluster: dd 0
LCDE_ByteOffset: dd 0

proc_locate_current_dir_entry endp

proc_delete_current_directory_entry proc near
                ; 14/03/2010
                ; INPUT ->
                ;  ES:DI Directory Entry Address
                ;  AX = Attributes Mask
                ; OUTPUT ->
                ;  ES:DI Directory Entry Address
                ;  clc -> deleted
                ;  stc -> not deleted 
                ;  AL = entry attributes
                ;  AH = entry/filename first char

                mov word ptr [DCDE_Attributes], ax
                mov ah, byte ptr ES:[DI]
                mov al, byte ptr ES:[DI]+0Bh
                cmp ah, 0E5h
                je short pass_dcde_delete_current_dir_entry
                or ah, ah
                jz short pass_dcde_delete_current_dir_entry  
		and byte ptr [DCDE_Attributes], al
                cmp al, byte ptr [DCDE_Attributes]
                jne short pass_dcde_delete_current_dir_entry  
                and byte ptr [DCDE_Attributes]+2, ah
                jnz short pass_dcde_delete_current_dir_entry  
                mov word ptr ES:[DI], 0E5h
                mov byte ptr [DirBuff_ValidData], 2
                retn 
pass_dcde_delete_current_dir_entry:
                stc
                retn

DCDE_Attributes: dw 0

proc_delete_current_directory_entry endp

proc_save_directory_buffer proc near
                ; 01/08/2011
                ; 14/08/2010 
                ; Output -> BX = Directory Buffer Segment 
                ; 01/05/2010 FS bugfix
                ; 18/04/2010
                ; 06/04/2010 root dir bugfix
                ; 14/03/2010
                ; INPUT ->
                ;  none
                ; OUTPUT ->
                ;  clc -> write OK...
                ;  stc -> error code in AL
                ;  stc & AL= 0Dh => CH/CL= FS/FAT type
                ;  BX = Directory Buffer Segment
                ;  AX, CX, DX will be modified
 
               mov bx, word ptr [Directory_Buffer]
	       cmp byte ptr [DirBuff_ValidData], 2
               je short loc_save_dir_buffer

loc_save_dir_buffer_retn:               
               xor ax, ax
               retn            

loc_save_dir_buffer:
                push si 
	        mov bh, byte ptr [DirBuff_Drv]
                sub bh, 'A'
                xor bl, bl
                mov si, offset Logical_DosDisks
                add si, bx
               ;18/04/2010
                mov cx, word ptr [SI][LD_FATtype]
                        ; ch = FS Type (A1h for FS)
                        ; cl = FAT Type (0 for FS)
               ;
               ; 01/08/2011
                or cl, cl
                jz short loc_save_dir_buff_stc_retn

loc_save_dir_buffer_check_cluster_no:    
  		mov ax, word ptr [DirBuff_Cluster]
                mov dx, word ptr [DirBuff_Cluster]+2
                
               ; 06/04/2010
                mov bx, ax
                or bx, dx
                jnz short loc_save_sub_dir_buffer
                mov ah, byte ptr [DirBuff_FATType]
                inc bl
               ; 01/08/2011
                cmp ah, bl
                jb short loc_save_dir_buff_inv_data_retn
                inc bl
                cmp bl, ah
                jb short loc_save_dir_buff_inv_data_retn

loc_save_root_dir_buffer:
               ; 01/08/2011
 	       ; 06/04/2010                
                mov bx, word ptr [SI][LD_BPB][RootDirEnts]
                add bx, 15
                shr bx, 1
                shr bx, 1
                shr bx, 1
 		shr bx, 1
                or  bx, bx
                jz short loc_save_dir_buff_stc_retn
                mov cx, bx 
                mov ax, word ptr [SI][LD_ROOTBegin]
		mov dx, word ptr [SI][LD_ROOTBegin]+2       
                jmp short loc_write_directory_to_disk
loc_save_dir_buff_stc_retn:
		stc
loc_save_dir_buff_inv_data_retn:
                mov al, 0Dh ; Invalid data !
                mov byte ptr [DirBuff_ValidData], 0 
loc_save_dir_buff_pop_si_retn:
               ; 18/04/2010 
                pop si
loc_save_dir_buff_return:
               ; 14/08/2010
                mov bx, word ptr [Directory_Buffer]
loc_write_directory_to_disk_err:
               ; 01/08/2011
                retn 

loc_save_sub_dir_buffer:
	  	sub ax, 2
                sbb dx, 0
                xor bh, bh
                mov bl, byte ptr [SI][LD_BPB][SecPerClust]
                mov cx, bx
                call proc_mul32
                add ax, word ptr [SI][LD_DATAbegin]
                adc dx, word ptr [SI][LD_DATAbegin]+2
loc_write_directory_to_disk:
                mov bx, word ptr [Directory_Buffer]
                push es
                mov es, bx
                xor bx, bx
                call proc_disk_write
                mov bx, es ; 14/08/2010
                pop es
                pop si
                jc short loc_write_directory_to_disk_err
loc_save_dir_buff_validate_retn:
                mov byte ptr [DirBuff_ValidData], 1
                retn

proc_save_directory_buffer endp


proc_convert_current_date_time proc near
                ; 13/06/2010 retn (bugfix)
                ; 12/06/2010
                ; converts date&time to dos dir entry format
                ; INPUT -> none
                ; OUTPUT -> DX = Date in dos dir entry format
                ;           AX = Time in dos dir entry format
 
                mov ah, 04h ; Return Current Date
                int 1Ah 

                mov al, ch ; <- century BCD
                and al, 0Fh
                mov ah, ch
                shr ah,1
                shr ah,1
                shr ah,1
                shr ah,1
                aad
                mov ch, al ; -> century 

                mov al, cl ; <- year BCD
                and al, 0Fh
                mov ah, cl
                shr ah,1
                shr ah,1
                shr ah,1
                shr ah,1
                aad
                mov cl, al ; -> year

                mov al, ch
                mov ah, 100
                mul ah
                xor ch, ch
                add ax, cx
                sub ax, 1980 ; ms-dos epoch
                mov cx, ax

                mov al, dh ; <- month in bcd
                and al, 0Fh
                mov ah, dh
                shr ah,1
                shr ah,1
                shr ah,1
                shr ah,1
                aad
                mov dh, al ; -> month

                mov al, dl ; <- day BCD
                and al, 0Fh
                mov ah, dl
                shr ah,1
                shr ah,1
                shr ah,1
                shr ah,1
                aad
                mov dl, al ; -> day

                mov al, cl ; count of years from 1980
                shl ax, 1
                shl ax, 1
                shl ax, 1
                shl ax, 1 
                or al, dh ; month of year, 1 to 12
                shl ax, 1
                shl ax, 1
                shl ax, 1
                shl ax, 1 
                shl ax, 1 
		or al, dl ; day of year, 1 to 31

                push ax ; push date

                mov ah, 02h ; Return Current Time
                int 1Ah

                mov al, ch ; <- hours BCD
                and al, 0Fh
                mov ah, ch
                shr ah,1
                shr ah,1
                shr ah,1
                shr ah,1
                aad
                mov ch, al ; -> hours

                mov al, cl ; <- minutes BCD
                and al, 0Fh
                mov ah, cl
                shr ah,1
                shr ah,1
                shr ah,1
                shr ah,1
                aad
                mov cl, al ; -> minutes

                mov al, dh ; <- seconds BCD
                and al, 0Fh
                mov ah, dh
                shr ah,1
                shr ah,1
                shr ah,1
                shr ah,1
                aad
                mov dh, al ; -> seconds

                mov al, ch ; hours
                shl ax, 1
                shl ax, 1
                shl ax, 1
                shl ax, 1
                shl ax, 1
                shl ax, 1 
                or al, cl ; minutes
                shl ax, 1
                shl ax, 1
                shl ax, 1
                shl ax, 1 
                shl ax, 1 
                shr dh, 1 ; 2 seconds
		or al, dl ; seconds

                pop dx ; pop date

                retn
                
proc_convert_current_date_time endp


proc_make_directory proc near
                ; 01/08/2011
                ; 30/04/2011
                ; 12/03/2011 Directory attributes input
                ; 25/12/2010 FS mkdir parent dir ddt bugfix 
                ; 16/10/2010 calculate FAT free space 
                ;             infinitive loop bugfix
                ; 21/08/2010 'calculate free space' modif. 
                ; 14/08/2010 modification
                ; 09/08/2010 no more files (12h) error return
                ; 24/07/2010 mkdir_ParentDir_FCluster
                ;            relocated
                ; 18/07/2010 Current dir level restoration
                ; 18/07/2010 update_cluster bugfixes
                ; 17/07/2010
                ; 10/07/2010
                ; INPUT ->
                ; DS:SI = ASCIIZ Directory name  (DS = CS)
                ; CL = Directory Attributes (12/03/2011)
                ; OUTPUT ->
                ; DX:AX = New sub dir's first cluster
                ; DS:SI = Logical Dos Drv Descr. Table Addr.
                ; stc -> error code in AL
                ; 

               ; 12/03/2011
;               test cl, 10h    (directory or volume name)
;               jz short loc_make_directory_access_denied
;               test cl, 08h
;               jnz short loc_make_directory_access_denied
                and cl, 07h
                mov byte ptr [mkdir_attrib], cl

                push si
                mov bh, byte ptr [Current_Drv]
                xor bl, bl 
                mov si, offset Logical_DOSDisks
                add si, bx
                pop bx ; 14/08/2010

               ; 10/07/2010 -> 1st writable disk check for trdos
               ; LD_DiskType = 0 for write protection (read only) 
                cmp byte ptr [SI][LD_DiskType], 1 ; 0 = Invalid
                jnb short loc_mkdir_check_file_sytem
                mov ax, 13h ; MSDOS err => Disk write-protected 
                mov dx, 0
                ; err retn: DX = 0, BX= Dir name offset
                ; DS:SI -> Dos drive description table address                
                retn

;loc_make_directory_access_denied:
               ; 12/03/2011
;               mov ax, 05h ; access denied (invalid attributes input)
;               stc
;               retn


loc_mkdir_check_file_sytem:
                cmp byte ptr [SI][LD_FATType], 1
                jnb short loc_mkdir_check_free_sectors_hw
loc_make_fs_directory:
               ; DX:AX -> Parent directory DDT Address
               ; DS:SI -> Logical DOS Drive DT
               ; BX -> Directory name offset as ASCIIZ name
               ; 25/12/2010 BugFix 
                mov ax, word ptr [Current_Dir_FCluster]
	        mov dx, word ptr [Current_Dir_FCluster]+2   
                call proc_make_fs_directory                
                retn

loc_mkdir_check_free_sectors_hw:
               ;14/08/2010
                cmp word ptr [SI][LD_FreeSectors]+2, 0
                jna short loc_mkdir_check_free_sectors_lw
                mov cx, 0FFFFh
                jmp short loc_make_fat_directory
loc_mkdir_check_free_sectors_lw:
                mov cx, word ptr [SI][LD_FreeSectors]
                or ch, ch
                jnz short loc_make_fat_directory 
                cmp cl, byte ptr [SI][LD_BPB]+SecPerClust
                jb short loc_mkdir_insufficient_disk_space
               ;

loc_make_fat_directory:
               ; 14/08/2010
                mov word ptr [mkdir_DirName_Offset], bx
                mov word ptr [mkdir_FreeSectors], cx

                mov al, byte ptr [SI][LD_BPB]+SecPerClust
                mov byte ptr [mkdir_SecPerClust], al

loc_mkdir_gffc_1:
                call proc_get_first_free_cluster
                jc short loc_mkdir_gffc_retn

               ; 15/08/2010
               ;or dx, dx
               ;jz short loc_mkdir_gffc_1_cont
               ;cmp dx, 0FFFFh
               ;jne short loc_mkdir_gffc_1_save_fcluster
               ;stc
               ;jmp short loc_mkdir_gffc_insufficient_disk_space

;loc_mkdir_gffc_1_cont: 
               ;cmp ax, 2
               ;jb short loc_mkdir_gffc_insufficient_disk_space

;loc_mkdir_gffc_1_save_fcluster:  
              ; 14/08/2010
                mov word ptr [mkdir_FFCluster], ax
                mov word ptr [mkdir_FFCluster]+2, dx

loc_mkdir_locate_ffe:
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
                jnc short loc_mkdir_set_ff_dir_entry_1
                pop si ; 30/04/2011 
                 ; DS:SI = Logical DOS Drv. Description Table Address 
                cmp ax, 2
                jne short loc_mkdir_stc_return

loc_mkdir_add_new_cluster:
               ; 14/08/2010
                cmp byte ptr [Current_FATType], 2
               ;cmp byte ptr [SI][LD_FATType], 2
                ja short loc_mkdir_add_new_cluster_check_fsc
                cmp byte ptr [Current_Dir_Level], 1
               ;cmp byte ptr [SI][LD_CDirLevel], 1
                jnb short loc_mkdir_add_new_cluster_check_fsc
               ; 09/08/2010 
                mov al, 12 ; No more files 
               ;
loc_mkdir_gffc_retn:
                retn

loc_mkdir_add_new_cluster_check_fsc:
               mov cx, word ptr [mkdir_FreeSectors]
             ; xor ah, ah 
               mov al, byte ptr [mkdir_SecPerClust]
               shl ax, 1 ; AX = 2 * AX
               cmp cx, ax
               jnb short loc_mkdir_add_new_subdir_cluster

loc_mkdir_insufficient_disk_space:
                mov dx, cx
;loc_mkdir_gffc_insufficient_disk_space:
                mov ax, 27h ; MSDOS err => insufficient disk space
                ; err retn: DX = Free sectors, BX= Dir name offset
                ; DS:SI -> Dos drive description table address
                ; cx = dx
                ;
                retn

loc_mkdir_stc_return:
                stc
                retn 

loc_mkdir_gffc_2:
              ; 14/08/2010
                call proc_get_first_free_cluster
                jc short loc_mkdir_gffc_retn

              ; 15/08/2010
               ;or dx, dx
               ;jz short loc_mkdir_gffc_2_cont
               ;cmp dx, 0FFFFh
               ;jne short loc_mkdir_gffc_2_save_fcluster
               ;stc
               ;jmp short loc_mkdir_gffc_insufficient_disk_space

;loc_mkdir_gffc_2_cont: 
               ;cmp ax, 2
               ;jb short loc_mkdir_gffc_insufficient_disk_space

;loc_mkdir_gffc_2_save_fcluster:  
              ; 14/08/2010
                mov word ptr [mkdir_FFCluster], ax
                mov word ptr [mkdir_FFCluster]+2, dx

                mov ax, word ptr [mkdir_LastDirCluster]
                mov dx, word ptr [mkdir_LastDirCluster]+2
               
                call proc_load_FAT_sub_directory 
                jc short loc_mkdir_gffc_retn

                mov es, bx
                xor di, di
              ;
loc_mkdir_set_ff_dir_entry_1:
              ; ES:DI -> Directory entry address
               mov si, word ptr [mkdir_DirName_Offset]
               mov ax, word ptr [mkdir_FFCluster]
               mov dx, word ptr [mkdir_FFCluster]+2

               mov cx, 10h ; CL= Directory attribute
                           ; CH = 0 -> File size is 0
              ; 12/03/2011
               or cl, byte ptr [mkdir_attrib] ; S, H, R  
               call proc_make_directory_entry

               pop si ; 30/04/2011

               push ds
               pop es

             ; 01/08/2011
               mov byte ptr [DirBuff_ValidData], 2
             ; 14/08/2010
               call proc_save_directory_buffer
               jnc loc_mkdir_set_ff_dir_entry_2

loc_mkdir_return:
               retn

loc_mkdir_add_new_subdir_cluster:
               ; 14/08/2010 modification
               ;mov ax, word ptr [DirBuff_Cluster]
               ;mov dx, word ptr [DirBuff_Cluster]+2 
               ;mov word ptr [mkdir_LastDirCluster], ax                
               ;mov word ptr [mkdir_LastDirCluster]+2, dx

                push word ptr [DirBuff_Cluster]
                pop word ptr [mkdir_LastDirCluster]                
                push word ptr [DirBuff_Cluster]+2
                pop word ptr [mkdir_LastDirCluster]+2       

                mov ax, word ptr [mkdir_FFCluster]
                mov dx, word ptr [mkdir_FFCluster]+2
                call proc_load_FAT_sub_directory 
                jc short loc_mkdir_return
pass_mkdir_add_new_subdir_cluster:
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
                mov byte ptr [DirBuff_ValidData], 2
                call proc_save_directory_buffer
                jc short loc_mkdir_return
loc_mkdir_save_added_cluster:
               ; 14/08/2010
                mov ax, word ptr [mkdir_LastDirCluster]
                mov dx, word ptr [mkdir_LastDirCluster]+2
                mov cx, word ptr [mkdir_FFCluster]
                mov bx, word ptr [mkdir_FFCluster]+2
                call proc_update_cluster
                ; 18/07/2010
                jnc short loc_mkdir_save_fat_buffer_0
                or ax, ax ;AX = 0 -> cluster value is 0 or eoc
                jnz short loc_mkdir_save_fat_buffer_stc_retn

loc_mkdir_save_fat_buffer_0:
               ;
                mov ax, word ptr [mkdir_FFCluster]
                mov dx, word ptr [mkdir_FFCluster]+2

               ; 14/08/2010
                mov word ptr [mkdir_LastDirCluster], ax
                mov word ptr [mkdir_LastDirCluster]+2, dx
               ;
                xor cx, cx
                dec cx
                mov bx, cx ; FFFFh
                call proc_update_cluster
                ; 18/07/2010
                jnc short loc_mkdir_save_fat_buffer_1
                or ax, ax
                jz short loc_mkdir_save_fat_buffer_1
loc_mkdir_save_fat_buffer_stc_retn:
                stc
loc_mkdir_save_fat_buffer_retn:
                retn

loc_mkdir_save_fat_buffer_1:
               ; Byte ptr [FAT_BUffValidData] = 2 
                call proc_save_fat_buffer
                jc short loc_mkdir_save_fat_buffer_retn

                mov byte ptr [mkdir_add_new_cluster], 1

                jmp loc_mkdir_upd_parent_dir_lmdt

loc_mkdir_update_sub_dir_cluster:
               mov ax, word ptr [mkdir_FFCluster]
               mov dx, word ptr [mkdir_FFCluster]+2
               xor cx, cx
               dec cx ; FFFFh
              ; 30/01/2011
              ; DS:SI = Logical DOS drv Desc. Table Address  
              ;mov bh, byte ptr [Current_Drv]
              ;xor bl, bl
              ;mov si, offset Logical_DosDisks
              ;add si, bx 
               mov bx, cx ; FFFFh
               call proc_update_cluster
              ; 18/07/2010
               jnc short loc_mkdir_save_fat_buffer_2
               or ax, ax ;AX = 0 -> cluster value is 0 or eoc
               jz short loc_mkdir_save_fat_buffer_2
               stc
               retn

loc_mkdir_set_ff_dir_entry_2:
               ; 30/04/2011
               ; DS:SI = Logical DOS Drv Desc. Table Address  
               ; 14/08/2010 modification

               ;mov si, offset Logical_DosDisks
               ;mov bh, byte ptr [Current_Drv]
               ;xor bl, bl 
               ;add si, bx

               ; 17/07/2010
                mov ax, word ptr [mkdir_FFCluster]
                mov dx, word ptr [mkdir_FFCluster]+2
               ; DS:SI = DOS Drive Description Table
               ; Load disk sectors as a directory cluster
                call proc_load_FAT_sub_directory 
                jc short retn_make_fat_directory

                mov es, bx
                mov di, 64
                
                mov ax, 256 ; word
                mul cx ; cx =  directory buffer sector count
                sub ax, 32 ; 64 bytes
                mov cx, ax
                xor ax, ax ; 0
                rep stosw  ; AX to ES:DI as CX times (DI= DI+2) 

                xor di, di ; 0

                push si ;30/04/2011
 
                mov si, offset mkdir_Name
                mov word ptr [SI], 2Eh ; db '.', '0'

                mov ax, word ptr [mkdir_FFCluster]
                mov dx, word ptr [mkdir_FFCluster]+2
                mov cx, 10h ; CL= Directory attribute
                           ; CH = 0 -> File size is 0
                call proc_make_directory_entry

                mov di, 32
                                
                mov ax, word ptr [Current_Dir_FCluster] ; parent dir
                mov dx, word ptr [Current_Dir_FCluster]+2 ; parent dir 

                mov word ptr [SI]+1, 2Eh ; db '.', '0'

                ;mov cx, 10h
                call proc_make_directory_entry

                pop si ; 30/04/2011

                push ds
                pop es

               ; 01/08/2011
                mov byte ptr [DirBuff_ValidData], 2
                call proc_save_directory_buffer
               ; 14/08/2010
                jnc short loc_mkdir_update_sub_dir_cluster
 
retn_make_fat_directory:
                retn

loc_mkdir_save_fat_buffer_2:
               ; Byte ptr [FAT_BuffValidData] = 2 
                call proc_save_fat_buffer
                jc short retn_make_fat_directory

                mov byte ptr [mkdir_add_new_cluster], 0

loc_mkdir_upd_parent_dir_lmdt:
              ; 16/10/2010
                call proc_update_parent_dir_lmdt

loc_mkdir_calculate_FAT_freespace:
              ; 16/10/2010
                mov byte ptr [CFS_error], 0
loc_mkdir_calculate_FAT_freespace_next:
              ; 21/08/2010 
                xor dx, dx
                mov ax, 1 ; Only one Cluster
                mov bx, 0FF02h ; BH= FF -> DS:SI -> Dos drv desc. table
                               ; BL= 2 -> subtract  (1 cluster)
                call proc_calculate_FAT_freespace
                and cx, cx ; cx = 0 -> valid free sector count
                jz short pass_mkdir_recalculate_FAT_freespace
                cmp byte ptr [CFS_error], 0
                ja short loc_mkdir_calculate_FAT_freespace_err

loc_mkdir_recalculate_FAT_freespace:
              ; 16/10/2010 
               inc byte ptr [CFS_error]
              ; 21/08/2010
               mov bx, 0FF00h
               call proc_calculate_FAT_freespace
              ; 16/10/2010
               and cx, cx
               jz short loc_mkdir_calculate_FAT_freespace_next
              
loc_mkdir_calculate_FAT_freespace_err:
              ; 16/10/2010
pass_mkdir_recalculate_FAT_freespace:
              ; 21/08/2010
              ; 14/08/2010
               cmp byte ptr [mkdir_add_new_cluster], 0
               ja loc_mkdir_gffc_2

loc_mkdir_retn_new_dir_cluster:
               mov ax, word ptr [mkdir_FFCluster]
               mov dx, word ptr [mkdir_FFCluster]+2
loc_mkdir_retn:
               retn
; 12/03/2011
mkdir_attrib: dw 0 
; 10/07/2010
mkdir_FFCluster: dd 0
mkdir_DirName_Offset: dw 0
; 17/07/2010
mkdir_add_new_cluster: db 0
mkdir_Name: db 13 dup (0)
; 14/08/2010
mkdir_SecPerClust: db 0
mkdir_FreeSectors: dw 0
mkdir_LastDirCluster: dd 0

proc_make_directory endp

proc_make_directory_entry proc near
               ; 09/08/2010 File Size in dword ptr DX:[BX]
               ; 17/07/2010
               ; INPUT ->
               ;  ES:DI = Directory Entry Address (32 bytes)
               ;  DS:SI = Dot File Name Location
               ;  DX:AX = First Cluster
               ;  File Size = 0 (Must be set later)
               ;  CL= Attributes
               ;  CH = 0 (File size = 0) 
               ;  (If CH>0, File size is in
               ;  dword ptr DS:[BX]) 
 
               ; OUTPUT -> 
               ;  ES:DI = Directory Entry Address (32 bytes)
               ;  DS:SI = Dot File Name Location (Capitalized)
               ;  If CH input = 0, File Size = 0
               ;  Otherwise file size is as dword ptr DS:[BX]
               ;  DX= Date, AX= Time in DOS Dir Entry format
               ;  BX = same
               ;  CX = same

               push cx

               mov byte ptr ES:[DI]+11, cl ; Attributes
               mov word ptr ES:[DI]+26, ax ; FClusterLw, 26
               mov word ptr ES:[DI]+20, dx ; FClusterHw, 20
               xor ax, ax 
               mov word ptr ES:[DI]+12, ax ; NTReserved, 12
  			                   ; CrtTimeTenth, 13
               xor dx, dx 
               or ch, ch
               jz short loc_make_direntry_set_filesize
               mov ax, word ptr [BX]
               mov dx, word ptr [BX]+2
                          
loc_make_direntry_set_filesize:
               mov word ptr ES:[DI]+28, ax ; FileSizeLW, 28
               mov word ptr ES:[DI]+30, dx ; FileSizeHW, 30

               call proc_convert_file_name
                ;ES:DI = Dir Entry Format File Name Location
                ;DS:SI = Dot File Name Location (capitalized)

               call proc_convert_current_date_time
                ; OUTPUT -> DX = Date in dos dir entry format
                ;           AX = Time in dos dir entry format
               mov word ptr ES:[DI]+14, ax ; CrtTime, 14
               mov word ptr ES:[DI]+16, dx ; CrtDate, 16
               mov word ptr ES:[DI]+18, dx ; LastAccDate, 18
               mov word ptr ES:[DI]+22, ax ; WrtTime, 14
               mov word ptr ES:[DI]+24, dx ; WrtDate, 16

               pop cx

               retn

proc_make_directory_entry endp

proc_update_parent_dir_lmdt proc near
               ; 01/08/2011
               ; 15/05/2011
               ; 30/04/2011 push si, pop si
               ; 16/10/2010 moved here from 
               ; proc_delete_file in FILE.ASM
               ; 
               ; INPUT -> none
               ; OUTPUT -> 
               ;  ES, AX, BX, CX, DX, DI will be changed

               ; 15/05/2011 FS Check
                mov ah, byte ptr [Current_Dir_Level]
                mov al, byte ptr [Current_FATType]
                cmp al, 1
                jb short loc_UPDLMDT_proc_retn
                  
loc_update_parent_dir_lm_date_time:
              ; 15/05/2011
              ; 13/06/2010 BugFix 
              ; 12/06/2010
              ; mov ah,  byte ptr [Current_Dir_Level]
                or ah, ah
                jz short loc_UPDLMDT_proc_retn
    
                push si ; 30/04/2011
              ; 18/07/2010
                mov byte ptr [UPDLMDT_CDirLevel], ah
                push word ptr [Current_Dir_FCluster]
                pop word ptr [UPDLMDT_CDirFCluster]
                push word ptr [Current_Dir_FCluster]+2
                pop word ptr [UPDLMDT_CDirFCluster]+2
               ; 12/06/2010 
                dec ah ; 15/05/2011 al -> ah
                push ds
                pop es
                mov cx, 12
                mov si, offset Path_Array

               ; 13/06/2010
                mov byte ptr [Current_Dir_Level], ah
                or ah, ah
                jnz short loc_update_parent_dir_lmdt_load_sub_dir_1
                cmp byte ptr [Current_FATType], 2
                ja short loc_update_parent_dir_lmdt_load_sub_dir_2
                xor al, al
                xor dx, dx
                jmp short loc_update_parent_dir_lmdt_load_sub_dir_3

loc_UPDLMDT_proc_retn:
              ; 16/10/2010 
                retn
         
loc_update_parent_dir_lmdt_load_sub_dir_1:
              ; 15/05/2011
              ; 13/06/2010
                mov al, 16
                mul ah 
                add si, ax
loc_update_parent_dir_lmdt_load_sub_dir_2:                
                mov ax, word ptr [SI]+12 ; Parent Dir First cluster LW
                mov dx, word ptr [SI]+14 ; Parent Dir First cluster HW
loc_update_parent_dir_lmdt_load_sub_dir_3:
               ; 13/06/2010
                mov word ptr [Current_Dir_FCluster], ax
                mov word ptr [Current_Dir_FCluster]+2, dx               

                add si, 16
                mov di, offset Dir_File_Name                
                rep movsb

                mov si, offset Logical_DosDisks
                mov bh, byte ptr [Current_Drv]
                xor bl, bl 
                add si, bx
                call proc_reload_current_directory
                jc short loc_update_parent_dir_lmdt_restore_cdirlevel

loc_update_parent_dir_lmdt_locate_dir: 
                mov si, offset Dir_File_Name        
                xor cx, cx
                mov ax, 0810h ; Only directories
                call proc_locate_current_dir_file
               ; ES:DI = DirBuff Directory Entry Address
                jc short loc_update_parent_dir_lmdt_restore_cdirlevel

                call proc_convert_current_date_time
                mov word ptr ES:[DI]+18, dx ; Last Access Date
                mov word ptr ES:[DI]+24, dx ; Last Write Date
                mov word ptr ES:[DI]+22, ax ; Last Write Time
               ; 01/08/2011 
                mov byte ptr [DirBuff_ValidData], 2
		call proc_save_directory_buffer
                jc short loc_update_parent_dir_lmdt_restore_cdirlevel
                xor al, al 
loc_update_parent_dir_lmdt_restore_cdirlevel:
               ; 18/07/2010 current directory level restoration
                mov ah, byte ptr [UPDLMDT_CDirLevel]
                mov byte ptr [Current_Dir_Level], ah
                push word ptr [UPDLMDT_CDirFCluster]
                pop word ptr [Current_Dir_FCluster]
                push word ptr [UPDLMDT_CDirFCluster]+2
                pop word ptr [Current_Dir_FCluster]+2

              ; 16/10/2010 
               ; push ds
               ; pop es

                pop si ; 30/04/2011

                retn

; 16/10/2010 (delfile-> UPDLMDT)
; 18/07/2010
UPDLMDT_CDirLevel: db 0
UPDLMDT_CDirFCluster: dd 0
 
proc_update_parent_dir_lmdt endp

proc_rename_directory_entry proc near
               ; 01/08/2011
               ; 20/11/2010 
               ; 19/11/2010
               ; INPUT -> (Current Directory)
               ;  CX = Directory Entry Number
               ;  DX:AX = First Cluster number of file or directory
               ;  BL = Longname Length (dir entry count)
               ;  BH = 0
               ;  DS:SI = New file (or directory) name (no path).
               ;          (ASCIZZ string)  
               ; OUTPUT -> 
               ;  clc (cf=0) -> successfull
               ;  stc (cf=1) -> error code in AX
               ;  ES, AX, BX, CX, DX, SI, DI will be changed

                cmp byte ptr CS:[Current_FATType], 0
                ja short loc_rename_directory_entry

                call proc_rename_fs_file_or_directory
                retn 
                
loc_rename_directory_entry:
                push ds
                pop es
                push cs
                pop ds

                mov byte ptr [DelFile_LNEL], bl
                mov word ptr [DelFile_EntryCounter], cx
                mov word ptr [DelFile_FCluster], ax
                mov word ptr [DelFile_FCluster]+2, dx

                mov ax, cx
                push si
                push es
                call proc_locate_current_dir_entry
                jnc short loc_rename_direntry_check_fcluster
loc_rename_direntry_pop_retn:
                pop dx
                pop dx
                retn

loc_rename_direntry_pop_invd_retn:
                mov dx, ax
                pop ax
                pop ax 
                stc
loc_rename_direntry_invd_retn:
                mov ax, 0Dh
loc_rename_retn:
                retn 

loc_rename_direntry_check_fcluster:
                mov ax, word ptr ES:[DI]+26
                mov dx, word ptr ES:[DI]+20
                cmp ax, word ptr [DelFile_FCluster]
                jne short loc_rename_direntry_pop_invd_retn
                cmp dx, word ptr [DelFile_FCluster]+2
                jne short loc_rename_direntry_pop_invd_retn
               ;mov bx, es
               ;mov dx, di
               ;push cs
               ;pop es
               ;mov di, offset Dir_File_name
                pop ds
                pop si
               ; NOTE: If file/dir name is more than 11 bytes
               ; without a dot, attributes (DI+11) byte will be overwritten !
               ;  (Dot file name input must be proper 
               ;  for 11 byte dir entry type file name output) 
                call proc_convert_file_name
                push cs
                pop ds
               ;mov di, dx  
               ;mov es, bx
               ;mov si, offset Dir_File_name 
               ;mov cx, 11
               ;rep movsb  
                mov byte ptr [DirBuff_ValidData], 2
                call proc_save_directory_buffer
                jc short loc_rename_retn

loc_rename_direntry_del_ln:
                xor dh, dh
                mov dl, byte ptr [DelFile_LNEL]
                or dl, dl
                jz short loc_rename_direntry_update_parent_dir_lm_date

                mov ax, word ptr [DelFile_EntryCounter]
                sub ax, dx
                jc short loc_rename_direntry_invd_retn

loc_rename_direntry_del_ln_continue: 
               ; AX = Directory Entry Number of the long name last entry
                call proc_delete_longname

loc_rename_direntry_update_parent_dir_lm_date:
                call proc_update_parent_dir_lmdt
               
                xor ax, ax 
               
                retn

proc_rename_directory_entry endp

proc_set_working_path proc near
                ; 08/02/2011 
                ; 05/02/2011
                ; 27/01/2011   
                ; Set/Changes current drive, directory and file
                ; depending on command tail
                ; (procedure is derivated from CMD_INTR.ASM 
                ; file or dir locating code of internal commands)
                ; (This procedure is prepared for INT 21H file/dir 
                ; functions and also to get compact code for 
                ; internal mainprog -command interpreter- commands)
                ; 
                ; INPUT: DS:SI -> Command tail (ASCIIZ string)
                ; AL= 0 -> any, AL > 0 -> file name is forced
                ; AH= CD -> Change directory permanently 
                ; AH <> CD -> Change directory as temporary    
                ; 
                ; OUTPUT: ES=DS, FindFile structure has been set
                ;        RUN_CDRV points previous current drive  
                ;        DS:SI = FindFile structure address
                ;        (DS=CS)       
                ;        AX, BX, CX, DX, DI will be changed
                ;   cf = 1 -> Error code in AX (AL)
                ;        stc & AX = 0 -> Bad command or path name 
                ;

                push cs
                pop es 

                mov word ptr CS:[SWP_Mode], ax
                mov al, byte ptr CS:[Current_Drv] ; 05/02/2011
                xor ah, ah
                mov word ptr CS:[SWP_DRV], ax  ; 05/02/2011
                
loc_swp_fchar:
                mov al, byte ptr [SI]
                cmp al, 20h
                ja short loc_swp_parse_path_name
                je short loc_swp_fchar_next

loc_swp_xor_retn:
                xor ax, ax
                stc

loc_swp_push_pop_retn:
                push cs
                pop ds
loc_swp_retn:
                mov si, offset FindFile_Drv
                retn 

loc_swp_fchar_next:
                inc si
                jmp short loc_swp_fchar  

loc_swp_parse_path_name:
                mov di, offset FindFile_Drv
                call proc_parse_pathname
                jc short loc_swp_push_pop_retn

loc_swp_checkfile_name:
                push cs
                pop ds
                cmp byte ptr [SWP_Mode], 0
                jna short loc_swp_drv
                cmp byte ptr [FindFile_Name], 20h
                jna short loc_swp_xor_retn

loc_swp_drv:
                mov dh, byte ptr [Current_Drv]
               ; 05/02/2011
               ;mov byte ptr [RUN_CDRV], dh

                mov dl, byte ptr [FindFile_Drv]
               ;cmp dl, dh
                cmp dl, byte ptr [Current_Drv]
                je short loc_swp_change_directory

                inc byte ptr [SWP_DRV_chg]
                call proc_change_current_drive
                jc short loc_swp_retn

loc_swp_change_directory:
                cmp byte ptr [FindFile_Directory], 21h
                cmc
                jnc short loc_swp_retn

                inc byte ptr [SWP_DRV_chg]
                inc byte ptr [Restore_CDIR]
                mov si, offset FindFile_Directory
                mov ah, byte ptr [SWP_Mode]+1 
                call proc_change_current_directory
                jc  short loc_swp_retn

loc_swp_change_prompt_dir_string:
                call proc_change_prompt_dir_string
                mov ax, 0
                jmp short loc_swp_retn 

SWP_Mode: dw 0
;05/02/2011
SWP_DRV: db 0
;08/02/2011
SWP_DRV_chg: db 0

proc_set_working_path endp

proc_reset_working_path proc near
                ; 08/02/2011         
                ; 06/02/2011
                ; 05/02/2011
                ; Restores current drive and directory 
                ; 
                ; INPUT: none
                ; OUTPUT: DL = SWP_DRV, AX = 0 -> OK
                ;         DS= CS = ES   
                ;    BX, CX, DX, SI, DI will be changed
                ;    AX = 0 -> DS:SI = Logical Dos Drv Desc. Table
  
                mov dx, cs
                mov ds, dx
                mov es, dx

                xor ax, ax
                dec ax 

                mov dx, word ptr [SWP_DRV]
                or dh, dh
                jz short loc_rwp_return

                cmp dl, byte ptr [Current_Drv]
                je short loc_rwp_restore_cdir
loc_rwp_restore_cdrv:
                call proc_change_current_drive 
                jmp short loc_rwp_restore_ok
loc_rwp_restore_cdir:
                mov bh, dl
                xor bl, bl
                mov si, offset Logical_DOSDisks
                add si, bx

                call proc_restore_current_directory

loc_rwp_restore_ok:
                mov dx, word ptr [SWP_DRV]
                xor ax, ax  
                mov word ptr [SWP_DRV_chg], ax
loc_rwp_return:
                retn

proc_reset_working_path endp


proc_return_current_directory_string proc near
               ; 27/03/2011
               ; INPUT:
               ;  DS:SI = Path Array Address 
               ;  ES:DI = Current Directory String Buffer
               ;  AH = Current Directory Level
               ;
               ; 09/10/2009 (proc_change_prompt_dir_string)
               ; OUTPUT => AX, BX, SI will be changed
               ;           DI will be same with input
               ;           CX = Current Directory String Length 
               ;

                push    di
                cmp     ah,0
                jna     short pass_write_path
               ;mov     byte ptr [DirLevel_Count], ah
                add     si, 16
                mov     bx, si
loc_write_path:
                mov     cx, 8
path_write_dirname1:
                lodsb
                cmp     al, 20h
                jna     short pass_write_dirname1
                mov     byte ptr ES:[DI], al
                inc     di
                cmp     di, offset End_Of_Current_Dir_Str
                jnb     short pass_write_path
                loop    path_write_dirname1
                cmp     byte ptr [SI], 20h
                jna     short pass_write_dirname2
                jmp     short loc_put_dot_cont_ext
pass_write_dirname1:
                mov     si, bx
                add     si, 8
                cmp     byte ptr [SI], 20h
                jna     short pass_write_dirname2
loc_put_dot_cont_ext:
                mov     byte ptr ES:[DI],"."
                mov     cx, 3
loc_check_dir_name_ext:
                lodsb
                inc     di
                cmp     al, 20h
                jna     short pass_write_dirname2
                mov     byte ptr ES:[DI], al
                cmp     di, offset End_Of_Current_Dir_Str
                jnb     short pass_write_path
                loop    loc_check_dir_name_ext
                inc     di
pass_write_dirname2:
               ; dec     byte ptr [DirLevel_Count]
                dec     ah
                jz      short pass_write_path
                add     bx, 16
                mov     si, bx
                mov     byte ptr ES:[DI],"/"
                inc     di
                jmp     short loc_write_path
pass_write_path:
                mov     byte ptr ES:[DI],0
                inc     di
                mov     cx, di
                pop     di
                sub     cx, di
                ; CX = Current Directory String Length
                retn

;DirLevel_Count: db 0

proc_return_current_directory_string endp


proc_get_current_directory proc near
               ; 27/03/2011
               ;
               ; INPUT-> DS:SI = Current Directory Buffer
               ;         DL = TRDOS Logical Dos Drive Number + 1
               ;              (0= Default/Current Drive)
               ;           
               ;   Note: Required dir buffer length may be <= 92 bytes
               ;         for TRDOS (7*12 name chars + 7 slash + 0)
               ; OUTPUT ->  DS:SI = Current Directory Buffer
               ;            ES is same with input
               ;            AX, BX, CX, DX, DI will be changed
               ;         CX/CL = Current Directory String Length
               ;         DL = Drive Number (1 based)
               ;             (If input is 0, output is current drv number) 
               ;         DH= same with input 
               ;   clc  (cf=0) -> AL = 0
               ;   stc  (cf=1) -> error code in AL 
              
loc_get_current_drive_0:
               cmp dl, 0
               ja short loc_get_current_drive_1
               mov dl, byte ptr CS:[Current_Drv]
               jmp short loc_get_current_drive_3
loc_get_current_drive_1:
               cmp dl, 13
               jna short loc_get_current_drive_2
               mov ax, 0Fh ; Invalid drive
               cmc ; stc
               retn

loc_get_current_drive_not_ready_retn:
               pop si
               mov ax, 15h ; Drive not ready
               retn  
 
loc_get_current_drive_2:
               dec dl ; Zero based (TRDOS Logical Dos Drive Number)
loc_get_current_drive_3:
               mov ah, dl
               xor al, al
               push si
               mov si, offset Logical_DosDisks
               add si, ax
               mov al, byte ptr CS:[SI]+LD_Name 
               cmp al, 'A'
               jb short loc_get_current_drive_not_ready_retn

loc_get_current_drive_4:
               push ds  ; Current dir buffer segment  
               push es 
               push cs
               pop ds             
               mov ah, byte ptr [SI][LD_CDirLevel]
	       or ah, ah
               jnz short loc_get_current_drive_6
loc_get_current_drive_5:
               pop es
               pop ds  
               pop si
              ;xor ah, ah ; mov ah, 0
               mov byte ptr [SI], ah
               xor cx, cx
               jmp short loc_get_current_drive_9

loc_get_current_drive_6:
               push cs
               pop es 
               mov di, offset PATH_Array
               push di
               add si, LD_CurrentDirectory
               mov cx, 64
               rep movsw
               pop si ; Path Array Address
               pop bx ; pushed es  
               pop es ; pushed ds (current dir buffer segment)
               pop di ; pushed si (current dir buffer offset) 
               push bx
loc_get_current_drive_7:
               push di  
               call proc_return_current_directory_string
              ;cmp dl, byte ptr [Current_Drv]
              ;je short loc_get_current_drive_8

              ;mov bh, byte ptr [Current_Drv]
              ;xor bl, bl
              ;mov si, offset Logical_DOSDisks
              ;add si, bx
              ;push es
              ;push cs
              ;pop es
              ;mov di, offset PATH_Array
              ;push di
              ;add si, LD_CurrentDirectory
              ;mov cx, 64
              ;rep movsw  
              ;pop es

loc_get_current_drive_8:
               pop si
               push es
               pop ds
               pop es
loc_get_current_drive_9:
               xor al, al
               inc dl ; 1 based drive number  
               retn

proc_get_current_directory endp

proc_delete_directory_entry proc near
                ; 01/08/2011
                ; 11/04/2011  
                ; 10/04/2011 
                ; this procedure is separated and moved here from
                ; proc_delete_file procedure (16/10/2010) in FILE.ASM
                ; for 'rename' compatibility 
                ;
                ; INPUT ->
                ;       DS:SI = Logical dos drv desc. table address 
                ;       ES:DI = Directory Buffer Entry Offset
                ;          CX = Dir Buff Entry Counter
                ;          BL = Longname Entry Length
                ;          BH = Logical DOS Drv Number 
                ; OUTPUT ->
                ;       DS:SI = Logical dos drv desc. table address
                ;       DX:AX = First cluster to be truncated/unlinked
                ;       stc -> Error code in AL
                ;       (ES), DI, BX, CX will be changed
                ;       clc & BH <> 0 -> LMDT write error  (BH = 1)
                ;       clc & BL <> 0 -> Long name delete error (BL = FFh) 

                mov byte ptr [DelFile_LNEL], bl
                mov word ptr [DelFile_EntryCounter], cx
               
               ; 03/04/2010
  	       ; ES:DI -> Directory buffer entry offset/address 
                
                mov ax, word ptr ES:[DI]+26 ; First Cluster Low Word
                mov dx, word ptr ES:[DI]+20 ; First Cluster High Word

                mov word ptr [DelFile_FCluster], ax
                mov word ptr [DelFile_FCluster]+2, dx

loc_del_short_name:
                ; 28/03/2010
              	mov byte ptr ES:[DI], 0E5h  ; Deleted sign
		; 01/08/2011
                mov byte ptr [DirBuff_ValidData], 2

	       ; 10/04/2011
               ; 18/04/2010 
		call proc_save_directory_buffer
                jc short loc_delete_direntry_err_return
 
loc_del_long_name:
               ; 03/04/2010 
                xor dh, dh
                mov dl, byte ptr [DelFile_LNEL]
                or dl, dl
               ; 16/10/2010
               ; 12/06/2010
                jz short loc_del_dir_entry_update_parent_dir_lm_date

               ; 11/04/2011
                mov byte ptr [DelFile_LNEL], dh ; 0              
               ;   
                mov ax, word ptr [DelFile_EntryCounter]
                sub ax, dx
               ;jnc short loc_del_long_name_continue
                jc short loc_del_dir_entry_update_parent_dir_lm_date   

;loc_del_direntry_inv_data_return:
;                mov ax, 0Dh ; Invalid data
;                retn

loc_del_long_name_continue: 
               ; AX = Directory Entry Number of the long name last entry
                call proc_delete_longname
               ;jc short loc_delete_direntry_err_return

loc_del_dir_entry_update_parent_dir_lm_date:
              ; 11/04/2011  
                sbb byte ptr [DelFile_LNEL], 0 
              ; 16/10/2010
                call proc_update_parent_dir_lmdt
              ; 11/04/2011   
                mov bh, 0
                adc bh, 0
                mov bl, byte ptr [DelFile_LNEL] 
loc_delete_direntry_return:
                mov ax, word ptr [DelFile_FCluster]
                mov dx, word ptr [DelFile_FCluster]+2
loc_delete_direntry_err_return:
                retn

;28/03/2010
;DelFile_DirEntryAddr: dw 0
DelFile_FCluster: dd 0
; 28/02/2010
DelFile_EntryCounter: dw 0
DelFile_LNEL: db 0

proc_delete_directory_entry endp


Dir_File_Name: db 13 dup(0)

File_Name:
		db  12 dup(20h)
		db  20h
Dir_Or_FileSize:
		db  10 dup(20h)
		db  20h
File_Attribute:
		db  4 dup(20h)
		db  20h
File_Day:
		db  2 dup('0')
		db  '/'
File_Month:
		db  2 dup('0')
		db  '/'
File_Year:
		db  4 dup('0')
		db  20h
File_Hour:
		db  2 dup('0')
		db  ':'
File_Minute:
		db  2 dup('0')
		db  0

Type_Dir:       db '<DIR>     ' 

Dir_Drive_Str:
                db "TR-DOS Drive "
Dir_Drive_Name:
                db "0:"
                db  0Dh, 0Ah
Vol_Str_Header:
                db "Volume Name: "
Vol_Name:       db 64 dup(0)
                db 0
Vol_Serial_Header:
                db 0Dh, 0Ah
                db "Volume Serial No: "
Vol_Serial2:    db "0000"
                db "-"
Vol_Serial1:    db "0000"
                db 0Dh, 0Ah
Dir_Str_Header:
                db "Directory: "
Dir_Str_Root:   db "/"
Dir_Str:        db 64 dup (0)
                dd 0
                db 0

PATH_Array:     db 128 dup(0)

LFN_Checksum:   db 0
LongFileName:   db 132 dup(0)
LongNameFound:  db 0
; 28/02/2010
LongName_EntryLength: db 0
