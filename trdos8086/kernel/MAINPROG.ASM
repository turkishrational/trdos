;*****************************************************************************
; MAINPROG.ASM  [ TRDOS KERNEL - COMMAND EXECUTER SECTION - MAIN PROGRAM ]
; (c) 2004-2011  Erdogan TAN  [ 17/01/2004 ]  Last Update: 09/11/2011
; ! Under Development !

proc_change_current_drive proc near
               ; 14/08/2011
               ; 01/08/2011  
               ; 08/08/2010
               ; 25/01/2010
               ; 
               ; 29/11/2009, 12/10/2009, 09/10/2009,
               ; 03/10/2009, 28/09/2009, 27/09/2009,
               ; 21/09/2009, 12/09/2009, 09/09/2009
               ; 
               ; INPUT : dl= logical dos drive number
               ; OUTPUT: CF = 1 -> Not succesfull
               ;         AX = Error code, AX = 0 -> successfull

               mov bh, dl
               xor bl, bl 
;              cmp dl, 1
;              jna short loc_ccdrv_initial_media_change_check

;loc_cddrv_check_last_dos_disk_no:
;              cmp byte ptr [Last_Dos_DiskNo], bh
;              jb short loc_return_change_drive_not_ready

loc_ccdrv_initial_media_change_check:
 	       mov si, offset Logical_DOSdisks
               add si, bx
              ; 01/08/2011
loc_ccdrv_dos_drive_name_check:
               cmp dl, 2
               jb short pass_ccdrv_dos_drive_name_check
              ; 14/08/2011  
               mov al, byte ptr [SI][LD_Name]
               sub al, "A"
               cmp al, dl
               jne short loc_ccdrv_drive_not_ready_err  
              ; 
pass_ccdrv_dos_drive_name_check:
               mov ah, byte ptr [SI][LD_MediaChanged]
               cmp ah, 6              ; VOLUME NAME CHECK/MOVE SIGN
               je short loc_ccdrv_get_FAT_volume_name_0
      
  	       cmp dl, 1
               jna short loc_gmcs_init_drv_fd

               or ah, ah
               jz short loc_change_current_drv3

loc_ccdrv_reset_ld_mediachanged:  
               jmp short loc_change_current_drv2

loc_ccdrv_drive_not_ready_err:
              ;01/08/2011 
               mov ax, 15h ; Drive not ready
loc_change_current_drive_stc_retn:
               stc
loc_change_current_drive_retn:
               retn

loc_gmcs_init_drv_fd:
               or ah, ah 
                ; ah = 1 is initialization sign (invalid_fd_parameter)
               jnz short loc_ccdrv_call_fd_init
               call proc_get_media_change_status
               jc short loc_ccdrv_drive_not_ready_err
               and ah, ah
               jz short loc_change_current_drv3
               xor ah, 6
               jnz short loc_ccdrv_drive_not_ready_err

loc_ccdrv_call_fd_init_check_vol_id:
              ; 08/08/2010
               call get_volume_serial_number
               jc short loc_ccdrv_call_fd_init
               mov word ptr [Current_VolSerial1], ax
               mov word ptr [Current_VolSerial2], dx
               mov dl, bh
               call floppy_drv_init
               jc short loc_change_current_drive_retn
               cmp ax, word ptr [Current_VolSerial1]
               jne short loc_reset_drv_fd_current_dir
               cmp dx, word ptr [Current_VolSerial2]
               je short loc_change_current_drv1

               jmp short loc_reset_drv_fd_current_dir

loc_ccdrv_call_fd_init:
               call floppy_drv_init
               jc short loc_change_current_drive_retn

loc_reset_drv_fd_current_dir:
               ; 03/10/2009
               xor ax, ax              
               mov byte ptr [SI][LD_CDirLevel], al
               mov di, si
               add di, LD_CurrentDirectory
               mov cx, 64
               rep stosw   
               xor bl, bl 
loc_ccdrv_get_FAT_volume_name_0:
               mov al, byte ptr [SI][LD_FATType]
 	       or al, al
               jz short loc_change_current_drv2

 	       push si 
               cmp al, 2
               ja short loc_ccdrv_get_FAT32_vol_name
             
loc_ccdrv_get_FAT2_16_vol_name:
               add si, LD_BPB + VolumeLabel
               jmp short loc_ccdrv_get_FAT_volume_name_1

loc_ccdrv_get_FAT32_vol_name:
	       add si, LD_BPB + FAT32_VolLab
loc_ccdrv_get_FAT_volume_name_1:
               push bx
               push si
               call proc_get_FAT_volume_name
               pop di
               pop bx
               jc short pass_ccdrv_move_volume_name
               and al, al
               jz short pass_ccdrv_move_volume_name

loc_ccdrv_move_FAT_volume_name:
              ;push cs
              ;pop es
               mov cx, 11
               rep movsb
               push cs
               pop ds
pass_ccdrv_move_volume_name:
	      ;push ds
              ;pop es
               pop si

loc_change_current_drv1:
               xor bl, bl
loc_change_current_drv2:
               mov byte ptr [SI][LD_MediaChanged], bl
loc_change_current_drv3:
               mov byte ptr [Current_Drv], bh

               ; 25/01/2010

               ; 12/10/2009 internal code 
               ; -> 29/11/2009 separate procedure
               call proc_restore_current_directory

               retn
 
proc_change_current_drive endp

proc_restore_current_directory proc near
               ; 25/01/2010
               ; 01/01/2010 -> Singlix FS functionality
               ; 29/11/2009 -> separate procedure
               ; for restoring previous cdir
               ; 12/10/2009 -> proc_change_current_drive
               ;
               ; INPUT-> DS:SI = DOs Drive Description Table
               ;         ES=DS
               ; OUTPUT ->  DS:SI = DOS Drive Description Table
               ;            DI = Offset Current_Dir_Drv 
               ;              
              
               ; 01/01/2010
               mov al, byte ptr [SI][LD_FATType]

               ; 25/01/2010
               mov byte ptr [Current_FATType], al

               mov ah, byte ptr [SI][LD_Name] 
               mov byte ptr [Current_Dir_Drv], ah

               and al, al
               jz short loc_restore_FS_current_directory

loc_restore_FAT_current_directory:
               mov ah, byte ptr [SI][LD_CDirLevel]
               mov byte ptr [Current_Dir_Level], ah    
	       or ah, ah
               jz short loc_ccdrv_reset_cdir_FAT_12_16_32_fcluster
               mov al, 16
               mul ah
               push si
               add si, ax
               mov ax, word ptr [SI][LD_CurrentDirectory]+12
               mov dx, word ptr [SI][LD_CurrentDirectory]+14
               pop si
               jmp short loc_ccdrv_reset_cdir_FAT_fcluster

               ; 01/01/2010
loc_restore_FS_current_directory:
               call proc_load_current_FS_directory 
               retn 

loc_ccdrv_reset_cdir_FAT_12_16_32_fcluster:
               cmp al, 3
               jb short loc_ccdrv_reset_cdir_FAT_12_16_fcluster
loc_ccdrv_reset_cdir_FAT32_fcluster:
               mov ax, word ptr [SI][LD_BPB][FAT32_RootFClust]
               mov dx, word ptr [SI][LD_BPB][FAT32_RootFClust]+2
               jmp short loc_ccdrv_check_rootdir_sign
loc_ccdrv_reset_cdir_FAT_12_16_fcluster:   
    	       xor al, al  ; xor ax, ax
               xor dx, dx
loc_ccdrv_check_rootdir_sign:
               cmp byte ptr [SI][LD_CurrentDirectory], 0
               jne short loc_ccdrv_reset_cdir_FAT_fcluster
              ;cmp byte ptr [SI][LD_CurrentDirectory], 'R'
              ;cmp word ptr [SI][LD_CurrentDirectory], 'OR'
              ;jne short loc_ccdrv_set_rootdir_FAT_fcluster
              ;cmp word ptr [SI][LD_CurrentDirectory]+2, 'TO'
              ;je short loc_ccdrv_reset_cdir_FAT_fcluster
loc_ccdrv_set_rootdir_FAT_fcluster:
               mov word ptr [SI][LD_CurrentDirectory]+12, ax
               mov word ptr [SI][LD_CurrentDirectory]+14, dx
  	       mov word ptr [SI][LD_CurrentDirectory], 'OR'
               mov word ptr [SI][LD_CurrentDirectory]+2,'TO'
loc_ccdrv_reset_cdir_FAT_fcluster:
               mov word ptr [Current_Dir_FCluster], ax
               mov word ptr [Current_Dir_FCluster]+2, dx
pass_ccdrv_reset_cdir_FAT_fcluster:
               mov di, offset PATH_Array
               ; 03/10/2009
               push si
               add si, LD_CurrentDirectory
               mov cx, 64
               rep movsw

               call proc_change_prompt_dir_string
               
               pop si 
               
               xor ax, ax
              ;xor dx, dx
 	       mov di, offset Current_Dir_Drv

               mov byte ptr [Restore_CDIR], al ; 0

               retn
; 29/11/2009
Restore_CDIR: db 0FFh ; Initial value -> any number except 0

proc_restore_current_directory endp

proc_dos_prompt proc near
                ;15/09/2011 External command bytes <=> 8
                ;11/09/2011
                ; programn.ext (12 byte) fixup
                ; argument moving fixup 
                ;13/9/2009
		;2004-2005
                ; 

               ;push ds
               ;pop  es 
loc_TRDOS_prompt:
                mov di, offset TextBuffer
                mov byte ptr [DI], "["
                inc di
                mov si, offset TRDOSPromptLabel
get_next_prompt_label_char:
                cmp byte ptr [SI], 20h
                jb  short pass_prompt_label
                movsb
                jmp short get_next_prompt_label_char
pass_prompt_label:
                mov byte ptr [DI], "]"
                inc di
                mov byte ptr [DI], 20h
                inc di
                mov si, offset Current_Dir_Drv
                movsw
                movsb 
loc_prompt_current_directory:
                cmp byte ptr [SI], 20h
                jb short pass_prompt_current_directory
                movsb
                jmp short loc_prompt_current_directory  
pass_prompt_current_directory:
                mov byte ptr [DI], '>'
                inc di
                mov byte ptr [DI], 0  
                mov si, offset TextBuffer
                call proc_printmsg
             
                mov     ah,03h
                mov     bx,07h                  
		int     10h
		mov     byte ptr [CursorColumn],dl
loc_rw_char:
                call    proc_rw_char
                mov     byte ptr [CommandBuffer]+79,0
loc_move_command:
		; 15/09/2011
		mov     si, offset CommandBuffer
                mov     di, si
                xor 	bx, bx
                xor     cx, cx
                ; 17/09/2011
first_command_char:
                mov     al, byte ptr [SI][BX]
	        inc	bl 
             	cmp     al, 20h
		ja      short pass_space_control
		jb      short loc_move_cmd_arguments_ok
                cmp     bl, 79
                jb      short first_command_char
                jmp 	short loc_move_cmd_arguments_ok
next_command_char:
		mov     al, byte ptr [SI][BX]
		inc     bl
                ; 17/09/2011
		cmp     al,20h
		jna     short pass_move_command
pass_space_control:
		cmp     al,61h
		jb      short pass_capitalize
		cmp     al,7Ah
		ja      short pass_capitalize
		and     al,0DFh
pass_capitalize:
                stosb   
                ;15/09/2011
                inc     cl
                cmp     bl, 79
                jb      short next_command_char 
		jmp     short loc_move_cmd_arguments_ok
pass_move_command:
               ; 11/09/2011
                xor	al, al ; 0
loc_move_cmd_arguments:
                mov     byte ptr [DI], al
                inc    	di
                cmp     bl, 79 ; 15/09/2011
                jnb     short loc_move_cmd_arguments_ok
                mov     al, byte ptr [SI][BX]
                inc     bl ; 15/09/2011
                cmp     al, 20h
                jnb 	short loc_move_cmd_arguments
                
loc_move_cmd_arguments_ok:
                mov     byte ptr [DI], 0
       
call_command_intepreter:
                call    command_interpreter

                mov     cx,79 ; 80 ?
                mov     di, offset CommandBuffer
                xor     al,al
                rep     stosb
               ;cmp     byte ptr [Program_Exit], 0
               ;ja      short loc_terminate_trdos
                
                mov     bx,07h 
                mov     al,0Dh
                mov     ah,0Eh
                int     10h
		mov     al,0Ah
		int     10h
                jmp     loc_TRDOS_prompt ; loop

loc_dos_prompt_retn:
  
                retn

proc_dos_prompt endp

proc_rw_char    proc    near
                ; 2004-2005
                ; 

                push    es
                xor     cx,cx
                mov     es,cx
readnextchar:
                xor     ah,ah
		int     16h
		and     al,al
		jz      short loc_arrow    
		cmp     al,0E0h          
		jz      short loc_arrow
		cmp     al,08h             
		jnz     short char_return
loc_back:
		cmp     dl, byte ptr [CursorColumn]
		ja      short prev_column
		jmp     short readnextchar
prev_column:
		dec     dl
set_cursor_pos:
		mov     ah,02h
		int     10h
              ; xor     bh,bh
                mov     bl,dl
                sub     bl, byte ptr [CursorColumn] 
                mov     cx,1
                mov     ah,09h
		mov     al,20h
                mov     byte ptr [CommandBuffer][BX],al
                mov     bl,7
loc_write_it:
		int     10h
                mov     dl, byte ptr ES:[CCCpointer]
                jmp     short readnextchar
loc_arrow:    
		cmp     AH,4Bh
		jz      short loc_back
		cmp     AH,53h
		jz      short loc_back
		cmp     AH,4Dh
		jnz     short readnextchar
		cmp     dl,79
		jnb     short readnextchar
		inc     dl
		jmp     short set_cursor_pos
char_return:
                mov     bl,dl
              ; xor     bh,bh 
                sub     bl, byte ptr [CursorColumn] 
                mov     ah,0Eh
                cmp     al,20h
		jb      short loc_escape
                mov     byte ptr [CommandBuffer][BX],al
                mov     bl,7
                cmp     dl,79
		jb      short loc_write_it
		jmp     short readnextchar
loc_escape:
		cmp     al,1Bh
                jnz     short pass_escape
                pop     es
                retn
pass_escape:
		cmp     al,0Dh
		jnz     short readnextchar
                int     10h
		mov     al,0Ah
		int     10h
                pop     es
                retn
proc_rw_char    endp

proc_show_date  proc    near
                ; (c) Erdogan Tan [2004-2005]
                ; 

                mov ah, 04h
                int 1Ah

                mov al, dl
                call proc_hex
                mov word ptr [Day], ax

                mov al, dh
                call proc_hex
                mov word ptr [Month], ax

                mov al, ch
                call proc_hex
                mov word ptr [Century], ax

                mov al, cl
                call proc_hex
                mov word ptr [Year], ax
                
                mov si, offset Msg_Show_Date
                call proc_printmsg

                retn
Msg_Show_Date:
                db   'Current date is '
Day:            db   '0'
		db   '0'
                db   '/'
Month:          db   '0'
		db   '0'
                db   '/'
Century:        db   '0'
                db   '0'
Year:           db   '0'
		db   '0'
                db   0Dh, 0Ah, 0

proc_show_date  endp

proc_set_date   proc near
                ; (c) Erdogan Tan [2004-2005]
                ; 

                mov si, offset Msg_Enter_Date
                call proc_printmsg
loc_enter_day_1:
                xor ah, ah
                int 16h
                ; al = ASCII Code of the Character
                cmp al, 13
                je loc_set_date_retn
                cmp al, 27
                je loc_set_date_retn
                mov byte ptr [Day], al
                cmp al, '0'
                jb loc_set_date_stc_0
                cmp al, '3'
                ja loc_set_date_stc_0
                mov ah, 0Eh
                int 10h
loc_enter_day_2:
                xor ah, ah
                int 16h
                ; al = ASCII Code of the Character
                cmp al, 27
                je loc_set_date_retn
                mov byte ptr [Day]+1, al
                cmp al, '0'
                jb loc_set_date_stc_1
                cmp al, '9'
                ja loc_set_date_stc_1
                cmp byte ptr [Day], '3'
                jb  short pass_set_day_31
                cmp al, '1'
                ja  loc_set_date_stc_1
pass_set_day_31:
                mov ah, 0Eh
                int 10h
loc_enter_separator_1:
                xor ah, ah
                int 16h
                ; al = ASCII Code of the Character
                cmp al, 27
                je  loc_set_date_retn
                cmp al, '-'
                je  short pass_set_date_separator_1
                cmp al, '/'
                jne loc_set_date_stc_2
pass_set_date_separator_1:
                mov ah, 0Eh
                int 10h
loc_enter_month_1:
                xor ah, ah
                int 16h
                ; al = ASCII Code of the Character
                cmp al, 27
                je  loc_set_date_retn
                mov byte ptr [Month], al
                cmp al, '0'
                jb  loc_set_date_stc_3
                cmp al, '1'
                ja  loc_set_date_stc_3
                mov ah, 0Eh
                int 10h
loc_enter_month_2:
                xor ah, ah
                int 16h
                ; al = ASCII Code of the Character
                cmp al, 27
                je loc_set_date_retn
                mov byte ptr [Month]+1, al
                cmp al, '0'
                jb  loc_set_date_stc_4
                cmp al, '9'
                ja  loc_set_date_stc_4
                cmp byte ptr [Month], '1'
                jb  short pass_set_month_12
                cmp al, '2'
                ja  loc_set_date_stc_4
pass_set_month_12:
                mov ah, 0Eh
                int 10h
loc_enter_separator_2:
                xor ah, ah
                int 16h
                ; al = ASCII Code of the Character
                cmp al, 27
                je  loc_set_date_retn
                cmp al, '-'
                je  short pass_set_date_separator_2
                cmp al, '/'
                jne loc_set_date_stc_5
pass_set_date_separator_2:
                mov ah, 0Eh
                int 10h
loc_enter_year_1:
                xor ah, ah
                int 16h
                ; al = ASCII Code of the Character
                cmp al, 27
                je  loc_set_date_retn
                mov byte ptr [Year], al
                cmp al, '0'
                jb  loc_set_date_stc_6
                cmp al, '9'
                ja  loc_set_date_stc_6
                mov ah, 0Eh
                int 10h
loc_enter_year_2:
                xor ah, ah
                int 16h
                ; al = ASCII Code of the Character
                cmp al, 27
                je  loc_set_date_retn
                mov byte ptr [Year]+1, al
                cmp al, '0'
                jb  loc_set_date_stc_7
                cmp al, '9'
                ja  loc_set_date_stc_7
                mov ah, 0Eh
                int 10h

loc_set_date_get_lchar_again:
                xor ah, ah
                int 16h

                cmp al, 13
                je short loc_set_date_progress
                cmp al, 27
                je short loc_set_date_retn

                cmp ax, 0E08h
                je short loc_set_date_bs_8
                cmp ax, 4BE0h
                je short loc_set_date_bs_8
                cmp ax, 4B00h
                je short loc_set_date_bs_8
                cmp ax, 53E0h
                je short loc_set_date_bs_8

                jmp short loc_set_date_get_lchar_again
                
loc_set_date_bs_8:
                mov al, 08h
                mov ah, 0Eh
                int 10h
                mov al, 20h
                mov ah, 09h
                int 10h
                jmp loc_enter_year_2

loc_set_date_progress:
                ; Get Current Date
                mov ah, 04h
                int 1Ah

                mov ah, byte ptr [Year]
                sub ah, '0'
                mov al, 16
                mul ah
                mov cl, al
                mov al, byte ptr [Year]+1
                sub al, '0'
                add cl, al

                mov ah, byte ptr [Month]
                sub ah, '0'
                mov al, 16
                mul ah
                mov dh, al
                mov al, byte ptr [Month]+1
                sub al, '0'
                add dh, al

                mov ah, byte ptr [Day]
                sub ah, '0'
                mov al, 16
                mul ah
                mov dl, al
                mov al, byte ptr [Day]+1
                sub al, '0'
                add dl, al

                mov ah, 05h
                int 1Ah

loc_set_date_retn:
                mov si, offset nextline
                call proc_printmsg
                retn

loc_set_date_stc_0:
                mov al, 07h
                mov ah, 0Eh
                int 10h
                jmp loc_enter_day_1
loc_set_date_stc_1:
                cmp ax, 0E08h
                je short loc_set_date_bs_1
                cmp ax, 4BE0h
                je short loc_set_date_bs_1
                cmp ax, 4B00h
                je short loc_set_date_bs_1
                cmp ax, 53E0h
                je short loc_set_date_bs_1
                mov al, 07h
                mov ah, 0Eh
                int 10h
                jmp loc_enter_day_2
loc_set_date_bs_1:
                mov al, 08h
                mov ah, 0Eh
                int 10h
                mov al, 20h
                mov ah, 09h
                int 10h
                jmp loc_enter_day_1
loc_set_date_stc_2:
                cmp ax, 0E08h
                je short loc_set_date_bs_2
                cmp ax, 4BE0h
                je short loc_set_date_bs_2
                cmp ax, 4B00h
                je short loc_set_date_bs_2
                cmp ax, 53E0h
                je short loc_set_date_bs_2
                mov al, 07h
                mov ah, 0Eh
                int 10h
                jmp loc_enter_separator_1
loc_set_date_bs_2:
                mov al, 08h
                mov ah, 0Eh
                int 10h
                mov al, 20h
                mov ah, 09h
                int 10h
                jmp loc_enter_day_2
loc_set_date_stc_3:
                cmp ax, 0E08h
                je short loc_set_date_bs_3
                cmp ax, 4BE0h
                je short loc_set_date_bs_3
                cmp ax, 4B00h
                je short loc_set_date_bs_3
                cmp ax, 53E0h
                je short loc_set_date_bs_3
                mov al, 07h
                mov ah, 0Eh
                int 10h
                jmp loc_enter_month_1
loc_set_date_bs_3:
                mov al, 08h
                mov ah, 0Eh
                int 10h
                mov al, 20h
                mov ah, 09h
                int 10h
                jmp loc_enter_separator_1
loc_set_date_stc_4:
                cmp ax, 0E08h
                je short loc_set_date_bs_4
                cmp ax, 4BE0h
                je short loc_set_date_bs_4
                cmp ax, 4B00h
                je short loc_set_date_bs_4
                cmp ax, 53E0h
                je short loc_set_date_bs_4
                mov al, 07h
                mov ah, 0Eh
                int 10h
                jmp loc_enter_month_2
loc_set_date_bs_4:
                mov al, 08h
                mov ah, 0Eh
                int 10h
                mov al, 20h
                mov ah, 09h
                int 10h
                jmp loc_enter_month_1
loc_set_date_stc_5:
                cmp ax, 0E08h
                je short loc_set_date_bs_5
                cmp ax, 4BE0h
                je short loc_set_date_bs_5
                cmp ax, 4B00h
                je short loc_set_date_bs_5
                cmp ax, 53E0h
                je short loc_set_date_bs_5
                mov al, 07h
                mov ah, 0Eh
                int 10h
                jmp loc_enter_separator_2
loc_set_date_bs_5:
                mov al, 08h
                mov ah, 0Eh
                int 10h
                mov al, 20h
                mov ah, 09h
                int 10h
                jmp loc_enter_month_2
loc_set_date_stc_6:
                cmp ax, 0E08h
                je short loc_set_date_bs_6
                cmp ax, 4BE0h
                je short loc_set_date_bs_6
                cmp ax, 4B00h
                je short loc_set_date_bs_6
                cmp ax, 53E0h
                je short loc_set_date_bs_6
                mov al, 07h
                mov ah, 0Eh
                int 10h
                jmp loc_enter_year_1
loc_set_date_bs_6:
                mov al, 08h
                mov ah, 0Eh
                int 10h
                mov al, 20h
                mov ah, 09h
                int 10h
                jmp loc_enter_separator_2
loc_set_date_stc_7:
                cmp ax, 0E08h
                je short loc_set_date_bs_7
                cmp ax, 4BE0h
                je short loc_set_date_bs_7
                cmp ax, 4B00h
                je short loc_set_date_bs_7
                cmp ax, 53E0h
                je short loc_set_date_bs_7
                mov al, 07h
                mov ah, 0Eh
                int 10h
                jmp loc_enter_year_2
loc_set_date_bs_7:
                mov al, 08h
                mov ah, 0Eh
                int 10h
                mov al, 20h
                mov ah, 09h
                int 10h
                jmp loc_enter_year_1
Msg_Enter_Date:
                db 'Enter new date (dd-mm-yy): '
                db 0

proc_set_date   endp

proc_show_time  proc    near
  		; (c) Erdogan Tan [2004-2005]
                ; 

                mov ah, 02h
                int 1Ah

                mov al, ch
                call proc_hex
                mov word ptr [Hour], ax

                mov al, cl
                call proc_hex
                mov word ptr [Minute], ax

                mov al, dh
                call proc_hex
                mov word ptr [Second], ax

                mov si, offset Msg_Show_Time
                call proc_printmsg

                retn
Msg_Show_Time:
                db   'Current time is '
Hour:           db   '0'
		db   '0'
                db   ':'
Minute:         db   '0'
		db   '0'
                db   ':'
Second:         db   '0'
                db   '0'
                db   0Dh, 0Ah, 0

proc_show_time  endp

proc_set_time   proc near
                ; (c) Erdogan Tan [2004-2005]
                ; 
                mov si, offset Msg_Enter_Time
                call proc_printmsg

loc_enter_hour_1:
                xor ah, ah
                int 16h
                ; al = ASCII Code of the Character
                cmp al, 13
                je loc_set_time_retn
                cmp al, 27
                je loc_set_time_retn
                mov byte ptr [Hour], al
                cmp al, '0'
                jb loc_set_time_stc_0
                cmp al, '2'
                ja loc_set_time_stc_0
                mov ah, 0Eh
                int 10h
loc_enter_hour_2:
                xor ah, ah
                int 16h
                ; al = ASCII Code of the Character
                cmp al, 27
                je loc_set_time_retn
                mov byte ptr [Hour]+1, al
                cmp al, '0'
                jb loc_set_time_stc_1
                cmp al, '9'
                ja loc_set_time_stc_1
                cmp byte ptr [Hour], '2'
                jb  short pass_set_time_24
                cmp al, '4'
                ja  loc_set_time_stc_1
pass_set_time_24:
                mov ah, 0Eh
                int 10h
loc_enter_time_separator_1:
                xor ah, ah
                int 16h
                ; al = ASCII Code of the Character
                cmp al, 27
                je  loc_set_time_retn
                cmp al, ':'
                jne loc_set_time_stc_2
                mov ah, 0Eh
                int 10h
loc_enter_minute_1:
                xor ah, ah
                int 16h
                ; al = ASCII Code of the Character
                cmp al, 27
                je  loc_set_time_retn
                mov byte ptr [Minute], al
                cmp al, '0'
                jb  loc_set_time_stc_3
                cmp al, '5'
                ja  loc_set_time_stc_3
                mov ah, 0Eh
                int 10h
loc_enter_minute_2:
                xor ah, ah
                int 16h
                ; al = ASCII Code of the Character
                cmp al, 27
                je  loc_set_time_retn
                mov byte ptr [Minute]+1, al
                cmp al, '0'
                jb  loc_set_time_stc_4
                cmp al, '9'
                ja  loc_set_time_stc_4
                mov ah, 0Eh
                int 10h
loc_enter_time_separator_2:
                mov word ptr [Second], 3030h
                xor ah, ah
                int 16h
                ; al = ASCII Code of the Character
                cmp al, 13
                je  loc_set_time_progress
                cmp al, 27
                je  loc_set_time_retn
                cmp al, ':'
                jne loc_set_time_stc_5
                mov ah, 0Eh
                int 10h
loc_enter_second_1:
                xor ah, ah
                int 16h
                ; al = ASCII Code of the Character
                cmp al, 13
                je  loc_set_time_progress
                cmp al, 27
                je  loc_set_time_retn
                mov byte ptr [Second], al
                cmp al, '0'
                jb  loc_set_time_stc_6
                cmp al, '5'
                ja  loc_set_time_stc_6
                mov ah, 0Eh
                int 10h
loc_enter_second_2:
                xor ah, ah
                int 16h
                ; al = ASCII Code of the Character
                cmp al, 27
                je  loc_set_time_retn
                mov byte ptr [Second]+1, al
                cmp al, '0'
                jb  loc_set_time_stc_7
                cmp al, '9'
                ja  loc_set_time_stc_7
                mov ah, 0Eh
                int 10h

loc_set_time_get_lchar_again:
                xor ah, ah
                int 16h

                cmp al, 13
                je short loc_set_time_progress
                cmp al, 27
                je short loc_set_time_retn

                cmp ax, 0E08h
                je short loc_set_time_bs_8
                cmp ax, 4BE0h
                je short loc_set_time_bs_8
                cmp ax, 4B00h
                je short loc_set_time_bs_8
                cmp ax, 53E0h
                je short loc_set_time_bs_8

                jmp short loc_set_time_get_lchar_again
                
loc_set_time_bs_8:
                mov al, 08h
                mov ah, 0Eh
                int 10h
                mov al, 20h
                mov ah, 09h
                int 10h
                jmp loc_enter_second_2

loc_set_time_progress:
                ; Get Current Time
                mov ah, 02h
                int 1Ah

                mov ah, byte ptr [Hour]
                sub ah, '0'
                mov al, 16
                mul ah
                mov ch, al
                mov al, byte ptr [Hour]+1
                sub al, '0'
                add ch, al

                mov ah, byte ptr [Minute]
                sub ah, '0'
                mov al, 16
                mul ah
                mov cl, al
                mov al, byte ptr [Minute]+1
                sub al, '0'
                add cl, al

                mov ah, byte ptr [Second]
                sub ah, '0'
                mov al, 16
                mul ah
                mov dh, al
                mov al, byte ptr [Second]+1
                sub al, '0'
                add dh, al

                mov ah, 03h
                int 1Ah

loc_set_time_retn:
                mov si, offset nextline
                call proc_printmsg
                retn

loc_set_time_stc_0:
                mov al, 07h
                mov ah, 0Eh
                int 10h
                jmp loc_enter_hour_1
loc_set_time_stc_1:
                cmp ax, 0E08h
                je short loc_set_time_bs_1
                cmp ax, 4BE0h
                je short loc_set_time_bs_1
                cmp ax, 4B00h
                je short loc_set_time_bs_1
                cmp ax, 53E0h
                je short loc_set_time_bs_1
                mov al, 07h
                mov ah, 0Eh
                int 10h
                jmp loc_enter_hour_2
loc_set_time_bs_1:
                mov al, 08h
                mov ah, 0Eh
                int 10h
                mov al, 20h
                mov ah, 09h
                int 10h
                jmp loc_enter_hour_1
loc_set_time_stc_2:
                cmp ax, 0E08h
                je short loc_set_time_bs_2
                cmp ax, 4BE0h
                je short loc_set_time_bs_2
                cmp ax, 4B00h
                je short loc_set_time_bs_2
                cmp ax, 53E0h
                je short loc_set_time_bs_2
                mov al, 07h
                mov ah, 0Eh
                int 10h
                jmp loc_enter_time_separator_1
loc_set_time_bs_2:
                mov al, 08h
                mov ah, 0Eh
                int 10h
                mov al, 20h
                mov ah, 09h
                int 10h
                jmp loc_enter_hour_2
loc_set_time_stc_3:
                cmp ax, 0E08h
                je short loc_set_time_bs_3
                cmp ax, 4BE0h
                je short loc_set_time_bs_3
                cmp ax, 4B00h
                je short loc_set_time_bs_3
                cmp ax, 53E0h
                je short loc_set_time_bs_3
                mov al, 07h
                mov ah, 0Eh
                int 10h
                jmp loc_enter_minute_1
loc_set_time_bs_3:
                mov al, 08h
                mov ah, 0Eh
                int 10h
                mov al, 20h
                mov ah, 09h
                int 10h
                jmp loc_enter_time_separator_1
loc_set_time_stc_4:
                cmp ax, 0E08h
                je short loc_set_time_bs_4
                cmp ax, 4BE0h
                je short loc_set_time_bs_4
                cmp ax, 4B00h
                je short loc_set_time_bs_4
                cmp ax, 53E0h
                je short loc_set_time_bs_4
                mov al, 07h
                mov ah, 0Eh
                int 10h
                jmp loc_enter_minute_2
loc_set_time_bs_4:
                mov al, 08h
                mov ah, 0Eh
                int 10h
                mov al, 20h
                mov ah, 09h
                int 10h
                jmp loc_enter_minute_1
loc_set_time_stc_5:
                cmp ax, 0E08h
                je short loc_set_time_bs_5
                cmp ax, 4BE0h
                je short loc_set_time_bs_5
                cmp ax, 4B00h
                je short loc_set_time_bs_5
                cmp ax, 53E0h
                je short loc_set_time_bs_5
                mov al, 07h
                mov ah, 0Eh
                int 10h
                jmp loc_enter_time_separator_2
loc_set_time_bs_5:
                mov al, 08h
                mov ah, 0Eh
                int 10h
                mov al, 20h
                mov ah, 09h
                int 10h
                jmp loc_enter_minute_2
loc_set_time_stc_6:
                cmp ax, 0E08h
                je short loc_set_time_bs_6
                cmp ax, 4BE0h
                je short loc_set_time_bs_6
                cmp ax, 4B00h
                je short loc_set_time_bs_6
                cmp ax, 53E0h
                je short loc_set_time_bs_6
                mov al, 07h
                mov ah, 0Eh
                int 10h
                mov word ptr [Second], 3030h
                jmp loc_enter_second_1
loc_set_time_bs_6:
                mov al, 08h
                mov ah, 0Eh
                int 10h
                mov al, 20h
                mov ah, 09h
                int 10h
                jmp loc_enter_time_separator_2
loc_set_time_stc_7:
                cmp ax, 0E08h
                je short loc_set_time_bs_7
                cmp ax, 4BE0h
                je short loc_set_time_bs_7
                cmp ax, 4B00h
                je short loc_set_time_bs_7
                cmp ax, 53E0h
                je short loc_set_time_bs_7
                mov al, 07h
                mov ah, 0Eh
                int 10h
                jmp loc_enter_second_2
loc_set_time_bs_7:
                mov al, 08h
                mov ah, 0Eh
                int 10h
                mov al, 20h
                mov ah, 09h
                int 10h
                jmp loc_enter_second_1
Msg_Enter_Time:
                db 'Enter new time: '
                db 0

proc_set_time   endp


proc_print_volume_info proc near
                ; 25/10/2009
                ; Volume Serial No: 
 		; 15/9/2009 
                ; INPUT  : AL = DOS Drive Number
                ; OUTPUT : AH = FS Type
                ;          AL = DOS Drive Name
                ; CLC => OK
                ; STC => Drive not ready 

                xor bl, bl
                mov bh, al
                mov si, offset Logical_DOSDisks
                add si, bx
                mov al, byte ptr [SI]
                cmp al,'A'  
                jnb short loc_pvi_reset_vol_name
                mov ah, byte ptr [SI][LD_FSType]
                retn

loc_pvi_reset_vol_name:                
		mov byte ptr [Vol_Drv_Name], al
                mov cx, 32
                mov di, offset Vol_Vol_Name
                push di
                mov ax, 2020h
                rep stosw 
                pop di

                push si
                mov ah, byte ptr [SI][LD_FSType]
                mov al, byte ptr [SI][LD_FATType]
		cmp ah, 0A1h
                je short loc_pvi_mov_fs_vol_name
                or ah, ah
                jz  short loc_pvi_pop_si_stc_retn 
                or al, al
                jnz short loc_pass_mov_vol_name_0
loc_pvi_pop_si_stc_retn:
                pop si
                mov al, byte ptr [SI]
                stc
		retn
               
loc_pvi_mov_fs_vol_name:
                add si, LD_FS_VolumeName
                mov cx, 64
                jmp short loc_vol_move_volume_name
loc_pass_mov_vol_name_0:
                mov cx, 11  
                cmp byte ptr [SI][LD_FATType], 2
                ja short loc_pass_mov_vol_name_2
                mov ax, word ptr [SI][LD_BPB][VolumeID]
                mov word ptr [Current_VolSerial1], ax  
                mov ax, word ptr [SI][LD_BPB][VolumeID]+2
                mov word ptr [Current_VolSerial2], ax  
                add si, LD_BPB+VolumeLabel
                jmp short loc_vol_move_volume_name
loc_pass_mov_vol_name_2:
                mov ax, word ptr [SI][LD_BPB][FAT32_VolID]
                mov word ptr [Current_VolSerial1], ax  
                mov ax, word ptr [SI][LD_BPB][FAT32_VolID]+2
                mov word ptr [Current_VolSerial2], ax  
                add si, LD_BPB+FAT32_VolLab
loc_vol_move_volume_name:
                lodsb
                test al, al
                jz short pass_vol_move_volume_name 
                stosb
                loop loc_vol_move_volume_name
pass_vol_move_volume_name:
                pop si
                push si
                cmp byte ptr [SI][LD_FSType], 0A1h
                jne short loc_pass_mov_vol_size
                mov ax, word ptr [SI][LD_FS_VolumeSerial]
                mov word ptr [Current_VolSerial1], ax  
                mov ax, word ptr [SI][LD_FS_VolumeSerial]+2
                mov word ptr [Current_VolSerial2], ax  
                mov ax, word ptr [SI][LD_FS_VolumeSize]
                mov dx, word ptr [SI][LD_FS_VolumeSize]+2
                mov bx, word ptr [SI][LD_FS_BytesPerSec]
                cmp dx, 127
                ja short pass_vol_size_bytes_calc
                jmp short loc_vol_size_mul32
loc_pass_mov_vol_size:
                mov ax, word ptr [SI][LD_TotalSectors]
                mov dx, word ptr [SI][LD_TotalSectors]+2
                mov bx, word ptr [SI][LD_BPB][BPB_BytsPerSec]
                cmp byte ptr [SI][LD_FATType],2
                ja short pass_vol_size_bytes_calc
                ; cmp dx, 127
              ; ja short pass_vol_size_bytes_calc
loc_vol_size_mul32:
                call Proc_Mul32
 		mov cx, offset VolSize_Bytes
                mov word ptr [VolSize_Unit1], cx    
                jmp short pass_volsize_overflow
pass_vol_size_bytes_calc:
                mov cx, offset VolSize_KiloBytes
                mov word ptr [VolSize_Unit1], cx 
               ;cmp bx, 1024
               ; jnb short loc_vol_ax_div_bx
              ;  push ax
              ;  push dx
              ;  xor dx, dx
              ;  mov ax, 1024
              ;  div bx
              ;  mov cx, ax
              ;  pop dx
              ;  pop ax
                mov cx, 2
                call Rx_Dos_Div32
;                jmp short pass_volsize_overflow
;loc_vol_ax_div_bx:
              ;  push ax
              ;  push dx
              ;  xor dx, dx
              ;  mov ax, 1024
              ;  xchg ax, bx
              ;  div bx
              ;  mov bx, ax
              ;  pop dx
              ;  pop ax
;               mov bx, 2
;loc_vol_size_mul32:
;               call Proc_Mul32
;                mov cx, offset VolSize_Bytes
;                mov word ptr [VolSize_Unit1], cx   
pass_volsize_overflow:
                mov di, offset Vol_Tot_Sec_Str_End
              ; mov byte ptr [DI], 0
                mov cx, 10
loc_vol_div32_1_again:
                call Rx_Dos_Div32
                add bl, '0'
                dec di
                mov byte ptr [DI], bl
                test ax, ax
                jnz short loc_vol_div32_1_again
                test dx, dx
                jnz short loc_vol_div32_1_again

                mov word ptr [Vol_Tot_Sec_Str_Start], di

                cmp byte ptr [SI][LD_FATType], 0
                ja short loc_write_vol_FAT_str
                mov di, offset Vol_FS_Name
                mov word ptr [DI], 'RT'
                inc di
		inc di
                mov word ptr [DI], 'F '
                inc di
                inc di
                mov word ptr [DI], '1S'
                mov bx, word ptr [SI][LD_FS_BytesPerSec]
 		mov ax, word ptr [SI][LD_FS_FreeSectors]
                mov dx, word ptr [SI][LD_FS_FreeSectors]+2
                cmp dx, 127
                jna short loc_vol_freespace_mul32
                jmp short pass_vol_freespace_bytes_calc
loc_write_vol_FAT_str:
                mov di, offset Vol_FS_Name
                mov word ptr [DI], 'AF'
                inc di
		inc di
                mov word ptr [DI], ' T'
                inc di
                inc di
                mov ax, '21'
                cmp byte ptr [SI][LD_FATType], 1
                jna short pass_write_vol_FS_str_0
                mov ax, '61'
 		cmp byte ptr [SI][LD_FATType], 2
                jna short pass_write_vol_FS_str_0
                mov ax, '23'
pass_write_vol_FS_str_0:
                mov word ptr [DI], ax
                mov bx, word ptr [SI][LD_BPB][BPB_BytsPerSec] 
pass_write_vol_FS_str_1:
                mov ax, word ptr [SI][LD_FreeSectors]
                mov dx, word ptr [SI][LD_FreeSectors]+2
                cmp dx, 127
                jna short loc_vol_freespace_mul32
pass_vol_freespace_bytes_calc:
                mov cx, offset VolSize_KiloBytes
                mov word ptr [VolSize_Unit2], cx 
                cmp bx, 1024
                jnb short loc_vol_freespace_ax_div_bx
              ;  push ax
              ;  push dx
              ;  xor dx, dx
              ;  mov ax, 1024
              ;  div bx
              ;  mov cx, ax
              ;  pop dx
              ;  pop ax
                mov cx, 2
                call Rx_Dos_Div32
                jmp short pass_vol_freespace_overflow
loc_vol_freespace_ax_div_bx:
              ;  push ax
              ;  push dx
              ;  xor dx, dx
              ;  mov ax, 1024
              ;  xchg ax, bx
              ;  div bx
              ;  mov bx, ax
              ;  pop dx
              ;  pop ax
                mov bx, 2
loc_vol_freespace_mul32:
                call Proc_Mul32
                mov cx, offset VolSize_Bytes
                mov word ptr [VolSize_Unit2], cx   
pass_vol_freespace_overflow:
 
                mov di, offset Vol_Free_Sectors_Str_End
              ; mov byte ptr [DI], 0
                mov cx, 10
loc_vol_div32_2_again:
                call Rx_Dos_Div32
                add bl, '0'
                dec di
                mov byte ptr [DI], bl
                test ax, ax
                jnz short loc_vol_div32_2_again
                test dx, dx
                jnz short loc_vol_div32_2_again

                mov word ptr [Vol_Free_Sectors_Str_Start], di

                mov  al, byte ptr [Current_VolSerial1]
                call proc_hex
                mov  word ptr [Vol_Vol_Serial1]+2, ax
                mov  al, byte ptr [Current_VolSerial1]+1
                call proc_hex
                mov  word ptr [Vol_Vol_Serial1], ax
                mov  al, byte ptr [Current_VolSerial2]
                call proc_hex
                mov  word ptr [Vol_Vol_Serial2]+2, ax
                mov  al, byte ptr [Current_VolSerial2]+1
                call proc_hex
                mov  word ptr [Vol_Vol_Serial2], ax

                mov si, offset Volume_in_drive
                call proc_printmsg

                mov si, offset Vol_Total_Sector_Header
                call proc_printmsg
                mov si, word ptr [Vol_Tot_Sec_Str_Start]
                call proc_printmsg
                mov si, word ptr [VolSize_Unit1]
                call proc_printmsg

                mov si, offset Vol_Free_Sectors_Header
                call proc_printmsg
                mov si, word ptr [Vol_Free_Sectors_Str_Start]
                call proc_printmsg
                mov si, word ptr [VolSize_Unit2]
                call proc_printmsg

                pop si

                mov ah, byte ptr [SI][LD_FSType]
                mov al, byte ptr [SI][LD_FATType]

                retn

FreeClusterCount: dd 0
Vol_Free_Sectors_Str_Start: dw 0
Vol_Free_Sectors_Str: db 10 dup(0)
Vol_Free_Sectors_Str_End: db 0
Vol_Free_Sectors_Header:
                db "Free Space  : ", 0
Vol_Tot_Sec_Str_Start: dw 0
Vol_Tot_Sec_Str: db 10 dup(0)
Vol_Tot_Sec_Str_End: db 0
Vol_Total_Sector_Header:
                db 0Dh, 0Ah, "Volume Size : ", 0
VolSize_Unit1:   dw 0
VolSize_Unit2:   dw 0
VolSize_KiloBytes: db " kilobytes", 0Dh, 0Ah, 0
VolSize_Bytes:  db " bytes", 0Dh, 0Ah, 0
Volume_in_drive:
                db 0Dh, 0Ah
Vol_FS_Name:    db "TR FS1 "
                db "Volume in drive "
Vol_Drv_Name:   db 30h
                db ":"
                db " is "
                db 0Dh, 0Ah
Vol_Vol_Name:   db 64 dup (20h)
                db 0Dh, 0Ah
Vol_Vol_Serial_Header:
                db 0Dh, 0Ah
                db "Volume Serial No: "
Vol_Vol_Serial2:    db "0000"
                db "-"
Vol_Vol_Serial1:    db "0000"
                db 0Dh, 0Ah, 0

proc_print_volume_info endp


proc_print_memory_info proc near
                ; 25/10/2009
                ; INPUT  : None
                ; OUTPUT : AH = FS Type
                ;          AL = DOS Drive Name
                ; CLC => OK
                ; STC => Drive not ready 

                mov ax, 150h ;336, beginning segment
                call proc_get_free_memory  
               ;push bx ; first free segment
                push dx ; max. consequtive memory units
                push ax ; free memory units
                        ; cx = total memory units
                  
                mov ax, 256
                mul cx

                mov di, offset Total_Mem_Str_End
              ; mov byte ptr [DI], 0
                mov cx, 10
loc_total_mem_div32_again:
                call Rx_Dos_Div32
                add bl, '0'
                dec di
                mov byte ptr [DI], bl
                test ax, ax
                jnz short loc_total_mem_div32_again
                test dx, dx
                jnz short loc_total_mem_div32_again

                mov word ptr [Total_Mem_Str_Start], di

                pop ax
                mov cx, 256
                mul cx

                mov di, offset Free_Mem_Str_End
              ; mov byte ptr [DI], 0
                mov cx, 10
loc_free_mem_div32_again:
                call Rx_Dos_Div32
                add bl, '0'
                dec di
                mov byte ptr [DI], bl
                test ax, ax
                jnz short loc_free_mem_div32_again
                test dx, dx
                jnz short loc_free_mem_div32_again

                mov word ptr [Free_Mem_Str_Start], di

                pop ax ; pushed dx
                mov cx, 256
                mul cx

                mov di, offset Free_Consq_Mem_Str_End
              ; mov byte ptr [DI], 0
                mov cx, 10
loc_free_consq_mem_div32_again:
                call Rx_Dos_Div32
                add bl, '0'
                dec di
                mov byte ptr [DI], bl
                test ax, ax
                jnz short loc_free_consq_mem_div32_again
                test dx, dx
                jnz short loc_free_consq_mem_div32_again

                mov word ptr [Free_Consq_Mem_Str_Start], di

               ;pop ax ; pushed bx, First free segment
               ;push ax
               ;call proc_hex
               ;mov  word ptr [First_Free_Segment]+2, ax
               ;pop ax
               ;xchg ah, al
               ;call proc_hex
               ;mov  word ptr [First_Free_Segment], ax
  
                mov si, offset Total_Memory_Header
                call proc_printmsg
                mov si, word ptr [Total_Mem_Str_Start]
                call proc_printmsg
                mov si, offset Mem_Size_Bytes
                call proc_printmsg

                mov si, offset Free_Memory_Header
                call proc_printmsg
                mov si, word ptr [Free_Mem_Str_Start]
                call proc_printmsg
                mov si, offset Mem_Size_Bytes
                call proc_printmsg
                
                mov si, offset Free_Consq_Memory_Header
                call proc_printmsg
                mov si, word ptr [Free_Consq_Mem_Str_Start]
                call proc_printmsg
                mov si, offset Mem_Size_Bytes
                call proc_printmsg

               ;mov si, offset First_Free_Segment_Str
               ;call proc_printmsg

                retn

;First_Free_Segment_Str: 
;                  db 0Dh, 0Ah, "First free segment : "
;First_Free_Segment: 
;                  dd 30303030h
;                  db 'h', 0Dh, 0Ah, 0

Total_Mem_Str_Start: dw 0
Total_Mem_Str: db 10 dup(0)
Total_Mem_Str_End: db 0

Total_Memory_Header:
                db 0Dh, 0Ah, "Total usable memory : ", 0

Free_Mem_Str_Start: dw 0
Free_Mem_Str: db 10 dup(0)
Free_Mem_Str_End: db 0

Free_Memory_Header:
                db 0Dh, 0Ah, "Total free memory size : ", 0

Free_Consq_Mem_Str_Start: dw 0
Free_Consq_Mem_Str: db 10 dup(0)
Free_Consq_Mem_Str_End: db 0

Free_Consq_Memory_Header:
                db 0Dh, 0Ah, "Max. size of free consequtive memory : ", 0

Mem_Size_Bytes: db " bytes", 0Dh, 0Ah, 0

proc_print_memory_info endp

get_volume_serial_number proc near
                   ; 08/08/2010
                   ; INPUT -> DL = Logical DOS Drive number
                   ; OUTPUT -> DX:AX = Volume serial number
                   ;          BL= FAT Type                    
                   ;          BH = Logical DOS drv Number (DL input)
                   ; stc -> Drive not ready


                   push si
                   mov bh, dl
                   xor bl, bl 
                   cmp byte ptr [Last_DOS_DiskNo], dl
                   jnb short loc_gvsn_start
loc_gvsn_stc_retn:
                   xor ax, ax
                   xor dx, dx
                   pop si
                   stc 
                   retn 
loc_gvsn_start:
                   mov si, offset Logical_DOSDisks
                   add si, bx
                   mov bl, byte ptr [SI][LD_FATtype]
                   and bl, bl
                   jz short loc_gvsn_fs
                   cmp bl, 2
                   ja short loc_gvsn_fat32
loc_gvsn_fat:
                   add si, LD_BPB + VolumeID
                   jmp short loc_gvsn_return
loc_gvsn_fat32: 
                   add si, LD_BPB + FAT32_VolID
                   jmp short loc_gvsn_return 
loc_gvsn_fs:
                   cmp byte ptr [SI][LD_FSType], 0A1h
                   jne short loc_gvsn_stc_retn 
                   add si, LD_FS_VolumeSerial
loc_gvsn_return:
                   mov ax, word ptr [SI]
                   mov dx, word ptr [SI]+2
                   pop si
                   retn  

get_volume_serial_number endp

get_environment_string proc near
                  ; 02/09/2011 
                  ; 01/09/2011
                  ; 28/08/2011
                  ; INPUT-> ES:DI = Output buffer
                  ; INPUT -> AL > 0 = AL = String sequence number
                  ;          AL = 0 -> DS:SI = Asciiz Set word 
                  ;                    (environment variable)
                  ; OUTPUT -> DS:SI is not changed
                  ;           ES:DI = String beginning address 
                  ;                   (includes zero tail)
                  ;           AX = String length (except zero tail)
                  ;           DX = Environment variables segment
                  ;           CF = 1 -> Not found (AX not valid)
                  ;

                  mov dx, word ptr CS:[SetEnv_Segment]
                  or dx, dx
                  jz short loc_get_env_string_with_word_stc_retn

                  or al, al
                  jz short loc_get_env_string_with_word

loc_get_env_string_with_seq_number:
                  push ds
                  push si
                  push cx
                  xor ah, ah
                  mov cl, 1
                  mov ch, al
                  mov ds, dx
                  xor si, si
                  lodsb
                  cmp al, 1
                  jb short loc_get_env_string_stc_retn3
               
loc_get_env_string_seq_number_check:
                  cmp ch, cl
                  ja short loc_get_env_string_seq_number_next
                  push di
                  stosb
loc_get_env_string_seq_number_repeat:
                  inc ah
                  lodsb
                  stosb
                  or al, al
                  jnz short loc_get_env_string_seq_number_repeat
loc_get_env_string_seq_number_ok:
                  pop di
                  xchg al, ah
loc_get_env_string_stc_retn3:
                  pop cx
                  pop si
                  pop ds
                  retn   
                  
loc_get_env_string_seq_number_next:
                  lodsb
                  or al, al
                  jnz short loc_get_env_string_seq_number_next
                  lodsb
                  cmp al, 1
                  jb short loc_get_env_string_stc_retn3
                  inc cl
                  jmp short loc_get_env_string_seq_number_check

loc_get_env_string_with_word:
                  push cx
                  push si
                  xor cx, cx
loc_get_env_string_calc_word_length:
                  lodsb 
                  cmp al, 20h
                  jb short loc_get_env_string_calc_word_length_ok
                  ; inc cx
                  inc cl
                  jmp short loc_get_env_string_calc_word_length
                  
loc_get_env_string_calc_word_length_ok:
                  pop si
                  or cl, cl
                  jnz short loc_get_env_string_calc_word_length_save
                  pop cx
              
loc_get_env_string_with_word_stc_retn:
                  xor ax, ax  
                  stc
                  retn
  
loc_get_env_string_calc_word_length_save:
                  mov word ptr CS:[Set_Str_Name_Length], cx

                  push ds
                  push si
                  push es
                  push di
                  
                  mov di, ds
                  mov es, di
                  mov di, si
                  mov ds, dx
                  xor si, si
                  
                  mov al, byte ptr [SI]
                  or al, al
                  jz short loc_get_env_string_stc_retn1

                  push si
                  inc si
                  mov word ptr CS:[Set_Str_Offset], di

loc_get_env_string_compare_rep:
                  mov ah, byte ptr ES:[DI]
                  inc di
                  cmp al, "a"
                  jb short loc_get_env_string_pass_cap1
                  cmp al, "z"
                  ja short loc_get_env_string_pass_cap2
                  and al, 0DFh
loc_get_env_string_pass_cap1:
                  cmp ah, "z"
                  ja short loc_get_env_string_compare_next
                  cmp ah, "a"
                  jb short loc_get_env_string_pass_cap2
                  and ah, 0DFh
loc_get_env_string_pass_cap2:
                  cmp al, ah
                  jne short loc_get_env_string_compare_next
                  lodsb
                  loop loc_get_env_string_compare_rep

                  cmp al, "="
                  jne short loc_get_env_string_compare_next
 
                  pop si

                  pop di 
                  pop es
                  push es
                  push di                  

                 ;xor cx, cx
loc_get_env_string_move_rep:
                  lodsb
                  stosb
                  or al, al
                  jz short loc_get_env_string_move_ok
                  inc cl 
                  cmp cl, 255
                  jb short loc_get_env_string_move_rep
                  xor al, al
                  stosb  
                                       
loc_get_env_string_move_ok:
                  mov ax, cx
                  jmp short loc_get_env_string_stc_retn2

loc_get_env_string_compare_next:
                  cmp si, 510
                  jnb short loc_get_env_string_compare_not_ok
                  and al, al
                  lodsb 
                  jnz short loc_get_env_string_compare_next
                  or al, al
                  jz short loc_get_env_string_compare_not_ok
                  pop cx ; pushed si
                  mov cx, si 
		  dec cx
                  push cx ; si
                  mov cx, word ptr CS:[Set_Str_Name_Length]
                  mov di, word ptr CS:[Set_Str_Offset]
                  jmp short loc_get_env_string_compare_rep

loc_get_env_string_compare_not_ok:
                  pop si
                  xor ax, ax   
loc_get_env_string_stc_retn1:
                  stc
loc_get_env_string_stc_retn2:
                  pop di
                  pop es
                  pop si
                  pop ds
                  pop cx 
                  retn

get_environment_string endp

set_environment_string proc near
                  ; 02/09/2011
                  ; 01/09/2011
                  ; 30/08/2011
                  ; 29/08/2011
                  ; INPUT-> DS:SI = Asciiz environment string
                  ; OUTPUT -> DS:SI is not changed
                  ;           CF = 1 -> Could not set, 
                  ;                     insufficient environment space
                  ;           AX, Dx will be changed 
                  ;
                  mov ax, ds
                  mov word ptr CS:[Set_Str_Segment], ax
                  mov word ptr CS:[Set_Str_Offset], si
                  xor ah, ah
                  lodsb
                  or al, al
                  jz short loc_set_env_string_stc
                  cmp al, "="
                  je short loc_set_env_string_stc
loc_set_env_chk_validation1:
                  inc ah
                  lodsb
                  cmp al, "="
                  je short loc_set_env_chk_validation2
                  cmp al, 20h
                  jnb short loc_set_env_chk_validation1

loc_set_env_string_stc:
                  mov si, word ptr CS:[Set_Str_Offset] 
                  stc
                  retn 
                     
loc_set_env_chk_validation2:
                  push cx
                  push bx
                  push es
                  push di

loc_set_env_chk_validation3:
                  lodsb
                  cmp al, 20h
                  je short loc_set_env_chk_validation3

                  pushf

                  mov bx, word ptr CS:[SetEnv_Segment]
                  or bx, bx ; word ptr CS:[SetEnv_Segment]
                  jnz short loc_set_env_chk_validation4

                  popf
                  jb short loc_set_env_string_nothing

loc_set_env_string_allocate_envb:
                  mov ax, cs
                  mov ds, ax
                 ; bx = 0 => Allocate the first free segment
                  mov cx, 512 ; Allocation size in bytes 
                  mov ax, 0107h ; AL=7, Environment Block allocation
                  xor dx, dx ; Do not check 512 byte sector boundary  
                  call proc_allocate_memory
                  jc short loc_set_env_string_allocate_envb_retn
loc_set_env_string_allocate_envb_ok:
                  mov word ptr [SetEnv_Segment], bx
                  mov es, bx
                  xor di, di
                  mov ax, word ptr [Set_Str_Segment]   
                  mov si, word ptr [Set_Str_Offset]
                  mov ds, ax
                  xor ah, ah ; mov ah, 0 
loc_set_env_string_copy_to_envb:
                  inc ah
                  lodsb
                  stosb
                  or al, al
                  jnz short loc_set_env_string_copy_to_envb
                 ;mov cx, 512 
                  xchg ah, al                 
                  sub cx, ax
                  xor al, al
                  rep stosb
                  push cs
                  pop ds
loc_set_env_string_allocate_envb_retn:  ; stc or clc return
                  pop di
                  pop es 
                  mov bx, word ptr [Set_Str_Segment]
                  mov si, word ptr [Set_Str_Offset]
                  mov ds, bx
                  pop bx
                  pop cx
                  retn

loc_set_env_string_nothing:
                  xor ax, ax
                  jmp short loc_set_env_string_allocate_envb_retn

loc_set_env_chk_validation4:
                  mov ds, bx
                  mov bx, cs
                  mov es, bx  
                  mov di, offset DOSBootSectorBuff
                  xor si, si 
                  mov cx, 511
loc_set_env_chk_validation5:
                  lodsb
                  stosb
                  or al, al
                  jnz short loc_set_env_chk_validation5_loop

                  mov al, byte ptr [SI]
                  and al, al
                  jz short loc_set_env_chk_validation6

loc_set_env_chk_validation5_loop:
                  loop loc_set_env_chk_validation5
                  inc si 
                  xor al, al
loc_set_env_chk_validation6:
                  stosb
                  dec cx  
                  jnz short loc_set_env_chk_validation6
 
                  popf
                  jb short loc_set_env_delete_variable

                  inc si
                  mov word ptr CS:[Set_Str_Old_ENVBL], si

loc_set_env_insert_variable:
                  xchg al, ah
                  mov word ptr CS:[Set_Str_Name_Length], ax
                  xor ax, ax
                  mov word ptr CS:[Set_Str_Old_Offset], ax

loc_set_env_insert_variable_move:                  
                  call loc_set_env_string_move
		  jc short loc_set_env_add_variable

                  call loc_set_env_string_compare
                  or cx, cx
                  jnz short loc_set_env_insert_variable_move

		  ; BX = Set_Str_Old_Offset before moving 
                  mov word ptr [Set_Str_New_Offset], bx

                  call loc_set_env_calculate_length
                 
                  mov dx, word ptr [Set_Str_Old_ENVBL]
                  sub dx, cx
                  
                  call loc_set_env_string_replace
                  
                  call loc_set_env_calculate_length
                  add dx, cx

                  cmp dx, 512
                  ja short loc_set_env_insufficient_space

                  mov word ptr [Set_Str_New_ENVBL], dx

loc_set_env_insert_variable_save:  
		  call loc_set_env_string_save
		  or cl, cl
                  jz short loc_set_env_string_allocate_envb_retn 

                  call loc_set_env_string_move
		  jmp short loc_set_env_insert_variable_save

loc_set_env_add_variable:
		  call loc_set_env_string_replace
                  call loc_set_env_calculate_length
                  mov dx, word ptr [Set_Str_Old_ENVBL]
                  mov ax, dx 
                  add dx, cx
                  cmp dx, 512
                  ja short loc_set_env_insufficient_space
                  mov word ptr [Set_Str_New_ENVBL], dx
                  dec ax
                  mov word ptr [Set_Str_new_Offset], ax
                  call loc_set_env_string_save
                  jnc loc_set_env_string_allocate_envb_retn 

loc_set_env_insufficient_space:
                  mov ax, 08h ; No space for new environment string
                  stc
                  jmp loc_set_env_string_allocate_envb_retn

loc_set_env_delete_variable:
                  mov cl, ah
                  xor ch, ch
                  mov ax, word ptr CS:[Set_Str_Segment]
                  mov si, word ptr CS:[Set_Str_Offset]
                  mov ds, ax
                  mov ax, cs 
                  mov es, ax
                  mov di, offset DOSBootSectorBuff
loc_set_env_delete_variable_chk_rep1:
                  push cx ; environment variable name length
                  push di ; environment variable beginning addr.
loc_set_env_delete_variable_chk_rep2:
                  lodsb
                  scasb
                  jne short loc_set_env_delete_variable_chk_next
                  loop loc_set_env_delete_variable_chk_rep2
                  pop di      
                  pop cx
                  mov al, 0FFh
                  rep stosb
loc_set_env_delete_variable_chk_rep3:
                  mov ah, byte ptr ES:[DI]
                  stosb
                  or ah, ah
                  jnz short loc_set_env_delete_variable_chk_rep3
   
loc_set_env_delete_variable_save:
                  mov cx, 512
                  mov ax, cs
                  mov ds, ax 
                  mov si, offset DOSBootSectorBuff
                  mov ax, word ptr [SetEnv_Segment]
                  mov es, ax
                  xor di, di
loc_set_env_del_var_save_next_byte:
                  lodsb 
                  cmp al, 0FFh
                  je short loc_set_env_del_var_save_loop 
                  stosb
                  or al, al
                  jnz short loc_set_env_del_var_save_loop
                  cmp byte ptr [SI], 0
                  jna short loc_set_env_del_var_save_ok
loc_set_env_del_var_save_loop:
                  loop loc_set_env_del_var_save_next_byte

loc_set_env_del_var_save_ok:
                  cmp di, 1
                  jna short loc_set_env_deallocate_envblk 
loc_set_env_del_var_save_reset:
                  cmp di, 511
                  ja loc_set_env_string_allocate_envb_retn
                  stosb
                  jmp short loc_set_env_del_var_save_reset

loc_set_env_deallocate_envblk:
                  mov bx, es
                  mov ax, 0107h ; AL=7, Environment Block allocation
                  call proc_deallocate_memory
                  mov word ptr [SetEnv_Segment], 0
                  jmp loc_set_env_string_allocate_envb_retn

loc_set_env_delete_variable_chk_next:
                  pop cx ; pushed di
                  add cx, 512
                  sub cx, di ; 1 to 511
                  mov si, word ptr CS:[Set_Str_Offset]
                  xor al, al ; mov al, 0 
                  repne scasb
                  pop cx ; environment variable name length
                  jmp short loc_set_env_delete_variable_chk_rep1

loc_set_env_string_move:
                  mov ax, cs
                  mov ds, ax
                  mov es, ax 
                  mov si, offset DOSBootSectorBuff
                  mov bx, word ptr [Set_Str_Old_Offset]
                  add si, bx
                  mov di, offset textBuffer
                  xor dx, dx
loc_set_env_string_move_next_byte:
                  inc bx
                  cmp bx, word ptr [Set_Str_Old_ENVBL]
                  jnb short loc_set_env_string_move_stc_retn
                  inc dl
                  lodsb
                  stosb 
                  or al, al
                  jz short loc_set_env_string_move_retn
loc_set_env_string_move_loop:
                  cmp dl, 255
                  jna short loc_set_env_string_move_next_byte 
                 ;xor al, al
                 ;stosb
loc_set_env_string_move_retn:
                  mov bx, word ptr [Set_Str_Old_Offset]
                  add word ptr [Set_Str_Old_Offset], dx 
                  retn
loc_set_env_string_move_stc_retn:
                  xor dl, dl
                  mov word ptr [textBuffer], dx 
                  cmc
                  retn

loc_set_env_string_compare:
                  mov ax, cs
                  mov ds, ax
                  mov cx, word ptr [Set_Str_Name_Length]
                  mov ax, cs
                  mov es, ax           
                  mov di, offset textBuffer
                  mov ax, word ptr [Set_Str_Segment] 
                  mov si, word ptr [Set_Str_Offset]
                  mov ds, ax
loc_set_env_string_compare_next_byte:
                  lodsb
 		  mov ah, byte ptr ES:[DI]
                  inc di
                  cmp al, "a"
                  jb short loc_set_env_string_compare_pass_cap1
                  cmp al, "z"
                  ja short loc_set_env_string_compare_pass_cap2
                  and al, 0DFh
loc_set_env_string_compare_pass_cap1:
                  cmp ah, "z"
                  ja short loc_set_env_string_compare_retn
                  cmp ah, "a"
                  jb short loc_set_env_string_compare_pass_cap2
                  and ah, 0DFh
loc_set_env_string_compare_pass_cap2:
                  cmp al, ah
                  jne short loc_set_env_string_compare_retn                        
                  loop loc_set_env_string_compare_next_byte
loc_set_env_string_compare_retn:
                  retn

loc_set_env_string_replace:
                  mov ax, cs
                  mov ds, ax
                  mov es, ax 
                  mov ax, word ptr [Set_Str_Segment] 
                  mov si, word ptr [Set_Str_Offset]
                  mov di, offset textBuffer
                  ;xor ch, ch
                  mov cl, 255
loc_set_env_string_replace_next_byte:
                  lodsb
                  cmp al, 20h
                  jb short loc_set_env_string_replace_last_byte
                  stosb 
                  loop short loc_set_env_string_replace_next_byte
loc_set_env_string_replace_last_byte:                  
                  xor al, al
                  stosb
                  retn  
                                            
loc_set_env_string_save:
                  mov ax, cs
                  mov ds, ax
                  mov si, offset textBuffer
                  mov di, word ptr [Set_Str_New_Offset]
                  mov ax, word ptr [SetEnv_Segment]
                  mov es, ax  
                  mov dx, di
                  mov bx, word ptr [Set_Str_New_ENVBL]
                  dec bx
                  xor cl, cl
loc_set_env_string_save_next_byte:
                  cmp di, bx
                  jnb short loc_set_env_string_save_last_byte
                  lodsb
                  stosb
                  inc cl  
                  or al, al
                  jnz short loc_set_env_string_save_next_byte
                  cmp di, bx
                  jnb short loc_set_env_string_save_retn2
                  mov ah, byte ptr [SI]
                  or ah, ah  
                  jnz short loc_set_env_string_save_retn1
loc_set_env_string_save_retn2:
                  mov byte ptr ES:[DI], al
loc_set_env_string_save_retn1:
                  mov word ptr [Set_Str_New_Offset], di
                  retn
loc_set_env_string_save_last_byte:                  
                  xor al, al
                  stc 
                  jmp short loc_set_env_string_save_retn2

loc_set_env_calculate_length:
                  mov ax, cs
                  mov ds, ax
                  mov si, offset textBuffer
                 ;xor ch, ch
                  xor cl, cl
loc_set_env_string_calc_l_next:
                  inc cl
                  lodsb 
                  or al, al
                  jnz short loc_set_env_string_calc_l_next
                  retn 
    
; 29/08/2011
Set_Str_Offset:   dw 0
Set_Str_Segment:  dw 0
; 02/09/2011
set_str_Old_ENVBL: dw 0 ; Length of environment block
set_str_New_ENVBL: dw 0 ; Length of environment block
; 30/08/2011
Set_Str_Name_Length: dw 0
Set_Str_Old_Offset: dw 0
Set_Str_New_Offset: dw 0

set_environment_string endp

; 28/08/2011
SetEnv_Segment: dw 0
;

mainprog_startup_configuration proc near
                  ; 17/09/2011
loc_load_mainprog_cfg_file:
                mov si, offset MainProgCfgFile
                mov ax, 05h ; 19/10/2010 -> Full open mode
                call proc_open_file
                jc short loc_load_mainprog_cfg_exit

loc_start_mainprog_configuration:
                mov word ptr [PSP_Address], ax

                mov ds, ax
                mov si, OF_Direntry
                add si, DirEntry_FileSize
                mov ax, word ptr [SI]
                mov dx, word ptr [SI]+2
                push cs
                pop ds 
                or dx, dx
                jnz loc_load_mainprog_cfg_close_exit

                mov word ptr [MainProgCfg_FileSize], ax
                mov dx, ax
 
                xor si, si
                mov word ptr [MainProgCfg_LineOffset], si

loc_mcfg_process_next_line_check:
                mov cx, ax
		mov ax, word ptr [PSP_Address]
                add ax, 10h  
                mov ds, ax 
                cmp byte ptr [SI], "*" ; Remark sign
                jne short loc_mcfg_process_next_line
                inc si
                jmp short loc_move_mainprog_cfg_nl1

loc_load_mainprog_cfg_exit:
                retn 

loc_mcfg_process_next_line:
                cmp cx, 79
                jna short loc_start_mainprog_cfg_process
                mov cx, 79 
loc_start_mainprog_cfg_process:
                mov di, offset CommandBuffer
               ;push cs
               ;pop es
loc_move_mainprog_cfg_line:
                lodsb
                cmp al, 20h
                jb short loc_move_mainprog_cfg_nl2
                stosb
                loop loc_move_mainprog_cfg_line
loc_move_mainprog_cfg_nl1:
                cmp si, dx
                jnb short loc_end_of_mainprog_cfg_line
                lodsb
                cmp al, 20h
 		jnb short loc_move_mainprog_cfg_nl1
loc_move_mainprog_cfg_nl2:
                cmp si, dx
                jnb short loc_end_of_mainprog_cfg_line
                mov al, byte ptr [SI]
                cmp al, 20h
 		ja short loc_end_of_mainprog_cfg_line
                inc si
                jmp short loc_move_mainprog_cfg_nl2                               

loc_end_of_mainprog_cfg_line:
                mov byte ptr ES:[DI], 0
                push cs
                pop ds
		mov word ptr [MainProgCfg_LineOffset], si
                
loc_move_mainprog_cfg_command:
		mov 	si, offset CommandBuffer
                mov     di, si
                xor 	bx, bx
                xor     cx, cx
loc_move_mcfg_first_cmd_char:
                mov     al, byte ptr [SI][BX]
	        inc	bl 
             	cmp     al, 20h
		ja      short loc_move_mcfg_cmd_capitalizing
		jb      short loc_move_mcfg_cmd_arguments_ok
                cmp     bl, 79
                jb      short loc_move_mcfg_first_cmd_char
                jmp 	short loc_move_mcfg_cmd_arguments_ok

loc_move_mcfg_next_cmd_char:
		mov     al, byte ptr [SI][BX]
		inc     bl
		cmp     al,20h
		jna     short loc_move_mcfg_cmd_ok
loc_move_mcfg_cmd_capitalizing:
		cmp     al,61h
		jb      short loc_move_mcfg_cmd_caps_ok
		cmp     al,7Ah
		ja      short loc_move_mcfg_cmd_caps_ok
		and     al,0DFh
loc_move_mcfg_cmd_caps_ok:
                stosb   
                inc     cl
                cmp     bl, 79
                jb      short loc_move_mcfg_next_cmd_char
		jmp     short loc_move_mcfg_cmd_arguments_ok
loc_move_mcfg_cmd_ok:
                xor	al, al ; 0
loc_move_mcfg_cmd_arguments:
                mov     byte ptr [DI], al
                inc    	di
                cmp     bl, 79
                jnb     short loc_move_mcfg_cmd_arguments_ok
                mov     al, byte ptr [SI][BX]
                inc     bl
                cmp     al, 20h
                jnb 	short loc_move_mcfg_cmd_arguments
                
loc_move_mcfg_cmd_arguments_ok:
                mov     byte ptr [DI], 0
       
loc_mcfg_process_cmd_interpreter:
                call    command_interpreter

                mov     ax, word ptr [MainProgCfg_FileSize]
                mov     dx, ax
                mov     si, word ptr [MainProgCfg_LineOffset]
                sub     ax, si
                ja      loc_mcfg_process_next_line_check
                 
loc_load_mainprog_cfg_close_exit:
                mov ax, word ptr [PSP_Address]
                call proc_close_file

 		mov     cx,79 ; 80 ?
                mov     di, offset CommandBuffer
                xor     al,al
                rep     stosb
                              
                mov     bx,07h 
                mov     al,0Dh
                mov     ah,0Eh
                int     10h
	        mov     al,0Ah
	        int     10h
     
                retn

MainProgCfgFile: db "MAINPROG.CFG", 0
MainProgCfg_FileSize: dw 0
MainProgCfg_LineOffset: dw 0          

mainprog_startup_configuration endp


Magic_Bytes:
                   db 4
                   db 1
Program_Version:
                   db 7
                   db "[TRDOS] Main Program v1.0.091111"
                   db 0Dh, 0Ah
                   db "(c) Erdogan Tan 2005-2011"
                   db 0Dh, 0Ah, 0

TRDOSPromptLabel: db "TRDOS"
                  db 0
                  db 5 dup(0)
                  db 0

TextBuffer:  	db 256 dup(0)
CommandBuffer: 	db 80 dup(0)
CursorColumn: 	db 0
CmdArgStart:    db 0

Current_VolSerial1: dw 0
Current_VolSerial2: dw 0

Current_Dir_Level: db 0
Current_Dir_FCluster: dd 0
Current_FATType: db 0
Current_Drv: db 0
Current_Dir_Drv: db "?"
                 db ":"
Current_Dir_Root:
                 db "/" 
Current_Directory:
                 db 90 dup (0) 
End_Of_Current_Dir_Str:
                 db 0
Current_Dir_StrLen: db 0           
