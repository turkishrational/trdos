; *****************************************************************************
;
; CMD_INTR.ASM [ TRDOS Command Interpreter Procedure ]  Last Update: 09/11/2011
;
; 09/11/2011 'mem' command 'memory' bugfix
; 08/11/2011 INT 21h Function AH=4Bh IRET modification
; 06/11/2011 'device' command modification, command line arguments
; 05/11/2011 'run' command modification (Interrupt reset will be prevented 
;             if any device drivers have been loaded/initialized). 
; 30/10/2011 'devlist' command (temporary or permanent?)
; 23/10/2011 'device' commmand (device driver file loading and initialization)
; 25/09/2011 Ctrl + Break handler modification
; 24/09/2011 INT 27h 'Terminate and Stay Resident' functionality 
; 18/09/2011 'echo' command (prints a string only)
; 16/09/2011
; 15/09/2011 External & internal command handling improvement 
; 14/09/2011
; 13/09/2011 Major modification for ctrl break handler and program termination
; 12/09/2011
; 11/09/2011 "cmp_cmd_external" jumps and related modifications
; 10/09/2011 external command (program) execution and "run" command modif.
; 03/09/2011 'path' command
; 02/09/2011 'set' command improvement
; 29/08/2011 'set' command
; 28/08/2011 'set' command
; 14/08/2011 'run' command stack pointer allocation modification for COM files
; 11/07/2011
; 10/07/2011 'copy' command read file error return modification
; 19/06/2011 'dir' command 'parse_path_name' modification
; 29/05/2011 'show' command tab space modification/improvement
; 21/05/2011
; 19/05/2011 'copy' command modification
; 18/05/2011 'move' command completed
; 30/04/2011
; 23/04/2011 'move' command
; 17/04/2011 TRDOS Invalid Function Call Handler
;            (Message service for non-valid interrupts and functions)
; 12/03/2011
; 05/02/2011 'run' command, INT 21h improvement
; 03/02/2011 
; 19/01/2011 'show' command modification
; 13/01/2011 'run' command, executable file command tail/arguments
; 20/11/2010 'rename' command completed
; 19/11/2010 'rename' command
; 16/11/2010 'rename' command recognition
; 19/10/2010 'show' command open_file mode 05h bugfix
; 16/10/2010 'rmdir' calc free space and parent dir lmdt modification & bugfix   
; 14/08/2010 'mkdir' and 'copy' command modification
; 09/08/2010 'del' command modification (procedure moving into FILE.ASM)
; 08/08/2010 'copy' command
; 07/08/2010 'copy' command
; 04/08/2010 'copy' command
; 01/08/2010 'copy' command recognition
; 31/07/2010 singlix fs 'mkdir' command completed (no modification)
; 18/07/2010 'mkdir' command completed (except singlix fs)
; 10/07/2010 'mkdir' command recognition
; 26/06/2010 'rmdir' command for Singlix FS is OK! 
; 20/06/2010 'rmdir' command Singlix FS recognition and some modification
; 13/06/2010 'rmdir' command bugfixes
; 05/06/2010 'rmdir' command (derivation from 'del' command procedures)
; 23/05/2010 'attrib' command (derivation from P2000.ASM, 17/12/2000)
; 16/05/2010 'del' command write fault message
; 09/05/2010 'del' command modification for Singlix FS 
; 23/04/2010 'del' command permission denied bugfix (push ds, pop es)
; 18/04/2010 'del' command Singlix FS recognition (not completed)
; 12/04/2010 'del' command completed (except Singlix FS)
; 28/02/2010 'del' command recognition
; 24/01/2010 'longname' singlix fs longname modification
; 07/12/2009 'run' error codes are in al (they were in ah) 
; 06/12/2009 'dir' command and 'show' command includes path
; 05/12/2009
; 29/11/2009 'run' command includes file path (directory)
; 14/11/2009
; 11/11/2009 'run' command bugfix from proc_create_psp
; 08/11/2009 'show' command in temporary format
; 04/11/2009 'run' command in temporary format
; 25/10/2009 'mem' command ('memory')
; 19/10/2009 'cd' command error return fix
; 18/10/2009 'longname' command
; 16/10/2009 'cd' command '..' modification
; 12/10/2009 'cd' command '.','..' modification
; 03/10/2009 'cd' command modification
; 22/09/2009
; 15/09/2009
; Copyright (C) 2005-2010 Erdogan TAN
; 29/01/2005
;
; TRDOS.ASM (include CMD_INTR.ASM)
;
; *****************************************************************************
;
; 13/09/2011
Kernel_VideoMode equ 3
Kernel_VideoPage equ 0
Kernel_CharColorAtr equ 7

command_interpreter proc near
 	       ; 15/09/2011             
  		mov byte ptr [Program_Exit],0
                cmp cx, 8 ; 15/09/2011
                ja cmp_cmd_external ; 16/09/2011
               ; Significiant improvement is needed at the last 
               ; development stage of current tr-dos kernel version
               ; because of internal command handling procedures
               ; are not good sequenced ! 
cmp_cmd_dir:
                mov cx,3
               ;mov si, offset CommandBuffer
                mov di, offset Cmd_Dir
get_char_dir:
                mov al, byte ptr [SI]
                inc si
		scasb
                jne cmp_cmd_cd
		loop get_char_dir
                mov al, byte ptr [SI]
                cmp al, 20h
                ja cmp_cmd_external

                mov word ptr [AttributesMask], 0800h

                ; 06/12/2009
                mov al, byte ptr [Current_Drv]
                mov byte ptr [RUN_CDRV], al

get_dfname_fchar:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je short get_dfname_fchar
                jb loc_print_dir_call_all
                cmp al, '-'
                jne short loc_print_dir_call_flt
get_next_attr_char:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je short get_next_attr_char
                jb loc_cmd_failed
                and al, 0DFh
                cmp al, 'D'
                jne short pass_only_directories
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                ja loc_cmd_failed
                or byte ptr [AttributesMask], 10h
                jmp short get_dfname_fchar_attr
pass_only_directories:
                cmp al, 'F'
                jne check_attr_s
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                ja loc_cmd_failed
                or byte ptr [AttributesMask]+1, 10h

get_dfname_fchar_attr:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je short get_dfname_fchar_attr
                jb short loc_print_dir_call_all

loc_print_dir_call_flt:
               ; 06/12/2009
                push ds
                pop es
                mov di, offset FindFile_Drv
                call proc_parse_pathname
                jnc short loc_print_dir_change_drv_1
               ; 19/06/2011
                cmp al, 1
                ja loc_run_cmd_failed
               ;dec si
               ;cmp byte ptr [SI], ':'
               ;jne loc_cmd_failed
               ;dec si
               ;mov dl, byte ptr [SI]
               ;and dl, 0DFh
               ;sub dl, 'A'
loc_print_dir_change_drv_1:
                mov dl, byte ptr [FindFile_Drv]
loc_print_dir_change_drv_2:
                cmp dl, byte ptr [RUN_CDRV]
                je short loc_print_dir_change_directory 
                call proc_change_current_drive
                jc loc_run_cmd_failed
loc_print_dir_change_directory:
                ; 05/12/2009
                cmp byte ptr [FindFile_Directory], 20h
                jna short pass_print_dir_change_directory

                ; 29/11/2009 -> Restore_CDIR
                inc byte ptr [Restore_CDIR]
                mov si, offset FindFile_Directory
                xor ah, ah ; CD_COMMAND sign -> 0 
                call proc_change_current_directory
                jc loc_run_cmd_failed
loc_print_dir_change_prompt_dir_string:
                call proc_change_prompt_dir_string

pass_print_dir_change_directory:
                mov si, offset FindFile_Name
                cmp byte ptr [SI], 20h
                ja short loc_print_dir_call

loc_print_dir_call_all:
                push si
                mov word ptr [SI], ".*"
                inc si
                inc si
                mov byte ptr [SI], "*"
                inc si
                mov byte ptr [SI], 0
                pop si

loc_print_dir_call:
                call proc_print_directory

                ; 29/11/2009  -> Restore current directory                
                push ds
                pop es

                mov bh, byte ptr [RUN_CDRV] ; it is set at the beginning
                                            ; of the 'run' command.
                cmp bh, byte ptr [Current_Drv]
                je short loc_print_dir_call_restore_cdir_retn
                mov dl, bh
                call proc_change_current_drive 
                retn
loc_print_dir_call_restore_cdir_retn:
                cmp byte ptr [Restore_CDIR], 0
                jna short pass_print_dir_call_restore_cdir_retn
                xor bl, bl
                mov si, offset Logical_DOSDisks
                add si, bx
 
                call proc_restore_current_directory

pass_print_dir_call_restore_cdir_retn:
                retn

check_attr_s_cap:
                and al, 0DFh
check_attr_s:
                cmp al, 'S'
                jne short pass_attr_s
                or byte ptr [AttributesMask], 4
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je get_dfname_fchar_attr
                jb short loc_print_dir_call_all
                and al, 0DFh
pass_attr_s:
                cmp al, 'H'
                jne short pass_attr_h
                or byte ptr [AttributesMask], 2
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je get_dfname_fchar_attr
                jb loc_print_dir_call_all
                jmp short check_attr_s_cap
pass_attr_h:
                cmp al, 'R'
                jne short pass_attr_r
                or byte ptr [AttributesMask], 1
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je get_dfname_fchar_attr
                jb loc_print_dir_call_all
                jmp short check_attr_s_cap
pass_attr_r:
                cmp al, 'A'
                jne loc_cmd_failed
                or byte ptr [AttributesMask], 20h
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je get_dfname_fchar_attr
                jb loc_print_dir_call_all
                jmp short check_attr_s_cap

AttributesMask: dw 0

cmp_cmd_cd:
                mov si, offset CommandBuffer
                mov ax, word ptr [SI]
                cmp ax, 'DC'
                jne cmp_cmd_drive
                add si, 2
                cmp byte ptr [SI], 20h
                ja cmp_cmd_external
get_char_cd:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je short get_char_cd
                jb loc_cmd_cd_return
get_next_cd_char:
                mov ah, byte ptr [SI]+1
                cmp ah, ':'
                jne short loc_cd_change_directory
                mov ah, byte ptr [SI]+2
                cmp ah, 20h
                ja loc_cmd_failed
                and al, 0DFh
                sub al, 'A'
                jc loc_cmd_failed
                mov dl, al 
                call proc_change_current_drive
                jc short loc_cd_drive_not_ready
                retn

loc_cd_change_directory:
                ; 29/11/2009
                ; AH = CDh   ; to separate 'CD' command from others
                             ; for restoring current directory
                             ; 0CDh sign is for saving cdir into 
                             ; DOS drv description table cdir area
                
                mov ah, 0CDh ; mov byte ptr [CD_COMMAND], 0CDh 

                ; 16/10/2009
               	call proc_change_current_directory
                jnc loc_cd_change_cdir_string
              
loc_cd_error_messages:
                cmp al, 3
                je short loc_cd_path_not_found
                cmp al, 15h 
                je short loc_cd_drive_not_ready
                cmp al, 18h ; Bad request structure length 
                je short loc_cd_command_failed
                cmp al, 1Ah ; Unknown media type, non-DOS disk
                je short loc_cd_command_failed
loc_cd_path_not_found:
                ; 19/10/2009
                ;
                push ax                
                ;
                mov si, offset Msg_Dir_Not_Found
                call proc_printmsg
                ;
                pop ax
                cmp ah, byte ptr [Current_Dir_Level]
                jnb short loc_cd_change_cdir_string
                mov byte ptr [Current_Dir_Level], ah 

loc_cd_change_cdir_string:
                call proc_change_prompt_dir_string
                retn

loc_cd_command_failed:
                ; 19/10/2009
                mov si, offset Msg_Bad_Command
                call proc_printmsg
                jmp short loc_cd_fail_drive_restart

loc_cd_drive_not_ready:
                mov si, offset Msg_Not_Ready_Read_Err
                call proc_printmsg
                ;
loc_cd_fail_drive_restart:
                 ; 19/10/2009                 
                mov dl, byte ptr [Current_Drv]
                call proc_change_current_drive
loc_cmd_cd_return:
                retn

cmp_cmd_drive:
                ; C:, D:, E: etc.
                cmp ah, ':'
                jne short cmp_cmd_ver
                mov ah, byte ptr [SI]+2
                cmp ah, 20h
                ja loc_cmd_failed
                and al, 0DFh
                sub al, 'A'
                jc loc_cmd_failed
                cmp al, byte ptr [Last_Dos_DiskNo]
                ja short loc_cd_drive_not_ready
                mov dl, al
                call proc_change_current_drive
                jc short loc_cd_drive_not_ready
                retn

cmp_cmd_ver:
		mov  cx,3
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_Ver
get_char_ver:
                mov  al, byte ptr [SI]
		inc  si
		scasb
                jne  short cmp_cmd_exit
                loop get_char_ver
                mov  al, byte ptr [SI]
		cmp  al,20h
                ja cmp_cmd_external
                mov si, offset Program_Version
                call proc_printmsg
                retn

cmp_cmd_exit:
                mov  cx,4
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_Exit
get_char_exit:
                mov  al, byte ptr [SI]
		inc  si
		scasb
                jne  short cmp_cmd_prompt
                loop get_char_exit
                mov  al, byte ptr [SI]
		cmp  al,20h
                ja cmp_cmd_external
                mov byte ptr [Program_Exit], 1
                retn

cmp_cmd_prompt:
                mov  cx,6
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_Prompt
get_char_prompt:
                mov  al, byte ptr [SI]
		inc  si
		scasb
                jne  short cmp_cmd_volume
                loop get_char_prompt
                mov  al, byte ptr [SI]
		cmp  al,20h
                ja cmp_cmd_external
get_prompt_name_fchar:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je short get_prompt_name_fchar
                ja short loc_change_prompt_label
                mov si, offset TRDOSPromptLabel
                mov word ptr [SI], "RT"
                inc si
                inc si
                mov word ptr [SI], "OD"
                inc si
                inc si
                mov byte ptr [SI], "S"
                inc si
                mov byte ptr [SI], 0
loc_cmd_prompt_return:
                retn
loc_change_prompt_label:
                ; push ds
                ; pop es
                mov cx, 11
                mov di, offset TRDOSPromptLabel
put_char_new_prompt_label:
                lodsb
                cmp al, 20h
                jb short pass_put_new_prompt_label
                stosb
                loop put_char_new_prompt_label
pass_put_new_prompt_label:
                mov byte ptr [DI], 0
                retn
cmp_cmd_volume:
                mov  cx,6
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_Volume
get_char_volume:
                mov  al, byte ptr [SI]
		inc  si
		scasb
                jne  short check_vol_str
                loop get_char_volume
                mov  al, byte ptr [SI]
                cmp  al,20h
                ja cmp_cmd_external
get_vol_drive_name_fchar:
                inc si
get_vol_drive_name_fchar1:
                mov al, byte ptr [SI]
                cmp al, 20h
                ja short pass_vol_current_drive
                je short get_vol_drive_name_fchar
                mov al, byte ptr [Current_Drv]
                jmp short loc_get_volume_size_info
check_vol_str:
                cmp SI, 4
                jne short cmp_cmd_longname
                cmp al,20h
                ja  loc_cmd_failed
                jmp short get_vol_drive_name_fchar1
pass_vol_current_drive:
                cmp al, 'A'
                jb  loc_cmd_failed
                cmp al, 'z'
                ja  loc_cmd_failed
                cmp al, 'Z'
                jna short pass_vol_drv_capitalize
                cmp al, 'a'
                jb  loc_cmd_failed
                and al, 0DFh
pass_vol_drv_capitalize:
                inc si
                mov ah, byte ptr [SI]
                cmp ah, ":"
                jne loc_cmd_failed
                sub al, 'A'
loc_get_volume_size_info:
                call proc_print_volume_info
                jc loc_cd_drive_not_ready
                retn
cmp_cmd_longname:
                mov  cx,8
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_LongName
get_char_longname:
                lodsb
		scasb
                jne  short cmp_cmd_date
                loop get_char_longname
                mov  al, byte ptr [SI]
		cmp  al,20h
                jna  short get_longname_fchar
                cmp  al, "."
                je   cmp_cmd_external
                jmp  loc_cmd_failed
get_longname_fchar:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je short get_longname_fchar
                jb short loc_longname_retn
                call proc_find_longname
                jnc short loc_print_longname
               
                or al, al
                jz short loc_longname_not_found
                  
                cmp al, 15h
                je loc_cd_drive_not_ready
               ;cmp al, 2
               ;je short loc_ln_file_dir_not_found
               ;cmp al, 3
               ;je short loc_ln_file_dir_not_found
               ;jmp short loc_ln_file_dir_not_found                
                
               ;cmp al, 8
               ;jne short pass_long_name_im_error
               ;mov si, offset Msg_Insufficient_Memory
               ;call proc_printmsg
               ;retn
;pass_long_name_im_error:

loc_ln_file_dir_not_found:
                mov si, offset Msg_File_Directory_Not_Found
                call proc_printmsg
                retn
loc_longname_not_found:
                mov si, offset Msg_Longname_Not_Found
                call proc_printmsg
                retn
loc_print_longname:
                ; 19/10/2009
               ;mov si, offset LongFileName
                mov di, offset TextBuffer
                push di 
               ; 24/01/2010
                cmp al, 0
                ja short loc_print_longname_1
loc_print_FS_longname: 
                lodsb
                stosb  
                or al, al
                jnz short loc_print_FS_longname
                jmp short loc_print_longname_2
               ; 
loc_print_longname_1:
                lodsw
                stosb  
                or al, al
                jnz short loc_print_longname_1
loc_print_longname_2:               
                pop si
                call proc_printmsg
                mov si, offset NextLine
                call proc_printmsg
loc_longname_retn:
                retn
               
cmp_cmd_date:
                mov  cx,4
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_Date
get_char_date:
                mov  al, byte ptr [SI]
		inc  si
		scasb
                jne  short cmp_cmd_time
                loop get_char_date
                mov  al, byte ptr [SI]
		cmp  al,20h
                ja  cmp_cmd_external
                call proc_show_date
                call proc_set_date
                retn
cmp_cmd_time:
                mov  cx,4
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_Time
get_char_time:
                mov  al, byte ptr [SI]
		inc  si
		scasb
              ; jne  short cmp_cmd_show
                jne  short cmp_cmd_cls
                loop get_char_time
                mov  al, byte ptr [SI]
		cmp  al,20h
                ja  cmp_cmd_external
                call proc_show_time
                call proc_set_time
                retn

cmp_cmd_cls:
                mov  cx,3
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_cls
get_char_cls:
                mov  al, byte ptr [SI]
		inc  si
		scasb
               ;jne  loc_cmd_failed
                jne  short cmp_cmd_mem
                loop get_char_cls
                mov  al, byte ptr [SI]
		cmp  al,20h
                ja  cmp_cmd_external

                call proc_clear_screen
retn_from_cls:
                retn

cmp_cmd_mem:
		mov  cx,3
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_Mem
get_char_mem:
                mov  al, byte ptr [SI]
		inc  si
		scasb
                jne  short cmp_cmd_show
                loop get_char_mem
                mov  al, byte ptr [SI]
		cmp  al,20h
                jna short loc_print_mem_info
               ; 09/11/2011 BugFix 
                cmp al, 'O'
                jne cmp_cmd_external
                inc si
                mov ax, word ptr [SI]
                cmp ax, 'YR'
                jne cmp_cmd_external
                inc si
                inc si
                cmp byte ptr [SI], 20h
                ja  cmp_cmd_external
loc_print_mem_info:
                call proc_print_memory_info
                retn

cmp_cmd_show:
                mov  cx,4
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_show
get_char_show:
                mov  al, byte ptr [SI]
		inc  si
		scasb
                jne  cmp_cmd_del
                loop get_char_show
                mov  al, byte ptr [SI]
		cmp  al,20h
                ja  cmp_cmd_external
get_showfile_fchar:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je short get_showfile_fchar
                ja short loc_show_parse_path_name
                retn

loc_show_parse_path_name:
                ; 06/12/2009
                push ds
                pop es
                mov di, offset FindFile_Drv
                call proc_parse_pathname
                jc loc_cmd_failed

loc_show_check_filename_exists:
                ; 06/12/2009
                mov si, offset FindFile_Name
                cmp byte ptr [SI], 20h
                jna loc_cmd_failed 

                mov dh, byte ptr [Current_Drv]
                mov byte ptr [RUN_CDRV], dh

loc_show_change_drv:
                mov dl, byte ptr [FindFile_Drv]
                cmp dl, dh
                je short loc_show_change_directory

                call proc_change_current_drive
                jc loc_file_rw_cmd_failed

loc_show_change_directory:
                ; 05/12/2009
                cmp byte ptr [FindFile_Directory], 20h
                jna short loc_findload_showfile

                ; 29/11/2009 -> Restore_CDIR
                inc byte ptr [Restore_CDIR]
                mov si, offset FindFile_Directory
                xor ah, ah ; CD_COMMAND sign -> 0 
                call proc_change_current_directory
                jc loc_file_rw_cmd_failed

;loc_show_change_prompt_dir_string:
               ;call proc_change_prompt_dir_string

loc_findload_showfile:
                mov si, offset FindFile_Name
                mov ax, 05h ; 19/10/2010 -> Full open mode
                call proc_open_file
                jc loc_file_rw_cmd_failed

loc_start_show_file:
                ; 06/12/2009
                mov word ptr [PSP_Address], ax

                mov es, ax
                mov di, OF_Direntry
                add di, DirEntry_FileSize
 
                mov ax, word ptr ES:[DI]
                mov dx, word ptr ES:[DI]+2
 
                push ax
 
               ;mov word ptr [Show_FileSize], ax
               ;mov word ptr [Show_FileSize]+2, dx  

	        mov si, offset nextline
	        call proc_printmsg

                mov si, 100h
              ; 13/09/2011    
                pop bx

               ;mov bx, word ptr [Show_FileSize]
	       ;mov dx, word ptr [Show_FileSize]+2
		mov cx, 22
                jmp short pass_show_wait_for_key
loop_show_file_char:
                and cx, cx
                jnz short pass_show_wait_for_key
		xor ah, ah
		int 16h
		cmp al, 1Bh
		jnz short pass_exit_show
end_of_show_file:
pass_show_file:
		mov si, offset nextline
		call proc_printmsg

                ; 14/11/2009
                mov ax, word ptr [PSP_Address]
                call proc_close_file

loc_show_file_restore_retn:
               ; 28/02/2010
               jmp loc_file_rw_restore_retn

;Show_FileSize: dd 0

pass_exit_show:
		mov cx, 20
pass_show_wait_for_key:
		mov al, byte ptr ES:[SI]
		cmp al, 0Dh
		jnz short pass_show_dec_cx
		dec cx
pass_show_dec_cx:
		cmp al, 09h
	       ; 29/05/2011 	
                je short loc_show_put_tab_space
pass_put_tab_space:
                push bx
                mov ah, 0Eh
               ; 13/09/2011
                mov BH, Kernel_VideoPage
                mov BL, Kernel_CharColorAtr
		int 10h
                pop bx
compare_bx_file_size:
		inc si
		jnz short pass_show_update_si_es
		mov ax, es
		add ax, 1000h
		mov es, ax
pass_show_update_si_es:
               ; 19/01/2011
                dec bx
                jnz short loop_show_file_char
check_dx_show_file:
		or dx, dx
                jz short end_of_show_file 
                dec dx
   	        jmp short loop_show_file_char

loc_show_put_tab_space:
               ; 29/05/2011
                push dx
                push bx
                push cx
               ; 13/09/2011
                mov bh, Kernel_VideoPage 
                mov ah, 03h ; get cursor position and size.
                int 10h
                xor ah, ah
                mov al, 8
                mov cx, ax
                xchg al, dl  ; dl  = 8
                div dl ; AX = Cursor column
                sub cl, ah
                mov al, 20h
                mov ah, 0Eh 
                mov bl, Kernel_CharColorAtr ; 13/09/2011
loop_show_put_tab_space:
		int 10h
                loop loop_show_put_tab_space
                pop cx
                pop bx
                pop dx
                jmp short compare_bx_file_size 

cmp_cmd_del:
                mov  cx,3
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_del
get_char_del:
                mov  al, byte ptr [SI]
		inc  si
		scasb
                jne  cmp_cmd_rename
                loop get_char_del
                mov  al, byte ptr [SI]
		cmp  al,20h
                ja  cmp_cmd_external
get_delfile_fchar:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je short get_delfile_fchar
                jb short loc_delfile_nofilename_retn

loc_delfile_parse_path_name:
                ; 28/02/2010
                push ds
                pop es
                mov di, offset FindFile_Drv
                call proc_parse_pathname
                jc loc_cmd_failed

loc_delfile_check_filename_exists:
                ; 28/02/2010
                mov si, offset FindFile_Name
                cmp byte ptr [SI], 20h
                jna loc_cmd_failed
                mov word ptr [DelFile_FNPointer], si 

loc_delfile_drv:
                mov dh, byte ptr [Current_Drv]
                mov byte ptr [RUN_CDRV], dh

                mov dl, byte ptr [FindFile_Drv]
                cmp dl, dh
                je short loc_delfile_change_directory

                call proc_change_current_drive
                jc loc_file_rw_cmd_failed

loc_delfile_change_directory:
                ; 05/12/2009
                cmp byte ptr [FindFile_Directory], 20h
                jna short loc_delfile_find

                ; 29/11/2009 -> Restore_CDIR
                inc byte ptr [Restore_CDIR]
                mov si, offset FindFile_Directory
                xor ah, ah ; CD_COMMAND sign -> 0 
                call proc_change_current_directory
                jc loc_file_rw_cmd_failed

;loc_delfile_change_prompt_dir_string:
               ;call proc_change_prompt_dir_string

loc_delfile_find:
	       ;mov si, offset FindFile_Name
 		mov si, word ptr [DelFile_FNPointer]
                mov ax, 1800h ; Except volume label and dirs
                call proc_find_first_file
                jc loc_file_rw_cmd_failed

loc_delfile_ambgfn_check:
                cmp bx, 0 ; Ambiguous filename chars used sign (BX>0)
                jna short loc_delfile_found
                mov al, 2 ; File not found sign
                stc
                jmp loc_file_rw_cmd_failed   

loc_delfile_nofilename_retn:
                retn

loc_delfile_found:
                and cl, 07h ; Attributes
               ;20/06/2010 
                jnz short loc_rename_permission_denied

loc_delfile_found_check_lnel:
               ; 03/04/2010
               ;mov al, ch ; If the Long name is yes/existing, ch > 0 
               ;or al, al
               ; 09/08/2010
                or ch, ch
                jnz short loc_delfile_ask_for_delete
loc_delfile_reset_lnel:
                mov byte ptr [LongName_EntryLength], ch

loc_delfile_ask_for_delete:
                mov si, offset Msg_DoYouWantDelete
                call proc_printmsg
                mov si, word ptr [DelFile_FNPointer]
                call proc_printmsg
                mov si, offset Msg_YesNo
                call proc_printmsg
loc_delfile_ask_again:
                xor ah, ah
                int 16h
                cmp al, 0Bh
                je short loc_do_not_delete_file
                and al, 0DFh
 		mov byte ptr [Y_N_nextLine], al
                cmp al, 'Y'
                je short loc_yes_delete_file_nextline
                cmp al, 'N'
                jne short loc_delfile_ask_again
loc_do_not_delete_file:
                mov si, offset Y_N_nextline
                call proc_printmsg
               ; 23/05/2010
                jmp loc_file_rw_restore_retn

loc_yes_delete_file_nextline:
                mov si, offset Y_N_nextline
                call proc_printmsg
loc_yes_delete_file:
               ; 09/08/2010
               mov bh, byte ptr [FindFile_Drv]
               mov bl, byte ptr [LongName_EntryLength]
               ; 09/08/2010 -> DirBuff_EntryCounter -> 28/02/2010
               mov cx, word ptr [DirBuff_EntryCounter]
  	       ; ES:DI -> Directory buffer entry offset/address 
               ; 09/08/2010
               call proc_delete_file
               jnc short loc_print_deleted_message

               cmp al, 05h
               je loc_rename_permission_denied
               stc
               jmp short loc_file_rw_cmd_failed

loc_print_deleted_message:
                mov si, offset Msg_Deleted
                call proc_printmsg
 
               ; clc 

loc_file_rw_restore_retn:
                ; 28/02/2010
loc_file_rw_cmd_failed:
                ; 28/02/2010
                pushf 
                ; loc_run_cmd_failed  
                ; 07/12/2009 -> error codes are in al (they were in ah)
                ; 06/12/2009 -> Restore Current Drive
                ; 29/11/2009 -> Restore Current Directory
                push ds
                pop es
 
                push ax

                mov bh, byte ptr [RUN_CDRV] ; it is set at the beginning
                                            ; of the 'run' command.
                cmp bh, byte ptr [Current_Drv]
                je short loc_file_rw_restore_cdir
                mov dl, bh
                call proc_change_current_drive 
                jmp short loc_file_rw_err_pass_restore_cdir

                ;28/02/2010
loc_rename_permission_denied:
 		mov si, offset Msg_Permission_denied
		call proc_printmsg
               ; 23/05/2010 
                jmp short loc_file_rw_restore_retn

loc_file_rw_restore_cdir:
                cmp byte ptr [Restore_CDIR], 0
                jna short loc_file_rw_err_pass_restore_cdir
 		xor bl, bl
                mov si, offset Logical_DOSDisks
                add si, bx
                call proc_restore_current_directory

loc_file_rw_err_pass_restore_cdir:
                pop ax
                popf
                jc loc_file_rw_check_write_fault
                retn

; 16/05/2010
loc_file_rw_check_write_fault:
                cmp al, 1Dh ; Write Fault
                jne loc_run_cmd_failed_cmp_al
                mov si, offset Msg_Not_Ready_Write_Err
                call proc_printmsg
                retn


DelFile_FNPointer: dw 0


cmp_cmd_rename:
               ;20/11/2010 
               ;16/11/2010
                mov  cx,6
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_Rename
get_char_rename:
                mov  al, byte ptr [SI]
		inc  si
		scasb
                jne  cmp_cmd_rmdir
                loop get_char_rename
                mov  al, byte ptr [SI]
		cmp  al, 20h
                ja  cmp_cmd_external
loop_scan_rename_source_1:
		inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je short loop_scan_rename_source_1
                jb short loc_rename_nofilename_retn
                mov word ptr [SourceFilePath], SI
loop_scan_rename_source_2:
                inc si  
                mov al, byte ptr [SI]
                cmp al, 20h
                je short loc_scan_rename_destination
               ; jb short loc_rename_nofilename_retn
                jb loc_cmd_failed
                jmp short loop_scan_rename_source_2

loc_rename_nofilename_retn:
                retn

loc_scan_rename_destination:
                mov byte ptr [SI], 0
loop_scan_rename_destination_1:
                inc si  
                mov al, byte ptr [SI]
                cmp al, 20h
                je short loop_scan_rename_destination_1
               ;jb short loc_rename_nofilename_retn
                jb loc_cmd_failed
                mov word ptr [DestinationFilePath], SI
loop_scan_rename_destination_2:
                inc si  
                mov al, byte ptr [SI]
                cmp al, 20h
                ja short loop_scan_rename_destination_2
                mov byte ptr [SI], 0
                push ds
                pop es

loc_rename_save_current_drive:
                mov dh, byte ptr [Current_Drv]
                mov byte ptr [RUN_CDRV], dh

loc_rename_sf_parse_path_name:
                mov si, word ptr [SourceFilePath] 
                mov di, offset FindFile_Drv
                call proc_parse_pathname
                jc loc_cmd_failed

loc_rename_sf_check_filename_exists:
                mov si, offset FindFile_Name
                cmp byte ptr [SI], 20h
                jna loc_cmd_failed
               ;mov word ptr [DelFile_FNPointer], si 

loc_rename_sf_drv:
               ;mov dh, byte ptr [Current_Drv]
               ;mov byte ptr [RUN_CDRV], dh

                mov dl, byte ptr [FindFile_Drv]
                cmp dl, dh
                je short loc_rename_sf_change_directory

                call proc_change_current_drive
                jc loc_file_rw_cmd_failed

loc_rename_sf_change_directory:
                cmp byte ptr [FindFile_Directory], 20h
                jna short loc_rename_sf_find

                inc byte ptr [Restore_CDIR]
                mov si, offset FindFile_Directory
                xor ah, ah ; CD_COMMAND sign -> 0 
                call proc_change_current_directory
                jc loc_file_rw_cmd_failed

;loc_rename_sf_change_prompt_dir_string:
               ;call proc_change_prompt_dir_string

loc_rename_sf_find:
                mov si, offset FindFile_Name
               ;mov si, word ptr [DelFile_FNPointer]
                mov ax, 0800h ; Except volume labels
                call proc_find_first_file
                jc loc_file_rw_cmd_failed

loc_rename_sf_ambgfn_check:
                cmp bx, 0 ; Ambiguous filename chars used sign (BX>0)
                jna short loc_rename_sf_found
                mov al, 2 ; File not found sign
                stc
                jmp loc_file_rw_cmd_failed   

loc_rename_sf_found:
                test cl, 07h ; Attributes, S-H-R
                jnz loc_rename_permission_denied
               ;mov byte ptr [Rename_Attribute], cl ; File or directory
                push ds
                pop es 
                mov si, offset FindFile_drv
                mov di, offset SourceFile_drv
                mov cx, 64
                rep movsw

loc_rename_df_parse_path_name:
                mov si, word ptr [DestinationFilePath]
                mov di, offset FindFile_Drv
                call proc_parse_pathname
                jc short loc_rename_df_cmd_failed

                mov dh, byte ptr [Current_Drv]
               ;mov dh, byte ptr [RUN_CDRV]

                mov dl, byte ptr [FindFile_Drv]
                cmp dl, dh
                jne short loc_rename_df_cmd_failed

loc_rename_df_check_dirname_exists:
                cmp byte ptr [FindFile_Directory], 0
                jna short loc_rename_df_check_filename_exists
loc_rename_df_cmd_failed:
                mov ax, 1 ; TRDOS 'Bad command or file name' error
                stc
                jmp loc_file_rw_cmd_failed
                 
loc_rename_df_check_filename_exists:
                mov si, offset FindFile_Name
                call proc_check_filename
                jc loc_mkdir_invalid_dir_name_chars
               ;mov word ptr [DelFile_FNPointer], si 
               ;cmp byte ptr [SI], 20h
               ;ja short loc_rename_df_find

               ;mov dh, byte ptr [Current_Drv] ; dh has not been changed
loc_rename_df_drv_check_writable:
                xor dl, dl

                mov si, offset Logical_DOSDisks
                add si, dx

                mov dl, byte ptr [SI][LD_DiskType]
                xchg dh, dl

                cmp dh, 1 ; 0 = Invalid
                jnb short loc_rename_df_compare_sf_df_name

                mov ax, 13h ; MSDOS err => Disk write-protected
                mov bx, word ptr [DestinationFilePath] 
                jmp loc_file_rw_cmd_failed

loc_rename_df_compare_sf_df_name:
                mov si, offset FindFile_Name
                mov di, offset SourceFile_Name
                mov cx, 12
loc_rename_df_compare_sf_df_name_next: 
                lodsb
                scasb
                jne short loc_rename_df_find
                or al, al
                jz short loc_rename_df_cmd_failed
                loop loc_rename_df_compare_sf_df_name_next 

loc_rename_df_find:
	        mov si, offset FindFile_Name
 		;mov si, word ptr [DelFile_FNPointer]
                xor ax, ax ; Any
                call proc_find_first_file
                jnc short loc_rename_df_found

loc_rename_df_check_error_code:
                cmp al, 2 ;Not found error
                je short loc_rename_df_move_find_struct_to_dest
                stc
                jmp loc_file_rw_cmd_failed

loc_rename_df_found:
                mov ax, 1 ;Bad command or file name error
                stc
                jmp loc_file_rw_cmd_failed

loc_rename_df_move_find_struct_to_dest:
                push ds
                pop es 
                mov si, offset FindFile_drv
                mov di, offset DestinationFile_drv
                mov cx, 64
                rep movsw

loc_rename_df_process_q_sf:
                mov cx, 12
                mov si, offset SourceFile_Name
                mov di, offset Rename_OldName
loc_rename_df_process_q_nml_1_sf:
                lodsb
                cmp al, 20h
                jna short loc_rename_df_process_q_nml_2_sf
                stosb
                loop loc_rename_df_process_q_nml_1_sf
loc_rename_df_process_q_nml_2_sf:
                mov byte ptr ES:[DI], 0

loc_rename_df_process_q_df:
                mov cx, 12
                mov si, offset DestinationFile_Name
                mov di, offset Rename_NewName
loc_rename_df_process_q_nml_1_df:
                lodsb
                cmp al, 20h
                jna short loc_rename_df_process_q_nml_2_df
                stosb
                loop loc_rename_df_process_q_nml_1_df
loc_rename_df_process_q_nml_2_df:
                mov byte ptr ES:[DI], 0

loc_rename_confirmation_question:
                mov si, offset Msg_DoYouWantRename
                call proc_printmsg
               ;mov al, byte ptr [Rename_Attribute]
                mov al, byte ptr [SourceFile_DirEntry]+11 ; Attributes
                and al, 10h
                jnz short loc_rename_confirmation_question_dir
loc_rename_confirmation_question_file:
                mov si, offset Rename_File
                call proc_printmsg 
                jmp short loc_rename_confirmation_question_as 
loc_rename_confirmation_question_dir:
                mov si, offset Rename_Directory
                call proc_printmsg   
loc_rename_confirmation_question_as:
                mov si, offset Rename_OldName
                call proc_printmsg
                mov si, offset Msg_File_rename_as
                call proc_printmsg
                mov si, offset Msg_YesNo
                call proc_printmsg
loc_rename_ask_again:
                xor ah, ah
                int 16h
                cmp al, 0Bh
                je short loc_do_not_rename_file
                and al, 0DFh
 		mov byte ptr [Y_N_nextLine], al
                cmp al, 'Y'
                je short loc_yes_rename_file
                cmp al, 'N'
                je short loc_do_not_rename_file
                jmp short loc_rename_ask_again
loc_do_not_rename_file:
                mov si, offset Y_N_nextline
		call proc_printmsg
                retn
loc_yes_rename_file:
                mov si, offset Y_N_nextline
		call proc_printmsg
               ; 19/11/2010
                mov si, offset Rename_NewName
                mov cx, word ptr [SourceFile_DirEntryNumber] 
                mov ax, word ptr [SourceFile_DirEntry]+26 ; First Cluster, LW 
                mov dx, word ptr [SourceFile_DirEntry]+20 ; First Cluster, HW 
                mov bl, byte ptr [SourceFile_LongNameEntryLength]  
                xor bh, bh
                call proc_rename_directory_entry
                jc loc_run_cmd_failed

                mov si, offset Msg_OK
                call proc_printmsg

                jmp loc_file_rw_restore_retn


;Rename_Attribute: db 0


cmp_cmd_rmdir:
               ; 05/06/2010
                mov  cx,5
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_rmdir
get_char_rmdir:
                mov  al, byte ptr [SI]
		inc  si
		scasb
               ;jne  cmp_cmd_rename
                jne cmp_cmd_attrib
                loop get_char_rmdir
                mov  al, byte ptr [SI]
		cmp  al,20h
                ja  cmp_cmd_external
get_rmdir_fchar:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je short get_rmdir_fchar
                jb short loc_rmdir_nodirname_retn

loc_rmdir_parse_path_name:
                ; 28/02/2010
                push ds
                pop es
                mov di, offset FindFile_Drv
                call proc_parse_pathname
                jc loc_cmd_failed

loc_rmdir_check_dirname_exists:
                ; 28/02/2010
                mov si, offset FindFile_Name
                cmp byte ptr [SI], 20h
                jna loc_cmd_failed
                mov word ptr [DelFile_FNPointer], si 

loc_rmdir_drv:
                mov dh, byte ptr [Current_Drv]
                mov byte ptr [RUN_CDRV], dh

                mov dl, byte ptr [FindFile_Drv]
                cmp dl, dh
                je short loc_rmdir_change_directory

                call proc_change_current_drive
                jc loc_file_rw_cmd_failed

loc_rmdir_change_directory:
                ; 05/12/2009
                cmp byte ptr [FindFile_Directory], 20h
                jna short loc_rmdir_find_directory

                ; 29/11/2009 -> Restore_CDIR
                inc byte ptr [Restore_CDIR]
                mov si, offset FindFile_Directory
                xor ah, ah ; CD_COMMAND sign -> 0 
                call proc_change_current_directory
                jc short loc_rmdir_check_error_code

;loc_rmdir_change_prompt_dir_string:
               ;call proc_change_prompt_dir_string

loc_rmdir_find_directory:
	       ;mov si, offset FindFile_Name
 		mov si, word ptr [DelFile_FNPointer]
                mov ax, 0810h ; Only directories
                call proc_find_first_file
                jnc short loc_rmdir_ambgfn_check
loc_rmdir_check_error_code:
               ; 05/06/2010
                cmp al, 2
                je short loc_rmdir_directory_not_found
                stc
                jmp loc_file_rw_cmd_failed
                 
loc_rmdir_ambgfn_check:
                cmp bx, 0 ; Ambiguous filename chars used sign (BX>0)
                jna short loc_rmdir_directory_found
loc_rmdir_directory_not_found:
               ; 05/06/2010 
                mov si, offset Msg_Dir_Not_Found
                call proc_printmsg

                jmp loc_file_rw_restore_retn

loc_rmdir_nodirname_retn:
                retn

loc_rmdir_directory_found:
                and cl, 07h ; Attributes
               ; 20/06/2010 
                jnz loc_rename_permission_denied

loc_rmdir_found_check_lnel:
               ;mov al, ch ; If the Long name is yes/existing, ch > 0 
               ;or al, al
                or ch, ch
                jz short pass_rmdir_save_lnel

loc_rmdir_save_lnel:
               ; mov al, byte ptr [LongName_EntryLength]
                mov ch, byte ptr [LongName_EntryLength]
pass_rmdir_save_lnel:
               ; mov byte ptr [DelFile_LNEL], al
                mov byte ptr [DelFile_LNEL], ch
                ; di = Directory Entry Offset (DirBuff)
                ; si = Directory Entry (FFF Structure)
               ; mov word ptr [DelFile_DirEntryAddr], di ; not required
               ; mov ax, word ptr ES:[DI]+26 ; First Cluster Low Word
               ; mov dx, word ptr ES:[DI]+20 ; First Cluster High Word
               ; cmp dx, 0
               ; ja short pass_rmdir_fc_check
               ; ROOT Dir First Cluster = 0
               ; cmp ax, 2
               ; jb loc_update_direntry_1
pass_rmdir_fc_check:
                mov si, offset Msg_DoYouWantRmDir
                call proc_printmsg
                mov si, word ptr [DelFile_FNPointer]
                call proc_printmsg
                mov si, offset Msg_YesNo
                call proc_printmsg
loc_rmdir_ask_again:
                xor ah, ah
                int 16h
                cmp al, 0Bh
                je short loc_do_not_delete_directory
                and al, 0DFh
 		mov byte ptr [Y_N_nextLine], al
                cmp al, 'Y'
                je short loc_rmdir_yes_delete_directory
                cmp al, 'N'
                jne short loc_rmdir_ask_again
loc_do_not_delete_directory:
                mov si, offset Y_N_nextline
                call proc_printmsg
               ; 23/05/2010
                jmp loc_file_rw_restore_retn

loc_rmdir_delete_fs_directory:
               ;20/06/2010
                cmp byte ptr [SI][LD_FSType], 0A1h
                jne loc_rename_permission_denied

                call proc_delete_fs_directory
                jnc loc_print_deleted_message

               ; 26/06/2010
                or ax, ax
                jz loc_rmdir_directory_not_empty_2         
                stc
                jmp loc_file_rw_cmd_failed

loc_rmdir_yes_delete_directory:
                mov si, offset Y_N_nextline
                call proc_printmsg

loc_rmdir_delete_short_name_check_dir_empty:
  	       ; ES:DI -> Directory buffer entry offset/address 
                mov ax, word ptr ES:[DI]+26 ; First Cluster Low Word
                mov dx, word ptr ES:[DI]+20 ; First Cluster High Word

                mov word ptr [DelFile_FCluster], ax
                mov word ptr [DelFile_FCluster]+2, dx

 		push word ptr [DirBuff_EntryCounter]
                pop word ptr [DelFile_EntryCounter]

               ; 20/06/2010
                xor bl, bl
		mov bh, byte ptr [FindFile_Drv]
                mov si, offset Logical_DOSDisks
                add si, bx

                cmp word ptr ES:[DI]+DirEntry_NTRes, 01A1h
                je short loc_rmdir_delete_fs_directory

               ;cmp byte ptr [SI][LD_FATtype], 1
               ;jnb short loc_rmdir_get_last_cluster
               ;mov ax, 0Bh ; Invalid Format
               ;jmp loc_file_rw_cmd_failed
  
loc_rmdir_get_last_cluster:
               ; 20/06/2010 
                push word ptr [DirBuff_Cluster]
                pop word ptr [RmDir_ParentDirCluster]

                push word ptr [DirBuff_Cluster]+2
                pop word ptr [RmDir_ParentDirCluster]+2

               ;push es
               ;pop word ptr [RmDir_DirEntrySegment]
                mov word ptr [RmDir_DirEntryOffset], di

                call proc_get_last_cluster
                jc loc_file_rw_cmd_failed
                cmp ax, word ptr [DelFile_FCluster]
                jne short loc_rmdir_multi_dir_clusters
                cmp dx, word ptr [DelFile_FCluster]+2
                jne short loc_rmdir_multi_dir_clusters
                mov byte ptr [RmDir_MultiClusters], 0
                jmp short pass_rmdir_multi_dir_clusters
 
loc_rmdir_multi_dir_clusters:
                mov byte ptr [RmDir_MultiClusters], 1
pass_rmdir_multi_dir_clusters:
                mov word ptr [RmDir_DirLastCluster], ax
                mov word ptr [RmDir_DirLastCluster]+2, dx
               ;12/06/2010 
                mov word ptr [RmDir_PreviousCluster], cx
                mov word ptr [RmDir_PreviousCluster]+2, bx
loc_rmdir_load_fat_sub_directory:
                call proc_load_FAT_sub_directory
                jc loc_file_rw_cmd_failed

loc_rmdir_find_last_dir_entry:
                push si
                mov si, offset Dir_File_Name
                mov byte ptr [SI], "*"
                mov byte ptr [SI]+8, "*"
                xor bx, bx ; Entry offset  = 0
loc_rmdir_find_last_dir_entry_next:
                mov ax, 0800h ; Except volume/long names
                xor cx, cx ; 0 = Find a valid file or dir name
                call proc_find_direntry  
                jc short loc_rmdir_empty_dir_cluster
                cmp bx, 1
                ja short loc_rmdir_directory_not_empty_1
loc_rmdir_dot_entry_check:
                cmp ch, '.' ; The first char of the dir entry
                jne short loc_rmdir_directory_not_empty_1
                or bl, bl
                jnz short pass_rmdir_dot_entry_check
                cmp byte ptr ES:[DI]+1, 20h
                jne short loc_rmdir_directory_not_empty_1
                inc bx
                jmp short loc_rmdir_find_last_dir_entry_next  

pass_rmdir_dot_entry_check:
                cmp word ptr ES:[DI]+1, ' .'
                jne short loc_rmdir_directory_not_empty_1 
                inc bx
                jmp short loc_rmdir_find_last_dir_entry_next  

loc_rmdir_directory_not_empty_1:
                pop ax ; pushed si 
loc_rmdir_directory_not_empty_2:
                mov si, offset Msg_Dir_Not_Empty
                call proc_printmsg
                jmp loc_file_rw_restore_retn

loc_rmdir_empty_dir_cluster:
                pop si

               ; 12/06/2010
loc_rmdir_set_prev_cluster_dir_last_cluster:
                cmp byte ptr [RmDir_MultiClusters], 0
                jna short loc_rmdir_unlink_dir_last_cluster

                mov ax, word ptr [RmDir_PreviousCluster]
                mov dx, word ptr [RmDir_PreviousCluster]+2
                xor cx, cx
                dec cx
               ; 13/06/2010
                mov bx,cx ; FFFFh
                call proc_update_cluster
                jnc short loc_rmdir_unlink_dir_last_cluster

loc_rmdir_unlink_stc_retn:
                cmp ax, 1
                cmc 
                jc loc_file_rw_cmd_failed
              ; 13/06/2010
                jmp short loc_rmdir_save_fat_buffer 
loc_rmdir_unlink_stc_retn_0Bh:
                mov ax, 0Bh ; Invalid format
                stc
                jmp loc_file_rw_cmd_failed
 
loc_rmdir_unlink_dir_last_cluster:
                mov ax, word ptr [RmDir_DirLastCluster]
                mov dx, word ptr [RmDir_DirLastCluster]+2
                xor cx, cx
                xor bx, bx
                call proc_update_cluster
                jnc short loc_rmdir_unlink_stc_retn_0Bh 
                ; Because of it is the last cluster
                ; uc procedure must return with eocc error 
                or ax, ax
                jz short loc_rmdir_save_fat_buffer ; eocc                
                stc
                jmp loc_file_rw_cmd_failed
                 
loc_rmdir_save_fat_buffer:
                cmp byte ptr [FAT_BuffValidData], 2
                jne short loc_rmdir_calculate_FAT_freespace
                call proc_save_fat_buffer
                jc loc_file_rw_cmd_failed

loc_rmdir_calculate_FAT_freespace:
              ; 16/10/2010
               mov byte ptr [CFS_error], 0
loc_rmdir_calculate_FAT_freespace_next:
	        mov ax, 1
                xor dx, dx
                mov bx, ax ; 1
              ;  bl=1 -> Add DX:AX to free space count
                dec bh  ; FFh = DS:SI is ready as Drive DT Offset
               ; DS:SI -> Logical Dos Drive Description Table
                call proc_calculate_FAT_freespace
               ; 16/10/2010 
                and cx, cx ; cx = 0 -> valid free sector count
                jz short pass_rmdir_recalculate_FAT_freespace

                cmp byte ptr [CFS_error], 0
                ja short loc_rmdir_calculate_FAT_freespace_err

loc_rmdir_recalculate_FAT_freespace:
              ; 16/10/2010 
               inc byte ptr [CFS_error]
               mov bx, 0FF00h
               call proc_calculate_FAT_freespace
               and cx, cx
               jz short loc_rmdir_calculate_FAT_freespace_next
              
loc_rmdir_calculate_FAT_freespace_err:
              ; 16/10/2010
pass_rmdir_recalculate_FAT_freespace:

                cmp byte ptr [RmDir_MultiClusters], 0
                jna short loc_rmdir_delete_short_name_continue

                mov ax, word ptr [DelFile_FCluster]
                mov dx, word ptr [DelFile_FCluster]+2       
              
                jmp loc_rmdir_get_last_cluster

loc_rmdir_delete_short_name_invalid_data:
                mov al, 0Dh ; Invalid data
                xor ah, ah
                stc
                jc loc_file_rw_cmd_failed
                          
loc_rmdir_delete_short_name_continue:
                mov ax, word ptr [RmDir_ParentDirCluster]
                mov dx, word ptr [RmDir_ParentDirCluster]+2
              ; 13/06/2010
                cmp ax, 2
                jnb short loc_rmdir_delete_short_name_cont_load_sub_dir
                or dx, dx
                jnz short loc_rmdir_delete_short_name_cont_load_sub_dir
                call proc_load_FAT_root_directory
                jc loc_file_rw_cmd_failed
                jmp short loc_rmdir_del_short_name_cont_ld_check_fclust

loc_rmdir_delete_short_name_cont_load_sub_dir:                
                call proc_load_FAT_sub_directory
                jc loc_file_rw_cmd_failed

loc_rmdir_del_short_name_cont_ld_check_fclust:                
                mov es, bx  
                mov di, word ptr [RmDir_DirEntryOffset]

                mov ax, word ptr ES:[DI]+26 ; First Cluster Low Word
                mov dx, word ptr ES:[DI]+20 ; First Cluster High Word
               ; 12-06-2010 Not necessary, if procedure is in solidstate 
                cmp ax, word ptr [DelFile_FCluster]
                jne short loc_rmdir_delete_short_name_invalid_data
                cmp dx, word ptr [DelFile_FCluster]+2
                jne short loc_rmdir_delete_short_name_invalid_data

                mov byte ptr ES:[DI], 0E5h ; Deleted sign
	       ;mov byte ptr [DirBuff_ValidData], 2
		call proc_save_directory_buffer
                jc loc_file_rw_cmd_failed 
               ;                    

loc_rmdir_del_long_name:
                xor dh, dh
                mov dl, byte ptr [DelFile_LNEL]
                or dl, dl
               ;16/10/2010
                jz short loc_rmdir_update_parent_dir_lmdt
             
                mov ax, word ptr [DelFile_EntryCounter]
                sub ax, dx
                jc loc_file_rw_cmd_failed
 
               ; AX = Directory Entry Number of the long name last entry
                call proc_delete_longname
               ;jc short loc_file_rw_cmd_failed

loc_rmdir_update_parent_dir_lmdt:
               ; 16/10/2010
                call proc_update_parent_dir_lmdt
               ;jc short loc_file_rw_cmd_failed

                mov si, offset Msg_OK
                call proc_printmsg
                jmp loc_file_rw_restore_retn

; 06/06/2010
RmDir_ParentDirCluster: dd 0
RmDir_DirlastCluster: dd 0
;12/06/2010
RmDir_PreviousCluster: dd 0
; 06/06/2010
;RmDir_DirEntrySegment: dw 0
RmDir_DirEntryOffset: dw 0
RmDir_MultiClusters: db 0

cmp_cmd_attrib:
                ; 23/05/2010
                ; Derivation from P2000.ASM
		; 17/12/2000
                mov cx,6
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_Attrib
get_char_attrib:
                mov al, byte ptr [SI]
                inc si
		scasb
               ; 10/07/2010 
                jne cmp_cmd_mkdir
               ; 
                loop get_char_attrib
                mov al, byte ptr [SI]
                cmp al, 20h
                ja  cmp_cmd_external
                xor ax, ax
                mov word ptr [Attr_Chars], ax
                mov byte ptr [Attributes], al
loop_scan_attrib_fparam:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                ja short loop_scan_attrib_params
                je short loop_scan_attrib_fparam
               ; 23/05/2010
loc_attr_file_nofilename_retn:
                retn
loop_scan_attrib_params:
                cmp al, '-'
                ja  loc_attr_file_parse_path_name
                je  short loc_attr_space
                cmp al, '+'
                jne loc_cmd_failed
loc_attr_space:
                mov ah, byte ptr [SI]+1 
                cmp ah, 20h
                ja  short pass_attr_space
                jb  loc_cmd_failed
loc_attr_space_inc:
                inc si
                jmp short loc_attr_space

pass_attr_space:
                and ah, 0DFh
                cmp ah, 'S'
                ja  loc_cmd_failed
                jb  short pass_attr_system
                mov ah, 04h   ; System
                jmp short pass_attr_archive
pass_attr_system:
                cmp ah, 'H'
                ja  short pass_attr_hidden
                jb  short pass_attr_read_only
                mov ah, 02h    ; Hidden
                jmp short pass_attr_archive
pass_attr_hidden:
                cmp ah, 'R'
                ja  loc_cmd_failed
                jb  short pass_attr_read_only ; Read only
                mov ah, 01h
                jmp short pass_attr_archive
pass_attr_read_only:
                cmp ah, 'A'
                jne short loc_chk_attr_enter
                mov ah, 20h    ; Archive
pass_attr_archive:
                cmp al, '-'
                jne short pass_reducing_attributes
                or  byte ptr [Attr_Chars], ah
                jmp short loc_change_attributes_inc
pass_reducing_attributes:
                or  byte ptr [Attr_Chars]+1, ah
loc_change_attributes_inc:
                inc si
                mov ah, byte ptr [SI]+1
                cmp ah, 20h
                jb  short pass_change_attr
                je  short loc_change_attributes_inc
                cmp ah, '-'
                ja  short loc_chk_next_attr_char1
                je  short loc_chk_next_attr_char0
                cmp ah, '+'
                jne short loc_chk_next_attr_char1
loc_chk_next_attr_char0:
                inc si
                mov ax, word ptr [SI]
                jmp short pass_attr_space
loc_chk_next_attr_char1:
                cmp byte ptr [SI], '-'
                ja  short pass_attr_space
                jmp short loc_attr_file_check_fname_fchar
loc_chk_attr_enter:
                cmp ah, 0Dh
                jne loc_cmd_failed
pass_change_attr:
                mov al, byte ptr [Attr_Chars]
                not al
                and byte ptr [Attributes], al
                mov al, byte ptr [Attr_Chars]+1
                or  byte ptr [Attributes], al
loc_show_attributes:
		mov si, offset nextline
		call proc_printmsg
loc_show_attributes_no_nextline:
                mov word ptr [Attr_Chars], 'ON'
                mov word ptr [Attr_Chars]+2, 'MR'
                mov word ptr [Attr_Chars]+4, 'LA'
                mov si, offset Attr_Chars
                mov al, byte ptr [Attributes]
                test al, 04h
                jz short pass_put_attr_s
                mov word ptr [SI], 0053h     ; S
                inc si
pass_put_attr_s:
                test al, 02h
                jz short pass_put_attr_h
                mov word ptr [SI], 0048h     ; H
                inc si
pass_put_attr_h:
                test al, 01h
                jz short pass_put_attr_r
                mov word ptr [SI], 0052h     ; R
                inc si
pass_put_attr_r:
                cmp al, 20h
                jb short pass_put_attr_a
                mov word ptr [SI], 0041h     ; A
pass_put_attr_a:
                mov si, offset Str_Attributes
		call proc_printmsg
		mov si, offset nextline
		call proc_printmsg
                jmp loc_file_rw_restore_retn 

loc_attr_file_check_fname_fchar:
                inc si
                cmp byte ptr [SI], 20h
                je short loc_attr_file_check_fname_fchar
                jb short pass_change_attr
                                   
loc_attr_file_parse_path_name:
                ; 28/02/2010
                push ds
                pop es
                mov di, offset FindFile_Drv
                call proc_parse_pathname
                jc loc_cmd_failed

loc_attr_file_check_filename_exists:
                ; 28/02/2010
                mov si, offset FindFile_Name
                cmp byte ptr [SI], 20h
                jna loc_cmd_failed
                mov word ptr [DelFile_FNPointer], si 

loc_attr_file_drv:
                mov dh, byte ptr [Current_Drv]
                mov byte ptr [RUN_CDRV], dh

                mov dl, byte ptr [FindFile_Drv]
                cmp dl, dh
                je short loc_attr_file_change_directory

                call proc_change_current_drive
                jc loc_file_rw_cmd_failed

loc_attr_file_change_directory:
                ; 05/12/2009
                cmp byte ptr [FindFile_Directory], 20h
                jna short loc_attr_file_find

                ; 29/11/2009 -> Restore_CDIR
                inc byte ptr [Restore_CDIR]
                mov si, offset FindFile_Directory
                xor ah, ah ; CD_COMMAND sign -> 0 
                call proc_change_current_directory
                jc loc_file_rw_cmd_failed

;loc_attr_file_change_prompt_dir_string:
               ;call proc_change_prompt_dir_string

loc_attr_file_find:
	       ;mov si, offset FindFile_Name
 		mov si, word ptr [DelFile_FNPointer]
                mov ax, 0800h ; Except volume labels
                call proc_find_first_file
                jc loc_file_rw_cmd_failed

loc_attr_file_ambgfn_check:
                cmp bx, 0 ; Ambiguous filename chars used sign (BX>0)
                jna short loc_attr_file_found
                mov al, 2 ; File not found sign
                stc
                jmp loc_file_rw_cmd_failed   

loc_attr_file_found:
               ; 23/05/2010
  	       ; ES:DI -> Directory buffer entry offset/address
               ; CL = File (or Directory) Attributes
               ; mov cl, byte ptr ES:[DI]+0Bh
                
                cmp word ptr [Attr_Chars], 0
                ja short loc_attr_file_change_attributes
                mov byte ptr [Attributes], cl
                jmp loc_show_attributes

loc_attr_file_change_attributes:
               ; 23/05/2010 
               ; 17/12/2000
                mov al, byte ptr [Attr_Chars]
                not al
                and cl, al
                mov al, byte ptr [Attr_Chars]+1
                or  cl, al

               ; 23/05/2010
                cmp word ptr ES:[DI]+DirEntry_NTRes, 01A1h ; Singlix FS
                je short loc_attr_file_fs_check

                mov byte ptr [Attributes], cl
                mov byte ptr ES:[DI]+0Bh, cl    ; Attributes

		;mov byte ptr [DirBuff_ValidData], 2

                ; 18/04/2010 
		call proc_save_directory_buffer
                jc loc_file_rw_cmd_failed

                jmp short loc_print_attr_changed_message

loc_attr_file_fs_check:
               ;23/05/2010
 	        mov bh, byte ptr [DirBuff_Drv]
                sub bh, 'A'
                xor bl, bl
                mov si, offset Logical_DosDisks
                add si, bx
                cmp byte ptr [SI][LD_FStype], 0A1h
                jnc short loc_attr_file_change_fs_file_attributes
                mov ax, 0Dh ; Invalid Data
                jmp loc_file_rw_cmd_failed
loc_attr_file_change_fs_file_attributes:
                ; cl = New MS-DOS File Attributes
                mov al, cl ; File/Directory Attributes
                xor ah, ah ; Attributes in MS-DOS format                  
                call proc_change_fs_file_attributes
                jc loc_file_rw_cmd_failed

                mov byte ptr [Attributes], cl 

loc_print_attr_changed_message:
               ; 23/05/2010
                mov si, offset Msg_New
                call proc_printmsg
 
                jmp loc_show_attributes_no_nextline

cmp_cmd_mkdir:
               ; 12/03/2011
               ; 10/07/2010
                mov  cx,5
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_mkdir
get_char_mkdir:
                mov  al, byte ptr [SI]
		inc  si
		scasb
                jne cmp_cmd_copy
                loop get_char_mkdir
                mov  al, byte ptr [SI]
		cmp  al,20h
                ja  cmp_cmd_external
get_mkdir_fchar:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je short get_mkdir_fchar
                jb short loc_mkdir_nodirname_retn

loc_mkdir_parse_path_name:
                ; 28/02/2010
                push ds
                pop es
                mov di, offset FindFile_Drv
                call proc_parse_pathname
                jc loc_cmd_failed

loc_mkdir_check_dirname_exists:
                mov si, offset FindFile_Name
                cmp byte ptr [SI], 20h
                jna loc_cmd_failed
                mov word ptr [DelFile_FNPointer], si
                call proc_check_filename
                jc short loc_mkdir_invalid_dir_name_chars

loc_mkdir_drv:
                mov dh, byte ptr [Current_Drv]
                mov byte ptr [RUN_CDRV], dh

                mov dl, byte ptr [FindFile_Drv]
                cmp dl, dh
                je short loc_mkdir_change_directory

                call proc_change_current_drive
                jc loc_file_rw_cmd_failed

loc_mkdir_change_directory:
                ; 05/12/2009
                cmp byte ptr [FindFile_Directory], 20h
                jna short loc_mkdir_find_directory

                ; 29/11/2009 -> Restore_CDIR
                inc byte ptr [Restore_CDIR]
                mov si, offset FindFile_Directory
                xor ah, ah ; CD_COMMAND sign -> 0 
                call proc_change_current_directory
                jc short loc_mkdir_check_error_code

;loc_mkdir_change_prompt_dir_string:
               ;call proc_change_prompt_dir_string

loc_mkdir_find_directory:
	       ;mov si, offset FindFile_Name
 		mov si, word ptr [DelFile_FNPointer]
                xor ax, ax ; any name (dir, file, volume)
                call proc_find_first_file
                jc short loc_mkdir_check_error_code

loc_mkdir_directory_found:
               ; 10/07/2010 
                mov si, offset Msg_Name_Exists
                call proc_printmsg

                jmp loc_file_rw_restore_retn

loc_mkdir_invalid_dir_name_chars:
               ; 11/07/2010 
                mov si, offset Msg_invalid_name_chars
                call proc_printmsg

                jmp loc_file_rw_restore_retn

loc_mkdir_check_error_code:
                cmp al, 2
               ;je short loc_mkdir_directory_not_found
                je short loc_mkdir_ask_for_yes_no
                stc
                jmp loc_file_rw_cmd_failed

loc_mkdir_nodirname_retn:
                retn

loc_mkdir_directory_not_found:
loc_mkdir_ask_for_yes_no:
                mov si, offset Msg_DoYouWantMkdir
                call proc_printmsg
                mov si, word ptr [DelFile_FNPointer]
                call proc_printmsg
                mov si, offset Msg_YesNo
                call proc_printmsg
loc_mkdir_ask_again:
                xor ah, ah
                int 16h
                cmp al, 0Bh
                je short loc_do_not_make_directory
                and al, 0DFh
 		mov byte ptr [Y_N_nextLine], al
                cmp al, 'Y'
                je short loc_mkdir_yes_make_directory
                cmp al, 'N'
                jne short loc_mkdir_ask_again
loc_do_not_make_directory:
                mov si, offset Y_N_nextline
                call proc_printmsg
                jmp loc_file_rw_restore_retn

loc_mkdir_yes_make_directory:
                mov si, offset Y_N_nextline
                call proc_printmsg

loc_mkdir_call_proc_make_directory:
               ; 14/08/2010
                mov si, word ptr [DelFile_FNPointer]
                mov cl, 10h ; Directory attributes (12/03/2011) 
                call proc_make_directory
                jc loc_file_rw_cmd_failed

                mov si, offset Msg_OK
                call proc_printmsg
                jmp loc_file_rw_restore_retn

cmp_cmd_copy:
               ; 21/05/2011
               ; 19/05/2011 Major Modification
               ; 23/04/2011 BugFix
                mov  cx,4
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_Copy
get_char_copy:
                mov  al, byte ptr [SI]
		inc  si
		scasb
                jne  cmp_cmd_move
                loop get_char_copy
                mov  al, byte ptr [SI]
		cmp  al, 20h
                ja  cmp_cmd_external
loop_scan_copy_source_1:
		inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je short loop_scan_copy_source_1
                jb short loc_copy_nofilename_retn
                mov word ptr [SourceFilePath], SI
loop_scan_copy_source_2:
                inc si  
                mov al, byte ptr [SI]
                cmp al, 20h
                je short loc_scan_copy_destination
               ; jb short loc_copy_nofilename_retn
                jb loc_cmd_failed
                jmp short loop_scan_copy_source_2

loc_copy_nofilename_retn:
                retn

loc_scan_copy_destination:
                mov byte ptr [SI], 0
loop_scan_copy_destination_1:
                inc si  
                mov al, byte ptr [SI]
                cmp al, 20h
                je short loop_scan_copy_destination_1
               ;jb short loc_copy_nofilename_retn
                jb loc_cmd_failed
                mov word ptr [DestinationFilePath], SI
loop_scan_copy_destination_2:
                inc si  
                mov al, byte ptr [SI]
                cmp al, 20h
                ja short loop_scan_copy_destination_2
                mov byte ptr [SI], 0
                
                push ds
                pop es

loc_copy_save_current_drive:
                mov dh, byte ptr [Current_Drv]
                mov byte ptr [RUN_CDRV], dh

loc_copy_source_file_to_destination_phase_1:
              ; 19/05/2011
                mov si, word ptr [SourceFilePath]
                mov di, word ptr [DestinationFilePath]

                mov al, 1  ; copy procedure Phase 1
                call copy_source_file_to_destination_file
                jnc short loc_copy_source_file_to_destination_question

loc_copy_cmd_failed_1:
               ; 21/05/2011
                or al, al
                jz loc_cmd_failed

              ; 11/07/2011
                cmp al, 27h ; Insufficient disk space 
                je loc_file_write_insuff_disk_space_msg
              ;  
                cmp al, 05h
                jne loc_run_cmd_failed

 		mov si, offset Msg_Permission_denied
		call proc_printmsg

                jmp loc_file_rw_restore_retn

loc_copy_source_file_to_destination_question:
               ; 19/05/2011
               ; dh = source file attributes
               ; dl > 0 -> destination file found
                and dl, dl            
                jz short loc_copy_source_file_to_destination_pass_owrq

loc_copy_ask_for_owr_yes_no:
                mov si, offset Msg_DoYouWantOverWriteFile
                call proc_printmsg
                mov si, offset DestinationFile_Name
                call proc_printmsg
                mov si, offset Msg_YesNo
                call proc_printmsg
loc_copy_ask_for_owr_again:
                xor ah, ah
                int 16h
                cmp al, 0Bh
                je short loc_do_not_copy_file
                and al, 0DFh
 		mov byte ptr [Y_N_nextLine], al
                cmp al, 'Y'
                je short loc_copy_yes_copy_file
                cmp al, 'N'
                je short loc_do_not_copy_file
                jmp short loc_copy_ask_for_owr_again

loc_copy_source_file_to_destination_pass_owrq:
		mov al, byte ptr [SourceFile_drv]
		add al, 'A'
		mov byte ptr [msg_source_file_drv], al
		mov al, byte ptr [DestinationFile_drv]
		add al, 'A'
		mov byte ptr [msg_destination_file_drv], al

		mov si, offset msg_source_file
		call proc_printmsg
		mov si, offset SourceFile_Directory
		call proc_printmsg
		mov si, offset SourceFile_Name
		call proc_printmsg
		mov si, offset msg_destination_file
		call proc_printmsg
		mov si, offset DestinationFile_Directory
		call proc_printmsg
		mov si, offset DestinationFile_Name
		call proc_printmsg
		mov si, offset msg_copy_nextline
		call proc_printmsg
		mov si, offset msg_copy_nextline
		call proc_printmsg

                or dl, dl ; byte ptr [DestinationFileFound]
                jnz short loc_copy_source_file_to_destination_pass_q

loc_copy_ask_for_new_file_yes_no:
                mov si, offset Msg_DoYouWantCopyFile
                call proc_printmsg
                mov si, offset Msg_YesNo
                call proc_printmsg
loc_copy_ask_for_new_file_again:
                xor ah, ah
                int 16h
                cmp al, 0Bh
                je short loc_do_not_copy_file
                and al, 0DFh
 		mov byte ptr [Y_N_nextLine], al
                cmp al, 'Y'
                je short loc_copy_yes_copy_file
                cmp al, 'N'
                jne short loc_copy_ask_for_new_file_again
loc_do_not_copy_file:
                mov si, offset Y_N_nextline
                call proc_printmsg
                jmp loc_file_rw_restore_retn

loc_copy_yes_copy_file:
                mov si, offset Y_N_nextline
                call proc_printmsg

loc_copy_source_file_to_destination_pass_q:
                mov al, 2  ; copy procedure Phase 1
                call copy_source_file_to_destination_file
                jc short loc_file_write_check_disk_space_err

               ; 10/07/2011
                or cl, cl
                jz short loc_copy_source_file_to_destination_OK
                
loc_copy_source_file_to_destination_not_OK:
               ; 10/07/2011
                mov si, offset msg_read_file_error_before_EOF
                call proc_printmsg
                jmp loc_file_rw_restore_retn                      
 
loc_copy_source_file_to_destination_OK:
                mov si, offset Msg_OK
                call proc_printmsg

                jmp loc_file_rw_restore_retn

loc_file_write_check_disk_space_err:
                cmp al, 27h ; Insufficient disk space 
                je short loc_file_write_insuff_disk_space_msg
                jb loc_file_rw_cmd_failed
                call loc_run_misc_error
                jmp loc_file_rw_restore_retn

loc_file_write_insuff_disk_space_msg:
                mov si, offset msg_insufficient_disk_space
                call proc_printmsg
                jmp loc_file_rw_restore_retn      

; 01/08/2010
SourceFilePath: dw 0
DestinationFilePath: dw 0


cmp_cmd_move:
               ; 21/05/2011
               ; 15/05/2011
               ; 23/04/2011
                mov  cx,4
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_move
get_char_move:
                mov  al, byte ptr [SI]
		inc  si
		scasb
                jne  cmp_cmd_set ; 28/08/2011
                loop get_char_move
                mov  al, byte ptr [SI]
		cmp  al, 20h
                ja  cmp_cmd_external
loop_scan_move_source_1:
		inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je short loop_scan_move_source_1
                jb short loc_move_nofilename_retn
                mov word ptr [SourceFilePath], SI
loop_scan_move_source_2:
                inc si  
                mov al, byte ptr [SI]
                cmp al, 20h
                je short loc_scan_move_destination
               ; jb short loc_move_nofilename_retn
                jb loc_cmd_failed
                jmp short loop_scan_move_source_2

loc_move_nofilename_retn:
               ; 15/05/2011
                retn

loc_scan_move_destination:
                mov byte ptr [SI], 0
loop_scan_move_destination_1:
                inc si  
                mov al, byte ptr [SI]
                cmp al, 20h
                je short loop_scan_move_destination_1
               ;jb short loc_move_nofilename_retn
                jb loc_cmd_failed
                mov word ptr [DestinationFilePath], SI

loop_scan_move_destination_2:
                inc si  
                mov al, byte ptr [SI]
                cmp al, 20h
                ja short loop_scan_move_destination_2
                mov byte ptr [SI], 0

loc_scan_move_destination_OK:
              ; 15/05/2011   
                mov si, word ptr [SourceFilePath]
                mov di, word ptr [DestinationFilePath]

                mov al, 1  ; move procedure Phase 1
                call move_source_file_to_destination_file
                jnc short loc_move_source_file_to_destination_question

loc_move_cmd_failed_1:
               ; 21/05/2011
                or al, al
                jz loc_cmd_failed 
               ; 18/05/2011
                cmp al, 11h
                je short loc_msg_not_same_device   
               ; 15/05/2011 
                cmp al, 05h
                jne loc_run_cmd_failed

 		mov si, offset Msg_Permission_denied
		call proc_printmsg

                jmp loc_file_rw_restore_retn

loc_msg_not_same_device:
               ;18/05/2011
                mov si, offset msg_not_same_drv 
                call proc_printmsg
                jmp loc_file_rw_restore_retn

loc_move_source_file_to_destination_question:
             	mov al, byte ptr [SourceFile_drv]
		add al, 'A'
		mov byte ptr [msg_source_file_drv], al
		mov al, byte ptr [DestinationFile_drv]
		add al, 'A'
		mov byte ptr [msg_destination_file_drv], al

		mov si, offset msg_source_file
		call proc_printmsg
		mov si, offset SourceFile_Directory
		call proc_printmsg
		mov si, offset SourceFile_Name
		call proc_printmsg
		mov si, offset msg_destination_file
		call proc_printmsg
		mov si, offset DestinationFile_Directory
		call proc_printmsg
		mov si, offset DestinationFile_Name
		call proc_printmsg
		mov si, offset msg_copy_nextline
		call proc_printmsg
		mov si, offset msg_copy_nextline
		call proc_printmsg

loc_move_ask_for_new_file_yes_no:
                mov si, offset Msg_DoYouWantMoveFile
                call proc_printmsg
                mov si, offset Msg_YesNo
                call proc_printmsg
loc_move_ask_for_new_file_again:
                xor ah, ah
                int 16h
                cmp al, 0Bh
                je short loc_do_not_move_file
                and al, 0DFh
 		mov byte ptr [Y_N_nextLine], al
                cmp al, 'Y'
                je short loc_move_yes_move_file
                cmp al, 'N'
                jne short loc_move_ask_for_new_file_again
loc_do_not_move_file:
                mov si, offset Y_N_nextline
                call proc_printmsg
                jmp loc_file_rw_restore_retn

loc_move_yes_move_file:
                mov si, offset Y_N_nextline
                call proc_printmsg

loc_move_source_file_to_destination_pass_q:
               ; 15/05/2011
                mov al, 2 ; move procedure Phase 2
                call move_source_file_to_destination_file
               ; jnc short loc_move_source_file_to_destination_OK

                jc short loc_move_cmd_failed_2

loc_move_source_file_to_destination_OK:
                mov si, offset Msg_OK
               ; jmp short loc_move_source_file_to_destination_retn

                call proc_printmsg

                jmp loc_file_rw_restore_retn

loc_move_cmd_failed_2:
               ; 15/05/2011
                cmp al, 27h
                jne loc_run_cmd_failed

                mov si, offset msg_insufficient_disk_space
                call proc_printmsg

                jmp loc_file_rw_restore_retn

cmp_cmd_set:
               ; 02/09/2011
               ;2005 - 28/08/2011 
                mov  cx,3
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_Set
get_char_set:
                mov  al, byte ptr [SI]
		inc  si
		scasb
                jne  cmp_cmd_path
                loop get_char_set
                mov  al, byte ptr [SI]
		cmp  al,20h
                ja  cmp_cmd_external
get_setenv_fchar:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je short get_setenv_fchar
                ja short loc_find_env
               ; 29/08/2011
                mov si, word ptr [SetEnv_Segment]
                or si, si
                jz short loc_cmd_set_retn

                mov ds, si
                xor si, si  
loc_print_setline:
                cmp byte ptr [SI],0
                jna short pass_print_setline
                call proc_printmsg
                push ds
                push si
                push cs
                pop ds
                mov si, offset NextLine
                call proc_printmsg 
                pop si
                pop ds
                jmp short loc_print_setline   

pass_print_setline:
                push cs
                pop ds

loc_cmd_set_retn: 
                retn

loc_find_env:
                cmp al, "="
                je loc_cmd_failed
                push si
loc_repeat_env_equal_check:
                inc si
                cmp byte ptr [SI],"="
                je short pass_env_equal_check
                cmp byte ptr [SI], 20h
                jnb short loc_repeat_env_equal_check
                mov byte ptr [SI], 0 
                pop si
               ;push cs
               ;pop es
                mov di, offset textbuffer
                xor al, al
                call get_environment_string
                jc short loc_cmd_set_retn

                mov si, offset textbuffer
                call proc_printmsg
                mov si, offset NextLine
                call proc_printmsg
                
                retn 
              
pass_env_equal_check:
               ; 29/08/2011
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                jnb short pass_env_equal_check
                xor al, al
                mov byte ptr [SI], al ; 0                

loc_call_set_env_string:
                pop si
                call set_environment_string
                jnc short loc_cmd_set_retn

loc_set_cmd_failed:
               ; 29/08/2011
                cmp al, 08h
                jne loc_cmd_failed

                mov si, offset Msg_No_Set_Space
                call proc_printmsg

                retn              

cmp_cmd_path:
               ; 03/09/2011
                mov  cx,4
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_Path
get_char_path:
                mov  al, byte ptr [SI]
		inc  si
		scasb
                jne  cmp_cmd_echo
                loop get_char_path
                mov  al, byte ptr [SI]
		cmp  al,20h
                ja   cmp_cmd_external
get_path_fchar:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je short get_path_fchar
                ja short loc_set_path
                cmp word ptr [SetEnv_Segment], 0
                jna short loc_cmd_path_retn

loc_print_path:
               ; push cs
               ; pop es 
                mov si, offset Cmd_Path 
                mov di, offset textbuffer
                xor al, al
                call get_environment_string
                jc short loc_cmd_path_retn

                mov si, offset textbuffer
                call proc_printmsg
                mov si, offset NextLine
                call proc_printmsg   

loc_cmd_path_retn: 
                retn

loc_set_path:
                push si 
loc_set_path_find_end:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                jnb short loc_set_path_find_end
                xor al, al
                mov byte ptr [SI], al ; 0 
loc_set_path_header: 
                pop si                  
                dec si
                mov byte ptr [SI], "="
                dec si
                mov byte ptr [SI], "H"
                dec si
                mov byte ptr [SI], "T"
                dec si
                mov byte ptr [SI], "A"
                dec si
                mov byte ptr [SI], "P"   

loc_path_call_set_env_string:
                call set_environment_string
                jc short loc_set_cmd_failed

                retn

cmp_cmd_echo:
               ; 18/09/2011
                mov  cx,4
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_Echo
get_char_echo:
                mov  al, byte ptr [SI]
		inc  si
		scasb
                jne  cmp_cmd_device
                loop get_char_echo
                mov  al, byte ptr [SI]
		cmp  al,20h
                ja   cmp_cmd_external
get_echo_string:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je short get_echo_string
                jb short loc_cmd_echo_retn
loc_print_echo_string:
                push si
get_end_of_echo_string:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                jnb short get_end_of_echo_string
                mov byte ptr [SI], 0                 
                pop si  
                call proc_printmsg
                mov si, offset NextLine
                call proc_printmsg   
loc_cmd_echo_retn:
loc_cmd_device_retn:
                retn

cmp_cmd_device:
               ;23/10/2011
                mov  cx,6
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_Device
get_char_device:
                mov  al, byte ptr [SI]
		inc  si
		scasb
                jne cmp_cmd_devlist
                loop get_char_device
                mov  al, byte ptr [SI]
		cmp  al, 20h
                jna  short get_device_fchar
                cmp  al, "."
                je   cmp_cmd_external
                jmp  loc_cmd_failed
get_device_fchar:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je short get_device_fchar
               ;06/11/2011
                jb short loc_cmd_device_retn

loc_device_move_command_tail:
                ;06/11/2011
                ;push ds
                ;pop es
                push si
                mov di, offset TextBuffer
loc_device_move_command_tail_next:
                stosb
                lodsb
                cmp al, 20h
                jnb short loc_device_move_command_tail_next  
                 
loc_device_move_command_tail_ok:
                xor al, al 
                stosb
                pop si   

loc_device_parse_path_name:
                mov di, offset FindFile_Drv
                call proc_parse_pathname
                jc  loc_cmd_failed

loc_device_check_filename_exists:
                mov si, offset FindFile_Name
                cmp byte ptr [SI], 20h
                jna loc_cmd_failed 

                mov dh, byte ptr [Current_Drv]
                mov byte ptr [RUN_CDRV], dh

loc_device_change_drv:
                mov dl, byte ptr [FindFile_Drv]
                cmp dl, dh
                je short loc_device_change_directory

                call proc_change_current_drive
                jc loc_file_rw_cmd_failed

loc_device_change_directory:
                cmp byte ptr [FindFile_Directory], 20h
                jna short loc_device_drv_file_load

                inc byte ptr [Restore_CDIR]
                mov si, offset FindFile_Directory
                xor ah, ah ; CD_COMMAND sign -> 0 
                call proc_change_current_directory
                jc loc_file_rw_cmd_failed

;loc_device_change_prompt_dir_string:
               ;call proc_change_prompt_dir_string

loc_device_drv_file_load:
                mov si, offset FindFile_Name
                call proc_find_load_device_file
                jc loc_file_rw_cmd_failed
               ; 05/11/2011 
                or ax, ax
                jnz loc_file_rw_restore_retn

loc_device_drv_initialization:
                call proc_device_init
               ; 05/11/2011 
                jmp loc_file_rw_restore_retn

cmp_cmd_devlist:
               ;30/10/2011
                mov  cx,7
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_DevList
get_char_devlist:
                mov  al, byte ptr [SI]
		inc  si
		scasb
                jne cmp_cmd_find
                loop get_char_devlist
                mov  al, byte ptr [SI]
		cmp  al, 20h
                jna  short loc_print_device_list
                cmp  al, "."
                je   cmp_cmd_external
                jmp  loc_cmd_failed

loc_print_device_list:
                call proc_print_device_list

                retn

cmp_cmd_external:
              ; 12/09/2011
              ; 11/09/2011
              ; 10/09/2011
                mov si, offset CommandBuffer
                mov ah, 0FFh ; external sign
                jmp short loc_run_check_filename 
cmp_cmd_run:
              ; 05/11/2011 -> DOS interrupt reset control
              ;  If any device driver files are loaded before,
              ;  dos interrupt reset would be wrong because
              ;  device driver may change interrupt vectors
              ;  for it's own functionality; also, open/loaded
              ;  device driver files never will be closed 
              ;  by TRDOS.
              ; 11/09/2011 
              ; 10/09/2011 external command/programm
              ;            and path modifications 
                mov  si, offset CommandBuffer
               ;mov  di, offset Cmd_Run
get_char_run:
                mov  ax, word ptr [SI]
                cmp  ax, "UR"
                jne  short cmp_cmd_external
                mov  ax, word ptr [SI]+2
		cmp  al, "N"
		jne  short cmp_cmd_external
                cmp  ah, 20h
                jna  short loc_run_command_chars_ok
               ; 12/09/2011
                push si
                add si, 4
                mov ah, 0FFh
                jmp short loc_run_get_next_filename_char
loc_run_command_chars_ok:                
                add  si, 4
loc_run_check_filename:
                mov al, byte ptr [SI]
                cmp al, 20h
                jb loc_cmd_failed
                ja short loc_run_check_filename_ok
                inc si
                jmp short short loc_run_check_filename

loc_run_check_filename_ok:
               ; 11/09/2011                 
                push si
loc_run_get_first_arg_pos:
                inc si
loc_run_get_next_filename_char:
                mov al, byte ptr [SI]
                cmp al, 20h
                ja short loc_run_get_first_arg_pos
                je short loc_run_get_external_arg_pos
                cmp ah, 0FFh ; external command/file
                jb short loc_run_filename_no_args
loc_run_get_external_arg_pos:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                ja short loc_run_get_first_arg_pos_ok
                je short loc_run_get_external_arg_pos 
loc_run_filename_no_args:
                or al, al
                jz short loc_run_get_first_arg_pos_ok 
                mov byte ptr [SI], 0
loc_run_get_first_arg_pos_ok:
                mov ax, si
                sub ax, offset CommandBuffer
                pop si
                mov byte ptr [CmdArgStart], al                                   

loc_run_parse_path_name:
                ; 29/11/2009
                push ds
                pop es
                mov di, offset FindFile_Drv
                call proc_parse_pathname
                jc loc_cmd_failed

loc_run_check_filename_exists:
                ; 06/12/2009
                mov si, offset FindFile_Name
                cmp byte ptr [SI], 20h
                jna loc_cmd_failed

                ; 06/12/2009 
loc_run_check_exe_filename_ext:
                ; 14/11/2009
                call proc_check_exe_filename_ext
               ; 10/09/2011
                jnc short loc_run_check_exe_filename_ext_ok
                cmp ah, 8
                ja loc_cmd_failed

loc_run_change_file_ext_to_com: 
              ; 10/09/2011               
                mov byte ptr [SI], "."
                inc si
                mov al, "C"                 
                mov byte ptr [SI], al
                inc si                 
                mov word ptr [SI], "MO"

loc_run_check_exe_filename_ext_ok:
                mov word ptr [EXE_ID], ax

loc_run_drv:
               ; 10/09/2011
                mov byte ptr [RUN_Manual_Path], 0
 		mov ax, word ptr [Current_Dir_FCluster]
     		mov dx, word ptr [Current_Dir_FCluster]+2
                mov word ptr [Run_CDIRFC], ax
                mov word ptr [Run_CDIRFC]+2, dx  
                ;
                mov dh, byte ptr [Current_Drv]
                mov byte ptr [RUN_CDRV], dh

                mov dl, byte ptr [FindFile_Drv]
                cmp dl, dh
                je short loc_run_change_directory
               
               ; 10/09/2011 
                add byte ptr [Run_Manual_path], 2
               ; 
                call proc_change_current_drive
                jc loc_run_cmd_failed

loc_run_change_directory:
                ; 05/12/2009
                cmp byte ptr [FindFile_Directory], 20h
                jna short loc_load_executable_file

               ; 10/09/2011
                inc byte ptr [Run_Manual_Path]
               ; 
                ; 29/11/2009 -> Restore_CDIR
                inc byte ptr [Restore_CDIR]
                mov si, offset FindFile_Directory
                xor ah, ah ; CD_COMMAND sign -> 0 
                call proc_change_current_directory
                jc loc_run_cmd_failed

loc_run_change_prompt_dir_string:
                call proc_change_prompt_dir_string

loc_load_executable_file:
                mov byte ptr [RUN_Auto_Path], 0
loc_load_executable_file_next:
                mov si, offset FindFile_Name
                mov ax, 04h
                call proc_open_file
                ; ES:BX = File size pointer (ES=CS)
                jnc loc_run_loaded_executable_file
                 
                ; 10/09/20111
                cmp al, 2 ; file not found
                jne loc_run_cmd_failed

               ; 11/09/2011
                mov ax, word ptr [EXE_ID]
                cmp ah, "." ; File name has extension sign
                je short loc_run_check_auto_path

loc_run_chk_ext_bfr_chg_to_exe:
                cmp al, "E"
               ;je loc change loc_run_change_file_ext_to_bat
                je short loc_run_check_auto_path
               ;cmp al, "C"
               ;jne short loc_run_check_auto_path ; ".BAT"

loc_run_change_file_ext_to_exe:
               ; 10/09/2011
                mov bx, offset FindFile_Name
                mov si, bx
                add bl, ah 
               ;mov byte ptr [BX], "."
                mov ax, "EX"
                inc bx
                mov byte ptr [BX], ah  
                inc bx
                mov word ptr [BX], ax
               ; 11/09/2011
                mov byte ptr [EXE_ID], ah ; "E"   
             	jmp short loc_load_executable_file_next

loc_run_change_file_ext_to_exe_ok:
                mov word ptr [EXE_ID], ax

loc_run_check_auto_path:
               ; 10/09/2011 
                mov al, byte ptr [RUN_Manual_Path]
                or al, al
                jnz loc_cmd_failed

loc_run_check_auto_path_again:
                mov ah, byte ptr [RUN_Auto_Path]                                 
                cmp ah, 0FFh ; Not a valid run path (in ENV block) 
                jnb loc_cmd_failed
                ; xor al, al 
                mov si, Offset Cmd_Path
                push ds ; 11/09/2011
                pop es ; 11/09/2011
                mov di, offset TextBuffer
                call get_environment_string
                jnc short loc_run_chk_filename_ext_again
                mov byte ptr [RUN_Auto_Path], 0FFh
                ;push ds
                ;pop es  
                jmp loc_cmd_failed

loc_run_chk_filename_ext_again:
               ; 11/09/2011
                mov cx, ax ; string length (except zero tail)
                mov ax, word ptr [EXE_ID]
                cmp ah, "."
                je short loc_run_chk_auto_path_pos
                 
loc_run_change_file_ext_to_com_again:
              ; 11/09/2011               
                mov bx, offset FindFile_Name
                mov si, bx
                add bl, ah 
               ;mov byte ptr [BX], "."
                inc bx
                mov al, "C"                 
                mov byte ptr [BX], al
                inc bx                
                mov word ptr [BX], "MO"

loc_run_change_filename_ext_to_com_again_ok:
                mov byte ptr [EXE_ID], al
            
loc_run_chk_auto_path_pos:
                mov ax, word ptr [RUN_Auto_Path]
                   ;ch = byte ptr [RUN_Manual_Path] = 0
                cmp ax, cx ; cx = string length (except zero tail)
                jnb loc_cmd_failed
                or al, al
                jnz short loc_run_auto_path_pos_move
                mov al, 5
loc_run_auto_path_pos_move:
                mov si, di ; offset TextBuffer
                add si, ax
loc_run_auto_path_pos_space_loop:
                lodsb
                cmp al, 20h 
                je short loc_run_auto_path_pos_space_loop
                jb loc_cmd_failed 
                stosb
loc_run_auto_path_pos_move_next: 
                lodsb
                cmp al, ";"
                je short loc_run_auto_path_pos_move_last_byte
                cmp al, 20h
                je short loc_run_auto_path_pos_move_next
                jb short loc_byte_ptr_end_of_path
                stosb
                jmp short loc_run_auto_path_pos_move_next 

loc_byte_ptr_end_of_path: 
                mov byte ptr [RUN_Auto_Path], 0FFh ;end of path
                jmp short loc_run_auto_path_move_ok 

loc_run_auto_path_pos_move_last_byte:
                mov ax, si
                sub ax, offset TextBuffer 
                mov byte ptr [RUN_Auto_Path], al ; next path position

loc_run_auto_path_move_ok:
                dec di
                mov al, "/"
                cmp byte ptr [DI], al
                je short loc_run_auto_path_move_file_name
                inc di
                mov byte ptr [DI], al
loc_run_auto_path_move_file_name:
                inc di   
                mov si, offset FindFile_Name
loc_run_auto_path_move_fn_loop:
              ; 10/09/2011
                lodsb
                stosb
                or al, al
                jnz short loc_run_auto_path_move_fn_loop

                mov si, offset TextBuffer
                mov di, offset FindFile_Drv
                call proc_parse_pathname
                jc loc_cmd_failed

                mov dh, byte ptr [Current_Drv]
                mov dl, byte ptr [FindFile_Drv]
                cmp dl, dh
                je short loc_run_change_directory_again
               
                call proc_change_current_drive
                jc loc_run_cmd_failed

loc_run_change_directory_again:
                ; 05/12/2009
                cmp byte ptr [FindFile_Directory], 20h
                jna short loc_load_executable_cdir_chk_again

                ; 29/11/2009 -> Restore_CDIR
                inc byte ptr [Restore_CDIR]
                mov si, offset FindFile_Directory
                xor ah, ah ; CD_COMMAND sign -> 0 
                call proc_change_current_directory
                jc loc_run_cmd_failed

loc_run_chg_prompt_dir_str_again:
                call proc_change_prompt_dir_string

loc_load_executable_cdir_chk_again:
               ;11/09/2011 
               ;10/09/2011
                mov ax, word ptr [Current_Dir_FCluster]
                mov dx, word ptr [Current_Dir_FCluster]+2
                cmp ax, word ptr [RUN_CDIRFC]
                jne loc_load_executable_file_next
                cmp dx, word ptr [RUN_CDIRFC]
                jne loc_load_executable_file_next
                xor al, al
                jmp loc_run_check_auto_path_again

loc_run_cmd_failed:
                ; 07/12/2009 -> error codes are in al (they were in ah)
                ; 06/12/2009 -> Restore Current Drive
                ; 29/11/2009 -> Restore Current Directory
                push ds
                pop es
 
                push ax

                mov bh, byte ptr [RUN_CDRV] ; it is set at the beginning
                                            ; of the 'run' command.
                
                cmp bh, byte ptr [Current_Drv]
                je short loc_run_restore_cdir
                mov dl, bh
                call proc_change_current_drive 
                jmp short loc_run_err_pass_restore_cdir

loc_run_restore_cdir:
                cmp byte ptr [Restore_CDIR], 0
                jna short loc_run_err_pass_restore_cdir
 		xor bl, bl
                mov si, offset Logical_DOSDisks
                add si, bx
                call proc_restore_current_directory

loc_run_err_pass_restore_cdir:
                pop ax 

loc_run_cmd_failed_cmp_al:
                ; End of Restore_CDIR code (29/11/2009)

                cmp al, 1
                je loc_cmd_failed
loc_run_dir_not_found:
                cmp al, 3
                jne short loc_run_file_notfound_msg
                ; Path not found (MS-DOS Error Code = 3)
                mov si, offset Msg_Dir_Not_Found
                call proc_printmsg
                retn
loc_run_file_notfound_msg:
                cmp al, 2 ; File not found
                jne short loc_run_file_drv_read_err
loc_print_file_notfound_msg: 
                mov si, offset Msg_file_not_found
                call proc_printmsg
                retn
loc_run_file_drv_read_err:
               ; 16/05/2010 -> Err: 1Eh (Read fault)
                cmp al, 1Eh ; Drive not ready or read error
                je short loc_run_file_print_drv_read_err
               ;
                cmp al, 15h ; Drive not ready (or read error)
                jne short loc_run_file_toobig
loc_run_file_print_drv_read_err:
                mov si, offset Msg_Not_Ready_Read_Err
                call proc_printmsg
                retn
loc_run_file_toobig:
                cmp al, 8 ; Not enough free memory to load&run file
                jne short loc_run_misc_error
                mov si, offset Msg_insufficient_memory
                call proc_printmsg
                retn  
loc_run_misc_error:
                call proc_hex
                mov word ptr [Error_Code], ax
                
                mov si, offset Msg_Error_Code 
                call proc_printmsg 
                retn

loc_run_loaded_executable_file:
                ; 14/11/2009
                mov word ptr [Run_Segment], ax
                mov word ptr [Run_Offset], 100h
                mov word ptr [PSP_Address], ax
                ; 08/11/2011
                mov dx, word ptr ES:[BX]
                mov word ptr [Run_File_Size], dx
		mov dx, word ptr ES:[BX]+2
                mov word ptr [Run_File_Size]+2, dx

loc_run_move_command_tail:
                ; 13/01/2011 Command arguments
                mov es, ax
                xor ah, ah
                mov al, byte ptr [CmdArgStart]
                mov si, ax
                mov al, 20h
                mov di, 81h
                stosb
loc_run_move_command_tail_n1:
               ; 11/09/2011
                mov al, byte ptr [SI][CommandBuffer]
                cmp al, 20h
		jb short loc_run_move_command_tail_OK
                ja short loc_run_move_command_tail_n3 
loc_run_move_command_tail_n2:
                cmp si, 78 ; buffer limit = 78byte+0Dh+0Ah
                jnb short loc_run_move_command_tail_OK
                inc si
                mov al, byte ptr [SI][CommandBuffer]
                cmp al, 20h
		jb short loc_run_move_command_tail_OK
                je short loc_run_move_command_tail_n2
loc_run_move_command_tail_n3:
                inc ah
loc_run_move_command_tail_n4:
		inc ah
                stosb 
                cmp si, 78 ; buffer limit = 78byte+0Dh+0Ah
                jnb short loc_run_move_command_tail_OK
                inc si
                mov al, byte ptr [SI][CommandBuffer]
		cmp al, 20h
		jnb short loc_run_move_command_tail_n4
                
loc_run_move_command_tail_OK:
                mov byte ptr ES:[DI], 0
                mov byte ptr ES:[80h], ah

              ; 13/09/2011 

                xor bx, bx

                mov byte ptr [INT21h_Err_Code], bl ; 05/02/2011
                mov byte ptr [INT21h_FFF_Valid], bl ; 05/02/2011
             
                mov byte ptr [Interrupt_Number], bl ; 16/04/2011 

                ; 14/11/2009
               ;mov byte ptr [INT21h_4Bh], 0
                mov byte ptr [INT21h_4Bh], bl

 		; 05/11/2011
                cmp byte ptr [DOS_Ints_Initialized], bl ; 0
                ja  loc_run_dos_int_init_ok
                ;

                mov es, bx

                push word ptr ES:[084h]
                pop word ptr [INT21h_Offset]
                push word ptr ES:[086h]
                pop word ptr [INT21h_Segment]
                mov word ptr ES:[084h], offset trdos_int21h_routine
                push cs
                pop word ptr ES:[086h]
                ; 

                push word ptr ES:[6Ch]   ; Int 1Bh Offset
                pop word ptr [INT1Bh_Offset]
                push word ptr ES:[6Eh]   ; Int 1Bh Segment
                pop word ptr [INT1Bh_Segment]

                mov word ptr ES:[6Ch], offset trdos_control_break_routine
                                             ; INT1Bh vector, offset
                push cs
                pop word ptr ES:[6Eh]        ; INT1Bh vector, segment

                push word ptr ES:[80h]   ; Int 20h Offset
                pop word ptr [INT20h_Offset]
                push word ptr ES:[82h]   ; Int 20h Segment
                pop word ptr [INT20h_Segment]

                mov word ptr ES:[80h], offset Run_Com_INT20h_Handler
                                             ; INT20h handler, offset
                push cs
                pop word ptr ES:[82h]        ; INT20h handler, segment

               ; 24/09/2011
                push word ptr ES:[088h]
                pop word ptr [INT22h_Offset]
                push word ptr ES:[08Ah]
                pop word ptr [INT22h_Segment]
                push word ptr ES:[08Ch]
                pop word ptr [INT23h_Offset]
                push word ptr ES:[08Eh]
                pop word ptr [INT23h_Segment]
                push word ptr ES:[090h]
                pop word ptr [INT24h_Offset]
                push word ptr ES:[092h]
                pop word ptr [INT24h_Segment]
               ; 24/09/2011
                mov ax, offset Run_Com_INT20h_Handler
                push ax
                pop word ptr ES:[088h] ; Int 22h vector
                push cs
                pop word ptr ES:[08Ah]
                push ax
                pop word ptr ES:[08Ch] ; Int 23h vector
                push cs
                pop word ptr ES:[08Eh]
                push ax
                pop word ptr ES:[090h] ; Int 24h vector
                push cs
                pop word ptr ES:[092h]
               ;
               ; 24/09/2011
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
               ; 24/09/2011
                mov cx, 24 ; INT 28h to INT 3Fh
                mov di, 0A0h ; int 28h
loop_set_inv_fc_vectors:
               ; 17/04/2011
                mov ax, offset trdos_ifc_handler
                stosw
                mov ax, cs
                stosw
                loop loop_set_inv_fc_vectors
               ;

loc_run_dos_int_init_ok:
                mov word ptr [StackSegment], ss
                mov word ptr [StackPointer], sp
                mov word ptr [BasePointer], bp

loc_run_loaded_executable_file_INT21h_4Bh:
                cmp byte ptr [EXE_ID], 'E'
                jne short far_jump_to_com_file
loc_relocate_exe_items:
                ; 2005
                ; This code written by Erdogan Tan
                ; by using "Dissecting DOS" book by
                ; Michael Podanoffsky (Rx_DOS)
                ; and source code by
                ; Alexei A. Frounze (OS for Dummies Loader)
                ; 
                mov ax, word ptr [Run_Segment]
                add ax, 10h
                mov es, ax
                add ax, word ptr ES:[08h] ;Size of header, in paragraphs
                mov cx, word ptr ES:[06h] ;Number of relocation items
                jcxz short exe_hdr_relocation_done ; 05/11/2011
                mov bx, word ptr ES:[18h] ;Relocation table offset
                ;jcxz short exe_hdr_relocation_done ; 05/11/2011
exe_hdr_relocation_cycle:
                mov di, word ptr ES:[BX]   ;Item Offset
                mov dx, word ptr ES:[BX+2] ;Item Segment (Relative]
                add dx, ax                 ;Item Segment (Absolute)
                push es
                mov es, dx
                add word ptr ES:[DI], ax ;Fixup
                pop es
                add bx, 4
                loop exe_hdr_relocation_cycle
exe_hdr_relocation_done:
                mov bx, ax
                add bx, word ptr ES:[0Eh] ;Initial SS Value
                mov ss, bx
                mov sp, word ptr ES:[10h] ;Initial SP Value
                add ax, word ptr ES:[16h] ;Initial CS Value
                mov bx, word ptr [Run_Segment]
                mov word ptr [Run_Segment], ax
                mov ax, word ptr ES:[14h] ;Initial IP Value
                mov word ptr [Run_Offset], ax
                mov bp, sp ; 14/08/2011
                jmp short far_jump_to_exe_file
far_jump_to_com_file:
                ; 25/11/2009
                mov bx, word ptr [Run_Segment]
                        ; cx = Allocation size in 100h bytes
               ; 14/08/2011 
                cmp cx, 100h ; 256
                jna short pass_run_com_set_sp_to_FFFEh
                mov ax, 0FFFEh ; cx >= 256 (<= 257)
                jmp short loc_run_com_set_sp_to_FFFEh
pass_run_com_set_sp_to_FFFEh:
               ; 14/08/2011
                mov ax, 100h 
                mul cx
                dec ax
                dec ax
               ;
loc_run_com_set_sp_to_FFFEh:
                mov ss, bx
                mov sp, ax
               ; 14/08/2011
                mov bp, sp 
                mov word ptr [BP], 0 
               ;
far_jump_to_exe_file:
                mov ds, bx
                mov es, bx
                push bx
               ; 10/09/2011 Keyboard buffer flushing
loc_run_keyboard_buff_flush:
              ; mov ah, 1
              ; int 16h
              ; jz short loc_run_keyboard_buff_flush_ok
              ; xor ah, ah
              ; int 16h
              ; jmp short loc_run_keyboard_buff_flush    
loc_run_keyboard_buff_flush_ok:               
               ; 
                xor ax, ax
                push ax
                ; 08/11/2011
                mov bx, word ptr CS:[Run_File_Size]+2
                mov cx, word ptr CS:[Run_File_Size]
                ; 12/11/2009
                mov dx, ax ; 0 
                xor si, si
                xor di, di
               ;mov bp, sp
                ;
                jmp dword ptr CS:[Run_Offset]

Run_Com_INT20h_Handler:
                ; CPU comes here via INT 20h or INT 21h Terminate Process
                ; pop ax ; IP
                ; pop ax ; CS 
                ; pop ax ; Flags

                push cs
                pop  ds

Run_Com_INT20h_Handler_ctrlbrk:
                ; 14/11/2009
                mov ax, word ptr [PSP_Address]
               ; 05/11/2011
                or ax, ax ; Device driver file check
                jz Invalid_fcall_return_from_device_driver
                
Run_Com_INT20h_Handler_close_file:
                ; 14/11/2009
                call proc_close_file

Run_Com_INT20h_Handler_restore_cdrv:
                ; 29/11/2009  -> Restore current directory                
                ;push ds
                ;pop es

                mov bh, byte ptr [RUN_CDRV] ; it is set at the beginning
                                            ; of the 'run' command.
                
                cmp bh, byte ptr [Current_Drv]
                je short Run_Com_INT20h_restore_cdir
                mov dl, bh
                call proc_change_current_drive 
                jmp short Run_Com_INT20h_check_INT21h_4Bh_sign
Run_Com_INT20h_restore_cdir:
                xor bl, bl
                mov si, offset Logical_DOSDisks
                add si, bx
                call proc_restore_current_directory
                ; End of 29/11/2009 restore cdir code
                
Run_Com_INT20h_check_INT21h_4Bh_sign:
                cmp byte ptr [INT21h_4Bh], 0
                ja run_com_exec_ret_with_iret

Run_Com_check_INT27h_sign:
                cli
                ; 24/09/2011 TSR return ode
                mov ax, word ptr [TSR_Size]
                and ax, ax
                jz short Run_Com_INT20h_reset_int_vectors
                xor ax, ax
                mov word ptr [TSR_Size], ax ; 0
                jmp short Run_Com_check_INT27h_sign_ok
               ;                                   

Run_Com_INT20h_reset_int_vectors:
               ; 05/11/2011
                cmp byte ptr [DOS_Ints_Initialized],al ; 0
                ja short Run_Com_INT20h_reset_int_vectors_ok
               ; 
                mov es, ax ; 0
                push word ptr [INT1Bh_Offset]
                pop word ptr ES:[6Ch]
                push word ptr [INT1Bh_Segment]
                pop word ptr ES:[6Eh]
                push word ptr [INT20h_Offset]
                pop word ptr ES:[80h]
                push word ptr [INT20h_Segment]
                pop word ptr ES:[82h]
                ; 14/11/2009
                push word ptr [INT21h_Offset]
                pop word ptr ES:[084h]
                push word ptr [INT21h_Segment]
                pop word ptr ES:[086h]
                ; 24/09/2011
                push word ptr [INT22h_Offset]
                pop word ptr ES:[088h]
                push word ptr [INT22h_Segment]
                pop word ptr ES:[08Ah]
                push word ptr [INT23h_Offset]
                pop word ptr ES:[08Ch]
                push word ptr [INT23h_Segment]
                pop word ptr ES:[08Eh]
                push word ptr [INT24h_Offset]
                pop word ptr ES:[090h]
                push word ptr [INT24h_Segment]
                pop word ptr ES:[092h]
                ;

Invalid_fcall_return_from_device_driver:
;Run_Com_check_INT27h_sign_ok:
                push ds
		pop  es
Run_Com_check_INT27h_sign_ok:
Run_Com_INT20h_reset_int_vectors_ok:
                mov ss, word ptr [StackSegment]
                mov sp, word ptr [StackPointer]
                mov bp, word ptr [BasePointer]
                sti

                ; 12/11/2009 
                mov ah, 0Eh
               ; 13/09/2011 
                mov BH, Kernel_VideoPage
                mov BL, Kernel_CharColorAtr
		int 10h
                ;  

                retn

run_com_exec_ret_with_iret:
               ;08/11/2011 push/pop position change
                dec byte ptr [INT21h_4Bh]
               ;24/09/2011 Parent PSP restoration
run_com_exec_retn_restore_parent_psp:
                mov ax, word ptr [PSP_Address]
                mov es, ax
               ;xor bx, bx
               ;mov ax, word ptr ES:[BX]
               ;cmp ax, 20CDh
               ;jne short run_com_exec_retn_pass_rest_parent_psp
                mov bx, 22 ; pspParentId
                mov ax, word ptr ES:[BX]
                or ax, ax
                jz short run_com_exec_retn_pass_rest_parent_psp
run_com_exec_retn_restore_parent_psp_ok:
                mov word ptr [PSP_Address], ax
run_com_exec_retn_pass_rest_parent_psp: 
               ;
                mov ss, word ptr [StackSegment21h]
                mov sp, word ptr [StackPointer21h]
                pop bx ; 08/11/2011 push pos. change
                pop es ; 08/11/2011
                pop bp ; 08/11/2011
                pop di
                pop si
                pop cx
               ; 03/02/2011 
                pop ax
                pop dx
                sti
                iret

run_com_tsr_handler:
                ; 24/09/2011
                ; CPU comes here by INT 27h Terminate and Stay Resident interrupt
                or dx, dx
                jz Run_Com_INT20h_Handler 
                
                push cs
                pop ds

                mov ax, dx
                xor dx, dx

                add ax, 255
                adc dx, 0
                mov cx, 256
                div cx
                mov word ptr [TSR_Size], ax

                pop ax ; ip
                pop bx ; cs
                push ax
                push bx

                mov ax, word ptr [PSP_Address]
                cmp ax, bx
                jne Run_Com_INT20h_Handler 

                xor ah, ah
                call proc_return_memory_block_allocation_status
                mov cx, word ptr [TSR_Size] 
                cmp dx, cx
                jna Run_Com_INT20h_Handler
 		
                shl cx, 1
                shl cx, 1
                shl cx, 1
                shl cx, 1
                add bx, cx 
                ; al = allocation type
                ; ah = 0
                inc ah ; mov ah, 1
                call proc_deallocate_memory

                push ds
                pop es             
                jmp Run_Com_INT20h_Handler_restore_cdrv

; 24/09/2011
TSR_Size: dw 0
; 24/09/2011
run_com_int22h_handler:
                jmp Run_Com_INT20h_Handler
run_com_int23h_handler:
                jmp Run_Com_INT20h_Handler
run_com_int24h_handler:
                jmp Run_Com_INT20h_Handler 
;

;29/11/2009
RUN_CDRV: db 0
;10/09/2011
RUN_CDIRFC: dd 0
Run_Auto_Path: db 0
Run_Manual_Path: db 0 ; 0 -> auto path sequence needed
;
;14/11/2009
PSP_Address: dw 0
INT21h_Offset: dw 0
INT21h_Segment: dw 0
;
INT21h_4Bh: db 0
StackSegment21h: dw 0
StackPointer21h: dw 0
StackSegment: dw 0
StackPointer: dw 0
BasePointer: dw 0
Run_Offset: dw 0
Run_Segment: dw 0
Run_File_Size: dd 0 ; 08/11/2011
EXE_ID: db 0
EXE_Dot: db 0 ; 10/09/2011
INT20h_Offset: dw 0
INT20h_Segment: dw 0
INT1Bh_Offset: dw 0
INT1Bh_Segment: dw 0
; 24/09/2011
INT22h_Offset:  dw 0
INT22h_Segment: dw 0 
INT23h_Offset:  dw 0
INT23h_Segment: dw 0 
INT24h_Offset:  dw 0
INT24h_Segment: dw 0   
;

  
; !!!
; Other internal commands will be put here
; !!!

; burasi find basi

cmp_cmd_find:
               ; Temporary
                mov  cx, 4
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_find
get_char_find:
                mov  al, byte ptr [SI]
		inc  si
		scasb
                jne  short cmp_cmd_readfile
                loop get_char_find
                mov  al, byte ptr [SI]
		cmp  al, 20h
                ja  cmp_cmd_external
get_cmdfindfile_fchar:
                inc  si
                mov  al, byte ptr [SI]
                cmp  al, 20h
                je   short get_cmdfindfile_fchar
                jb   short loc_cmdfind_findfile_retn
loc_cmdfind_findfile:
                mov  byte ptr [Msg_Files_Found], "0" 
                mov  ax, 0800h
                call proc_find_first_file
                push ds
                pop  es
                jc   short loc_cmdfind_filenotfound
loc_cmdfind_nextfile:
                inc  byte ptr [Msg_Files_Found]
                mov  di, offset FindFile_Name
                call get_file_name
                call proc_printmsg
                mov  si, offset nextline
                call proc_printmsg
                call proc_find_next_file
                push ds
                pop  es
                jnc  short loc_cmdfind_nextfile 

loc_cmdfind_filenotfound:
               mov   si, offset Msg_Files_Found
               call  proc_printmsg
               stc
loc_cmdfind_findfile_retn:
               retn

; burasi find sonu

cmp_cmd_readfile:
               ; Temporary
               ;29/05/2011
                mov  cx,8
                mov  si, offset CommandBuffer
                mov  di, offset Cmd_ReadFile
get_char_readfile:
                mov  al, byte ptr [SI]
		inc  si
		scasb
               ;jne  loc_cmd_failed
               ; 10/09/2011
                jne cmp_cmd_run
                loop get_char_readfile
                mov  al, byte ptr [SI]
		cmp  al, 20h
                jna  short get_readfile_fchar
                cmp  al, "."
                je   cmp_cmd_external
                jmp  loc_cmd_failed
get_readfile_fchar:
                inc si
                mov al, byte ptr [SI]
                cmp al, 20h
                je short get_readfile_fchar
                ja short loc_readfile_parse_path_name
                retn

loc_readfile_parse_path_name:
                push ds
                pop es
                mov di, offset FindFile_Drv
                call proc_parse_pathname
                jc  loc_cmd_failed

loc_readfile_check_filename_exists:
                mov si, offset FindFile_Name
                cmp byte ptr [SI], 20h
                jna loc_cmd_failed 

                mov dh, byte ptr [Current_Drv]
                mov byte ptr [RUN_CDRV], dh

loc_readfile_change_drv:
                mov dl, byte ptr [FindFile_Drv]
                cmp dl, dh
                je short loc_readfile_change_directory

                call proc_change_current_drive
                jc loc_file_rw_cmd_failed

loc_readfile_change_directory:
                cmp byte ptr [FindFile_Directory], 20h
                jna short loc_readfile_open

                inc byte ptr [Restore_CDIR]
                mov si, offset FindFile_Directory
                xor ah, ah ; CD_COMMAND sign -> 0 
                call proc_change_current_directory
                jc loc_file_rw_cmd_failed

;loc_readfile_change_prompt_dir_string:
               ;call proc_change_prompt_dir_string

loc_readfile_open:
                mov si, offset FindFile_Name
                xor ax, ax  ; Read file via Buffer
                call proc_open_file
                jc loc_file_rw_cmd_failed

loc_readfile_start_read:
               ;push ds
               ;pop es
                mov word ptr [PSP_Address], ax

                mov si, offset nextline
	        call proc_printmsg

               ; 13/09/2011
                 
                mov cx, 22

loc_readfile_next_read:
                push cx
                mov ax, word ptr [PSP_Address]
                xor dx, dx
                mov cl, 1 ; 1 Byte read
                mov bx, offset ReadFile_Byte  
                call proc_read_file
                mov ax, cx
                pop cx
                jnc short loop_readfile_read_char
                cmp dh, 0FFh
                je short loop_readfile_read_char
          
loc_readfile_next_read_error:
                xor ah, ah 
                mov al, dh 
                push ax
                mov ax, word ptr [PSP_Address]
                call proc_close_file
                pop ax
                stc
                jmp loc_file_rw_cmd_failed
                
loop_readfile_read_char:
                mov byte ptr [ReadFile_EOF], dh
                or al, al ; Number of read bytes
                jz short loc_readfile_end_of_read  
		and cx, cx
                jnz short pass_readfile_wait_for_key
		xor ah, ah
		int 16h
		cmp al, 1Bh
		je short loc_readfile_end_of_read

loc_readfile_next_page:
		mov cx, 20
pass_readfile_wait_for_key:
		mov al, byte ptr ES:[BX]
		cmp al, 0Dh
		jne short pass_readfile_dec_cx
		dec cx
pass_readfile_dec_cx:
		cmp al, 09h
		je short loc_readfile_put_tab_space

pass_readfile_put_tab_space:
                mov ah, 0Eh
                mov bh, Kernel_VideoPage
                mov bl, Kernel_CharColorAtr
		int 10h
loc_readfile_check_eof:
                cmp byte ptr [ReadFile_EOF], 0FFh
                jne short loc_readfile_next_read

loc_readfile_end_of_read:
		mov si, offset nextline
		call proc_printmsg

                mov ax, word ptr [PSP_Address]
                call proc_close_file

                jmp loc_file_rw_restore_retn

loc_readfile_put_tab_space:
                push cx
                mov bh, Kernel_VideoPage ; 13/09/2011 
                mov ah, 03h ; get cursor position and size.
                int 10h
                xor ah, ah
                mov al, 8
                mov cx, ax
                xchg al, dl  ; dl  = 8
                div dl ; AX = Cursor column
                sub cl, ah
                mov al, 20h
                mov ah, 0Eh
                mov bl, Kernel_CharColorAtr
loop_readfile_put_tab_space:
		int 10h
                loop loop_readfile_put_tab_space
                pop cx
                jmp short loc_readfile_check_eof 


;Read_FileSize: dd 0
ReadFile_EOF: db 0
ReadFile_Byte: dw 0

loc_cmd_failed:
                cmp byte ptr [CommandBuffer], 20h
                jna short loc_cmd_return
                mov si, offset Msg_Bad_Command
                call proc_printmsg
loc_cmd_return:
                retn

command_interpreter endp

proc_check_exe_filename_ext proc near
                ; 10/09/2011 major modification 
                ; 14/11/2009
                ; INPUT -> DS:SI = Dot File Name
                ; 
                ; OUTPUT -> clc => EXE_ID in AL
                ;          DS:SI = Last char + 1 position
                ; -> stc => Invalid executable file name
                ;    or no file name extension if AH<=8
                ;    AL= Last file name char     
                ; clc-> AL='M' (COM), AL='E' (EXE)
  
                xor ah, ah
loc_run_check_filename_ext:                
                mov al, byte ptr [SI]
                cmp al, 21h
                jb short loc_check_exe_fn_retn 
                inc ah
                inc si
                cmp al, "."
                jne short loc_run_check_filename_ext                
                                 
loc_run_check_filename_ext_dot:
                cmp ah, 2 ; .??? is not valid
                mov ah, al ; "." 
                jb short loc_check_exe_fn_retn
loc_run_check_filename_ext_dot_ok:
                lodsb
                and al, 0DFh 
loc_run_check_filename_ext_com:
                cmp al, "C"
                ja short loc_run_check_filename_ext_exe
               ;jb short loc_run_check_filename_ext_bat
                jb short loc_check_exe_fn_retn  
                lodsb
		and al, 0DFh 
                cmp al, "O"
                jne short loc_check_exe_fn_stc
                lodsb
		and al, 0DFh
                cmp al, "M"
                jne short loc_check_exe_fn_stc
                retn                 

loc_run_check_filename_ext_exe:
                cmp al, "E"
                jne short loc_check_exe_fn_stc 
                lodsb
		and al, 0DFh 
                cmp al, "X"
                jne short loc_check_exe_fn_stc 
                lodsb
		and al, 0DFh 
                cmp al, "E"
                jne short loc_check_exe_fn_stc
                retn                 

;No BAT file recognication for now, 10/09/2011                  
;loc_run_check_filename_ext_bat:
;               cmp al, "B"
;               jne short loc_check_exe_fn_stc 
;               lodsb
;		and al, 0DFh 
;               cmp al, "A"
;               jne short loc_check_exe_fn_stc 
;               lodsb
;		and al, 0DFh 
;               cmp al, "T"
;               je  short loc_check_exe_fn_retn
loc_check_exe_fn_stc:
                stc 
loc_check_exe_fn_retn:
                retn
              
proc_check_exe_filename_ext endp


; INTERNAL COMMANDS
Cmd_Dir: db "DIR"
; Cmd_Cd: db "CD"
; Cmd_Drive: db "C:" ; D:, E: etc.
Cmd_Ver: db "VER"
Cmd_Exit: db "EXIT"
Cmd_Prompt: db "PROMPT"
Cmd_Volume: db "VOLUME"
Cmd_LongName: db "LONGNAME"
Cmd_Date: db "DATE"
Cmd_Time: db "TIME"
;Cmd_Run: db "RUN"
Cmd_Set: db "SET" ; 02/09/2011
Cmd_Cls: db "CLS"
Cmd_Show: db "SHOW"
Cmd_Del: db "DEL"
Cmd_Attrib: db "ATTRIB"
Cmd_Rename: db "RENAME"
Cmd_Rmdir: db "RMDIR"
Cmd_Mkdir: db "MKDIR"
Cmd_Copy: db "COPY"
Cmd_Move: db 'MOVE' ; 23/04/2011
Cmd_Path: db "PATH", 0 ; 03/09/2011
Cmd_Find: db "FIND" ; temporary
Cmd_Mem: db "MEM" ; MEMORY
Cmd_ReadFile: db 'READFILE' ; 29/05/2011, temporary
Cmd_Echo: db "ECHO" ; 17/09/2011
;Cmd_Remark: db "*" ; 17/09/2011
Cmd_Device: db 'DEVICE' ; 23/10/2011
Cmd_DevList: db 'DEVLIST' ; 30/10/2011

Program_Exit: db 0
Program_VideoMode: db 0
Program_VideoPage: db 0

InCtrlBreak: db 0
                       
trdos_control_break_routine:
               ; 25/09/2011 Modification
               ; 13/09/2011 Modification
                cli
                push ds
                push ax 
                push cs
                pop ds
                cmp byte ptr [InCtrlBreak], 0
                ja short return_from_ctrl_break
                push si
                push dx
                push cx
                push bx
                mov byte ptr [InCtrlBreak], 1
                mov ah, 0Fh ; Get Video Display Mode
                int 10h
                ;AH= Number of columns
                ;AL= Video mode
                ;BH= Current video page
                mov word ptr [Program_VideoMode],ax 
                mov byte ptr [Program_VideoPage],bh
                cmp al, 3
                je short pass_set_video_mode_ctrlbreak_1
                mov ah, 0 ; Set Video Mode
                mov al, 3 ; 80*25 text mode, color with CGA card
                int 10h
pass_set_video_mode_ctrlbreak_1:
                mov ah, 5 ; Set Video Page
                mov al, 7 ; Page 7  (for Control+Break Yes/No Message)
                int 10h
                mov ah, 6 ; Clear Screen (AL=0)
                mov al, 0
                mov bh, 7
                xor cx, cx
                mov dh, 24
                mov dl, 79
                int 10h
                mov ah, 2 ; Set Cursor Position
              ; mov bh, 7
                xor dx,dx
                int 10h
                mov si, offset msg_terminate_program
                call proc_printmsg
             
                pop bx
                pop cx 
                pop dx  
                pop si

loc_ask_for_terminate_program:
                mov ah, 10h
                int 16h
                cmp al, 1Bh
                je short ctrl_brk_terminate_cancel 
                and al, 0DFh ;Capitalized char
                cmp al, 'Y'
                je short loc_ctrlbreak_terminate_program
                cmp al, 'N'
                jne short loc_ask_for_terminate_program
ctrl_brk_terminate_cancel:
                mov al, byte ptr [Program_VideoMode]
                cmp al, 3
                je short pass_set_video_mode_1_ctrlbreak_2
                mov ah, 0 ; Set Video Mode
                int 10h
pass_set_video_mode_1_ctrlbreak_2:
                mov ah, 5 ; Set Video Page
                mov al, byte ptr [Program_VideoPage]
                int 10h
return_from_ctrl_break:
                pop ax
                pop ds
                sti 
                iret  

loc_ctrlbreak_terminate_program:
                pop ax
                pop ds   
                sti  
                jmp run_com_int20h_handler_ctrlbrk

Msg_Terminate_Program:
                db 7
                db "Do you want to terminate running program (Y/N) ?"
                db 0Dh, 0Ah, 0

Msg_Bad_Command:
                db "Bad command or file name!"
                db 0Dh, 0Ah, 0
Msg_Not_Ready_Read_Err:
                db "Drive not ready or read error!"
                db 0Dh, 0Ah, 0
Msg_Insufficient_Memory:
                db "Insufficient memory!"
                db 0Dh, 0Ah, 0
Msg_Dir_Not_Found:
                db "Directory not found!"
                db 0Dh, 0Ah, 0
Msg_File_Not_Found:
                db "File not found!"
                db 0Dh, 0Ah, 0
Msg_File_Directory_Not_Found:
                db "File or directory not found!"
                db 0Dh, 0Ah, 0
Msg_LongName_Not_Found:
                db "Long name not found!"
                db 0Dh, 0Ah, 0
Msg_Files_Found:
                db '0'
                db ' file(s) found !'
                db 0Ah, 0Ah, 0

Msg_DoYouWantRmdir:
                db "Do you want to delete directory ",0

Msg_DoYouWantDelete:
                db "Do you want to delete file ",0
Msg_YesNo:      db " (Y/N) ? ", 0
Msg_Deleted:    db "Deleted...", 0Dh, 0Ah, 0

Msg_Permission_denied:
                db 7
                db "Permission denied!", 0Dh, 0Ah, 0

; 16/05/2010
Msg_Not_Ready_Write_Err:
                db "Drive not ready or write error!"
                db 0Dh, 0Ah, 0
; 06/06/2010
Msg_Dir_Not_Empty:
                db "Directory not empty!"
                db 0Dh, 0Ah, 0
;
; 07/10/2010
Msg_invalid_name_chars:
                db "Invalid file or directory name chars!"
                db 0Dh, 0Ah, 0
Msg_Name_Exists: db "File or directory name exists!"
                db 0Dh, 0Ah, 0
Msg_DoYouWantMkdir:
                db "Do you want to make directory ",0  
;

; 08/08/2010
Msg_DoYouWantOverWriteFile:
                db "Do you want to overwrite file ",0
  
Msg_DoyouWantCopyFile:
                db "Do you want to copy source file to destination",0
;
; 23/04/2011
Msg_DoyouWantMoveFile:
                db "Do you want to move file",0

; 16/11/2010
Msg_DoYouWantRename:
                db "Do you want to rename ", 0
Rename_File:    db "file ", 0
Rename_Directory: db "directory ", 0
Rename_OldName: db 13 dup (0)  
Msg_File_rename_as: db " as "
Rename_NewName: db 13 dup (0)
;

; 09/08/2010
msg_insufficient_disk_space:
                db "Insufficient disk space!" 
                db 0Dh, 0Ah, 0
;
; 23/04/2010
msg_not_same_drv:
                db "Not same drive!" 
                db 0Dh, 0Ah, 0 

; 29/08/2011
Msg_No_Set_Space:
                db "Insufficient environment space !"
                db 0Dh, 0Ah, 0 
 
; 20/06/2010 -> "Run failure -> Command Failed !"
Msg_Error_Code:
                db 'Command failed ! Error code : '
Error_Code:     db '00h'
                db 0Ah, 0Ah, 0

; 04/04/2010
Y_N_nextline: db 0, 0Dh, 0Ah, 0

; 23/05/2010
Msg_New:
		   db "New "
                   db 0
Attributes:        db 0
Str_Attributes:
                   db "Attributes : "
Attr_Chars:        db "NORMAL"
                   db 0

; 11/07/2010
Msg_OK:            db "OK.", 0Dh, 0Ah, 0

; 10/07/2011
Msg_read_file_error_before_EOF:
                   db "Read file error before EOF !"
                   db 0Ah, 0Ah, 0                    

; 01/08/2010 GEÇÝCÝ
msg_source_file: db 0Dh, 0Ah, "Source file name      :   "
msg_source_file_drv: db " :", 0
msg_destination_file: db 0Dh, 0Ah, "Destination file name :   "
msg_destination_file_drv: db " :", 0
msg_copy_nextline: db 0Dh, 0Ah, 0
