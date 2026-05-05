; ****************************************************************************
; dow.s (TRDOS 386, TRDOS v2.0.11 - sample binary file, 'dow.prg')
; ----------------------------------------------------------------------------
; DOW.PRG ! '"Day of Week" - 'systime' TEST program for TRDOS 386 !
;
; 30/04/2026
;
; [ Last Modification: 30/04/2026 ]
;
; ****************************************************************************
; ref: 'dirlist.s' (30/04/2026) 

; 30/04/2026
; 14/07/2020
; 31/12/2017
; TRDOS 386 (v2.0) system calls
_ver 	equ 0
_exit 	equ 1
_fork 	equ 2
_read 	equ 3
_write	equ 4
_open	equ 5
_close 	equ 6
_wait 	equ 7
_create	equ 8
_rename	equ 9
_delete	equ 10
_exec	equ 11
_chdir	equ 12
_time 	equ 13
_mkdir 	equ 14
_chmod	equ 15
_rmdir	equ 16
_break	equ 17
_drive	equ 18
_seek	equ 19
_tell 	equ 20
_memory	equ 21
_prompt	equ 22
_path	equ 23
_env	equ 24
_stime	equ 25
_quit	equ 26
_intr	equ 27
_dir	equ 28
_emt 	equ 29
_ldrvt 	equ 30
_video 	equ 31
_audio	equ 32
_timer	equ 33
_sleep	equ 34
_msg    equ 35
_geterr	equ 36
_fpstat	equ 37
_pri	equ 38
_rele	equ 39
_fff	equ 40
_fnf	equ 41
_alloc	equ 42
_dalloc equ 43
_calbac equ 44
_dma	equ 45
_stdio	equ 46

%macro sys 1-4
    ; 29/04/2016 - TRDOS 386 (TRDOS v2.0)	
    ; 03/09/2015	
    ; 13/04/2015
    ; Retro UNIX 386 v1 system call.		
    %if %0 >= 2   
        mov ebx, %2
        %if %0 >= 3    
            mov ecx, %3
            %if %0 = 4
               mov edx, %4   
            %endif
        %endif
    %endif
    mov eax, %1
    ;int 30h
    int 40h ; TRDOS 386 (TRDOS v2.0)		   
%endmacro


[BITS 32] ; 32-bit intructions

[ORG 0] 

START_CODE:
	;mov	esi, esp
	;lodsd
	;cmp	eax, 2 ; two arguments (program file name & date)
	;jb	short terminate ; nothing to do
	;lodsd ; program file name address 
	;lodsd ; text file name address
	
	; EAX = arg2 ; date (text)

	;push	eax

	;mov	esi, msg_program
	;call	print_msg

	;xor	ah, ah
	;int	32h

	;pop	eax

	; Get Date&Time in MSDOS (TRDOS 386) format (1980->1980) 
	sys	_time, 3
	
	; EAX = Current Time (RTC)	
	;  AL = Second (DL in MSDOS)
	;  AH = Minute (CL in MSDOS)
	;  HW of EAX = Hour (CH in MSDOS)
	; EDX = Current System Date (RTC)
	;  DL = Day (DL in MSDOS)
	;  DH = Month (DH in MSDOS)
	;  HW of EDX = Year (CX in MSDOS)

	mov 	byte [second], al
	mov 	byte [minute], ah
	shr	eax, 16
	mov 	byte [hour], al

	mov 	byte [day], dl
	mov 	byte [month], dh
	shr	edx, 16
	mov 	word [year], dx

	push	dword [day]
	push	dword [month]
	push	dword [year]

	call	DayOfWeek
	
	add	esp, 12

	mov	[dow], eax

	mov	esi, header
	call	print_msg

	mov	esi, newline
	call	print_msg

	mov	edi, txt_date

	mov	eax, [day]
	call	bin_to_str_2
	
	mov	byte [edi],'/'
 	inc	edi
	
	mov	eax, [month]
	call	bin_to_str_2
	
	mov	byte [edi],'/'
 	inc	edi

	mov	eax, [year]
	call	bin_to_str_4

	mov	byte [edi],0

	mov	esi, txt_date
	call	print_msg

	mov	esi, _space
	call	print_msg	

	mov	ebx, [dow]
	shl	ebx, 2	; * 4
	add	ebx, day_names
	mov	esi, [ebx]
	call	print_msg

	mov	esi, newline
	call	print_msg

	mov	edi, txt_time

	mov	eax, [hour]
	call	bin_to_str_2
	
	mov	byte [edi],':'
 	inc	edi
	
	mov	eax, [minute]
	call	bin_to_str_2
	
	mov	byte [edi],':'
 	inc	edi

	mov	eax, [second]
	call	bin_to_str_2

	mov	byte [edi],0

	mov	esi, txt_time
	call	print_msg

	mov	esi, newline
	call	print_msg

	sys	_exit, 0

;-----------------------------------------------------------------

hang:
	jmp	short hang

;-----------------------------------------------------------------

bin_to_str_4:
	mov	ebx, 5
	jmp	short bin_to_str
bin_to_str_2:
	mov	ebx, 3
bin_to_str:
	mov	ebp, esp
	mov	ecx, 10
bin2str_div:
	xor	edx, edx
	div	ecx
	add	dl, '0'
	push	edx
	cmp	eax, 0
	ja	short bin2str_div
pop_next:
	pop	eax
	dec	ebx
	jz	skip_stosb
	stosb
skip_stosb:
	cmp	esp, ebp
	jb	short pop_next
zero_prefix:
	or	ebx, ebx
	jz	bin2str_ok
	dec	ebx
	jz	bin2str_ok
	mov	byte [edi-1],'0'
	stosb

	jmp	zero_prefix
bin2str_ok:
	retn	
	
;-----------------------------------------------------------------

print_msg:
	mov	ebx, 0Fh       ; white characters (bl)
		               ; video page 0 (bh)
print_msg_@:
	mov	ah, 0Eh ; teletype output (write tty)
	lodsb
_p_nextchar:
	int	31h
	lodsb
	and	al, al
	jnz	short _p_nextchar
	retn

;-----------------------------------------------------------------
;
;-----------------------------------------------------------------

; Work out what the day of the week is for a given date
; Speed optimized x86 assembly (PIII) by Lingo
;
; C Algorithm by Tomohiko Sakamoto
; int dayofweek(int d, int m, int y) {
;   static int t[] = { 0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4 };
;   y -= m < 3;
;   return ( y + y/4 - y/100 + y/400 + t[m-1] + d) % 7;
; }
; year  = [0, 9999+]
; month = [1, 12]
; day   = [1, 31]

;DayOfWeekL  PROTO year:DWORD, month:DWORD, day:DWORD

DayOfWeek:
  mov  ecx, [esp+8]               ; ecx=month
  mov  edx, [esp+4]               ; edx=year
  cmp  ecx, 3                     ; test if month is < 3
  sbb  edx, 0                     ; if month < 3 then dec edx->years
  mov  ecx, [monthdata + ecx*4-4] ; ecx-> monthdata[m-1]
  mov  eax, edx                   ; eax=edx=years
  imul edx, 0A3D8h                ; dividing edx=years by 100 == imul edx, 0A3D8h / shr edx, 22
  add  ecx, [esp+12]              ; add day; ecx= monthdata[m-1] + day
  add  ecx, eax                   ; ecx-> monthdata[m-1] + day + years
  shr  eax, 2                     ; eax= years/4
  add  eax, ecx                   ; eax= monthdata[m-1] + day + years + years/4
  ;mov ecx, [esp]                 ; ecx->return address
  ;mov [esp+12], ecx              ; ecx->return address to right place
  shr  edx, 22                    ; end of dividing ; edx=years/100
  sub  eax, edx                   ; eax= monthdata[m-1] +day + years + years/4 - years/100
  shr  edx, 2                     ; edx=years/400
  add  eax, edx                   ; eax= monthdata[m-1] +day + years + years/4 - years/100 + years/400
  mov  edx, 24924925h             ; dividing eax by 7 == mul edx, eax ; 24924925h=dword (1/7 * 2^35)
  mov  ecx, eax                   ; ecx= monthdata[m-1]  + day + years + years/4 - years/100 + years/400
  mul  edx                        ; dividing eax by 7 ->result in edx
  mov  eax, ecx                   ; eax= monthdata[m-1] + day + years + years/4 - years/100 + years/400
  lea  ecx, [edx+edx*2]           ; multiplying edx by 7 ->
  lea  edx, [ecx+edx*4]           ; ->result in edx
  ;add  esp, 3*4                  ; clearing the stack from 3 dword parameters
  sub  eax, edx                   ; eax= (monthdata[m-1] + day + years + years/4 - years/100 + years/400) % 7
  retn                            ; faster return then return 3*4

;-----------------------------------------------------------------
;
;-----------------------------------------------------------------

monthdata: dd 0,3,2,5,0,3,5,1,4,6,2,4

;-----------------------------------------------------------------

	 db 0
header:	 db 0Dh, 0Ah
	 db "Current Date&Time: "
newline: db 0Dh,0Ah,0
_space:	 db " ",0

_sunday:   db "SUNDAY",0
_monday:   db "MONDAY",0
_tuesday:  db "TUESDAY",0
_wesneday: db "WESNEDAY",0
_thursday: db "THURSDAY",0
_friday:   db "FRIDAYDAY",0
_saturday: db "SATURDAY",0


align 4

day_names:
	dd _sunday
	dd _monday
	dd _tuesday
	dd _wesneday
	dd _thursday
	dd _friday
	dd _saturday
	
;-----------------------------------------------------------------
;  uninitialized data
;-----------------------------------------------------------------

bss:

ABSOLUTE bss

alignb 4

year:	resd 1
month:	resd 1
day:	resd 1
hour:	resd 1
minute: resd 1
second: resd 1
dow:	resd 1

txt_date:
	resb 12
txt_time:
	resb 10
