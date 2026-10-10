;BEEF MOUSE (THE COW OF MOUSE DRIVERS)
;PS/2 MOUSE DRIVER FOR ANY OS
;Tested in FreeDOS
;converted to FASM by Shawn T. Cook (Bugs fixed!)
;Version 3.0
;runs smoother now.

; ****************************************************************************
; ps2mouse.asm (32 bit version by Erdogan Tan) for TRDOS 386 
; ----------------------------------------------------------------------------
; Assembling: nasm ps2mouse.asm -l ps2mouse.txt -o PS2MOUSE.PRG
; ****************************************************************************
; 10/10/2026

[BITS 32]
[ORG 0] 

;mov ah, 0        ; set video mode to 13h
;mov al, 13h
mov eax, 13h
;int 10h
int 31h

;I will use the NASM syntax!! CONVERTED TO FASM!
;I made this code for a Logitech M-S48a mouse. (normal PS/2 Code)
;For some reason the keyboard wil stall after program is terminated :S

;If you have problems understandig this code take a look at my mouseprog for the COM-mouse

;(Piece of disclaimer)
;(I do NOT give any guarantee or what so ever on this piece of code.)
;(I am not responsible for any damage whatsoever                    )
;(By using this code u accept the two lines above                   )
;(USAGE IS AT YOUR OWN RISK                                         )

JMP MAINP

;***********************************************************************
;Activate mouse port (PS/2)
;***********************************************************************
PS2SET:
  mov al, 0xa8    ; enable mouse port
  ;out 0x64, al   ; write to keyboard controller
  mov dx, 0x64
  mov ah, 1       ; write port (byte)
  int 0x34        ; TRDOS 386 IOCTL interrupt
  call CHKPRT     ; check if command is progressed (demand!)
ret

;***********************************************************************
;Check if command is accepted. (not got stuck in inputbuffer)
;***********************************************************************
CHKPRT:
  ;mov ecx, 65536
  xor ecx, ecx 
again:
  ;in al, 0x64    ; read from keyboard controller
  mov dx, 0x64
  mov ah, 0       ; read port (byte)
  int 0x34	  ; TRDOS 386 IOCTL interrupt
  test al, 2      ; Check if input buffer is empty
  jz gogo
  ;jmp again      ; (demand!) This may couse hanging, use only when sure.
  loop again
gogo:
  ret

;***********************************************************************
;Write to mouse
;***********************************************************************
WMOUS:
  mov al, 0xd4    ; write to mouse device instead of to keyboard
  ;out 0x64, al   ; write to keyboard controller
  mov dx, 0x64
  mov ah, 1       ; write port (byte)
  int 0x34	  ; TRDOS 386 IOCTL interrupt
  call CHKPRT     ; check if command is progressed (demand!)
ret

;***********************************************************************
;mouse output buffer full
;***********************************************************************
MBUFFUL:
  ;mov ecx, 65536
  xor ecx, ecx 
mn:
  ;in al, 0x64    ; read from keyboard controller
  mov dx, 0x64
  mov ah, 0       ; read port (byte)
  int 0x34
  test al, 0x20   ; check if mouse output buffer is full
  jz mnn
  loop mn
mnn:
  ret

;***********************************************************************
;Write activate Mouse HardWare
;***********************************************************************
ACTMOUS:
  call WMOUS
  mov al, 0xf4    ; Command to activate mouse itself (Stream mode)
  ;out 0x60, al   ; write ps/2 controller output port (activate mouse)
  mov dx, 0x60
  mov ah, 1       ; write port (byte)
  int 0x34
  call CHKPRT     ; check if command is progressed (demand!)
  call CHKMOUS    ; check if a byte is available
ret

;***********************************************************************
;Check if mouse has info for us
;***********************************************************************
CHKMOUS:
  mov bl, 0
  ;mov ecx, 65536
  xor  ecx, ecx
vrd:
  ;in al, 0x64    ; read from keyboard controller
  mov dx, 0x64
  mov ah, 0       ; read port (byte)
  int 0x34
  test al, 1      ; check if controller buffer (60h) has data
  jnz yy
  loop vrd
  mov bl, 1
yy:
  ret

;***********************************************************************
;Disable Keyboard
;***********************************************************************
DKEYB:
  mov al, 0xad    ; Disable Keyboard
  ;out 0x64, al   ; write to keyboard controller
  mov dx, 0x64
  mov ah, 1       ; write port (byte)
  int 0x34
  call CHKPRT     ; check if command is progressed (demand!)
ret

;***********************************************************************
;Enable Keyboard
;***********************************************************************
EKEYB:
  mov al, 0xae    ; Enable Keyboard
  ;out 0x64, al   ; write to keyboard controller
  mov dx, 0x64
  mov ah, 1       ; write port (byte)
  int 0x34
  call CHKPRT     ; check if command is progressed (demand!)
ret

;***********************************************************************
;Get Mouse Byte
;***********************************************************************
GETB:
cagain:
  call CHKMOUS    ; check if a byte is available
  or bl, bl
  jnz cagain
  call DKEYB      ; disable keyboard to read mouse byte
  ;xor eax, eax
  ;in al, 0x60    ; read ps/2 controller output port (mouse byte)
  mov dx, 0x60
  mov ah, 0       ; read port (byte)
  int 0x34
  push eax
  call EKEYB      ; enable keyboard
  pop eax
ret

;***********************************************************************
;Get 3 Mouse Bytes (PS/2 packet)
;***********************************************************************

GETB3:
c_again:
  call CHKMOUS    ; check if a byte is available
  or bl, bl
  jnz c_again
  call DKEYB      ; disable keyboard to read mouse byte
getfirstbyte:
  ;xor eax, eax
  ;in al, 0x60    ; read ps/2 controller output port (mouse byte)
  mov dx, 0x60
  mov ah, 0       ; read port (byte)
  int 0x34
  mov bl, al
  and bl, 1
  mov [LBUTTON], bl
  mov bl, al
  and bl, 2
  mov [RBUTTON], bl
  mov bl, al
  and bl, 4
  mov [MBUTTON], bl
  mov bl, al
  and bl, 8
  mov [XCOORDN], bl
  mov bl, al
  and bl, 16
  mov [YCOORDN], bl
  mov bl, al
  and bl, 32
  mov [XFLOW], bl
  mov bl, al
  and bl, 64
  mov [YFLOW], bl
getsecondbyte:
  ;xor eax, eax
  ;in al, 0x60    ; read ps/2 controller output port (mouse byte)
  mov dx, 0x60
  mov ah, 0       ; read port (byte)
  int 0x34
  mov [XCOORD], al
getthirdbyte:
  ;xor eax, eax
  ;in al, 0x60    ; read ps/2 controller output port (mouse byte)
  mov dx, 0x60
  mov ah, 0       ; read port (byte)
  int 0x34
  mov [YCOORD], al
  call EKEYB      ; enable keyboard
  ret


;***********************************************************************
;Get 1ST Mouse Byte
;***********************************************************************
; GETFIRST:
;  call GETB       ; Get byte1 of packet
;  ;xor ah, ah
;  mov bl, al
;  and bl, 1
;  mov [LBUTTON], bl
;  mov bl, al
;  and bl, 2
;  mov [RBUTTON], bl
;  mov bl, al
;  and bl, 4
;  mov [MBUTTON], bl
;  mov bl, al
;  and bl, 8
;  mov [XCOORDN], bl
;  mov bl, al
;  and bl, 16
;  mov [YCOORDN], bl
;  mov bl, al
;  and bl, 32
;  mov [XFLOW], bl
;  mov bl, al
;  and bl, 64
;  mov [YFLOW], bl
; ret

;***********************************************************************
;Get 2ND Mouse Byte
;***********************************************************************
; GETSECOND:
;  call GETB       ; Get byte2 of packet
;  ;xor ah, ah
;  mov [XCOORD], al
; ret

;***********************************************************************
;Get 3RD Mouse Byte
;***********************************************************************
; GETTHIRD:
;  call GETB       ; Get byte3 of packet
;  ;xor ah, ah
;  mov [YCOORD], al
; ret

;-----------------------------------------------------------------------
;***********************************************************************
;* MAIN PROGRAM
;***********************************************************************
;-----------------------------------------------------------------------

MAINP:

  xor eax, eax
checkit:
  ; check keyboard buffer 
  mov ah, 11h
  int 32h
  jz doit
  ; read key
  mov ah, 10h
  int 32h
  jmp checkit
doit:

  call PS2SET
  call ACTMOUS
  call GETB    ; Get the responce byte of the mouse (like: Hey i am active)
               ; If the bytes are mixed up, remove this line or add another of this line.
main:
  ;call GETFIRST
  ;call GETSECOND
  ;call GETTHIRD
  call GETB3
   
;*NOW WE HAVE XCOORD & YCOORD* + the button status of L-butten and R-button 
; and M-button allsow overflow + sign bits

;!!!
;! The Sign bit of X tells if the XCOORD is Negative or positive. (if 1 this means -256)
;! The XCOORD is allways positive
;!!!

;???
;? Like if:    X-Signbit = 1     Signbit
;?                |
;?             XCOORD = 11111110 ---> -256 + 254 = -2  (the mouse cursor goes left)
;?                      \      /
;?                       \    /
;?                        \Positive
;???

;?? FOR MORE INFORMATION ON THE PS/2 PROTOCOL SEE BELOW!!!!

;!!!!!!!!!!!!!
;the rest of the source... (like move cursor) (i leave this up to you m8!)
;!!!!!!!!!!!!!

;*************************************************************
;Allright, Allright i'll give an example!  |EXAMPLE CODE|
;*************************************************************
;=============================
;**Mark a position on scr**
;=============================
 
   xor ecx, ecx
   xor edx, edx
   
   mov cl, [XCOORD]
   mov dl, [YCOORD]
   
   cmp cl, 0
   je NoxChange
   cmp cl, 0
   jg Subx
   ;sub word [xpos], 1
   dec word [xpos]
   jmp NoxChange

Subx:
   ;add word [xpos], 1
   inc word [xpos]

NoxChange:
   cmp dl, 0
   je NoyChange
   cmp dl, 0
   jg Addy
   ;add word [ypos], 1
   inc word [ypos]
   jmp NoyChange
Addy:
   ;sub word [ypos], 1
   dec word [ypos]

NoyChange:
   mov cx, [xpos]
   mov dx, [ypos]

   ;mov ah, 0Ch
   ;mov al, 0Fh
   mov eax, 0C0Fh
   ;int 10h 
   int 31h
 
 mov byte [row], 15
 mov byte [col], 0

;=============================
;**go to start position**
;=============================
 call GOTOXY

;=============================
;**Lets display the X coord**
;=============================
 mov esi, strcdx ; display the text for Xcoord
 call disp
 xor ah, ah
 mov al, [XCOORD]
 call DISPDEC
 mov esi, stretr ; goto nextline on scr
 call disp

;=============================
;**Lets display the Y coord**
;=============================
 mov esi, strcdy ; display the text for Ycoord
 call disp
 xor ah, ah
 mov al, [YCOORD]
 call DISPDEC
 mov esi, stretr ; goto nextline on scr
 call disp

;=============================
;**Lets display the L button**
;=============================
 mov  esi, strlbt ; display the text for Lbutton
 call disp
 mov al, [LBUTTON]
 xor ah, ah
 call DISPDEC
 mov esi, stretr ; goto nextline on scr
 call disp

;=============================
;**Lets display the R button**
;=============================
 mov esi, strrbt ; display the text for Rbutton
 call disp
 mov al, [RBUTTON]
 xor ah, ah
 call DISPDEC
 mov esi, stretr ; goto nextline on scr
 call disp
 
;=============================
;**Lets display the M button**
;=============================
 mov esi, strmbt ; display the text for Mbutton
 call disp
 mov al, [MBUTTON]
 xor ah, ah
 call DISPDEC
 mov esi, stretr ; goto nextline on scr
 call disp
 
;=============================
;**Lets display sign of y
;=============================
 mov esi, signy  ; display the text for Mbutton
 call disp
 mov al, [YCOORDN]
 xor ah, ah
 call DISPDEC
 mov esi, stretr ; goto nextline on scr
 call disp

;=============================
;**stop program on keypress** 
;=============================

; Note: sometimes it takes a while for the program stops, or keyboard stalls
;       due to more time is spend looking at the PS/2 mouse port (keyb disabled)

    ;xor eax, eax
    mov ah, 11h
    int 32h
    jnz quitprog

;*************************************************************
 jmp main
 
quitprog:
 ;mov ah, 0     ; set video mode to 03h
 ;mov al, 03h
 mov eax, 03h 
 int 31h

 mov eax, 1    ; Return to OS
 int 40h       ; (sys _exit)	
     
;-----------------------------------------------------------------------
;***********************************************************************
;* END OF MAIN PROGRAM
;***********************************************************************
;-----------------------------------------------------------------------

;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;X Dont Worry about this displaypart, its yust ripped of my os.
;X (I know it could be done nicer but this works Razz)
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;XXX
;************************************************
;* Displays AX in a decimal way 
;************************************************
DISPDEC:
    mov byte [zerow], 0
    mov [varbuff], eax
    ;xor eax, eax
    ;xor ecx, ecx
    ;xor edx, edx
    ;mov ebx, 10000
    ;mov [deel], ebx
    mov	dword [deel], 10000
mainl:
    mov ebx, [deel]
    mov eax, [varbuff]
    xor edx, edx
    ;xor ecx, ecx
    div ebx
    mov [varbuff], edx
    jmp ydisp

vdisp:
    cmp byte [zerow], 0
    je nodisp

ydisp:
    mov ah, 0Eh     ; BIOS teletype
    add al, 48      ; lets make it a 0123456789 Very Happy
    mov bx, 1       
    ;int 10h        ; invoke BIOS
    int 31h	    ; TRDOS 386 video bios interrupt	

    mov byte [zerow], 1
    jmp yydis

nodisp:

yydis:
    xor edx, edx
    ;xor ecx, ecx
    ;xor ebx, ebx
    mov eax, [deel]
    ;cmp eax, 1
    ;je bver
    ;cmp eax, 0
    ;je bver
    cmp	eax, 1
    jna	short bver	
    mov ebx, 10
    div ebx
    mov [deel], eax
    jmp mainl

bver:
   ret

;***************END of PROCEDURE*********************************
;****************************************************************
;* PROCEDURE disp      
;* display a string at ds:si via BIOS
;****************************************************************
disp:
HEAD:
    lodsb           ; load next character
    cmp al, 0       ; test for NUL character
    je DONE         ; if NUL char found then goto done
    mov ah, 0Eh     ; BIOS teletype
    mov bx, 1       ; make it a nice fluffy blue (mostly it will be grey but ok..)
    ;int 10h        ; invoke BIOS
    int 31h	    ; TRDOS 386 video bios interrupt	

    jmp HEAD
 
DONE: 
   ret

;*******************End Procedure ***********************

;*****************************
;*GOTOXY  go back to startpos
;*****************************
GOTOXY:
    mov ah, 2
    mov bh, 0       ; 0:graphic mode 0-3: in modes 2&3 0-7: in modes 0&1
    mov dl, [col]
    mov dh, [row]
    ;int 10h
    int 31h	    ; TRDOS 386 video bios interrupt	
ret

;*******END********
;
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;XXX
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

;***********************************************************************
;variables
;***********************************************************************
LBUTTON: db 0x00   ;  Left   button status 1=PRESSED 0=RELEASED
RBUTTON: db 0x00   ;  Right  button status 1=PRESSED 0=RELEASED
MBUTTON: db 0x00   ;  Middle button status 1=PRESSED 0=RELEASED
XCOORD:  db 0x00   ;  the moved distance (horizontal)
YCOORD:  db 0x00   ;  the moved distance (vertical)
XCOORDN: db 0x00   ;  Sign bit (positive/negative) of X Coord
YCOORDN: db 0x00   ;  Sign bit (positive/negative) of Y Coord
XFLOW:   db 0x00   ;  Overflow bit (Movement too fast) of X Coord
YFLOW:   db 0x00   ;  Overflow bit (Movement too fast) of Y Coord

;************************************
;* Some var's of my display function
;************************************

align 4

deel:    dd 0x0000
varbuff: dd 0x0000

zerow:   db 0x00

strlbt:  db "Left button:   ", 0x00
strrbt:  db "Right button:  ", 0x00
strmbt:  db "Middle button: ", 0x00
strcdx:  db "Mouse moved (X): ", 0x00
strcdy:  db "Mouse moved (Y): ", 0x00
signy:   db "Sign (y): " ,0x00
stretr:  db 0x0D, 0x0A, 0x00
strneg:  db "-", 0x00
strsp:   db " ", 0x00
row:     db 0x00
col:     db 0x00

align 2

xpos:    dw 0x9F
ypos:    dw 0x63

;***********************************************************************
; PS/2 mouse protocol (Standard PS/2 protocol)
;***********************************************************************
; ----------------------------------------------------------------------
;
; Data packet format: 
; Data packet is 3 byte packet. 
; It is send to the computer every time mouse state changes 
; (mouse moves or keys are pressed/released). 
; 
;   Bit7 Bit6 Bit5 Bit4 Bit3 Bit2 Bit1 Bit0 
; 
; 1. YO   XO   YS   XS   1    MB   RB   LB 
; 2. X7   X6   X5   X4   X3   X2   X1   X0
; 3. Y7   Y6   Y5   Y4   Y3   Y2   Y1   Y0
;
; This means:
; YO   :  Overflow bit for Y-coord (movement to fast)
; XO   :  Overflow bit for X-coord (movement to fast) 
; X0-X7:  byte of the x-coord 
; Y0-Y7:  byte of the y-coord
; LB   :  Left button pressed
; RB   :  Right button pressed
; MB   :  Middle button pressed
;
;*********************************************************************** 
;If you want to use the scroll function u might look up the protocol at 
;the company that made the mouse. 
;(the packet format will then mostly be greater then 3)
;***********************************************************************

