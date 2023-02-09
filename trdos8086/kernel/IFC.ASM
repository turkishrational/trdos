; *****************************************************************************
;
; IFC.ASM  [ TRDOS INVALID FUNCTION CALL Handler ]
; Copyright (C) 2005 Erdogan TAN  [ 03/02/2011 ] Last Update: 17/04/2011
;
; (IFC procedure is moved to this file from INT_21H.ASM) 
;
; TRDOS.ASM (include IFC.ASM)
;
; *****************************************************************************

trdos_ifc_routine:
               ; 17/04/2011
                cmp byte ptr [Interrupt_Number], 21h
                jne short trdos_ifc_handler
                push cs
                pop ds
                jmp short loc_invalid_function_call 
trdos_ifc_handler:
               ; 19/02/2011
               ; 03/02/2011 
                push ds
                pop word ptr CS:[INT_DS]
                push cs
                pop ds

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
                push bp
                mov bp, word ptr [INT_SPBP] 
                and word ptr [BP], 0FFFEh ; clc
                pop bp
               ;  

                push word ptr [INT_CS]
                push word ptr [INT_IP]

               ; push word ptr [INT_DS]
               ; pop ds
               ;

               ; 17/04/2011

               mov ax, word ptr [INT_CS]
               mov si, word ptr [INT_IP]
               dec si
               mov ds, ax
               mov al, byte ptr [SI]
               push cs
               pop ds
               mov byte ptr [Interrupt_Number], al
               mov ax, word ptr [INT_AX]
              ;mov si, word ptr [INT_SI]  
               
loc_invalid_function_call:
              ; 03/02/2011 Modification 

                push ax
                call proc_hex
                mov word ptr [ValStr_AX]+2, ax
                pop ax
                mov al, ah
                call proc_hex
                mov word ptr [ValStr_AX], ax

                mov word ptr [ValStr_AH], ax
                mov al, byte ptr [Interrupt_Number] ; 16/04/2011
                call proc_hex
                mov word ptr [ValStr_INT], ax

                mov al, byte ptr [INT_IP]+1
                call proc_hex
                mov word ptr [ValStr_IP], ax
                mov al, byte ptr [INT_IP]
                call proc_hex
                mov word ptr [ValStr_IP]+2, ax

                mov al, byte ptr [INT_CS]+1
                call proc_hex
                mov word ptr [ValStr_CS], ax
                mov al, byte ptr [INT_CS]
                call proc_hex
                mov word ptr [ValStr_CS]+2, ax

                mov al, byte ptr [INT_Flags]+1
                call proc_hex
                mov word ptr [ValStr_Flags], ax
                mov al, byte ptr [INT_Flags]
                call proc_hex
                mov word ptr [ValStr_Flags]+2, ax

               ; 03/02/2011
                mov al, byte ptr [INT_BX]+1
                call proc_hex
                mov word ptr [ValStr_BX], ax
                mov al, byte ptr [INT_BX]
                call proc_hex
                mov word ptr [ValStr_BX]+2, ax

                mov al, byte ptr [INT_CX]+1
                call proc_hex
                mov word ptr [ValStr_CX], ax

                mov al, byte ptr [INT_CX]
                call proc_hex
                mov word ptr [ValStr_CX]+2, ax
                
                mov al, byte ptr [INT_DX]+1
                call proc_hex
                mov word ptr [ValStr_DX], ax
                mov al, byte ptr [INT_DX]
                call proc_hex
                mov word ptr [ValStr_DX]+2, ax

                mov al, byte ptr [INT_SI]+1
                call proc_hex
                mov word ptr [ValStr_SI], ax
                mov al, byte ptr [INT_SI]
                call proc_hex
                mov word ptr [ValStr_SI]+2, ax

                mov al, byte ptr [INT_DI]+1
                call proc_hex
                mov word ptr [ValStr_DI], ax
                mov al, byte ptr [INT_DI]
                call proc_hex
                mov word ptr [ValStr_DI]+2, ax

                mov al, byte ptr [INT_DS]+1
                call proc_hex
                mov word ptr [ValStr_DS], ax
                mov al, byte ptr [INT_DS]
                call proc_hex
                mov word ptr [ValStr_DS]+2, ax

                mov al, byte ptr [INT_ES]+1
                call proc_hex
                mov word ptr [ValStr_ES], ax
                mov al, byte ptr [INT_ES]
                call proc_hex
                mov word ptr [ValStr_ES]+2, ax

                mov al, byte ptr [INT_SS]+1
                call proc_hex
                mov word ptr [ValStr_SS], ax
                mov al, byte ptr [INT_SS]
                call proc_hex
                mov word ptr [ValStr_SS]+2, ax

                mov al, byte ptr [INT_BP]+1
                call proc_hex
                mov word ptr [ValStr_BP], ax
                mov al, byte ptr [INT_BP]
                call proc_hex
                mov word ptr [ValStr_BP]+2, ax

                mov al, byte ptr [INT_SP]+1
                call proc_hex
                mov word ptr [ValStr_SP], ax
                mov al, byte ptr [INT_SP]
                call proc_hex
                mov word ptr [ValStr_SP]+2, ax

                mov ah, 0
                mov al, 3 ; 80*25 text mode, color with CGA card
                int 10h

                mov si, offset msg_invalid_function_call
                call proc_printmsg
                mov si, offset msg_function_registers
                call proc_printmsg

                jmp run_com_int20h_handler_ctrlbrk

; 16/04/2011
Interrupt_Number: db 0

; 03/02/2011

INT_IP: dw 0
INT_CS: dw 0
INT_FLAGS: dw 0
INT_SPBP:dw 0
INT_DS: dw 0
INT_ES: dw 0
INT_SS: dw 0
INT_SP: dw 0
INT_BP: dw 0
INT_SI: dw 0
INT_DI: dw 0
INT_BX: dw 0
INT_CX: dw 0
INT_DX: dw 0
INT_AX: dw 0

msg_invalid_function_call:
                db 7
                db 0Dh, 0Ah, 0Dh, 0Ah
                db "*** Invalid function call for TRDOS ! ***"
                db 0Dh, 0Ah, 0
msg_function_registers:
                db 0Dh, 0Ah
                db "INT "
ValStr_INT:     dw 3030h
                db "h"
                db "    "
                db "Function AH = "
ValStr_AH:      dw 3030h
                db "h"
                db 0Dh, 0Ah, 0Dh,0Ah
                db "Registers: ", 0Dh, 0Ah
                db "AX = "
ValStr_AX:      dd 30303030h
                db "h    "
                db "BX = "
ValStr_BX:      dd 30303030h
                db "h    "
                db "CX = "
ValStr_CX:      dd 30303030h
                db "h    "
                db "DX = "
ValStr_DX:      dd 30303030h
                db "h", 0Dh, 0Ah
                db "CS = "
ValStr_CS:      dd 30303030h
                db "h    "
                db "IP = "
ValStr_IP:      dd 30303030h
                db "h    "
                db "Flags = "
ValStr_Flags:   dd 30303030h
                db "h", 0Dh, 0Ah
                db "DS = "
ValStr_DS:      dd 30303030h
                db "h    "
                db "ES = "
ValStr_ES:      dd 30303030h
                db "h    "
                db "SI = "
ValStr_SI:      dd 30303030h
                db "h    "
                db "DI = "
ValStr_DI:      dd 30303030h
                db "h", 0Dh, 0Ah
                db "SS = "
ValStr_SS:      dd 30303030h
                db "h    "
                db "SP = "
ValStr_SP:      dd 30303030h
                db "h    "
                db "BP = "
ValStr_BP:      dd 30303030h
                db "h", 0Dh, 0Ah, 0Dh, 0Ah, 0
