
;***************************************************************************
;  Initialization and ending macros
;
;
;***************************************************************************
; This MACRO is the first one called in the main program.  The initial
; value of DS and CS point to the PSP.  The command line arguments are
; copied from the PSP before DS is loaded with the data segment value.

startexe MACRO
start:
         push ds       ; Set up stack for return from EXE file.
         sub  ax,ax
         push ax
         cld           ; Make sure direction flag is cleared since string
                       ; instructions are used extensively in the code.

         mov  si,80h   ; Get length of command line string
                       ; Initial value of DS is the same as CS.
         lodsb
         xor  ah,ah    ; Clear top half of AH for next instruction.
         mov  cx,ax      ; Put value in CX for rep movs.

         mov  ax,datarea     ; Set up extra segment.
         mov  es,ax
         lea  di,comlindata  ; Copy command line in this buffer.

         rep  movsb

         mov  ax,datarea  ; Set up data segment address.
         mov  ds,ax
         mov  oldsp,sp    ; Store stack value in global variable.
         clrscr 1,1,25,80  ;Clear the screen.
         ENDM

;--------------------------------------------------------------------------
; This is the last MACRO called in the program.  It is jumped into if an
; error occurs.  If processing ends normally then it will jump over the
; error messages.  It can be jumped to even if I am in the middle of a
; procedure call since I have stored the correct stack value in a global
; variable.
endexe   MACRO
         jmp  leave
readerror:
         lea  ax,err_mess7 ;disk read error (on absolute read)
         mov  outmessaddr,ax
         jmp  leave
writerror:
         lea  ax,err_mess8 ;disk write error (on absolute write)
         mov  outmessaddr,ax
leave:
         clrscr 1,1,25,80
         putcur 25,1           ; Put cursor at bottom of screen.
         cmp  outmessaddr,0    ; If address is zero then no message is needed.
         je   nomore
         mov  ax,outmessaddr
         push ax
         mov  ax,1        ; This is the calling sequence for the video routine.
         push ax
         mov  ax,25
         push ax
         call thevid
         add  sp,6
nomore:
         mov  sp,oldsp  ; Restore stack value.
         ret
         ENDM
;--------------------------------------------------------------------------
; This MACRO is used to skip spaces between labels opcodes and operands.
; It will advance the SI register until it finds a non-blank character.
;
eatspace MACRO
         LOCAL doagain
doagain: lodsb
         cmp   al,' '
         je    doagain
         dec   si
         ENDM
;--------------------------------------------------------------------------
; This MACRO sets up the gloabal variable "thedriv" from the command line
; data.  If no valid drive letter is used then it will default to the
; value thedriv was initialized to in the data segment.
setprams MACRO
         LOCAL noparm

         lea  si,comlindata
         eatspace
         lodsb
         and  al,0DFh
         sub  al,41h
         cmp  al,26
         ja   noparm
         mov  thedriv,al
noparm:
         ENDM
