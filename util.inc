
;***************************************************************************
; These two macros are used to simulate an immediate shift left and shift
; right.  This form is faster than using the CL register.  For large shift
; values the form using the CL register may be smaller.
;***************************************************************************
; Shift register "which" to the left "num" places.
;
shlm     MACRO which,num
         REPT num
         shl   &which,1
         ENDM
         ENDM
;--------------------------------------------------------------------------
; Shift register "which" to the right "num" places.
;
shrm     MACRO which,num
         REPT num
         shr   &which,1
         ENDM
         ENDM
;--------------------------------------------------------------------------

;***************************************************************************
; These are high level macros used in the main procedures.  The functions
; are somewhat diverse.
;***************************************************************************
;
; This MACRO calls other MACROS which set the stack up, set the video
; base address, get the command line parameters and set up the DS and ES
; registers.
;
setup    MACRO
         startexe  ;Set up stack for return from EXE files.
         setprams
         setvideo  ;Determine the video mode.
         ENDM
;--------------------------------------------------------------------------
; This MACRO reads the sector specified by "thesec" into the location
; specified buffer.
;         mov  al,thedriv
;         mov  cx,&count
;         mov  dx,&thesec
;         lea  bx,&buffer
;         int  25h
;         lahf
;         popf
;         sahf
;         jnc  goodread
;         jmp  readerror
;
getsecs  MACRO thesec,count,buffer
         LOCAL goodread
         mov  ah,2
         mov  al,&count
         mov  dx,&thesec
         mov  cx, ?
         mov  dh,thehead
         mov  dl,thedriv
         lea  bx,&buffer
         mov
         int  25h
         lahf
         popf
         sahf
         jnc  goodread
         jmp  readerror
goodread:
         ENDM
;--------------------------------------------------------------------------
; This MACRO reads the sector specified by "thesec" into the location
; specified buffer.
;
putsecs  MACRO thesec,count,buffer
         LOCAL goodrite

         mov  al,thedriv
         mov  cx,&count
         mov  dx,&thesec
         lea  bx,&buffer
         int  26h
         lahf
         popf
         sahf
         jnc  goodrite
         jmp  writerror
goodrite:
         ENDM
;--------------------------------------------------------------------------