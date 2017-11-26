
;***************************************************************************
; The following macros are used for video and cursor manipulation.  The
; writes themselves are directly to video memory BIOS is used for several
; other services.
;***************************************************************************
; "putcon" performs the calling sequence to the procedure "thevid".
; The procedure expects to see the row, column and address of an ASCIIZ
; string on the stack.
; row     -- Row in which to write.
; col     -- Column in which to write.
; astring -- Label corresponding to an ASCIIZ string.
;
putcon   MACRO  row,col,astring
         lea   ax,&astring  ; Push values on stack.
         push  ax
         mov   ax,&col
         push  ax
         mov   ax,&row
         push  ax
         call  thevid
         add   sp,6         ;Restore stack.
         ENDM

;--------------------------------------------------------------------------
; This macro is the main part of the procedure thevid.
; The procedure uses direct video writes to display a string on the screen.
; It moves the BP register and uses it to index the variables on the stack.
;
; If the information is too large to fit on the screen then it will wait
; for a key, clear the screen and then start back at the top again.
;
videout  MACRO
         LOCAL  theloop,done,was_cr
         push  bp                 ;Save BP.
         mov   bp,sp              ;Set BP to top of stack.
         mov   ax,video_base      ;Set ES to base address of video memory.
         mov   es,ax
         mov   ax,160             ;Number of bytes per row
         mov   bx,[bp+4]  ;row    ;Number of rows.
         dec   bx                 ;Offset is numbered from (0,0) rather
                                  ;than (1,1).
         mul   bx                 ;bx = 160*(row-1)
         mov   cx,[bp+6]          ;Number of col
         dec   cx                 ;cx = (col-1)
         shl   cx,1               ;cx = 2*(col-1)
         add   ax,cx              ;ax = 160*(row-1)+2*(col-1)
         mov   di,ax              ;Put offset into di.
         mov   si,[bp+8]          ;Load source location into SI.
         mov   ah,theatt
theloop:
         lodsb                    ;Load character from string.
         or    al,al              ;Check for end of string.
         je    done
         cmp   al,CR              ;Check for carraige return.
         je    was_cr
         cmp   di,4000            ;Check for end of video memory.
         jl    $+5
         jmp   waitmes            ;Print "more" and wait for key.
         stosw
         jmp   theloop
was_cr:
         inc   si                 ;If it was a carraige return then
                                  ;go to beginning of next line.
         mov   ch,ah              ;Store attribute byte.
         mov   ax,di              ;Load offset in ax.
         mov   cl,160             ;Load 160 into cl for division.
                                  ;The loss of remainder will set the offset
                                  ;back to the start of a row.
         div   cl
         inc   al                 ;Increment row count by one for LF.
         mul   cl                 ;Multiply back for actual offset.
         mov   di,ax
         mov   ah,ch              ;Restore the attribute in AH.
         jmp   theloop            ;Go back to business as usual.
done:
         mov   ax,datarea         ;Put ES back on Data segment.
         mov   es,ax
         pop   bp                 ;Restore BP.
         ret
waitmes:                          ;This is branched to if the data goes past
         push  ax                 ; the end of the screen.
         push  cx
         push  bx
         push  dx                 ;Save regs and print "more" at lower right
         mov   di,3990            ; corner of screen.
         mov   al,'m'
         stosw
         mov   al,'o'
         stosw
         mov   al,'r'
         stosw
         mov   al,'e'
         stosw
         putcur 25,80             ;Move cursor to lower right corner of screen.
         getch                    ;Get a key from the user.
         mov   ax,0720h
         mov   di,0               ;Clear the screen.
         mov   cx,2000
         rep   stosw
         pop   cx
         pop   dx                 ;Restore the registers.
         pop   bx
         pop   ax
         mov   di,2               ;Reset DI
         jmp   theloop            ;Back to business as usual.
         ENDM

;--------------------------------------------------------------------------
;
; This MACRO performs a BIOS call to determine the video mode.  This
; information is used to determine the video base address.
;
setvideo MACRO
         LOCAL color,done
         mov  invmask,0f870h ; Set up masks for mono modes.
         mov  clrmask,08807h
         mov  theatt,07h
         mov  ah,15
         int  10h
         cmp  al,7         ;If video mode is 7 then the board is monochrome.
         jne  color        ;Otherwise it is a color board.
         mov  video_base,VIDMONO
         jmp  done
color:
         mov  video_base,VIDCOLOR
         cmp  al,2   ; If color card is in mono mode then use mono attributes.
         je   done
         mov  theatt,COLORATT
         mov  invmask,08f2fh
         mov  clrmask,08e10h
done:
         ENDM

;--------------------------------------------------------------------------
; "putcur" uses a BIOS call to put the cursor at location (row,col)
;
putcur   MACRO row,col
         mov  ah,2
         mov  bh,0
         mov  dl,&col
         dec  dl        ;BIOS numbers from (0,0)
         mov  dh,&row
         dec  dh
         int  10h
         ENDM
;--------------------------------------------------------------------------
; "clrscr" uses BIOS's scroll screen function in order to clear the screen.
;
clrscr   MACRO rowu,colu,rowl,coll
         mov  ah,6     ;BIOS function int 10h 06.
         mov  al,0     ;Clear entire window.
         mov  bh,7     ;Attribute of scrolled off lines.
         mov  ch,&rowu
         dec  ch
         mov  cl,&colu
         dec  cl
         mov  dh,&rowl
         dec  dh
         mov  dl,&coll
         dec  dl
         int  10h
         ENDM
;--------------------------------------------------------------------------
; This macro scrolls the screen "rows" number of lines.
; The window parameters are optional, if left out the whole screen scrolls.
;
scroll   MACRO rows,rou,colu,rol,coll,isback
ifb <isback>
         mov  ah,6     ;BIOS function int 10h 06 for forward scroll.
else
         mov  ah,7     ; else backward scroll.
endif
         mov  al,&rows

         mov  bh,theatt  ; load attribute for scrolled lines.
ifb <rou>
         mov  ch,0
else
         mov  ch,&rou-1
endif
ifb <colu>
         mov  cl,0
else
         mov  cl,&colu-1
endif
ifb <rol>
         mov  dh,24
else
         mov  dh,&rol-1
endif
ifb <coll>
         mov  dl,79
else
         mov  dl,&coll-1
endif
         int  10h
         ENDM
;--------------------------------------------------------------------------
; "getch" uses a BIOS call to wait for the user to press a key.
; The key is returned in ax.  This program throws away the key value.
;
getch    MACRO
         mov  ah,0    ;
         int  16h
         ENDM
;--------------------------------------------------------------------------
; This is the main body of a direct video write routine.  It is used to
; change the attributes of the locations specified on the stack.
; [bp+4] is ro
; [bp+6] is starting column
; [bp+8] is ending column
; [bp+10] is the proper mask to use.
;
; This procedure is used to both invert and clear screen attributes depending
; on the mask.
;
attmac   MACRO
         push bp
         mov  bp,sp
         push es
;                    row          starting column
         putdi <word ptr [bp+4]>,<word ptr [bp+6]>
         shl  di,1
         add  di,1
         mov  cx,[bp+8]  ; Ending column.
         sub  cx,[bp+6]  ; starting column
         inc  cx         ; result in cx for loop command.
         mov  es,video_base
         mov  ax,word ptr [bp+10] ; Now the mask.
theloop:
         and  es:[di],ah
         or   es:[di],al
         inc  di
         inc  di
         loop theloop  ; Perform the loop.

         pop  es      ; And go home.
         mov  sp,bp
         pop  bp
         ret
         ENDM
;--------------------------------------------------------------------------
; These macros perform the calling sequence to the procedure which contains
; the above macro.
;
; This is the reverse video macro.
;
invatt   MACRO ro,col1,col2
         push invmask
         xor  ah,ah
         mov  al,col2
         push ax
         mov  al,col1
         push ax
         mov  al,ro
         push ax
         call difatt
         add  sp,8
         ENDM
;--------------------------------------------------------------------------
; This is the restore normal attribute MACRO.
;
clratt   MACRO ro,col1,col2
         push clrmask
         xor  ah,ah
         mov  al,col2
         push ax
         mov  al,col1
         push ax
         mov  al,ro
         push ax
         call difatt
         add  sp,8
         ENDM
;--------------------------------------------------------------------------
; This macro stores the screen's contents on the stack.  It is used to
; preserve the screen when calling the various routines.
;
savescn  MACRO
         push es
         push ds
         mov  ax,ss
         mov  es,ax
         mov  ds,video_base
         sub  sp,4000
         mov  di,sp
         xor  si,si
         mov  cx,2000
         rep  movsw
         mov  ax,ss:[di]
         mov  ds,ax
         mov  ax,ss:[di+2]
         mov  es,ax
         ENDM
;--------------------------------------------------------------------------
; This routine is the complement to savescn, it restores the previously
; saved screen.
;
restscn  MACRO
         mov  es,video_base
         xor  di,di
         mov  ax,ss
         mov  ds,ax
         mov  si,sp
         mov  cx,2000
         rep  movsw
         add  sp,4000
         pop  ds
         pop  es
         ENDM
;--------------------------------------------------------------------------