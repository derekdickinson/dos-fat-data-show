; This macro clear the middle of a screen buffer leaving only the border.
; -- buffer is the label for the buffer.
clrbuf   MACRO buffer
         LOCAL doagain
         lea  di,buffer
         mov  bx,di
         add  bx,1920
         mov  al,' '
         add  di,81
doagain:
         mov  cx,78
         rep  stosb
         add  di,2
         cmp  di,bx
         jl   doagain
         ENDM
;--------------------------------------------------------------------------
; This macro writes out a hex number to a buffer pointed to by DI.
;
hex_out  MACRO
         LOCAL iszero,isgood1,isgood2
         and   ax,0FFh   ;Zero out AH.
         jz    iszero   ;Check if number is zero to avoid an error on div.
         mov   cx,16    ;Put 16 in cl and zero out ch.
         div   cl       ;Perform division to get most significant digit in
iszero:                 ;al and least significant digit in ah.
         add   al,30h   ;Add 30h to put numbers in ASCII.
         cmp   al,3ah
         jl    isgood1
         add   al,7
isgood1:
         stosb
         mov   cl,8     ;Shift least significant digit into al, convert it
         shr   ax,cl    ;to ASCII and put it in the output buffer.
         add   al,30h
         cmp   al,3ah
         jl    isgood2
         add   al,7
isgood2:
         stosb
         ENDM
;--------------------------------------------------------------------------
; This MACRO is the body of a procedure which copies a binary value in AX
; into a decimal value in the output buffer pointed to by DI.
;
theasc   MACRO
         LOCAL anoth,done,newchar
         push  bx

         xor   bx,bx     ; Zero out bx.
anoth:
         xor   dx,dx
         div   radx
         add   dx,30h    ; Convert modulus to ASCII.
         mov   wordbuf[bx],dl  ; store in intermediate buffer
         inc   bx
         cmp   ax,0      ; Through when the divisor is zero.
         jne   anoth

         cmp   field,0   ; Optional minimum field width.  Padded with zeroes.
         je    nofield
         mov   cl,field
         sub   cl,bl     ; "bl" points to end of string and indicates how
         mov   al,'0'    ; many chars it will occupy.
         rep   stosb

nofield:

         dec   bx

newchar:
         mov   al,wordbuf[bx]  ; Copy chars to output buffer in reverse
         stosb                 ; order.
         dec   bx
         cmp   bx,0
         jl    done
         jmp   newchar
done:
         pop   bx
         ret
         ENDM

;--------------------------------------------------------------------------
; This MACRO is the analagout to the above routine for 32 bit numbers.
; the algorithm is described in C in the following code:
;main()
;{
;  unsigned int d=0xfe45,a=0x4444,i=0,j,jnk;
;  int radix=10;
;  unsigned char *result,output[50],numd[11],numa[11],tt=0,
;    thm[]={0,0,0,0,6,3,5,5,6,0,0,0,0,0,0,0,0};
;
;  result=&output[1];
;
;  memset(output,0,50);
;  memset(d,0,11);
;  memset(a,0,11);
;
;  for(i=0;i<5;i++)
;  {
;    numd[i]=d % radix;
;    d/=radix;
;
;    numa[i]=a % radix;
;    a/=radix;
;  }
;
;  for(j=0;j<11;j++)
;  {
;    result[j-1]=(tt % radix) + 0x30;
;    tt=tt/radix+numa[j];
;    for(i=5;i>0;i--) tt+=numd[5-i]*thm[i+j-1];
;  }
;
;  printf("%s",strrev(result));
;
;}
; This algorithm could be improved in several ways but time did not permit
; optimization.  Incidentally, the two loops could be combined to one and
; the "a" and "d" arrays can probably be eliminated.
;
doubmac  MACRO
         LOCAL doloop,nexterm,zeros,newchar,next,done
         push  bx
         push  si
         push  di

         mov   cx,5
         mov   si,ax
         mov   di,dx
         xor   bx,bx
doloop:
         mov   ax,si
         xor   dx,dx
         div   radx
         mov   numa[bx],dl
         mov   si,ax

         mov   ax,di
         xor   dx,dx
         div   radx
         mov   numd[bx],dl
         mov   di,ax

         inc   bx
         loop  doloop         ; The first loop initializing the arrays.

         xor   si,si
         xor   ax,ax
restart:
         xor   dx,dx
         div   radx
         add   dx,30h
         add   al,numa[si]
         mov   wordbuf[si-1],dl

         xor   di,di
         mov   bx,5
nexterm:
         mov   cx,ax
         mov   al,numd[di]
         inc   di
         mul   byte ptr thm[si+bx-1]  ;Already initialized in Data segment.
         add   ax,cx
         dec   bx
         jnz   nexterm

         inc   si
         cmp   si,11
         jl    restart
         pop   di

zeros:
         inc   di
         dec   si
         cmp   si,0
         je    go
         cmp   byte ptr wordbuf[si],30h ; Strip off leading zeroes.
         jle   zeros
go:
         dec   di

newchar:
         mov   al,wordbuf[si]
         stosb
         dec   si
         cmp   si,0
         jge   newchar      ; Reverse string and write it out.

         pop   si
         pop   bx
         ret
         ENDM

;--------------------------------------------------------------------------
; This MACRO puts the 16 or 8 bit hex value into the output buffer specified
; by "buffer".  Conditional assembly is used to determine whether to use a
; 8 or 16 bit version.
;
puthxbf  MACRO val,ro,col,buffer
         LOCAL iszero,isgood1,isgood2
         mov   ax,80             ;Number of bytes per row
         mov   bx,&ro             ;Number of rows.
         dec   bx                 ;Offset is numbered from (0,0) rather
                                  ;than (1,1).
         mul   bx                 ;bx = 80*(row-1)
         mov   cx,&col            ;Number of col
         dec   cx                 ;cx = (col-1)
         add   ax,cx              ;ax = 80*(row-1)+(col-1)
         mov   di,ax              ;Put offset into di.
         lea   ax,&buffer
         add   di,ax
         mov   al,&val
if (TYPE &val) EQ 1
         hex_out
else
         push  ax
         mov   cl,8
         shlm  ax,8
         hex_out
         pop   ax
         hex_out
endif
         ENDM
;--------------------------------------------------------------------------
; This MACRO copies "count" bytes into the buffer specified by "buffer"
;
putascbf MACRO val,ro,col,buffer,count
         mov   ax,80             ;Number of bytes per row
         mov   bx,&ro             ;Number of rows.
         dec   bx                 ;Offset is numbered from (0,0) rather
                                  ;than (1,1).
         mul   bx                 ;bx = 80*(row-1)
         mov   cx,&col            ;Number of col
         dec   cx                 ;cx = (col-1)
         add   ax,cx              ;ax = 80*(row-1)+(col-1)
         mov   di,ax              ;Put offset into di.
         lea   ax,buffer
         add   di,ax
         lea   si,&val
         mov   cx,count
         rep   movsb
         ENDM
;--------------------------------------------------------------------------
; This MACRO sets DI to the offset indicated by "ro" and "col"
;
putdi    MACRO ro,col
ifb <ro>
         mov   di,&col-1
else
         mov   ax,80             ;Number of bytes per row
         mov   bx,&ro             ;Number of rows.
         dec   bx                 ;Offset is numbered from (0,0) rather
                                  ;than (1,1).
         mul   bx                 ;bx = 80*(row-1)
         mov   cx,&col            ;Number of col
         dec   cx                 ;cx = (col-1)
         add   ax,cx              ;ax = 80*(row-1)+(col-1)
         mov   di,ax              ;Put offset into di.
endif
         ENDM
;--------------------------------------------------------------------------
; This MACRO puts the the decimal number "val" into the buffer at location
; specified by "ro" and "col".  The optional "isfield" parameter allows the
; insertion of leading zeros.  Conditional assembly is used extensively in
; order to match the size of the operand.

numout  MACRO val,ro,col,buffer,isfield

ifidn <val>,<ax>
        push  ax
endif

ifnb <isfield>
        mov   field,&isfield
endif
        putdi <ro>,<col>
        lea   ax,&buffer
        add   di,ax

ifidn <val>,<ax>
        pop  ax
        call donmout
else
  if (TYPE &val) EQ 1
        xor  ax,ax
        mov  al,val
        call donmout
  else
    if (TYPE &val) EQ 2
        mov  ax,val
        call donmout
    else
        mov  dx,word ptr &val+2
        mov  ax,word ptr &val
        call dodoub
    endif
  endif
endif

ifnb <isfield>
        mov  field,0
endif

        ENDM
