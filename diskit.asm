; PROGRAM ma-490 Low level Disk edit program program.
;
              assume    cs:codeseg,ds:datarea

; "defines.inc" Contains all the equates.
              include   defines.inc

; Contains the Structure definitions.
              include   strucs.inc

; Contains these macros.
; shlm,shrm,setup,getsecs,putsecs
              include   util.inc

; Data definitions.
              include   diskdata.inc

; Contains these macros.
; putcon,videout,setvideo,putcur,clrscr,scroll,getch,attmac,invatt,clratt,
; savescn,restscn
              include   videomac.inc

; Contains these macros.
; startexe,endexe,eatspace,setprams,
              include   startmac.inc

; Contains these macros.
; clrbuf,hex_out,theasc,doubmac,puthxbuf,putascbuf,putdi,numout
              include   bufouts.inc
;--------------------------------------------------------------------------
; This MACRO displays the boot record information on the screen.
;
bootshow MACRO
         LOCAL
         savescn   ; Save screen on stack.
         mov   ax,ds          ;Set ES to base address of buffer.
         mov   es,ax
         mov   di,80*2+35  ; display drive character.
         mov   al,thedriv
         add   al,41h
         mov   byte ptr bootscr[di],al
         puthxbf    bootrec.thejump[0],5,49,bootscr
         puthxbf    bootrec.thejump[1],5,52,bootscr
         puthxbf    bootrec.thejump[2],5,55,bootscr
         putascbf   bootrec.oemname,6,49,bootscr,8
         numout     bootrec.bytprsec,7,49,bootscr
         numout     bootrec.secprclus,8,49,bootscr
         numout     bootrec.resecs,9,49,bootscr
         numout     bootrec.numfats,10,49,bootscr
         numout     bootrec.rootents,11,49,bootscr
         numout     bootrec.totsecs,12,49,bootscr
         puthxbf    bootrec.mediadesc,13,49,bootscr
         numout     bootrec.secsprfat,14,49,bootscr
         numout     bootrec.secsprtrak,15,49,bootscr
         numout     bootrec.numheads,16,49,bootscr
         numout     bootrec.numhids,17,49,bootscr
         putcon 1,1,bootscr
         getch
         restscn
         ret
         ENDM
;--------------------------------------------------------------------------
; Check for valid boot record, this program only supports double sided
; floppy drives.  If another kind is found then display message and still
; give them boot record information since it is still valid.
;
checkboot MACRO
          LOCAL isgood
          lea  si,bootrec.bytprsec
          lea  di,dummyboot
          mov  cx,13
          repe cmpsb
          je  isgood
          clrbuf startscr
          putcon 1,1,startscr
          putcon 3,1,badboot
          putcon 23,4,key_prompt
          getch
          call doboot
          jmp leave
isgood:
          ENDM
;--------------------------------------------------------------------------
; Writes the bitwise attribute value to the line buffer.
;
doattr   MACRO
         LOCAL loopit,zero
         lea   di,linebuf
         add   di,ARCCOL-1
         mov   dl,curdir.att
         shlm  dl,2
         mov   cx,6
loopit:
         mov   al,'0'
         shl   dl,1
         jnc   zero
         mov   al,'1'
zero:    stosb
         inc   di
         loop  loopit
         ENDM
;--------------------------------------------------------------------------
; Writes the time into the line buffer.
;
dotime   MACRO nosetdi
         mov  ax,curdir.time
         and  ax,HOURMASK
         shrm ax,HOURSHIF
         numout ax,,HOURCOL,linebuf,2
         mov  al,':'
         stosb
         mov  ax,curdir.time
         and  ax,MINMASK
         shrm ax,MINSHIF
         numout ax,,MINCOL,linebuf,2
         mov  al,':'
         stosb
         mov  ax,curdir.time
         and  ax,SECMASK
         shl  ax,1
         numout ax,,SECCOL,linebuf,2
         ENDM
;--------------------------------------------------------------------------
; Writes the date into the attribute buffer.
;
dodate   MACRO nosetdi
         mov  ax,curdir.date
         and  ax,MONTHMASK
         shrm ax,MONTHSHIF
         numout ax,,MONCOL,linebuf,2
         mov  al,'/'
         stosb
         mov  ax,curdir.date
         and  ax,DAYSMASK
         shrm ax,DAYSSHIF
         numout ax,,DAYCOL,linebuf,2
         mov  al,'/'
         stosb
         mov  ax,curdir.date
         and  ax,YEARMASK
         shrm ax,YEARSHIF
         add  ax,80
         numout ax,,YEARCOL,linebuf,2
         ENDM
;--------------------------------------------------------------------------
; Main body of entline procedure, writes the directory entry into the line
; buffer "linebuf".
;
entmac   MACRO
         lea   di,linebuf
         inc   di
         mov   al,' '
         mov   cx,78      ;Clear line buffer.
         rep   stosb

         lea   di,linebuf
         add   di,NAMECOL-1
         lea   si,curdir.entname  ; Write name out to buffer.
         mov   cx,8
         rep   movsb

         lea   di,linebuf
         add   di,EXTCOL-1
         lea   si,curdir.ext ;Write out extension to buffer.
         mov   cx,3
         rep   movsb

         doattr    ; Write out file attribute to buffer.
         dotime    ; Write out time to line buffer.
         dodate    ; Write out date to line buffer.
         numout  <curdir.cluster>,,CLUSCOL,linebuf  ;Cluster number
         numout  <curdir.siz>,,SIZCOL,linebuf    ;File size.
         ret
         ENDM
;--------------------------------------------------------------------------
; Main body of showdir procedure.  Displays the directory on the screen
;
;
dirmac   MACRO
         LOCAL doagain,done,through
         clrbuf startscr       ;Clear the buffer.
         putcon 1,1,startscr  ;Use it to clear screen.
         lea  si,directory   ; Get first entry in directory.
         mov  dirpoint,si
         mov  row,3
doagain:
         mov  cx,32        ; Copy current directory value into curdir.
         lea  di,curdir
         rep  movsb

         cmp  byte ptr curdir.entname[0],0 ; Check for end of directory.
         jne  $+5
         jmp  done

         call entline           ; Put entry in linebuf
         putcon row,1,linebuf   ; Display linebuf

         mov  si,dirpoint     ; Go to next entry in directory.
         add  si,32
         mov  dirpoint,si

         inc  row            ; Increment row.
         cmp  row,23
         jl   doagain
done:
         mov  ax,row
         dec  ax
         mov  maxrow,ax ;Set up maxrow for long or short directories.
         cmp  ax,22
         jae  through

         putcon row,4,endmess  ;If short directory write end message.
through:
         putcon 1,1,menutop   ;Display the rest of the menu.
         putcon 23,1,menubot
         mov  row,3          ; Set up global variables and put proper
         invline             ; things in reverse video.
         mov    si,selected
         invatt 24,startcols[si],endcols[si]  ; Make selected command in
                                              ; reverse video.
         lea  si,directory  ; Set up dirpoint to point to first entry.
         mov  dirpoint,si

         mov  cx,32       ; Copy its value into a buffer containing the
         lea  di,curdir   ; current directory value.
         rep  movsb

         ret
         ENDM
;--------------------------------------------------------------------------
; This MACRO is the main body of the procedure dotrun.  It is used to
; truncate the file length to 3000 bytes.
;
trunmac  MACRO
         LOCAL abort,noand,noshif,doagain,isbig,through
         cmp  curdir.entname[0],0e5h  ; Abort if entry has been deleted.
         jne  $+5
         jmp  through
         cmp  curdir.entname[0],0aeh  ; Abort if on the "End" entry.
         jne  $+5
         jmp  through
         savescn              ; Save screen on the stack.
         push row             ; Save row value.

         clrbuf startscr
         putcon 1,1,startscr   ; Clear the screen

         mov  row,3
         call entline          ; Display the current entry on row 3
         putcon row,1,linebuf

         putcon 1,1,menutop
         putcon 4,1,trunhead
         putcon 23,3,key_prompt  ; Display the rest of the screen.
         getch

         cmp  ah,15h    ; Check for "Y" or "y"
         je   $+5
         jmp  abort

         cmp  word ptr curdir.siz+2,0  ; Check for small file size.
         je   $+5
         jmp  isbig

         cmp  word ptr curdir.siz,3000
         ja   isbig
         jmp  abort    ; If file is already less than 3000 byte in length
isbig:                 ; then abort.

         mov  truncount,0
         mov  di,dirpoint

         mov  word ptr [di].siz,3000   ; Put new size in directory
         mov  word ptr [di].siz+2,0

         mov  word ptr curdir.siz,3000  ; Put new size in curdir buffer.
         mov  word ptr curdir.siz+2,0

         mov  ax,curdir.cluster  ; Now modify FAT.
doagain:
         mov  dx,ax     ; Save cluster value.
         shr  ax,1      ; Divide by two
         pushf          ; Save carry flag.

         add  ax,dx     ; Get value equal to 1.5 * cluster.
         mov  si,ax     ; This value should point to the next cluster.
         lea  ax,fat1
         add  si,ax
         mov  di,si
         lodsw
         popf          ; Get flags, the carry bit indicates whether the
                       ; cluster should be anded with 0fffh or shited right.
         jc   noand
         mov  bx,0fffh ; This value will be used if the loop is exited.
         and  ax,0fffh ; Put value in ax for next retrieve.
         jmp  noshif
noand:
         mov  bx,0fff0h ; This value will be used if the loop is exited.
         shrm ax,4     ; Put value in ax.
noshif:
         inc truncount
         cmp truncount,3  ; Check for new final cluster.
         jl  doagain

         mov  dx,word ptr [di]
         or   dx,bx                ; Or previous value with mask in bx.
         mov  word ptr [di],dx     ; Write value out to fat1.

         add  di,1024
         mov  word ptr [di],dx     ; Write value out to fat2.

         call killist            ; Set the rest of the clusters to zero.

         putsecs 1,2,fat1       ; Write the fats out to disk.
         putsecs 3,2,fat2
         putsecs 5,7,directory  ; Write out directory to disk.
abort:
         pop  row              ; Restore row value.
         restscn               ; Restore screen.

         call entline
         putcon row,1,linebuf  ; Display the new directory value.
         invline
through:
         ret
         ENDM
;--------------------------------------------------------------------------
; This is the main body of the procedure killist.  It is used in the trunc
; and delete routines in order to zero out the rest of the fat entries.  The
; starting cluster value is assumed to be in ax.
;
killmac  MACRO
         LOCAL noshif,noand,doagain,done
doagain:
         cmp  ax,0ff6h  ; Check for End of file.
         ja   done

         mov  dx,ax  ; Same calculations as in trunmac to traverse list.
         shr  ax,1
         pushf
         add  ax,dx
         mov  si,ax
         lea  ax,fat1
         add  si,ax
         mov  di,si
         lodsw
         popf
         jc   noand
         mov  dx,ax

         and  dx,0f000h         ; Write zeroes out without affecting
         mov  word ptr [di],dx  ; adjacent pointers.

         add  di,1024
         mov  word ptr [di],dx  ; Write value to fat2 also.

         and  ax,0fffh          ;Extract cluster value.
         jmp  doagain
noand:
         mov  dx,ax

         and  dx,000fh          ; Write zeroes out without affecting
         mov  word ptr [di],dx  ; adjacent clusters.

         add  di,1024
         mov  word ptr [di],dx ; Write value to fat2 also.

         shrm ax,4          ;Extract cluster value.
         jmp  doagain
done:
         ret
         ENDM
;--------------------------------------------------------------------------
; This is the main body of the dodele procedure.  It deletes a file from the
; directory.  The same style is used as DOS uses.  All the FAT table entries
; are freed but the directory entry is merely marked with an E5h in the first
; byte of the name.
;
delemac  MACRO
         LOCAL noshif,noand,doagain,abort,done,through

         cmp  curdir.entname[0],0e5h ; Check for already deleted entry.
         jne  $+5
         jmp  through
         cmp  curdir.entname[0],0aeh ; Check for end marker.
         jne  $+5
         jmp  through

         savescn  ; Save screen contents.
         push row ; Save row.

         clrbuf startscr
         putcon 1,1,startscr  ; Clear the screen.

         mov  row,3
         call entline
         putcon row,1,linebuf  ; Display the current entry on line three.

         putcon 1,1,menutop
         putcon 4,1,delehead
         putcon 23,3,key_prompt
         getch

         cmp  ah,15h     ; If key was not "Y" or "y" then abort.
         jne  abort

         mov  di,dirpoint
         mov  byte ptr [di],0e5h     ; Mark directory entry as deleted.
         mov  curdir.entname[0],0e5h ; Mark curdir as deleted.

         mov  ax,curdir.cluster   ; Free the clusters allocated to the file.
         call killist

         putsecs 1,2,fat1
         putsecs 3,2,fat2        ; Write the values to disk.
         putsecs 5,7,directory
abort:
         pop  row
         restscn
         call entline          ; Restore environment.
         putcon row,1,linebuf
         invline
through:
         ret
         ENDM
;--------------------------------------------------------------------------
; This is the main body of the dofat procedure which performs the FAT command.
; The procedure will procede through the clusters one at a time starting at
; offset 0.
;
fatmac   MACRO
         LOCAL doagain,done,noshif,noand

         savescn    ; Save the screen.
         push row
         clrbuf startscr

         lea  si,fat1        ; Set initial values.
         mov  fatpoint,si

         mov  isand,TRUE

         mov  fatcount,0
         mov  row,5
         mov  col_count,FIRSTFAT

doagain:
         numout  fatcount,row,5,startscr  ; Write value to screen.

         putdi row,8
         lea  ax,startscr
         add  di,ax
         mov  al,'³'      ; Write visual seperator to screen.
         stosb

         mov  si,fatpoint  ; Get next FAT entry.
         lodsw
         cmp  isand,FALSE  ; Determine whether to and it or shift it.
         je   noand
         dec  si
         mov  fatpoint,si
         and  ax,0fffh
         mov  isand,FALSE  ; Reverse state for next cluster.
         jmp  noshif

noand:
         mov  isand,TRUE   ; Ditto last comment.
         mov  fatpoint,si
         shrm ax,4
noshif:

         numout ax,row,col_count,startscr  ; Write cluster number to screen.

         add  col_count,FATWIDTH    ; Go to next screen location.
         cmp  col_count,LASTFAT     ; Check to see if I should go to next row.
         jae  $+5
         jmp  doagain
         mov  col_count,FIRSTFAT   ; Start new row.
         inc  row
         inc  fatcount
         cmp  row,24           ; Check for bottom of screen.
         jae  $+5
         jmp  doagain
done:
         putcon 1,1,startscr
         putcon 2,1,fathead
         putcon 24,3,key_prompt
         getch                  ; Display screen and wait.

         clrbuf startscr
         mov  row,5
         mov  si,fatpoint
         lea  ax,fat1
         sub  si,ax
         cmp  si,532        ; Check for end of FAT table.
         jae  $+5
         jmp  doagain      ; Display last values of FAT table.

         pop  row
         restscn
         ret
         ENDM
;--------------------------------------------------------------------------
; This macro copies the contents of the 2nd through 22nd rows of the screen
; to a buffer.  The the contents of this buffers is written to disk in both
; the append and create routines.
;
copscrn  MACRO
         LOCAL again,nexrow

         mov   bx,2        ; Starting row number.
         mov   si,160      ; Offset of column 1 row 2.
         lea   di,copbuff  ; Get the address of the buffer.
         add   di,themod   ; This value is used if appending to a file it
         push  ds          ;   indicates where the original file's last data
         mov   ax,video_base ; ended.
         mov   ds,ax
nexrow:
         mov   cx,80     ; Desired number of bytes.
again:
         lodsw          ; Get char plus attribute,
         stosb          ;  write char alone out.
         loop  again
         mov   al,CR    ; Add carraige return linefeed to output.
         stosb
         mov   al,LF
         stosb
         inc   bx
         cmp   bx,23
         jl    nexrow
         pop   ds
         ENDM
;--------------------------------------------------------------------------
; Get time from DOS to put in directory entry.  The values returned must be
; massaged back into the directory format.
;
getime   MACRO
         mov  ah,2ch
         int  21h
         xor  ax,ax
         shlm ch,3
         or   ah,ch
         xor  ch,ch
         shlm cx,MINSHIF
         or   ax,cx
         shrm dx,9
         or   ax,dx
         ENDM
;--------------------------------------------------------------------------
; Get date from DOS and put in directory entry.  Again, a little extra
; processing is required to load the necessary information into the entry.
;
getdate  MACRO
         mov  ah,2Ah
         int  21h
         xor  ax,ax
         sub  cx,1980
         shlm cx,YEARSHIF
         or   ax,cx
         push dx
         shrm dx,3
         and  dx,MONTHMASK
         or   ax,dx
         pop  dx
         and  dx,DAYSMASK
         or   ax,dx
         ENDM
;--------------------------------------------------------------------------
; This macro makes a directory entry for the Create command.  The entry
; newdir is initialized at load time to most of the proper values.  The
; remaining fields are assigned values here.
mkdirent MACRO
         LOCAL found,again

         getime
         mov  newdir.time,ax  ; Assign time and date fields.
         getdate
         mov  newdir.date,ax

         lea  di,directory     ; Start search for first unused entry.
again:
         cmp  [di].entname[0],0e5h  ; Look for deleted file indicator.
         je   found
         cmp  [di].entname[0],0   ; Look for unused directory entry.
         je   found
         add  di,32
         jmp  again
found:

         mov  ax,theclust[0]
         mov  newdir.cluster,ax   ; Put starting cluster value in entry.

         lea  si,newdir    ; Copy value into the directory.
         mov  cx,32
         rep  movsb

         ENDM
;--------------------------------------------------------------------------
; This macro performs a search for a specified number of free clusters in
; the FAT table.  First fit is used.
;
; This macro is the main body of the procedure loadclus which performs the
; functions of loading the three global arrays,
;
;   1.) clustand -- boolean indicating whether cluster is anded or shifted.
;   2.) theclust -- theclust, the cluster's value.
;   3.) clusptr  -- the offset in memory of the cluster pointer.
;
; The arrays are initialized to the correct values for the next unused
; clusters.  This routine is used by append and create file functions.
; The number of clusters found is determined by the global variable
; cluscont.  The calling routine sets this variable to the number of
; clusters that it needs.
;
; Incidentally, If I were rewritting this program from scratch I would use
; global arrays of structures of this form.  On bootup I would initialize
; the structures and then most manipulations could be performed fairly
; easily.  Oh well, live and learn.
;
clusmac  MACRO
         LOCAL doagain,noand,noshif
         mov  isand,TRUE       ;Boolean variable to indicate whether to AND
                               ; value read in with 0f000h or to shift right
                               ; three places.
         mov  fatcont,0ffffh   ;Initialize count to -1 since it is
                               ; incremented prior to indexing.
         lea  si,fat1
         mov  fatpoint,si  ; Start search at begginning of FAT.

         mov  bx,0        ; Free cluster array index.
doagain:
         inc  fatcont     ; Increment fat table counter.
         mov  si,fatpoint

         mov  cx,si        ; Save offset of cluster in case this is an
                           ;  unused cluster.

         lodsw             ; Get cluster value.
         cmp  isand,FALSE
         je   noand

         dec  si
         mov  fatpoint,si  ; Setup fatpoint for next entry in table.
         mov  isand,FALSE  ; Setup boolean for next entry in table.

         and  ax,0fffh     ; Prepare cluster number for "0" comparison.
         jmp  noshif
noand:
         mov  isand,TRUE   ; Set up for next entry in table.
         mov  fatpoint,si

         shrm ax,4         ; Prepare cluster for "0" comparison.
noshif:
         cmp  ax,0         ; Is cluster marked free?
         jne  doagain      ;  if no then go to next cluster.
                           ; else ...
         mov  di,bx
         mov  dl,isand
         mov  clustand[di],dl  ; Put boolean value in array.

         shl  di,1
         mov  dx,fatcont
         mov  theclust[di],dx  ; Put cluster number in array.

         mov  clusptr[di],cx   ; Put cluster offset in array.

         inc  bx          ; Increment free cluster counter.

         cmp  bx,cluscont ; Enough free clusters yet?
         jl   doagain   ; If no, then find more else

         add  di,2                ; Put an EOF in end value for later
         mov  theclust[di],0fffh  ;  use.

         ret
         ENDM
;--------------------------------------------------------------------------
; The main body of the create file ("docrea") procedure.
;
creamac  MACRO
         clrbuf startscr  ; Set up screen buffer.
         mov  themod,0    ; Offset in screen buffer will be zero -- this
                          ;   is used in copscrn, its needed for append.
         copscrn          ; Copy screen into a intermediate buffer.

         mov  cluscont,2  ; We need two clusters to hold the screen.
         call loadclus    ; Get the free cluster values.

         mkdirent        ; Set up ne directory entry.
         call resclus    ; Patch the necessary fat entries.

         mov  dx,theclust[0]   ; Write out the values to the disk.
         sub  dx,2
         shl  dx,1
         add  dx,12
         putsecs dx,2,copbuff  ; First cluster of screen dump.

         mov  dx,theclust[2]
         sub  dx,2
         shl  dx,1
         add  dx,12
         putsecs dx,2,copbuff2  ; Second cluster of screen dump.

         putsecs 1,2,fat1       ; Now write out fats and directory.
         putsecs 3,2,fat2
         putsecs 5,7,directory

         putcon 1,1,startscr    ; Inform user of the file's creation.
         putcon 3,1,creahead
         putcon 24,3,key_prompt
         getch

         call  showdir        ; Redisplay directory with new value.
         ret
         ENDM
;--------------------------------------------------------------------------
; This macro is the main body of the procedure resclus, its purpose is to
; assign the correct values to the free cluster entries it the FAT that were
; identified in a call to loadclus.
resclmac MACRO
         LOCAL again,noshif
         xor  bx,bx        ; Index into the free cluster array.
         mov  cx,cluscont  ; count of how many clusters to grab.
again:
         mov  dx,theclust[bx+2]  ; Cluster value needs to point to next
                                 ; available cluster.
         mov  si,clusptr[bx]     ; Cluster's offset.
         mov  ax,[si]

         shr  bx,1              ; Divide by two for byte address.
         cmp  clustand[bx],TRUE
         jne  noshif
         shlm dx,4         ; Line up value for write to fat table.
noshif:
         or   ax,dx       ; Since value was zero then an "or" is all that
                          ; is needed.
         mov  [si],ax     ; Put value in table.

         mov  di,si       ; Copy value into second fat table also.
         add  di,1024
         mov  [di],ax

         shl  bx,1        ; Shift index back to correct value and go to next
         inc  bx          ;  word entry.
         inc  bx
         loop again       ; Loop till done, cx was initialized to cluscont.
         ret
         ENDM
;--------------------------------------------------------------------------
; This macro modifies the proper fields of the directory entry for the
; append operation and determines how many more clusters will be needed
; for the append (either 1 or 2).
mdirent  MACRO
         LOCAL found,again,nothuge,needtwo
         getime
         mov  curdir.time,ax   ; Load in new time and date.
         getdate
         mov  curdir.date,ax

         mov  cluscont,2       ; Initialize to 2 clusters needed, change if
                               ; necessary.
         xor  dx,dx
         mov  ax,word ptr curdir.siz  ; Get low word of size, since cluster
                                      ;  size is always evenly divisible by
                                      ;  high word then the modulus of the
                                      ;  low word is all that is needed.
         mov  cx,1024                 ;
         div  cx
         mov  themod,dx               ; Divide to obtain modulus.
         cmp  dx,(1024*2-1722)    ; Compare to see if new value needs one
                                  ;  or two clusters.
         ja   needtwo
         dec  cluscont      ; If only one more is needed then decrement
                            ; cluster count.
needtwo:

         add  word ptr curdir.siz,1722  ; Add new data to file size.
         jnc  nothuge
         inc  word ptr curdir.siz+2    ; Carry value if necessary.
nothuge:
         lea  si,curdir        ; Copy results into directory.
         mov  di,dirpoint
         mov  cx,32
         rep  movsb
         ENDM
;--------------------------------------------------------------------------
; The main body of the append routine.
appemac  MACRO
         LOCAL noshif,noand,abort,through
         cmp  curdir.entname[0],0e5h     ; Check for deleted entry.
         jne  $+5
         jmp  abort
         cmp  curdir.entname[0],0aeh     ; Check for end of directory.
         jne  $+5
         jmp  abort
         savescn          ; Save the screen.
         clrbuf startscr
         mdirent        ; Modify directory entry and set cluscont and themod.
         call loadclus  ;  Determine free clusters.
         call resclus   ;  allocate free clusters.
         mov  ax,curdir.cluster  ; Now link last cluster of file to the
doagain:                         ;  newly allocated clusters.
         cmp  ax,0ff6h
         ja   done
         mov  lastclus,ax        ; Loop through the old file's list until
         mov  dx,ax              ;  EOF is encountered.  This logic is the
         shr  ax,1               ; same as the Blocks command.
         pushf
         add  ax,dx
         mov  si,ax
         lea  ax,fat1
         add  si,ax
         lodsw
         popf
         jc   noand
         mov  bx,0f000h   ; Set up mask in case this is the EOF.
         and  ax,0fffh
         jmp  doagain
noand:
         mov  bx,000fh   ; Ditto last comment.
         shrm ax,4
         jmp  doagain

done:
         sub  si,2
         mov  ax,[si]     ; Go back to EOF and get value.

         mov  dx,theclust[0]  ; Get needed cluster value.
         and  ax,bx           ; Mask out the EOF.
         cmp  bx,000fh        ; Set flags.
         jnz  noshif
         shlm dx,4
noshif:
         or   ax,dx           ; Write new cluster value into register.
         mov  [si],ax    ; Write value into fat table.

         mov  dx,lastclus  ; I need last cluster number to append data.
         sub  dx,2
         shl  dx,1
         add  dx,12
         getsecs dx,2,copbuff  ; Read in the last clustor.

         dec  themod          ; Copy screen at proper offset in the buffer.
         copscrn
         putsecs dx,2,copbuff  ; Now copy the new sectors out to disk.

         mov  dx,theclust
         sub  dx,2
         shl  dx,1
         add  dx,12
         putsecs dx,2,copbuff2

         cmp  cluscont,2     ; If necessary copy the next cluster.
         jl   through

         mov  dx,theclust[2]
         sub  dx,2
         shl  dx,1
         add  dx,12
         putsecs dx,2,copbuff3

through:

         putsecs 1,2,fat1
         putsecs 3,2,fat2        ; Update fat tables and directory on disk.
         putsecs 5,7,directory

         putcon 1,1,startscr
         putcon 3,1,appehead    ; Write out screen.
         putcon 24,3,key_prompt
         getch
         restscn                ; Restore previous screen and put modified
         call  entline          ;  directory entry on the screen.
         putcon  row,1,linebuf
         invline
abort:
         ret
         ENDM
;--------------------------------------------------------------------------
; This macro takes a cluster value in ax and determines the offset in the
; fat and then reads the new cluster value from that location.  It then
; does the necessary anding or shifting to leave the next cluster value
; in the ax register.
getoffs  MACRO
         LOCAL noshif,noand
         mov  dx,ax
         shr  ax,1
         pushf
         add  ax,dx
         mov  si,ax
         lea  ax,fat1
         add  si,ax
         lodsw
         popf
         jc   noand
         and  ax,0fffh
         jmp  noshif
noand:
         shrm ax,4
noshif:
         ENDM
;--------------------------------------------------------------------------
; Main part of blocks routine.  Displays the blocks associated with the
; a file.
;
blocmac  MACRO
         LOCAL doagain,done
         savescn           ; Save screen and row.
         push row
         clrbuf startscr
         mov  row,9
         mov  col_count,8
         mov  ax,curdir.cluster
doagain:
         push ax
         numout ax,row,col_count,startscr   ; Traverse linked list until eof
         pop  ax                            ; is found and copy values into
         cmp  ax,0ff6h                      ; buffer as they are retrieved.
         ja   done
         getoffs
         add  col_count,FATWIDTH     ; Update column and row counters.
         cmp  col_count,72
         jl   doagain
         mov  col_count,8
         inc  row
         jmp  doagain
done:
         putcon 1,1,startscr  ; Write resulting value to screen and put the
         mov  row,3           ; directory entry whose blocks are used at the
         call entline         ; top of the screen.
         putcon row,1,linebuf
         putcon 1,1,menutop
         putcon 4,1,blochead
         putcon 24,3,key_prompt
         getch
         pop  row   ; Restore screen and row.
         restscn
         ret
         ENDM
;--------------------------------------------------------------------------
; This macro puts the directory entry displayed on the screen in reverse
; video.
invline  MACRO
         invatt <byte ptr row>,NAMECOL,NAMECOL+7
         invatt <byte ptr row>,EXTCOL,EXTCOL+2
         invatt <byte ptr row>,ARCCOL,ARCCOL+10
         invatt <byte ptr row>,HOURCOL,HOURCOL+7
         invatt <byte ptr row>,MONCOL,MONCOL+7
         invatt <byte ptr row>,CLUSCOL,CLUSCOL+2
         invatt <byte ptr row>,SIZCOL,SIZCOL+10
         ENDM
;--------------------------------------------------------------------------
; Clear the reverse video from a line.
;
clrline  MACRO
         clratt <byte ptr row>,NAMECOL,SIZCOL+10
         ENDM
;--------------------------------------------------------------------------
; Macro of procedure which is called when a dowm arrow key is pushed.
downmac  MACRO
         LOCAL notmax,notend,smalldir
         add  dirpoint,32   ; Increment directory pointer to the next entry.
         mov  si,dirpoint

         mov  cx,32       ; Load value into a buffer.
         lea  di,curdir
         rep  movsb

         cmp  byte ptr curdir.entname[0],0  ; Check for end of directory.
         jne  notend

         lea  si,endir     ; If it is the end then wrap around to "end"
         mov  dirpoint,si  ;  dummy entry.
         mov  cx,32
         lea  di,curdir
         rep  movsb
notend:

         clrline         ; Clear the previous line's attributes.
         inc  row
         mov  ax,row     ; Increment row and check for row overflow.
         cmp  ax,maxrow
         jle  notmax
         dec  row
         cmp  maxrow,22  ; If the row overflowed then check for long or
         jl   smalldir   ;  short directory.

         scroll 1,3,NAMECOL,22,SIZCOL+10  ; Scroll screen if long directory.
         call entline
         putcon row,1,linebuf
         jmp  notmax

smalldir:
         inc  row        ; else wraparound.
         dec  ax
         cmp  ax,maxrow
         jle  notmax
         mov  row,3
notmax:
         invline   ; Invert newline.

         ENDM
;--------------------------------------------------------------------------
; This macro is called if an up arrow key is pushed.  Conceptually it is
; very similar to the down arrow code.
;
upmac    MACRO
         LOCAL notmin,notstart,notend,smalldir

         sub  dirpoint,32
         mov  si,dirpoint  ; Decrement pointer value.

         mov  cx,32
         lea  di,curdir ; Copy value to buffer.
         rep  movsb

         cmp  byte ptr curdir.entname[0],0   ; Check for start of directory.
         jne  notstart
         lea  si,endir
notend:
         add  si,32
         cmp  byte ptr [si],0  ; If so then reset to last with a small
         jne  notend           ; search.

         sub  si,32    ; Back up since we overshot to find end.

         mov  dirpoint,si

         mov  cx,32        ; Now copy to buffer.
         lea  di,curdir
         rep  movsb
notstart:

         clrline
         dec  row     ; Now decrement row and scroll or wraparound
                      ;  if necessary.
         cmp  row,3
         jae  notmin
         inc  row
         cmp  maxrow,22
         jl   smalldir
         scroll 1,3,NAMECOL,22,SIZCOL+10,backward
         call entline
         putcon row,1,linebuf
         jmp  notmin
smalldir:
         mov  ax,maxrow
         inc  ax
         mov  row,ax
notmin:
         invline

         ENDM
;--------------------------------------------------------------------------
; This macro is performed if the right arrow key was pressed.
;
doright  MACRO
         LOCAL inrange

         clratt 24,3,78    ; Clear the previous reverse video attribute.
         inc  selected        ; Increment the selected value.
         cmp  selected,MAXSEL
         jle  inrange         ; Check for index out of range.
         mov  selected,0   ; Wraparound if it is.
inrange:
         mov  si,selected
         invatt 24,startcols[si],endcols[si] ; Put new selection in reverse.
         ENDM
;--------------------------------------------------------------------------
; Left arrow macro almost the same as above.
;
doleft   MACRO
         LOCAL inrange
         clratt 24,3,78
         dec  selected
         cmp  selected,0
         jge  inrange
         mov  selected,MAXSEL
inrange:
         mov  si,selected
         invatt 24,startcols[si],endcols[si]
         ENDM
;--------------------------------------------------------------------------
; Main menu function of code.  Reads in a character and uses jump table
; to determine which label to execute.  The scan value in the second byte
; is used to perform the jump since it is unique for each key on the
; keyboard.
;
domenu   MACRO
mainloop:
         getch   ; Get a key from BIOS.
         mov  dx,ax  ; Save value in case its needed later, actually it isn't
                    ; but this would make the code more expandable.
         and  ax,0FF00h  ; Clear low byte.
         shrm ax,7     ; Set up ax for word index.
         mov  si,ax
         jmp  mainjmp[si]
doenter:                    ; This label is executed when an enter key is
                            ;  key is pressed.
         mov  si,selected
         shl  si,1
         call calltab[si]   ; Use "selected" to index call table.
         jmp  mainloop
downarr:                ; Down arrow key label.
         downmac
         jmp  mainloop
uparr:                  ; Up arrow key label.
         upmac
         jmp  mainloop
rightarr:               ; Rigth arrow key label.
         doright
         jmp  mainloop
leftarr:                ; Left arrow key label.
         doleft
         jmp  mainloop
errkey:                 ; Any other key label (except the Escape key).
         call dohelp
         jmp  mainloop
         ENDM
;--------------------------------------------------------------------------
;***************************************************************************
; The main procedure!
;***************************************************************************

codeseg       segment
main      proc far

          setup      ;Perform set up operations for an EXE file.

          putcon 1,1,startscr  ; Write header screen.
          putcur 26,1          ; get rid of cursor
          getch                ; wait for key.

          getsecs 0,1,bootrec  ; Get boot record from disk.
          checkboot            ; Check validity.
          getsecs 1,2,fat1     ; Get fat tables.
          getsecs 3,2,fat2
          getsecs 5,7,directory  ; Get root directory.

          call showdir  ; Display the directory and initialize variables.
          domenu        ; Go interactive!

          endexe     ; Return to operating system.

main      endp

showdir   proc   near ; Display the directory and initialize variables.
          dirmac
showdir   endp

dohelp    proc near    ; Show help screen.
          savescn
          putcon 1,1,helpscr
          getch
          restscn
          ret
dohelp    endp

doboot    proc near    ; Show boot record information.
          bootshow
doboot    endp

dobloc    proc near    ; Show blocks allocated to a file.
          blocmac
dobloc    endp

dofat     proc near    ; Display fat table entries.
          fatmac
dofat     endp

docrea    proc near    ; Create a file with screen contents.
          creamac
docrea    endp

dodele    proc near    ; Delete a file from the directory.
          delemac
dodele    endp

doappe    proc  near   ; Append a copy of screen to file.
          appemac
doappe    endp

dotrun    proc  near   ; Truncate a file to 3000 bytes.
          trunmac
dotrun    endp

doquit    proc  near   ; Pop return address off of stack and then Quit.
          pop  ax
          jmp leave
doquit    endp

donmout   proc near    ; Write a 16 bit decimal number to buffer.
          theasc
donmout   endp

dodoub    proc near   ; Write a 32 bit number to buffer.
          doubmac
dodoub    endp

thevid    proc near  ; A direct video memory output routine.
          videout
thevid    endp

difatt    proc near  ; A direct video memory attribute changing routine.
          attmac
difatt    endp

entline   proc near  ; Copy directory entry into a line buffer.
          entmac
entline   endp

killist   proc near  ; Frees the entries in the FAT table associated with a
                     ;  file.
          killmac
killist   endp

loadclus  proc near  ; Finds next available free clusters.
          clusmac
loadclus  endp

resclus   proc near  ; Assigns values to free clusters.
          resclmac
resclus   endp

codeseg   ends

;***************************************************************************
; Declaration of stack segment.
;***************************************************************************

st_seg   segment stack
         db   10000 dup (0)
st_seg   ends

              end  start   ;End of assembly.
