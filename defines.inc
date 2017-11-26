; This file contains the defines for the program.

VIDCOLOR   equ 0b800h   ; The base address of color video memory.
VIDMONO    equ 0b000h   ; The base address of monochrome video memory.
COLORATT   equ 1eh      ; Color screens video attribute.
CR         equ 13       ; The ASCII code for carraige return.
LF         equ 10       ; The ASCII code for line feed.
EOF        equ 26       ; The End Of File marker.

FALSE      equ 0
TRUE       equ 1

MAXSEL     equ 8       ; Maximum value of selected.

FIRSTFAT   equ 11      ; Column value of first fat table entry when displayed
                       ; "FAT".
FATWIDTH   equ 7       ; Columns per fat table entry.
LASTFAT    equ 75      ; Last valid fat table column.

HOURMASK   equ 0f800h  ; Shift and mask values for decoding the different
HOURSHIF   equ 0bh     ; fields of the directory's time and date.
MINMASK    equ 07e0h
MINSHIF    equ 5h
SECMASK    equ 001fh
SECSHIF    equ 0

YEARMASK   equ 0fe00h
YEARSHIF   equ 9h
MONTHMASK  equ 01e0h
MONTHSHIF  equ 5h
DAYSMASK   equ 001fh
DAYSSHIF   equ 0

NAMECOL    equ 4    ; The columns in which the different fields are displayed.
EXTCOL     equ 13
ARCCOL     equ 21
DIRCOL     equ 23
VOLCOL     equ 25
SYSCOL     equ 27
HIDCOL     equ 29
REOCOL     equ 31
HOURCOL    equ 35
MINCOL     equ 38
SECCOL     equ 41
MONCOL     equ 46
DAYCOL     equ 49
YEARCOL    equ 52
CLUSCOL    equ 59
SIZCOL     equ 65
