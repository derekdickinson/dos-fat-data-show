
;***************************************************************************
;  Structure definitions.
;
;
;***************************************************************************

; This is the structure of the Boot record.
bootstruc  STRUC
thejump    db 3 dup (0)  ; Jump instructions to boot routines.
oemname    db 8 dup (0)  ; The OEM's name.
bytprsec   dw ?          ; The number of bytes / sector
secprclus  db ?          ;  "      "     sectors / cluster
resecs     dw ?          ; Reserved sectors.
numfats    db ?          ; Number of fat tables.
rootents   dw ?          ; The number of root directory entries.
totsecs    dw ?          ; Total number of sectors.
mediadesc  db ?          ; Media descriptor byte.
secsprfat  dw ?          ; Sectors per fat table.
secsprtrak dw ?          ; Sectors per track.
numheads   dw ?          ; Number of heads.
numhids    dw ?          ; Number of hidden sectors.
           db 1000 dup (?)  ; reserved area.
bootstruc  ENDS

; The structure for a directory entry.
dirstruc   STRUC
entname    db 8 dup (0)  ; The name of the entry.
ext        db 3 dup (0)  ; extension
att        db ?          ; Attribute byte
reserved   db 10 dup(?)  ; Unknown to me but DOS wants it.
time       dw ?          ; Just like it says, time.
date       dw ?          ; date
cluster    dw ?          ; Starting cluster of file in FAT table.
siz        dd ?          ; Size of file, high word first.
dirstruc   ENDS
