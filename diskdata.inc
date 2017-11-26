
;***************************************************************************
; Declarations for the data segment.
;***************************************************************************

datarea  segment

; Save the old stack pointer value so that if I abort after a disk error
; the system stack is preserved.
oldsp         dw  ?

; Used to store the address of an error message if the program is aborted.
outmessaddr   dw   0

; Drive designator, default to drive A if no drive specfied on command line.
thedriv       db   0

; Indicator of number of rows on screen,  22 if long directory.
maxrow        dw   0

; Used to store a pointer into the fat table.
fatpoint      dw   ?

; Used to indicate whether the last entry from the fat table was anded or
; shifted.
isand         db   0

; A counter for the cluster number in the fat table.
fatcount      db   0

; Used in "FAT" and "Blocks" to keep track of what column to display cluster
; number.
col_count     dw   ?

; Used in numerous places as a row counter.
row           dw   ?

; Pointer into the directory data area.  Points to current (the one in
; reverse video) directory entry.
dirpoint      dw   ?

; Used in video routines to make part of screen reverse video.
invmask       dw   ?
; Used in video routines to retrun screen to normal attribute.
clrmask       dw   ?

; Keeps track of which command is in reverse video.
selected      dw   0

; Used to count through entries clusters for "Trunc" command.
truncount     db   0

; Short for the MODulus, this is used to indicate how deep in the segment
; to start copying the screen when appending data to a file.  Its value
; will be the modulus of filezize/clustersize.
themod        dw   0

; Table used to store address of commands.  The "selected" variable above is
; used as an index into this table when "Enter" is pressed.
calltab       dw   dohelp,doboot,dobloc,dofat
              dw   docrea,dodele,doappe,dotrun,doquit

; Tables indicating where to start and stop inverting a selected command.
; This is also indexed by the "selected" variable.
startcols label byte
db  4,11,22,31,37,46,55,64,72

endcols   label byte
db  9,20,29,35,44,53,62,70,77

; Keyboard jump table, this table is indexed by the scan code of the key
; returned from the keyboard BIOS call.  Used for main conditional branch
; in the menu.
mainjmp       dw   256 dup(errkey)  ;Initialize entire array to error label.
org           mainjmp + 2*01h       ;Patch "Esc" key.
              dw   leave
org           mainjmp + 2*1Ch       ;Patch "Enter" key.
              dw   doenter
org           mainjmp + 2*50h       ;Patch "Down arrow" key.
              dw   downarr
org           mainjmp + 2*48h       ;Patch "Up arrow" key.
              dw   uparr
org           mainjmp + 2*4Dh       ;Patch "Right arrow" key.
              dw   rightarr
org           mainjmp + 2*4Bh       ;Patch "Left arrow" key.
              dw   leftarr
org           mainjmp + 2*256       ;Skip to end of table.


key_prompt    db   'Press any Key to continue',0 ;Self explanatory.

video_base    dw   ?            ;Base address of video memory.
theatt        db   ?            ;The attribute used for video display.

comlindata    db   128 dup(0)   ;Extra copy of command line data.

; The radix used for decimal conversions.
radx         	dw   10
              db   ?     ;dummy value for indexing convenience

wordbuf       db   20 dup(0) ;Buffer used to store converted number.
three         db   3      ; These are temporary values used in ASCII
field         db   0      ; conversion.

numa          db   10 dup(0) ;Temporary buffers used for 32 bit integer
numd          db   10 dup(0) ;to ASCII confersion.

; Multiplication value used in conversion of 32 bit integers, notice the
; digits read from right to left "65536" or hex 10000.
thm           db   0,0,0,0,6,3,5,5,6,0,0,0,0,0,0,0,0,0

; Used to indicate the end of the directory.
endmess       db   'ฎ End ฏ',0

; These error messages are if a disk error occurs.
err_mess7     db   'Absolute disk read error!',0
err_mess8     db   'Absolute disk write error!',0

; Buffer containing the current value of the directory entry.
curdir        dirstruc <>

; Value of default directory entry when creating a new file.
newdir        label dirstruc
db            'DIRECTRY'
db            '   '
db            20h
db            10 dup (0)
dw            0
dw            0
dw            0
dd            1722

; The data area in which the boot record is loaded.
bootrec       bootstruc <>

; Dummy value used to check for the proper disk type for this program.
dummyboot     label byte
dw            512
db            2
dw            1
db            2
dw            112
dw            720
db            0fdh
dw            2

; The following structures are used together to hold the directory and
; ending indicators.
              dirstruc <>      ; Needed to check for upper bound.
endir         label  dirstruc  ; Needed to show the user the end of directory.
db            'ฎ End ฏ',0
db            24 dup (0)
directory     dirstruc 112 dup (<>)  ;The main directory buffer.

; The two fat table buffers.
EVEN
fat1          dw   512 dup (0)
fat2          dw   512 dup (0)

; These buffers are used to copy data from the screen and from the disk in
; the create and append routines.
copbuff  db 512*2 dup (26) ;End of file markers
copbuff2 db 512*2 dup (26)
copbuff3 db 512*2 dup (26)

; Lastclus is used to determine which clusters to patch in several
; of the routines.
lastclus dw 0

; Cluscont is used to indicate how many clusters are required when expanding
; or creating a file.
cluscont dw 0

; These three values are loaded in the procedure loadclus.  They are the
; proper values for the next unused cluster in the FAT table.  Clustand is
; a boolean to indicate whether the cluster was anded or shifted.  Theclust
; is the cluster value and clusptr is its location in memory.
clustand db 5 dup (0)
theclust dw 5 dup (0)
clusptr  dw 5 dup (0)

; FAT table cluster counter.
fatcont  dw ?

; This buffer is used to copy the directory values into before copying.
linebuf   label byte
db 'บ                                                                              บ',0

; The startscreen is a originally displayed to orient the user and is used as
; a general screen buffer after that.
startscr  label byte
db 'ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป'
db 'บ   Programming assignment 3 in CS-490 by Derek Dickinson.                     บ'
db 'บ                                                                              บ'
db 'บ   This program performs several functions:                                   บ'
db 'บ                                                     Command                  บ'
db 'บ   - Displays information about the boot record.    "Boot Rec"                บ'
db 'บ   - Displays the blocks allocated to a file.       "Block"                   บ'
db 'บ   - Dislays the File Allocation Table              "FAT"                     บ'
db 'บ   - Creates a file with the current screen as      "Create"                  บ'
db 'บ     contents.                                                                บ'
db 'บ   - Deletes a file.                                "Delete"                  บ'
db 'บ   - Appends a copy of the screen to a file.        "Append"                  บ'
db 'บ   - Truncates a file to 3000 bytes.                "Trunc"                   บ'
db 'บ                                                                              บ'
db 'บ   The following screen will be a display of the disk''s directory with a list บ'
db 'บ   of commands across the bottom.  Press "Enter" when this screen appears to  บ'
db 'บ   get an explanation of how to operate the program.                          บ'
db 'บ                                                                              บ'
db 'บ   The directory entries on the following screen which begin with the "ๅ" are บ'
db 'บ   deleted files.  Attempting to "Delete", "Trunc" or "Append" to these files บ'
db 'บ   will have no affect!                                                       บ'
db 'บ                                                                              บ'
db 'บ   Press any key to continue                                                  บ'
db 'บ                                                                              บ'
db 'ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ',0

helpscr  label byte ; Self explanatory
db 'ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป'
db 'บ                                                                              บ'
db 'บ   Help!                                                                      บ'
db 'บ                                                                              บ'
db 'บ   If you didn''t look closely at the previous screen then go look at it now.  บ'
db 'บ                                                                              บ'
db 'บ   You should notice that a directory entry and a command are in reverse      บ'
db 'บ   video.  The up and down arrow keys change which directory entry is in      บ'
db 'บ   reverse video and the right and left arrow keys change which command is    บ'
db 'บ   in reverse video.                                                          บ'
db 'บ                                                                              บ'
db 'บ   Pressing the "Enter" will perform the command which is in reverse video.   บ'
db 'บ   If the command is to be performed is on a specific entry then the entry    บ'
db 'บ   in reverse video will be used.                                             บ'
db 'บ                                                                              บ'
db 'บ   The only other command that the program will accept is the "Esc" key       บ'
db 'บ   which will exit you from the program.                                      บ'
db 'บ                                                                              บ'
db 'บ   If you type an incorrect key then this screen will pop back up to remind   บ'
db 'บ   you which keys to use.                                                     บ'
db 'บ                                                                              บ'
db 'บ                                                                              บ'
db 'บ   Press any key to continue                                                  บ'
db 'บ                                                                              บ'
db 'ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ',0


fathead   label byte
db 'บ  Fat Table Display,  Cluster number = row#*10+col#                           บ'
db 'บ          0      1      2      3      4      5      6      7      8      9    บ'
db 'บ         ฤฤฤ    ฤฤฤ    ฤฤฤ    ฤฤฤ   ฤฤฤ     ฤฤฤ    ฤฤฤ    ฤฤฤ    ฤฤฤ    ฤฤฤ   บ',0
;;;;บ         xx0    xx1    xx2    xx3    xx4    xx5    xx6    xx7    xx8    xx9   บ'

bootscr   label byte
db 'ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป'
db 'บ                                                                              บ'
db 'บ   Boot Record Information, Drive                                             บ'
db 'บ                                                                              บ'
db 'บ   The jump instruction starting boot record :          hex                   บ'
db 'บ                                    OEM name :                                บ'
db 'บ                              Bytes / Sector :                                บ'
db 'บ                           Sectors / Cluster :                                บ'
db 'บ                            Reserved Sectors :                                บ'
db 'บ                        Number of Fat Tables :                                บ'
db 'บ            Number of Root Directory Entries :                                บ'
db 'บ                               Total Sectors :                                บ'
db 'บ                            Media Descriptor :    hex                         บ'
db 'บ                         Sectors / FAT Table :                                บ'
db 'บ                             Sectors / Track :                                บ'
db 'บ                             Number of Heads :                                บ'
db 'บ                    Number of Hidden Sectors :                                บ'
db 'บ                                                                              บ'
db 'บ                                                                              บ'
db 'บ                                                                              บ'
db 'บ                                                                              บ'
db 'บ                                                                              บ'
db 'บ  Press any Key to continue                                                   บ'
db 'บ                                                                              บ'
db 'ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ',0


menutop   label byte
db 'ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป'
db 'บ  Name     Ext     A D V S H R     Time       Date      Clust      Size       บ',0

blochead  label byte
db 'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
db 'บ The file above occupies the clusters listed below.  These clusters are       บ'
db 'บ listed in the order in which the file is stored.   The final cluster is      บ'
db 'บ merely an end-of-file marker, it does not point to another cluster.          บ',0

creahead  label byte
db 'บ A file named "DIRECTRY" has just been created.  It contains the contents of  บ'
db 'บ the previous screen.  The first line and the last three lines were excluded  บ'
db 'บ from the file.                                                               บ',0

badboot   label byte
db 'บ The boot record of this disk does not match the expected format.  The        บ'
db 'บ boot record information will be displayed and then the program will exit.    บ'
db 'บ This program will work on 9 sector double sided diskets.                     บ',0

appehead  label byte
db 'บ The contents of the screen (excluding the first and last three rows) was     บ'
db 'บ just appended onto the entry which was in reverse video.  You will observe   บ'
db 'บ that the filesize is now 1722 bytes larger.                                  บ',0

delehead  label byte
db 'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
db 'บ                                                                              บ'
db 'บ Press "Y" (or "y") to delete the above file.  Any other key will abort       บ'
db 'บ delete.                                                                      บ',0

trunhead  label byte
db 'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
db 'บ                                                                              บ'
db 'บ Press "Y" (or "y") to truncate the above file down to 3000 bytes.            บ',0

menubot   label byte
db 'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
db 'บ   Help   Boot Rec   Blocks   FAT   Create   Delete   Append   Trunc   Quit   บ'
db 'ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ',0

datarea  ends