PAGE    70, 140
TITLE   MP1.ASM         Your Name       Today's Date

COMMENT #
        This program computes the weekly paychecks of employees at UIUC
        based on their respective hourly wage, the number of hours that
        they have logged, and overtime wage if it applies.
        #

;********** SECTION 1:  Define Constants ****************************

        CR      EQU     0Dh
        LF      EQU     0Ah
        SPA     EQU     20h
        RECLEN  EQU     18      ; Length of each record is constant


;********** SECTION 2:  Declare External Procedures *****************

;       Functions in LIB291.LIB
;       These functions are free to be used by you.

        extrn binasc:near, dspmsg:near, dosxit:near, dspout:near
        extrn kbdine:near

        ;       Complete descriptions of the LIB291 functions can be
        ;       found in your lab manuals.  Use these functions for
        ;       displaying output on the screen.

        extrn mp1xit:near            ; Terminates program

;       Functions in LIBMP1.LIB
;       You will need to write these functions for this program.
        extrn  LIBCalcOT:near
        extrn  LIBCalcTP:near
        extrn  LIBPrintD:near
        extrn  LIBPrintT:near
        extrn  LIBPrintID:near
        extrn  LIBHandleA:near
        extrn  LIBHandleB:near


;********* SECTION 3:  Define Stack Segment **************************

stkseg  segment stack           ; STACK SEGMENT
        db      64 dup ('STACK  ')
stkseg  ends

;********* SECTION 4:  Define Code Segment **************************

cseg segment public 'CODE'
        assume cs:cseg, ds:cseg, ss:stkseg, es:nothing

;********* SECTION 5:  Declare Variables for Main Procedure **********
;  The format for the time worksheet database:
;  Each record is 18 bytes long.
;       Emp_ID:         4 ASCII letters or numbers + '$'        = 5 bytes
;         Wage:         1-byte unsigned integer                 = 1 byte
;          OTr:         1-byte unsigned integer                 = 1 byte
;    Sun...Sat:         1-byte unsigned integers (7 elements)   = 7 bytes
;          OTp:         2-byte unsigned integer                 = 2 bytes
;         Totp:         2-byte unsigned integer                 = 2 bytes
;
;       ID,'$',Wage,OT_rate,Sun,Mon,Tue,Wed,Thu,Fri,Sat,OT_pay,Total_pay

INCLUDE time.dta

PUBLIC time
PUBLIC numrec
PUBLIC PrintT
PUBLIC PrintID

;  INCLUDE places contents of file time.dta here.
;  This file has two variables defined:  time and numrec.
;
;  time is an array of 18-byte records.
;  numrec is defined as a 16-bit integer that stores the number of records.

;  head is the HEADER that needs to be the first line printed out.
head db '-ID-- '
     db ' Wage OTr Sun Mon Tue Wed Thu Fri Sat   OTp      Totp',CR,LF,'$'

Hdr2 db 'Emp_ID    OT_Pay   Total_Pay',CR,LF,'$'

entr  db CR,LF,'$'              ;   Mimics hitting the enter key
space db SPA,SPA,SPA,'$'        ;   Types 3 Spaces

Menu  db CR,LF,'What info do you want?',CR,LF
      db '(a) Employee ID.',CR,LF
      db '(b) How much OT pay and Total Pay for Employee(0-9).',CR,LF
      db '(c) Display database.',CR,LF
      db '    Press ESC key to return to the DOS prompt.',CR,LF,'$'

EmpMsg db 'Enter the number (0-9) of the employee that you want.',CR,LF,'$'
InvalidMesg db 'Invalid Input.  Choose Again.',CR,LF,'$'

buff  db 7 dup(?)               ;   Temporary buffer used by BINASC lib291 call

;********** SECTION 6:  Main Procedure ****************************

main    proc    far
        mov     ax,cseg
        mov     ds,ax                   ; Initialize ds = cs

        call    CalcOT

        call    CalcTP

Refresh:
        call    PrintHdr

        call    PrintRec

        call    Ent             ;  Prints CR and LF to screen

Display:
        call    DisplayMenu;

        call    kbdine
        call    Ent

        cmp     al,1Bh          ; Was Escape key pressed?
        je      Exit

        cmp     al,'a'
        jne     NOTa
        call    HandleA
        jmp    Display
NOTa:   cmp     al,'b'
        jne     NOTab
        call    HandleB
        jmp     Display
NOTab:
        cmp     al,'c'
        jne     InvalidIO
        jmp     Refresh
InvalidIO:
        call    InvalidInput
        call    Ent
        jmp     Display

Exit:   call mp1xit
main    endp

;  Procedure to print three spaces based on definition of space
PrintSpace PROC NEAR
        mov     dx, OFFSET space
        call    dspmsg
        ret
PrintSpace ENDP

;  Procedure to print CR,LF
Ent PROC NEAR
        mov     dx, OFFSET entr
        call    dspmsg
        ret
Ent ENDP

;  Procedure to print Hdr2 for HandleB function
PrintHdr2       PROC    NEAR
        call    Ent
        mov     dx, OFFSET Hdr2
        call    dspmsg
        ret
PrintHdr2       ENDP

;  Procedure to display Menu message
DisplayMenu     PROC    NEAR
          mov     dx, OFFSET Menu
          call    dspmsg
          call    Ent
          ret
DisplayMenu     ENDP

InvalidInput PROC NEAR
        call    Ent
        mov     dx, OFFSET InvalidMesg
        call    dspmsg
        ret
InvalidInput    ENDP

PrintHdr PROC   NEAR
        mov     dx, cs:OFFSET head
        call    dspmsg
        call    Ent
        ret
PrintHdr        ENDP

;  PLEASE NOTE: You may choose to use this skeleton code for PrintRec to print
;               your database records; but you are forewarned that bp and di
;               are changed in the loop.  So you can either incorporate
;               them into your procedures or simply replace PrintRec
;               with your own code to print out the database records.
;               Just comment out the code in PrintRec and replace with your own.
;               Also, do not worry to much about the spacing as long
;               as your output correctly reflects the database records.
PrintRec  PROC  NEAR
        mov     bp,0
        mov     di,0
PrintL:
        call    PrintID
        call    PrintD
        call    PrintT
        call    Ent

        add     bp,RECLEN
        inc     di
        cmp     di,WORD PTR [numrec]
        jnz     PrintL
        ret
PrintRec        ENDP

;******************************************************************
;***************  You need to code these functions ****************
;******************************************************************
CalcOT  PROC    NEAR
        push    ax
        push    bx
        push    cx
        push    dx
        mov     bx, OFFSET time
        mov     cx, numrec
NextRec:
        mov     al, [bx+7]              ; Sunday hours
        add     al, [bx+13]             ; Saturday hours
        mul     BYTE PTR [bx+5]         ; Wage
        mov     dl, [bx+6]              ; make OT_rate 16-bit
        xor     dh,dh
        mul     dx                      ; OT_rate
        mov     [bx+14], ax             ; store result
        add     bx,18                   ; next element
        loop    NextRec
        pop     dx
        pop     cx
        pop     bx
        pop     ax
        ret
CalcOT          ENDP

CalcTP  PROC    NEAR
        call    LIBCalcTP  ;  Comment this function out; Insert your code here.
        ret
CalcTP          ENDP


HandleA PROC    NEAR
        call    LIBHandleA     ;   Comment this out.  Insert your function here.
        ret
HandleA         ENDP

HandleB PROC    NEAR
        call    LIBHandleB     ;   Comment this out.  Insert your function here.
        ret
HandleB         ENDP

PrintID PROC    NEAR
        call    LIBPrintID     ;   Comment this out.  Insert your function here.
        ret
PrintID         ENDP

PrintD  PROC    NEAR
        call    LIBPrintD      ;   Comment this out.  Insert your function here.
        ret
PrintD          ENDP

PrintT  PROC    NEAR
        call    LIBPrintT      ;   Comment this out.  Insert your function here.
        ret
PrintT          ENDP

cseg    ends
        end     main
