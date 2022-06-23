TITLE MP2:Infix-Calculator     Your_Name     Todays_Date
.MODEL SMALL
.386
COMMENT *
        In this MP, you will write a program which will take input
        from the keyboard which will be parsed and solved as an 
        arithmatic equation
        
        ECE291: Machine Problem 2
        Prof. Zbigniew Kalbarczyk
        Guest Author: Brandon Tipp
        Univeristy of Illinois
        Dept. of Electrical & Computer Engineering
        Fall 1999
        
        Ver 1.0
        *

;====== SECTION 1: Define constants =======================================
;misc ASCII codes
BEL     EQU     007h
BS      EQU     008h
CR      EQU     00Dh
LF      EQU     00Ah
SPACE   EQU     020h
ESCKEY  EQU     01Bh

MAX_BUFF_LEN EQU 80  ; Bytes; limit input to one line
NUM_ACCEPT   EQU 33  ; The number of acceptable characters

; Definitions for the control string
NULL       EQU  00
STARTNUM   EQU  78
ENDNUM     EQU  110
ENDSTR     EQU  36
NEGATE     EQU  0C4h ; ascii for a hyphen

; Definitions for parsing input Buff
NUMWANTED  EQU  00h  ; code represents an expected number token

PUBLIC BEL, BS, CR, LF, SPACE, ESCKEY
PUBLIC MAX_BUFF_LEN, NUM_ACCEPT
PUBLIC NULL, STARTNUM, ENDNUM, ENDSTR, NEGATE


; GetOperands MACRO
; This macro may be used and edited freely.  Edit this comment
; block to reflect changes
; Purpose: Get the left and right operands to an operation.
;          Nullifies the right operand
; Inputs: X, the place to store the left operand  (must be a register, not bp, di, si, or bx)
;         Y, the place to store the right operand (must be a register, not bp, di, si, or bx)
; Ouputs: X, holds the number that was indicated by GetOp1
;         Y, holds the number that was indicated by GetOp2
;         The number indicated by GetOp2 is nullified from the string
;         BP , the return value from GetOp1, indicating where the number is
; Notes:  Performs no error checking.
GetOperands MACRO x, y 
        call GetOp2
        mov y, word ptr controlStr[bp + 1]
        mov word ptr controlStr[bp],  NULL
        mov word ptr controlStr[bp + 2], NULL
        call GetOp1
        mov x, word ptr controlStr[bp + 1]
endm


;====== SECTION 2: Declare external procedures ============================

EXTRN   kbdine:near, dspout:near, dspmsg:near, binasc:near, ascbin: near
EXTRN   mp2xit:near, kbdin:near 

;====== Library procedures ================================================
;Free Library procedure
EXTRN  LibDspCtrlStr:near

;The follwing library procedures must be replaced with your own code.
EXTRN   LibInput:near
EXTRN   LibCheckParens:near
EXTRN   LibParse:near
EXTRN   LibCheckDone:near
EXTRN   LibFindInner:near
EXTRN   LibFindOne:near
EXTRN   LibSolveOne:near
EXTRN   LibGetOp1:near
EXTRN   LibGetOp2:near

;These procedures need to be public so the library may call them
PUBLIC  GetOp1, GetOp2

;====== SECTION 3: Define stack segment ===================================

STKSEG SEGMENT STACK                    ; *** STACK SEGMENT ***
        db      64 dup ('STACK   ')
STKSEG ENDS

;====== SECTION 4: Define code segment ====================================

CSEG SEGMENT PUBLIC 'code'              ; *** CODE SEGMENT ***
ASSUME  cs:cseg, ds:cseg, ss:stkseg, es:nothing

;====== SECTION 5: Declare variables for main procedure ===================
inputBuff      db MAX_BUFF_LEN dup (?), '$'
controlStr     db MAX_BUFF_LEN * 3 dup (?), ENDSTR

buff     db 7 dup(?)

accept   db '0123456789!*/%+-(){}[] <>&|^~',BS,CR,LF,ESCKEY ; all acceptable input chars

inputMsg db CR,LF,'Please enter an equation.',CR,LF
         db 'Hit <ESC>, or enter an empty equation to exit.',CR,LF,'$'

parseMsg db 'Verifying and parsing the string...',CR,LF,'$'

; A lookup table of the priority of each of the operations
priority db 33 dup (0) ; control characters
         db 16         ; !
         db 0, 0, 0    ; ", #, $
         db 14         ; %
         db 8          ; &
         db 0          ; '
         db 2          ; (
         db 0          ; )
         db 14         ; *
         db 12         ; +
         db 0          ; ,
         db 12         ; -
         db 0          ; .
         db 14         ; /
         db 12 dup (0) ; numerals, :, ;
         db 10         ; <
         db 0          ; =
         db 10         ; >
         db 31 dup (0) ; ?, @, caps, [, \, ]
         db 6          ; ^
         db 29 dup (0) ; _, ', lower case letters, {
         db 4          ; |
         db 0          ; }
         db 17         ; ~
         db 69 dup (0) ; DELete, the extended charaters
         db 17         ; Long dash for negate
         db 59 dup (0) ; the rest of the extended characters

errMsg   db BEL,'The equation was not fully simplified due to an error.',CR,LF,'$'
errMsg0  db BEL,'Type 0 error: Divide by zero.',CR,LF,'$'
errMsg1  db BEL,'Type 1 error: Parenthesis mismatch.',CR,LF,'$'
errMsg2  db BEL,'Type 2 error: Input overflow.',CR,LF,'$'
errMsg3  db BEL,'Type 3 error: Calculation overflow.',CR,LF,'$'
errMsg4  db BEL,'Type 4 error: Missing operand',CR,LF,'$'
errMsg5  db BEL,'Type 5 error: Encountered a unary operator while expecting a binary operator.',CR,LF,'$'
errMsg6  db BEL,'Type 6 error: Encountered a number while expecting a binary operator.',CR,LF,'$'
errMsg7  db BEL,'Type 7 error: Encountered a binary operator while expecting a number.',CR,LF,'$'
errMsg8  db BEL,'Type 8 error: Encountered end of string while expecting a number.',CR,LF,'$'
errMsg9  db BEL,'Type 9 error: No numbers found in control string.',CR,LF,'$'
errMsg10 db BEL,'Type 10 error: Unknown operation encountered.',CR,LF,'$'
errMsg11 db BEL,'Type 11 error: Attempting to resolve parenthesis containing too many operands.',CR,LF,'$'
errMsg12 db BEL,'Type 12 error: Cannot take the factorial of a negative number.',CR,LF,'$'

public inputBuff, controlStr
public inputMsg, parseMsg, priority, buff, accept
public inputMsg, errMsg0, errMsg1, errMsg2, errMsg3, errMsg4, errMsg5
public errMsg6, errMsg7, errMsg8, errMsg9, errMsg10, errMsg11, errMsg12

;====== SECTION 6: Procedures ============================================


;Comment your funcitons!!!
Input proc near
     push    ax
     push    bx
     push    dx
     push    si
     push    di

     ;; al = read character
     ;; bx = base of accept array
     ;; dl = character to display
     ;; si = index into accept array
     ;; di = next location in inputBuff

     mov     dx, OFFSET inputMsg        ; display prompt for equation
     call    dspmsg

     mov     bx, OFFSET accept          ; init pointer to accept array
     mov     di, OFFSET inputBuff       ; init pointer to inputBuf array

  getchar:
     xor     si, si
     call    kbdin

  checkchar:
     cmp     si, NUM_ACCEPT             ; are we at end of accept array?
     je      badchar                    ; if yes, char is not valid
     cmp     al, BYTE PTR [bx+si]
     je      handlechar
     inc     si
     jmp     checkchar

  badchar:
     mov     dl, BEL
     call    dspout
     jmp     getchar

  handlechar:
     cmp     al, LF             ; handle line feed
     je      getchar
     cmp     al, ESCKEY         ; handle escape
     jne     handlecr
     stc
     jmp     Input_done

  handlecr:
     cmp     al, CR                     ; handle carriage return
     jne     handlebs
     mov     dl, al                     ; input carriage return should output
     call    dspout                     ; both a carriage return and a line
     mov     dl, LF                     ; feed
     call    dspout
     mov     BYTE PTR [di], '$'         ; string is done, so terminate it
     cmp     di, OFFSET inputBuff       ; handle empty string
     jne     Input_done
     stc
     jmp     Input_done

  handlebs:
     cmp     al, BS                     ; handle backspace
     jne     checkoverflow
     cmp     di, OFFSET inputBuff       ; check for underflow
     je      badchar
     mov     dl, al                     ; backspace needs to erase preceding
     call    dspout                     ; char with a SPACE character in
     mov     dl, SPACE                  ; addition to moving the cursor
     call    dspout                     ; backwards
     mov     dl, al
     call    dspout
     dec     di                         ; move pointer backwards
     jmp     getchar

  checkoverflow:
     cmp     di, OFFSET inputBuff + MAX_BUFF_LEN - 1    ; check for overflow
     je      badchar

  display:
     mov     dl, al
     call    dspout
     mov     BYTE PTR [di], al           ; put char in inputBuf
     inc     di                          ; advance pointer
     jmp     getchar

  Input_done:
     pop     di
     pop     si
     pop     dx
     pop     bx
     pop     ax
     ret
Input endp

;Comment your code!!!
CheckParens proc near
     push    dx
     push    di
     push    bp

     ;; di = next location in inputBuff
     ;; bp = stack frame

     mov     bp, sp                     ; start a stack frame
     mov     di, OFFSET inputBuff       ; move to start of inputBuff

  CheckParens_checkeos:
     cmp     BYTE PTR [di], '$'         ; check for end of string
     je      cleanup

  openparen:
     cmp     BYTE PTR [di], '('         ; if char is an open paren push it
     jne     closeparen                 ; onto the stack and advance to next
     push    '('                        ; char
     inc     di
     jmp     CheckParens_checkeos
  closeparen:
     cmp     BYTE PTR [di], ')'         ; if char is a close paren, first
     jne     openbrace                  ; ensure stack is not empty, then pop
     cmp     sp, bp                     ; char off stack and ensure parens
     je      CheckParens_error          ; match, otherwise throw an error
     pop     ax
     cmp     al, '('
     jne     CheckParens_error
     inc     di                         ; if parens match advance to next char
     jmp     CheckParens_checkeos

  openbrace:
     cmp     BYTE PTR [di], '{'         ; if char is an open brace push it
     jne     closebrace                 ; onto the stack and advance to next
     push    '{'                        ; char
     mov     BYTE PTR [di], '('         ; convert '{' to '(' in inputBuff
     inc     di
     jmp     CheckParens_checkeos
  closebrace:
     cmp     BYTE PTR [di], '}'         ; if char is a close brace, first
     jne     openbracket                ; ensure stack is not empty, then pop
     cmp     sp, bp                     ; char off stack and ensure braces
     je      CheckParens_error          ; match, otherwise throw an error
     pop     ax
     cmp     al, '{'
     jne     CheckParens_error
     mov     BYTE PTR [di], ')'         ; convert '}' to ')' in inputBuff
     inc     di                         ; if parens match advance to next char
     jmp     CheckParens_checkeos

  openbracket:
     cmp     BYTE PTR [di], '['         ; if char is an open bracket push it
     jne     closebracket               ; onto the stack and advance to next
     push    '['                        ; char
     mov     BYTE PTR [di], '('         ; convert '[' to '(' in inputBuff
     inc     di
     jmp     CheckParens_checkeos
  closebracket:
     cmp     BYTE PTR [di], ']'         ; if char is a close bracket, first
     jne     nextchar                   ; ensure stack is not empty, then pop
     cmp     sp, bp                     ; char off stack and ensure brackets
     je      CheckParens_error          ; match, otherwise throw an error
     pop     ax
     cmp     al, '['
     jne     CheckParens_error
     mov     BYTE PTR [di], ')'         ; convert ']' to ')' in inputBuff
  nextchar:
     inc     di                         ; if parens match advance to next char
     jmp     CheckParens_checkeos

  cleanup:
     cmp     sp, bp                     ; confirm no dangling parens on stack
     je      CheckParens_done

  CheckParens_error:
     mov     dx, OFFSET errMsg1
     call    dspmsg
     mov     sp, bp                     ; pop the stack frame
     stc

  CheckParens_done:
     pop     bp
     pop     di
     pop     dx
     ret
CheckParens endp

;Comment loops in your code!!!
Parse proc near
     push    ax
     push    bx
     push    dx
     push    di

     ;; ax = binary number result of ascbin procedure
     ;; bx = next location in inputBuff
     ;; dh = expected token, if dh == NUMWANTED, number expected
     ;; di = next location in controlStr

     mov     dh, NUMWANTED              ; begin by expecting a number
     mov     bx, OFFSET inputBuff       ; move to start of inputBuff
     mov     di, OFFSET controlStr      ; move to start of controlStr

  Parse_checkeos:
     cmp     BYTE PTR [bx], '$'         ; check for end of string
     jne     checknumber
     cmp     dh, NUMWANTED              ; string must end wanting an operator
     je      errencounteredeos
     mov     BYTE PTR [di], ENDSTR
     jmp     Parse_done

  checknumber:
     cmp     BYTE PTR [bx], '0'
     jb      checkminussign
     cmp     BYTE PTR [bx], '9'
     ja      checkminussign
     cmp     dh, NUMWANTED              ; are we expecting a number?
     jne     errencounterednum

  convertnumber:
     call    ascbin
     cmp     dl, 0                      ; check for ascbin error
     je      storenumber
     cmp     dl, 6                      ; check for undocumented status code
     je      storenumber
     jmp     errinputoverflow

  storenumber:
     mov     BYTE PTR [di], STARTNUM
     inc     di
     mov     WORD PTR [di], ax
     add     di, 2
     mov     BYTE PTR [di], ENDNUM
     inc     di
     not     dh                         ; expect operator as next token
     jmp     Parse_checkeos

  checkminussign:
     cmp     BYTE PTR [bx], '-'
     jne     checkopenparen
     cmp     dh, NUMWANTED
     jne     placeoperator              ; is minus sign; add it to controlStr
     cmp     BYTE PTR [bx+1], '$'       ; peek ahead...
     je      errencounteredop           ; eos means '-' is an operator :(
     cmp     BYTE PTR [bx+1], '('
     je      insertnegate               ; '(' means it's a negate operator
     cmp     BYTE PTR [bx+1], '-'
     je      insertnegate               ; '-' means it's a negate operator
     cmp     BYTE PTR [bx+1], '0'
     jb      errencounteredop
     cmp     BYTE PTR [bx+1], '9'
     ja      errencounteredop
     jmp     convertnumber

  insertnegate:
     mov     BYTE PTR [di], NEGATE	; add negate operator to controlStr
     inc     bx
     inc     di
     jmp     Parse_checkeos

  checkopenparen:
     cmp     BYTE PTR [bx], '('
     jne     checkcloseparen
     mov     al, BYTE PTR [bx]
     mov     BYTE PTR [di], al
     inc     bx
     inc     di
     mov     dh, NUMWANTED              ; next token must be a number
     jmp     Parse_checkeos

  checkcloseparen:
     cmp     BYTE PTR [bx], ')'
     jne     checkspace
     cmp     dh, NUMWANTED		; invalid if a number is expected
     je      errencounteredop
     mov     al, BYTE PTR [bx]
     mov     BYTE PTR [di], al
     inc     bx
     inc     di
     mov     dh, 1                      ; next token must be an operator
     jmp     Parse_checkeos

  checkspace:
     cmp     BYTE PTR [bx], SPACE
     jne     handleoperator
     inc     bx                         ; ignore space and move to next char
     jmp     Parse_checkeos

  handleoperator:
     cmp     dh, NUMWANTED
     je      errencounteredop

  placeoperator:
     mov     al, BYTE PTR [bx]          ; place operator into controlStr
     mov     BYTE PTR [di], al
     inc     bx
     inc     di
     mov     dh, NUMWANTED              ; next token must be a number
     jmp     Parse_checkeos

  errencounteredeos:
     mov     dx, OFFSET errMsg8
     jmp     Parse_error

  errencounterednum:
     mov     dx, OFFSET errMsg6
     jmp     Parse_error

  errinputoverflow:
     mov     dx, OFFSET errMsg2
     jmp     Parse_error

  errencounteredop:
     mov     dx, OFFSET errMsg7

  Parse_error:
     call    dspmsg
     stc

  Parse_done:
     pop     di
     pop     dx
     pop     bx
     pop     ax
     ret
parse endp

 
;Comment the registers that are used as variables!!!
CheckDone proc near
        call LibCheckDone
        ret
CheckDone endp


;Comment code that is confusing!!! 
FindInner proc near
        call LibFindInner
        ret
FindInner endp

;Don't write a comment for every line!!!
FindOne proc near
        call LibFindOne
        ret
FindOne endp

;Don't write paragraphs of comments!!!
SolveOne proc near
        call LibSolveOne
        ret
SolveOne endp

;Write your comments well!!!
GetOp1 proc near
        call LibGetOp1
        ret
GetOp1 endp

 
;When you duplicate code, don't duplicate commentes!!! 
GetOp2 proc near
        call LibGetOp2
        ret
GetOp2 endp 


MAIN PROC NEAR
     mov     ax, cseg                ; Initialize Default Segment register
     mov     ds, ax  
  
  begin:
     call    Input
     jc      FinalExit  ; if done, exit
     
     mov     dx, offset parseMsg
     call    dspmsg

     call    checkParens
     jc      Error 

     call    Parse
     jc      Error      ; on err, start over
  solve:
     call    LibDspCtrlStr ; show the current state of calculation
     
     call    CheckDone
     jc      Error
     jz      begin
     
   continue: 
     call    kbdin       ; pause before continueing
     cmp     al, ESCKEY
     jz      FinalExit
     cmp     al, LF
     jz      continue

     ;find and solve the operator of highest precidence
     call    FindInner
     call    FindOne
     jc      Error
     call    SolveOne
     jc      Error
     
     jmp solve
     
  Error:
     mov     dx, offset errMsg
     call    dspmsg
     jmp     begin     
     
FinalExit:
     call    mp2xit                  ; Exit to DOS

MAIN ENDP

CSEG ENDS
        END MAIN                 

