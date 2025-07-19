.386P
.model flat   
 
data segment use16
    input_buf db 26           ;MAX NUMBER OF CHARACTERS ALLOWED (25).
              db ?            ;NUMBER OF CHARACTERS ENTERED BY USER.
              db 26 dup(0)    ;CHARACTERS ENTERED BY USER.
    
    first_num_whole_part db 26 dup (0)
    first_num_whole_part db 26 dup (0)

data ends
 
code segment 'code' use16
assume cs:code, ds:data
    
input proc
    push   ebp
    mov    ebp, esp

    mov    edx, [ebp + 2 + 4]
    xor    eax, eax
    mov    ah, 0Ah
    int    21h

    mov    edx, [ebp + 2 + 6]
    inc    edx
    mov    esi, edx
    mov    ecx, [esi]
    xor    ch, ch
    add    si, cx
    mov    byte ptr [si+1], '$'
    inc    dx

    pop    ebp
    ret
input endp
  

parse_float proc; void -> float
  push EBP
  mov EBP, ESP
  
  sub ESP, 4  ; индикатор знака. 0 - положительное число, 1 - отрицательное
  mov byte ptr [EBP - 4], 0
  
  xor ecx, ecx
  mov cl, [offset input_buf + 1]
  mov ebx, offset input_buf + 2
  
  cmp [ebx], '-'
  jne __parse_float_numbers 
    mov BYTE PTR [EBP - 1], 1

__parse_float_numbers:
  xor eax, eax
__parse_float_start:
  cmp [ebx], '0'
    jl __parse_float_check_dot
  cmp [ebx], '9'
    jg __parse_float_check_dot

  mul 10
  add eax, [ebx]
  sub eax, '0'

  inc ebx
  loopnz __parse_float_start
  
__parse_float_check_dot:
  cmp ecx, 0
  je __parse_float_build

    






  

  
  
  


  

  
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  mov ESP, EBP
  pop EBP
  ret
parse_float endp
 
factorial proc
                          push   EBP
                          mov    EBP, ESP
 
                          mov    eax, dword ptr [EBP + 6]
                          shr    eax, 16
 
                          cmp    dword ptr [EBP + 6], 1
                          jg     __factorial_recurse
 
    __factorial_base_case:
                          mov    EAX, 1
                          jmp    __factorial_ret
 
    __factorial_recurse:  
                          mov    EAX, dword ptr [EBP + 6]
                          dec    EAX
                          push   EAX
 
                          call   factorial
 
                          mov    Ebx, dword ptr [EBP + 6]
                          mul    Ebx
                          jmp    __factorial_ret
 
    __factorial_ret:      
                          mov    ESP, EBP
                          pop    EBP
                          ret
                          endp

 
main proc
  mov    eax, data
  mov    ds, eax

  xor    eax, eax

  mov    ah, 0Ah                     ;SERVICE TO CAPTURE STRING FROM KEYBOARD.
  mov    edx, offset buff
  int    21h

  mov    si, offset buff + 1         ;NUMBER OF CHARACTERS ENTERED.
  mov    cl, [ si ]                  ;MOVE LENGTH TO CL.
  mov    ch, 0                       ;CLEAR CH TO USE CX.
  inc    cx                          ;TO REACH CHR(13).
  add    si, cx                      ;NOW SI POINTS TO CHR(13).
  mov    al, '$'
  mov    [ si ], al                  ;REPLACE CHR(13) BY '$'.

  xor    eax, eax
  mov    ah, 4Ch
  int    21h
  endp
 
code ends
end main