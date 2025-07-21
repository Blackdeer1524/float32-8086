.386P
.model flat   
 
data segment use16
    input_buf db 26         ;MAX NUMBER OF CHARACTERS ALLOWED (25).
              db ?          ;NUMBER OF CHARACTERS ENTERED BY USER.
              db 26 dup(0)  ;CHARACTERS ENTERED BY USER.

    err_unexpected_chr db "Invalid symbol in number"
data ends
 
code segment 'code' use16
    assume cs:code, ds:data

exit_invalid_char proc 
    xor    eax, eax
    
    mov dx, offset err_unexpected_chr
	mov ah, 09h
	int 21h

    mov    ah, 4Ch
    int    21h
exit_invalid_char endp
    
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
    

; eax, ecx, edx are caller-safe!
read_whole_part proc ; (char len, char *str, char *read_len)
    push ebp
    mov ebp, esp
    
    ; callee-safe registers
    push bx
    push si
    
    ; subroutine body 
    xor dx, dx
    mov dl, byte ptr [ebp + 6]
    len EQU dx
    len_l equ dl
    
    mov bx, word ptr [ebp + 6 + 1]  ; string ptr
    str_ptr EQU bx
    
    xor eax, eax
    whole_part EQU EAX
    whole_part_l EQU AL 

    xor si, si
_loop:
    cmp si, len
    je _no_dot
    
    cmp byte ptr [bx + si], '.'
    je _found_dot
    
    cmp [bx + si], '0'
    jl _error
    cmp [bx + si], '9' 
    jg _error
    
    imul whole_part, whole_part, 10
    add whole_part, [bx + si] - '0'
    
    inc si
    jmp _loop

_no_dot:
    mantissa EQU ECX

    xor ECX, ECX
    jmp _build_float
    
_found_dot:
    push whole_part

    inc si
    add str_ptr, si
    push str_ptr

    sub len, si
    push len_l
    
    call parse_mantissa
    pop len_l
    pop str_ptr
    
    pop mantissa 
    
    xchg mantissa, whole_part

_build_float:
    power EQU dl
    xor power, power
    
    sub ESP, 4
    MOV DWORD PTR [EBP - 4], 0
    
_loop2:
    cmp whole_part, 1
        je _loop2_end 
    
    inc power
    
    mov byte ptr [ebp - 4], whole_part_l
    and byte ptr [ebp - 4], 1 ; whole (mod 2)
    shl byte ptr [ebp - 4], 7
    
    shr mantissa, 1
    or mantissa, dword ptr [ebp - 4]
    
    shr whole_part
    jmp _loop2
    
_loop2_end:
    cmp mantissa, 0
    je _epilogue
    
    add power, 127
    shr mantissa, 8











    
    



_epilogue:
    mov EAX, mantissa
    add ESP, 4

    ; restoring registers
    pop si
    pop bx

    mov esp, ebp
    pop ebp
    ret

_error:
    call exit_invalid_char 
read_whole_part endp


parse_mantissa proc  ; (char len, char [data *] str)
    push ebp
    mov ebp, esp
    
    push bx
    push si
    push di
    ; subroutine body

    xor eax, eax ; mantissa
    xor ecx, ecx
    mov cl, BYTE PTR [EBP + 6]      ; str_length
    mov bx, WORD PTR [EBP + 6 + 1]  ; str_ptr. points after a dot symbol
    
    MAX_MANTISSA_SIZE = 23
    cmp cl, MAX_MANTISSA_SIZE 
    jle _mantissa_is_at_most_23
        mov cl, MAX_MANTISSA_SIZE

    _mantissa_is_at_most_23:
    
    xor si, si 
_normaize_loop:
    cmp si, cx
    jge _loop_end 
    
    cmp byte ptr [bx + si], '0'
    jl _error

    cmp byte ptr [bx + si], '9'
    jg _error
    
    sub byte ptr [bx + si], '0'
    inc si
_normaize_loop_end:

    sub ESP, 2
    has_decimal_part EQU byte ptr [EBP - 1]
    iteration_count EQU byte ptr [EBP - 2]
    xor iteration_count, iteration_count

_decimal_part_outer_start:
    cmp iteration_count, MAX_MANTISSA_SIZE
        je _decimal_part_outer_end

    inc iteration_count

    mov si, cx
    dec si
    
    mov di, 31

    xor edx, edx 
    mov has_decimal_part, 0 
    _decimal_part_inner:
        digit EQU byte ptr [bx + si]
        
        add dl, digit 
        add digit, dl ; multiply digit by 2 with a carry
        
        cmp digit, 10
        jl _decimal_part_inner_digit_lt_10
            sub digit, 10
            mov dl, 1
            jmp _decimal_part_inner_digit_lt_10_done
        _decimal_part_inner_digit_lt_10:
            xor dl, dl
            jmp _decimal_part_inner_digit_lt_10_done 
 
       _decimal_part_inner_digit_lt_10_done: 
        cmp digit, 0
            je cmp_done 
            mov has_decimal_part 1
        cmp_done:
        
        cmp si, 0
        je _decimal_part_inner_end

        dec si
        jmp _decimal_part_inner
_decimal_part_inner_end:    
    cmp has_decimal_part, 1
        jne _no_decimal_part_left

        shl EDX, DI
        dec DI
        
        or eax, edx
        jmp _decimal_part_outer_start

    _no_decimal_part_left:
        cmp EDX, 1
        jne _decimal_part_outer_end

        shl EDX, DI 
        dec DI
        
        or eax, edx
        jmp _decimal_part_outer_end 
_decimal_part_outer_end:
    ADD ESP, 2

    ; epilogue
    pop di
    pop si
    pop bx

    mov esp, ebp
    pop ebp
    ret

_error: 
    call exit_invalid_char    
parse_mantissa endp



parse_float proc; void -> float
    push EBP
    mov EBP, ESP

    sub ESP, 4  
    mov byte ptr [EBP - 4], 0

    xor ecx, ecx
    mov cl, [offset input_buf + 1]
    mov si, offset input_buf + 2

    cmp [ebx], '-'
    jne __parse_float_numbers
        mov BYTE PTR [EBP - 1], 1

__parse_float_numbers:
    xor eax, eax
__parse_float_start:
    cmp [ebx], '.'
       je __parse_float_decimal_part

    cmp [ebx], '0'
        jl __parse_float_check_dot
    cmp [ebx], '9'
        jg __parse_float_check_dot

    mul 10
    add eax, [ebx]
    sub eax, '0'

    inc ebx
    loopnz __parse_float_start

__parse_float_decimal_part:


    

    
    je __parse_float_build


    mov ESP, EBP
    pop EBP
    ret



parse_float endp
 
main:     
    mov    eax, data
    mov    ds, eax

    mov    eax, offset input_buf
    push   eax
    call   input

    xor    eax, eax
    mov    ah, 4Ch
    int    21h
code ends
end main
