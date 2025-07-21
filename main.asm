.386P
.model flat   
 
data segment use16
    ;input_buf db 26         ;MAX NUMBER OF CHARACTERS ALLOWED (25).
    ;          db ?          ;NUMBER OF CHARACTERS ENTERED BY USER.
    ;          db 26 dup(0)  ;CHARACTERS ENTERED BY USER.
    input_str db 3 
              db "3.1", "$"

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
parse_float proc ; (uint16 len, char *str)
    push ebp
    mov ebp, esp
    
    sub ESP, 8;
    mov dword ptr [EBP - 4], 0 ; sign data
    mov dword ptr [EBP - 8], 0 ; mantissa buffer
    
    ; callee-safe registers
    push bx
    push si
    
    ; subroutine body 
    len EQU dx
    mov len, word ptr [ebp + 6]
    
    str_ptr EQU bx
    mov str_ptr, word ptr [ebp + 6 + 2]  ; string ptr
    
    xor eax, eax
    whole_part EQU EAX
    whole_part_l EQU AL 

    xor si, si
    
    cmp byte ptr [str_ptr], '-'
        jne _loop
        mov byte ptr [EBP - 1], 080h  ; 2^7
        inc si
_loop:
    cmp si, len
    je _no_dot
    
    cmp byte ptr [str_ptr + si], '.'
    je _found_dot
    
    cmp byte ptr [str_ptr + si], '0'
    jl _error
    cmp byte ptr [str_ptr + si], '9' 
    jg _error
    
    imul whole_part, whole_part, 10
    add whole_part_l, byte ptr [str_ptr + si]
    sub whole_part_l, '0'
    
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
    push len
    
    call parse_mantissa
    pop len
    pop str_ptr
    
    pop mantissa 
    
    xchg mantissa, whole_part

_build_float:
    power EQU EDX
    xor power, power
    
_loop2:
    cmp whole_part, 1
        jle _loop2_end 
    
    inc power
    
    mov byte ptr [ebp - 5], whole_part_l
    and byte ptr [ebp - 5], 1 ; whole (mod 2)
    shl byte ptr [ebp - 5], 7
    
    shr mantissa, 1
    or mantissa, dword ptr [ebp - 8]
    
    shr whole_part, 1
    jmp _loop2
    
_loop2_end:
    cmp mantissa, 0
    je _epilogue
    
    add power, 127
    shl power, 24
    shr power, 1
    
    or power, dword ptr [ebp - 4]  ; adding a sign bit
    shr mantissa, 9
    or mantissa, power

_epilogue:
    mov EAX, mantissa

    ; restoring registers
    pop si
    pop bx

    mov esp, ebp
    pop ebp
    ret

_error:
    call exit_invalid_char 
parse_float endp


parse_mantissa proc  ; (uint16 len, char [data *] str)
    push ebp
    mov ebp, esp
    
    sub ESP, 2
    
    push bx
    push si
    push di
    ; subroutine body

    xor eax, eax ; mantissa
    xor ecx, ecx
    mov cx, word PTR [EBP + 6]      ; str_length
    mov bx, WORD PTR [EBP + 6 + 2]  ; str_ptr. points after a dot symbol
    
    MAX_MANTISSA_SIZE = 23
    cmp cl, MAX_MANTISSA_SIZE 
    jle _mantissa_is_at_most_23
        mov cl, MAX_MANTISSA_SIZE

    _mantissa_is_at_most_23:
    
    xor si, si 
_normaize_loop:
    cmp si, cx
    jge _normaize_loop_end
    
    cmp byte ptr [bx + si], '0'
    jl _error

    cmp byte ptr [bx + si], '9'
    jg _error
    
    sub byte ptr [bx + si], '0'
    inc si
    jmp _normaize_loop
_normaize_loop_end:

    has_decimal_part EQU byte ptr [EBP - 1]
    iteration_count EQU byte ptr [EBP - 2]
    mov iteration_count, 0

    mov di, 31
_decimal_part_outer_start:
    cmp iteration_count, MAX_MANTISSA_SIZE
        je _decimal_part_outer_end

    inc iteration_count

    mov si, cx
    dec si
    
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
            mov has_decimal_part, 1
        cmp_done:
        
        cmp si, 0
        je _decimal_part_inner_end

        dec si
        jmp _decimal_part_inner
_decimal_part_inner_end:    
    cmp has_decimal_part, 1
        jne _no_decimal_part_left

        push CX
        mov CX, DI

        shl EDX, CL
        dec DI

        pop CX
        
        or eax, edx
        jmp _decimal_part_outer_start

    _no_decimal_part_left:
        cmp EDX, 1
        jne _decimal_part_outer_end

        push CX
        mov CX, DI

        shl EDX, CL
        dec DI

        pop CX
        
        or eax, edx
        jmp _decimal_part_outer_end 
_decimal_part_outer_end:
    ; epilogue
    pop di
    pop si
    pop bx
    
    ADD ESP, 2

    mov esp, ebp
    pop ebp
    ret

_error: 
    call exit_invalid_char    
parse_mantissa endp

main:     
    mov    eax, data
    mov    ds, eax


    mov dx, offset input_str + 1
    push dx

    xor dx, dx
    mov dl, [input_str]
    push dx
    call parse_float

    xor    eax, eax
    mov    ah, 4Ch
    int    21h
code ends
end main
