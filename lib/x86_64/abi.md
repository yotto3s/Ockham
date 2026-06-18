# **Ockham Application Binary Interface (ABI) for x86\_64**

This document defines the calling convention, register usage, and stack layout for the Ockham Native ABI on the x86\_64 architecture.

Unlike the standard System V AMD64 ABI (used by C/C++ on Linux and macOS), the Ockham Native ABI natively supports **multiple return values** in registers, eliminating the need for hidden pointer arguments when returning small structs or multiple variables.

## **1\. Register Usage**

The x86\_64 architecture provides 16 general-purpose integer registers and 16 floating-point (XMM) registers. The Ockham ABI categorizes them as follows:

| Register | 32-bit | Role in Ockham ABI | Preserved across calls? |
| :---- | :---- | :---- | :---- |
| **%rax** | %eax | Return Value 1 / Scratch | Caller-saved (No) |
| **%rdx** | %edx | Argument 3 / Return Value 2 | Caller-saved (No) |
| **%rcx** | %ecx | Argument 4 / Return Value 3 | Caller-saved (No) |
| **%r8** | %r8d | Argument 5 / Return Value 4 | Caller-saved (No) |
| **%rdi** | %edi | Argument 1 | Caller-saved (No) |
| **%rsi** | %esi | Argument 2 | Caller-saved (No) |
| **%r9** | %r9d | Argument 6 | Caller-saved (No) |
| **%r10** | %r10d | Scratch / Temporary | Caller-saved (No) |
| **%r11** | %r11d | Scratch / Temporary | Caller-saved (No) |
| **%rbx** | %ebx | General Purpose | **Callee-saved (Yes)** |
| **%r12** | %r12d | General Purpose | **Callee-saved (Yes)** |
| **%r13** | %r13d | General Purpose | **Callee-saved (Yes)** |
| **%r14** | %r14d | General Purpose | **Callee-saved (Yes)** |
| **%r15** | %r15d | General Purpose | **Callee-saved (Yes)** |
| **%rbp** | %ebp | Base Pointer (Frame Pointer) | **Callee-saved (Yes)** |
| **%rsp** | %esp | Stack Pointer | **Callee-saved (Yes)** |

*Note: Floating-point values follow the same argument sequence using %xmm0 through %xmm7, and return sequence using %xmm0 through %xmm3.*

## **2\. Argument Passing**

Arguments are passed from left to right. To maintain partial compatibility and ease of debugging, Ockham passes arguments identically to the System V ABI.

1. **First 6 Integer/Pointer Arguments:** Passed in %rdi, %rsi, %rdx, %rcx, %r8, %r9.  
2. **First 8 Floating-Point Arguments:** Passed in %xmm0 through %xmm7.  
3. **Additional Arguments:** If a function takes more than 6 integer arguments, the 7th argument and beyond are pushed onto the stack in **reverse order** (right-to-left) before the call instruction.

## **3\. Multiple Return Values (The Ockham Extension)**

Standard C limits returns to %rax and %rdx. Ockham natively supports up to **4 return registers**, allowing tuples or small structs to be returned entirely in hardware registers.

When a function returns:

1. **First Value:** %rax (or %xmm0 for floats)  
2. **Second Value:** %rdx (or %xmm1 for floats)  
3. **Third Value:** %rcx (or %xmm2 for floats)  
4. **Fourth Value:** %r8 (or %xmm3 for floats)

### **Spilling Return Values**

If a function returns **more than 4 values** (or a struct larger than 32 bytes), the ABI falls back to a hidden pointer:

1. The caller allocates space on its own stack for the return values.  
2. The caller passes a pointer to this space as a hidden **first argument** in %rdi.  
3. All standard arguments are shifted right by one register (e.g., Arg 1 moves to %rsi).  
4. The callee writes the return values to the hidden pointer address and returns the pointer in %rax.

## **4\. Stack Layout and Alignment**

The stack grows **downwards** (from higher memory addresses to lower memory addresses).

### **Stack Alignment Requirement (Crucial)**

Immediately before the call instruction is executed, the stack pointer (%rsp) **must be aligned to a 16-byte boundary**.

Because the call instruction automatically pushes an 8-byte return address onto the stack, the stack will technically be misaligned (%rsp % 16 \== 8\) upon entering the callee. The callee's prologue corrects this.

### **Standard Stack Frame (Prologue)**

Every Ockham function must begin with this standard prologue to establish the Base Pointer (%rbp):

push %rbp          \# Save caller's base pointer  
mov %rsp, %rbp     \# Establish new base pointer  
sub $SIZE, %rsp    \# Allocate stack space for local variables (SSA slots)

*SIZE must be calculated such that %rsp remains 16-byte aligned if making further function calls.*

### **Frame Visualized**

\+-----------------------+   
| Arguments 7+          |  \<-- Higher addresses  
\+-----------------------+  
| Return Address (8b)   |  \<-- Pushed by 'call'  
\+-----------------------+  
| Saved %rbp (8b)       |  \<-- %rbp points here  
\+-----------------------+  
| Local SSA Regs / Vars |  
| ...                   |  \<-- %rsp points here (Must be 16-byte aligned before next 'call')  
\+-----------------------+  \<-- Lower addresses

## **5\. Foreign Function Interface (FFI)**

Because Ockham uses a custom multiple-return convention (%rcx and %r8), it is **not entirely binary compatible** with C standard libraries (libc) for functions returning large structs.

To call external C functions (e.g., malloc, printf, system libraries), the Ockham compiler must implement an ABI switch.

### **extern "C" Rules**

When the frontend encounters a function marked extern "C":

1. **Calls:** The backend must ensure that at most 2 values are expected to be returned (%rax and %rdx).  
2. **Variadic Arguments:** For functions like printf, the C ABI requires %al (the lowest byte of %rax) to contain the number of vector registers (%xmm) used for arguments.  
3. **Returns:** If an extern "C" function returns a struct larger than 16 bytes, the backend must automatically allocate the hidden pointer and pass it in %rdi, as C will not use %rcx or %r8 to return data.
