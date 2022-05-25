	.file	"fib-noOpt.cpp"
	.text
	.globl	x1
	.bss
	.align 4
	.type	x1, @object
	.size	x1, 4
x1:
	.zero	4
	.globl	x2
	.data
	.align 4
	.type	x2, @object
	.size	x2, 4
x2:
	.long	1
	.section	.rodata
	.align 8
.LC0:
	.string	"void fib_forward(int&, int&, int)"
.LC1:
	.string	"fib-noOpt.cpp"
.LC2:
	.string	"tmp == (x1) - ((x2) - (x1))"
.LC3:
	.string	"!(i == 1)"
	.text
	.globl	_Z11fib_forwardRiS_i
	.type	_Z11fib_forwardRiS_i, @function
_Z11fib_forwardRiS_i:
.LFB0:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$48, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movl	%edx, -36(%rbp)
	movl	$1, -8(%rbp)
.L5:
	movl	-8(%rbp), %eax
	cmpl	-36(%rbp), %eax
	je	.L6
	movq	-32(%rbp), %rax
	movl	(%rax), %edx
	movq	-24(%rbp), %rax
	movl	(%rax), %eax
	subl	%eax, %edx
	movl	%edx, %eax
	movl	%eax, -4(%rbp)
	movq	-32(%rbp), %rax
	movl	(%rax), %edx
	movq	-24(%rbp), %rax
	movl	(%rax), %eax
	addl	%eax, %edx
	movq	-32(%rbp), %rax
	movl	%edx, (%rax)
	movq	-24(%rbp), %rax
	movl	(%rax), %edx
	movl	-4(%rbp), %eax
	addl	%eax, %edx
	movq	-24(%rbp), %rax
	movl	%edx, (%rax)
	movq	-24(%rbp), %rax
	movl	(%rax), %edx
	movq	-32(%rbp), %rax
	movl	(%rax), %ecx
	movq	-24(%rbp), %rax
	movl	(%rax), %eax
	subl	%eax, %ecx
	movl	%ecx, %eax
	subl	%eax, %edx
	movl	%edx, %eax
	cmpl	%eax, -4(%rbp)
	je	.L3
	leaq	.LC0(%rip), %rcx
	movl	$22, %edx
	leaq	.LC1(%rip), %rsi
	leaq	.LC2(%rip), %rdi
	call	__assert_fail@PLT
.L3:
	addl	$1, -8(%rbp)
	cmpl	$1, -8(%rbp)
	jne	.L5
	leaq	.LC0(%rip), %rcx
	movl	$24, %edx
	leaq	.LC1(%rip), %rsi
	leaq	.LC3(%rip), %rdi
	call	__assert_fail@PLT
.L6:
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	_Z11fib_forwardRiS_i, .-_Z11fib_forwardRiS_i
	.section	.rodata
	.align 8
.LC4:
	.string	"void fib_reverse(int&, int&, int)"
.LC5:
	.string	"tmp == (x2) - (x1)"
.LC6:
	.string	"!(i == n)"
	.text
	.globl	_Z11fib_reverseRiS_i
	.type	_Z11fib_reverseRiS_i, @function
_Z11fib_reverseRiS_i:
.LFB1:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$48, %rsp
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movl	%edx, -36(%rbp)
	movl	-36(%rbp), %eax
	movl	%eax, -8(%rbp)
.L11:
	cmpl	$1, -8(%rbp)
	je	.L12
	subl	$1, -8(%rbp)
	movq	-24(%rbp), %rax
	movl	(%rax), %edx
	movq	-32(%rbp), %rax
	movl	(%rax), %ecx
	movq	-24(%rbp), %rax
	movl	(%rax), %eax
	subl	%eax, %ecx
	movl	%ecx, %eax
	subl	%eax, %edx
	movl	%edx, %eax
	movl	%eax, -4(%rbp)
	movq	-24(%rbp), %rax
	movl	(%rax), %eax
	subl	-4(%rbp), %eax
	movl	%eax, %edx
	movq	-24(%rbp), %rax
	movl	%edx, (%rax)
	movq	-32(%rbp), %rax
	movl	(%rax), %edx
	movq	-24(%rbp), %rax
	movl	(%rax), %eax
	subl	%eax, %edx
	movq	-32(%rbp), %rax
	movl	%edx, (%rax)
	movq	-32(%rbp), %rax
	movl	(%rax), %edx
	movq	-24(%rbp), %rax
	movl	(%rax), %eax
	subl	%eax, %edx
	movl	%edx, %eax
	cmpl	%eax, -4(%rbp)
	je	.L9
	leaq	.LC4(%rip), %rcx
	movl	$37, %edx
	leaq	.LC1(%rip), %rsi
	leaq	.LC5(%rip), %rdi
	call	__assert_fail@PLT
.L9:
	movl	-8(%rbp), %eax
	cmpl	-36(%rbp), %eax
	jne	.L11
	leaq	.LC4(%rip), %rcx
	movl	$38, %edx
	leaq	.LC1(%rip), %rsi
	leaq	.LC6(%rip), %rdi
	call	__assert_fail@PLT
.L12:
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	_Z11fib_reverseRiS_i, .-_Z11fib_reverseRiS_i
	.section	.rodata
.LC7:
	.string	"int main()"
.LC8:
	.string	"!(i == 0)"
	.align 8
.LC9:
	.string	"((x1) == (0)) && ((x2) == (1))"
	.text
	.globl	main
	.type	main, @function
main:
.LFB2:
	.cfi_startproc
	endbr64
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$16, %rsp
	movl	$0, -4(%rbp)
.L16:
	cmpl	$100000, -4(%rbp)
	je	.L14
	movl	$2000, %edx
	leaq	x2(%rip), %rsi
	leaq	x1(%rip), %rdi
	call	_Z11fib_forwardRiS_i
	movl	$2000, %edx
	leaq	x2(%rip), %rsi
	leaq	x1(%rip), %rdi
	call	_Z11fib_reverseRiS_i
	addl	$1, -4(%rbp)
	cmpl	$0, -4(%rbp)
	jne	.L16
	leaq	.LC7(%rip), %rcx
	movl	$52, %edx
	leaq	.LC1(%rip), %rsi
	leaq	.LC8(%rip), %rdi
	call	__assert_fail@PLT
.L14:
	movl	x1(%rip), %eax
	testl	%eax, %eax
	jne	.L17
	movl	x2(%rip), %eax
	cmpl	$1, %eax
	je	.L20
.L17:
	leaq	.LC7(%rip), %rcx
	movl	$54, %edx
	leaq	.LC1(%rip), %rsi
	leaq	.LC9(%rip), %rdi
	call	__assert_fail@PLT
.L20:
	movl	$0, %eax
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 9.4.0-1ubuntu1~20.04.1) 9.4.0"
	.section	.note.GNU-stack,"",@progbits
	.section	.note.gnu.property,"a"
	.align 8
	.long	 1f - 0f
	.long	 4f - 1f
	.long	 5
0:
	.string	 "GNU"
1:
	.align 8
	.long	 0xc0000002
	.long	 3f - 2f
2:
	.long	 0x3
3:
	.align 8
4:
