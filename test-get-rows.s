	.text
	.file	"xirtam"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3               # -- Begin function main
.LCPI0_0:
	.quad	-4616189618054758400    # double -1
.LCPI0_1:
	.quad	4607182418800017408     # double 1
.LCPI0_2:
	.quad	4611686018427387904     # double 2
.LCPI0_3:
	.quad	4616189618054758400     # double 4
.LCPI0_4:
	.quad	4613937818241073152     # double 3
	.text
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -16
	movl	$3, %edi
	movl	$3, %esi
	callq	initMatrix_CG@PLT
	movq	%rax, %rbx
	movsd	.LCPI0_0(%rip), %xmm0   # xmm0 = mem[0],zero
	movq	%rax, %rdi
	callq	storeVal@PLT
	movsd	.LCPI0_1(%rip), %xmm0   # xmm0 = mem[0],zero
	movq	%rbx, %rdi
	callq	storeVal@PLT
	movq	%rbx, %rdi
	movsd	.LCPI0_1(%rip), %xmm0   # xmm0 = mem[0],zero
	callq	storeVal@PLT
	movq	%rbx, %rdi
	movsd	.LCPI0_1(%rip), %xmm0   # xmm0 = mem[0],zero
	callq	storeVal@PLT
	movsd	.LCPI0_2(%rip), %xmm0   # xmm0 = mem[0],zero
	movq	%rbx, %rdi
	callq	storeVal@PLT
	movsd	.LCPI0_3(%rip), %xmm0   # xmm0 = mem[0],zero
	movq	%rbx, %rdi
	callq	storeVal@PLT
	movq	%rbx, %rdi
	movsd	.LCPI0_1(%rip), %xmm0   # xmm0 = mem[0],zero
	callq	storeVal@PLT
	movq	%rbx, %rdi
	movsd	.LCPI0_2(%rip), %xmm0   # xmm0 = mem[0],zero
	callq	storeVal@PLT
	movsd	.LCPI0_4(%rip), %xmm0   # xmm0 = mem[0],zero
	movq	%rbx, %rdi
	callq	storeVal@PLT
	movq	%rbx, (%rsp)
	leaq	.Lfmt(%rip), %rdi
	movsd	.LCPI0_4(%rip), %xmm0   # xmm0 = mem[0],zero
	movb	$1, %al
	callq	printf@PLT
	xorl	%eax, %eax
	addq	$16, %rsp
	.cfi_def_cfa_offset 16
	popq	%rbx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.Lfmt,@object           # @fmt
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt:
	.asciz	"%g\n"
	.size	.Lfmt, 4

	.section	".note.GNU-stack","",@progbits
