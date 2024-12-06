; ModuleID = 'probe1.fe2e3f7351368e89-cgu.0'
source_filename = "probe1.fe2e3f7351368e89-cgu.0"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; core::f64::<impl f64>::to_int_unchecked
; Function Attrs: inlinehint nonlazybind uwtable
define i32 @"_ZN4core3f6421_$LT$impl$u20$f64$GT$16to_int_unchecked17hd577c44956bf36fcE"(double %self) unnamed_addr #0 {
start:
; call <f64 as core::convert::num::FloatToInt<i32>>::to_int_unchecked
  %0 = call i32 @"_ZN65_$LT$f64$u20$as$u20$core..convert..num..FloatToInt$LT$i32$GT$$GT$16to_int_unchecked17h05e720baf2d57f9fE"(double %self)
  ret i32 %0
}

; <f64 as core::convert::num::FloatToInt<i32>>::to_int_unchecked
; Function Attrs: inlinehint nonlazybind uwtable
define internal i32 @"_ZN65_$LT$f64$u20$as$u20$core..convert..num..FloatToInt$LT$i32$GT$$GT$16to_int_unchecked17h05e720baf2d57f9fE"(double %self) unnamed_addr #0 {
start:
  %0 = alloca i32, align 4
  %1 = fptosi double %self to i32
  store i32 %1, ptr %0, align 4
  %2 = load i32, ptr %0, align 4, !noundef !2
  ret i32 %2
}

; probe1::probe
; Function Attrs: nonlazybind uwtable
define void @_ZN6probe15probe17hed5626d085803452E() unnamed_addr #1 {
start:
; call core::f64::<impl f64>::to_int_unchecked
  %_1 = call i32 @"_ZN4core3f6421_$LT$impl$u20$f64$GT$16to_int_unchecked17hd577c44956bf36fcE"(double 1.000000e+00)
  ret void
}

attributes #0 = { inlinehint nonlazybind uwtable "probe-stack"="inline-asm" "target-cpu"="broadwell" "target-features"="-avx512pf,-tsxldtrk,+cx16,+sahf,-tbm,-avx512ifma,-sha,+crc32,-fma4,-vpclmulqdq,+prfchw,+bmi2,-cldemote,+fsgsbase,-avx512bf16,-amx-tile,-raoint,-uintr,-gfni,+popcnt,-ptwrite,+aes,-avx512bitalg,-movdiri,-widekl,-xsaves,-avx512er,-avxvnni,-avx512fp16,-avx512vnni,-amx-bf16,-avxvnniint8,-avx512vpopcntdq,-pconfig,-clwb,-cmpccxadd,-avx512f,-xsavec,-clzero,-pku,-amx-fp16,+mmx,-lwp,-rdpid,-xop,+rdseed,-waitpkg,-prefetchi,-kl,-movdir64b,-sse4a,-avx512bw,-avxneconvert,-clflushopt,+xsave,-avx512vbmi2,+64bit,-avx512vl,-serialize,-hreset,+invpcid,-avx512cd,+avx,-vaes,-amx-int8,+cx8,+fma,+rtm,+bmi,-enqcmd,+rdrnd,-mwaitx,+sse4.1,+sse4.2,+avx2,+fxsr,-wbnoinvd,+sse,+lzcnt,+pclmul,-rdpru,-avxifma,+f16c,+ssse3,-sgx,-prefetchwt1,+cmov,-avx512vbmi,-shstk,+movbe,-avx512vp2intersect,+xsaveopt,-avx512dq,+sse2,+adx,+sse3" }
attributes #1 = { nonlazybind uwtable "probe-stack"="inline-asm" "target-cpu"="broadwell" "target-features"="-avx512pf,-tsxldtrk,+cx16,+sahf,-tbm,-avx512ifma,-sha,+crc32,-fma4,-vpclmulqdq,+prfchw,+bmi2,-cldemote,+fsgsbase,-avx512bf16,-amx-tile,-raoint,-uintr,-gfni,+popcnt,-ptwrite,+aes,-avx512bitalg,-movdiri,-widekl,-xsaves,-avx512er,-avxvnni,-avx512fp16,-avx512vnni,-amx-bf16,-avxvnniint8,-avx512vpopcntdq,-pconfig,-clwb,-cmpccxadd,-avx512f,-xsavec,-clzero,-pku,-amx-fp16,+mmx,-lwp,-rdpid,-xop,+rdseed,-waitpkg,-prefetchi,-kl,-movdir64b,-sse4a,-avx512bw,-avxneconvert,-clflushopt,+xsave,-avx512vbmi2,+64bit,-avx512vl,-serialize,-hreset,+invpcid,-avx512cd,+avx,-vaes,-amx-int8,+cx8,+fma,+rtm,+bmi,-enqcmd,+rdrnd,-mwaitx,+sse4.1,+sse4.2,+avx2,+fxsr,-wbnoinvd,+sse,+lzcnt,+pclmul,-rdpru,-avxifma,+f16c,+ssse3,-sgx,-prefetchwt1,+cmov,-avx512vbmi,-shstk,+movbe,-avx512vp2intersect,+xsaveopt,-avx512dq,+sse2,+adx,+sse3" }

!llvm.module.flags = !{!0, !1}

!0 = !{i32 8, !"PIC Level", i32 2}
!1 = !{i32 2, !"RtLibUseGOT", i32 1}
!2 = !{}
