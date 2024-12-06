; ModuleID = 'probe2.50718a19eac86d8d-cgu.0'
source_filename = "probe2.50718a19eac86d8d-cgu.0"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; probe2::probe
; Function Attrs: nonlazybind uwtable
define void @_ZN6probe25probe17hb0148beb4e432956E() unnamed_addr #0 {
start:
  %0 = alloca i32, align 4
  store i32 -2147483648, ptr %0, align 4
  %1 = load i32, ptr %0, align 4, !noundef !2
  ret void
}

; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i32 @llvm.bitreverse.i32(i32) #1

attributes #0 = { nonlazybind uwtable "probe-stack"="inline-asm" "target-cpu"="broadwell" "target-features"="-avx512pf,-tsxldtrk,+cx16,+sahf,-tbm,-avx512ifma,-sha,+crc32,-fma4,-vpclmulqdq,+prfchw,+bmi2,-cldemote,+fsgsbase,-avx512bf16,-amx-tile,-raoint,-uintr,-gfni,+popcnt,-ptwrite,+aes,-avx512bitalg,-movdiri,-widekl,-xsaves,-avx512er,-avxvnni,-avx512fp16,-avx512vnni,-amx-bf16,-avxvnniint8,-avx512vpopcntdq,-pconfig,-clwb,-cmpccxadd,-avx512f,-xsavec,-clzero,-pku,-amx-fp16,+mmx,-lwp,-rdpid,-xop,+rdseed,-waitpkg,-prefetchi,-kl,-movdir64b,-sse4a,-avx512bw,-avxneconvert,-clflushopt,+xsave,-avx512vbmi2,+64bit,-avx512vl,-serialize,-hreset,+invpcid,-avx512cd,+avx,-vaes,-amx-int8,+cx8,+fma,+rtm,+bmi,-enqcmd,+rdrnd,-mwaitx,+sse4.1,+sse4.2,+avx2,+fxsr,-wbnoinvd,+sse,+lzcnt,+pclmul,-rdpru,-avxifma,+f16c,+ssse3,-sgx,-prefetchwt1,+cmov,-avx512vbmi,-shstk,+movbe,-avx512vp2intersect,+xsaveopt,-avx512dq,+sse2,+adx,+sse3" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.module.flags = !{!0, !1}

!0 = !{i32 8, !"PIC Level", i32 2}
!1 = !{i32 2, !"RtLibUseGOT", i32 1}
!2 = !{}
