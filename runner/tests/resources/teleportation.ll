%Result = type opaque
%Qubit = type opaque

@0 = internal constant [4 x i8] c"0_a\00"
@1 = internal constant [6 x i8] c"1_a0r\00"
@2 = internal constant [6 x i8] c"2_a1r\00"
@3 = internal constant [6 x i8] c"3_a2r\00"
@4 = internal constant [6 x i8] c"4_a3r\00"

define i64 @ENTRYPOINT__main() #0 {
block_0:
  call void @__quantum__rt__initialize(i8* null)
  call void @__quantum__qis__h__body(%Qubit* inttoptr (i64 2 to %Qubit*))
  call void @__quantum__qis__cx__body(%Qubit* inttoptr (i64 2 to %Qubit*), %Qubit* inttoptr (i64 1 to %Qubit*))
  call void @__quantum__qis__cx__body(%Qubit* inttoptr (i64 0 to %Qubit*), %Qubit* inttoptr (i64 2 to %Qubit*))
  call void @__quantum__qis__h__body(%Qubit* inttoptr (i64 0 to %Qubit*))
  call void @__quantum__qis__m__body(%Qubit* inttoptr (i64 2 to %Qubit*), %Result* inttoptr (i64 0 to %Result*))
  %var_1 = call i1 @__quantum__rt__read_result(%Result* inttoptr (i64 0 to %Result*))
  br i1 %var_1, label %block_1, label %block_2
block_1:
  call void @__quantum__qis__x__body(%Qubit* inttoptr (i64 1 to %Qubit*))
  br label %block_2
block_2:
  call void @__quantum__qis__m__body(%Qubit* inttoptr (i64 0 to %Qubit*), %Result* inttoptr (i64 1 to %Result*))
  %var_3 = call i1 @__quantum__rt__read_result(%Result* inttoptr (i64 1 to %Result*))
  br i1 %var_3, label %block_3, label %block_4
block_3:
  call void @__quantum__qis__z__body(%Qubit* inttoptr (i64 1 to %Qubit*))
  br label %block_4
block_4:
  call void @__quantum__qis__reset__body(%Qubit* inttoptr (i64 2 to %Qubit*))
  call void @__quantum__qis__m__body(%Qubit* inttoptr (i64 1 to %Qubit*), %Result* inttoptr (i64 2 to %Result*))
  call void @__quantum__qis__reset__body(%Qubit* inttoptr (i64 0 to %Qubit*))
  call void @__quantum__qis__reset__body(%Qubit* inttoptr (i64 1 to %Qubit*))
  call void @__quantum__qis__x__body(%Qubit* inttoptr (i64 0 to %Qubit*))
  call void @__quantum__qis__h__body(%Qubit* inttoptr (i64 2 to %Qubit*))
  call void @__quantum__qis__cx__body(%Qubit* inttoptr (i64 2 to %Qubit*), %Qubit* inttoptr (i64 1 to %Qubit*))
  call void @__quantum__qis__cx__body(%Qubit* inttoptr (i64 0 to %Qubit*), %Qubit* inttoptr (i64 2 to %Qubit*))
  call void @__quantum__qis__h__body(%Qubit* inttoptr (i64 0 to %Qubit*))
  call void @__quantum__qis__m__body(%Qubit* inttoptr (i64 2 to %Qubit*), %Result* inttoptr (i64 3 to %Result*))
  %var_6 = call i1 @__quantum__rt__read_result(%Result* inttoptr (i64 3 to %Result*))
  br i1 %var_6, label %block_5, label %block_6
block_5:
  call void @__quantum__qis__x__body(%Qubit* inttoptr (i64 1 to %Qubit*))
  br label %block_6
block_6:
  call void @__quantum__qis__m__body(%Qubit* inttoptr (i64 0 to %Qubit*), %Result* inttoptr (i64 4 to %Result*))
  %var_8 = call i1 @__quantum__rt__read_result(%Result* inttoptr (i64 4 to %Result*))
  br i1 %var_8, label %block_7, label %block_8
block_7:
  call void @__quantum__qis__z__body(%Qubit* inttoptr (i64 1 to %Qubit*))
  br label %block_8
block_8:
  call void @__quantum__qis__reset__body(%Qubit* inttoptr (i64 2 to %Qubit*))
  call void @__quantum__qis__m__body(%Qubit* inttoptr (i64 1 to %Qubit*), %Result* inttoptr (i64 5 to %Result*))
  call void @__quantum__qis__reset__body(%Qubit* inttoptr (i64 0 to %Qubit*))
  call void @__quantum__qis__reset__body(%Qubit* inttoptr (i64 1 to %Qubit*))
  call void @__quantum__qis__h__body(%Qubit* inttoptr (i64 0 to %Qubit*))
  call void @__quantum__qis__h__body(%Qubit* inttoptr (i64 2 to %Qubit*))
  call void @__quantum__qis__cx__body(%Qubit* inttoptr (i64 2 to %Qubit*), %Qubit* inttoptr (i64 1 to %Qubit*))
  call void @__quantum__qis__cx__body(%Qubit* inttoptr (i64 0 to %Qubit*), %Qubit* inttoptr (i64 2 to %Qubit*))
  call void @__quantum__qis__h__body(%Qubit* inttoptr (i64 0 to %Qubit*))
  call void @__quantum__qis__m__body(%Qubit* inttoptr (i64 2 to %Qubit*), %Result* inttoptr (i64 6 to %Result*))
  %var_11 = call i1 @__quantum__rt__read_result(%Result* inttoptr (i64 6 to %Result*))
  br i1 %var_11, label %block_9, label %block_10
block_9:
  call void @__quantum__qis__x__body(%Qubit* inttoptr (i64 1 to %Qubit*))
  br label %block_10
block_10:
  call void @__quantum__qis__m__body(%Qubit* inttoptr (i64 0 to %Qubit*), %Result* inttoptr (i64 7 to %Result*))
  %var_13 = call i1 @__quantum__rt__read_result(%Result* inttoptr (i64 7 to %Result*))
  br i1 %var_13, label %block_11, label %block_12
block_11:
  call void @__quantum__qis__z__body(%Qubit* inttoptr (i64 1 to %Qubit*))
  br label %block_12
block_12:
  call void @__quantum__qis__reset__body(%Qubit* inttoptr (i64 2 to %Qubit*))
  call void @__quantum__qis__h__body(%Qubit* inttoptr (i64 1 to %Qubit*))
  call void @__quantum__qis__m__body(%Qubit* inttoptr (i64 1 to %Qubit*), %Result* inttoptr (i64 8 to %Result*))
  call void @__quantum__qis__h__body(%Qubit* inttoptr (i64 1 to %Qubit*))
  call void @__quantum__qis__reset__body(%Qubit* inttoptr (i64 0 to %Qubit*))
  call void @__quantum__qis__reset__body(%Qubit* inttoptr (i64 1 to %Qubit*))
  call void @__quantum__qis__x__body(%Qubit* inttoptr (i64 0 to %Qubit*))
  call void @__quantum__qis__h__body(%Qubit* inttoptr (i64 0 to %Qubit*))
  call void @__quantum__qis__h__body(%Qubit* inttoptr (i64 2 to %Qubit*))
  call void @__quantum__qis__cx__body(%Qubit* inttoptr (i64 2 to %Qubit*), %Qubit* inttoptr (i64 1 to %Qubit*))
  call void @__quantum__qis__cx__body(%Qubit* inttoptr (i64 0 to %Qubit*), %Qubit* inttoptr (i64 2 to %Qubit*))
  call void @__quantum__qis__h__body(%Qubit* inttoptr (i64 0 to %Qubit*))
  call void @__quantum__qis__m__body(%Qubit* inttoptr (i64 2 to %Qubit*), %Result* inttoptr (i64 9 to %Result*))
  %var_16 = call i1 @__quantum__rt__read_result(%Result* inttoptr (i64 9 to %Result*))
  br i1 %var_16, label %block_13, label %block_14
block_13:
  call void @__quantum__qis__x__body(%Qubit* inttoptr (i64 1 to %Qubit*))
  br label %block_14
block_14:
  call void @__quantum__qis__m__body(%Qubit* inttoptr (i64 0 to %Qubit*), %Result* inttoptr (i64 10 to %Result*))
  %var_18 = call i1 @__quantum__rt__read_result(%Result* inttoptr (i64 10 to %Result*))
  br i1 %var_18, label %block_15, label %block_16
block_15:
  call void @__quantum__qis__z__body(%Qubit* inttoptr (i64 1 to %Qubit*))
  br label %block_16
block_16:
  call void @__quantum__qis__reset__body(%Qubit* inttoptr (i64 2 to %Qubit*))
  call void @__quantum__qis__h__body(%Qubit* inttoptr (i64 1 to %Qubit*))
  call void @__quantum__qis__m__body(%Qubit* inttoptr (i64 1 to %Qubit*), %Result* inttoptr (i64 11 to %Result*))
  call void @__quantum__qis__h__body(%Qubit* inttoptr (i64 1 to %Qubit*))
  call void @__quantum__qis__reset__body(%Qubit* inttoptr (i64 0 to %Qubit*))
  call void @__quantum__qis__reset__body(%Qubit* inttoptr (i64 1 to %Qubit*))
  call void @__quantum__rt__array_record_output(i64 4, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @0, i64 0, i64 0))
  call void @__quantum__rt__result_record_output(%Result* inttoptr (i64 2 to %Result*), i8* getelementptr inbounds ([6 x i8], [6 x i8]* @1, i64 0, i64 0))
  call void @__quantum__rt__result_record_output(%Result* inttoptr (i64 5 to %Result*), i8* getelementptr inbounds ([6 x i8], [6 x i8]* @2, i64 0, i64 0))
  call void @__quantum__rt__result_record_output(%Result* inttoptr (i64 8 to %Result*), i8* getelementptr inbounds ([6 x i8], [6 x i8]* @3, i64 0, i64 0))
  call void @__quantum__rt__result_record_output(%Result* inttoptr (i64 11 to %Result*), i8* getelementptr inbounds ([6 x i8], [6 x i8]* @4, i64 0, i64 0))
  ret i64 0
}

declare void @__quantum__rt__initialize(i8*)

declare void @__quantum__qis__h__body(%Qubit*)

declare void @__quantum__qis__cx__body(%Qubit*, %Qubit*)

declare void @__quantum__qis__m__body(%Qubit*, %Result*) #1

declare i1 @__quantum__rt__read_result(%Result*)

declare void @__quantum__qis__x__body(%Qubit*)

declare void @__quantum__qis__z__body(%Qubit*)

declare void @__quantum__qis__reset__body(%Qubit*) #1

declare void @__quantum__rt__array_record_output(i64, i8*)

declare void @__quantum__rt__result_record_output(%Result*, i8*)

attributes #0 = { "entry_point" "output_labeling_schema" "qir_profiles"="adaptive_profile" "required_num_qubits"="3" "required_num_results"="12" }
attributes #1 = { "irreversible" }

; module flags

!llvm.module.flags = !{!0, !1, !2, !3, !4, !5}

!0 = !{i32 1, !"qir_major_version", i32 1}
!1 = !{i32 7, !"qir_minor_version", i32 0}
!2 = !{i32 1, !"dynamic_qubit_management", i1 false}
!3 = !{i32 1, !"dynamic_result_management", i1 false}
!4 = !{i32 5, !"int_computations", !{!"i64"}}
!5 = !{i32 5, !"float_computations", !{!"double"}}
