; ModuleID = 'xirtam'
source_filename = "xirtam"

%struct.matrix = type { i32, i32, double*, i32 }

@fmt = private unnamed_addr constant [4 x i8] c"%g\0A\00", align 1

declare i32 @printf(i8*, ...)

declare i32 @display(%struct.matrix*)

declare %struct.matrix* @initMatrix_CG(i32, i32)

declare %struct.matrix* @storeVal(%struct.matrix*, double)

declare double @pub_get(%struct.matrix*, double, double)

declare i32 @pub_set(%struct.matrix*, double, double, double)

declare %struct.matrix* @matrixMult(%struct.matrix*, %struct.matrix*)

declare %struct.matrix* @mAdd(%struct.matrix*, %struct.matrix*)

declare double @mNumRows(%struct.matrix*)

define i32 @main() {
entry:
  %r = alloca double
  %m = alloca %struct.matrix*
  %matrix_init = call %struct.matrix* @initMatrix_CG(i32 3, i32 3)
  %store_val = call %struct.matrix* @storeVal(%struct.matrix* %matrix_init, double -1.000000e+00)
  %store_val1 = call %struct.matrix* @storeVal(%struct.matrix* %matrix_init, double 1.000000e+00)
  %store_val2 = call %struct.matrix* @storeVal(%struct.matrix* %matrix_init, double 1.000000e+00)
  %store_val3 = call %struct.matrix* @storeVal(%struct.matrix* %matrix_init, double 1.000000e+00)
  %store_val4 = call %struct.matrix* @storeVal(%struct.matrix* %matrix_init, double 2.000000e+00)
  %store_val5 = call %struct.matrix* @storeVal(%struct.matrix* %matrix_init, double 4.000000e+00)
  %store_val6 = call %struct.matrix* @storeVal(%struct.matrix* %matrix_init, double 1.000000e+00)
  %store_val7 = call %struct.matrix* @storeVal(%struct.matrix* %matrix_init, double 2.000000e+00)
  %store_val8 = call %struct.matrix* @storeVal(%struct.matrix* %matrix_init, double 3.000000e+00)
  store %struct.matrix* %matrix_init, %struct.matrix** %m
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), double 3.000000e+00)
  ret i32 0
}
