#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

static void die(const char *message)
{
    perror(message);
    exit(1);

}

struct matrix {
  int num_rows;
  int num_cols;
  double* matrixAddr; // accessed [row][col]
  int buildPosition;
};
typedef struct matrix matrix;

int debug = 0;

double get(struct matrix* m, int r,int c){
  //set m[r][c]
  int kill = 0;
  if (r>((m->num_rows)-1)){
    perror("row index out of range when setting matrix ");
    kill = 1;
  }
  if (c>((m->num_cols)-1)){
    perror("col index out of range when setting matrix ");
    kill = 1;
  }
  if(kill==1){
    die("");
  }
  int idx = c + (r * (m->num_cols));
  return m->matrixAddr[idx];
}

void set( struct matrix* m,int r,int c,double v){
  //get
  int kill = 0;
  if (r>((m->num_rows)-1)){
    perror("row index out of range when setting matrix ");
    kill = 1;
  }
  if (c>((m->num_cols)-1)){
    perror("col index out of range when setting matrix ");
    kill = 1;
  }
  if(kill==1){
    die("");
  }
  int idx = c + (r * (m->num_cols));
  m->matrixAddr[idx]=v;
}

double pub_get(struct matrix* m, double r,double c){
  //public getter
  //set m[r][c]
  // int idx = r + (c * (m->num_rows));
  //cast row and col to int
  return get(m,(int)r,(int)c);
}

void pub_set( struct matrix* m, double r,double c, double v){
  //public setter
  // int idx = r + (c * (m->num_rows));
  //cast row and col to int
  set(m,(int)r,(int)c,v);
}

matrix* storeVal(matrix* target, double value) {

    int position = target->buildPosition;
    int curr_row = position / target->num_cols;
    int curr_col = position % target->num_cols;

    if(debug == 1) {
        printf("Storing: %f\n", value);
        printf("in row: %d\n", curr_row);
        printf("in col: %d\n", curr_col);
    }

    target->matrixAddr[position] = value;
    target->buildPosition = target->buildPosition + 1;
    return target;
}

matrix* initMatrix(double* listOfValues, int num_cols, int num_rows) {
  double* matrixValues = malloc(num_rows * num_cols * sizeof(double*));

  if(debug == 1) {
      printf("Building matrix:\n");
      printf("num_rows: %d\n", num_rows);
      printf("num_cols: %d\n", num_cols);
  }

  //set all values in matrix to 0 if list of values is NULL
  if (listOfValues == NULL) {
    for(int r = 0; r < num_rows; r++) {
      for(int c = 0; c < num_cols; c++) {
        // matrixValues[r + (c * num_rows)]=0;
        matrixValues[c + (r * num_cols)]=0;
      }
    }
  }
    
  //load values from a list of values
  else {
    for(int r = 0; r < num_rows; r++) {
      for(int c = 0; c < num_cols; c++) {
        // int idx = c + (r * num_cols);
        int idx = c + (r * num_cols);
        matrixValues[idx]=listOfValues[idx];
      }
    }
  }

  //return a pointer to matrix struct
  matrix* result = malloc(sizeof(struct matrix));
  result->num_cols = num_cols;
  result->num_rows = num_rows;
  result->matrixAddr = matrixValues;
  result->buildPosition = 0;
  return result;
}

matrix* initMatrix_CG( int num_cols, int num_rows) {
    return initMatrix(NULL, num_cols, num_rows);
}

matrix* mAdd(matrix* lhs, matrix* rhs) {
  //check dimensions
  if (lhs->num_rows != rhs->num_rows || lhs->num_cols != rhs->num_cols) {
    perror("Addition size mismatch.");
    perror("Add");
    exit(1);
  }
  int rows = lhs->num_rows;
  int cols= lhs->num_cols;
  matrix *result = initMatrix(NULL, cols, rows);
  for(int i=0; i < rows; i++) {
    for(int j=0; j < cols; j++) {
        double sum = get(lhs,i,j)+get(rhs,i,j);
        set(result,i,j,sum);
    }
  }

  return result;
}

/*
mNumRows
input -> xirtam matrix object
output -> int, number of rows
description: return the number of rows in a matrix
*/
double mNumRows(matrix* lhs){
    return lhs->num_rows;
}

/*
matrixInverse
input -> xirtam matrix object
output -> the inverse of the matrix M^-1
description: return the inverse of the matrix input
*/

matrix* matrixTranspose(matrix* lhs){
  int rows = lhs->num_rows;
  int cols = lhs->num_cols;
  matrix *transpose = initMatrix(NULL, cols, rows);
  for (int i = 0; i< cols; i++){
    for (int j = 0; j< rows; j++){
      set(transpose, j, i, get(lhs, i, j));
      printf("row num: ");
      printf("hi %d", j);
      printf("\n");
      printf("raw %f", get(lhs, i, j));
      printf("\n");

    }
  }
  
}

matrix* matrixMult(matrix* lhs, matrix* rhs) {
  //check dimensions//our original code xirtam
  if (lhs->num_cols != rhs->num_rows) {
    die("matrix multiplication dimensions mismatch, must have (AxM)*(MxB)");
  }
  int rows = lhs->num_rows;
  int cols= rhs->num_cols;//(r1xc1)*(r2xc2)
  matrix *result = initMatrix(NULL, cols, rows);
  for(int i=0; i<rows; i++) {
    for(int j=0; j<cols; j++) {
      for (int k=0; k < rhs->num_rows; k++){
        set(result,i,j,get(result,i,j)+(get(lhs,i,k)*get(rhs,k,j)));
      }
    }
  }
  return result;
}
void display(matrix* input) {
    int row = input->num_rows;
    int col = input->num_cols;
    for(int i = 0; i<row; i++) {
        for(int j=0; j<col; j++) {
          if (j == 0) {
            printf("%.2f", get(input,i,j));
          } else {
          printf(" %.2f", get(input,i,j));
          }
            // printf(" %.2f", input->matrixAddr[i][j]);
        }
        printf("\n");
    }
}
//======================================================================
// Test below: move the function above this line if it works
//
//======================================================================

// TEMP FUNCS FROM SHIDA'S TESTING

// SHIDA'S TEMP FUNCS END


int main(int argc,char** argv) {
  printf("lior's transpose funct\n");
  double firstmat[] = {1,2,3,4,5,6};
  matrix *matrix_OG = initMatrix(firstmat, 3, 2);
  display(matrix_OG);
  matrix *matrix_result = matrixTranspose(matrix_OG);
  display(matrix_result);





  //run tests of each function
  //initMatrix and display of empty matrix
  printf("\n===========testing empty init========\n");
  matrix *null_matrix=initMatrix(NULL, 2, 2);
  // printf("NULL MATRIX: \n");
  // display(null_matrix);

  

printf("\n===========testing list init========\n");
  //initMatrix and display of 2x2 matrix
  double vals1[] = {91, 2, 3, 222, 7, 6};
  double *list1 = vals1;
  matrix *m = initMatrix(list1, 3, 2);
  // printf("2x2 MATRIX: \n");

  display(m);
    for( int i = 0; i < 4; i++) {
      //fill first 4 values, i,e., first row as wel as first element of second row
      m = storeVal(m, 5);
      printf("Storing 5: \n");
      display(m);
  }
  printf("\n===========testing public get and set ========\n");
  double r = 1.1; double c = 0;double setval = 151.99;
  printf("get 1,0 of above matrix: %.2f\n", pub_get(m,r,c));
  pub_set(m,r,c,setval);
  printf("set 1,0 of above matrix: \n");
  display(m);


  
  
  // //add 2 of the same matrix
  printf("\n===========testing addition========\n");
  double vals1a[] = {1,2,3,4,5,6};
  double *list1a = vals1a;
  matrix *ma = initMatrix(list1a, 2, 3);
  matrix *result_sum = mAdd(ma, ma);
  display(result_sum);//


  // //multiply two matrices
  printf("\n===========testing multiplication========\n");
  double v1[] = {1,2,3,4,5,6};
  double v2[] = {10,11,20,21,30,31};
  matrix *m1 = initMatrix(v1, 3, 2);
  matrix *m2 = initMatrix(v2, 2, 3);
  matrix *result_product = matrixMult(m1, m2);
  // Should yield
  // 140.00 146.00
  // 320.00 335.00
  display(result_product);

  printf("\n Below are Shida testing on weird cases.\n");
  double k1[] = {-4, 2, 4, 422, 21, 2, 0.4, 6.2, -3};
  double k2[] = {1.01, 2, 0.91, 422, 21, -3, 0.4, 6.2, 32.74};
  matrix *n1 = initMatrix(k1, 3, 3);
  matrix *n2 = initMatrix(k2, 3, 3);
  matrix *result_product2 = mAdd(n2, n1);
  // Should yield
  // 140.00 146.00
  // 320.00 335.00
  display(result_product2);


  // double source[] = {0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0};
  // double dest[] = {0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0, 0,0,0,0,0,0};
  // matrix* d = initMatrix(dest, 5, 6);
  // matrix* s = initMatrix(source, 5, 6);
  

  // int cur_r;
  //   int cur_c;
  //   double desired_min;
  //   double desired_min_plus_1;

  //   d = copy_first_row(s, d);
  //   d = copy_first_col(s, d);

  //   for (cur_r = 1; cur_r < 6; cur_r = cur_r + 1) {
  //       for (cur_c = 1; cur_c < 5; cur_c = cur_c + 1) {
            
  //           if (get(s, cur_r, cur_c) == 1) {
  //               desired_min = min(get(d, cur_r-1, cur_c), get(d, cur_r, cur_c-1), get(d, cur_r-1, cur_c-1));
  //               desired_min_plus_1 = desired_min + 1;
  //               set(d, cur_r, cur_c, desired_min_plus_1);

  //               if (cur_r == 4 && cur_c == 3) {
  //                 printf("WE ARE HERE \n");
  //                 printf("%f", desired_min);
  //               }
  //           } else {
  //               set(d, cur_r, cur_c, 0);
  //           }


  //       }
  //   }

  //   display(d);

  // //scalar multiplication
  // matrix *result_scalar = timesScalar(m, 3);
  // printf("SCALAR MULTIPLICATION OF ORIGINAL MATRIX BY 3: \n");
  // display(result_scalar);

  // //determinant of 2x2 matrix
  // printf("The determinant is %d\n", determinant(m, 2));
  
  // //determinant of 3x3 matrix
  // int vals2[] = {6, 1, 1, 4, -2, 5, 2, 8, 7};
  // int *list2 = vals2;
  // matrix *n = initMatrix(list2, 3, 3);
  // printf("3x3 MATRIX: \n");
  // display(n);
  // printf("The determinant is %d\n", determinant(n, 3));
  
  // //dot product tests
  // int values_1[4] = {1,2,3,4};
  // matrix* m1 = initMatrix(&values_1[0], 2, 2);
  // int values_2[4] = {5,6,7,8};
  // matrix* m2 = initMatrix(&values_2[0], 2, 2);

  // printf("The dot product of matrices 1 and 2 is %d\n", dotProduct(m1,m2));

  // transpose(m1);
  // display(m1);
}