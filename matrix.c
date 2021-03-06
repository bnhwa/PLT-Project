#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// Authored by Bailey Hwa, Shida Jing, and Andrew Gorovoy
// Referenced the past project Matrx. However, we noticed 
// that the past project code did not work, and we
// made heavy modifications. At this point, this file is pretty
// much original, except for general skeleton stuff and helper functions.

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
  //get m[r][c]
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
  //set m[r][c] to v
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

  if (r < 0) {
    perror("Row value is less than 0");
    exit(1);
  }
  if (c < 0) {
    perror("Column value is less than 0");
    exit(1);
  }
  return get(m,(int)r,(int)c);
}

void pub_set( struct matrix* m, double r,double c, double v){
  if (r < 0) {
    perror("Row value is less than 0");
    exit(1);
  }
  if (c < 0) {
    perror("Column value is less than 0");
    exit(1);
  }
  set(m,(int)r,(int)c,v);
}


double getrows(matrix* m) {

  return (double) m->num_rows;
}

double getcols(matrix* m) {
  return (double) m->num_cols;
}


matrix* autofill(double num_cols, double num_rows, double value) {

    if  (   ((int) num_cols < 1 )|| ((int) num_rows < 1)    ) {
      perror("Number of columns or number of rows is not valid.\nRows and columns must be a positive number.");
      exit(1);
    }

    double* matrixValues = malloc( (int) num_rows * (int) num_cols * sizeof(double*));

    for(int r = 0; r < num_rows; r++) {
      for(int c = 0; c < num_cols; c++) {
        // matrixValues[r + (c * num_rows)]=0;
        matrixValues[c + (r * (int)num_cols)]=(int) value;
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
        matrixValues[c + (r * num_cols)]=0;
      }
    }
  }
    
  //load values from a list of values
  else {
    for(int r = 0; r < num_rows; r++) {
      for(int c = 0; c < num_cols; c++) {
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
        }
        printf("\n");
    }
}


matrix* transpose(matrix* input) {
  //switch rows and cols, get empty(i.e., zeroed matrix of transposed size, then fill)
  int rows = input->num_rows;
  int cols = input->num_cols;
  matrix *result = initMatrix(NULL, rows, cols);
  for(int i=0; i<rows; i++) {
    for(int j=0; j<cols; j++) {
        set(result, j,i, get(input,i,j));
    }
  }
  return result;
}


#ifdef BUILD_TEST
int main(int argc,char** argv) {
  //run tests of each function
  //initMatrix and display of empty matrix
  printf("\n===========testing empty init========\n");
  matrix *null_matrix=initMatrix(NULL, 2, 2);


printf("\n===========testing list init========\n");
  //initMatrix and display of 2x2 matrix
  double vals1[] = {91, 2, 3, 222, 7, 6};
  double *list1 = vals1;
  matrix *m = initMatrix(list1, 3, 2);

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

  printf("\n===========testing transpose========\n");
  double v1t[] = {1,2,3,4,5,6};
  matrix *m1t = initMatrix(v1t, 2, 3);
  display(transpose(m1t));

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


  // Below is Shida testing demo program 
  
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
}
#endif
