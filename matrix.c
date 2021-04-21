#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static void die(const char *message)
{
    perror(message);
    exit(1);

}

struct matrix {
  int num_rows;
  int num_cols;
  double** matrixAddr; // accessed [row][col]
  int buildPosition;
};
typedef struct matrix matrix;

int debug = 0;



matrix* storeVal(matrix* target, double value) {

    int position = target->buildPosition;
    int curr_row = position / target->num_cols;
    int curr_col = position % target->num_cols;

    if(debug == 1) {
        printf("Storing: %f\n", value);
        printf("in row: %d\n", curr_row);
        printf("in col: %d\n", curr_col);
    }

    target->matrixAddr [curr_row][curr_col] = value;
    target->buildPosition = target->buildPosition + 1;
    return target;
}

matrix* initMatrix(double* listOfValues, int num_cols, int num_rows) {
  double** matrixValues = malloc(num_rows * sizeof(double*));

  if(debug == 1) {
      printf("Building matrix:\n");
      printf("num_rows: %d\n", num_rows);
      printf("num_cols: %d\n", num_cols);
  }

  //set all values in matrix to NULL if list of values is NULL
  if (listOfValues == NULL) {
    for(int i = 0; i < num_rows; i++) {
      double* matrix_row = malloc(num_cols * sizeof(double));
      *(matrixValues + i) = matrix_row;
      for(int j = 0; j < num_cols; j++) {
        matrix_row[j] = 0;
      }
    }
  }
    
  //load values from a list of values
  else {
    for(int i = 0; i < num_cols; i++) {
      double* matrix_col = malloc(num_rows * sizeof(double));
      *(matrixValues + i) = matrix_col;
      for(int j = 0; j < num_rows; j++) {
        matrix_col[j] = listOfValues[i*num_rows + j];
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
        double sum = lhs->matrixAddr[i][j] + rhs->matrixAddr[i][j];
       result->matrixAddr[i][j] = sum;
    }
  }

  return result;
}


// void getCofactor(matrix* m, matrix* temp, int p, int q, int n) {
//     int i=0, j=0;
//     for(int row=0; row<n; row++) {
//         for(int col=0; col<n; col++) {
//             if(row != p && col != q) {
//                 temp->matrixAddr[i][j++] = m->matrixAddr[row][col];
//                 if(j == n-1) {
//                     j=0;
//                     i++;
//                 }
//             }
//         }
//     }
// }


// int determinant(matrix* input, int n) {
//     int row = input->num_rows;
//     int col = input->num_cols;
//     int d = 0;

//     if(row==col) {
//         //base case: matrix contains single element
//         if(n == 1) 
//             return input->matrixAddr[0][0];

//         matrix* temp = initMatrix(NULL, row, col);

//         int sign=1;
//         for(int f=0; f<n; f++) {
//             getCofactor(input, temp, 0, f, n);
//             d += sign * input->matrixAddr[0][f] * determinant(temp, n-1);
//             sign = -sign;
//         }

//         return d;

//     }
//     else {
//         printf("Your matrix must be square to compute the determinant.\n");
//         return 0;
//     }

// }


// matrix* dotProduct(matrix* lhs, matrix* rhs) {
//     //check to make sure matrices are the same size
//     if (lhs->num_cols != rhs->num_rows) {
//         die("Matrices are not the same size!");
//     } 
//     //once we know that matrices are same size, we can compute result
//     matrix *result = initMatrix(NULL, rhs->num_cols, lhs->num_rows);
//     for (int i=0; i < lhs->num_rows; i++)
//     {
//         for (int j=0; j < rhs->num_cols; j++)
//         {
//             for (int k=0; k < rhs->num_rows; k++)
//             {
//                 result->matrixAddr[i][j] += lhs->matrixAddr[i][k] * rhs->matrixAddr[k][j];
//             }
//         }
//     }
//     return result;
// }

// matrix* transpose(matrix* input) {
//     int rows = input->num_cols;
//     int cols = input->num_rows;

//     int** matrixValues = malloc(cols * sizeof(int*));

//     for (int i = 0; i < rows; i++) {
//         int* matrix_col = malloc(rows * sizeof(int));
//         *(matrixValues + i) = matrix_col;
//         for (int j = 0; j < cols; j++) {
//             matrix_col[j] = *(*((input->matrixAddr) + j)+i);
//         }
//     } 

//     input->num_rows = rows;
//     input->num_cols = cols;
//     input->matrixAddr = matrixValues;

//     return input;
// }


matrix* matrixMult(matrix* lhs, matrix* rhs) {
  //check dimensions
  if (lhs->num_rows != rhs->num_rows || lhs->num_cols != rhs->num_rows) {
    die("matrix add size mismatch");
  }
  int rows = lhs->num_rows;
  int cols= lhs->num_cols;
  matrix *result = initMatrix(NULL, rows, cols);
  for(int i=0; i<rows; i++) {
    for(int j=0; j<cols; j++) {
        double product = lhs->matrixAddr[i][j] * rhs->matrixAddr[i][j];
        result->matrixAddr[i][j] = product;
    }
  }

  return result;
}


// matrix* timesScalar(matrix* input, int scalar) {
//   int rows = input->num_rows;
//   int cols= input->num_cols;
//   matrix *result = initMatrix(NULL, rows, cols);
//   for(int i=0; i<rows; i++) {
//     for(int j=0; j<cols; j++) {
//         int product = input->matrixAddr[i][j] * scalar;
//         result->matrixAddr[i][j] = product;
//     }
//   }

//   return result;
// }


void display(matrix* input) {
    int row = input->num_rows;
    int col = input->num_cols;
    for(int i = 0; i<row; i++) {
        for(int j=0; j<col; j++) {
            printf(" %.2f", input->matrixAddr[i][j]);
        }
        printf("\n");
    }
}

// int string_length(char *s) {
//     return strlen(s);
// }

// char *string_get(char *s, int i) {
//     char *c = malloc(2);
//     c[0] = s[i];
//     c[1] = '\0';
//     return c;
// }

// char *string_concat(char *s1, char *s2) {
//     char *new = (char *) malloc(strlen(s1) + strlen(s2) + 1);
//     strcpy(new, s1);
//     strcat(new, s2);
//     return new;
// }

// int string_equals(char *s1, char *s2) {
//     return (strcmp(s1, s2) == 0);
// }

// char *string_substr(char *s, int start, int end) {
//   char *substr = malloc(end - start+1);
//     int i;
//     for(i = 0; i < (end - start); i++) {
//         substr[i] = s[start + i];
//     }
//     substr[end-start]=0;
//     return substr;
// }



#ifdef BUILD_TEST
int main(int argc,char** argv) {
  //run tests of each function
  //initMatrix and display of empty matrix
  matrix *null_matrix=initMatrix(NULL, 2, 2);
  // printf("NULL MATRIX: \n");
  // display(null_matrix);

  


  //initMatrix and display of 2x2 matrix
  double vals1[] = {91, 2, 3, 222, 5, 6};
  double *list1 = vals1;
  matrix *m = initMatrix(list1, 2, 3);
  // printf("2x2 MATRIX: \n");
  display(m);

  //TODO test codegen builder
  for( int i = 0; i < 4; i++) {
      m = storeVal(m, 5);
      printf("Stroring 5: \n");
      display(m);
  }
  
  
  // //add 2 of the same matrix
  // matrix *result_sum = mAdd(m, m);
  // printf("ADD TWO OF THE ORIGINAL 2x2 MATRIX: \n");
  // display(result_sum);

  // //multiply two matrices
  // matrix *result_product = matrixMult(m, m);
  // printf("MULTIPLY TWO OF THE ORIGINAL 2X2 MATRIX: \n");
  // display(result_product);

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
#endif
