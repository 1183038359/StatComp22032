#include <Rcpp.h>
using namespace Rcpp;

//' @title The maximum number of points on a line
//' @description Find the maximum number of points on the two-dimensional plane that can be connected into a straight line
//' @param points Two dimensional matrix, the first column is x and the second column is y
//' @return the mximum number
//' @examples
//' \dontrun{
//' points<-matrix(c(1,1,2,2,3,3),3,2,byrow = TRUE)
//' maxPoints(points)
//' }
//' @importFrom Rcpp evalCpp
//' @useDynLib StatComp22032
//' @export
// [[Rcpp::export]]
int maxPoints(NumericMatrix points) {
  int len = points.nrow();
  // when the number of points is not enough
  if(len < 3) {
    return len;
  }
  int maxNum = 2;
  NumericVector maxrecord;
  // Traverse every two points
  for(double i = 0; i < len - 1; i ++) {
    for(double j = i + 1; j < len; j ++) {
      // count the Number of equal slopes.
      NumericVector record ={i,j};
      int count = 2;
      double dx = points(i,0) - points(j,0);
      double dy = points(i,1) - points(j,1);
      // Compare with other points
      for(int k = j + 1; k < len; k ++) {
        // If the slopes are equal
        if(dx * (points(i,1) - points(k,1)) == dy * (points(i,0) - points(k,0))) {
          count ++;
          record.push_back(k);
        }
      }
      maxrecord=(maxNum>=count)?maxrecord:record;
      maxNum = (maxNum>=count)? maxNum: count;
      if(maxNum > len / 2) return maxNum;
    }  
  }
  // Rprintf("the maxrecord is: ",maxrecord);
  
  return maxNum;
}

