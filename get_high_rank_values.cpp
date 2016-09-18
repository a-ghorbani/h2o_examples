
#include <Rcpp.h>
// #include <RcppArmadillo.h>
// //[[Rcpp::depends(RcppArmadillo)]]

#include <iostream>
#include <vector>

// From https://github.com/tjhladish/AbcSmc/blob/master/ranker.h
#include "ranker.h"

using namespace Rcpp;

//[[Rcpp::export]]
List get_high_rank_values(
  NumericMatrix & rank, 
  const CharacterMatrix & val,
  const CharacterVector & colNames, 
  int num) {
  
  int ncol = rank.ncol(); // rank.n_cols; //rank.ncol(); 
  int nrow = rank.nrow(); // n_rows; //nrow(); 
  
  uint ncol1 = ncol; 
  uint n = num; 
  
  //std::vector<int> w[nrow];
  std::vector<std::vector<int> > w(nrow, std::vector<int>(num));
  //std::vector<int> w;
  double * m = REAL(transpose(rank)) ; 
  for(int i=0; i < nrow; i++){
    //w = w2d[i]; 
    //NumericMatrix::Row r = rank.row(i);
    partial_order(&m[i*ncol], ncol1, w[i], n) ; 
    /*for(int j=0; j < num; j++){
      std::cout << "w(" << i << "," << j << ") " << w[i][j] << " - " << rank(i,j) << " - " << *(&m[i*ncol]+j) << "\n"; 
    }*/
  }
  
  NumericMatrix   resRank    (nrow, num);
  CharacterMatrix resVal     (nrow, num);
  CharacterMatrix resColNames(nrow, num);
  
  for(int i=0; i < nrow; i++){
    for(int j=0; j < num; j++){
      //std::cout << "w(" << i << "," << j << ") " << w[i][j] << "\\n"; 
      resRank     (i,j) = rank    (i,w[i][j]); 
      resVal      (i,j) = val     (i,w[i][j]); 
      resColNames (i,j) = colNames(  w[i][j]); 
    }
  }
  
  List z = List::create(resRank, resVal, resColNames) ;
  return(z) ; 
}
