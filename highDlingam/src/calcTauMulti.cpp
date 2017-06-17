#include<RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::vec calcTauMultiC(int pa, const arma::uvec & ch, int k, const arma::umat & condSets,
                    const arma::umat & anSets, const arma::mat & Y, const arma::mat & yty) {
  
  int i, z;
  double n = Y.n_rows;
  arma::vec resid;
  arma::vec ret(Y.n_cols);
  ret.fill(1e10);
  arma::urowvec cond;
  arma::vec b;
  
  // Get regression coefficients
  for(z = 0; z < condSets.n_rows; z++) {
    cond = condSets.row(z);
    b = solve(yty.submat(cond, cond), ((arma::vec) yty.col(pa)).elem(cond)) ;
    resid = Y.col(pa) - Y.cols(cond) * b ;
    // calculate tau
    
    arma::vec resid_k_1 = pow(resid, k - 1);
    double resid_var = mean((arma::vec) pow(resid, 2));
    double resid_k = mean((arma::vec) pow(resid, k));
    
    for(i = 0; i < ch.n_elem; i++){
      ret(ch(i)) = std::min(ret(ch(i)),
                            fabs((1.0 / n ) * dot(resid_k_1, Y.col(ch(i))) * resid_var  -
         resid_k * (1.0 / n) * dot(resid, Y.col(ch(i)))));
    }

    
    for(i = 0; i < anSets.n_cols; i++){
      // Replace with inner product function to hopefully not allocate memory for element-wise
      // multiplication 
      ret(anSets(z, i)) = std::min(ret(anSets(z, i)), 
                                    fabs((1.0 / n) * dot(resid_k_1, Y.col(anSets(z, i))) *
                                           resid_var - resid_k * (1.0 / n) * dot(resid , Y.col(anSets(z, i)))));
    }
  }
  
  return ret;
}


