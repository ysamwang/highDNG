#include<RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::List calcTauC(int pa, const arma::uvec & ch, int k, const arma::umat & condSets,
                    const arma::umat & anSets, const arma::mat & Y, const arma::mat & yty) {
  
  
  int i, z;
  double n = Y.n_rows;
  arma::vec resid;
  arma::vec ret1(ch.n_elem);
  arma::vec ret2(Y.n_cols);
  ret1.fill(1e10);
  ret2.fill(1e10);
  arma::urowvec cond;
  arma::vec b;
  double tauStatMax = 1e10;
  
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
    ret1(i) = std::fabs((1.0 / n ) * dot(resid_k_1, Y.col(ch(i))) * resid_var  -
             resid_k * (1.0 / n) * dot(resid, Y.col(ch(i))));
  }

    tauStatMax = std::min(tauStatMax, max(ret1));

  // Rcpp::Rcout << "Getting an" << std::endl;
    for(i = 0; i < anSets.n_cols; i++){
      // Replace with inner product function to hopefully not allocate memory for element-wise
      // multiplication 
      // Rcpp::Rcout << anSets(z, i) << " ";
      ret2(anSets(z, i)) = std::min(ret2(anSets(z, i)), 
                  fabs((1.0 / n ) * dot(resid_k_1, Y.col(anSets(z, i))) * resid_var -
                    resid_k * (1.0 / n) * dot(resid, Y.col(anSets(z, i)))));
    }
  }

  return Rcpp::List::create(Rcpp::Named("tauStat") = tauStatMax,
                            Rcpp::Named("an") = ret2);
}


