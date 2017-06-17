# library('Rcpp')
# library('RcppArmadillo')
# library('inline')

src <- 
"
Rcpp::List calcTauC(int pa, arma::uvec ch, arma::uvec an, int k, arma::uvec cond, arma::mat & Y, arma::mat & yty) {
  
  int n = Y.n_rows;
  int i;
  arma::vec resid;

  // Get regression coefficients
  if(cond.n_elem > 0){
    arma::vec b = solve(yty.submat(cond, cond), ((arma::vec) yty.col(pa)).elem(cond)) ;
    // get residuals 
    resid = Y.col(pa) - Y.cols(cond) * b ;
  } else {
    resid = Y.col(pa) ;
    Rcout << 2<<std::endl;
  }
  
  // calculate tau
  arma::vec ret1(size(ch));
  arma::vec ret2(size(an));

  for(i = 0; i < ch.n_elem; i++){
  ret1[i] = mean((arma::vec) pow(resid, k - 1) % Y.col(ch(i))) * mean((arma::vec) pow(resid, 2)) -
      mean((arma::vec) pow(resid, k)) * mean(resid % Y.col(ch(i)));
  }
  
  for(i = 0; i < an.n_elem; i++){
  ret2[i] = mean((arma::vec) pow(resid, k - 1) % Y.col(an(i))) * mean((arma::vec) pow(resid, 2)) -
      mean((arma::vec) pow(resid, k)) * mean(resid % Y.col(an(i)));
  }

  List::create(ret1, ret2);
}
"

cppFunction(code=src, depends="RcppArmadillo", verbose = F)
