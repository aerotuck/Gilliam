
//////////////////////////////////////////////////////////////////////////////////
//
// PKG_CPPFLAGS=`Rscript -e 'Rcpp:::CxxFlags()'` PKG_LIBS=`Rscript -e 'Rcpp:::LdFlags()'` R CMD SHLIB Gilliam5.VB.cpp

#include <Rcpp.h>
#include <math.h>
#include <Rmath.h>
//#include <RInside.h>

using namespace Rcpp;

RNGScope scope;

//////////////////////////////////////////////////////////////////////////////////
//  Below are some random number generators
//  They call the R functions through Rcpp
//  They return them back as doubles so that
//  you can use them directly in the code.

//////////////////////////////////////////////////////////////////////////////////
//  This function returns a single normal random variate in double
//  rnorm is the R function
//  d1 means it returns a double and a single value
//  Status:  PASSED TESTING
double rnormd1(double a, double b){
   double res1, res1b;
   SEXP Res1a;
   Res1a = Rcpp::rnorm(1,0,1);
   res1b = Rcpp::as<double>(Res1a);
   res1 = a + b*res1b;
   return res1;
}

//////////////////////////////////////////////////////////////////////////////////
//  This function returns a single uniform random variate in double
//  runif is the R function
//  d1 means it returns a double and a single value
//  Status:  PASSED TESTING
double runifd1(double a, double b){
   double res1, res1b;
   SEXP Res1a;
   Res1a = Rcpp::runif(1,0,1);
   res1b = Rcpp::as<double>(Res1a);
   res1 = a + (b-a)*res1b;
   return res1;
}

//////////////////////////////////////////////////////////////////////////////////
//  This function returns a single chisquare random variate in double
//  rchisq is the R function
//  d1 means it returns a double and a single value
//  Status:  PASSED TESTING
double rchisqd1(double a){
   double res1, res1b;
   SEXP Res1a;
   Res1a = Rcpp::rchisq(1,a);
   res1b = Rcpp::as<double>(Res1a);
   res1 = res1b;
   return res1;
}


//////////////////////////////////////////////////////////////////////////////////
//  Below are some denisities generators
//  They call the R functions through Rcpp
//  They return them back as doubles so that
//  you can use them directly in the code.

//////////////////////////////////////////////////////////////////////////////////
//  This function returns the log density of a normal random variate.
//  dnorm is the R function
//  d1 means it returns a double and a single value
//  Status:  PASSED TESTING
double dnormlogd1(double X, double mu, double sigma){
   double res1;
   res1 = -0.5*log(2.0*PI) - log(sigma) - 1/(2*sigma*sigma)*(X-mu)*(X-mu);
   return res1;
}

//////////////////////////////////////////////////////////////////////////////////
//  This function returns the log density of a gamma random variate.
//  dgamma is the R function
//  d1 means it returns a double and a single value
//  Status:  PASSED TESTING
double dgammalogd1(double X, double a1, double b1){
   double res1;
   res1 = a1*log(b1) - lgamma(a1) + (a1-1)*log(X) - b1*X;
   return res1;
}

//////////////////////////////////////////////////////////////////////////////////
//  This function returns the log density of a gamma random variate.
//  dchisq is the R function
//  d1 means it returns a double and a single value
//  Status:  NEEDS TESTING
double dchisqlogd1(double X, double a1){
   double res1;
   res1 = dgammalogd1(X, a1*0.5, 0.5);
   return res1;
}

//////////////////////////////////////////////////////////////////////////////////
//  This function returns the log density of a inverse gamma random variate.
//  No Corresponding R function
//  d1 means it returns a double and a single value
//  Status:  NEEDS TESTING
double dgammainvlogd1(double X, double a1, double b1){
   double res1;
   res1 = a1*log(b1) - lgamma(a1) - (a1+1)*log(X) - b1/X;
   return res1;
}

//////////////////////////////////////////////////////////////////////////////////
//  This function returns the log density of a inverse chisq random variate.
//  dinvchisq is the R function
//  d1 means it returns a double and a single value
//  Status:  NEEDS TESTING
extern "C" SEXP dchisqinvlogd1R(SEXP _X, SEXP _v1){
  double res1,X=as<double>(_X), v1 = as<double>(_v1);
   res1 = dgammainvlogd1(X, v1*0.5, 0.5);
   return wrap(res1);
}
extern "C" SEXP dchisqinvlogdVVR(SEXP _X, SEXP _v1){
  NumericVector X(_X), v1(_v1);
  int I = X.size(),i;
  double res1=0.0;
for(i=0; i< I; i++){
  res1 += dgammainvlogd1(X(i),v1(i)*0.5,0.5);
  }
 return wrap(res1);
}

extern "C" SEXP dchisqinvlogdVSR(SEXP _X, SEXP _v1){
  NumericVector X(_X);
  int I = X.size(),i;
  double res1=0.0,v1=as<double>(_v1);
for(i=0; i<I; i++){
  res1 += dgammainvlogd1(X(i),v1*0.5,0.5);
  }
 return wrap(res1);
}

SEXP dchisqinvlogdVS(SEXP _X, SEXP _v1){
   NumericVector X(_X);
  int I = X.size(),i;
  double res1=0.0,v1=as<double>(_v1);
for(i=0;i<I;i++){
  res1 += dgammainvlogd1(X(i),v1*0.5,0.5);
  }
 return wrap(res1);
}

SEXP dchisqinvlogdVV(SEXP _X, SEXP _v1){
  NumericVector X(_X), v1(_v1);
  int I = X.size(),i;
  double res1=0.0;
for(i=0;i<I;i++){
  res1 += dgammainvlogd1(X(i),v1(i)*0.5,0.5);
  }
 return wrap(res1);
}

double dchisqinvlogd1(double X, double v1){
   double res1;
   res1 = dgammainvlogd1(X, v1*0.5, 0.5);
   return res1;
}

//////////////////////////////////////////////////////////////////////////////////
//  This function returns the log density of a beta random variate.
//  dbeta is the R function
//  d1 means it returns a double and a single value
//  Status:  NEEDS TESTING
double dbetalogd1(double X, double a1, double b1){
   double res1;
   res1 = lgamma(a1+b1) - lgamma(a1) - lgamma(b1) + (a1-1)*log(X) + (b1-1)*log(1-X);
   return res1;
}

//////////////////////////////////////////////////////////////////////////////////
//  This function returns the sum of a vector.
//  sum is the R function
//  d1 means it returns a double and a single value
//  Status:  NEEDS TESTING
double VSumd1(SEXP x1){
   double res1;
   int I,i;
   Rcpp::NumericVector X(x1);
   I = X.size();
   res1 = 0.0;
   for(i=0; i<I; i++){
      res1 = res1 + X(i);
   }
   return res1;
}

//////////////////////////////////////////////////////////////////////////////////
//  This function returns the mean of a vector.
//  sum is the R function
//  d1 means it returns a double and a single value
//  Status:  NEEDS TESTING
double VMeand1(SEXP x1){
   double res1;
   int I,i;
   Rcpp::NumericVector X(x1);
   I = X.size();
   res1 = 0.0;
   for(i=0; i<I; i++){
      res1 = res1 + X(i)/I;
   }
   return res1;
}

//////////////////////////////////////////////////////////////////////////////////
//  This function returns the population estimate.
//  given the number counted and detection probability
//  Status:  NEEDS TESTING
SEXP VPopEst(SEXP _N1, SEXP _Detect1){
   int I,i;
   Rcpp::NumericVector N1(_N1);
   Rcpp::NumericVector Detect1(_Detect1);
   I = N1.size();
   Rcpp::NumericVector res1(I);
   for(i=0; i<I; i++){
      res1(i) = N1(i)/Detect1(i);
   }
   return res1;
}

//////////////////////////////////////////////////////////////////////////////////
//  This function returns the sum of a Matrix.
//  sum is the R function
//  Status:  PASSED TESTING
double MSumd1(SEXP x1){
   double res1;
   int I, i, J, j;
   Rcpp::NumericMatrix X(x1);
   I = X.nrow(), J=X.ncol();
   res1 = 0.0;
   for(i=0; i<I; i++){
     for(j=0; j<J; j++){
       res1 = res1 + X(i,j);
     }
   }
   return res1;
}

//////////////////////////////////////////////////////////////////////////////////
//  This function returns the means of the columns of a matrix x1.
//  sum is the R function
//  Status:  PASSED TESTING
SEXP MMean1(SEXP x1){
   int I, i, J, j;
   Rcpp::NumericMatrix X(x1);
   I = X.nrow(), J=X.ncol();
   Rcpp::NumericVector res1(J);
   double res1a;
   for(j=0; j<J; j++){
     res1a = 0.0;
     for(i=0; i<I; i++){
       res1a = res1a + X(i,j)/I;
     }
     res1(j) = res1a;
   }
   return res1;
}

//////////////////////////////////////////////////////////////////////////////////
//  This function returns the sums of the columns of a matrix x1.
//  sum is the R function
//  Status:  PASSED TESTING
SEXP MSum1(SEXP x1){
   int I, i, J, j;
   Rcpp::NumericMatrix X(x1);
   I = X.nrow(), J=X.ncol();
   Rcpp::NumericVector res1(J);
   double res1a;
   for(j=0; j<J; j++){
     res1a = 0.0;
     for(i=0; i<I; i++){
       res1a = res1a + X(i,j);
     }
     res1(j) = res1a;
   }
   return res1;
}

//////////////////////////////////////////////////////////////////////////////////
//  This function fills a matrix with a single value.
//  matrix(x1,nrow=I,ncol=J) is the R function
//  Status:  NEEDS TESTING
SEXP Mfill1(double x1, int I, int J){
   int  i, j;
   Rcpp::NumericMatrix res1(I,J);
   for(i=0; i<I; i++){
     for(j=0; j<J; j++){
       res1(i,j) = x1;
     }
   }
   return res1;
}

////////////////////////////////////////////////////////////////////////////////////
//  Logistic function
//  x1 is a scalar (double)
//  returns a scalar (double)
//  Status:  PASSED TESTING
double logistic1(double x1){
  double result1;
  result1 = exp(x1)/(1+exp(x1));
  return result1;
}

//////////////////////////////////////////////////////////////////////////////////
//  VonBertanaLafi (however you spell it) Growth Functon
//  t1 is latter time point
//  t0 is early time point
//  k1 is growth parameter
//  L01 is floor (smallest value)
//  Linf1 is ceiling (largest value)
//  Status:  PASSED TESTING
double VonBert(double tt1, double tt0, double k1, double L01, double Linf1){
  //double tt1=Rcpp::as<double>(t1),tt0=Rcpp::as<double>(t0);
  double dif1=tt1-tt0;
  double result;
  double result1 = Linf1 - (Linf1 - L01)*exp(-k1*dif1);
  if( result1 < L01){
    result = L01;
  }else{
    result = result1;
  }
  return result;
}


//////////////////////////////////////////////////////////////////////////////////
//  VonBertanaLafi Full Conditional (however you spell it) Growth Functon
//  t1 is latter time point
//  t0 is early time point
//  k1 is growth parameter
//  L01 is floor (smallest value)
//  Linf1 is ceiling (largest value)
//  Status:  PASSED TESTING
double VonBertFC(double tt1, double tt0, double k1, double L01, double Linf1, double sigma1){
  //double tt1=Rcpp::as<double>(t1),tt0=Rcpp::as<double>(t0);
  double dif1=tt1-tt0;
  double result;
  double m1 = Linf1 - (Linf1 - L01)*exp(-k1*dif1);
  double tau0 = (Linf1 - L01)*(Linf1 - L01)/4;
  double m0 = (Linf1 -  L01)/2;
  double sigma2 = sigma1*sigma1;
  double mp1 = (1/tau0*m0 + 1/sigma2*m1)/(1/tau0 + 1/sigma2);
  double vp1 = 1/(1/tau0 + 1/sigma2);
  double result1 = mp1 + sqrt(vp1)*rnormd1(0.0,1.0);
  if( result1 < L01){
    result = L01;
  }else{
    result = result1;
  }
  return result;
}


//////////////////////////////////////////////////////////////////////////////////
//  VonBertanaLafi (however you spell it) Growth Functon for a matrix
//  t1 is latter time point Matrix
//  t0 is early time point  Matrix
//  k1 is growth parameter
//  L01 is floor (smallest value)
//  Linf1 is ceiling (largest value)
SEXP VonBertLM(SEXP tt1, SEXP tt0i1, SEXP tt0i2, double k1,double L01, double Linf1){
  Rcpp::NumericVector t1(tt1), t0i2(tt0i2);
  Rcpp::NumericMatrix t0i1(tt0i1);
  int I = t0i1.nrow(), J=t0i1.ncol();
  Rcpp::NumericMatrix Lij(I,J);
  for(int i=0;i<I;i++){
    for(int j=0;j<J;j++){
      if(t0i1(i,j)==1){
	Lij(i,j) = VonBert(t1(j),t0i2(i),k1,L01,Linf1);
      }
    }
  }
  return Lij;
}

//////////////////////////////////////////////////////////////////////////////////
//  This returns the likelihood associated with capture
//  x1  is a matrix
//  ab1 is a matrix for if the fish has been born or not
//  ad1 is a matrix for if the fish has died or not
//  Dij is a matrix corresponding to the likelihood of capture of the individual fish
//  Status:  PASSED TESTING
double capture2(SEXP x1, SEXP ab1, SEXP ad1, SEXP Dij){
  Rcpp::NumericMatrix x(x1);
  Rcpp::NumericMatrix ab(ab1);
  Rcpp::NumericMatrix ad(ad1);
  Rcpp::NumericMatrix D(Dij);
  int I = x.nrow(), J = x.ncol();
  double test1;
  double resulter = 0.0;
    for(int i=0;i<I;i++){
      for(int j=0;j<J;j++){
	test1 = ab(i,j)*ad(i,j);
        if((test1==1) & (D(i,j)>0.0)){
	  resulter = resulter + log(D(i,j));
	}else{
	  resulter = resulter + log(1-D(i,j));
        }
        if(ISNAN(resulter)){
	  //Rcpp::Rcout << "i: " << i << " j:" << j << " Dij: " << D(i,j) << std::endl;
	}
      }
    }
    //Rcpp::Rcout << "Capture 2: " << resulter << std::endl;
  return resulter;
}

//////////////////////////////////////////////////////////////////////////////////
// This returns the likelihood of mortality between now and the next time period
// ab1 a matrix indicating whether the fish has been born
// ad1 a matrxi indicating whether the fish has died
// S1 is a matrix of probabilities that a specific fish will die.
// Status:  PASSED TESTING
double mortality1( SEXP ab1, SEXP ad1, SEXP S1){
  Rcpp::NumericMatrix ab(ab1);
  Rcpp::NumericMatrix ad(ad1);
  Rcpp::NumericMatrix S(S1);
  int I,J,i,j, flag1;
  double result=0.0;
  I = ad.nrow();
  J = ad.ncol();
  for(i=0; i<I; i++){
    for(j=0; j<J; j++){
      flag1 = ab(i,j)*ad(i,j);
      if(flag1==1){
	result = result +log(S(i,j));
      }
      else{
	result = result +log(1 - S(i,j));
      }
    }
   }
   return result;
}

//////////////////////////////////////////////////////////////////////////////////
// This returns the likelihood of birth between now and the next time period
// ab1 a matrix indicating whether the fish has been born
// csi2 is a matrix of probabilities that a specific fish will be born.
// Status:  PASSED TESTING
double birth1( SEXP ab1, SEXP csi2){
  Rcpp::NumericMatrix csi(csi2);
  Rcpp::NumericMatrix ab(ab1);
  int I, i, J, j;
  I = csi.nrow(), J=csi.ncol();
  double result1 = 0.0;
  for(i=0; i<I; i++){
    for(j=0; j<J; j++){
      if(ab(i,j)==1){
        result1 = result1 + log(csi(i,j));
      }
      else{
        result1 = result1 + log(1- csi(i,j));
      }
    }
  }
  return result1;
}

////////////////////////////////////////////////////////////////////////////////////
//  Survival
//  Sij     Current Matrix of Survival probabilities (used for sizing)
//  b0      Current scalar of b0 survival function coefficients
//  time1   Vector of sampling times.
//   b1      Relationship of fish to length
//   Lij     Length of the fish as modeled.
//   Dij     Data on length of fish.
//   Note this assumes that the initial survival probability is the same
//   Status:  PASSED TESTING
SEXP Surv3(SEXP Sij, double b0, double b1, SEXP time1, SEXP Dij, SEXP Lij){
  Rcpp::NumericMatrix Sij1(Sij);
  Rcpp::NumericMatrix Dij1(Dij);
  Rcpp::NumericMatrix Lij1(Lij);
  Rcpp::NumericVector time11(time1);
  int I = Sij1.nrow(), J = Sij1.ncol(),i,j;
  Rcpp::NumericMatrix result1(I,J);
  for(i=0; i<I; i++){
      for(j=0; j<J; j++){
	  if(Dij1(i,j) > 0.0){
	    result1(i,j) = logistic1(b0 + b1*Dij1(i,j));
          }else{
            result1(i,j) = logistic1(b0 + b1*Lij1(i,j));
          }
      }
   }
   return result1;
}

////////////////////////////////////////////////////////////////////////////////////
//  Survival
//  b0      Current scalar of b0 survival function coefficients
//  time1   Vector of sampling times.
//   b1      Relationship of fish to length
//   Lij     Length of the fish as modeled.
//   Dij     Data on length of fish.
//   Note this assumes that the initial survival probability is the same
//   Status:  PASSED TESTING
SEXP Surv4(double b0, double b1, SEXP Dij, SEXP Lij){
  Rcpp::NumericMatrix Dij1(Dij);
  Rcpp::NumericMatrix Lij1(Lij);
  int I = Dij1.nrow(), J = Dij1.ncol(),i,j;
  Rcpp::NumericMatrix result1(I,J);
  for(i=0; i<I; i++){
      for(j=0; j<J; j++){
	  if(Dij1(i,j) > 0.0){
	    result1(i,j) = logistic1(b0 + b1*Dij1(i,j));
          }else{
            result1(i,j) = logistic1(b0 + b1*Lij1(i,j));
          }
      }
   }
   return result1;
}


///////////////////////////////////////////////////////////////////////////////
//  Detection
//  Dij     Data matrix to ensure that the real values are used
//  Lij     Current Matrix of lengths
//  g00     Current scalar of intercept
//  gj0     Current vector of offsets due to sampling time
//  gl0     Current slope associated with length by location
//  Status:  PASSED TESTING
SEXP Detect2(SEXP Diji, SEXP Liji, double g00, double gl0){
  Rcpp::NumericMatrix Dij(Diji);
  Rcpp::NumericMatrix Lij(Liji);
  int I, J, i, j;
  I = Dij.nrow(), J = Dij.ncol();
  Rcpp::NumericMatrix DijOut(I,J);
  for(i=0; i<I; i++){
    for(j=0; j<J; j++){
      if(Dij(i,j) > 0.0){
        DijOut(i,j) = logistic1(g00 + gl0*Dij(i,j));
      }else{
        DijOut(i,j) = logistic1(g00 + gl0*Lij(i,j));
      }
    }
  }
  return DijOut;
}

////////////////////////////////////////////////////////////////////////////////
//  LengthLike0
//  Dij       Length matrix with values when captured and 0 otherwise
//  Lij       Expected length
//  sigma1    variance parameter
//  Status:  PASSED TESTING
double LengthLike0(SEXP Diji, SEXP Liji, double sigma1){
  Rcpp::NumericMatrix Dij(Diji);
  Rcpp::NumericMatrix Lij(Liji);
  int I, J, i, j;
  I = Dij.nrow(), J = Dij.ncol();
  double result1 = 0.0;
  for(i=0; i<I; i++){
    for(j=0; j<J; j++){
      //if(Dij(i,j)>0.0){
        result1 = result1 +  dnormlogd1(Dij(i,j), Lij(i,j), sigma1);
      //}
    }
  }
  return result1;
}

/////////////////////////////////////////////////////////////////////////////////////
//  birthSample2
//  csi1      probability of birth parameter
//  csi1step  step value for csi1 parameter
//  csibeta1  first parameter in the beta prior distribution for csi1
//  csibeta2  second paramter in the beta prior distribution for csi1
//  Also needs  ab1, D0caplike, S0Mortlike, g00prior, b0prior, k0prior, L0Like1, C0birthlike , csi2prior, totalpost0
//  Status:  PASSED TESTING      Verify list is correctly created.
SEXP birthSample2(SEXP csi1, SEXP csi1step, SEXP ab1,
		  SEXP csibeta1, SEXP csibeta2, SEXP C0birthlike, SEXP csiprior, SEXP ones1){
  double csi11 = Rcpp::as<double>(csi1);
  double csi1step1 = Rcpp::as<double>(csi1step);
  Rcpp::NumericMatrix ab11(ab1);
  Rcpp::NumericMatrix one1(ones1);
  int I=one1.nrow(), J=one1.ncol(), i, j;
  Rcpp::NumericMatrix csi1testM(I,J);
  Rcpp::NumericMatrix csi1currM(I,J);
  double csi1beta1 = Rcpp::as<double>(csibeta1), csi1beta2 = Rcpp::as<double>(csibeta2);
  double C0bl1 = Rcpp::as<double>(C0birthlike), csiprior1 = Rcpp::as<double>(csiprior);
  double csi1test, C0bl1proposed, csipriorproposed, C0bl1current, csipriorcurrent, Unif0, postdiff1;
  csi1test = csi11 + csi1step1*rnormd1(0.0,1.0);
  if(csi1test > 0.0){
     if(csi1test < 1.0){
       csipriorcurrent = dbetalogd1(csi11, csi1beta1, csi1beta2);
       csipriorproposed = dbetalogd1(csi1test, csi1beta1, csi1beta2);
        //Create the matrix
        for(i=0; i<I; i++){
          for(j=0; j<J; j++){
	    csi1testM(i,j) = csi1test*1.0;
            csi1currM(i,j) = csi11*1.0;
          }
        }
        C0bl1proposed = birth1(ab11, csi1testM);
        C0bl1current = birth1(ab11, csi1currM);
        postdiff1 = C0bl1proposed - C0bl1current + csipriorproposed - csipriorcurrent;
        Unif0 = log(runifd1(0.0,1.0));
        if( postdiff1 > Unif0 ){
	  csi11 = csi1test*1.0;
          csiprior1 = csiprior1 + csipriorproposed - csipriorcurrent;
          C0bl1 = C0bl1 + C0bl1proposed - C0bl1current;
        }
     }
  }
  return Rcpp::List::create(Rcpp::Named("csi1") = csi11,
                            Rcpp::Named("csiprior") = csiprior1,
                            Rcpp::Named("C0birthlike") = C0bl1);
}

/////////////////////////////////////////////////////////////////////////////////////
//  birthSample2
//  csi1      probability of birth parameter
//  csi1step  step value for csi1 parameter
//  csibeta1  first parameter in the beta prior distribution for csi1
//  csibeta2  second paramter in the beta prior distribution for csi1
//  Also needs  ab1, D0caplike, S0Mortlike, g00prior, b0prior, k0prior, L0Like1, C0birthlike , csi2prior, totalpost0
//  Status:  PASSED TESTING      Verify list is correctly created.
SEXP birthSample1(double csi11, double csi1step1, SEXP _ab1,
		  double csi1beta1, double csi1beta2, double C0bl1, double csiprior1){
  //double csi11 = Rcpp::as<double>(_csi11);
  //double csi1step1 = Rcpp::as<double>(_csi1step1);
  Rcpp::NumericMatrix ab11(_ab1);
  int I=ab11.nrow(), J=ab11.ncol(), i, j;
  Rcpp::NumericMatrix csi1testM(I,J);
  Rcpp::NumericMatrix csi1currM(I,J);
  //double csi1beta1 = Rcpp::as<double>(_csi1beta1), csi1beta2 = Rcpp::as<double>(_csi1beta2);
  //double C0bl1 = Rcpp::as<double>(_C0bl1), csiprior1 = Rcpp::as<double>(_csiprior1);
  double csi1test, C0bl1proposed, csipriorproposed, C0bl1current, csipriorcurrent, Unif0, postdiff1;
  csi1test = csi11 + csi1step1*rnormd1(0.0,1.0);
  if(csi1test > 0.0){
     if(csi1test < 1.0){
       csipriorcurrent = dbetalogd1(csi11, csi1beta1, csi1beta2);
       csipriorproposed = dbetalogd1(csi1test, csi1beta1, csi1beta2);
        //Create the matrix
        for(i=0; i<I; i++){
          for(j=0; j<J; j++){
	    csi1testM(i,j) = csi1test*1.0;
            csi1currM(i,j) = csi11*1.0;
          }
        }
        C0bl1proposed = birth1(ab11, csi1testM);
        C0bl1current = birth1(ab11, csi1currM);
        postdiff1 = C0bl1proposed - C0bl1current + csipriorproposed - csipriorcurrent;
        Unif0 = log(runifd1(0.0,1.0));
        if( postdiff1 > Unif0 ){
	  csi11 = csi1test*1.0;
          csiprior1 = csiprior1 + csipriorproposed - csipriorcurrent;
          C0bl1 = C0bl1 + C0bl1proposed - C0bl1current;
        }
     }
  }
  return Rcpp::List::create(Rcpp::Named("csi1") = wrap(csi11),
                            Rcpp::Named("csiprior") = wrap(csiprior1),
                            Rcpp::Named("C0birthlike") = wrap(C0bl1));
}


//////////////////////////////////////////////////////////////////////////////////
//  Sample mortality parameter
//  Status:  PASSED TESTING     Need to make sure the list is created correctly.
SEXP b00Sample4(SEXP _b0, SEXP _b1, SEXP _b0step, SEXP _b1m, SEXP _b1s,
                           SEXP Diji, SEXP Siji, SEXP Liji, SEXP ad1i, SEXP ab1i, SEXP time1i, SEXP _S0MortLike, SEXP _b0prior){
  double b0 = Rcpp::as<double>(_b0);
  double b1 = Rcpp::as<double>(_b1);
  double b0step = Rcpp::as<double>(_b0step);
  double b1m = Rcpp::as<double>(_b1m);
  double b1s = Rcpp::as<double>(_b1s);
  double S0MortLike = Rcpp::as<double>(_S0MortLike);
  double b0prior = Rcpp::as<double>(_b0prior);
  Rcpp::NumericMatrix Dij(Diji);
  Rcpp::NumericMatrix Sij(Siji);
  Rcpp::NumericMatrix Lij(Liji);
  Rcpp::NumericMatrix ab1(ab1i);
  Rcpp::NumericMatrix ad1(ad1i);
  Rcpp::NumericMatrix time1(time1i);
  double S0MLcurrent, S0MLproposed, b0priorCurrent, b0test, b0priorprop, S0Test0, S0Test1, postdiff1, Unif0;
  //int I=Sij.nrow(), J=Sij.ncol();
  S0MLcurrent = mortality1(ad1, ab1, Sij);
  b0priorCurrent = dnormlogd1(b0, b1m, b1s);
  b0test = b0 + b0step*rnormd1(0.0,1.0);
  SEXP SijOut;
  SEXP SijTest = Surv4(b0test, b1, Dij, Lij);
  S0MLproposed = mortality1(ad1, ab1, SijTest);
  b0priorprop = dnormlogd1(b0test, b1m, b1s);
  S0Test0 = S0MLcurrent + b0priorCurrent;
  S0Test1 = S0MLproposed + b0priorprop;
  postdiff1 = S0Test1 - S0Test0;
  Unif0 = log(runifd1(0.0,1.0));
  if(postdiff1 > Unif0){
    b0 = b0test*1.0;
    b0prior = b0prior - b0priorCurrent + b0priorprop;
    SijOut = Rcpp::clone(SijTest);
    S0MortLike = S0MortLike - S0MLcurrent + S0MLproposed;
  }else{
    SijOut = Rcpp::clone(Sij);
  }
  return Rcpp::List::create(Rcpp::Named("b0") = wrap(b0),
			    Rcpp::Named("b0test") = wrap(b0test),
                            Rcpp::Named("b0prior") = wrap(b0prior),
                            Rcpp::Named("S0MortLike") = wrap(S0MortLike),
                            Rcpp::Named("Sij0") = SijOut);
}


//////////////////////////////////////////////////////////////////////////////////
//  Sample mortality parameter
//  Status:  PASSED TESTING     Need to make sure the list is created correctly.
SEXP b00Sample1(double b0, double b1, double b0step, double b1m, double b1s,
                           SEXP Diji, SEXP Siji, SEXP Liji, SEXP ad1i, SEXP ab1i, SEXP time1i, double S0MortLike, double b0prior){
  // double b0 = Rcpp::as<double>(_b0);
  // double b1 = Rcpp::as<double>(_b1);
  // double b0step = Rcpp::as<double>(_b0step);
 // double b1m = Rcpp::as<double>(_b1m);
  // double b1s = Rcpp::as<double>(_b1s);
  // double S0MortLike = Rcpp::as<double>(_S0MortLike);
  // double b0prior = Rcpp::as<double>(_b0prior);
  Rcpp::NumericMatrix Dij(Diji);
  Rcpp::NumericMatrix Sij(Siji);
  Rcpp::NumericMatrix Lij(Liji);
  Rcpp::NumericMatrix ab1(ab1i);
  Rcpp::NumericMatrix ad1(ad1i);
  Rcpp::NumericVector time1(time1i);
  double S0MLcurrent, S0MLproposed, b0priorCurrent, b0test, b0priorprop, S0Test0, S0Test1, postdiff1, Unif0;
  //int I=Sij.nrow(), J=Sij.ncol();
  S0MLcurrent = mortality1(ad1, ab1, Sij);
  b0priorCurrent = dnormlogd1(b0, b1m, b1s);
  b0test = b0 + b0step*rnormd1(0.0,1.0);
  SEXP SijOut;
  SEXP SijTest = Surv4(b0test, b1, Dij, Lij);
  S0MLproposed = mortality1(ad1, ab1, SijTest);
  b0priorprop = dnormlogd1(b0test, b1m, b1s);
  S0Test0 = S0MLcurrent + b0priorCurrent;
  S0Test1 = S0MLproposed + b0priorprop;
  postdiff1 = S0Test1 - S0Test0;
  Unif0 = log(runifd1(0.0,1.0));
  if(postdiff1 > Unif0){
    b0 = b0test*1.0;
    b0prior = b0prior - b0priorCurrent + b0priorprop;
    SijOut = Rcpp::clone(SijTest);
    S0MortLike = S0MortLike - S0MLcurrent + S0MLproposed;
  }else{
    SijOut = Rcpp::clone(Sij);
  }
  return Rcpp::List::create(Rcpp::Named("b0") = wrap(b0),
			    Rcpp::Named("b0test") = wrap(b0test),
                            Rcpp::Named("b0prior") = wrap(b0prior),
                            Rcpp::Named("S0MortLike") = wrap(S0MortLike),
                            Rcpp::Named("Sij0") = SijOut);
}


///////////////////////////////////////////////////////////////////////////////////////////
//  b1hierSample2
//  b0      vector for probability of survival parameter
//  b0step  step vector for b0 parameter
//  Sij0    Matrix of survival probabilities
//  Also needs  ab1, D0caplike, S0Mortlike, g00prior, b0prior, k0prior, L0Like1, C0birthlike , csi2prior, totalpost0
//  Status:  PASSED TESTING     Make sure the list returns correctly
SEXP b1hierSample2(double b1, double b1sigma, SEXP b01i,
		   double b1priorm, double b1priors, double b1Like, double lambda1, int df1){
  Rcpp::NumericVector b01(b01i);
  //double b1 = Rcpp::as<double>(_b1), b1sigma = Rcpp::as<double>(_b1sigma), b1priorm = Rcpp::as<double>(_b1priorm);
  //double b1priors = Rcpp::as<double>(_b1priors), b1Like = Rcpp::as<double>(_b1Like), lambda1 = Rcpp::as<double>(_lambda1);
  //int df1 = Rcpp::as<int>(_df1);
  int b01n, df2, i;
  double b1LikeCurrent, b1LikeProposed, b01m, b01pm, b01ps, b11, ss1, s1a, b1sigma2 ;
  b1LikeCurrent = dnormlogd1(b1, b1priorm, b1priors) + dchisqinvlogd1(b1sigma*b1sigma, df1);
  b01n = b01.size();
  //Rcout << "---------------------------------------------------" << std::endl;
  //Rcout << "b01n: " << b01n << std::endl;
  b01m = 0.0;
  for(i=0; i<b01n; i++){
    //Rcout << "b01: " << b01(i) << std::endl;
    b01m = b01m + b01(i)/b01n;
  }
  //Rcout << "b01m: " << b01m << std::endl;
  b01pm = (b1priorm/b1priors + b01n*b01m/b1sigma)/(1.0/b1priors + b01n/b1sigma);
  //Rcout << "b01pm: " << b01pm << std::endl;
  //Rcout << "b1sigma: " << b1sigma << std::endl;
  b01ps = 1.0/(1.0/b1priors + b01n/b1sigma);
  //Rcout << "b01ps: " << b01ps << std::endl;
  b11 = b01pm + sqrt(b01ps)*rnormd1(0.0,1.0);
  //Rcout << "b11: " << b11 << std::endl;
  ss1 = lambda1*1.0;
  for(i=0; i<b01n; i++){
    ss1 = ss1 + (b01(i)-b11)*(b01(i)-b11);
  }
  //Rcout << "ss1: " << ss1 << std::endl;
  df2 = df1 + b01n;
  s1a = rchisqd1(df2);
  b1sigma2 = ss1/s1a;
  b1LikeProposed = dnormlogd1(b11, b1priorm, b1priors) + dchisqinvlogd1(b1sigma2, df2);
  b1sigma = sqrt(b1sigma2);
  //Rcout << "b1sigma: " << b1sigma << std::endl;
  b1Like = b1Like - b1LikeCurrent + b1LikeProposed;
  //Rcout << "Done! with b1hierSample2" << std::endl;
  return Rcpp::List::create(Rcpp::Named("b1") = b11,
			    Rcpp::Named("b01m") = b01m,
			    Rcpp::Named("b01pm") = b01pm,
			    Rcpp::Named("b01ps") = b01ps,
                            Rcpp::Named("b1sigma") = b1sigma,
                            Rcpp::Named("b1Like") = b1Like);
}

////////////////////////////////////////////////////////////////////////////////
//  Sample mortality parameter
//  Status:  PASSED TESTING      make sure the list returns correctly
SEXP bl0Sample1(double b0, double bl0, double bl0step, double b1m, double b1s, SEXP Diji, SEXP Siji,
                           SEXP Liji, SEXP ad1i, SEXP ab1i, SEXP time1i, double S0MortLike, double bl0prior){
  Rcpp::NumericMatrix Dij(Diji);
  Rcpp::NumericMatrix Sij(Siji);
  Rcpp::NumericMatrix Lij(Liji);
  Rcpp::NumericMatrix ab1(ab1i);
  Rcpp::NumericMatrix ad1(ad1i);
  Rcpp::NumericVector time1(time1i);
  //double b0 = Rcpp::as<double>(_b0), bl0 = Rcpp::as<double>(_bl0), bl0step = Rcpp::as<double>(_bl0step);
  //double b1m = Rcpp::as<double>(_b1m), b1s = Rcpp::as<double>(_b1s), S0MortLike = Rcpp::as<double>(_S0MortLike);
  //double bl0prior = Rcpp::as<double>(_bl0prior);
  double S0MLcurrent, S0MLproposed, bl0priorCurrent, bl0test, bl0priorprop, S0Test0, S0Test1, postdiff1, Unif0;
  //int I=Sij.nrow(), J=Sij.ncol();
  SEXP SijOut, SijTest;
  S0MLcurrent = mortality1(ad1, ab1, Sij);
  bl0priorCurrent = dnormlogd1(bl0, b1m, b1s);
  bl0test = bl0 + bl0step*rnormd1(0.0,1.0);
  SijTest = Surv4(b0, bl0test, Dij, Lij);
  S0MLproposed = mortality1(ad1, ab1, SijTest);
  bl0priorprop = dnormlogd1(bl0test, b1m, b1s);
  S0Test0 = S0MLcurrent + bl0priorCurrent;
  S0Test1 = S0MLproposed + bl0priorprop;
  postdiff1 = S0Test1 - S0Test0;
  Unif0 = log(runifd1(0.0,1.0));
  if(postdiff1 > Unif0){
    bl0 = bl0test*1.0;
    bl0prior = bl0prior - bl0priorCurrent + bl0priorprop;
    SijOut = Rcpp::clone(SijTest);
    S0MortLike = S0MortLike - S0MLcurrent + S0MLproposed;
  }else{
    SijOut = Rcpp::clone(Sij);
  }
  return Rcpp::List::create(Rcpp::Named("b0") = bl0,
                            Rcpp::Named("b0prior") = bl0prior,
                            Rcpp::Named("S0MortLike") = S0MortLike,
                            Rcpp::Named("Sij0") = SijOut);
}

///////////////////////////////////////////////////////////////////////////////////
//  g00sample3
//  g00      probability of birth parameter
//  g00step  step value for g00 parameter
//  Lij      Matrix of lengths
//  Also needs  ab1, D0caplike, S0Mortlike, g00prior, totalpost0
//  Status:  PASSED TESTING
SEXP g00Sample1(double g00, double g00step, double gl0, double g01, double g01sigma, double g00prior,
                SEXP d1i, SEXP Liji, SEXP Dij0i, SEXP t0i1i, SEXP ad1i, SEXP ab1i, double D0caplike){
  double g00priorcurrent, g00priorproposed, D0capcurrent, D0test1, g00test, D1caplike, D1test1, postdiff1, Unif0;
  //double g00 = Rcpp::as<double>(_g00), g00step = Rcpp::as<double>(_g00step), gl0 = Rcpp::as<double>(_gl0);
  //double g01 = Rcpp::as<double>(_g01), g01sigma = Rcpp::as<double>(_g01sigma), g00prior = Rcpp::as<double>(_g00prior);
  //double D0caplike = Rcpp::as<double>(_D0caplike);
  Rcpp::NumericMatrix d1(d1i);
  Rcpp::NumericMatrix Lij(Liji);
  Rcpp::NumericMatrix Dij0(Dij0i);
  Rcpp::NumericMatrix t0i1(t0i1i);
  Rcpp::NumericMatrix ad1(ad1i);
  Rcpp::NumericMatrix ab1(ab1i);
  Rcpp::NumericMatrix Dij=Rcpp::clone(Dij0);
  int I, J;
  I = Dij0.nrow(), J = Dij0.ncol();
  Rcpp::NumericMatrix Dij1(I,J);
  //Rcpp::Rcout << "g00sample --------------------------------" << std::endl;
  g00priorcurrent = dnormlogd1(g00, g01, g01sigma);
  D0capcurrent = capture2(t0i1, ab1, ad1, Dij0);
  //Rcpp::Rcout << "D0capcurrent: " << D0capcurrent<< std::endl;
  //Rcpp::Rcout << "g00priorcurrent: " << g00priorcurrent << std::endl;
  D0test1 = D0capcurrent + g00priorcurrent;
  //Rcpp::Rcout << "D0test1: " <<D0test1 <<std::endl;
  g00test = g00 + g00step*rnormd1(0.0,1.0);
  //Rcpp::Rcout << "g00test: " << g00test << std::endl;
  Dij1 = Detect2(d1, Lij, g00test, gl0);
  g00priorproposed = dnormlogd1(g00test, g01, g01sigma);
  D1caplike = capture2(t0i1, ab1, ad1, Dij1);
  D1test1 = D1caplike + g00priorproposed;
  postdiff1 = D1test1 - D0test1;				 // Find the difference in the posteriors
  Unif0 = log(runifd1(0.0,1.0));				 // Generate a log uniform random variable
  if(postdiff1 > Unif0){				         // Determine whether to keep the new sample
    g00 = g00test; 		                                 // update gj0
    g00prior = g00prior - g00priorcurrent + g00priorproposed;    // update the prior value
    Dij = Rcpp::clone(Dij1);
    D0caplike = D0caplike - D0capcurrent + D1caplike;
  }else{
    Dij = Rcpp::clone(Dij0);
  }
  //Rcpp::Rcout << "g00 out: " << g01 << std::endl;
  return Rcpp::List::create(Rcpp::Named("g00") = g00,
			    Rcpp::Named("g00test") = g00test,
                            Rcpp::Named("g00prior") = g00prior,
                            Rcpp::Named("D0caplike") = D0caplike,
                            Rcpp::Named("Dij0") = Dij,
			    Rcpp::Named("PD1") = postdiff1,
			    Rcpp::Named("Unif0") = Unif0
 			  );
}

/////////////////////////////////////////////////////////////////////////////////////////
//  g01hierSample1
//  g01     vector for probability of detection parameter
//  Status  PASSED TESTING
SEXP g01hierSample1(double g01, double g01sigma, SEXP g00i, double g01priorm, double g01priors, double g01Like, double lambda1, int df1){
  double g01sm, g01x, g01pm, g01ps, ss1, s1a, g01sigma2;
  Rcpp::NumericVector g00(g00i);
  //double g01 = Rcpp::as<double>(_g01), g01sigma = Rcpp::as<double>(_g01sigma), g01priorm = Rcpp::as<double>(_g01priorm), g01priors = Rcpp::as<double>(_g01priors);
  //double g01Like = Rcpp::as<double>(_g01Like), lambda1 = Rcpp::as<double>(_lambda1);
  //int df1 = Rcpp::as<int>(_df1);
  int g01n, i, df2;
  g01n = g00.size();
  g01x = g00.size();
  g01sm = VMeand1(g00);
  //Rcpp::Rcout << " g01sigma before: " << g01sigma;
  g01sigma2 = g01sigma*g01sigma;
  //Rcpp::Rcout << " g01sigma2 after: " << g01sigma2;
  g01pm = (g01priorm/g01priors + g01n*g01sm/g01sigma2)/(1.0/g01priors + g01n/g01sigma2);
  g01ps = 1.0/(1.0/g01priors + g01n/g01sigma2);
  g01 = g01pm + sqrt(g01ps)*rnormd1(0.0,1.0);
  ss1 = lambda1*1.0;
  for(i=0; i<g01n; i++){
    ss1 = ss1 + (g00(i)-g01)*(g00(i)-g01);
  }
  df2 = df1 + g01x;
  s1a = rchisqd1(df2);
  g01sigma2 = ss1/s1a;
  //Rcpp::Rcout << "In g01hier, g01: "<<g01<<" g01priorm: "<<g01priorm<<" g01priors: "<<g01priors<<" g01sigma2: " << g01sigma2<< " df2: "<<df2<<std::endl;
  g01Like = dnormlogd1(g01, g01priorm, g01priors) + dchisqinvlogd1(g01sigma2, df2);
  g01sigma = sqrt(g01sigma2);
  return Rcpp::List::create(Rcpp::Named("g01") = g01,
			    Rcpp::Named("g01sm") = g01sm,
                            Rcpp::Named("g01sigma") = g01sigma,
                            Rcpp::Named("g01Like") = g01Like);
}

///////////////////////////////////////////////////////////////////////////////////////////
//  gl0sample3
//  gl0      probability of birth parameter
//  gl0step  step value for b0 parameter
//  Lij      Matrix of lengths
//  Also needs  ab1, D0caplike, S0Mortlike, g00prior, b0prior, k0prior, L0Like1, C0birthlike , csi2prior, totalpost0
//  Status:  PASSED TESTING
SEXP gl0Sample1(double gl0, double gl0step, double g00, double gl1, double gl1sigma, double g00prior,
                SEXP d1, SEXP Lij, SEXP Dij0, SEXP t0i1, SEXP ad1, SEXP ab1, double D0caplike){
  double gl1priorcurrent, D0capcurrent, D0test1, gl0test, D1caplike, gl1priorproposed, D1test1, postdiff1, Unif0;
  //double gl0 = as<double>(_gl0), gl0step= as<double>(_gl0step),g00= as<double>(_g00),gl1= as<double>( _gl1),gl1sigma= as<double>(_gl1sigma), g00prior = as<double>(_g00prior), D0caplike=as<double>(_D0caplike);
  SEXP Dij1, Dij0out;
  //Rcpp::Rcout << " ----------------------------------------------------------" << std::endl;
  //Rcpp::Rcout << "gl0: " << gl0 << " g00: " << g00 << " D0caplike: " << D0caplike << std::endl;
  gl1priorcurrent = dnormlogd1(gl0, gl1, gl1sigma);
  D0capcurrent = capture2(t0i1, ab1, ad1, Dij0);
  D0test1 = D0capcurrent + gl1priorcurrent;
  gl0test = gl0 + gl0step*rnormd1(0.0,1.0);
  Dij1 = Detect2(d1, Lij, g00, gl0test);
  D1caplike = capture2(t0i1, ab1, ad1, Dij1);
  gl1priorproposed = dnormlogd1(gl0test, gl1, gl1sigma);
  D1test1 = D1caplike + gl1priorproposed;
  postdiff1 = D1test1 - D0test1;		// Find the difference in the posteriors
  Unif0 = log(runifd1(0.0,0.1));
   //Rcpp::Rcout << "gl0test: "<<gl0test<< std::endl;			// Generate a log uniform random variable
   //Rcpp::Rcout << "Unif0: "<<Unif0<< std::endl;
   //Rcpp::Rcout << "postdiff1: "<<postdiff1<< std::endl;
   //Rcpp::Rcout << " D0capcurrent: " << D0capcurrent<< " D1caplike: " << D1caplike<< std::endl;

   //Rcpp::Rcout << "D0test1: " << D0test1 << " D1test1: " << D1test1 <<std::endl;
if(postdiff1 > Unif0){         		// Determine whether to keep the new sample
    gl0 = gl0test*1.0; 		                // update gj0
    g00prior = g00prior - gl1priorcurrent + gl1priorproposed;       // update the prior value
    Dij0out = Rcpp::clone(Dij1);
    D0caplike = D1caplike - D0capcurrent + D1caplike;
  }else{
    Dij0out = Rcpp::clone(Dij0);
  }
  return Rcpp::List::create(Rcpp::Named("gl0") = gl0,
                            Rcpp::Named("g00prior") = g00prior,
                            Rcpp::Named("Dij0") = Dij0out,
                            Rcpp::Named("D0caplike") = D0caplike);
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//  gl1hierSample1
//  gl1     vector for probability of detection parameter that relate to length
//  Status:  PASSED TESTING
SEXP gl1hierSample1(double gl1, double gl1sigma, SEXP _gl0, double gl1priorm, double gl1priors, double gl1prior, double lambda1, int df1){
   double gl1n, gl1x, gl1sm, gl1sigma2, gl1pm, gl1ps, ss1, s1a;
   int df2, i;
//   double gl1 = as<double>(_gl1), gl1sigma = as<double>(_gl1sigma), gl1priorm = as<double>(_gl1priorm), gl1priors = as<double>(_gl1priors), gl1prior = as<double>(_gl1prior), lambda1= as<double>(_lambda1);
 //  int df1 = as<int>(_df1);
   Rcpp::NumericVector gl0(_gl0);
   gl1n = gl0.size()*1.0;
   gl1x = gl0.size();
   gl1sm = VMeand1(gl0);
   gl1sigma2 = gl1sigma*gl1sigma;
   gl1pm = (gl1priorm/gl1priors + gl1n*gl1sm/gl1sigma2)/(1.0/gl1priors + gl1n/gl1sigma2);
   gl1ps = 1.0/(1.0/gl1priors + gl1n/gl1sigma2);
   gl1 = gl1pm + sqrt(gl1ps)*rnormd1(0.0,0.1);
   ss1 = lambda1*1.0;
   for(i=0; i<gl1n; i++){
     ss1 = ss1 + (gl0(i)-gl1)*(gl0(i)-gl1);
   }
   df2 = df1 + gl1x;
   s1a = rchisqd1(df2);
   gl1sigma2 = ss1/s1a;
   gl1prior = dnormlogd1(gl1, gl1priorm, gl1priors) + dchisqinvlogd1(gl1sigma2, df1);
   gl1sigma = sqrt(gl1sigma2);
   return Rcpp::List::create(Rcpp::Named("gl1") = gl1,
			     Rcpp::Named("gl1sm") = gl1sm,
			     Rcpp::Named("gl1pm") = gl1pm,
                             Rcpp::Named("gl1sigma") = gl1sigma,
                             Rcpp::Named("gl1prior") = gl1prior);
}


////////////////////////////////////////////////////////////////////////////////////////////////
//  k1sample5  Has both the mortality and the detection portions added.
//  k1       growth parameter
//  k1step   step value for k1 parameter
//  Lij      Matrix of lengths
//  Also needs  ab1, D0caplike, S0Mortlike, g00prior, b0prior, k0prior, L0Like1, C0birthlike , csi2prior, totalpost0
//  Status:  NEEDS TESTING
SEXP k1Sample5(double k1, double k1step, double k1a1,double k1b1, SEXP _Lij, double L01, double Linf1, double sigma1, SEXP _Dij0, double g00,
               double gl0, SEXP _d1, SEXP _ti, SEXP _t0i1, SEXP _t0i2, SEXP _ad1, SEXP _ab1, double D0caplike, double S0Mortlike,double k0prior,
               double L0Like1, SEXP _Sij0, double b0, double b1, SEXP _time1){
  double D0caplikeCurrent, S0MortlikeCurrent, L0LikeCurrent, k1priorCurrent, k10test;
  double S0MortlikeProposed, L0LikeProposed, D0caplikeProposed;
  double k1priorproposed, postCurrent, postProposed, postdiff1, Unif0;
  //double //k1 = as<double>(_k1), k1step = as<double>(_k1step),k1a1 = as<double>(_k1a1), k1b1 = as<double>(_k1b1),
    //L01 = as<double>(_L01), Linf1 = as<double>(_Linf1);
  // double sigma1 = as<double>(_sigma1), g00 = as<double>(_g00), gl0 = as<double>(_gl0)//, D0caplike = as<double>(_D0caplike), S0Mortlike = as<double>(_S0Mortlike);
  //double// k0prior = as<double>(_k0prior), L0Like1 = as<double>(_L0Like1),
  // b0 = as<double>(_b0), b1 = as<double>(_b1);
    Rcpp::NumericMatrix Dij1, Sij1, Sij0out, Dij0out, Lij0out;

  int I, J, i, j;
  Rcpp::NumericMatrix t0i1(_t0i1);
  Rcpp::NumericMatrix Lij0(_Lij);
  Rcpp::NumericVector t0i2(_t0i2);
  Rcpp::NumericMatrix ab1(_ab1);
  Rcpp::NumericMatrix ad1(_ad1);
  Rcpp::NumericMatrix Sij0(_Sij0);
  Rcpp::NumericMatrix d1(_d1);
  Rcpp::NumericMatrix Dij0(_Dij0);
  Rcpp::NumericVector time1(_time1);
  double sigma1a = sqrt(sigma1);
  //Rcout << "------------------- Current -----------------" << std::endl;
  //Rcpp::Rcout << "k1: "<< k1 << std::endl;
  D0caplikeCurrent = capture2(t0i1, ab1, ad1, Dij0);
  //Rcpp::Rcout << "Capture: "<< D0caplikeCurrent << std::endl;
  //Rcpp::Rcout << "b0: " << b0 << " bl: " << b1 << std::endl;
  S0MortlikeCurrent = mortality1(ad1, ab1, Sij0);
  //Rcpp::Rcout << "Mortality: "<< S0MortlikeCurrent << std::endl;
  L0LikeCurrent = LengthLike0(d1, Lij0, sigma1a);
  //Rcpp::Rcout << "Length: "<< L0LikeCurrent << std::endl;
  k1priorCurrent = dgammalogd1(k1,k1a1,k1b1);
  //Rcout << "------------------- Proposed -----------------" << std::endl;
  k10test = k1 + k1step*rnormd1(0.0,1.0);
  //Rcpp::Rcout << "k10test: "<< k10test << std::endl;
  double titemp, t02temp;
  Rcpp::NumericVector ti(_ti);
  I = Lij0.nrow(), J = Lij0.ncol();
  Rcpp::NumericMatrix Lij1(I,J);
  if(k10test > 0.0){
    for(i=0; i<I; i++){
      for(j=0; j<J; j++){
        //if(t0i1(i,j) == 1){// here's the problem, t0i1 is a vector not a matrix
	  titemp=ti(j);
          t02temp =t0i2(i); // Also here, t0i2 is a vector..., was t0i2(i,j);
	  Lij1(i,j) = VonBertFC(titemp,t02temp, k10test, L01, Linf1, sigma1a);
	//}
      }
    }
    Dij1 = Detect2(d1, Lij1, g00, gl0);
    Sij1 =  Surv4(b0, b1, d1, Lij1);
    S0MortlikeProposed = mortality1(ad1, ab1, Sij1);
    L0LikeProposed = LengthLike0(d1, Lij1, sigma1a);
    D0caplikeProposed = capture2(t0i1, ab1, ad1, Dij1);
    k1priorproposed = dgammalogd1(k10test, k1a1, k1b1);
    //Rcpp::Rcout << "Capture: "<< D0caplikeProposed << std::endl;
    //Rcpp::Rcout << "Mortality: "<< S0MortlikeProposed << std::endl;
    //Rcpp::Rcout << "Length: "<< L0LikeProposed << std::endl;
    postCurrent = D0caplikeCurrent + S0MortlikeCurrent + L0LikeCurrent + k1priorCurrent;
    postProposed = D0caplikeProposed + S0MortlikeProposed + L0LikeProposed + k1priorproposed;
    postdiff1 = postProposed - postCurrent;
    Unif0 = log(runifd1(0.0,1.0));
    //Rcpp::Rcout << "Postdiff: "<< postdiff1 << "  Unif0: " << Unif0 << std::endl;
    if( postdiff1 > Unif0){
	   k1 = k10test;
	   k0prior = k0prior + k1priorproposed - k1priorCurrent;
	   D0caplike = D0caplike + D0caplikeProposed - D0caplikeCurrent;
	   S0Mortlike = S0Mortlike + S0MortlikeProposed - S0MortlikeCurrent;
	   L0Like1 = L0Like1 + L0LikeProposed - L0LikeCurrent;
           Sij0 = Rcpp::clone(Sij1);
	   Dij0 = Rcpp::clone(Dij1);
	   Lij0 = Rcpp::clone(Lij1);
    }
  }//else{
    //Rcpp::Rcout<< "Compat Negative"<<std::endl;
     //Sij0out = Rcpp::clone(Sij0);
     //Dij0out = Rcpp::clone(Dij0);
     //Lij0out = Rcpp::clone(Lij0);
     //Rcpp::Rcout<< "Compat End"<<std::endl;
   //}
  return Rcpp::List::create(Rcpp::Named("k1") = k1,
                              Rcpp::Named("k0prior") = k0prior,
                              Rcpp::Named("Dij0") = Dij0,
                              Rcpp::Named("Lij0") = Lij0,
                              Rcpp::Named("D0caplike") = D0caplike,
                              Rcpp::Named("L0Like1") = L0Like1,
                              Rcpp::Named("Sij0") = Sij0,
                              Rcpp::Named("S0Mortlike") = S0Mortlike);
}

////////////////////////////////////////////////////////////////////////////////////////////////
//  k1sample5log  Has both the mortality and the detection portions added.
//  k1       growth parameter
//  k1step   step value for k1 parameter
//  Lij      Matrix of lengths
//  Also needs  ab1, D0caplike, S0Mortlike, g00prior, b0prior, k0prior, L0Like1, C0birthlike , csi2prior, totalpost0
//  Status:  NEEDS TESTING
//  Note that the parameters k1a1 and k1b1 are now k1m1 and k1s1.
SEXP k1Samplelog5(double k1, double k1step, double k1m1,double k1s1, SEXP _Lij, double L01, double Linf1, double sigma1, SEXP _Dij0, double g00,
               double gl0, SEXP _d1, SEXP _ti, SEXP _t0i1, SEXP _t0i2, SEXP _ad1, SEXP _ab1, double D0caplike, double S0Mortlike,double k0prior,
               double L0Like1, SEXP _Sij0, double b0, double b1, SEXP _time1){
  double D0caplikeCurrent, S0MortlikeCurrent, L0LikeCurrent, k1priorCurrent, k10test;
  double S0MortlikeProposed, L0LikeProposed, D0caplikeProposed;
  double k1priorproposed, postCurrent, postProposed, postdiff1, Unif0;
  Rcpp::NumericMatrix Dij1, Sij1, Sij0out, Dij0out, Lij0out;
  int I, J, i, j;
  Rcpp::NumericMatrix t0i1(_t0i1);
  Rcpp::NumericMatrix Lij0(_Lij);
  Rcpp::NumericVector t0i2(_t0i2);
  Rcpp::NumericMatrix ab1(_ab1);
  Rcpp::NumericMatrix ad1(_ad1);
  Rcpp::NumericMatrix Sij0(_Sij0);
  Rcpp::NumericMatrix d1(_d1);
  Rcpp::NumericMatrix Dij0(_Dij0);
  Rcpp::NumericVector time1(_time1);
  double sigma1a = sqrt(sigma1);
  //Rcout << "------------------- Current -----------------" << std::endl;
  //Rcpp::Rcout << "k1: "<< k1 << std::endl;
  D0caplikeCurrent = capture2(t0i1, ab1, ad1, Dij0);
  //Rcpp::Rcout << "Capture: "<< D0caplikeCurrent << std::endl;
  //Rcpp::Rcout << "b0: " << b0 << " bl: " << b1 << std::endl;
  S0MortlikeCurrent = mortality1(ad1, ab1, Sij0);
  //Rcpp::Rcout << "Mortality: "<< S0MortlikeCurrent << std::endl;
  L0LikeCurrent = LengthLike0(d1, Lij0, sigma1a);
  //Rcpp::Rcout << "Length: "<< L0LikeCurrent << std::endl;
  ///  This is changed to log normal.
  k1priorCurrent = dnormlogd1(log(k1),k1m1,k1s1);
  //Rcout << "------------------- Proposed -----------------" << std::endl;
  k10test = k1 + k1step*rnormd1(0.0,1.0);
  //Rcpp::Rcout << "k10test: "<< k10test << std::endl;
  double titemp, t02temp;
  Rcpp::NumericVector ti(_ti);
  I = Lij0.nrow(), J = Lij0.ncol();
  Rcpp::NumericMatrix Lij1(I,J);
  if(k10test > 0.0){
    for(i=0; i<I; i++){
      for(j=0; j<J; j++){
        //if(t0i1(i,j) == 1){// here's the problem, t0i1 is a vector not a matrix
	  titemp=ti(j);
          t02temp =t0i2(i); // Also here, t0i2 is a vector..., was t0i2(i,j);
	  Lij1(i,j) = VonBertFC(titemp,t02temp, k10test, L01, Linf1,sigma1a);
	//}
      }
    }
    Dij1 = Detect2(d1, Lij1, g00, gl0);
    Sij1 =  Surv4(b0, b1, d1, Lij1);
    S0MortlikeProposed = mortality1(ad1, ab1, Sij1);
    L0LikeProposed = LengthLike0(d1, Lij1, sigma1a);
    D0caplikeProposed = capture2(t0i1, ab1, ad1, Dij1);
    k1priorproposed = dnormlogd1(log(k10test), k1m1, k1s1);
    //Rcpp::Rcout << "Capture: "<< D0caplikeProposed << std::endl;
    //Rcpp::Rcout << "Mortality: "<< S0MortlikeProposed << std::endl;
    //Rcpp::Rcout << "Length: "<< L0LikeProposed << std::endl;
    postCurrent = D0caplikeCurrent + S0MortlikeCurrent + L0LikeCurrent + k1priorCurrent;
    postProposed = D0caplikeProposed + S0MortlikeProposed + L0LikeProposed + k1priorproposed;
    postdiff1 = postProposed - postCurrent;
    Unif0 = log(runifd1(0.0,1.0));
    //Rcpp::Rcout << "Postdiff: "<< postdiff1 << "  Unif0: " << Unif0 << std::endl;
    if( postdiff1 > Unif0){
	   k1 = k10test;
	   k0prior = k0prior + k1priorproposed - k1priorCurrent;
	   D0caplike = D0caplike + D0caplikeProposed - D0caplikeCurrent;
	   S0Mortlike = S0Mortlike + S0MortlikeProposed - S0MortlikeCurrent;
	   L0Like1 = L0Like1 + L0LikeProposed - L0LikeCurrent;
           Sij0 = Rcpp::clone(Sij1);
	   Dij0 = Rcpp::clone(Dij1);
	   Lij0 = Rcpp::clone(Lij1);
    }
  }//else{
    //Rcpp::Rcout<< "Compat Negative"<<std::endl;
     //Sij0out = Rcpp::clone(Sij0);
     //Dij0out = Rcpp::clone(Dij0);
     //Lij0out = Rcpp::clone(Lij0);
     //Rcpp::Rcout<< "Compat End"<<std::endl;
   //}
  return Rcpp::List::create(Rcpp::Named("k1") = k1,
                              Rcpp::Named("k0prior") = k0prior,
                              Rcpp::Named("Dij0") = Dij0,
                              Rcpp::Named("Lij0") = Lij0,
                              Rcpp::Named("D0caplike") = D0caplike,
                              Rcpp::Named("L0Like1") = L0Like1,
                              Rcpp::Named("Sij0") = Sij0,
                              Rcpp::Named("S0Mortlike") = S0Mortlike);
}

///////////////////////////////////////////////////////////////////////////////////////////
//  k1b1Sample
//  Pulls together the k1 parameters for each of the treatment/nontreatment groups.
//  Status:   PASSED TESTED
SEXP k1b1Sample1(double k1b1, double k1b1step, double k1b2, double k1a2, double k1a1, SEXP _k1, double k0prior){
  double k11currentlike, k1b1test, k1b1testlike, postdiff1, Unif0;
  int I, i;
  Rcpp::NumericVector k1(_k1);
  I = k1.size();
  //double k1b1 = as<double>(_k1b1), k1b1step = as<double>(_k1b1step), k1b2 = as<double>(_k1b2), k1a2 = as<double>(_k1a2), k1a1 = as<double>(_k1a1), k0prior = as<double>(_k0prior);
  k11currentlike = dgammalogd1(k1b1,k1a2,k1b2);
  for(i=0; i<I; i++){
    k11currentlike = k11currentlike + dgammalogd1(k1(i),k1a1,k1b1);
  }
  k1b1test = k1b1 + k1b1step*rnormd1(0.0, 1.0);
  if(k1b1test > 0){
    k1b1testlike = dgammalogd1(k1b1test, k1a2, k1b2);
    for(i=0; i<I; i++){
       k1b1testlike = k1b1testlike + dgammalogd1(k1(i),k1a1,k1b1test);
    }
    postdiff1 = k1b1testlike - k11currentlike;
    Unif0 = log(runifd1(0.0,1.0));
    if(postdiff1 > Unif0){

      k1b1 = k1b1test;
      k0prior = k0prior - k11currentlike + k1b1testlike;
    }
  }

  return Rcpp::List::create(Rcpp::Named("k1b1") = k1b1,
                            Rcpp::Named("k0prior") = k0prior);
}

//////////////////////////////////////////////////////////////////////////////////////////////////
//  gl1hierSample1
//  gl1     vector for probability of detection parameter that relate to length
//  Status:  PASSED TESTING
SEXP k1hierSample1(double k1m1, double k1s1, SEXP _k1, double k1priorm, double k1priors, double k0prior, double lambda1, int df1){
   double k1n, k1x, k1sm, k1sigma2, k1sigma, k1pm, k1ps, ss1, s1a;
   int df2, i;
   Rcpp::NumericVector k1(_k1);
   k1n = k1.size()*1.0;
   k1x = k1.size();
   Rcpp::NumericVector k1log(k1x);
   for(i=0; i<k1n; i++){
     k1log(i) = log(k1(i));
   }
   k1sm = VMeand1(k1log);
   k1sigma2 = k1s1*k1s1;
   k1pm = (k1priorm/k1priors + k1n*k1sm/k1sigma2)/(1.0/k1priors + k1n/k1sigma2);
   k1ps = 1.0/(1.0/k1priors + k1n/k1sigma2);
   k1m1 = k1pm + sqrt(k1ps)*rnormd1(0.0,0.1);
   ss1 = lambda1*1.0;
   for(i=0; i<k1n; i++){
     ss1 = ss1 + (k1log(i)-k1m1)*(k1log(i)-k1m1);
   }
   df2 = df1 + k1x;
   s1a = rchisqd1(df2);
   k1sigma2 = ss1/s1a;
   k0prior = dnormlogd1(k1m1, k1priorm, k1priors) + dchisqinvlogd1(k1sigma2, df1);
   k1sigma = sqrt(k1sigma2);
   return Rcpp::List::create(Rcpp::Named("k1m1") = k1m1,
			     Rcpp::Named("k1sm") = k1sm,
			     Rcpp::Named("k1pm") = k1pm,
                             Rcpp::Named("k1s1") = k1sigma,
                             Rcpp::Named("k0prior") = k0prior);
}



////////////////////////////////////////////////////////////////////////////////////////////
//  sigma1sample2
//  sigma1       growth parameter
//  d1           Matrix of lengths
//  Lij          Matrix of fitted lengths
//  lambda1      lambda parameter for chisquare
//  df1          prior degrees of freedom
//  Status:      NEEDS TESTING
SEXP sigma1sample2(double sigma1, SEXP _t0i1, SEXP _Lij, SEXP _d1, double lambda1,
                   int df1, double L0Like2, double sigma1prior){
   double count1, SSE1, df2, sigma1priorRemoved, s1a, sigma2, L0Like1, L0Like2Removed;
   //Rcpp::Rcout << "In sigma1sample2"<<std::endl;
   Rcpp::NumericMatrix t0i1(_t0i1);
   Rcpp::NumericMatrix Lij(_Lij);
   Rcpp::NumericMatrix d1(_d1);
   //  double sigma1 = as<double>(_sigma1), lambda1 = as<double>(_lambda1), sigma1prior = as<double>(_sigma1prior), L0Like2 = as<double>(_L0Like2);
   //int df1 = as<int>(_df1);
   int I, J, i, j;
   I = d1.nrow(), J=d1.ncol();
   L0Like2Removed = L0Like2 - LengthLike0(d1, Lij, sigma1);
   count1 = 0.0;
   SSE1 = lambda1;
   //Rcpp::Rcout << "In sigma1sample2,prior to loop"<<std::endl;
   for(i=0; i<I; i++){
     for(j=0; j<J; j++){
       //if(t0i1(i,j) == 1){
          SSE1 = SSE1 + (d1(i,j) - Lij(i,j))*(d1(i,j) - Lij(i,j));
	  // Rcpp::Rcout << "In sigma1sample2, in if statement of loop "<< i << " i "<< j << " j " << std::endl;
          count1 = count1 + 1;
       //}
     }
   }
   // Rcpp::Rcout << "In sigma1sample2, through loop"<<std::endl;
   df2 = count1 + df1;
   sigma1priorRemoved = sigma1prior - dchisqinvlogd1(sigma1 + lambda1, df2);
   s1a = rchisqd1(df2);
   sigma2 = sqrt(SSE1/s1a);
   L0Like1 = LengthLike0(d1, Lij, sigma2) + L0Like2Removed;
   sigma1prior = sigma1priorRemoved + dchisqinvlogd1(sigma2*sigma2, df2);
   //Rcpp::Rcout << "In sigma1sample2, about to return"<<std::endl;
   return Rcpp::List::create(Rcpp::Named("sigma2") = wrap(sigma2),
                             Rcpp::Named("L0Like1") = wrap(L0Like1),
                             Rcpp::Named("sigma1prior") = wrap(sigma1prior));
}



extern "C" SEXP indmat(SEXP n){
  Rcpp::NumericMatrix mat(n);
int N = mat.nrow();
Rcpp::NumericMatrix total(N,N);

for(int i=0;i<N;i++){
for(int j=0;j<N;j++){
total(i,j)= i+j+2;
}
}
return wrap(total);
}

extern "C" SEXP multmat(SEXP n){
  Rcpp::NumericMatrix mat(n);
int N = mat.nrow();
Rcpp::NumericMatrix total(N,N);

for(int i=0;i<N;i++){
for(int j=0;j<N;j++){
total(i,j)= (i+1)*(j+1);
}
}
return wrap(total);
}


double maxmat(SEXP _x){
  Rcpp::NumericMatrix x(_x);
  double result1 = -100000000.0;
  int I = x.nrow(), J=x.ncol(), i, j;
  for(i=0;i<I;i++){
    for(j=0;j<J;j++){
      if(x(i,j) > result1){
	result1 = x(i,j);
      }
    }
  }
  return result1;
}

double minmat(SEXP _x){
  Rcpp::NumericMatrix x(_x);
  double result1 = 100000000.0;
  int I = x.nrow(), J=x.ncol(), i, j;
  for(i=0;i<I;i++){
    for(j=0;j<J;j++){
      if(x(i,j) < result1){
	result1 = x(i,j);
      }
    }
  }
  return result1;
}
//SEXP Surv3(SEXP Sij, double b0, double b1, SEXP time1, SEXP Dij, SEXP Lij)
//mortality1( SEXP ab1, SEXP ad1, SEXP S1)
//capture2(SEXP x1, SEXP ab1, SEXP ad1, SEXP Dij)
//double LengthLike0(SEXP Diji, SEXP Liji, double sigma1)
// double VonBert(double tt1, double tt0, double k1, double L01, double Linf1)

extern "C" SEXP Surv3prep(SEXP _Inlist){
   Rcpp::List n2(_Inlist);
   NumericMatrix Sij=n2["Sij"];

   NumericMatrix time1 = n2["time1"];
   NumericMatrix Dij = n2["Dij"];
   NumericMatrix Lij = n2["Lij"];
   double b0 = n2["b0"];
   double b1 = n2["b1"];
   SEXP out=Surv3( Sij,  b0,  b1,  time1,  Dij,  Lij);
   return out;
}


extern "C" SEXP Surv2(SEXP Sij, SEXP _b0, SEXP _b1, SEXP time1, SEXP Dij, SEXP Lij){
  // Rcpp::NumericMatrix Sij1(Sij);
   Rcpp::NumericMatrix Dij1(Dij);
   Rcpp::NumericMatrix Lij1(Lij);
  //Rcpp::NumericVector time11(time1);
  int I = Dij1.nrow(), J = Dij1.ncol(),i,j;
  Rcpp::NumericMatrix result1(I,J);
   double b0=as<double>(_b0), b1=as<double>(_b1);
   for(i=0; i<I; i++){
      for(j=0; j<J; j++){
  	  if(Dij1(i,j) > 0.0){
  	    result1(i,j) = logistic1(b0 + b1*Dij1(i,j));
          }else{
            result1(i,j) = logistic1(b0 + b1*Lij1(i,j));
          }
      }
   }

  return wrap(result1);
}

//extern "C" SEXP try1( SEXP
//		double out;
//logistic1(b0 + b1*Dij1[i,j])
extern "C" SEXP mortalityR( SEXP ab1, SEXP ad1, SEXP S1){
  Rcpp::NumericMatrix ab(ab1);
  Rcpp::NumericMatrix ad(ad1);
  Rcpp::NumericMatrix S(S1);
  int I,J,i,j, flag1;
  double result=0.0;
  I = ad.nrow();
  J = ad.ncol();
  for(i=0; i<I; i++){
    for(j=0; j<J; j++){
      flag1 = ab(i,j)*ad(i,j);
      if(flag1==1){
	result = result +log(S(i,j));
      }
      else{
	result = result +log(1 - S(i,j));
      }
    }
   }
  return wrap(result);
}

extern "C" SEXP DetectR(SEXP Diji, SEXP Liji, SEXP _g00, SEXP _gl0){
  Rcpp::NumericMatrix Dij(Diji);
  Rcpp::NumericMatrix Lij(Liji);
  double g00 = as<double>(_g00),gl0=as<double>(_gl0);
  int I, J, i, j;
  I = Dij.nrow(), J = Dij.ncol();
  Rcpp::NumericMatrix DijOut(I,J);
  for(i=0; i<I; i++){
    for(j=0; j<J; j++){
      if(Dij(i,j) > 0.0){
        DijOut(i,j) = logistic1(g00 + gl0*Dij(i,j));
      }else{
        DijOut(i,j) = logistic1(g00 + gl0*Lij(i,j));
      }
    }
  }
  return wrap(DijOut);
}

extern "C" SEXP birthR( SEXP ab1, SEXP csi2){
  Rcpp::NumericMatrix csi(csi2);
  Rcpp::NumericMatrix ab(ab1);
  int I, i, J, j;
  I = csi.nrow(), J=csi.ncol();
  double result1 = 0.0;
  for(i=0; i<I; i++){
    for(j=0; j<J; j++){
      if(ab(i,j)==1){
        result1 = result1 + log(csi(i,j));
      }
      else{
        result1 = result1 + log(1- csi(i,j));
      }
    }
  }
  return wrap(result1);
}

extern "C" SEXP captureR(SEXP x1, SEXP ab1, SEXP ad1, SEXP Dij){
  //Rcpp::Rcout << "Made it2?" << std::endl;
  Rcpp::NumericMatrix x(x1);
  Rcpp::NumericMatrix ab(ab1);
  Rcpp::NumericMatrix ad(ad1);
  Rcpp::NumericMatrix D(Dij);
  int I = x.nrow(), J = x.ncol();
  double test1;
  double resulter = 0.0;
    for(int i=0;i<I;i++){
      for(int j=0;j<J;j++){
	test1 = ab(i,j)*ad(i,j);
        if((test1==1) & (D(i,j)>0.0)){
	  resulter = resulter + log(D(i,j));
	}else{
	  resulter = resulter + log(1-D(i,j));
        }
      }
    }
    return wrap(resulter);
}

extern "C" SEXP LengthLikeR(SEXP Diji, SEXP Liji, SEXP _sigma1){
  Rcpp::NumericMatrix Dij(Diji);
  Rcpp::NumericMatrix Lij(Liji);
  double sigma1=as<double>(_sigma1);
  int I, J, i, j;
  I = Dij.nrow(), J = Dij.ncol();
  double result1 = 0.0;
  for(i=0; i<I; i++){
    for(j=0; j<J; j++){
      if(Dij(i,j)>0.0){
        result1 = result1 +  dnormlogd1(Dij(i,j), Lij(i,j), sigma1);
      }
    }
  }

  return wrap(result1);
}


/// /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///  A function to sample the birth parameter
///  Requires a list as an input
///
extern "C" SEXP csi_sample1(SEXP list_in1, SEXP _nsim1){
   //Rcout << " In Function" << std::endl;
   Rcpp::List in1(list_in1);
   int nsim1 = as<int>(_nsim1);
   //Rcpp::Rcout <<"Sims: "<< nsim1 <<std::endl;
   NumericMatrix csi2cc = in1["csi2cc"];
   NumericMatrix csi2ci = in1["csi2ci"];
   NumericMatrix csi2lc = in1["csi2lc"];
   NumericMatrix csi2li = in1["csi2li"];
   NumericMatrix csi2tc = in1["csi2tc"];
   NumericMatrix csi2ti = in1["csi2ti"];
   NumericMatrix csi2uc = in1["csi2uc"];
   NumericMatrix csi2ui = in1["csi2ui"];

   double csiprior = as<double>(in1["csiprior"]);
   double C0birthlike = as<double>(in1["C0birthlike"]);
   double csibeta1 = as<double>(in1["csibeta1"]);
   double csibeta2 = as<double>(in1["csibeta2"]);

   NumericVector csi1 = in1["csi1"];
   NumericVector csi1step = in1["csi1step"];
   NumericMatrix ccab1 = in1["ccab1"];
   NumericMatrix ciab1 = in1["ciab1"];
   NumericMatrix lcab1 = in1["lcab1"];
   NumericMatrix liab1 = in1["liab1"];
   NumericMatrix tcab1 = in1["tcab1"];
   NumericMatrix tiab1 = in1["tiab1"];
   NumericMatrix ucab1 = in1["ucab1"];
   NumericMatrix uiab1 = in1["uiab1"];

   NumericVector b00 = in1["b00"];
   NumericVector bl0 = in1["bl0"];
   NumericVector g00 = in1["g00"];
   NumericVector gl0 = in1["gl0"];
   NumericVector gl1 = in1["gl1"];
   NumericVector gl0step = in1["gl0step"];
   //Rcpp::Rcout << "gl0step" << gl0step(1) << std::endl;
   NumericVector g00step = in1["g00step"];
   NumericVector g01m = in1["g01m"];
   NumericVector g01s = in1["g01s"];
   NumericVector gl1s = in1["gl1sigma"];

   double g00prior = as<double>(in1["g00prior"]);
   double D0caplike = as<double>(in1["D0caplike"]);
   double gl0prior = as<double>(in1["gl0prior"]);
   NumericVector b00step = in1["b00step"];
   NumericVector b01m = in1["b01m"];
   NumericVector b01s = in1["b01s"];
   NumericVector bl0step = in1["bl0step"];
   NumericVector bl1m = in1["bl1m"];
   NumericVector bl1s = in1["bl1s"];

   double b00prior = as<double>(in1["b00prior"]);
   double bl0prior = as<double>(in1["bl0prior"]);
   double S0Mortlike = as<double>(in1["S0Mortlike"]);
   //double totalpost0 = as<double>(in1["totalpost0"]);

   NumericMatrix cc1 = in1["cc1"];
   NumericMatrix ccSij = in1["ccSij"];
   NumericMatrix ccLij = in1["ccLij"];
   NumericMatrix ccad1 = in1["ccad1"];
   NumericVector cctime1 = in1["cctime1"];
   NumericMatrix ci1 = in1["ci1"];
   NumericMatrix ciSij = in1["ciSij"];
   NumericMatrix ciLij = in1["ciLij"];
   NumericMatrix ciad1 = in1["ciad1"];
   NumericVector citime1 = in1["citime1"];
   NumericMatrix lc1 = in1["lc1"];
   NumericMatrix lcSij = in1["lcSij"];
   NumericMatrix lcLij = in1["lcLij"];
   NumericMatrix lcad1 = in1["lcad1"];
   NumericVector lctime1 = in1["lctime1"];
   NumericMatrix li1 = in1["li1"];
   NumericMatrix liSij = in1["liSij"];
   NumericMatrix liLij = in1["liLij"];
   NumericMatrix liad1 = in1["liad1"];
   NumericVector litime1 = in1["litime1"];
   NumericMatrix tc1 = in1["tc1"];
   NumericMatrix tcSij = in1["tcSij"];
   NumericMatrix tcLij = in1["tcLij"];
   NumericMatrix tcad1 = in1["tcad1"];
   NumericVector tctime1 = in1["tctime1"];
   NumericMatrix ti1 = in1["ti1"];
   NumericMatrix tiSij = in1["tiSij"];
   NumericMatrix tiLij = in1["tiLij"];
   NumericMatrix tiad1 = in1["tiad1"];
   NumericVector titime1 = in1["titime1"];
   NumericMatrix uc1 = in1["uc1"];
   NumericMatrix ucSij = in1["ucSij"];
   NumericMatrix ucLij = in1["ucLij"];
   NumericMatrix ucad1 = in1["ucad1"];
   NumericVector uctime1 = in1["uctime1"];
   NumericMatrix ui1 = in1["ui1"];
   NumericMatrix uiSij = in1["uiSij"];
   NumericMatrix uiLij = in1["uiLij"];
   NumericMatrix uiad1 = in1["uiad1"];
   NumericVector uitime1 = in1["uitime1"];
   NumericVector cct0i1 = in1["cct0i1"];
   NumericMatrix ccDij = in1["ccDij"];
   NumericVector cit0i1 = in1["cit0i1"];
   NumericMatrix ciDij = in1["ciDij"];
   NumericVector lct0i1 = in1["lct0i1"];
   NumericMatrix lcDij = in1["lcDij"];
   NumericVector lit0i1 = in1["lit0i1"];
   NumericMatrix liDij = in1["liDij"];
   NumericVector tct0i1 = in1["tct0i1"];
   NumericMatrix tcDij = in1["tcDij"];
   NumericVector tit0i1 = in1["tit0i1"];
   NumericMatrix tiDij = in1["tiDij"];
   NumericVector uct0i1 = in1["uct0i1"];
   NumericMatrix ucDij = in1["ucDij"];
   NumericVector uit0i1 = in1["uit0i1"];
   NumericMatrix uiDij = in1["uiDij"];

   ///  Added stuff here.
   NumericVector g01priorm = in1["g01priorm"];
   NumericVector g01priors = in1["g01priors"];
   NumericVector gl1priorm = in1["gl1priorm"];
   NumericVector gl1priors = in1["gl1priors"];
   NumericVector b01priorm = in1["b01priorm"];
   NumericVector b01priors = in1["b01priors"];
   NumericVector bl1priorm = in1["bl1priorm"];
   NumericVector bl1priors = in1["bl1priors"];
   NumericVector g00hvc(4);
   NumericVector g00hvi(4);
   NumericVector gl0hvc(4);
   NumericVector gl0hvi(4);
   NumericVector b00hvc(4);
   NumericVector b00hvi(4);
   NumericVector bl0hvc(4);
   NumericVector bl0hvi(4);
   double g01prior = as<double>(in1["g01prior"]);
   double lambda1 = as<double>(in1["lambda1"]);
   int df1 = as<int>(in1["df1"]);
   SEXP DetectMeancc, DetectMeanci, DetectMeanlc, DetectMeanli;
   SEXP DetectMeantc, DetectMeanti, DetectMeanuc, DetectMeanui;
   NumericVector DetectNcc = in1["DetectNcc"];
   NumericVector DetectNci = in1["DetectNci"];
   NumericVector DetectNlc = in1["DetectNlc"];
   NumericVector DetectNli = in1["DetectNli"];
   NumericVector DetectNtc = in1["DetectNtc"];
   NumericVector DetectNti = in1["DetectNti"];
   NumericVector DetectNuc = in1["DetectNuc"];
   NumericVector DetectNui = in1["DetectNui"];
   NumericVector PopEstcc, PopEstci, PopEstlc, PopEstli, PopEsttc, PopEstti, PopEstuc, PopEstui;
      int Ncc=DetectNcc.size(), Nci=DetectNci.size(),Nlc=DetectNlc.size(),Nli=DetectNli.size(),Ntc=DetectNtc.size(),Nti=DetectNti.size(),Nuc=DetectNuc.size(),Nui=DetectNui.size();
     NumericMatrix PopEstOutcc(nsim1,Ncc), PopEstOutci(nsim1,Nci), PopEstOutlc(nsim1,Nlc), PopEstOutli(nsim1,Nli), PopEstOuttc(nsim1,Ntc), PopEstOutti(nsim1,Nti), PopEstOutuc(nsim1,Nuc), PopEstOutui(nsim1,Nui);

   int iter1;

     const NumericVector k1 = in1["k1"];
   // NumericVector k1step = in1["k1step"];

   // NumericVector k1a1 = in1["k1a1"];
   // NumericVector k1b1 = in1["k1b1"];

   ///  Stuff for the lognormal k.
   //Rcout <<"k1 stuff" << std::endl;
   // const NumericVector k1m1 = in1["k1m1"];
   // const NumericVector k1s1 = in1["k1s1"];
   // NumericVector k1priorm = in1["k1priorm"];
   //  NumericVector k1priors = in1["k1priors"];
   //Rcout << "Post k1 stuff" << std::endl;

  // double L01 = as<double>(in1["L01"]);
   NumericVector Linf1 = in1["Linf1"];
   //double k0prior = as<double>(in1["k0prior"]);
   double L0Like1 = as<double>(in1["L0Like1"]);

   NumericVector sigma1 = in1["sigma1"];

   NumericVector cct0i2 = in1["cct0i2"];
   NumericVector cit0i2 = in1["cit0i2"];
   NumericVector lct0i2 = in1["lct0i2"];
   NumericVector lit0i2 = in1["lit0i2"];
   NumericVector tct0i2 = in1["tct0i2"];
   NumericVector tit0i2 = in1["tit0i2"];
   NumericVector uct0i2 = in1["uct0i2"];
   NumericVector uit0i2 = in1["uit0i2"];

   double sigma1prior = as<double>(in1["sigma1prior"]);

   ///  Create some containers for the output.
   NumericMatrix csi1out(nsim1, 8);
   NumericMatrix b00out(nsim1, 8);
   NumericMatrix bl0out(nsim1, 8);
   NumericMatrix g00out(nsim1, 8);
   NumericMatrix gl0out(nsim1, 8);
   //  NumericMatrix k1out(nsim1, 8);
   NumericMatrix sigma1out(nsim1, 8);
   //NumericMatrix k1b1out(nsim1, 4);
   NumericMatrix gl1mout(nsim1, 4);
   NumericMatrix g01mout(nsim1, 4);
   NumericMatrix bl1mout(nsim1, 4);
   NumericMatrix b01mout(nsim1, 4);
   //Rcout<< "Start Fun" << std::endl;

   for(iter1=0;iter1<nsim1;iter1++){

   /// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
   /// Sample the Birth Parameters    csi parameters
   /// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
   Rcpp::List birthcc = birthSample1(csi1(0), csi1step(0), wrap(ccab1), csibeta1, csibeta2, C0birthlike, csiprior);
   csi1(0) = as<double>(birthcc["csi1"]);
   csiprior = as<double>(birthcc["csiprior"]);
   C0birthlike = as<double>(birthcc["C0birthlike"]);
   Rcpp::List birthci = birthSample1(csi1(1), csi1step(1), wrap(ciab1), csibeta1, csibeta2, C0birthlike, csiprior);
   csi1(1) = as<double>(birthci["csi1"]);
   csiprior = as<double>(birthci["csiprior"]);
   C0birthlike = as<double>(birthci["C0birthlike"]);
   Rcpp::List birthlc = birthSample1(csi1(2), csi1step(2), wrap(lcab1), csibeta1, csibeta2, C0birthlike, csiprior);
   csi1(2) = as<double>(birthlc["csi1"]);
   csiprior = as<double>(birthlc["csiprior"]);
   C0birthlike = as<double>(birthlc["C0birthlike"]);
   Rcpp::List birthli = birthSample1(csi1(3), csi1step(3), wrap(liab1), csibeta1, csibeta2, C0birthlike, csiprior);
   csi1(3) = as<double>(birthli["csi1"]);
   csiprior = as<double>(birthli["csiprior"]);
   C0birthlike = as<double>(birthli["C0birthlike"]);
   Rcpp::List birthtc = birthSample1(csi1(4), csi1step(4), wrap(tcab1), csibeta1, csibeta2, C0birthlike, csiprior);
   csi1(4) = as<double>(birthtc["csi1"]);
   csiprior = as<double>(birthtc["csiprior"]);
   C0birthlike = as<double>(birthtc["C0birthlike"]);
   Rcpp::List birthti = birthSample1(csi1(5), csi1step(5), wrap(tiab1), csibeta1, csibeta2, C0birthlike, csiprior);
   csi1(5) = as<double>(birthti["csi1"]);
   csiprior = as<double>(birthti["csiprior"]);
   C0birthlike = as<double>(birthti["C0birthlike"]);
   Rcpp::List birthuc = birthSample1(csi1(6), csi1step(6), wrap(ucab1), csibeta1, csibeta2, C0birthlike, csiprior);
   csi1(6) = as<double>(birthuc["csi1"]);
   csiprior = as<double>(birthuc["csiprior"]);
   C0birthlike = as<double>(birthuc["C0birthlike"]);
   Rcpp::List birthui = birthSample1(csi1(7), csi1step(7), wrap(uiab1), csibeta1, csibeta2, C0birthlike, csiprior);
   csi1(7) = as<double>(birthui["csi1"]);
   csiprior = as<double>(birthui["csiprior"]);
   C0birthlike = as<double>(birthui["C0birthlike"]);

   /// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
   /// Sample the mortality parameters (intercept) Parameters    b00  parameters
   /// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
   /// b00Sample1
   Rcpp::List b00cc = b00Sample1(b00(0), bl0(0), b00step(0), b01m(0), b01s(0),
                                 cc1, ccSij, ccLij, ccad1, ccab1, cctime1, S0Mortlike, b00prior);
   b00(0) = as<double>(b00cc["b0"]);
   b00prior = as<double>(b00cc["b0prior"]);
   S0Mortlike = as<double>(b00cc["S0MortLike"]);
   ccSij = as<NumericMatrix>(b00cc["Sij0"]);
   ///
   Rcpp::List b00ci = b00Sample1(b00(1), bl0(1), b00step(1), b01m(1), b01s(1),
                                 ci1, ciSij, ciLij, ciad1, ciab1, citime1, S0Mortlike, b00prior);
   b00(1) = as<double>(b00ci["b0"]);
   b00prior = as<double>(b00ci["b0prior"]);
   S0Mortlike = as<double>(b00ci["S0MortLike"]);
   ciSij = as<NumericMatrix>(b00ci["Sij0"]);
   ///
   Rcpp::List b00lc = b00Sample1(b00(2), bl0(2), b00step(2), b01m(0), b01s(0),
                                 lc1, lcSij, lcLij, lcad1, lcab1, lctime1, S0Mortlike, b00prior);
   b00(2) = as<double>(b00lc["b0"]);
   b00prior = as<double>(b00lc["b0prior"]);
   S0Mortlike = as<double>(b00lc["S0MortLike"]);
   lcSij = as<NumericMatrix>(b00lc["Sij0"]);
   ///
   Rcpp::List b00li = b00Sample1(b00(3), bl0(3), b00step(3), b01m(1), b01s(1),
                                 li1, liSij, liLij, liad1, liab1, litime1, S0Mortlike, b00prior);
   b00(3) = as<double>(b00li["b0"]);
   b00prior = as<double>(b00li["b0prior"]);
   S0Mortlike = as<double>(b00li["S0MortLike"]);
   liSij = as<NumericMatrix>(b00li["Sij0"]);
   ///
   Rcpp::List b00tc = b00Sample1(b00(4), bl0(4), b00step(4), b01m(0), b01s(0),
                                 tc1, tcSij, tcLij, tcad1, tcab1, tctime1, S0Mortlike, b00prior);
   b00(4) = as<double>(b00tc["b0"]);
   b00prior = as<double>(b00tc["b0prior"]);
   S0Mortlike = as<double>(b00tc["S0MortLike"]);
   tcSij = as<NumericMatrix>(b00tc["Sij0"]);
   ///
   Rcpp::List b00ti = b00Sample1(b00(5), bl0(5), b00step(5), b01m(1), b01s(1),
                                 ti1, tiSij, tiLij, tiad1, tiab1, titime1, S0Mortlike, b00prior);
   b00(5) = as<double>(b00ti["b0"]);
   b00prior = as<double>(b00ti["b0prior"]);
   S0Mortlike = as<double>(b00ti["S0MortLike"]);
   tiSij = as<NumericMatrix>(b00ti["Sij0"]);
   ///
   Rcpp::List b00uc = b00Sample1(b00(6), bl0(6), b00step(6), b01m(0), b01s(0),
                                 uc1, ucSij, ucLij, ucad1, ucab1, uctime1, S0Mortlike, b00prior);
   b00(6) = as<double>(b00uc["b0"]);
   b00prior = as<double>(b00uc["b0prior"]);
   S0Mortlike = as<double>(b00uc["S0MortLike"]);
   ucSij = as<NumericMatrix>(b00uc["Sij0"]);
   ///
   Rcpp::List b00ui = b00Sample1(b00(5), bl0(5), b00step(5), b01m(1), b01s(1),
                                 ui1, uiSij, uiLij, uiad1, uiab1, uitime1, S0Mortlike, b00prior);
   b00(7) = as<double>(b00ui["b0"]);
   b00prior = as<double>(b00ui["b0prior"]);
   S0Mortlike = as<double>(b00ui["S0MortLike"]);
   uiSij = as<NumericMatrix>(b00ui["Sij0"]);
   ///
   ///
   /// //////////////////////////////////////////////////////////////////////////////////////////////////////////////
   /// Generate a sample from the second level of the hierarchy for the b00 parameters
   /// //////////////////////////////////////////////////////////////////////////////////////////////////////////////
   ///
   /// Control group

   b00hvc(0) = as<double>(b00cc["b0"]);;
   b00hvc(1) = as<double>(b00lc["b0"]);
   b00hvc(2) = as<double>(b00tc["b0"]);
   b00hvc(3) = as<double>(b00uc["b0"]);
   Rcpp::List b01hsc = b1hierSample2(b01m(0), b01s(0),  b00hvc, b01priorm(0), b01priors(0),
				     b00prior, lambda1, df1);
   b01m(0) = as<double>(b01hsc["b1"]);
   b01s(0) = as<double>(b01hsc["b1sigma"]);
   b00prior = as<double>(b01hsc["b1Like"]);
   ///
   /// Intervention group
   b00hvi(0) = as<double>(b00ci["b0"]);;
   b00hvi(1) = as<double>(b00li["b0"]);
   b00hvi(2) = as<double>(b00ti["b0"]);
   b00hvi(3) = as<double>(b00ui["b0"]);
   Rcpp::List b01hsi = b1hierSample2(b01m(1), b01s(1),  b00hvi, b01priorm(1), b01priors(1),
				     b00prior, lambda1, df1);
   b01m(1) = as<double>(b01hsi["b1"]);
   b01s(1) = as<double>(b01hsi["b1sigma"]);
   b00prior = as<double>(b01hsi["b1Like"]);

   /// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
   /// Sample the mortality parameters (slope) Parameters    bl0  parameters
   /// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
   /// bl0Sample1
   //Rcout << "bl0 parameters" <<std::endl;
   Rcpp::List bl0cc = bl0Sample1(b00(0), bl0(0), bl0step(0), bl1m(0), bl1s(0),
				 cc1, ccSij, ccLij, ccad1, ccab1, cctime1, S0Mortlike, bl0prior);
   bl0(0) = as<double>(bl0cc["b0"]);
   bl0prior = as<double>(bl0cc["b0prior"]);
   S0Mortlike = as<double>(bl0cc["S0MortLike"]);
   ccSij = as<NumericMatrix>(bl0cc["Sij0"]);
   ///
   //Rcout << "bl0 parameters 0" <<std::endl;
   Rcpp::List bl0ci = bl0Sample1(b00(1), bl0(1), bl0step(1), bl1m(1), bl1s(1),
				 ci1, ciSij, ciLij, ciad1, ciab1, citime1, S0Mortlike, bl0prior);
   bl0(1) = as<double>(bl0ci["b0"]);
   bl0prior = as<double>(bl0ci["b0prior"]);
   S0Mortlike = as<double>(bl0ci["S0MortLike"]);
   ciSij = as<NumericMatrix>(bl0ci["Sij0"]);
   ///
   //Rcout << "bl0 parameters 1" <<std::endl;
   Rcpp::List bl0lc = bl0Sample1(b00(2), bl0(2), bl0step(2), bl1m(0), bl1s(0),
				 lc1, lcSij, lcLij, lcad1, lcab1, lctime1, S0Mortlike, bl0prior);
   bl0(2) = as<double>(bl0lc["b0"]);
   bl0prior = as<double>(bl0lc["b0prior"]);
   S0Mortlike = as<double>(bl0lc["S0MortLike"]);
   lcSij = as<NumericMatrix>(bl0lc["Sij0"]);
   ///
   //Rcout << "bl0 parameters 2" <<std::endl;
   Rcpp::List bl0li = bl0Sample1(b00(3), bl0(3), bl0step(3), bl1m(1), bl1s(1),
				 li1, liSij, liLij, liad1, liab1, litime1, S0Mortlike, bl0prior);
   bl0(3) = as<double>(bl0li["b0"]);
   bl0prior = as<double>(bl0li["b0prior"]);
   S0Mortlike = as<double>(bl0li["S0MortLike"]);
   liSij = as<NumericMatrix>(bl0li["Sij0"]);
   ///
   //Rcout << "bl0 parameters 3" <<std::endl;
   Rcpp::List bl0tc = bl0Sample1(b00(4), bl0(4), bl0step(4), bl1m(0), bl1s(0),
				 tc1, tcSij, tcLij, tcad1, tcab1, tctime1, S0Mortlike, bl0prior);
   bl0(4) = as<double>(bl0tc["b0"]);
   bl0prior = as<double>(bl0tc["b0prior"]);
   S0Mortlike = as<double>(bl0tc["S0MortLike"]);
   tcSij = as<NumericMatrix>(bl0tc["Sij0"]);
   ///
   //Rcout << "bl0 parameters 4" <<std::endl;
   Rcpp::List bl0ti = bl0Sample1(b00(5), bl0(5), bl0step(5), bl1m(1), bl1s(1),
				 ti1, tiSij, tiLij, tiad1, tiab1, titime1, S0Mortlike, bl0prior);
   bl0(5) = as<double>(bl0ti["b0"]);
   bl0prior = as<double>(bl0ti["b0prior"]);
   S0Mortlike = as<double>(bl0ti["S0MortLike"]);
   tiSij = as<NumericMatrix>(bl0ti["Sij0"]);
   ///
   //Rcout << "bl0 parameters 5" <<std::endl;
   Rcpp::List bl0uc = bl0Sample1(b00(6), bl0(6), bl0step(6), bl1m(0), bl1s(0),
				 uc1, ucSij, ucLij, ucad1, ucab1, uctime1, S0Mortlike, bl0prior);
   bl0(6) = as<double>(bl0uc["b0"]);
   bl0prior = as<double>(bl0uc["b0prior"]);
   S0Mortlike = as<double>(bl0uc["S0MortLike"]);
   ucSij = as<NumericMatrix>(bl0uc["Sij0"]);
   ///
   //Rcout << "bl0 parameters 6" <<std::endl;
   Rcpp::List bl0ui = bl0Sample1(b00(7), bl0(7), bl0step(7), bl1m(1), bl1s(1),
				 ui1, uiSij, uiLij, uiad1, uiab1, uitime1, S0Mortlike, bl0prior);
   bl0(7) = as<double>(bl0ui["b0"]);
   bl0prior = as<double>(bl0ui["b0prior"]);
   S0Mortlike = as<double>(bl0ui["S0MortLike"]);
   uiSij = as<NumericMatrix>(bl0ui["Sij0"]);
   ///
   //Rcout << "bl0 parameters 7" <<std::endl;
   ///
   /// //////////////////////////////////////////////////////////////////////////////////////////////////////////////
   /// Generate a sample from the second level of the hierarchy for the b00 parameters
   /// //////////////////////////////////////////////////////////////////////////////////////////////////////////////
   ///
   /// Control group
   //Rcout << "bl0 parameter means" <<std::endl;
   bl0hvc(0) = as<double>(bl0cc["b0"]);;
   bl0hvc(1) = as<double>(bl0lc["b0"]);
   bl0hvc(2) = as<double>(bl0tc["b0"]);
   bl0hvc(3) = as<double>(bl0uc["b0"]);
   Rcpp::List bl1hsc = b1hierSample2(bl1m(0), bl1s(0),  bl0hvc, bl1priorm(0), bl1priors(0),
				     bl0prior, lambda1, df1);
   bl1m(0) = as<double>(bl1hsc["b1"]);
   bl1s(0) = as<double>(bl1hsc["b1sigma"]);
   bl0prior = as<double>(bl1hsc["b1Like"]);
   ///
   /// Intervention group
   bl0hvi(0) = as<double>(bl0ci["b0"]);;
   bl0hvi(1) = as<double>(bl0li["b0"]);
   bl0hvi(2) = as<double>(bl0ti["b0"]);
   bl0hvi(3) = as<double>(bl0ui["b0"]);
   Rcpp::List bl1hsi = b1hierSample2(bl1m(1), bl1s(1),  bl0hvi, bl1priorm(1), bl1priors(1),
				     bl0prior, lambda1, df1);
   bl1m(1) = as<double>(bl1hsi["b1"]);
   bl1s(1) = as<double>(bl1hsi["b1sigma"]);
   bl0prior = as<double>(bl1hsi["b1Like"]);

   ///
   /// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
   /// Sample the detection parameters (intercept) Parameters    g00  parameters
   /// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
   /// g00Sample1
   //Rcout << "g00 parameters" <<std::endl;
   Rcpp::List g00cc = g00Sample1(g00(0), g00step(0), gl0(0), g01m(0), g01s(0), g00prior,
                                cc1, ccLij, ccDij, cct0i1, ccad1, ccab1, D0caplike);
   g00(0) = as<double>(g00cc["g00"]);
   g00prior = as<double>(g00cc["g00prior"]);
   D0caplike = as<double>(g00cc["D0caplike"]);
   ccDij = as<NumericMatrix>(g00cc["Dij0"]);
   ///
   Rcpp::List g00ci = g00Sample1(g00(1), g00step(1), gl0(1), g01m(1), g01s(1), g00prior,
                                ci1, ciLij, ciDij, cit0i1, ciad1, ciab1, D0caplike);
   g00(1) = as<double>(g00ci["g00"]);
   g00prior = as<double>(g00ci["g00prior"]);
   D0caplike = as<double>(g00ci["D0caplike"]);
   ciDij = as<NumericMatrix>(g00ci["Dij0"]);
   ///
   Rcpp::List g00lc = g00Sample1(g00(2), g00step(2), gl0(2), g01m(0), g01s(0), g00prior,
                                lc1, lcLij, lcDij, lct0i1, lcad1, lcab1, D0caplike);
   g00(2) = as<double>(g00lc["g00"]);
   g00prior = as<double>(g00lc["g00prior"]);
   D0caplike = as<double>(g00lc["D0caplike"]);
   lcDij = as<NumericMatrix>(g00lc["Dij0"]);
   ///
   Rcpp::List g00li = g00Sample1(g00(3), g00step(3), gl0(3), g01m(1), g01s(1), g00prior,
                                li1, liLij, liDij, lit0i1, liad1, liab1, D0caplike);
   g00(3) = as<double>(g00li["g00"]);
   g00prior = as<double>(g00li["g00prior"]);
   D0caplike = as<double>(g00li["D0caplike"]);
   liDij = as<NumericMatrix>(g00li["Dij0"]);
   ///
   Rcpp::List g00tc = g00Sample1(g00(4), g00step(4), gl0(4), g01m(0), g01s(0), g00prior,
                                tc1, tcLij, tcDij, tct0i1, tcad1, tcab1, D0caplike);
   g00(4) = as<double>(g00tc["g00"]);
   g00prior = as<double>(g00tc["g00prior"]);
   D0caplike = as<double>(g00tc["D0caplike"]);
   tcDij = as<NumericMatrix>(g00tc["Dij0"]);
   ///
   Rcpp::List g00ti = g00Sample1(g00(5), g00step(5), gl0(5), g01m(1), g01s(1), g00prior,
                                ti1, tiLij, tiDij, tit0i1, tiad1, tiab1, D0caplike);
   g00(5) = as<double>(g00ti["g00"]);
   g00prior = as<double>(g00ti["g00prior"]);
   D0caplike = as<double>(g00ti["D0caplike"]);
   tiDij = as<NumericMatrix>(g00ti["Dij0"]);
   ///
   Rcpp::List g00uc = g00Sample1(g00(6), g00step(6), gl0(6), g01m(0), g01s(0), g00prior,
                                uc1, ucLij, ucDij, uct0i1, ucad1, ucab1, D0caplike);
   g00(6) = as<double>(g00uc["g00"]);
   g00prior = as<double>(g00uc["g00prior"]);
   D0caplike = as<double>(g00uc["D0caplike"]);
   ucDij = as<NumericMatrix>(g00uc["Dij0"]);
   ///
   Rcpp::List g00ui = g00Sample1(g00(7), g00step(7), gl0(7), g01m(1), g01s(1), g00prior,
                                ui1, uiLij, uiDij, uit0i1, uiad1, uiab1, D0caplike);
   g00(7) = as<double>(g00ui["g00"]);
   g00prior = as<double>(g00ui["g00prior"]);
   D0caplike = as<double>(g00ui["D0caplike"]);
   uiDij = as<NumericMatrix>(g00ui["Dij0"]);
   ///
   /// //////////////////////////////////////////////////////////////////////////////////////////////////////////////
   /// Generate a sample from the second level of the hierarchy for the g00 parameters
   /// //////////////////////////////////////////////////////////////////////////////////////////////////////////////
   ///
   /// Control group
   //Rcout << "g00 parameter means" <<std::endl;
   g00hvc(0) = as<double>(g00cc["g00"]);;
   g00hvc(1) = as<double>(g00lc["g00"]);
   g00hvc(2) = as<double>(g00tc["g00"]);
   g00hvc(3) = as<double>(g00uc["g00"]);
   Rcpp::List g01hsc = g01hierSample1(g01m(0), g01s(0),  g00hvc, g01priorm(0), g01priors(0),
				     g01prior, lambda1, df1);
   g01m(0) = as<double>(g01hsc["g01"]);
   g01s(0) = as<double>(g01hsc["g01sigma"]);
   g01prior = as<double>(g01hsc["g01Like"]);
   ///
   /// Intervention group
   g00hvi(0) = as<double>(g00ci["g00"]);;
   g00hvi(1) = as<double>(g00li["g00"]);
   g00hvi(2) = as<double>(g00ti["g00"]);
   g00hvi(3) = as<double>(g00ui["g00"]);
   //Rcpp::Rcout << "In loop: g01m : " << g01m(1) << " g01s : " << gl1s(1) <<" g01priors(1): " << g01priors(1)  <<" g01priorm(1): " << g01priorm(1) <<std::endl;
   Rcpp::List g01hsi = g01hierSample1(g01m(1), g01s(1),  g00hvi, g01priorm(1), g01priors(1),
				     g01prior, lambda1, df1);
   g01m(1) = as<double>(g01hsi["g01"]);
   g01s(1) = as<double>(g01hsi["g01sigma"]);
   g01prior = as<double>(g01hsi["g01Like"]);
   /// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
   /// Sample the detection parameters (slope) Parameters    gl0  parameters
   /// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
   /// gl0Sample1
   //Rcout << "gl0 parameters" <<std::endl;
   Rcpp::List gl0cc = gl0Sample1(gl0(0), gl0step(0), g00(0), gl1(0), gl1s(0), gl0prior,
				 cc1, ccLij, ccDij, cct0i1, ccad1, ccab1, D0caplike);
   gl0(0) = as<double>(gl0cc["gl0"]);
   gl0prior = as<double>(gl0cc["g00prior"]);
   D0caplike = as<double>(gl0cc["D0caplike"]);
   ccDij = as<NumericMatrix>(gl0cc["Dij0"]);
   ///
   Rcpp::List gl0ci = gl0Sample1(gl0(1), gl0step(1), g00(1), gl1(1), gl1s(1), gl0prior,
				 ci1, ciLij, ciDij, cit0i1, ciad1, ciab1, D0caplike);
   gl0(1) = as<double>(gl0ci["gl0"]);
   gl0prior = as<double>(gl0ci["g00prior"]);
   D0caplike = as<double>(gl0ci["D0caplike"]);
   ciDij = as<NumericMatrix>(gl0ci["Dij0"]);
   ///
   Rcpp::List gl0lc = gl0Sample1(gl0(2), gl0step(2), g00(2), gl1(0), gl1s(0), gl0prior,
				 lc1, lcLij, lcDij, lct0i1, lcad1, lcab1, D0caplike);
   gl0(2) = as<double>(gl0lc["gl0"]);
   gl0prior = as<double>(gl0lc["g00prior"]);
   D0caplike = as<double>(gl0lc["D0caplike"]);
   lcDij = as<NumericMatrix>(gl0lc["Dij0"]);
   ///
   Rcpp::List gl0li = gl0Sample1(gl0(3), gl0step(3), g00(3), gl1(1), gl1s(1), gl0prior,
				 li1, liLij, liDij, lit0i1, liad1, liab1, D0caplike);
   gl0(3) = as<double>(gl0li["gl0"]);
   gl0prior = as<double>(gl0li["g00prior"]);
   D0caplike = as<double>(gl0li["D0caplike"]);
   liDij = as<NumericMatrix>(gl0li["Dij0"]);
   ///
   Rcpp::List gl0tc = gl0Sample1(gl0(4), gl0step(4), g00(4), gl1(0), gl1s(0), gl0prior,
				 tc1, tcLij, tcDij, tct0i1, tcad1, tcab1, D0caplike);
   gl0(4) = as<double>(gl0tc["gl0"]);
   gl0prior = as<double>(gl0tc["g00prior"]);
   D0caplike = as<double>(gl0tc["D0caplike"]);
   tcDij = as<NumericMatrix>(gl0tc["Dij0"]);
   ///
   Rcpp::List gl0ti = gl0Sample1(gl0(5), gl0step(5), g00(5), gl1(1), gl1s(1), gl0prior,
				 ti1, tiLij, tiDij, tit0i1, tiad1, tiab1, D0caplike);
   gl0(5) = as<double>(gl0ti["gl0"]);
   gl0prior = as<double>(gl0ti["g00prior"]);
   D0caplike = as<double>(gl0ti["D0caplike"]);
   tiDij = as<NumericMatrix>(gl0ti["Dij0"]);
   ///
   Rcpp::List gl0uc = gl0Sample1(gl0(6), gl0step(6), g00(6), gl1(0), gl1s(0), gl0prior,
				 uc1, ucLij, ucDij, uct0i1, ucad1, ucab1, D0caplike);
   gl0(6) = as<double>(gl0uc["gl0"]);
   gl0prior = as<double>(gl0uc["g00prior"]);
   D0caplike = as<double>(gl0uc["D0caplike"]);
   ucDij = as<NumericMatrix>(gl0uc["Dij0"]);
   ///

   Rcpp::List gl0ui = gl0Sample1(gl0(7), gl0step(7), g00(7), gl1(1), gl1s(1), gl0prior,
				 ui1, uiLij, uiDij, uit0i1, uiad1, uiab1, D0caplike);
   gl0(7) = as<double>(gl0ui["gl0"]);
   gl0prior = as<double>(gl0ui["g00prior"]);
   D0caplike = as<double>(gl0ui["D0caplike"]);
   uiDij = as<NumericMatrix>(gl0ui["Dij0"]);
   ///
   /// //////////////////////////////////////////////////////////////////////////////////////////////////////////////
   /// Generate a sample from the second level of the hierarchy for the g00 parameters
   /// //////////////////////////////////////////////////////////////////////////////////////////////////////////////
   ///
   //Rcout << "gl0 parameter means" <<std::endl;
   /// Control group
   gl0hvc(0) = as<double>(gl0cc["gl0"]);;
   gl0hvc(1) = as<double>(gl0lc["gl0"]);
   gl0hvc(2) = as<double>(gl0tc["gl0"]);
   gl0hvc(3) = as<double>(gl0uc["gl0"]);
   Rcpp::List gl1hsc = gl1hierSample1(gl1(0), g01s(0),  gl0hvc, gl1priorm(0), gl1priors(0),
				     g01prior, lambda1, df1);
   gl1(0) = as<double>(gl1hsc["gl1"]);
   gl1s(0) = as<double>(gl1hsc["gl1sigma"]);
   g01prior = as<double>(gl1hsc["gl1prior"]);
   //Rcpp::Rcout <<" In loop: g01prior for Control Group: " << g01prior <<std::endl;
   ///
   /// Intervention group
   gl0hvi(0) = as<double>(gl0ci["gl0"]);;
   gl0hvi(1) = as<double>(gl0li["gl0"]);
   gl0hvi(2) = as<double>(gl0ti["gl0"]);
   gl0hvi(3) = as<double>(gl0ui["gl0"]);
   //Rcpp::Rcout <<" In loop: g01prior before Intervention Group: " << g01prior <<std::endl;
   Rcpp::List gl1hsi = gl1hierSample1(gl1(1), gl1s(1),  gl0hvi, gl1priorm(1), gl1priors(1),
				     g01prior, lambda1, df1);
   //Rcpp::Rcout <<" In loop: g01prior for Control Group: " << g01prior <<std::endl;
   gl1(1) = as<double>(gl1hsi["gl1"]);
   gl1s(1) = as<double>(gl1hsi["gl1sigma"]);
   g01prior = as<double>(gl1hsi["gl1prior"]);

   Rcpp::List SS2cc = sigma1sample2(sigma1(0),
				    cct0i1, ccLij, cc1, lambda1, df1, L0Like1, sigma1prior);
   sigma1(0) = as<double>(SS2cc["sigma2"]);
   L0Like1 = as<double>(SS2cc["L0Like1"]);
   sigma1prior = as<double>(SS2cc["sigma1prior"]);
   ///

   Rcpp::List SS2ci = sigma1sample2(sigma1(1),
				    cit0i1, ciLij, ci1, lambda1, df1, L0Like1, sigma1prior);
   sigma1(1) = as<double>(SS2ci["sigma2"]);
   L0Like1 = as<double>(SS2ci["L0Like1"]);
   sigma1prior = as<double>(SS2ci["sigma1prior"]);
   ///

   Rcpp::List SS2lc = sigma1sample2(sigma1(2),
				    lct0i1, lcLij, lc1, lambda1, df1, L0Like1, sigma1prior);
   sigma1(2) = as<double>(SS2lc["sigma2"]);
   L0Like1 = as<double>(SS2lc["L0Like1"]);
   sigma1prior = as<double>(SS2lc["sigma1prior"]);
   ///
   Rcpp::List SS2li = sigma1sample2(sigma1(3),
				    lit0i1, liLij, li1, lambda1, df1, L0Like1, sigma1prior);
   sigma1(3) = as<double>(SS2li["sigma2"]);
   L0Like1 = as<double>(SS2li["L0Like1"]);
   sigma1prior = as<double>(SS2li["sigma1prior"]);
   ///
   Rcpp::List SS2tc = sigma1sample2(sigma1(4),
				    tct0i1, tcLij, tc1, lambda1, df1, L0Like1, sigma1prior);
   sigma1(4) = as<double>(SS2tc["sigma2"]);
   L0Like1 = as<double>(SS2tc["L0Like1"]);
   sigma1prior = as<double>(SS2tc["sigma1prior"]);
   ///
   Rcpp::List SS2ti = sigma1sample2(sigma1(5),
				    tit0i1, tiLij, ti1, lambda1, df1, L0Like1, sigma1prior);
   sigma1(5) = as<double>(SS2ti["sigma2"]);
   L0Like1 = as<double>(SS2ti["L0Like1"]);
   sigma1prior = as<double>(SS2ti["sigma1prior"]);
   ///
   Rcpp::List SS2uc = sigma1sample2(sigma1(6),
				    uct0i1, ucLij, uc1, lambda1, df1, L0Like1, sigma1prior);
   sigma1(6) = as<double>(SS2uc["sigma2"]);
   L0Like1 = as<double>(SS2uc["L0Like1"]);
   sigma1prior = as<double>(SS2uc["sigma1prior"]);
   ///
 // Rcpp::Rcout << "Almost Done Processing?" << std::endl;
   Rcpp::List SS2ui = sigma1sample2(sigma1(7),
				    uit0i1, uiLij, ui1, lambda1, df1, L0Like1, sigma1prior);
   sigma1(7) = as<double>(SS2ui["sigma2"]);
   L0Like1 = as<double>(SS2ui["L0Like1"]);
   sigma1prior = as<double>(SS2ui["sigma1prior"]);
   ///
   /// ///////////////////////////////////////////////////////////////////////////////////////////////////
   /// Determine the population estimates
   /// ///////////////////////////////////////////////////////////////////////////////////////////////////

DetectMeancc = MMean1(ccDij);
   DetectMeanci = MMean1(ciDij);
   DetectMeanlc = MMean1(lcDij);
   DetectMeanli = MMean1(liDij);
   DetectMeantc = MMean1(tcDij);
   DetectMeanti = MMean1(tiDij);
   DetectMeanuc = MMean1(ucDij);
   DetectMeanui = MMean1(ucDij);
   PopEstcc = VPopEst(DetectNcc, DetectMeancc);
   PopEstci = VPopEst(DetectNci, DetectMeanci);
   PopEstlc = VPopEst(DetectNlc, DetectMeanlc);
   PopEstli = VPopEst(DetectNli, DetectMeanli);
   PopEsttc = VPopEst(DetectNtc, DetectMeantc);
   PopEstti = VPopEst(DetectNti, DetectMeanti);
   PopEstuc = VPopEst(DetectNuc, DetectMeanuc);
   PopEstui = VPopEst(DetectNui, DetectMeanui);
   /// Store the values
   /// These are for the 8 valued matrices
   for(int j=0;j<8;j++){
      csi1out(iter1, j) = csi1(j);
      b00out(iter1, j) = b00(j);
      bl0out(iter1, j) = bl0(j);
      g00out(iter1, j) = g00(j);
      gl0out(iter1, j) = gl0(j);
      //   k1out(iter1, j) = k1(j);
      sigma1out(iter1, j) = sigma1(j);
   }
   /// These are for the 2 valued matrices
   for(int j=0;j<2;j++){
     //    k1b1out(iter1, j) = k1m1(j);
     //    k1b1out(iter1, j+2) = k1s1(j);
      gl1mout(iter1, j) = gl1(j);
      gl1mout(iter1, j+2) = gl1s(j);
      g01mout(iter1, j) = g01m(j);
      g01mout(iter1, j+2) = g01s(j);
      b01mout(iter1, j) = b01m(j);
      b01mout(iter1, j+2) = b01s(j);
      bl1mout(iter1, j) = bl1m(j);
      bl1mout(iter1, j+2) = bl1s(j);
   }
   for(int j=0;j<Ncc;j++){
      PopEstOutcc(iter1,j) = PopEstcc(j);
   }

    for(int j=0;j<Nci;j++){
      PopEstOutci(iter1,j) = PopEstci(j);
   }

  for(int j=0;j<Nlc;j++){
      PopEstOutlc(iter1,j) = PopEstlc(j);
   }
     for(int j=0;j<Nli;j++){
      PopEstOutli(iter1,j) = PopEstli(j);
   }

  for(int j=0;j<Ntc;j++){
      PopEstOuttc(iter1,j) = PopEsttc(j);
   }
  for(int j=0;j<Nti;j++){
      PopEstOutti(iter1,j) = PopEstti(j);
   }
  for(int j=0;j<Nuc;j++){
      PopEstOutuc(iter1,j) = PopEstuc(j);
   }
  for(int j=0;j<Nui;j++){
      PopEstOutui(iter1,j) = PopEstui(j);
   }
  //Rcpp::Rcout << " ---------------------------------------------------------------------" <<std::endl;
  //Rcpp::Rcout << "End of iteration: " << iter1+1 <<std::endl;
  //Rcpp::Rcout << " ---------------------------------------------------------------------" <<std::endl;
   } // End Sample Loop

   /// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
   /// Return the results
   /// ////////////////////////////////////////////////////////////////////////////////////////////////////////////
   ///
   return Rcpp::List::create(Rcpp::Named("csi1") = csi1out,
			     // Rcpp::Named("k1") = k1out,
			     Rcpp::Named("sigma1") = sigma1out,
                             Rcpp::Named("b00") = b00out,
			     Rcpp::Named("bl0") = bl0out,
			     Rcpp::Named("g00") = g00out,
			     Rcpp::Named("gl0") = gl0out,
			     Rcpp::Named("gl1m") = gl1mout,
                 Rcpp::Named("g01m") = g01mout,
		         Rcpp::Named("bl1m") = bl1mout,
		         Rcpp::Named("b01m") = b01mout
			    );
}   /// End function

int Nproposal(int Nold, int nobs){
  int obs = Nold -nobs;
  int Prop = nobs + as<int>(rpois(1,obs));
  return Prop;
}

double Nlogpropden(int Nold, int Nnew){
 IntegerVector out1;
 out1[0] = Nnew, out1[1]=Nold;

  double out = Rf_dpois(Nnew, Nold, 0);
  return out;
}

NumericMatrix DetectionProb(double g00, double gl0, NumericMatrix Length){
	Rcpp::Rcout << "In DetectionProb" << std::endl;
  int J = Length.ncol();
  int I = Length.nrow();
  NumericMatrix p(I,J);
	Rcpp::Rcout << "DetectionProb: Initialized" << std::endl;
  for(int i=0; i<I; i++){
	  for(int j=0; j<J; j++){
    p(i,j) = logistic1(g00 + gl0*Length(i,j));
  }}
  return p;
}

double Birthday(int time, NumericVector Days){
  double BD;
  if(time ==1){
	  double term = Days[1];
	  BD = runifd1( -0.5, term);}
  else {
	  double first = Days[time-1], second = Days[time];
	  BD = runifd1(first, second);
  }
  return BD;
}



NumericVector VonBertVec(double BD, NumericVector Times, double k, double L0, double Linf){
  int J = Times.size();
  NumericVector lengths(J);
  for(int i=0; i<J; i++){
    lengths(i) = VonBert(Times(i), BD, k, L0, Linf);
  }
  return lengths;
}

long int Factorial2(int N){
	long int out=1;
	for(int i=0; i<N; i++){
		out *= i+1;
	}
return out;
}

int NMH(int Nnew, int Nold, int nobs, NumericMatrix p, NumericMatrix survmat){
  // Need to add the proposal functions...
	Rcpp::Rcout << "NMH: Inside" << std::endl;
	Rcpp::Rcout << "NNew: " << Nnew <<" Nold: "<< Nold << std::endl;

	int J = abs(Nnew-Nold), width = p.ncol(), ratio = Factorial2(Nnew)*Factorial2(Nold-nobs)/Factorial2(Nnew-nobs)/Factorial2(Nold);
  double out = log(ratio);
	Rcpp::Rcout << "Width: "<< width  << std::endl;
	Rcpp::Rcout << "NMH: Initialize "  << std::endl;
    int i, k;
  if(Nnew > Nold){
		Rcpp::Rcout << "NMH: In if "  << std::endl;

    for(i=0; i<J; i++){
      for(k=0; k<width; k++){
	    out += survmat(i,k)*log(1.0-p(nobs + i,k));
      }}}
  else {
   for( i=0; i<J; i++){
	Rcpp::Rcout << "NMH: In else "  << std::endl;
      for(k=0; k< width; k++){
	out -= survmat(i,k)*log(1.0-p(nobs + i,k));
}}
  }
	Rcpp::Rcout << "NMH: After Loops" << std::endl;
  int mh;
  double ran = log(runifd1(0.0,1.0));
  if(ran<out){mh =1;}
  else {mh=0;}
  int N = mh*Nnew + (1-mh)*Nold;
  return N;
}

NumericVector CumulVec(NumericVector vec){
	 int I = vec.size();
	 NumericVector out(I);
	 for(int i=1; i<I;i++){
		 out(i)=out(i-1)+vec(i);
	 }
return out;
}


int rmult(NumericVector vec){
   int I = vec.size();
   int out;
   NumericVector vecout = CumulVec(vec);
   double ran = runifd1(0,1);
   for(int i=1; i<I;i++){
	  if(ran>vecout(i-1) && ran< vecout(i)){
		  out=i;
		  break;
	  }}
return out;
}

int RecruitMort (double csi, double b00, double b0l, double g00, double gl0, int Nold, NumericVector Times, int Nobserved, double k, double L0, double Linf){
  // First Initialize
	Rcpp::Rcout << "In RecruitMort" << std::endl;
  int Nprop = Nproposal(Nold, Nobserved);
  int Nmax = std::max(Nprop, Nold);
  int Nnew = Nmax - Nobserved;
  int J = Times.size(), outnew;
  IntegerVector Period(Nnew);
  NumericMatrix Dij(Nnew,J), Lij(Nnew,J);
  double tempBD;
  NumericVector tempLength;
	Rcpp::Rcout << "RecruitMort: Initialized" << std::endl;
  for(int i=0; i<Nnew; i++){
	  Period(i) = rmult(csi);

    // Calculate Birth Date
    tempBD = Birthday(Period(i), Times);

    // Calculate Lengths (Deterministic given birthday, set size at birth)
    // Generate lengths at each time step
    tempLength = VonBertVec(tempBD, Times, k, L0, Linf);

    for(int j=0; j<J; j++){
    Lij(i,j) = tempLength(j);
    }
  } // End For Loop
	Rcpp::Rcout << "RecruitMort: After Loop" << std::endl;
    SEXP morttemp = Surv4(b00, b0l, Dij, Lij);
    NumericMatrix morttemp2(morttemp);
    Rcpp::Rcout << "RecruitMort: After Surv4" << std::endl;
    NumericMatrix dettemp = DetectionProb(g00, gl0, Lij);
	Rcpp::Rcout << "RecruitMort: After DetectionProb" << std::endl;
	Rcpp::Rcout << "Nnew: "<<Nprop<< " Nold: " << Nold << std::endl;
    outnew = NMH(Nprop, Nold, Nobserved, dettemp, morttemp2);
	Rcpp::Rcout << "RecruitMort: After NMH" << std::endl;
  return outnew;
}

NumericVector vectorize(NumericMatrix matin, int i){
int J = matin.ncol();
NumericVector vecin;
for(int j=0; j<J; j++){
	vecin(j)= matin(i,j);
}
return vecin;
}

RcppExport SEXP NPopEst(SEXP _b00, SEXP _b0l, SEXP _g00, SEXP _g0l, SEXP _csi, SEXP _Nobserved, SEXP _Times){
	Rcpp::Rcout << "In NPopEst" << std::endl;
	NumericVector b00(_b00);
	Rcpp::Rcout << "b00" << std::endl;
	NumericVector b0l(_b0l);
	Rcpp::Rcout << "b0l" << std::endl;
	NumericVector g00(_g00);
	Rcpp::Rcout << "g00" << std::endl;
	NumericVector g0l(_g0l);
	Rcpp::Rcout << "gl0" << std::endl;

	NumericVector Times(_Times);
	Rcpp::Rcout << "First Line" << std::endl;
	NumericVector csi(_csi);
	Rcpp::Rcout << "Converted" << std::endl;
	int I = b00.size(), Nobserved= as<int>(_Nobserved);
    IntegerVector Nout(I);
    Nout(0) = Nobserved + 10;
    NumericVector temp2;
    double L0=30.0, Linf = 100.0, k=1/0.00254868;
	  Rcpp::Rcout << "Prior to loop" << std::endl;
	double test;
	  for(int i=1; i<I; i++){
		test = csi(i);
		Rcpp::Rcout << test << std::endl;
		Rcpp::Rcout << "csi class:"<< csi(1)<<" "<< Nout(i-1)<< std::endl;
        		Nout(i) = RecruitMort(csi(i), b00(i), b0l(i), g00(i), g0l(i), Nout(i-1), Times, Nobserved, k, L0, Linf);
					}
	return wrap(Nout);
}


// int main(){}
