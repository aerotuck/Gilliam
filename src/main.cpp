/*
 * main.cpp
 *
 *  Created on: Jul 18, 2012
 *      Author: matthewkrachey
 */


#ifdef INSIDE
#include <Rcpp.h>
#include <RInside.h>                    // for the embedded R via RInside
#include "rcpp_hello_world.h"
using namespace Rcpp;
using namespace std;
int main(int argc, char *argv[]) {
    RInside R(argc, argv);              // create an embedded R instance
    SEXP s = rcpp_hello_world();
    Language call("print",s);
    call.eval();
    return 0;
}
#endif
