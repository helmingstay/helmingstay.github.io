#include <Rcpp.h>
using namespace Rcpp ;

// [[Rcpp::export]]
void appendRcpp(  List fillVecs, NumericVector newLengths, NumericMatrix retmat, NumericVector retmatLengths) {
    // "append" fill oldmat w/  
    // we will loop through rows, filling retmat in with the vectors in list
    // then update retmat_size to index the next free
    // newLenths isn't used, added for compatibility
    NumericVector fillTmp;
    int sizeOld, sizeAdd, sizeNew;
    // pull out dimensions of matrix to fill
    int nrow = retmat.nrow();
    int ncol = retmat.ncol();
    // check that dimensions match
    if ( nrow != retmatLengths.size() || nrow != fillVecs.size()) { 
        throw std::range_error("In appendC(): dimension mismatch");
    }
    for (int ii = 0; ii<nrow; ii++){
        // for each vector / row in retmat
        // vector to add to retmat
        fillTmp = fillVecs[ii];
        // compute lengths
        sizeOld = retmatLengths[ii];
        sizeAdd = fillTmp.size();
        sizeNew = sizeOld + sizeAdd;
        // error checking - stop on overfill
        if ( sizeNew >= ncol) {
            throw std::range_error("In appendC(): exceeded max cols");
        }
        // iterator for row to fill
        NumericMatrix::Row retRow = retmat(ii, _);
        // fill row of return matrix, starting at first non-zero elem
        std::copy( fillTmp.begin(), fillTmp.end(), retRow.begin() + sizeOld);
        // update size of retmat
        retmatLengths[ii] = sizeNew;
    }
}
