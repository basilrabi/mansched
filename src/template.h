#include <Rcpp.h>

template <class T>
T combine ( T a, T b ) {
  std::size_t n = a.size() + b.size();
  T output = Rcpp::no_init( n );
  std::size_t index = 0;
  std::copy( a.begin(), a.end(), output.begin() + index );
  index += a.size();
  std::copy( b.begin(), b.end(), output.begin() + index );
  return output;
}

template <class T>
Rcpp::LogicalVector isElement ( T a, T b ) {
  R_xlen_t i = a.length();
  R_xlen_t j = b.length();
  Rcpp::LogicalVector out ( i, false );
  for ( R_xlen_t x = 0; x < i; x++ )
  {
    for (R_xlen_t y = 0; y < j; y++ )
    {
      if ( a[x] == b[y] )
      {
        out[x] = true;
        break;
      }
    }
  }
  return out;
}
