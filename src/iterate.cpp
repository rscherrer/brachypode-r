#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<double> iterate_cpp(

    const std::vector<double> &N0,
    const size_t &tend,
    const double &x,
    const List &pars

) {

  // Sanity checks
  if (N0.size() != 2u) throw std::runtime_error("N0 should be of length 2.");
  if (N0[0u] < 0.0 || N0[1u] < 0.0) throw std::runtime_error("N0 cannot have negative elements");

  std::vector<double> N(N0);

  // Unpack parameters
  const double rmax = pars["rmax"];
  const double epsilon = pars["epsilon"];
  const size_t K1 = pars["K1"];
  const size_t K2 = pars["K2"];
  const double a = pars["a"];
  const double theta1 = pars["theta1"];
  const double theta2 = pars["theta2"];
  const double c = pars["c"];

  // Check the parameters
  if (rmax < 0.0) throw std::runtime_error("rmax cannot be negative.");
  if (epsilon < 0.0) throw std::runtime_error("epsilon cannot be negative.");
  if (a < 0.0) throw std::runtime_error("a cannot be negative");
  if (theta1 < 0.0) throw std::runtime_error("theta1 cannot be negative");
  if (theta2 < 0.0) throw std::runtime_error("theta2 cannot be negative");
  if (c < 0.0 || c > 1.0) throw std::runtime_error("c should be between zero and one.");

  // Loop through time
  for (size_t t = 0u; t < tend; ++t) {

    const double s1 = 1.0 / (1.0 + exp(a * (theta1 - x)));
    const double s2 = 1.0 / (1.0 + exp(a * (theta2 - x)));
    const double y = rmax - epsilon * x;
    const double r1 = exp(y * (1.0 - N[0u] / (c * K1)));
    const double r2 = exp(y * (1.0 - N[1u] / ((1 - c) * K2)));

    const double N1 = s1 * c * (r1 * N[0u] + r2 * N[1u]);
    const double N2 = s2 * (1 - c) * (r1 * N[0u] + r2 * N[1u]);

    N[0u] = N1;
    N[1u] = N2;

  }

  return N;

}
