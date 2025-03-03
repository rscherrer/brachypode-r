#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<double> iterate_di_cpp(

  const std::vector<double> &N0,
  const size_t &tend,
  const std::vector<double> &x,
  const List &pars

) {

  // Sanity checks
  if (N0.size() != 4u) throw std::runtime_error("N0 should be of length 4.");
  for (size_t i = 0u; i < N0.size(); ++i) if (N0[i] < 0.0) throw std::runtime_error("N0 cannot have negative elements");
  if (x.size() != 2u) throw std::runtime_error("x should be of length 2");

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

    const double s11 = 1.0 / (1.0 + exp(a * (theta1 - x[0u])));
    const double s12 = 1.0 / (1.0 + exp(a * (theta2 - x[0u])));
    const double s21 = 1.0 / (1.0 + exp(a * (theta1 - x[1u])));
    const double s22 = 1.0 / (1.0 + exp(a * (theta2 - x[1u])));

    const double y1 = rmax - epsilon * x[0u];
    const double y2 = rmax - epsilon * x[1u];
    const double r11 = exp(y1 * (1.0 - (N[0u] + N[2u]) / (c * K1)));
    const double r12 = exp(y1 * (1.0 - (N[1u] + N[3u]) / ((1 - c) * K2)));
    const double r21 = exp(y2 * (1.0 - (N[0u] + N[2u]) / (c * K1)));
    const double r22 = exp(y2 * (1.0 - (N[1u] + N[3u]) / ((1 - c) * K2)));

    const double N11 = s11 * c * (r11 * N[0u] + r12 * N[1u]);
    const double N12 = s12 * (1 - c) * (r11 * N[0u] + r12 * N[1u]);
    const double N21 = s21 * c * (r21 * N[2u] + r22 * N[3u]);
    const double N22 = s22 * (1 - c) * (r21 * N[2u] + r22 * N[3u]);

    N[0u] = N11;
    N[1u] = N12;
    N[2u] = N21;
    N[3u] = N22;

  }

  return N;

}
