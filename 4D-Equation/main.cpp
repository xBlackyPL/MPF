#include <iostream>
#include <random>

#include "blaze/math/UniformMatrix.h"

using namespace blaze;
double eps = 1e-10;

const unsigned int dimensions = 4;
using Guess = std::array<double, dimensions>;

template <typename T>
Guess normalize_random_values(T vec) {
    std::random_device random_device{};
    std::mt19937 generator{random_device()};
    std::normal_distribution<> distribution{0, 1};

    std::vector<double> random_values_square;
    double length = 0;
    for (const auto& value : vec) {
        auto square = value * value;
        random_values_square.push_back(square);
        length += square;
    }
    for (auto& value : random_values_square) {
        value /= length;
    }

    std::vector<double> res;
    for (const auto& value : random_values_square) {
        if (distribution(generator) > 0) {
            res.push_back(sqrt(value));
        } else {
            res.push_back(-1 * sqrt(value));
        }
    }

    Guess result;
    std::copy_n(std::begin(res), dimensions, std::begin(result));
    return result;
}

Guess generate_random_values() {
    std::random_device random_device{};
    std::mt19937 generator{random_device()};
    std::normal_distribution<> distribution{0, 1};

    std::vector<double> random_values{
        distribution(generator), distribution(generator),
        distribution(generator), distribution(generator)};

    return normalize_random_values(random_values);
}

DynamicMatrix<double> generate_r(Guess guess) {
    double a = guess[0];
    double b = guess[1];
    double c = guess[2];
    double d = guess[3];
    double a2 = a * a;
    double b2 = b * b;
    double c2 = c * c;
    double d2 = d * d;

    return DynamicMatrix<double>{
        {a2 * b2 - c2 - d2, -2 * a * d + 2 * b * c, 2 * a * c + 2 * b * d},
        {2 * a * d + 2 * b * c, a2 - b2 + c2 - d2, -2 * a * b + 2 * c * d},
        {-2 * a * c + 2 * b * d, 2 * a * b + 2 * c * d, a2 - b2 - c2 + d2}};
}

int main(int argc, char const* argv[]) {
    StaticMatrix<double, 3UL, 3UL> ingred_a{{1.1880, 0.0787, 0.7829},
                                            {0.0787, 1.1128, -0.0244},
                                            {0.7820, -0.0244, 1.3956}};

    StaticMatrix<double, 3UL, 3UL> ingred_b{{1.02469, 0.1090, 0.6547},
                                            {0.1090, 1.0386, -0.2852},
                                            {0.6547, -0.2852, 1.6309}};

    return 0;
}
