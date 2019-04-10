#include <omp.h>
#include <unistd.h>
#include <array>
#include <iostream>
#include <memory>
#include <random>

#include "pso.hpp"

const unsigned int dimensions = 4;
using Guess = std::array<float, dimensions>;
using Matrix = std::vector<std::vector<float>>;

template <typename T>
Guess normalize_random_values(T vec) {
    float a = vec[0];
    float b = vec[1];
    float c = vec[2];
    float d = vec[3];
    float sqrt_length = sqrt(a * a + b * b + c * c + d * d);
    a /= sqrt_length;
    b /= sqrt_length;
    c /= sqrt_length;
    d /= sqrt_length;

    Guess result{a, b, c, d};
    return result;
}

Matrix generate_r(Guess guess) {
    float a = guess[0];
    float b = guess[1];
    float c = guess[2];
    float d = guess[3];
    return Matrix{{a * a + b * b - c * c - d * d, -2 * a * d + 2 * b * c,
                   2 * a * c + 2 * b * d},
                  {2 * a * d + 2 * b * c, a * a - b * b + c * c - d * d,
                   -2 * a * b + 2 * c * d},
                  {-2 * a * c + 2 * b * d, 2 * a * b + 2 * c * d,
                   a * a - b * b - c * c + d * d}};
}

Matrix generate_rt(Guess guess) {
    float a = guess[0];
    float b = guess[1];
    float c = guess[2];
    float d = guess[3];
    return Matrix{{a * a + b * b - c * c - d * d, 2 * a * d + 2 * b * c,
                   -2 * a * c + 2 * b * d},
                  {-2 * a * d + 2 * b * c, a * a - b * b + c * c - d * d,
                   2 * a * b + 2 * c * d},
                  {2 * a * c + 2 * b * d, -2 * a * b + 2 * c * d,
                   a * a - b * b - c * c + d * d}};
}

Matrix matrix_multiplication(const Matrix &m1, const Matrix &m2) {
    Matrix result(3, {0.f, 0.f, 0.f});

    for (size_t i = 0UL; i < m1.size(); ++i) {
        for (size_t j = 0UL; j < m1[i].size(); ++j) {
            float value = 0;
            for (size_t k = 0UL; k < m1.size(); ++k) {
                value += m1.at(i).at(k) * m2.at(k).at(j);
            }
            result.at(i).at(j) = value;
        }
    }
    return result;
}

Matrix matrix_subtraction(const Matrix &m1, const Matrix &m2) {
    Matrix result(3, {0.f, 0.f, 0.f});
    for (size_t i = 0UL; i < m1.size(); ++i) {
        for (size_t j = 0UL; j < m1.size(); ++j) {
            result.at(i).at(j) = m1.at(i).at(j) - m2.at(i).at(j);
        }
    }
    return result;
}

float equation(float *x, int n) {
    std::vector<float> raw_guess{x[0], x[1], x[2], x[3]};
    auto guess = normalize_random_values(raw_guess);

    Matrix a = {{1.1880f, 0.0787f, 0.7829f},
                {0.0787f, 1.1128f, -0.0244f},
                {0.7829f, -0.0244f, 1.3956f}};

    Matrix b = {{1.0269, 0.1090, 0.6547},
                {0.1090, 1.0386, -0.2852},
                {0.6547, -0.2852, 1.6309}};

    Matrix r = generate_r(guess);
    Matrix rt = generate_rt(guess);
    Matrix result = matrix_multiplication(r, a);
    Matrix result1 = matrix_multiplication(result, rt);
    Matrix result2 = matrix_subtraction(result1, b);

    float sum_of_squares = 0;
    for (size_t i = 0UL; i < result2.size(); ++i) {
        for (size_t j = 0UL; j < result2.size(); ++j) {
            sum_of_squares += result2.at(i).at(j) * result2.at(i).at(j);
        }
    }
    return sum_of_squares;
}

int main(int argc, char const *argv[]) {
    float ftemp = 0.f;
    float f = 0.f;
    int flag, nt;
    float eps = 0.00001f;
    int n = 4;
    int nb = 128;

    auto x = std::unique_ptr<float>(new float(n));
    auto x_temp = std::unique_ptr<float>(new float(n));
    auto min = std::unique_ptr<float>(new float(n));
    auto max = std::unique_ptr<float>(new float(n));

    for (int i = 0; i < n; i++) {
        min.get()[i] = -1.f;
        max.get()[i] = 1.f;
    }
    int max_iter = -1;

#pragma omp parallel
    nt = omp_get_num_threads();
    omp_set_nested(1);
    omp_set_num_threads(2);
    flag = 0;
#pragma omp parallel
    {
        if (omp_get_thread_num() == 0) {
            omp_set_num_threads(nt);
            f = pso(x.get(), n, &equation, nb, min.get(), max.get(), eps,
                    &max_iter, x_temp.get(), &ftemp);
            flag = 1;
        } else {
            while (1) {
                sleep(1);
                if (ftemp < 0.01) {
                    max_iter = 1;
                }
                if (flag == 1) {
                    break;
                }
            }
        }
    }

    double sum_of_parametr_squares = sqrt(x.get()[0] * x.get()[0] + x.get()[1] * x.get()[1] +
                    x.get()[2] * x.get()[2] + x.get()[3] * x.get()[3]);

    for (int j = 0; j < n; j++) {
        std::cout << std::to_string(x.get()[j]/sum_of_parametr_squares) << " ";
    }
    std::cout << std::to_string(sum_of_parametr_squares) << "\t" << std::to_string(f) << std::endl;
    return 0;
}
