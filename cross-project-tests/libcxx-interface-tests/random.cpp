#include <random>
#include <map>
#include <iostream>
#include <cassert>

int main(){
    
    // Generating sequences of seeds for random number generator
    std::seed_seq seq{1, 2, 3, 4, 5};
    std::vector<std::uint32_t> seeds(10);
    seq.generate(seeds.begin(), seeds.end());
    std::cout << "Sequences of seeds: ";
    for (auto it = seeds.begin(); it != seeds.end(); it++)
        std::cout << *it << " ";
    std::cout << std::endl;
    
    std::random_device rd;
    std::mt19937 gen(rd());

    // Drawing 10 numbers from uniform distribution
    std::uniform_int_distribution<int> uniform_d(0, 9);
    std::cout << "Uniform(0,9] distribution: ";
    for (int n=0; n != 10; ++n)
        std::cout << uniform_d(gen) << " ";
    std::cout << std::endl;

    // Drawing 10 numbers from Bernoulli distribution
    std::bernoulli_distribution bernoulli_d(0.25);
    std::cout << "Bernoulli distribution: ";
    for (int n=0; n != 10; ++n)
        std::cout << bernoulli_d(gen) << " ";
    std::cout << std::endl;

    // Drawing 10 numbers from binomial distribution
    std::binomial_distribution<> binom_d(4, 0.5);
    std::cout << "Binomial distribution: ";
    for (int n=0; n != 10; ++n)
        std::cout << binom_d(gen) << " ";
    std::cout << std::endl;

    // Drawing 10 numbers from Poisson distribution
    std::poisson_distribution<> poisson_d(4);
    std::cout << "Poisson distribution: ";
    for (int n=0; n != 10; ++n)
        std::cout << poisson_d(gen) << " ";
    std::cout << std::endl;

    // Drawing 10 numbers from exponential distribution
    std::exponential_distribution<> exponential_d(1);
    std::cout << "Exponential distribution: ";
    for (int n=0; n != 10; ++n)
        std::cout << exponential_d(gen) << " ";
    std::cout << std::endl;

    // Drawing 10 numbers from normal distribution
    std::normal_distribution<> normal_d{5.0, 2.0};
    std::cout << "Normal distribution: ";
    for (int n=0; n != 10; ++n)
        std::cout << normal_d(gen) << " ";
    std::cout << std::endl;

    // Drawing 10 numbers from discrete distribution
    std::discrete_distribution<> discrete_d({40, 10, 10, 40});
    std::cout << "Discrete distribution: ";
    for (int n=0; n != 10; ++n)
        std::cout << discrete_d(gen) << " ";
    std::cout << std::endl;

    return 0;
}
