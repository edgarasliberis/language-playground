
def prime(max_prime):
    """
        Iterates through all primes from 2 to @max_prime.
        Uses Sieve of Erathostenes to check for primality.

        @param upper bound for a prime number that iterator will yield
        @return iterator returning primes
    """
    i = 2
    prime_flags = [True] * (max_prime + 1)
    while i**2 <= max_prime:
        if not prime_flags[i]:
            i += 1
            continue

        # i is a prime
        yield i

        # Eliminate all multiples of i
        j = i
        while j * i <= max_prime:
            prime_flags[j * i] = False
            j += 1
        i += 1

    for i in range(i, max_prime + 1):
        if prime_flags[i]:
            yield i

for i in prime(121):
    print "returned ", i