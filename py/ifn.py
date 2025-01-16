import math

def gaussian_comparison(mu1, sd1, mu2, sd2):
    """
    Determine the range where the probability density of N1(mu1, sd1) > N2(mu2, sd2).
    
    Args:
        mu1 (float): Mean of the first Gaussian distribution.
        sd1 (float): Standard deviation of the first Gaussian distribution.
        mu2 (float): Mean of the second Gaussian distribution.
        sd2 (float): Standard deviation of the second Gaussian distribution.
    
    Returns:
        tuple: A tuple containing the range (x1, x2) where p(N1) > p(N2).
               If the range is unbounded, x1 or x2 will be None.
               If both distributions are degenerate, returns a special message.
    """
    # Check for degenerate distributions (sd1 or sd2 is 0)
    if sd1 == 0 and sd2 == 0:
        if mu1 == mu2:
            return "p(N1) == p(N2) everywhere"
        elif mu1 < mu2:
            return (mu1, None)  # p(N1) > p(N2) at x = mu1
        else:
            return (None, mu2)  # p(N1) > p(N2) at x = mu2
    elif sd1 == 0:
        return (mu1, None)  # p(N1) > p(N2) at x = mu1
    elif sd2 == 0:
        return (None, mu2)  # p(N1) > p(N2) at x = mu2

    # Coefficients for the quadratic inequality
    A = (1 / sd2**2) - (1 / sd1**2)
    B = (2 * mu1 / sd1**2) - (2 * mu2 / sd2**2)
    C = (mu2**2 / sd2**2) - (mu1**2 / sd1**2) - 2 * (math.log(sd2) - math.log(sd1))
    
    if A == 0:  # Linear case
        if B == 0:
            return None  # p(N1) and p(N2) are identical
        x = -C / B
        return (None, x) if B > 0 else (x, None)
    
    # Solve the quadratic equation
    discriminant = B**2 - 4 * A * C
    if discriminant < 0:
        return None  # No solutions, one Gaussian dominates everywhere
    
    sqrt_discriminant = math.sqrt(discriminant)
    x1 = (-B - sqrt_discriminant) / (2 * A)
    x2 = (-B + sqrt_discriminant) / (2 * A)
    
    # Determine the regions where the inequality holds
    if A > 0:
        return (None, x1), (x2, None)  # Outside the roots
    else:
        return (x1, x2)  # Between the roots

# Example usage
mu1, sd1 = 0, 0
mu2, sd2 = 1, 1
ranges = gaussian_comparison(mu1, sd1, mu2, sd2)
print("Ranges where p(N1) > p(N2):", ranges)

