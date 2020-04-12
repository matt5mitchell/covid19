## Fit SEIR model

# Fit SEIR model to US data

# Fixed population size, full population in S1 initially
# Movement to S2 based on Gallup polling data
# Use known nu (E to I), gamma (I to R), and mu (I to D) values
# Use R0 values from est_R0_two_compartments.R and gamma to calculate beta values

# Use optimizer to fit cumulative deaths
# Parameters to fit: beta1, beta2, mu, and possibly gamma