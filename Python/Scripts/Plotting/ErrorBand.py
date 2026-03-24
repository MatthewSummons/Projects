import matplotlib.pyplot as plt
import numpy as np
from numpy.polynomial import Polynomial
from scipy.optimize import curve_fit
import seaborn as sns

ORANGE = '#D57541'
BLUE   = '#2E5F7F'

sns.set_theme()
N = 100
x = np.linspace(start=0.0, stop=0.175, num=N)

r, r_err = 0.0275, 0.001    # Inner Radius of Cylinder (± error)
R, R_err = 0.0315, 0.001    # Outer Radius of Cylinder (± error)
g = 9.81                    # Acceleration due to Gravity


def omega(h:float | np.ndarray, r:float, R:float) -> float | np.ndarray:
    return np.sqrt((4 * g * h) / (3 * R ** 2 + r ** 2))


def omega_sqr(h:float | np.ndarray, r:float, R:float) -> float | np.ndarray:
    return (4 * g * h) / (3 * R ** 2 + r ** 2)

def sqrt(x, a, b):
    return a + b * np.sqrt(x)


lower_bounds = omega(x, r + r_err, R + R_err)
theoretical = omega(x, r, R)
upper_bounds = omega(x, r - r_err, R - R_err)

# Experimental Data (Height in m, Angular Frequency (& its square) in rad/s, and their respective errors)
x_data, err_x    = [0.03, 0.06, 0.09, 0.12, 0.15],             [0.002] * 5
y, err_y         = [12.08, 21.67, 28.56, 34.27, 36.96],        [0.42, 1.49, 1.30, 2.80, 2.17]
y_sqr, sqr_err_y = [146.00, 469.42, 815.67, 1174.56, 1366.03], [5.62, 64.75, 74.15, 192.20, 160.71]

# Least-Squares Fits for Experimental Data
popt, _ = curve_fit(sqrt, x_data, y)
print("Coefficients for Angular Frequency Fit (a, b):", popt)
best_fit_y = sqrt(x, *popt)


series_fit_w_sqr = Polynomial.fit(x_data, y_sqr, 1)
print("Coefficients for Squared Angular Frequency Fit (a, b):", series_fit_w_sqr.coef)
best_fit_y_sqr = series_fit_w_sqr(x)


# Figure 1
plt.figure("Angular Frequency on Height", figsize=(10, 6))

plt.plot(x, theoretical, '-', color="ORANGE", label='Theoretical')
plt.fill_between(x, lower_bounds, upper_bounds, color=ORANGE, alpha=0.2)

plt.errorbar(x_data, y, xerr=err_x, yerr=err_y, capsize=3, ms=5, color=BLUE, linestyle='None')
plt.plot(x, best_fit_y, '--', color=BLUE, alpha=0.5, label='Experimental')

plt.xlabel("Vertical Height (m)")
plt.ylabel("Angular Frequency ($rads^{-1}$)")
plt.legend(title='Angular Frequency on Height')

# Figure 2
plt.figure("Squared Angular Frequency on Height", figsize=(10, 6))
plt.plot(x, omega_sqr(x, r, R), '-', color='#D57541', label='Theoretical')
plt.fill_between(x, omega_sqr(x, r + r_err, R + R_err), omega_sqr(x, r - r_err, R - R_err), color=ORANGE, alpha=0.2)

plt.errorbar(x_data, y_sqr, xerr=err_x, yerr=sqr_err_y, capsize=3, ms=5, color=BLUE, linestyle='None')
plt.plot(x, best_fit_y_sqr, '--', color=BLUE, alpha=0.5, label='Experimental')
plt.xlabel("Vertical Height (m)")
plt.ylabel("Squared Angular Frequency ($rad^2s^{-2}$)")
plt.legend(title='Squared Angular Frequency on Height')

plt.show()