#!/usr/bin/env python3

import os
import sys
import argparse

if len(sys.argv) < 2:
    print("usage: {} VAL".format(sys.argv[0]))
    sys.exit(1)

base = 5000
income = int(sys.argv[1])
if income < base:
    print("no tax")
    sys.exit(1)

threshold = [36000, 144000, 300000, 420000, 660000, 960000]
percentage = [3, 10, 20, 25, 30, 35, 45]

def get_tax(value):
    if value <= threshold[0]:
        return value * percentage[0] / 100
    elif value <= threshold[1]:
        return get_tax(threshold[0]) + (value - threshold[0]) * percentage[1] / 100
    elif value <= threshold[2]:
        return get_tax(threshold[1]) + (value - threshold[1]) * percentage[2] / 100
    elif value <= threshold[3]:
        return get_tax(threshold[2]) + (value - threshold[2]) * percentage[3] / 100
    elif value <= threshold[4]:
        return get_tax(threshold[3]) + (value - threshold[3]) * percentage[4] / 100
    elif value <= threshold[5]:
        return get_tax(threshold[4]) + (value - threshold[4]) * percentage[5] / 100
    else:
        return get_tax(threshold[5]) + (value - threshold[5]) * percentage[6] / 100


insurance_percentae = 0.12
special = 0.01
others = 0.05

MONTH = 12
year_income = income * MONTH
year_income_for_tax = (income * (1.0 - insurance_percentae - special - others) - base) * MONTH
if year_income_for_tax < 0:
    print("something must be wrong")
    sys.exit(1)
year_tax = get_tax(year_income_for_tax)
print("tax: {:>8.2f}, income: {:>6d}, percent: {:>6.2f}".format(year_tax, year_income, year_tax / year_income))
