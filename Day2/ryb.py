'''
Somewhat based on the code from "Paint Inspired Color Compositing"
by N. Gossett and B. Chen.

'''
from __future__ import division
from numpy import array, floor
from sympy import Symbol, expand

# Colors from paletton.com
c000 = [255, 255, 255] # white
c100 = [255, 0, 0] # red
c010 = [255, 255, 0] # yellow
c011 = [0, 255, 0] # green
c001 = [3, 79, 255] # blue
c101 = [165, 1, 255] # purple
c110 = [255, 170, 0] # orange
c111 = [0, 0, 0] # black
C0 = array([[[c000, c001], [c010, c011]], [[c100, c101], [c110, c111]]])

# Colors from the paper:
c000 = [1, 1, 1] # white
c100 = [1, 0, 0] # red
c010 = [1, 1, 0] # yellow
c011 = [0, 0.66, 0.2] # green
c001 = [0.163, 0.373, 0.6] # blue
c101 = [0.5, 0, 0.5] # purple
c110 = [1, 0.5, 0] # orange
c111 = [0.2, 0.094, 0] # black
C0 = 255 * array([[[c000, c001], [c010, c011]], [[c100, c101], [c110, c111]]])


def weight(t, C, func):
    '''
    t: a number between 0 (for C[0]) and 255 (for C[1])
    C: two values to interpolate between
    func: the interpolation function. func(0) = 0, func(1) = 1
    '''
    return C[0] + (C[1] - C[0]) * func(t/255)

def ryb_to_rgb(r, y, b, func=lambda t: t):
    '''
    Calculate a trilinear (up to func) interpolation
    for the point (r, y, b) within the 3d cube
    '''
    C1 = array([[weight(b, C0[i, j], func)
                 for j in (0, 1)]
                for i in (0, 1)])
    C2 = array([weight(y, C1[i], func) for i in (0, 1)])
    C3 = weight(r, C2, func)

    return tuple(C3)

r = Symbol('r')
y = Symbol('y')
b = Symbol('b')

# rgb = ryb_to_rgb(r, y, b, func=lambda t: t*t*(3-2*t)) # This interpolation was done in the paper
rgb = ryb_to_rgb(r, y, b, func=lambda t: t)

print '{ red = floor <|', expand(rgb[0])
print ', green = floor <|', expand(rgb[1])
print ', blue = floor <|', expand(rgb[2])
