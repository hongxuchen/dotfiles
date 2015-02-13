#!/usr/bin/env python
import time


class Marsaglia:

    """Pseduo-random generator

    http://www.math.uni-bielefeld.de/~sillke/ALGORITHMS/random/marsaglia-c
    """

    def __init__(self, i1=0, i2=0):
        self.z = i1 or 362436069
        self.w = i2 or 521288629

    def nextInt(self):
        self.z = (36969 * (self.z & 65535) + (self.z >> 16)) & 0xffffffff
        self.w = (18000 * (self.w & 65535) + (self.w >> 16)) & 0xffffffff
        i = (((self.z & 0xffff) << 16) | (self.w & 0xffff) & 0xffffffff)
        return i

    def nextDouble(self):
        i = self.nextInt() / 4294967296
        return (1 + i) if i < 0 else i

    def createRandomized(self):
        now = int(time.time())
        self.z = (now // 60000) & 0xffffffff
        self.w = now & 0xffffffff


if __name__ == "__main__":
    m = Marsaglia()
    m.createRandomized()

    print(m.nextInt())
    print(m.nextDouble())
