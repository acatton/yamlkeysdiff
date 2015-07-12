#!/usr/bin/env python
# -*- coding: utf-8 -*-

import random
import json
import string

class ToJsonEncoder(json.JSONEncoder):
    def default(self, obj):
        if hasattr(obj, 'to_json'):
            return obj.to_json()
        else:
            return super().default(obj)


class RandomObject(object):
    def __init__(self, depth=1):
        self.rand = random.SystemRandom()
        self.depth = depth

    def random_length(self):
        return self.rand.randint(0, 50 // self.depth)

    def random_string(self):
        length = self.random_length()
        return ''.join(self.rand.choice(string.ascii_letters) for i in range(length))

    def random_dict(self):
        length = self.random_length()
        keys = set(self.random_string() for i in range(length))
        klass = self.__class__
        return {k: klass(self.depth + 1) for k in keys}

    def random_list(self):
        length = self.random_length()
        klass = self.__class__
        return [klass(self.depth + 10) for i in range(length)]

    def to_json(self):
        i = self.rand.random()
        if i < 0.40:
            return self.random_dict()
        elif i < 0.50:
            return self.random_list()
        else:
            return self.random_string()

def run(args):
    print(json.dumps(RandomObject(), cls=ToJsonEncoder))

if __name__ == '__main__':
    import sys
    run(sys.argv[1:])
