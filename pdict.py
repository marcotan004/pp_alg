import collections.abc
# Taken from PP3 solution

class PersistentDictionary(collections.abc.Mapping):
    """Data structure that behaves like a dict except it's immutable, you
    can't write d[x] = y. Instead, you call d.set(x, y) which returns
    a new PersistentDictionary.

    This implementation is not particularly fancy or efficient; it's
    just a linked list, but that's fairly typical."""

    def __init__(self, xys=()):
        self._items = xys

    def __getitem__(self, key):
        for (x, y) in self.items():
            if x == key:
                return y
        raise KeyError()

    def __iter__(self):
        for x, y in self.items():
            yield x

    def __len__(self):
        return len(list(self.items()))

    def items(self):
        xys = self._items
        while xys != ():
            ((x, y), xys) = xys
            yield (x, y)

    def set(self, key, value):
        return PersistentDictionary(((key, value), self._items))

    def __str__(self):
        return str(dict(self.items()))
