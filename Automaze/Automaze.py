from random import randrange, uniform, choice
from collections import defaultdict, OrderedDict

class Map:
    _tiles = {
        'wall': ' ',
        'walk': '█',
        'door': '▒'
    }

    def __init__(self, width, height, default='wall'):
        self.width = width
        self.height = height
        self.map = list(self._tiles[default] * width * height)

    def printable(self):
        lines = [self.map[i:i+self.width]
                 for i in range(0, len(self.map), self.width)]
        return '\n'.join([''.join(l) for l in lines])

    def get(self, tile, x, y):
        if not self.fits(x, y): return False
        return self.map[y*self.width + x] == self._tiles[tile]

    def put(self, tile, x, y):
        if not self.fits(x, y): return
        self.map[y*self.width + x] = self._tiles[tile]

    def is_all(self, tile, x, y, w, h):
        if not self.fits(x, y, w, h): return False
        return all([self.map[j*self.width + i] == self._tiles[tile]
                    for i in range(x, x+w) for j in range(y, y+h)])

    def fits(self, x, y, w=0, h=0):
        return all([x >= 0, y >= 0, x+w < self.width, y+h < self.height])

class Automaze:
    def __init__(self, gen=True,
                 width=150, height=90,
                 windiness=0.1, extra_doors=0.05,
                 room_tries=350, room_min=17, room_max=21, room_ratio=1.4):
        self.width = width
        self.height = height

        self.windiness = windiness
        self.extra_doors = extra_doors

        self.room_attempts = room_tries
        self.room_min_size = room_min
        self.room_max_size = room_max
        self.room_max_ratio = room_ratio

        self._directions = ['dn', 'rx', 'up', 'lx']

        self._maze = Map(1, 1)
        if gen: self.generate()

    def generate(self):
        self._maze = Map(self.width, self.height)

        self._regions = defaultdict(lambda: -1)
        self._region_count = 0

        self._add_rooms()
        self._add_paths()
        self._connect_regions()
        self._remove_deadends()

        return self

    def get_map(self):
        return self._maze.printable()

    def _add_rooms(self):
        def odd(n):
            return int(n/2) * 2 + 1

        def stretch(n):
            return odd(n * uniform(1.0, self.room_max_ratio))

        for t in range(self.room_attempts):
            x, y = randrange(0, self.width, 2), randrange(0, self.height, 2)

            room_range = self.room_max_size - self.room_min_size
            w = h = odd(self.room_min_size + randrange(room_range))

            if t % 2 == 0: w = stretch(w)
            else: h = stretch(h)

            if not self._maze.is_all('wall', x, y, w, h): continue

            for i in range(x, x+w):
                for j in range(y, y+h):
                    self._carve(i, j)

            self._end_region()

    def _add_paths(self):
        for x in range(0, self.width, 2):
            for y in range(0, self.height, 2):
                if not self._can_carve(x, y): continue
                self._growing_tree(x, y)
                self._end_region()

    def _growing_tree(self, x, y):
        def get_safe_dirs(x, y):
            return [d for (d, (i, j))
                      in zip(self._directions, self._neighbors(x, y, 2))
                      if self._can_carve(i, j)]

        next_dir = None
        queue = [(x, y)]

        self._carve(x, y)

        while queue != []:
            x, y = queue[-1]
            safe_dirs = get_safe_dirs(x, y)

            if safe_dirs == []:
                queue.pop()
                next_dir = None
                continue

            if next_dir not in safe_dirs or uniform(0, 1) < self.windiness:
                next_dir = choice(safe_dirs)

            for i in range(2):
                x, y = self._move(next_dir, x, y)
                self._carve(x, y)

            queue.append((x, y))

    def _connect_regions(self):
        def get_neighboring_regions(x, y):
            rs = [self._regions[k] for k in self._neighbors(x, y)]
            return sorted(OrderedDict.fromkeys([r for r in rs if r >= 0]))

        def get_doors():
            return {(x, y): rs for x in range(1, self.width, 2)
                               for y in range(1, self.height, 2)
                               for rs in [get_neighboring_regions(x, y)]
                               if self._can_carve(x, y)
                               if len(rs) == 2}

        doors = get_doors()

        while doors:
            x, y = choice(list(doors.keys()))
            rs = doors[(x, y)]

            self._open_door(x, y)

            for (x, y) in doors.keys():
                if doors[(x, y)] == rs and uniform(0, 1) < self.extra_doors:
                    self._open_door(x, y)

            doors = {k: v for (k, v) in doors.items() if v != rs}

    def _remove_deadends(self):
        def count_walls(x, y):
            return sum([self._can_carve(i, j) or not self._maze.fits(i, j)
                        for (i, j) in self._neighbors(x, y)])

        def check_deadend(x_range, y_range):
            return {(x, y) for x in x_range for y in y_range
                           if not self._can_carve(x, y)
                           if self._maze.fits(x, y)
                           if count_walls(x, y) > 2}

        ends = check_deadend(range(0, self.width, 2), range(0, self.height, 2))

        while ends:
            x, y = ends.pop()
            self._uncarve(x, y)
            ends = ends | check_deadend(range(x-1, x+2), range(y-1, y+2))

    def _can_carve(self, x, y):
        return self._maze.get('wall', x, y)

    def _carve(self, x, y):
        self._maze.put('walk', x, y)
        self._regions[(x, y)] = self._region_count

    def _uncarve(self, x, y):
        self._maze.put('wall', x, y)
        if (x, y) in self._regions: self._regions.pop((x, y))

    def _open_door(self, x, y):
        self._maze.put('door', x, y)

    def _end_region(self):
        self._region_count += 1

    def _move(self, d, x, y, steps=1):
        dx = steps*((d == 'rx') - (d == 'lx'))
        dy = steps*((d == 'dn') - (d == 'up'))
        return (x+dx, y+dy)

    def _neighbors(self, x, y, dist=1):
        for d in self._directions: yield self._move(d, x, y, dist)

if __name__ == "__main__":
    dungeon = Automaze()
    print(dungeon.get_map())
