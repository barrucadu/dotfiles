#! /usr/bin/env python

import os, copy

from itertools import *
from collections import deque, defaultdict

pac_dir = "/var/lib/pacman/local/"

# depends contains %CONFLICTS%, 
# %DEPENDS%, %OPTDEPENDS%, %PROVIDES%

# desc contains %URL%, %REPLACES%, %LICENSE%,
# %NAME%, %GROUPS%, %BUILDDATE%, %REASON%, %DESC%,
# %SIZE%, %PACKAGER%, %ARCH%, %INSTALLDATE%, %VERSION%

# weird stuff:
# bonnie++ is only program with GCC as dep
# man-db and manpages are not linked

# 8 chars at 21 pt =ish 100 px

pj = os.path.join

def l_part(n, c):
    return n.partition(c)[0]

def reduce_by(fn, data, arg_list):
    data = fn(data, arg_list.pop(0))
    if not arg_list:
	return data
    return reduce_by(fn, data, arg_list)

def clean(n):
    n = n.strip()
    return reduce_by(l_part, n, list('><:='))

def load_info(file):
    info = {}
    mode = None
    for line in file:
	line = clean(line)
	if not line:
	    continue
	if line.startswith('%'):
	    mode = line
	    info[mode] = []
	    continue
	info[mode].append(line)
    file.close()
    return info

def strip_info(info):
    keep = ['DEPENDS', 'OPTDEPENDS', 'PROVIDES', 'SIZE']
    info = dict((k.strip('%'),v) for k,v in info.iteritems())
    name = info['NAME'][0]
    info = dict((k,v) for k,v in info.iteritems() if k in keep)
    if 'SIZE' in info:
        info['SIZE'] = int(info['SIZE'][0], 10)
    else:
        info['SIZE'] = 0
    return name, info

def load_tree():
    packages = [p for p,d,f in os.walk(pac_dir) if f]
    tree = {}
    for p in packages:
	info = {}
	file = open(pj(p,'depends'), 'r')
	info.update(load_info(file))
	file = open(pj(p,'desc'), 'r')
        info.update(load_info(file))
        try:
	    name, info = strip_info(info)
	    tree[name] = info
        except:
            print 'Error reading package', p
    return tree

def search_provides(package, tree):
    "use only on load_tree data"
    tree2 = dict((p,tree[p]['PROVIDES']) for p in tree if 'PROVIDES' in tree[p])
    return [p for p in tree2 if package in tree2[p]]

def actually_installed(packages, tree):
    "use only on load_tree data"
    simple = set(packages) & set(tree.keys())
    maybe = set(packages) - simple
    for p in maybe:
        provides = search_provides(p, tree)
	if len(provides) > 1:
	    print 'error:', p, 'found in', provides
	if len(provides) == 1:
	    simple.add(provides[0])
	# len 0 means not installed optdep
    return list(simple)

def merge_tree(tree):
    "merge provides, depends, optdepends"
    tree2 = {}
    # merge
    for p in tree:
	tp = defaultdict(list, tree[p])
	deps = tp['DEPENDS'] + tp['OPTDEPENDS']
	# remove unused optdeps
	deps = actually_installed(deps, tree)
	tree2[p] = (tree[p]['SIZE'], deps)
    return tree2

def full_deps(package, tree):
    "returns every package in dep tree"
    deps = set()
    to_crawl = deque([package])
    while to_crawl:
	current = to_crawl.popleft()
	if current in deps:
	    continue
	deps.add(current)
	current_deps = set(tree[current][1])
	to_crawl.extend(current_deps - deps)
    return list(deps)

def invert_tree(tree):
    "turns depends-on into required-by"
    reqs = dict((p,(tree[p][0], [])) for p in tree)
    for p in tree:
	deps = tree[p][1]
	[reqs[d][1].append(p) for d in deps]
    return reqs

def flatten(listOfLists):
    return list(chain.from_iterable(listOfLists))

def rle(m):
    return [(n, len(list(g))) for n,g in groupby(m)]

def single_depends(tree):
    "packages with only one parent"
    all_deps = flatten(v[1] for k,v in tree.iteritems())
    dep_count = dict(rle(sorted(all_deps)))
    return (k for k,v in dep_count.iteritems() if v == 1)

def compress_chains(tree):
    "single depends are absorbed into parent"
    while True:
        singles = single_depends(tree)
	try:
	    s = singles.next()
	except StopIteration:
	    return tree
	req_by = invert_tree(tree)
	parent = req_by[s][1][0]
	#print 'merge', s, 'into', parent
	new_size = tree[parent][0] + tree[s][0]
	new_deps = tree[parent][1] + tree[s][1]
	new_deps = list(set(new_deps))
	new_deps.remove(s)
	tree[parent] = (new_size, new_deps)
	tree.pop(s)

def sum_sizes(packages, tree):
    return sum(tree[p][0] for p in packages if p in tree)

def shared_size(package, tree):
    "package and all deps"
    return sum_sizes(full_deps(package, tree), tree)

def biggest_packs(tree):
    packs = [(shared_size(p, tree), p) for p in tree]
    packs.sort()
    packs.reverse()
    return [p for s,p in packs]

def dep_sizes(tree):
    "include deps in size"
    return dict((p, (shared_size(p, tree), tree[p][1])) for p in tree)

def pt_sizes(tree):
    "size in bytes -> size in points"
    min_pt = 10
    max_pt = 100
    sizes = [deps[0] for p,deps in tree.iteritems()]
    min_s = min(sizes)
    max_s = max(sizes)
    for p, deps in tree.iteritems():
        size = deps[0]
        pt = int((max_pt-min_pt)*(size-min_s)/(max_s-min_s) + min_pt)
        tree[p] = (pt, tree[p][1])
    return tree

def drawable_tree():
    tree = compress_chains(merge_tree(load_tree()))
    return pt_sizes(tree)

def toplevel_packs(tree):
    "do this before bidrection"
    toplevel = set(tree.keys())
    for name in tree:
        deps = tree[name][1]
        toplevel = toplevel - set(deps)
    return toplevel


#print 'worst shared packages:', biggest_packs(tree)[:20]
#print 'most crucial packages:', biggest_packs(invert_tree(tree))[:20]

