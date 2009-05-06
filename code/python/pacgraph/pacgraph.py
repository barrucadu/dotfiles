#! /usr/bin/env python

import random, math, subprocess
from itertools import *

import pac_parse

def bidirection(packs):
    "directed graph -> undirected graph"
    packs2 = dict((n, [(0,0), []]) for n in packs.keys())
    for name in packs:
        dim, links = packs[name]
        packs2[name][0] = dim
        packs2[name][1].extend(links)
        for link in links:
            packs2[link][1].append(name)
    for name in packs2:
        packs2[name][1] = list(set(packs2[name][1]))
    return packs2

def prioritized(packs):
    "returns list of names, sorted by priority"
    # first are the most 'central'
    stats = [(len(v[1]), k) for k,v in packs.items()]
    stats = [n for l,n in reversed(sorted(stats))]
    # but slip in anyone who's deps are met early
    stats2 = []
    fs = frozenset
    for n in stats:
        if n in stats2:
            continue
        stats2.append(n)
        plotted = fs(stats2)
        deps_met = [k for k,v in packs.items() if fs(v[1]) <= plotted]
        for d in fs(deps_met) - plotted:
            stats2.append(d)
    return stats2

def ran_rad():
    return random.random()*2*3.14159

def bbox(center, dim):
    c,d = center,dim
    x1,x2 = c[0]-d[0]//2, c[0]+d[0]//2
    y1,y2 = c[1]-d[1]//2, c[1]+d[1]//2
    return [x1, y1, x2, y2] 

def common_ranges(r1, r2):
    "returns true if overlap"
    r1,r2 = list(r1), list(r2)
    a = sorted([r1, r2])
    a = a[0] + a[1]
    b = sorted(r1 + r2)
    return a != b

def in_box(bbox1, bbox2):
    cr = common_ranges
    r1x = bbox1[0::2]
    r1y = bbox1[1::2]
    r2x = bbox2[0::2]
    r2y = bbox2[1::2]
    return cr(r1x, r2x) and cr(r1y, r2y)

def all_bboxes(name, coords, pri=None):
    b_list = []
    if pri is None:
        name_list = coords.keys()
    else:
        name_list = pri[:pri.index(name)]
    for n in name_list:
        c,d = coords[n]
        b_list.append(bbox(c,d))
    return b_list

def normalize(point, origin):
    p2 = point[0]-origin[0], point[1]-origin[1]
    length = (p2[0]**2 + p2[1]**2)**0.5
    return p2[0]/length, p2[1]/length

def link_pull(name, origin_n, packs, coords):
    "average of angles of links"
    origin = coords[origin_n][0]
    norm_ps = lambda ps: [normalize(c, origin) for c in ps if c not in [(0,0), origin]] 
    good_links = packs[name][1]
    bad_links  = packs[origin_n][1]
    g_centers  = [coords[l][0] for l in good_links]
    g_centers  = norm_ps(g_centers)
    b_centers  = [coords[l][0] for l in bad_links]
    b_centers  = norm_ps(b_centers)
    b_centers  = [(-x,-y) for x,y in b_centers]
    centers = g_centers + b_centers
    if not centers:  
        # new branch, try to avoid existing branches
        centers = [coords[l][0] for l in coords.keys()]
        centers = norm_ps(centers)
        if not centers:
            return (0,0)
        centers = [(-x,-y) for x,y in centers]
    return sum(zip(*centers)[0]), sum(zip(*centers)[1])

def xy2rad(x,y):
    "adds some wiggle so things are less spindly"
    if (x,y) == (0,0):
        return ran_rad()
    wiggle = 0.35  # radians
    wiggle = random.random()*wiggle - wiggle/2.0
    return math.atan2(y,x) + wiggle

def pol2xy(o,a,r):
    return int(o[0]+r*math.cos(a)), int(o[1]+r*math.sin(a))

def pt2dim(name, pt):
    x_scale = 0.65
    y_scale = 1.50
    return int(len(name)*pt*x_scale), int(pt*y_scale)

def empty_coords(packs):
    return dict((k, [(0,0), pt2dim(k,v[0])]) for k,v in packs.items())

def best_origin(name, pri, packs):
    "returns sibling with most links, or root"
    possible = pri[:pri.index(name)]
    possible = [n for n in possible if n in packs[name][1]]
    if not possible:
        return pri[0]  # root package
    return possible[0]

def place(packs):
    "radial placement algo, returns non-overlapping coords"
    coords = empty_coords(packs)
    # coords = {name: [(x_pos,y_pos), (x_size, y_size)], ...}
    pri = prioritized(packs)
    for name in pri[1:]:
        origin_name = best_origin(name, pri, packs)
        print 'placing', name, 'around', origin_name
        origin = coords[origin_name][0]
        heading = xy2rad(*link_pull(name, origin_name, packs, coords))
        scale = len(packs[name][1])+1  # more links need more room
        step,r = 5*scale,5*scale
        b_list = all_bboxes(name, coords, pri)
        while True:
            coords[name][0] = pol2xy(origin, heading, r)
            bb1 = bbox(*coords[name])
            o = any(in_box(bb1, bb2) for bb2 in b_list)
            #print name, r, step, o
            if o:
                if step < 0:
                    step = step * -1
                step = step * 2
            else:
                if 0 < step < 4*scale:
                    break
                if step > 0:
                    step = step * -1
                step = step // 3
            if -scale < step < 0:
                step = -scale
            if 0 >= step > scale:
                step = scale
            r = abs(r + step)
    return coords

def offset_coord(c,d):
    "corrects textbox origin"
    return c[0]-d[0]//2, c[1]  #+d[1]//2

def xml_wrap(tag, inner, **kwargs):
    kw = ' '.join('%s="%s"' % (k, v) for k,v in kwargs.items())
    if inner is None:
        return '<%s %s/>' % (tag, kw)
    return '<%s %s>%s</%s>' % (tag, kw, inner, tag)

def control_point(p1, p2):
    dx = abs(p2[0] - p1[0])
    lower  = (p1,p2)[p1[1]<p2[1]]
    higher = (p2,p1)[p1[1]<p2[1]]
    return (lower[0]+higher[0])//2, lower[1]+dx//2

def quad_spline(p1, p2):
    "boofor DSL in XML"
    p1,p2 = sorted((p1,p2))
    x1,y1 = p1
    x2,y2 = p2
    xc,yc = control_point(p1, p2)
    return 'M%i,%i Q%i,%i %i,%i' % (x1,y1, xc,yc, x2,y2)

def svg_text(text, center_dim, size):
    p = offset_coord(*center_dim)
    x,y = str(p[0]), str(p[1])
    pt = str(size)
    kw = {'x':x,'y':y,'font-size':pt}
    return xml_wrap('text', text, **kw) 

def svg_spline(point1, point2):
    return xml_wrap('path', None, d=quad_spline(point1, point2))

def all_points(coords):
    "slightly incomplete, clips the splines"
    points = []
    for wbox in all_bboxes(None, coords, None):
        points.append(wbox[:2])
        points.append(wbox[2:])
    return points

def recenter(coords, points):
    "shift everything into quadrant 1"
    xs,ys = zip(*points)
    min_x = min(xs)
    min_y = min(ys)
    for name in coords:
        p = coords[name][0]
        coords[name][0] = p[0]-min_x, p[1]-min_y
    return coords

def window_size(points):
    xs,ys = zip(*points)
    return max(xs)-min(xs), max(ys)-min(ys)

def svgify(packs, coords, toplevel):
    text1,text2,paths = [],[],[]
    bottomlevel = set(packs) - toplevel
    all_ps = all_points(coords)
    coords = recenter(coords, all_ps)
    for pack in bottomlevel:
        size,links = packs[pack]
        cd = coords[pack]
        text1.append(svg_text(pack, cd, size))
    for pack in toplevel:
        size, links = packs[pack]
        cd = coords[pack]
        text2.append(svg_text(pack, cd, size))
    for pack in packs:
        size,links = packs[pack]
        p1 = coords[pack][0]
        for link in [l for l in links if l<pack]:
            p2 = coords[link][0]
            paths.append(svg_spline(p1,p2))
    svg = open('pac3.svg', 'w')
    svg.write('<svg width="%i" height="%i">\n' % window_size(all_ps))
    svg.write('<g style="stroke:#e3e3e3; stroke-opacity:0.15; fill:none;">\n')
    for path in paths:
        svg.write(path+'\n')
    svg.write('</g>\n')
    svg.write('<g font-family="Monospace" fill="#fcfcfc">\n')
    for text in text1:
        svg.write(text+'\n')
    svg.write('</g>\n')
    svg.write('<g font-family="Monospace" fill="#e7fdfc">\n')
    for text in text2:
        svg.write(text+'\n')
    svg.write('</g>\n')
    svg.write('</svg>')
    svg.close()

def call(cmd):
    subprocess.call([cmd], shell=True)

def main():
    print 'Loading package info'
    tree = pac_parse.drawable_tree()
    toplevel = pac_parse.toplevel_packs(tree)
    packs = bidirection(tree)
    print 'Placing all packages'
    coords = place(packs)
    print 'Saving SVG'
    svgify(packs, coords, toplevel)
    print 'Rendering SVG'
    if 'inkscape' in tree:
        call('inkscape -D -b "#666666" -e pac3.png pac3.svg')
        return
    if 'svg2png' in tree:
        call('svg2png pac3.svg pac3.png')
        call('mogrify -background #666666 -layers flatten pac3.png')
        return
    if 'imagemagick' in tree:
        call('convert pac3.svg pac3.png')
        return
    print 'No way to convert SVG to PNG.'
    print 'Inkscape, svg2png or imagemagick would be nice.'

if __name__ == "__main__":
    main()


