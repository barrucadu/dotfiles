hc attr theme.tiling.reset   1
hc attr theme.floating.reset 1

### Frame backgrounds
hc set frame_bg_transparent 1

### Frame Borders
hc set frame_border_active_color "#222222"
hc set frame_border_normal_color "#101010"

### Tile Borders
active_colour="#9fbc00"
normal_colour="#454545"
urgent_colour="orange"

# Tiling
hc set_attr theme.tiling.active.border_width 2
hc set_attr theme.tiling.active.color        "$active_colour"

hc set_attr theme.tiling.normal.border_width 2
hc set_attr theme.tiling.normal.color        "$normal_colour"

hc set_attr theme.tiling.urgent.border_width 2
hc set_attr theme.tiling.urgent.color        "$urgent_colour"

### Floating window borders
hc set_attr theme.floating.active.border_width 7
hc set_attr theme.floating.active.color        "$active_colour"

hc set_attr theme.floating.normal.border_width 1
hc set_attr theme.floating.normal.color        "$normal_colour"

hc set_attr theme.floating.urgent.border_width 3
hc set_attr theme.floating.urgent.color        "$urgent_colour"

### Padding 
hc set frame_gap     4
hc set window_gap    0
hc set frame_padding 0

# Remove borders and gaps when there is no ambiguity
hc set smart_window_surroundings 0
hc set smart_frame_surroundings  1

# Don't show borders unless focus or nonempty
hc set always_show_frame 0
