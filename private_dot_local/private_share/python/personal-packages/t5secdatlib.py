# T5 Column Delimited format
# https://travellermap.com/doc/fileformats#t5-column-delimited-format

import random


###############################################################################
## Utilities
###############################################################################

def clamp(lo, x, hi):
    return max(lo, min(hi, x))


def roll(n=1):
    return sum(random.randrange(1, 6) for _ in range(n))


###############################################################################
## The `World` class
###############################################################################

class World:
    UWP_FIELDS = {
        "starport_quality": (0, True, [0, 10, 11, 12, 13, 14]),
        "planet_size": (1, False, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
        "atmosphere_type": (2, False, None),
        "hydrographics": (3, False, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
        "population": (4, False, None),
        "government_type": (5, False, None),
        "law_level": (6, False, None),
        # 7 is the dash
        "tech_level": (8, False, None),
    }

    UWP_EXPLAINER = [
        ("starport_quality", {
            "X": "there is no starport",
            "A": "excellent quality, refined fuel for sale, has a shipyard capable of building and repairing jump-capable craft of all sizes",
            "B": "good quality, refined fuel for sale, has a shipyard capable of building non-jump-capable spacecraft and repairing jump-capable spacecraft",
            "C": "routine quality, unrefined fuel for sale, has a shipyard capable of building small craft and repairing jump-capable spacecraft",
            "D": "poor quality, unrefined fuel for sale, has repair facilities for hull damage only",
            "E": "frontier, no fuel or facilities, just a patch of flat ground with a landing beacon",
        }),
        ("planet_size", {
            "0": "diameter under 1000km (eg, an asteroid or orbital complex), negligible surface gravity",
            "1": "diameter ~1600km (eg, Triton), 0.05g surface gravity",
            "2": "diameter ~3200km (eg, Luna, Europa), 0.15g surface gravity",
            "3": "diameter ~4800km (eg, Mercury, Ganymede), 0.25g surface gravity",
            "4": "diameter ~6400km, 0.35g surface gravity",
            "5": "diameter ~8000km (eg, Mars), 0.45g surface gravity",
            "6": "diameter ~9600km, 0.7g surface gravity",
            "7": "diameter ~11,200km, 0.9g surface gravity",
            "8": "diameter ~12,800km (eg, Earth), 1g surface gravity",
            "9": "diameter ~14,400km, 1.25g surface gravity",
            "A": "diameter ~16,000km, 1.4g surface gravity",
        }),
        ("atmosphere_type", {
            "0": "none (eg, Luna), vacc suit required",
            "1": "trace (eg, Mars), vacc suit required",
            "2": "very thin, tainted, respirator with filter required",
            "3": "very thin, respirator required",
            "4": "thin, tainted, filter required",
            "5": "thin, no special equipment required",
            "6": "standard (eg, Earth), no special equipment required",
            "7": "standard, tainted, filter required",
            "8": "dense, no special equipment required",
            "9": "dense, tainted, filter required",
            "A": "exotic, air supply required",
            "B": "corrosive, vacc suit required",
            "C": "insidious, vacc suit required",
            "D": "very dense, no special equipment required (if staying at high altitude)",
            "E": "low, no special equipment required (if staying at low altitude)",
            "F": "unusual, outside UWP classification",
        }),
        ("hydrographics", {
            "0": "0% to 5% surface liquid, a desert world",
            "1": "6% to 15% surface liquid, a dry world",
            "2": "16% to 25%, a few small seas",
            "3": "26% to 35%, small seas and oceans",
            "4": "36% to 45%, a wet world",
            "5": "45% to 55%, large oceans",
            "6": "56% to 65%",
            "7": "66% to 75%, Earth-like",
            "8": "76% to 85%, a water world",
            "9": "86% to 95%, only a few small islands and archipelagos",
            "A": "96% to 100%, almost entirely water",
        }),
        ("population", {
            "0": "unpopulated",
            "1": "tens, a tiny farmstead or a single family",
            "2": "hundreds, a village",
            "3": "thousands",
            "4": "tens of thousands, a small town",
            "5": "hundreds of thousands, an average city",
            "6": "millions",
            "7": "tens of millions, a large city",
            "8": "hundreds of millions",
            "9": "billions, present-day Earth",
            "A": "tens of billions",
            "B": "hundreds of billions, an incredibly crowded world",
            "C": "trillions, a world-city",
            "D": "tens of trillions, a world-city",
            "E": "hundreds of trillions, a world-city",
            "F": "thousands of trillions, a world-city",
        }),
        ("government_type", {
            "0": "none (eg, a family, clan, or anarchy)",
            "1": "company / corporation - ruling functions are performed by a managerial elite, most citizens are employees or dependants (eg, a corporate outpost, asteroid mine, or feudal domain)",
            "2": "participating democracy - ruling functions are reached by the advice and consent of the citizenry directly (eg, a collective, tribal council, or comm-linked consensus)",
            "3": "self-perpetuating oligarchy - ruling functions are performed by a restricted minority, with little or no input from the mass of citizenry (eg, a plutocracy or hereditary ruling class)",
            "4": "representative democracy - ruling functions are performed by elected representatives (eg, a republic or democracy)",
            "5": "feudal technocracy - ruling functions are performed by specific individuals for persons who agree to be ruled by them, relationships are based on the performance of technical activities which are mutually beneficial (those with access to higher technology tend to have higher social status)",
            "6": "captive government - ruling functions are performed by an imposed leadership answerable to an outside group (eg, a colony or conquered area)",
            "7": "balkanisation - no central authority exists, rival government complete for control (eg, civil war)",
            "8": "civil service bureaucracy - ruling functions are performed by government agencies employing individuals selected for their expertise (eg, a technocracy or communism)",
            "9": "impersonal bureaucracy - ruling functions are performed by agencies which have become insulated from the governed citizens (eg, entrenched castes of bureaucrats, or a decaying empire)",
            "A": "charismatic dictator - ruling functions are performed by agencies directed by a single leader who enjoys the overwhelming confidence of the citizens (eg, a revolutionary leader, messiah, or emperor)",
            "B": "non-charismatic leader - a previous charismatic dictator has been replaced by a leader through normal channels (eg, a military dictatorship or hereditary kingship)",
            "C": "charismatic oligarchy - ruling functions are performed by a select group of members of an organisation or class which enjoys the overwhelming confidence of the citizenry (eg, a junta or revolutionary council)",
            "D": "religious dictatorship - ruling functions are performed by a religious organisation without regard to the sspecific individual needs of the citizenry (eg, a cult, transcendent philosophy, or psionic group mind)",
            "E": "religious autocracy - government by a single religious leader having absolute power over the citizenry (eg, a messiah)",
            "F": "totalitarian oligarchy - government by an all-powerful minority which maintains absolute control through widespread coercion and oppression (eg, a world church or ruthless corporation)",
        }),
        ("law_level", {
            "0": "nothing is legally prohibited",
            "1": "poison gas, explosives, undetectable weapons, WMDs, and battle dress are prohibited",
            "2": "poison gas, explosives, undetectable weapons, WMDs, portable energy and laser weapons, and combat armour and battle dress are prohibited",
            "3": "military weapons and flak armour (or better) are prohibited",
            "4": "military weapons, light assault weapons, SMGs, and cloth armour (or better) are prohibited",
            "5": "military weapons, light assault weapons, SMGs, personal concealable weapons and mesh armour (or better) are prohibited",
            "6": "personal concealable weapons, firearms (except shotguns and stunners) and mesh armour (or better) are prohibited",
            "7": "personal concealable weapons, firearms (except stunners) and mesh armour (or better) are prohibited",
            "8": "personal concealable weapons, firearms (including stunners), bladed weapons, and visible armour are prohibited",
            "9": "all weapons and armour are prohibited",
            "A": "all weapons and armour are prohibited",
            "B": "all weapons and armour are prohibited",
            "C": "all weapons and armour are prohibited",
            "D": "all weapons and armour are prohibited",
            "E": "all weapons and armour are prohibited",
            "F": "all weapons and armour are prohibited",
        }),
        ("tech_level", {
            "0": "primitive - stone age, only the simplest tools and principles",
            "1": "primitive - bronze / iron age, science is mostly superstition but basic metalworking is known",
            "2": "primitive - renaissance, the scientific method is known",
            "3": "primitive - early 19th century, steam power and primitive firearms",
            "4": "industrial - late 19th century, plastics, radio, and similar technologies widespread",
            "5": "industrial - mid 20th century, electricity, telecommunications, and internal combustion widespread, some primitive computers",
            "6": "industrial - late 20th century, fission power, advanced computing, and the dawn of the space age",
            "7": "pre-stellar - modern-day Earth, computers widespread, can reach orbit reliably, and has telecommunications satellites",
            "8": "pre-stellar - fusion power viable, can travel to other worlds in the same system and build permanent space habitats",
            "9": "pre-stellar - gravitic technology discovered",
            "A": "early stellar - jump-1 technology, intestellar travel and trade viable",
            "B": "early stellar - jump-2 technology, computers powerful enough to create true AI",
            "C": "average stellar - jump-3 technology, weather control, man-portable plasma weapons, vehicle-portable fusion weapons",
            "D": "average stellar - jump-4 technology, easy cloning of body parts, battle-dress, spacecraft can go underwater",
            "E": "average stellar - jump-5 technology, flying cities, man-portable fusion weapons",
            "F": "high stellar - jump-6 technology, synthetic anagathics, black globe generators",
        }),
    ]

    MECHANICAL_NOTES = [
        ("High Gravity", lambda w: w.planet_size == 10, "DM-1 to all skill checks until acclimatisation after 1D weeks (automatic with Athletics (strength))"),
        ("Low Gravity", lambda w: w.planet_size <= 6, "DM-1 to physical skill checks until acclimatisation after 1D weeks (automatic with Athletics (dexterity))"),
        ("No Gravity", lambda w: w.planet_size == 0, "using a weapon without the Zero-G trait requires an Athletics (dexterity) check to avoid spinning out of control (regain control with a further Athletics (dexterity) check)"),
        ("Tainted Atmosphere", lambda w: w.atmosphere_type in [2, 4, 7, 9], "breathing causes 1D damage at regular intervals"),
        ("Corrosive Atmosphere", lambda w: w.atmosphere_type == 11, "breathing causes 1D damage each round"),
        ("Insidious Atmosphere", lambda w: w.atmosphere_type == 12, "standard-quality protection degrades after 2D hours"),
        ("Vacuum", lambda w: w.atmosphere_type == 0, "unprotected exposure to vacuum causes a cumulative 1D damage each round and 2D*10 rads each round")
    ]

    TRADE_CODES = [
        ("Agricultural", "Ag", "dedicated to farming and food production, often divided into vast semi-feudal estates", [
            ("atmosphere_type", lambda x: x >= 4 and x <= 9),
            ("hydrographics", lambda x: x >= 4 and x <= 8),
            ("population", lambda x: x in [5, 6, 7])
        ]),
        ("Asteroid", "As", "usually mining colonies, but can also be orbital factories or colonies", [
            ("planet_size", lambda x: x == 0),
            ("atmosphere_type", lambda x: x == 0),
            ("hydrographics", lambda x: x == 0)
        ]),
        ("Barren", "Ba", "uncolonised and empty", [
            ("population", lambda x: x == 0),
            ("government_type", lambda x: x == 0),
            ("law_level", lambda x: x == 0)
        ]),
        ("Desert", "De", "dry and barely habitable", [
            ("atmosphere_type", lambda x: x >= 2),
            ("hydrographics", lambda x: x == 0)
        ]),
        ("Fluid Oceans", "Fl", "surface liquid is something other than water, and so is incompatible with Earth-derived life", [
            ("atmosphere_type", lambda x: x >= 10),
            ("hydrographics", lambda x: x >= 1)
        ]),
        ("Garden", "Ga", "Earth-like", [
            ("planet_size", lambda x: x in [6, 7, 8]),
            ("atmosphere_type", lambda x: x in [5, 6, 8]),
            ("hydrographics", lambda x: x in [5, 6, 7])
        ]),
        ("High Population", "Hi", "a population in the billions or more", [
            ("population", lambda x: x >= 9)
        ]),
        ("High Tech", "Ht", "among the most technologically advanced in Charted Space", [
            ("tech_level", lambda x: x >= 12)
        ]),
        ("Ice-Capped", "Ic", "cold and ry, most of the surface liquid is frozen in polar ice caps", [
            ("atmosphere_type", lambda x: x <= 1),
            ("hydrographics", lambda x: x >= 1)
        ]),
        ("Industrial", "In", "dominated by factories and cities", [
            ("atmosphere_type", lambda x: x in [0, 1, 2, 4, 7, 9]),
            ("population", lambda x: x >= 9)
        ]),
        ("Low Population", "Lo", "a population of only a few thousand or less", [
            ("population", lambda x: x <= 3)
        ]),
        ("Low Tech", "Lt", "pre-industrial and cannot produce advanced goods", [
            ("tech_level", lambda x: x <= 5)
        ]),
        ("Non-Agricultural", "Na", "too dry or barren to support the population using conventional food production", [
            ("atmosphere_type", lambda x: x <= 3),
            ("hydrographics", lambda x: x <= 3),
            ("population", lambda x: x >= 6)
        ]),
        ("Non-Industrial", "Ni", "too low in population to maintain an extensive industrial base", [
            ("population", lambda x: x <= 6)
        ]),
        ("Poor", "Po", "lacking resources, viable land, or sufficient population to be anything other than a marginal colony", [
            ("atmosphere_type", lambda x: x >= 2 and x <= 5),
            ("hydrographics", lambda x: x <= 3)
        ]),
        ("Rich", "Ri", "blessed with a stable government and viable biosphere, making it an economic powerhouse", [
            ("atmosphere_type", lambda x: x in [6, 8]),
            ("population", lambda x: x in [6, 7, 8]),
            ("government_type", lambda x: x >= 4 and x <= 9)
        ]),
        ("Vacuum", "Va", "no atmosphere", [
            ("atmosphere_type", lambda x: x == 0)
        ]),
        ("Water World", "Wa", "almost entirely water-ocean across the surface", [
            ("hydrographics", lambda x: x >= 10)
        ])
    ]

    def __init__(self, row):
        self.__row = {
            "hex": "????",
            "name": "New World",
            "uwp": "X000000-0",
            "remarks": "",
            "{ix}": "",
            "(ex)": "",
            "[cx]": "",
            "n": "-",
            "b": "-",
            "z": "-",
            "pbg": "000",
            "w": "0",
            "a": "--",
            "stellar": ""
        }
        self.__row.update(row)

    def __getattr__(self, name):
        if name in self.UWP_FIELDS:
            return self.__uwp_get(self.UWP_FIELDS[name])
        elif name in self.__row:
            return self.__row[name]
        else:
            raise AttributeError

    def __setattr__(self, name, value):
        if name == "_World__row":
            super().__setattr__(name, value)
        elif name in self.UWP_FIELDS:
            self.__uwp_set(self.UWP_FIELDS[name], value)
        elif name in self.__row:
            self.__row[name] = value
        else:
            super().__setattr__(name, value)

    def __str__(self):
        return f"{self.hex} {self.name} {self.uwp}"

    def __repr__(self):
        return f"'{self}'"

    def __uwp_get(self, field_def, hex=False):
        pos, _, _ = field_def
        value = self.__row["uwp"][pos]
        if hex:
            return value

        if value == "?":
            return -1
        elif value == "X":
            return 0
        else:
            return int(value, 16)

    def __uwp_set(self, field_def, value):
        pos, zero_is_x, whitelist = field_def

        if whitelist is not None and value not in whitelist and value != -1:
            raise ValueError

        if value == -1:
            hex_value = "?"
        elif zero_is_x and value == 0:
            hex_value = "X"
        else:
            hex_value = f"{value:X}"

        self.__row["uwp"] = self.__row["uwp"][:pos] + hex_value + self.__row["uwp"][pos+1:]

    def trade_codes(self, short=True, full=False):
        def check(criterion):
            field, condition = criterion
            value = getattr(self, field)
            return condition(value)

        trade_codes = []
        for (name, abbrev, explanation, criteria) in self.TRADE_CODES:
            if all(map(check, criteria)):
                if full:
                    trade_codes.append((name, abbrev, explanation))
                elif short:
                    trade_codes.append(abbrev)
                else:
                    trade_codes.append(name)
        return trade_codes

    def explain(self, allegiance_names=None, html=False, html_h=3):
        to_header = lambda s: f"<h{html_h}>{s}</h{html_h}>" if html else f"\n## {s}"
        to_line = lambda k, v: f"<b>{k}:</b> {v}" if html else f"{k}: {v}"

        uwp_lines = []
        for field, explainer in self.UWP_EXPLAINER:
            name = " ".join(field.split("_")).title()
            hex_value = self.__uwp_get(self.UWP_FIELDS[field], hex=True)
            explanation = "unknown" if hex_value == "?" else explainer[hex_value]
            uwp_lines.append(to_line(f"{name} ({hex_value})", explanation))

        trade_code_lines = []
        for name, abbrev, explanation in self.trade_codes(full=True):
            trade_code_lines.append(to_line(f"{name} ({abbrev})", explanation))

        remark_lines = []
        if self.a != "--":
            if allegiance_names is None:
                remark_lines.append("Part of an interstellar polity")
            else:
                remark_lines.append(f"Part of the {allegiance_names[self.a]}")
        if "pbg" in self.__row and self.__row["pbg"][2] not in ["?", "0"]:
            remark_lines.append("Has a gas giant suitable for fuel skimming")

        mechanical_note_lines = []
        for name, criterion, explanation in self.MECHANICAL_NOTES:
            if criterion(self):
                mechanical_note_lines.append(to_line(name, explanation))

        out = []
        for (header, lines) in [(None, uwp_lines), ("Trade Codes", trade_code_lines), ("Remarks", remark_lines), ("Mechanical Notes", mechanical_note_lines)]:
            if lines:
                if header is not None:
                    out.append(to_header(header))
                if html:
                    out.append("<ul>")
                out.extend([(f"<li>{l}</li>" if html else l) for l in lines])
                if html:
                    out.append("</ul>")

        return "\n".join(out)



###############################################################################
## World generation
###############################################################################

def roll_planet_size():
    return roll(2) - 2

def roll_atmosphere_type(planet_size):
    return clamp(0, roll(2) - 7 + planet_size, 15)

def roll_temperature(atmosphere_type, hot=False, cold=False):
    """Temperature is not reflected in the UWP, but can influence hydrographics.
    """

    res = roll(2)
    if atmosphere_type in [2, 3]:
        res -= 2
    elif atmosphere_type in [4, 5, 14]:
        res -= 1
    elif atmosphere_type in [8, 9]:
        res += 1
    elif atmosphere_type in [10, 13, 15]:
        res += 2
    elif atmosphere_type in [11, 12]:
        res += 6

    if hot:
        res += 4
    if cold:
        res -= 4

    return clamp(0, res, 15)

def roll_hydrographics(planet_size, atmosphere_type, temperature=7):
    if planet_size in [0, 1]:
        return 0

    res = roll(2) - 7
    if atmosphere_type in [0, 1, 10, 11, 12]:
        res -= 4

    if temperature in [10, 11]:
        res -= 2
    elif temperature >= 12:
        res -= 6

    return clamp(0, res, 15)

def roll_population():
    return roll(2) - 2

def roll_government_type(population):
    return clamp(0, roll(2) - 7 + population, 12)

def roll_law_level(government_type):
    return clamp(0, roll(2) - 7, 15)

def roll_starport_quality(population):
    res = roll(2)
    if population in [8, 9]:
        res += 1
    elif population >= 10:
        res += 2
    elif population in [3, 4]:
        res -= 1
    elif population <= 2:
        res -= 2

    if res <= 2:
        return 0
    elif res in [3, 4]:
        return 14
    elif res in [5, 6]:
        return 13
    elif res in [7, 8]:
        return 12
    elif res in [9, 10]:
        return 11
    else:
        return 10

def roll_tech_level(planet_size, atmosphere_type, hydrographics, population, government_type, starport_quality):
    res = roll()

    if starport_quality == 10:
        res += 6
    elif starport_quality == 11:
        res += 4
    elif starport_quality == 12:
        res += 2
    elif starport_quality == 0:
        res -= 4

    if planet_size in [0, 1]:
        res += 2
    elif planet_size in [2, 3, 4]:
        res += 1

    if atmosphere_type in [0, 1, 2, 3, 10, 11, 12, 13, 14, 15]:
        res += 1

    if hydrographics in [0, 9]:
        res += 1
    elif hydrographics == 10:
        res += 2

    if population in [1, 2, 3, 4, 5, 8]:
        res += 1
    elif population == 9:
        res += 2
    elif population == 10:
        res += 4

    if government_type in [0, 5]:
        res += 1
    elif government_type == 7:
        res += 2
    elif government_type in [13, 14]:
        res -= 2

    return clamp(0, res, 15)

def roll_world(hot=False, cold=False):
    w = World({})

    w.planet_size = roll_planet_size()
    w.atmosphere_type = roll_atmosphere_type(w.planet_size)
    # temperature is not in the UWP
    temperature = roll_temperature(w.atmosphere_type, hot=hot, cold=cold)
    w.hydrographics = roll_hydrographics(w.planet_size, w.atmosphere_type, temperature=temperature)
    w.population = roll_population()
    w.government_type = roll_government_type(w.population)
    w.law_level = roll_law_level(w.government_type)
    w.starport_quality = roll_starport_quality(w.population)
    w.tech_level = roll_tech_level(w.planet_size, w.atmosphere_type, w.hydrographics, w.population, w.government_type, w.starport_quality)

    return w


###############################################################################
## Sector data file parser
###############################################################################

def parse_world_data_from_lines(lines):
    field_widths = None
    skip = False
    worlds = {}
    for line in lines:
        if skip:
            skip = False
            continue
        if field_widths is None:
            name = ""
            in_padding = False
            width = 0
            field_widths = []
            for c in line:
                if c == "\n":
                    # end of line
                    field_widths.append((name.lower(), width))
                elif c == " ":
                    # field length padding
                    width += 1
                    in_padding = True
                elif name and in_padding:
                    # start of a new field
                    field_widths.append((name.lower(), width - 1))
                    name = c
                    width = 1
                    in_padding = False
                else:
                    # name of current field
                    name += c
                    width += 1
            # skip divider line
            skip = True
        else:
            row = {}
            for (field, width) in field_widths:
                row[field] = line[:width].strip()
                line = line[width+1:]
            worlds[row["hex"]] = World(row)
    return worlds

def parse_world_data_from_file(filename):
    with open(filename, "r") as f:
        return parse_world_data_from_lines(f.readlines())
