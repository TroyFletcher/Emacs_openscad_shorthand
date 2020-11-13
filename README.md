# Emacs_openscad_shorthand
![alt text](https://github.com/TroyFletcher/Emacs_openscad_shorthand/raw/main/emacs_scad.png "shorthand example")
***
Emacs integration of an OpenSCAD shorthand to reduce openscad syntax

Open an empty buffer, type openscad shorthand, and call ```(escad-current-buffer)```

This function will write complete openscad syntax to ~/test.scad.
***

### Example:
```
@ $fn=20;
y 4 2
t 2 4 0
 r 90 0 0
  #c 2 2 2
 x
x

```
### Generates:
```
$fn=20;
cylinder(d=4, h=2);
translate([2,4,0]){
rotate([90,0,0]){
#cube([2,2,2]);
};
};
```

### NOTE: Whitespace in source buffer is ignored, but recommended

# SHORTHAND SYNTAX

## @
_openscad syntax passthrough_

_Do not process as escad shorthand_

**EX:** ```@ $fn=20;```

**EX:** ```@ overall_height = 36;```

Trailing space not necessary, but recommended for clarity. Variables, once defined, may be used in place of numerical values or evaluations **without** spaces.

**EX:** ```c 1 overall_height+10 2```

## \#
_highlight element_

_prepend element with # for highlighting in openscad_

**EX:** #c 1 2 3 = ```#cube([1,2,3]);```

Leading whitespace is not a problem, but there should not be a space
between the hash and the next character

## c

_cube width depth height_

**EX:** c 1 2 3 = ```cube([1,2,3]);```

## y
_cylinder diameter height_

**EX:** y 4 2 = ```cylinder(d=4, h=2);```

## t
_translate X Y Z_

**NOTE:** Opens braces, must be closed with x

**EX:** t 1 2 3 = ```translate([2,4,0]){```

## r
_rotate X Y Z_

NOTE: Opens braces, must be closed with x

**EX:** r 90 180 0 = ```rotate([90,180,0]){```

## u
_insert union element_

**NOTE:** Opens braces, must be closed with x

**EX**: u = ```union() {```

## d
_insert difference element_

**NOTE:** Opens braces, must be closed with x

**EX:** d = ```difference() {```

## x
_Close braces_

Used to close braces previously opened by t, r, u, d

**EX:** x = ```};```


