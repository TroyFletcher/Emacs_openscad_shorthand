# Emacs_openscad_shorthand
![alt text](https://github.com/TroyFletcher/Emacs_openscad_shorthand/raw/main/emacs_scad.png "shorthand example")
***
Emacs integration of an OpenSCAD shorthand to reduce openscad syntax

Open an empty buffer, type openscad shorthand, and call ```(escad-current-buffer)```

This function will write complete openscad syntax to ~/test.scad.
***

### Example:
```
@ unit = 19.05;
@ u = unit;

@ module base() {
 d
  n
   u u u sbh
   @ stand();
   n 
    t 0 u-pd 0
     @ stand();
    x x x
  t (u-sbd)/2 (u-sbd)/2 -1
   u sbd sbd sbh+10
   t -1 -1 6
    u sbd+2 sbd+2 sbh+4
   x
  x
  t pw/2 -1 (axel_diameter/2)+15
   r -90 0 0
    y axel_diameter u+2
   x x x
x

```
### Generates:
```
unit = 19.05;
u = unit;

module base() {
difference() {
union() {
cube([u,u,sbh]);
stand();
union() {
translate([0,u-pd,0]){
stand();
};};};
translate([(u-sbd)/2,(u-sbd)/2,-1]){
cube([sbd,sbd,sbh+10]);
translate([-1,-1,6]){
cube([sbd+2,sbd+2,sbh+4]);
};
};
translate([pw/2,-1,(axel_diameter/2)+15]){
rotate([-90,0,0]){
cylinder(d=axel_diameter, h=u+2);
};};};
};

```

### NOTE: Whitespace in source buffer is ignored, but recommended
### NOTE: Flet on some versions will throw a warning to use cl-flet, but cl-flet rejects reassignment of t, so ignore it. Reducing syntax and improving feedback is what is important, not adhering to standards. t should be translate!

# SHORTHAND SYNTAX

## @
_openscad syntax passthrough_

_Do not process as escad shorthand_

**EX:** ```@ $fn=20;```

**EX:** ```@ overall_height = 36;```

Trailing space not necessary, but recommended for clarity. Variables, once defined, may be used in place of numerical values or evaluations **without** spaces.

**EX:** ```c 1 (overall_height+10)/5 2```

## \#
_highlight element_

_prepend element with # for highlighting in openscad_

**EX:** #u 1 2 3 = ```#cube([1,2,3]);```

Leading whitespace is not a problem, but there should not be a space
between the hash and the next character

## c

_circle Radius/Diameter value_

**EX:** c r 5 = ```circle(r=5);```

## s

_square X Y (optional) center_

**EX:** s 5 5 t= ```square([5,5], center=true);```

## u

_cUbe width depth height_

**EX:** u 1 2 3 = ```cube([1,2,3]);```

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

## n
_insert uNion element_

**NOTE:** Opens braces, must be closed with x

**EX**: n = ```union() {```

## d
_insert difference element_

**NOTE:** Opens braces, must be closed with x

**EX:** d = ```difference() {```

## h
_insert hull element_

**NOTE:** Opens braces, must be closed with x

**EX:** m = ```hull() {```

## m
_insert minkowski element_

**NOTE:** Opens braces, must be closed with x

**EX:** m = ```minkowski() {```

## re

_rotate angle convexivity_

_insert rotate_extrude element_

**NOTE:** Opens braces, must be closed with x

**EX:** re 360 2  = ```rotate_extrude([360,2]){```

## p

_rotate (points) (paths)_

_insert polygon element_

**EX:** p (0 0 &nbsp; 40 10 &nbsp; 50 30 &nbsp; 30 50 ) = ```polygon(points=[[0,0],[40,10],[50,30],[30,50],]);```

**EX:** p (0 0 &nbsp; 40 10 &nbsp; 50 30 &nbsp; 30 50 ) (0 1 2 3) = ```polygon(points=[[0,0],[40,10],[50,30],[30,50],],paths=[[0, 1, 2, 3]]);```

**NOTE:** paths currently accepts numbers only! See commit ```2b1c762``` for why and how to change it

## x
_Close braces_

Used to close braces previously opened by t, r, u, d

**EX:** x = ```};```


**EX:** x = ```};```

**EX:** x x x = ```};};};```

**NOTE:** x calls may be stacked up to 3 per line MAX! If you have any idea why flet stops recursively calling the function past 3 nestings, let me know!

# This is alpha code!
I am aware this is not the right format for an emacs library, if you want to correct the syntax, please feel free. This is Alpha, and I put it together in a few hours in the middle of the night while tending sick kids. There are probably mistakes!
