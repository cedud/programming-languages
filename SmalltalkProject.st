Object subclass: #Polygon
	instanceVariableNames: 'vertex id'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ProgrammingLanguages'
!

!Polygon methodsFor: 'initialize-release'!
    initialize: vertexCount name: new_name
        "Object Constructor - Polygon"
        id := new_name.
        vertex := Array new: vertexCount.
        vertex at: 1 put: 0@0.
    !
!

!Polygon methodsFor: 'accessing'!
    id
        "Gives the polygon name"
        ^id
    !
    id: new_name
        "Sets a new name for polygon"
        id := new_name
    !
!

!Polygon methodsFor: 'actions'!
    print: msg
        "Shows info about polygon"
        Transcript cr; show: msg ; cr.
        vertex keysAndValuesDo: [ :index :ver |
            Transcript show: 'Vertex ', index asString , ':', Character tab asString, (ver x roundTo:0.0001) printString, ' @ ', (ver y roundTo:0.0001) printString; cr
        ].
        Transcript show: 'Area: ', (self area roundTo:0.0001) printString; cr.
    !
    
    
    rotate: angle
    | radians cosine sine newPoint punkt newX newY |
        "Rotates polygon by given angle"
     angle <= 0 ifTrue: [
     Transcript show: 'Degree is less than 0!'; cr.
     ^'Exiting rotation'.
     ].
     angle > 90 ifTrue: [
     Transcript show: 'Degree is more than 90!'; cr.
     ^'Exiting rotation'.
     ].
    
    radians := (Float pi * 2) - (angle degreesToRadians).
    cosine := radians cos.
    sine := radians sin.
    2 to: vertex size do: [:i |
        punkt := vertex at: i.
        newX := (punkt x * cosine) - (punkt y * sine).
        newY := (punkt x * sine) + (punkt y * cosine).
        newPoint := newX @ newY.
        vertex at: i put: newPoint.
    ].
    self print: 'Rotated by: ', angle asString , ' degrees'.
    !
!


Polygon subclass: #Square
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ProgrammingLanguages'
!

!Square methodsFor: 'arithmetic'!
    + shape
        "Adds 2 shapes in terms of area and creates a new Square of that size"
        | p |

        p := self area + shape area.
        ^(Square new) initialize: p sqrt
    !
!

!Square methodsFor: 'actions'!
    area
        "Calculate square area"
        
        ^((((vertex at: 3) x squared + (vertex at: 3) y squared) sqrt) / (2 sqrt)) squared
    !
!

!Square methodsFor: 'modifiers'!
    scaling: s
        | side |
        
        side := (vertex at: 2) x.
        self print: 'Square scaled by: ', s asString .
        ^(Square new) initialize: side * s
    !
!

!Square methodsFor: 'initialize-release'!
    initialize: side
        "Creates a square with a given size"

        super initialize: 4 name: 'Square'.
        vertex at: 2 put: side@0.
        vertex at: 3 put: side@side.
        vertex at: 4 put: 0@side.
    !
!


Polygon subclass: #IsoscelesTriangle
    instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'ProgrammingLanguages'
!

!IsoscelesTriangle methodsFor: 'initialize-release'!
    initialize: base height: H
        "Creates an Isosceles Triangle"
        super initialize: 3 name: 'IsoscelesTriangle'.
        vertex at: 2 put: base@0.
        vertex at: 3 put: (base/2)@H.
    !
!

!IsoscelesTriangle methodsFor: 'arithmetic'!
    + shape
        | newArea oldBase oldHeight areaScale |
        oldBase := (vertex at: 2) x.
        oldHeight := (vertex at: 3) y.
        newArea := self area + shape area.
        areaScale := (newArea / self area) sqrt.
        
        ^(IsoscelesTriangle new) initialize: (oldBase * areaScale) height: (oldHeight * areaScale)
    !
!

!IsoscelesTriangle methodsFor: 'actions'!
    area
        | newArea |
        "Calculates area of Isosceles Triangle"
        newArea :=((((vertex at: 2) x - (vertex at: 1) x) * ((vertex at: 3) y - (vertex at: 1) y)) - (((vertex at: 3) x - (vertex at: 1) x) * ((vertex at: 2) y - (vertex at: 1) y))) abs  / 2.
        ^ newArea
    !
!

!IsoscelesTriangle methodsFor: 'modifiers'!
    scaling: scale
        "Scales the polygon sides according to given scale"

        | base H |
        base := (vertex at: 2) x.
        H := (vertex at: 3) y.
        self print: 'Triangle scaled by: ', scale asString .
        ^(IsoscelesTriangle new) initialize: base * scale height: H * scale
    !
    
    
!


"Tests"

k := (Square new) initialize: 3.
t := (IsoscelesTriangle new) initialize: 2 height: 2.

Transcript show: 'Given polygons:'; cr.
k print: 'Square'.
t print: 'Triangle'.

k rotate: 45.
Transcript cr; show: 't + k:'; cr.
t1 := t + k.
t1 rotate: 45.

Transcript cr; show: 'k + t:'.
t3 := k + t .
t3 print: ''.

k2 := k scaling: 0.5.
t2 := t scaling: 4.