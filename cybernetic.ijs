NB. routines from Cybernetic Music(CM). This is a semi-direct translation 
NB. from APL. Where J has obvious idioms that simplify the code
NB. I have made the changes. 
NB. Ideally this code should be reworked and put in tacit form 
NB. where possible.

NB. Note about the APL code from the book Cybernetic Music(CM) by Jaxitron
NB. CM tries to keep it's vectors (arrays) as 2-dimensional with usually 
NB. only 1 row specified. Not sure why that is I haven't studied the 
NB. book enough to see if there is a use case for 2 or more rows of 
NB. notes. APL and J seem to prefer vectors that are specified without
NB. a 2-dimenional rank. CM seems to go through a lot of reshaping to 
NB. maintain that single row 2-dimensional structure. I have 
NB. tried to take a more traditional J approach where it was easy to
NB. do so. That means using vectors(arrays) that return only their
NB. length when their dimensions are checked with the '$' operation
NB. this is the J equivalent to the 'rho' operator in APL.
NB. over time this could should be refactored to using traditional
NB. J dimensioning where appropriate and functionality is not 
NB. disturbed.

require 'files'

NB. Global variables
TONSYS =: 12
NOTES =: i. TONSYS
PIANO =: 9 + i.88
NOTENAME =: 'C  C# D  D# E  F  F# G  G# A  Bb B  '
FORS =: 'b#'
NTS =: 'CDEFGAB'
NUM =: 0 2 4 5 7 9 11
DGTS =: '0 123456789._'
CLSNDX =: 0 0 1 1 1 2 2 3 3 3 3 4 4 4 5 5 5 6 6 6 6 6 7 7 7 8 8 8 8 9 9 9 10 11 11 11
CLSNM =: 12 6$'-MI7  ','-MA7  ','MI7   ','MI    ','+MI   ','LG7   ','MA7   ','+7    ','+MA7  ','7+3   ','MA7+3 ','+MA7+3'

NB. Formatting functions
   fmt =: 8!:0
   fmt1=: 8!:1
   fmt2=: 8!:2

NB. some music theory
NB. 	Intervals		Names
NB. 0   perfect unison		(diminished second)
NB. 1	minor second		(augmented unison)
NB. 2	major second		(diminished third)
NB. 3	minor third		(augmented second)
NB. 4 	major third		(diminished fourth)
NB. 5 	perfect fourth		(augmented third)
NB. 6	augmented fourth or diminished fifth
NB. 7	perfect fifth		(diminished sixth)
NB. 8	minor sixth		(augmented fifth)
NB. 9	major sixth		(diminished seventh)
NB. 10	minor seventh		(augmented sixth)
NB. 11	major seventh		(diminished octave)
NB. 12	perfect octave		(augmented seventh)

   int =: 3 : 0
(1 }. y) - _1 }. y
)

NB. Found in Chapter 7 on page 54 and transcribed here
NB. resultant creates the Schillinger interference pattern for 
NB. rhythm
resultant =: 3 : 0
z =. (($y),*/y)$0
for_i. y do.
z =. ((*/y)$1,(_1+i)$0) i_index} z
end.
r=. int (+/\ +. /z)i.1 + i. 1 + +/ +./ z
attk =. +/z
r,: (I. (0 ~: attk)) { attk
)

NB. Rhythm function missing page 56

NB. scli - allows the composer think in terms of single
NB. sets of intervals as opposed to sclit
scli =: 3 : 0
j =: (($y)*>.(5*TONSYS)%+/y)$y
scl =: (|. +/\0, -|. j), 1}.+/\0,j
scl i. 0
)

showscl =: 4 : 0
mn =. x * TONSYS
mx =. y * TONSYS
((scl >: mn) *. scl <: mx)# scl
)

brk =: 4 : 0
j =. (0 ~: x)#,x
k =. +/j
z =. ((<.y%k)*$,x)$ x
r =. k | y
L0 =. 1
while. L0 do.
k =. ((+/\j)<:r)#j
r =. r - +/k
z =. z,k
if. r = 0 do. z return. end.
if. (r = 0 { j)+. r = 1 do.
  L0 =. 0
  continue.
end.
j =. }. j
if. 0 < $j do.
  continue.
end.
L0 =. 0
end.
r =. r$1
z,r
)

NB. sclit - aloows the composer to create related patterns in 
NB. different scales
sclit =: 4 : 0
j =. y brk x
scli j
)

NB. symscl - 
symscl =: 4 : 0
z =: +/\0,(_1 + >./ frctn (TONSYS|x) %TONSYS)$x
,+/\"1 z,.(($z),$y)$y
)

NB. frctn - provides numerator and denominator of a decimal fraction
NB. In the APL code this is used to determine a reduced fraction
NB. involving division by TONSYS
NB. as such to determine a simplified fraction the original denominator
NB. is supplied and you loop through values less than or equal to
NB. the original denominator to calculate a simplified fraction
NB. below is a J looping pseudocode of the original APL
NB. frctn =: 4 : 0
NB. i =. 1
NB. whilest. x >: i do.
NB. a =. (i$0), i%(i-1) }. 1+ i. x
NB. z =. a i. y
NB. if. z <: x do.
NB. break.
NB. end.
NB. end.
NB. i,z
NB. )

NB. frctn - j version
NB. don't need a denominator value in J frctn can be monadic
NB. this uses the extended precision didatic function x:
NB. which will return numerator and denominator in reduced form
NB. as a vector
frctn =: 2&x: 

NB. scale =: 0 2 4 5 7 9 11
NB. we can extract chords from this scale by the following
NB. (7|(i.7)  +/ 2 * i.3) { scale
NB. this will create the seven chords of the given scale
NB. more general if 
NB. N is the size of the scale
NB. E is expand the chord by E number of tones
NB. S is the number of notes in the chords
NB. xpnd - expand scale into its chords
xpnd =: 4 : 0
N =. #x
'E S' =. y
z =. (N|(i. N) +/ E * i.S) { x
)
   NB. SIGMAS are just all combinations of intervals which have
   NB. distinct notes
   NB.    12|0,+/\3 3 3  which maps 4 notes interval 3 apart 
   NB. into 12 note scale has the following ordinal note numbers
   NB. 0 3 6 9
   NB. However 12|0,+/\4 4 4 will loop back around and repeat the
   NB. same note
   NB. 0 4 8 0
   NB. The goal therefore seeems to be to go through all combinations
   NB. of intervals taken together and output a set of unique
   NB. note sequences. The choice of too many intervals increases
   NB. the search space dramatically but for 2 or 3 intervals it
   NB. is quite doable

NB. sigmas support function rpts finds duplicate ordinal tone seq
rpts =: #/.~

NB. i2np will map all notes into a single octave
i2np =: 3 : 'TONSYS|+/\0,y'

jsigmas =: 4 : 0
cnotes =. x
NB. interverals =. cnotes - 1
NB. intervals =. ((#y) #.^:_1 (i. (#y)^cnotes-1))){y
intervalsel =. (#y) #.^:_1 (i. (#y)^cnotes-1)
intervals =. intervalsel { y
allchords =. TONSYS| +/\"1 (0,. intervals) 
selection =. (1 =>./"1 rpts"1 allchords)
(selection#intervals);(selection#allchords);n2pno (selection#allchords)
)
 
sigmaout =: '/Users/tmcguire/musicXML/sigma.out'
sigmas =: 4 : 0
S =. x
A =. |: 0 1$y
for_j. i. 2 + TONSYS - 2 do.
Z =: (2 1 0 |:(1,(1{.$A),$y)$y),(($y),$A)$A
Z =: (, $~ | -.~ $)Z
k =: TONSYS|+/\Z
K =: (TONSYS*k=0)+k
NB. recalculate Z in sonme obtuse way
NB. z=:(+./"0 1>:+/(1+i. TONSYS)=/k)#"0 Z
z =: (+./"0 1>:+/(1+i. TONSYS)=/K)#"0 Z
if. -. j e. S do.
  continue.
end.
NB. This is a chord of interest so prepare results
(40$'*') fappends sigmaout
((": 1 {. $ z),' SIGMA ',(": j),'''S') fappends sigmaout
(":z) fappends sigmaout

if. j >: >./S do.
  return.
end.

NB. Set A to z and recalculate for next interval
A =. z
end.
)

NB. CHORD CONSTRUCTION ROUTINES
NB. support structures
HS =: 4 2$4 3 3 4 4 4 5 5
PREF =: 3 3$ 0 1 _4 1 1 _4 1 0 2
VS =: 3 3$ 1 2 0 2 0 1 2 1 0

NB. shcon
shcon =: 4 : 0
'IRC STR' =. y
S0 =. 0 { STR
RT =. 0 { IRC
i =. 1
Z =. +/\ RT,S0 { HS
Z =. (1,$Z)$Z
SN =. i { STR
cyc =. {. (+./((i.2){"1 PREF))=(({. $PREF),2)$ S0,SN)#i. {.$PREF)
)

NB. cshcon - construct cords based on cycle vector that specifies
NB.          intervals between successive root tones
cshcon =: 4 : 0
k =. */($,y),$,x
z=. i2np (k$y) { HS
n2pno z+|:(|.$z)$ }. + /\0,k$x
)

NB. N2P - convert numeric representation of notes to letter form
NB. this is a direct translation of the APL Cybernetic Music routine
NB. into J
N2P =: 3 : 0
NB. if. 0 = $,y do.
NB.  return.
NB. end.
n=. (_2 {. 1, 1, $y)$y
kn =. TONSYS|n
kd =. <. n%TONSYS
o =. ((_1}.$n),1){.kd
kd =. kd -(o,. _1 }."1 kd)
kf =. (, $~ 1 -.~ $) kn -/ NUM
kf =. (kf*(|kf)=(<./"1|kf) */ (#NUM)$1)
kf =. kf,.TONSYS
kf =. (1 = +/\"1 +/\"1 kf ~: 0)*kf
kf =. (_1 }. $kf)$(,kf~:0)#,kf
kf =. (1,$kf)$(kf ~: TONSYS)*kf
kn =. (NUM i. kn - kf) { NTS
flds =. 4 + (>./ (,|kf))
orho =. _1 {.$kn
kn =. ((_1}.$kn),orho*flds)$,(,kn),.(($,kn),_1+flds)$' '
(kn;kf;kd;flds; orho) N2POUT 1
)

NB. N2POUT - finishing routine that uses the Local scoping rules in
NB. J. The calling routine sets up the values of kf, kn, kd, flds,
NB. orho etc 
N2POUT =: 4 : 0
'kn kf kd flds orho' =. x
i=.0
if. flds ~: 4 do.
whilst. (flds-4)> i do.
 tcol =. ,((kf=0)+2*kf >: 1){(1 0 1 expansion FORS)
 kn =. tcol (,i+1+flds* i. orho)} ,kn
 kf =. (*kf)*(kf~:0)* _1+|kf
 i =. i + 1
end.
end.
kn =. ' ',kn
if. y = 0 do.
 return.
end.
NB. More processing if y=1
kn =. ((,|kd){(1}.DGTS)) (,flds* i. _1{. $kd)}kn
kn =. ((1+* 1}.,kd){('- +')) (,_1+flds* 1}.i. _1{.$,kd)}kn
kn =. (":0),':',(' ',kn)
)

N2PJ =: 3 : 0
NB. if. 0 = $,y do.
NB.  return.
NB. end.
n=. y
kn =. TONSYS|n
kd =. <. n%TONSYS
o =. 1{.kd
kd =. kd - (o, _1 }. kd)
kf =. (, $~ 1 -.~ $) kn -/ NUM
kf =. (kf*(|kf)=(<./"1|kf) */ (#NUM)$1)
kf =. kf,.TONSYS
kf =. (1 = +/\"1 +/\"1 kf ~: 0)*kf
kf =. (,kf~:0)#,kf
kf =. (kf ~: TONSYS)*kf
kn =. (NUM i. kn - kf) { NTS
flds =. 4 + (>./ (,|kf))
orho =. _1 {.$kn
kn =. (orho*flds)$,(,kn),.(($,kn),_1+flds)$' '
(kn;kf;kd;flds; orho) N2POUT 1
)

n2pno =: 3 : '(1 3*$y)$,(TONSYS|y) { (TONSYS,3) $ NOTENAME'


NB. ROCT - relative octave processing. The goal is to move down
NB. a list of note numbers and record upticks and down tics in 
NB. octave.
NB. x is the starting relative octave (usually 0)
NB. y is the array of notes
ROCT =: 4 : 0
aoct =. <.y%TONSYS
smoutput aoct
loct =. x
oct =. 1$loct
for_i. aoct do.
  delta =. i - loct
  oct =. oct, i - loct 
  if. delta ~: 0 do.
    loct =. i
  end.
end.
oct
)

NB. ROCT1 - redesign of octave processing based on the methodology
NB. in Cybernetic Music. That process is a shift right of absolute
NB. octave calculation with reinsertion of the original starting
NB. octave.
ROCT1 =: 3 : 0
lkd =. <.y%TONSYS
lkd - _1 |.!.({.lkd) lkd
)

NB. n2p - in J using boxing to generate and display pitch names
NB.       this makes it much easier to manipulate than the APL
NB.       routines originally developed
n2p =: 3 : 0
NB. Octave would be the relative octave away from main octave
NB. The octave is found by dividing by the # of tones in an octave
NB. ie. divide by TONSYS and then using floor <. to produce the 
NB. integer truncation. Then use format ": to turn it all into
NB. Text
NB. using ROCT as the function the call would be:
NB. octave =. ": each ;/ }. 0 ROCT y
NB. the above is not quite correct as it assumes a starting octave
NB. of zero which may not be the case in all note sequences

NB. the preferred usage is more characteritic of how the relative
NB. octaves are calculated in Cybernetic Music's APL code. 
NB. The use of a function instead of inline J code was chosen to 
NB. isolate the code for development and testing purposes
octave =. ": each ;/ ROCT1 y

NB. The following will map a note to it's text pitch name
NB. Cybernetic Music calculates sharps and flats and adds them to 
NB. the base tones of 'CDEFGAB' here 3 character note names are used
NB. with sharps and flats already included and based on the 
NB. traditional rendering for a C-scale. Certain key signatures may have
NB. a different traditional rendering and in the future different 
NB. NOTENAME variable would be need to be used
pitch =. (TONSYS|y) { _3 <\NOTENAME
(('0';' ')&stringreplace each octave),:pitch
)

NB. What would be nice would be an XML language that could be 
NB. loaded into a browser to display output as a musical score

NB. Pitch to musicXML
NB. Given pitch name just do key of C translation to XML for now
p2xml =: 3 : 0
   fileout =. '/Users/tmcguire/musicXML/musicout.xml'
   mxmlhead =.  freads '/Users/tmcguire/musicXML/basemusic.xml'
   mxmlattr =.  freads '/Users/tmcguire/musicXML/measure.xml'
   mxmlendmeasure =.  freads '/Users/tmcguire/musicXML/endmeasure.xml'
   mxmlendmusic =.  freads '/Users/tmcguire/musicXML/endmusic.xml'
   mxmlhead fwrites fileout
   for_i. |:y do.
     'o p' =. i
     ('    <measure number="',(": i_index + 1),'">') fappends fileout
if. i_index = 0 do.   mxmlattr fappend fileout end.
        ('      <note>',LF,'       <pitch>') fappends fileout
          ('      <step>',p,'</step>') fappends fileout
   mxmlendmeasure fappends fileout
   end.
   mxmlendmusic fappends fileout
)

NB. melody routines
NB. melody support routines
expansion =: #^:_1
TMP =: 4 : 0
k=. >./($,y),|x
(k$y)+k$,x{.M
)

BGN =: 4 : 0
NB.(x - 0{x) + {. y
if. ($x) = ($y)do.
 (x - 0{x) + y
else.
 (x - 0{x) + {. y
end.
)

   CDNC212 =: 4 : 0
k =. (2+y)$ 2 1 2
+/\(x- +/k),k
)

DVLP2 =: 4 : 0
M=: P=. (1+2*$,y)$x
P =. (1 TMP y) (1+2*i. $,y)} P
M =: P,P
M =: M, _1 TMP 1+i. $,y
M =: M,M BGN _1 TMP 1
P =. (P BGN _1 TMP 1) + (($P)$0 1) expansion -1+i. $, y
M =: M,P
NB. Error in my original translation of APL code
NB. DVLP2 function on page 120. I used 'take' ('{.')
NB. instead of 'drop' ('}.') the following line is the corrected
NB. direct translation:
M =: M,(_2}.P),((0{P)+ 3 1 _3 _2),(0{P)
NB. The above is correct when tested on  the first 2 examples on page 122
NB. however in hand code on page 119 there is no (_2}.P)
NB. so I believe there's an error in the output displayed there
NB. and in the hand code. If you use the following line you will get
NB. the expected output for 0 DVLP2 1 but it then breaks the other 
NB. examples
NB. M =: M,((0{P)+ 3 1 _3 _2),(0{P)
P =. (0{P) CDNC212 $,y
M =: M,P,P,P
M =: M,(1 + _1{.P),_1 + ($,y){.P
)


