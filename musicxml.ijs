NB. musicxml.ijs - routines to print out musical score
NB. The output of this can be uploaded to the following site:
NB. https://www.soundslice.com
NB. this will render the MusicXML into standard music notation
NB. it also gives you a play button to hear the music through
NB. your computers sound card.

mxmlhdrsrc =: 0 : 0
<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE score-partwise PUBLIC "-//Recordare//DTD MusicXML 4.0 Partwise//EN" "http://www.musicxml.org/dtds/partwise.dtd">
)

mxmlscrwise =: 0 : 0
<score-partwise version="4.0">
)

mxmlhdr =: 3 : 0
OCTAVE =: 4 + ". ':' -.~ ;{.y
RestNotes =: }.y
LF chopstring mxmlhdrsrc
)

mxmlpartlist =: 3 : 0
z =. <'<part-list>'
z =. z,(,_2 mxmlscorepart\y)
z =. z,<'</part-list>'
z =. z,<'<part id="P1">'
z
)

mxmlscorepart =: 3 : 0
'spname pname' =. y
z =. <'<score-part id="',spname,'">'
z =. z,<'<part-name>',pname,'</part-name>'
z =. z,<'</score-part>'
z
)

mxmltreble4_4 =: 0 : 0
     <attributes>
        <divisions>1</divisions>
        <key>
          <fifths>0</fifths>
        </key>
        <time>
          <beats>4</beats>
          <beat-type>4</beat-type>
        </time>
        <clef>
          <sign>G</sign>
          <line>2</line>
        </clef>
      </attributes>
)

mxmlnote =: 3 : 0
NB.'reloct notes'=. 
octnote =. ,`((<'0')&,)@.(1&=@#) ] ,_2 <\y
'reloct notes' =. octnote
OCTAVE =: OCTAVE + ". reloct
accdtl =: _1 {. notes
z =. <'<note>'
if. notes ~: 'R' do.
z =. z,<'<pitch>'
z =. z,<'<step>',({.notes),'</step>'
if. accdtl = '#' do.
  z =. z,<'<alter>1</alter>'
elseif. accdtl = 'b' do.
  z =. z,<'<a;ter>-1</alter>'
end.
z =. z,<'<octave>',(":OCTAVE),'</octave>'
z =. z,<'</pitch>'
else.
z =. z,<'<rest />'
end.
z =. z,<'<duration>1</duration>'
z =. z,<'<voice>1</voice>'
z =. z,<'<type>quarter</type>'
if. accdtl = '#' do.
  z =.z,<'<accidental>sharp</accidental>'
elseif. accdtl = 'b' do.
  z =.z,<'<accidental>flat</accidental>'
end. 
z =. z,<'<stem>up</stem>'
z =. z,<'</note>'
)

mxmlmeasure1 =: 3 : 0
z =. <'<measure number="1">'
z =. z,LF chopstring mxmltreble4_4
z =. z, ;mxmlnote each y
z =. z,<'</measure>'
z
)

mxmlmeasurenxt =: 3 : 0
z =. ''
for_i. _4 ]\ y do.
NB. if there is a blank box that would represent a rest
z =. z,< '<measure number="',(":i_index+2),'">'
i =. ((<'')&=i)}i,:(#i)$<'R'
z =. z,(;mxmlnote each i -.<'')
z =. z,<'</measure>'
end.
z
)

mxmlend =: 0 : 0
</part>
</score-partwise>
)

NB. CM2Mxml - convert Cybernetic Music notes to MusicXML
NB. for now just key of C, treble clef, 4/4 time signature
NB. use only quarter notes.
NB. This will place accidentals (sharps and flats) and empty
NB. notes will show up as rests in the music
CM2Mxml =: 4 : 0
 Z=: mxmlhdr y
 Z=: Z,<mxmlscrwise
 Z=: Z,mxmlpartlist 'P1';'Music'
 Z=: Z, mxmlmeasure1 4{. RestNotes
 Z=: Z, mxmlmeasurenxt 4}. RestNotes
 Z=: Z,LF chopstring mxmlend
 (jpath x) fwrites~ ; ,&LF each Z
)

NB. Common usage of the above function:
NB.   '~/MusicXML/JEx2.xml' CM2Mxml (<'')-.~ chopstring N2PJ 1 DVLP2 2 3
NB.
NB. DVLP2 generates a melody
NB. N2PJ generates a string of pitches, these pitches have the following format:
NB. [[+|-](1-9)](CDEFGAorB)[#|b]
NB. +1C# indicates C-sharp in the next OCTAVE from current octave
NB. C is just the note C in the current OCTAVE
NB. -1Bb is one OCTAVE down from current note is B-flat
NB.
NB. using chop string these octave/note sequences are boxed
NB. this leaves a bunch of empty boxes where spaces used to be
NB. using the '-.' operator the empty boxes are removed.
NB. N2PJ the first box will be the Cyber Music starting Octave 0
NB. this is equivalent to OCTAVE 4 in MusicXML. This transposition
NB. is accounted for in the above code
