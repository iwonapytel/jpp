| package |
package := Package name: 'ParserLL'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #GramarLL1;
	add: #ParserLL1;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: 'Core\Object Arts\Dolphin\Base\Dolphin';
	yourself).

package!

"Class Definitions"!

Object subclass: #GramarLL1
	instanceVariableNames: 'start rules symbols nonTerminals terminals rulesMap rightOccurs'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #ParserLL1
	instanceVariableNames: 'start rules grammar stack'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

GramarLL1 guid: (GUID fromString: '{F30ACE0A-1969-4063-B654-1719383F4B2B}')!
GramarLL1 comment: ''!
!GramarLL1 categoriesForClass!Kernel-Objects! !
!GramarLL1 methodsFor!

checkIfLL1
	|llTable|
	llTable := self llTable.
	llTable do: [:x | [x do: [:y | ((y size) > 1) ifTrue: [^false]]]].
	^true!

first
	|first changed nullables|
	first := Dictionary new.
	nonTerminals do: [:x | first add: (x->(Set new))].
	nullables := self nullable.
	 [
		changed := false.
		nonTerminals do: [:x|
			|prods newValue|
			prods := rulesMap at: x.
			newValue := Set new.
			prods do: [:y | newValue addAll: (self firstSym: y firstArr: first nullables: nullables)].
			((first at: x)  equals: newValue) ifFalse: [(first at: x) addAll: newValue. changed := true].
			Transcript show: changed printString.
		].
		changed.
	] whileTrue: [].
	^first!

firstSym: x firstArr: a nullables: nlbls
	|term firstArr firsts firstLetter|
	term := x.
	firstArr := a.
	firsts := Set new.
	term isEmpty ifTrue: [^firsts].
	firstLetter := term at: 1.
	(terminals includes: firstLetter) ifTrue: [firsts add: firstLetter. ^firsts].
	firsts addAll: (firstArr at: firstLetter).
	(nlbls at: firstLetter) ifTrue: [firsts addAll: (self firstSym: (term allButFirst) firstArr: firstArr nullables: nlbls)].
	^firsts!

follow
	|follows firsts nullables changed|
	firsts := self first.
	nullables := self nullable.
	changed := false.
	follows := Dictionary new.
	nonTerminals do: [:x | follows add: (x->Set new)].

	 [
		changed := false.
		nonTerminals do: [:x|
			|rightRules newValue|
			rightRules := rightOccurs at: x.
			newValue := Set new.
			rightRules do: [:y | newValue addAll: (self followRule: y followNonTerm: x firsts: firsts nullables: nullables followsArr: follows)].
			(newValue equals: (follows at: x)) ifFalse: [(follows at: x) addAll: newValue. changed := true].
		].
		changed.
	] whileTrue: [].
	^follows
	

	
!

followRule: rule followNonTerm: nonterm firsts: firsts nullables: nullables followsArr: follows
	|followSet right Y delta|
	followSet := Set new.
	right := rule value.
	Y := rule key.
	delta := right copyFrom: ((right indexOf: nonterm) + 1).
	followSet addAll: (self firstSym: delta firstArr: firsts nullables: nullables).
	(self nlbl: delta nlblArray: nullables) ifTrue: [followSet addAll: (follows at: Y)].
	^followSet
	
	!

getNonTerminals
	nonTerminals := Set with: start.
	rules do: [:a | nonTerminals add: (a key)].
	^nonTerminals!

getSymbols
	symbols := Set with: start.
	rules do: [:a | symbols add: (a key). symbols addAll: (a value).].
	^symbols!

llTable
	|firsts follows nullables llTable rows|
	nullables := self nullable.
	firsts := self first.
	follows := self follow.
	
	llTable := Dictionary new.
	rows := Dictionary new.
	terminals do: [:x | rows add: (x->Set new)].
	nonTerminals do: [:x | llTable add: (x->rows)].
	
	rules do: [:x |
		|prod nonTerm column firstSet followSet|
		prod := x value.
		nonTerm := x key.
		column := llTable at: nonTerm.
		firstSet := self firstSym: prod firstArr: firsts nullables: nullables.
		firstSet do: [:y | (column at: y) add: x.].
		(self nlbl: prod nlblArray: nullables) ifTrue: [
			followSet := (follows at: nonTerm).
			followSet do: [:y | (column at: y) add: x]]
	].

	^llTable
	!

nlbl: p nlblArray: a
	|prod arr|
	prod := p.
	arr := a.
	prod isEmpty ifTrue: [^true].
	(prod anySatisfy: [:x | terminals includes: x]) ifTrue: [^false].
	(prod conform: [:x | (arr at: x) == true]) ifTrue: [^true].
	^false.!

nonTerminals
	^nonTerminals!

nullable
	|nullable changed|
	nullable := Dictionary new.
	nonTerminals do: [:x | nullable add: (x->false)].
	
	 [
		changed := false.
		nonTerminals do: [:x|
			|prods newValue|
			prods := rulesMap at: x.
			newValue := prods anySatisfy: [:y | self nlbl: y nlblArray: nullable].
			(newValue ~= (nullable at: x)) ifTrue: [nullable add: x->true. changed := true].
		].
		changed.
	] whileTrue: [].
	^nullable!

rightOccurs
	^rightOccurs!

rulesMap
	^rulesMap!

start: s rules: r
	start := s.
	rules := r.
	symbols := self getSymbols.
	nonTerminals :=self getNonTerminals.
	terminals := symbols - nonTerminals.
	rulesMap :=Dictionary new.
	rightOccurs := Dictionary new.
	nonTerminals do: [:x | rulesMap add: (x->(Set new)). rightOccurs add: (x->(Set new))].
	rules do: [:x | (rulesMap at: (x key)) add: (x value)].
	rules do: [:x | (x value) do: [:y | (nonTerminals includes: y) ifTrue: [(rightOccurs at: y) add: x]]].
!

symbols
	^symbols!

terminals
	^terminals! !
!GramarLL1 categoriesFor: #checkIfLL1!public! !
!GramarLL1 categoriesFor: #first!public! !
!GramarLL1 categoriesFor: #firstSym:firstArr:nullables:!private! !
!GramarLL1 categoriesFor: #follow!public! !
!GramarLL1 categoriesFor: #followRule:followNonTerm:firsts:nullables:followsArr:!private! !
!GramarLL1 categoriesFor: #getNonTerminals!private! !
!GramarLL1 categoriesFor: #getSymbols!private! !
!GramarLL1 categoriesFor: #llTable!public! !
!GramarLL1 categoriesFor: #nlbl:nlblArray:!private! !
!GramarLL1 categoriesFor: #nonTerminals!public! !
!GramarLL1 categoriesFor: #nullable!public! !
!GramarLL1 categoriesFor: #rightOccurs!public! !
!GramarLL1 categoriesFor: #rulesMap!public! !
!GramarLL1 categoriesFor: #start:rules:!public! !
!GramarLL1 categoriesFor: #symbols!public! !
!GramarLL1 categoriesFor: #terminals!public! !

!GramarLL1 class methodsFor!

start: s rules: r
	^super new start: s rules: r! !
!GramarLL1 class categoriesFor: #start:rules:!public! !

ParserLL1 guid: (GUID fromString: '{9579FD80-7040-4BFC-9AE0-3D941E73572D}')!
ParserLL1 comment: ''!
!ParserLL1 categoriesForClass!Kernel-Objects! !
!ParserLL1 methodsFor!

accept
	|nullable|
	nullable := grammar nullable.
	^(nullable at: start)!

deriv: x
	stack add: x.
	^stack.!

grammar
	^grammar!

ifTerminates: term terminateArr: arr
	term isEmpty ifTrue: [^true].
	(term conform: [:x | ((grammar terminals) includes: x) ifTrue: [true] ifFalse: [arr at: x]]) ifTrue: [^true].
	^false.!

predict
	|firsts|
	firsts := grammar first.
	^(firsts at: start).!

react: x
	|newParser|
	newParser := ParserLL1 start: start rules: rules.
	newParser deriv: x.
	^newParser.!

reject
	|terminates changed rulesMap|
	terminates := Dictionary new.
	(grammar nonTerminals) do: [:x | terminates add: x->false].
	rulesMap := grammar rulesMap.

	[
		changed := false.
		(grammar nonTerminals) do: [ :x |
			|xRules newValue|
			xRules := rulesMap at: x.
			newValue := xRules anySatisfy: [:y| self ifTerminates: y terminateArr: terminates].
			(newValue ~= (terminates at: x)) ifTrue: [terminates add: x->true. changed := true].
		].
		changed.
	] whileTrue: [].
	
	^(terminates at: start) not.
	
!

start: s rules: r
	start := s.
	rules := r.
	grammar := GramarLL1 start: s rules: r.
	stack := OrderedCollection new.! !
!ParserLL1 categoriesFor: #accept!public! !
!ParserLL1 categoriesFor: #deriv:!public! !
!ParserLL1 categoriesFor: #grammar!public! !
!ParserLL1 categoriesFor: #ifTerminates:terminateArr:!private! !
!ParserLL1 categoriesFor: #predict!public! !
!ParserLL1 categoriesFor: #react:!public! !
!ParserLL1 categoriesFor: #reject!public! !
!ParserLL1 categoriesFor: #start:rules:!public! !

!ParserLL1 class methodsFor!

start: s rules: r
	|parser|
	parser := super new start: s rules: r.
	parser grammar checkIfLL1 ifFalse: [^nil].
	^parser! !
!ParserLL1 class categoriesFor: #start:rules:!public! !

"Binary Globals"!

